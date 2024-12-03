// AllocFunctionRecognizer.cpp
// Copyright (c) Lup Gratian
//
// Implements the AllocFunctionRecognizer class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "AllocFunctionRecognizer.hpp"

namespace Analysis {

void AllocFunctionRecognizer::Execute() {
	auto callGraph = GetCallGraph();
	callGraph->FindRoots(true);
    callGraph->ReverseInvocationTraversalFromRoots(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::Visit(CallNode* node, CallGraph* callGraph) {
	// If the function is a candidate collect all return
	// instructions and check if all return a pointer that
	// originates from a call to a memory allocation function.
	AllocationList allocations;
	auto function = node->GetFunction();
	
	if(IsCandidate(function) == false) {
		return true; // Contiue with next function.
	}

	if(CollectReturns(function, allocations) == false) {
		return true;
	}

	// All paths return valid pointers, mark the fact that
	// the function allocates memory. The result can be improved 
	// if the program exits if no memory could be allocated
	// (where the pointer is returned it is known that it never
	//  can be the null pointer, can help some optimizations).
	auto tag = GetOrCreateTag(function);

	if(IsCheckedAllocation(allocations)) {
		tag->SetIsCheckedAllocLike(true);
	}
	else tag->SetIsAllocLike(true);

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::IsCandidate(Function* function) {
	// The function must return a pointer and at least
	// a function should be called inside. We check later that
	// the called function is an allocation function.
	return  function->IsDefinition() &&
		   (function->IsVoid() == false) &&
		    function->ReturnType()->IsPointer() &&
		   (function->CallInstructionCount() > 0);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::FindBaseAllocCalls(Operand* op, AllocationList& allocations,
											     bool phiAllowed) {
	// Walk the chain of possible 'addr'/'field' instructions
	// until the 'call' instruction is found.
	while(op) {
		if(op->HasDefiningInstruction() == false) {
			// No variables can be the base.
			break;
		}

		if(auto callInstr = op->DefiningInstrAs<CallInstr>()) {
			// Found a call, check if it allocates memory.
			bool isChecked;

			if(IsAllocationCall(callInstr, isChecked)) {
				allocations.Add(CallCheckedPair(callInstr, isChecked));
				return true;
			}
			
			break;
		}
		else if(auto addrInstr = op->DefiningInstrAs<AddressInstr>()) {
			op = addrInstr->BaseOp();
		}
		else if(auto fieldInstr = op->DefiningInstrAs<FieldInstr>()) {
			op = fieldInstr->BaseOp();
		}
		else if(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
			op = ptopInstr->TargetOp();
		}
		else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
			return FindBaseAllocCalls(questInstr->TrueOp(), allocations, phiAllowed) &&
				   FindBaseAllocCalls(questInstr->FalseOp(), allocations, phiAllowed);
		}
		else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
			// Give up if a 'phi' was already processed
			// (this also prevents entering phi cycles).
			if(phiAllowed == false) {
				break;
			}

			for(int i = 0; i < phiInstr->OperandCount(); i++) {
				if(FindBaseAllocCalls(phiInstr->GetOperand(i), allocations, 
									  false /* phiAllowed */) == false) {
					return false;
				}
			}

			return true;
		}
		else {
			// Any other instruction not supported here.
			break;
		}
	}

	return false; // No valid 'call' found.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::IsAllocationCall(CallInstr* instr, bool& isChecked) {
	// In most cases the called function is known
	// and the check is fast. If an indirect call is made
	// and the possible targets are known we check each one.
	if(auto calledFunct = instr->GetCalledFunction()) {
		return IsAllocationCall(calledFunct, isChecked);
	}

	if(auto callGraph = GetCallGraph()) {
		auto callSite = callGraph->GetCallSite(instr);

		// No unknown function or group should be called,
		// the effects are unknown.
		if((callSite->CallsUnknownFunctions() == false) &&
		   (callSite->CallsNodeGroup() == false) &&
		   (callSite->CalledFunctionsCount() <= 8)) {
			bool valid = true;
			isChecked = true;

			callSite->ForEachCalledFunction([&valid, &isChecked, this]
											(Function* funct) -> bool {
				bool isCallChecked;
				if(IsAllocationCall(funct, isCallChecked) == false) {
					valid = false;
				}

				isChecked = isChecked && isCallChecked;
				return valid;
			});

			return valid;
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::IsAllocationCall(Function* function, bool& isChecked) {
	// First check if the function has the tag already attached,
	// otherwise try to use language information.
	isChecked = false;
	
	if(auto tag = function->GetTag<CFamilyTag>()) {
		isChecked = tag->IsCheckedAllocLike();
		return tag->IsAllocLike();
	}
	else if(auto languageInfo = GetLanguageInfo()) {
		if(languageInfo->IsAllocFunction(function)) {
			return true;
		}
		else if(languageInfo->IsCheckedAllocFunction(function)) {
			isChecked = true;
			return true;
		}
	}

	

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::CollectReturns(Function* function, 
										     AllocationList& allocations) {
	// Check if on all paths a pointer originating from a call
	// to a memory allocation function is returned.
	bool valid = true;

	function->ForEachInstructionOfType<ReturnInstr>([&valid, &allocations, this]
													(ReturnInstr* instr) -> bool {
		auto returnedOp = instr->ReturnedOp();

		if(returnedOp->IsNullConstant() ||
		   returnedOp->IsUndefinedConstant()) {
			valid = false;
			return false; // Stop enumeration.
		}

		// Remember if it is known already that the pointer
		// is definitely not null.
		valid = FindBaseAllocCalls(returnedOp, allocations);
		return valid;
	});

	return valid;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::IsCheckedAllocation(AllocationList& allocations) {
	// Check if the pointer for each allocated memory location
	// is checked if it is null, and if true the application is exited.
	// Example in C:
	// void* data = malloc(size);
	// if(data == NULL) exit();
	// else return data; // 'data' is definitely not NULL.
	for(int i = 0; i < allocations.Count(); i++) {
		if(allocations[i].IsChecked) {
			// No other check needed here.
			continue;
		}

		if(IsCheckedAllocation(allocations[i].Call->ResultOp()) == false) {
			return false;
		}
	}

	// All allocations are checked.
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::IsCheckedAllocation(Temporary* temp) {
	// 'op' points to the allocated memory. There are two ways 
	// the test could be done by the user:
	// t1 = ucmp eq op, nullptr
	// if t1, exitBlock, return block
	//    or
	// t1 = ucmp neq op, nullptr
	// if t1, trueBlock, exitBlock
	DebugValidator::IsTrue(temp->HasUsers());

	// First skip over any pointer conversions,
	// they would prevent the detection.
	temp = WithoutPtop(temp);
	auto ucmpInstr = temp->GetUser(0)->As<UcmpInstr>();

	if((ucmpInstr == nullptr)                   ||
	   (ucmpInstr->IsEquality() == false)       ||
	   (ucmpInstr->HasDestinationOp() == false) ||
	   (ucmpInstr->ResultOp()->HasSingleUser() == false)) {
		return false;
	}

	// Canonicalize by moving the constant on the right.
	if(ucmpInstr->LeftOp()->IsConstant()) {
		ucmpInstr->InvertOrder(true /* invertOperands */, 
							   false /* invertEquality */);
	}

	if(ucmpInstr->RightOp()->IsNullConstant() == false) {
		return false;
	}

	// The user should be an 'if' instruction that selects
	// the exit block if the pointer is null.
	auto ifInstr = ucmpInstr->ResultOp()->GetUser(0)->As<IfInstr>();

	if(ifInstr == nullptr) {
		return false;
	}

	if(ucmpInstr->IsEqual()) {
	   return IsForcedExitBlock(ifInstr->TrueTargetBlock());
	}
	else return IsForcedExitBlock(ifInstr->FalseTargetBlock());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocFunctionRecognizer::IsForcedExitBlock(Block* block) {
	// For simplicity and speed scan only this block
	// and search for a call to a function that exits the application.
	bool found = false;

	block->ForEachInstruction([&found, this](Instruction* instr) -> bool {
		if(auto callInstr = instr->As<CallInstr>()) {
			if(auto languageInfo = GetLanguageInfo()) {
				if(languageInfo->IsProgramExitPoint(callInstr)) {
					found = true;
				}
			}
		}

		return found == false;
	});

	return found;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Temporary* AllocFunctionRecognizer::WithoutPtop(Temporary* temp) {
	while(temp->HasUsers() && temp->GetUser(0)->Is<PtopInstr>()) {
		temp = temp->GetUser(0)->GetDestinationOp();
	}

	return temp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CFamilyTag* AllocFunctionRecognizer::GetOrCreateTag(Function* function) {
	CFamilyTag* tag = function->GetTag<CFamilyTag>();

	if(tag == nullptr) {
		tag = CFamilyTag::GetCFamily();
		function->AddTag(tag);
	}

	return tag;
}

} // namespace Analysis