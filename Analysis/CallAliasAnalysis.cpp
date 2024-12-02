// CallAliasAnalysis.cpp
// Copyright (c) Lup Gratian
//
// Implements the CallAliasAnalysis class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "CallAliasAnalysis.hpp"

namespace Analysis {

// special cases for:
//! - global 
//    - no addr taken
//    - no indirect write in called functs
//! - local vars
//    - no addr taken
//    - no escaped address
//    - no indirect write in called functs
//! - parameter
//? Is accessed by parameter funct

MemoryResult CallAliasAnalysis::ComputeCallEffects(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);

	// Most calls are direct (through a function reference).
	// If the call is done through a pointer the Call Graph might
	// known which are the possible targets and a better
	// result than 'Unknown' could be returned sometimes.
	if(auto calledFunct = instr->GetCalledFunction()) {
		return ComputeCallEffects(calledFunct, instr);
	}
	else if(auto callGraph = GetCallGraph()) {
		return ComputeCallEffects(callGraph, instr);
	}
	else return MemoryResult::GetUnknown();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MemoryResult CallAliasAnalysis::ComputeCallEffects(Function* funct, CallInstr* instr) {
	DebugValidator::IsNotNull(funct);

	// The effects of the function are determined using the results
	// of previous interprocedural analysis that annotated the function
	// or associated various tags describing the effects.
	MemoryResult result = MemoryResult::GetNone();

	if(funct->IsIndirectRead()) {
		result.SetOperation(MemoryType::Indirect, 
                            MemoryOperation::Read);
	}

	if(funct->IsIndirectWrite()) {
		result.SetOperation(MemoryType::Indirect,
                            MemoryOperation::Write);
	}

	// Check if any global variables are read or written.
	// If no information is available presume there are.
	ComputeGlobalEffects(funct, instr, result);

	// Check if any pointer parameter is read or written.
	// If no information is available presume there are.
	ParameterInfo paramInfo;
	ComputeParameterEffects(funct, instr, paramInfo, result);
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MemoryResult CallAliasAnalysis::ComputeCallEffects(CallGraph* callGraph, CallInstr* instr) {
	DebugValidator::IsNotNull(callGraph);
	DebugValidator::IsNull(instr->GetCalledFunction());

	// Unify the effects of the possibly called functions.
	// If the unknown function is called sadly we need to presume
	// that any memory might be modified.
	auto callSite = callGraph->GetCallSite(instr);

	if(callSite->CallsUnknownFunctions()) {
		return MemoryResult::GetUnknown();
	}

	// If node groups are called merge the results 
	// of all contained functions.
	MemoryResult result = MemoryResult::GetNone();

	callSite->ForEachCalledNode([&result, instr, this]
                                (CallNodeBase* callNode) -> bool {
		if(callNode->IsNodeGroup()) {
			auto nodeGroup = static_cast<CallNodeGroup*>(callNode);
			auto groupResult = ComputeCallEffects(nodeGroup, instr);
			result = MergeResults(result, groupResult);
		}
		else {
			auto nodeFunction = static_cast<CallNode*>(callNode)->GetFunction();
			auto nodeResult = ComputeCallEffects(nodeFunction, instr);
			result = MergeResults(result, nodeResult);
		}

		return true;
	});

	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MemoryResult CallAliasAnalysis::ComputeCallEffects(CallNodeGroup* nodeGroup, 
												   CallInstr* instr) {
	DebugValidator::IsNotNull(nodeGroup);
	MemoryResult result = MemoryResult::GetNone();

	nodeGroup->ForEachNode([&result, instr, this](CallNodeBase* callNode) -> bool {
		if(callNode->IsNodeGroup()) {
			result = MemoryResult::GetUnknown();
			return false;
		}

		auto nodeFunction = static_cast<CallNode*>(callNode)->GetFunction();
		auto nodeResult = ComputeCallEffects(nodeFunction, instr);
		result = MergeResults(result, nodeResult);
		return true;
	});

	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallAliasAnalysis::ComputeGlobalEffects(Function* funct, CallInstr* instr,
											 MemoryResult& result) {
	// Check if we know something about the accessed global variables.
	// If the function accesses indirect memory we sadly need to presume
	// that it might point to global variables.
	if(funct->IsDeclaration()) {
		// For declarations we might know the effect if
		// the function is an intrinsic or it is part of the
		// standard libraru and language information is available.
		return ComputeGlobalEffectsForDecl(funct, instr, result);
	}

	// Check for indirect memory access.
	if(funct->IsIndirectRead()) {
		result.SetOperation(MemoryType::Global, 
                            MemoryOperation::Read);
	}

	if(funct->IsIndirectWrite()) {
		result.SetOperation(MemoryType::Global, 
                            MemoryOperation::Write);
	}

	if(result.ReadsAndWritesGlobalMemory()) {
		// No reason to continue.
		return;
	}

	// Check if global variables are accessed.
	if(auto globalSideEffects = funct->GetTag<GlobalSideEffectsTag>()) {
		if(globalSideEffects->HasUnknownEffects()) {
			result.SetOperation(MemoryType::Global, 
                                MemoryOperation::ReadWrite);
			return;
		}

		if(globalSideEffects->ReadsVariables()) {
			result.SetOperation(MemoryType::Global,
                                MemoryOperation::Read);
		}

		if(globalSideEffects->WritesVariables()) {
			result.SetOperation(MemoryType::Global, 
                                MemoryOperation::Write);
		}
	}
	else {
		// With no information we sadly need to presume that
		// global variables are both read and written.
		result.SetOperation(MemoryType::Global,
                            MemoryOperation::ReadWrite);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
template <class Info>
void CallAliasAnalysis::ComputeParameterEffects(Function* funct, CallInstr* instr,
												Info& parameterInfo, 
												MemoryResult& result) {
	// Accumulate the effects of each parameter that is a pointer.
	// Because the parametes might point to global variables
	// we need to check this and mark appropriately.
	for(int i = 0; i < funct->ParameterCount(); i++) {
		auto parameterVariable = funct->GetParameterVariable(i);
		bool set = false;

		if(parameterVariable->IsPointer()) {
			if(parameterInfo.ReadsParameter(instr, funct, i)) {
				set = true;
				result.SetOperation(MemoryType::Parameters, 
                                    MemoryOperation::Read);

				if(IsNotGlobalVariable(funct->GetParameter(i)) == false) {
					result.SetOperation(MemoryType::Global, 
                                        MemoryOperation::Read);
				}
			}

			if(parameterInfo.WritesParameter(instr, funct, i)) {
				set = true;
				result.SetOperation(MemoryType::Parameters, 
                                    MemoryOperation::Write);
				
				if(IsNotGlobalVariable(funct->GetParameter(i)) == false) {
					result.SetOperation(MemoryType::Global,
                                        MemoryOperation::Write);
				}
			}

			if(set && // Don't query if not changed.
			   result.ReadsAndWritesParametersMemory() &&
			   result.ReadsAndWritesGlobalMemory()) {
				// No reason to continue.
				return;
			}
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallAliasAnalysis::ComputeGlobalEffectsForDecl(Function* funct, CallInstr* instr,
													MemoryResult& result) {
	DebugValidator::IsNotNull(funct);
	DebugValidator::IsTrue(funct->IsDeclaration());

	if(auto intrinsic = funct->As<Intrinsic>()) {
		ComputeGlobalEffectsForIntrinsic(intrinsic, instr, result);
	}
	else if(auto languageInfo = GetLanguageInfo()) {
		// Some library functions have no side effects.
		if(languageInfo->CallMayHaveSideEffects(instr) == false) {
			result = MemoryResult::GetNone();
			return;
		}

		// Check if global variables may be written.
		if(languageInfo->CallMayWriteGlobals(funct)) {
			result.SetOperation(MemoryType::Global, 
                                MemoryOperation::Write);
		}

		if(languageInfo->CallMayReadGlobals(funct)) {
			result.SetOperation(MemoryType::Global, 
                                MemoryOperation::Read);
		}

		// Check each parameter and determine the effects.
		LanguageParameterInfo langParamInfo(languageInfo, this);
		ComputeParameterEffects(funct, instr, langParamInfo, result);
	}
	else {
		// With no information we sadly need to presume that
		// global variables are both read and written.
		result = MemoryResult::GetUnknown();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CallAliasAnalysis::ComputeGlobalEffectsForIntrinsic(Intrinsic* intrinsic, 
														 CallInstr* instr,
														 MemoryResult& result) {
	// Math and bitwise intrinsics don't affect memory at all.
	// The same is true for 'prefetch' (it's just a hint for the CPU).
	if(intrinsic->IsMathIntrinsic()    ||
	   intrinsic->IsBitwiseIntrinsic() ||
	   intrinsic->Is<PrefetchIntr>()) {
		result = MemoryResult::GetNone();
		return;
	}

	// 'setMemory' writes to memory, while 'copyMemory'
	// both writes and reads. They might write to global variables.
	if(intrinsic->Is<SetMemoryIntr>()) {
		result.SetOperation(MemoryType::Parameters, 
                            MemoryOperation::Write);

		if(IsNotGlobalVariable(SetMemoryIntr::GetDestination(instr)) == false) {
			result.SetOperation(MemoryType::Global, 
                                MemoryOperation::Write);
		}

		return;
	}
	else if(intrinsic->Is<CopyMemoryIntr>()) {
		result.SetOperation(MemoryType::Parameters, 
                            MemoryOperation::ReadWrite);

		if(IsNotGlobalVariable(CopyMemoryIntr::GetDestination(instr)) == false) {
			result.SetOperation(MemoryType::Global, 
                                MemoryOperation::Write);
		}

		if(IsNotGlobalVariable(CopyMemoryIntr::GetSource(instr)) == false) {
			result.SetOperation(MemoryType::Global, 
                                MemoryOperation::Read);

			// If the destination aliases the source
			// it also might be written.
			if(result.WritesGlobalMemory() == false) {
				auto source = CopyMemoryIntr::GetSource(instr);
				auto dest = CopyMemoryIntr::GetDestination(instr);

				if(Parent()->HasAliasWithUnknownSize(source, dest)) {
					result.SetOperation(MemoryType::Global, 
                                        MemoryOperation::Write);
				}
			}
		}

		return;
	}

	// For any other intrinsic presume the behavior is unknown.
	result = MemoryResult::GetUnknown();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MemoryResult CallAliasAnalysis::MergeResults(MemoryResult resultA, 
											 MemoryResult resultB) {
	if(resultA.DoesNotAccessMemory()) {
		return resultB;
	}
	else if(resultB.DoesNotAccessMemory()) {
		return resultA;
	}
	else if(resultA.AccessesUnknownMemory() ||
			resultB.AccessesUnknownMemory()) {
		return MemoryResult::GetUnknown();
	}

	// Merge the known information.
	MemoryResult result = MemoryResult::GetNone();

	if(resultA.AccessesGlobalMemory() || 
	   resultB.AccessesGlobalMemory()) {
		auto operation = (MemoryOperation)((int)resultA.OperationForGlobalMemory() |
										   (int)resultB.OperationForGlobalMemory());
		result.SetOperation(MemoryType::Global, operation);
	}

	if(resultA.AccessesIndirectMemory() || 
	   result.AccessesIndirectMemory()) {
		auto operation = (MemoryOperation)((int)resultA.OperationForIndirectMemory() |
										   (int)resultB.OperationForIndirectMemory());
		result.SetOperation(MemoryType::Indirect, operation);
	}

	if(resultA.AccessesParametersMemory() || 
	   resultB.AccessesParametersMemory()) {
		auto operation = (MemoryOperation)((int)resultA.OperationForParametersMemory() |
										   (int)resultB.OperationForParametersMemory());
		result.SetOperation(MemoryType::Parameters, operation);
	}

	return result;
}

} // namespace Analysis