// LoopInvariantMotion.hpp
// Copyright (c) Lup Gratian
//
// Implements the LoopInvariantMotion class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "LoopInvariantMotion.hpp"

//! USE REAL ALIAS ANALYSIS
//! MOVE STORES OUTSIDE LOOP
//! MOVE INSTR TO OUTERMOST LOOP where it is invariant (not in steps like now)
//! (see robert morgan book)

namespace Optimization {

void LoopInvariantMotion::Execute(Function* function) {
    auto loopTag = function->GetTag<LoopTag>();

    if(loopTag) {
        loopTag->ForEachLoop([this](Loop* loop) -> bool {
            if(loop->IsTopLevel()) {
                HoistLoopInvariantInstructions(loop);
            }

            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoopInvariantMotion::IsLoopInvariant(Instruction* instr, Loop* loop) {
    // An instruction is considered loop-invariant if it is safe
    // to hoist it outside the loop (special checks are needed for
    // loads and calls), no safety constraint is used on them
    // and all their operands are loop invariant.
    if(auto loadInstr = instr->As<LoadInstr>()) {
        return IsLoopInvariantLoad(loadInstr, loop);
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        return IsLoopInvariantCall(callInstr, loop);
    }
    else if(instr->IsBranching() || instr->IsPhi() || instr->IsStore()) {
        // It's not valid to hoist these instruction types.
        return false;
    }

    // Each operand must be invariant.
    for(int i = 0; i < instr->SourceOpCount(); i++) {
        if(IsLoopInvariant(instr->GetSourceOp(i), loop) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoopInvariantMotion::IsLoopInvariant(Operand* op, Loop* loop) {
    // Any operand which is not an instruction is loop-invariant.
    // Instructions are considered loop-invariant only if they
    // are defined outside the loop.
    return (op->HasDefiningInstruction() == false) ||
           (loop->IsDefinedInLoop(op->DefiningInstruction()) == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoopInvariantMotion::IsLoopInvariantLoad(LoadInstr* instr, Loop* loop) {
    // A 'load' is loop-invariant only if the loaded address is loop-invariant
    // and it can be guaranteed there is no instruction inside the loop
    // that could modify the memory location.
    return GetSafetyInfo()->IsSafeToReorderLoad(instr) &&
           IsLoopInvariant(instr->SourceOp(), loop) &&
           MayBeWrittenInLoop(instr->SourceOp(), loop) == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoopInvariantMotion::MayBeWrittenInLoop(Operand* op, Loop* loop) {
    bool written = false;

    loop->ForEachBlock([&, this](Block* block) -> bool {
        // If we already scanned this block it might be known
		// it does not contain any 'store' or 'call' instructions.
		if(noWriteAccessBlocks_.IsSet(block->Id())) {
			return true; // Continue with next block.
		}

		bool hasNoStoreOrCall = true;

        block->ForEachInstruction([&](Instruction* instr) -> bool {
            if(instr->IsStore()) {
				//! TODO: Use alias analysis
				hasNoStoreOrCall = false;
                written = true;
                return false;
            }
            else if(auto callInstr = instr->As<CallInstr>()) {
				//! TODO: use alias analysis
				hasNoStoreOrCall = false;
                written = true;
                return false;
            }

            return true;
        });

		// Subsequent queries should not scan the whole block again
		// if it doesn't contain 'store' or 'call' instructions.
		if(hasNoStoreOrCall) {
			noWriteAccessBlocks_.SetBit(block->Id());
		}

		// No reason to scan the rest of the blocks
		// if it was found that the operand may be written.
        return written == false;
    });

    return written;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoopInvariantMotion::IsLoopInvariantCall(CallInstr* instr, Loop* loop) {
    // A 'call' is loop invariant only if it doesn't depend on
    // the global application state and all it's arguments
    // are also loop-invariant. For pointers we need to check that
    // there is no instruction in the loop that may write to them.
    auto function = instr->GetCalledFunction();

    if(function == nullptr) {
        //! TODO: this could be improved using the Call Graph
        return false;
    }

    if((IsNotStateDependent(function) == false) || function->IsVarargs()) {
        return false;
    }

    // Check if each argument is loop-invariant. If the argument
    // is a pointer special aliasing checks need to be made.
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        auto argument = instr->GetArgument(i);

        if(argument->IsPointer()) {
			//! TODO: use alias info
			return false;	
		}
		else if(IsLoopInvariant(argument, loop) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoopInvariantMotion::IsNotStateDependent(CallInstr* instr) {
	auto function = instr->GetCalledFunction();

    if(function == nullptr) {
        // The call is through a pointer, check if the Call Graph
		// knows the potential called functions.
		if(auto callGraph = GetCallGraph()) {
			auto callNode = callGraph->GetCallSite(instr);

			if((callNode->CallsUnknownFunctions() == false) &&
			   (callNode->CallsNodeGroup() == false)) {
				bool notStateDependent = true;

				callNode->ForEachCalledFunction([&](Function* funct) -> bool {
					if(IsNotStateDependent(funct) == false) {
						notStateDependent = false;
						return false;
					}
					else return true;
				});

				return notStateDependent;
			}
		}

        return false;
    }

    return IsNotStateDependent(function) &&
		   (function->IsVarargs() == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoopInvariantMotion::IsNotStateDependent(Function* function) {
    // The function should either be a known intrinsic
    // or it should be marked with the 'nostate' flag.
    if(function->IsNoState()) {
        return true;
    }
    else if(auto intrinsic = function->As<Intrinsic>()) {
        return intrinsic->IsMathIntrinsic() || 
               intrinsic->IsBitwiseIntrinsic();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoopInvariantMotion::HoistLoopInvariantInstructions(Loop* loop) {
    loop->ForEachNestedLoop([this](Loop* nestedLoop) -> bool {
        HoistLoopInvariantInstructions(nestedLoop);
        return true;
    });

    loop->ForEachBlock([this, loop](Block* block) -> bool {
		if(visitedBlocks_.IsNotSet(block->Id())) {
			HoistLoopInvariantInstructions(block, loop);
			visitedBlocks_.SetBit(block->Id());
		}

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoopInvariantMotion::HoistLoopInvariantInstructions(Block* block, Loop* loop) {
    auto instr = block->FirstInstruction();

    while(instr) {
        auto nextInstr = instr->NextInstruction();

        if(IsLoopInvariant(instr, loop) &&
		   IsProfitableToHoist(instr)) {
            MoveToPreheader(instr, loop);
        }

        instr = nextInstr;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoopInvariantMotion::MoveToPreheader(Instruction* instr, Loop* loop) {
    auto preheader = loop->CreatePreheader();
    instr->RemovedFromParent();
    preheader->InsertInstructionBefore(instr, preheader->LastInstruction());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LoopInvariantMotion::IsProfitableToHoist(Instruction* instr) {
	//! TODO: use profile info, don't hoist if on "cold" path!
    // If instr is load require a "hotter" block.
	return true;
}

} // namespace Optimization