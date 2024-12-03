// SafetyInfo.cpp
// Copyright (c) Lup Gratian
//
// Implements the SafetyInfo class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SafetyInfo.hpp"

namespace Analysis {

bool SafetyInfo::IsDefinitelyDead(Instruction* instr) {
    // An instruction is dead if its result is not used by any other instruction.
    // We need to take care of 'store', 'call' and volatile 'load' though.
    if(auto loadInstr = instr->As<LoadInstr>()) {
        // If it's marked as 'volatile' we can't eliminate it.
        if(IsSafeToEliminateLoad(loadInstr) == false) {
            return false;
        }

        return (loadInstr->ResultOp() == nullptr) ||
               (loadInstr->ResultOp()->HasNoUser());
    }
    else if(auto storeInstr = instr->As<StoreInstr>()) {
        // We don't know where we write the result, so be conservative.
        // An exception is when we store an undefined value.
        if(IsSafeToEliminateStore(storeInstr) == false) {
            return false;
        }
        return storeInstr->SourceOp()->IsUndefinedConstant();
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        if(callInstr->ResultOp() && callInstr->ResultOp()->HasUsers()) {
            return false;
        }

        // A special case are intrinsics, for which we may know that they
        // don't have any side-effects.
        if(auto intrinsic = callInstr->GetIntrinsic()) {
            if(intrinsic->IsMathIntrinsic() || intrinsic->IsBitwiseIntrinsic()) {
                // min, max, sin, bswap, etc.
                return true;
            }
            else if(intrinsic->IsBoundsCheckIntrinsic()) {
                // A bounds checking from which we determined that the access
                // is always in range can be safely removed.
                return callInstr->GetArgument(0)->IsOneInt();
            }
        }

        // We need to presume that the 'call' has side-effects.
        return false;
    }
    else if(instr->IsBranching()) {
        // We presume that branching instructions are not dead.
        return false;
    }
    
    // For all other cases we check if the result is used by any other instruction.
    if(instr->HasDestinationOp() == false) {
        return true;
    }

    return instr->GetDestinationOp()->HasNoUser();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::IsSafeToEliminateLoad(LoadInstr* instr) {
    // 'volatile' loads should not be eliminated.
    return instr->IsVolatile() == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::IsSafeToEliminateStore(StoreInstr* instr) {
    // 'volatile' stores should not be eliminated.
    return instr->IsVolatile() == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::IsSafeToReorderLoad(LoadInstr* instr) {
    // It is best not reorder volatile loads and mandatory
    // not to reorder loads found in synchronization regions.
    if(instr->IsVolatile()) {
        return false;
    }

    return instr->HasTag<SynchronizationTag>() == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::IsSafeToReorderStore(StoreInstr* instr) {
    // It is best not reorder volatile stores and mandatory
    // not to reorder stores found in synchronization regions.
    if(instr->IsVolatile()) {
        return false;
    }

    return instr->HasTag<SynchronizationTag>() == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::CanBeSpeculated(Instruction* instr, Block* toBlock) {
    DebugValidator::IsNotNull(instr);

    // An instruction can safely be executed speculatively
    // if it doesn't have any visible side-effect on the program.
    // Most arithmetic and all logical instructions match this condition.
    if(instr->IsLogical()) {
        return true;
    }
    else if(instr->IsArithmetic()) {
        // Don't try to speculate some floating point instructions, 
        // they are expensive and it may not be worth it.
        if(instr->IsFloatArithmetic()) {
            auto arithInstr = instr->As<ArithmeticInstr>();
            
            // We can speculate only if the user indicates that
            // floating-point exceptions can be ignored.
            if(arithInstr->IsFast()) {
                return instr->IsFadd() || 
                       instr->IsFsub() || 
                       instr->IsFmul();
            }
        }

        switch(instr->GetOpcode()) {
            case Opcode::Div:
            case Opcode::Udiv:
            case Opcode::Mod:
            case Opcode::Umod: {
                // We can speculate div/mod only if we are certain that
                // the operation cannot trap (right operand is zero).
                // This can't happen if we have a constant that is not zero,
                // or if we know that at least one bit is set in the right operand.
                if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                    return intConst->IsZero() == false;
                }

                auto unit = instr->ParentBlock()->ParentFunction()->ParentUnit();
                OperandInfo opInfo(unit);
                return opInfo.IsNotZero(instr->GetSourceOp(1), toBlock);
            }
            default: {
                // All other arithmetic instruction can be speculated.
                return true;
            }
        }
    }
    else if(instr->IsComparison() ||
            instr->IsConversion() ||
            instr->IsAddressing()) {
        return true;
    }
    else if(instr->IsControl()) {
        if(instr->IsIf()) {
            return true;
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            // Some intrinsics can be speculated.
            auto intrinsic = callInstr->GetIntrinsic();

            if(intrinsic == nullptr) {
                return false;
            }

            // Allow only cheap intrinsics to be speculated.
            if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
                return mathIntrinsic->IsMin() ||
                       mathIntrinsic->IsMax() || 
                       mathIntrinsic->IsAbs();
            }
            else return intrinsic->IsBitwiseIntrinsic();
        }
    }
    else if(instr->IsQuestion()) {
        return true;
    }
    else if(auto loadInstr = instr->As<LoadInstr>()) {
        // Volatile loads should not be touched.
        if(IsSafeToReorderLoad(loadInstr) == false) {
            return false;
        }

        // Check if we know that this 'load' can't trap.
        // This happens when there is a 'load' or 'store' from/to the same address 
		// in a block that dominates this one (if there would have been an exception
		// control wouldn't have reached this point).
        auto unit = instr->ParentBlock()->ParentFunction()->ParentUnit();
        return OperandInfo(unit).IsPointerNotNull(loadInstr->SourceOp(), toBlock);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::DefinitionDominatesBlock(Operand* op, Block* block, bool* failed) {
    DebugValidator::IsNotNull(block);
    DebugValidator::IsNotNull(op);

    // This is intended to work even if the dominator tree is not available,
    // but then, of course, in a much more limited fashion.
    // Constants and parameters dominate all blocks.
    if(op->IsConstant() || op->IsParameter()) {
        return true;
    }

    // Variables and functions dominate all blocks too.
    if(op->IsVariableReference() || op->IsFunctionReference()) {
        return true;
    }

    // Now check for temporaries.
    if(auto temp = op->As<Temporary>()) {
        auto definingInstr = temp->DefiningInstruction();
        return Dominates(definingInstr->ParentBlock(), block, failed);
    }

	if(failed) *failed = true;
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::Dominates(Block* a, Block* b, bool* failed) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);

    // A block dominates itself.
    if(a == b) {
        return true;
    }

    // The entry b dominates all other ones.
    if(a->IsFunctionEntry()) {
        return true;
    }

    // If no dominator tree is available try to handle the common cases
    // of 'if-then' and 'if-then-else' patterns.
    if(b->HasSinglePredecessor()) {
		if(failed) *failed = false;
        return b->PredecessorAt(0) == a;
    }
    else if(b->PredecessorCount() == 2) {
        if(a == b->PredecessorAt(0)) {
            // We may have 'if-then'.
			if(failed) *failed = false;
            auto otherPred = b->PredecessorAt(1);
            return otherPred->HasSinglePredecessor() &&
                   (otherPred->PredecessorAt(0) == a);
        }
        else if(a == b->PredecessorAt(1)) {
            // We may have 'if-then'.
			if(failed) *failed = false;
            auto otherPred = b->PredecessorAt(0);
            return otherPred->HasSinglePredecessor() &&
                   (otherPred->PredecessorAt(0) == a);
        }

        // Check if we have the diamond-shape pattern that is specific for
        // 'if-then-else' (both predecessors should have the same predecessor).
        auto pred1 = b->PredecessorAt(0);
        auto pred2 = b->PredecessorAt(1);

        if((pred1->PredecessorCount() != 1) ||
           (pred2->PredecessorCount() != 1)) {
			if(failed) *failed = false;
			return false;
        }
            
		if(failed) *failed = false;
        auto pred1Pred = pred1->PredecessorAt(0);
        auto pred2Pred = pred2->PredecessorAt(0);
        return (pred1Pred == pred2Pred) && (pred1Pred == a);
    }

    // Use the Dominator Tree if it is available.
    if(client_) {
        if(auto domTree = client_->DominatorTreeRequest(a->ParentFunction())) {
			if(failed) *failed = false;
            return domTree->Dominates(a, b);
        }
    }

	if(failed) *failed = true;
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::Dominates(Instruction* a, Instruction* b, bool* failed) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);

    // If the instructions are in the same block 
    // check that 'a' is positioned before 'b'.
    if(a->ParentBlock() == b->ParentBlock()) {
        while(a) {
            if(a == b) {
                return true;
            }

            a = a->NextInstruction();
        }

		if(failed) *failed = false;
        return false;
    }

    return Dominates(a->ParentBlock(), b->ParentBlock(), failed);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::OperandsDominateBlock(Instruction* instr, Block* block, bool* failed) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(block);

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        if(DefinitionDominatesBlock(instr->GetSourceOp(i), block, failed) == false) {
            // Give up, no reason to continue.
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::CanBeSpeculatedInBlock(Instruction* instr, Block* block) {
    // An instruction can be executed speculatively in a different block
    // if it can be speculated and if the definition points of all source
    // operands dominate the block. This guarantees that all source operands 
    // have been evaluated when the block is reached.
    if(CanBeSpeculated(instr) == false) {
        return false;
    }
    
    return OperandsDominateBlock(instr, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::IsAlwaysExecuted(Block* block) {
    DebugValidator::IsNotNull(block);

    // The entry block is always executed. For other blocks
    // we try to prove they are not control dependent on any condition.
    if(block->IsFunctionEntry()) {
        return true;
    }

    // Walk up to the entry block and check if the block
    // is the continuation block in a (series of) if-then-else pattern(s).
    if(IsNotControlDependent(block)) {
        return true;
    }

    // Use the Control Dependence Graph if it is available.
    if(client_) {
        auto function = block->ParentFunction();

        if(auto graph = client_->ControlDependenceGraphRequest(function)) {
            return graph->IsControlDependent(block) == false;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::IsAlwaysExecuted(Instruction* instr) {
    DebugValidator::IsNotNull(instr);
    return IsAlwaysExecuted(instr->ParentBlock());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::IsNotControlDependent(Block* block, int level) {
    // If we reached the function entry the block is always executed.
    // If it has one or more than two predecessors we presume
    // that it is control dependent without doing other checks.
    if(block->IsFunctionEntry()) {
        return true;
    }
    else if(block->PredecessorCount() != 2) {
        return false;
    }

    // Check if the block is the continuation of a if-then
    // or if-then-else block pattern. If it is we continue
    // searching for the entry block starting with the 'if' block.
    if(level == 3) {
        return false;
    }

    auto thenBlock = block->PredecessorAt(0);
    auto elseBlock = block->PredecessorAt(1);

    if((thenBlock->PredecessorCount() == 1) &&
       (elseBlock->PredecessorCount() == 1) &&
       (thenBlock->PredecessorAt(0) == elseBlock->PredecessorAt(0))) {
        return IsNotControlDependent(thenBlock->PredecessorAt(0), level + 1);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::CallMayHaveSideEffects(CallInstr* instr, bool testIndirectCall) {
	DebugValidator::IsNotNull(instr);

	if(auto intrinsic = instr->GetIntrinsic()) {
		// Most intrinsics don't have any side effects.
		return intrinsic->IsMathIntrinsic()    ||
			   intrinsic->IsBitwiseIntrinsic() ||
			   intrinsic->Is<PrefetchIntr>();
	}
	else if(auto calledFunction = instr->GetCalledFunction()) {
		// The called function is known.
		return FunctionMayHaveSideEffects(calledFunction);
	}
	else if(testIndirectCall && client_) {
		// The call is indirect through a pointer, check if all possible
		// called functions have been found and combine the results.
		if(auto callGraph = client_->CallGraphRequest()) {
			auto callSite = callGraph->GetCallSite(instr);

			if((callSite->CallsUnknownFunctions()  ||
			    callSite->CallsExternalFunctions() ||
				callSite->CallsNodeGroup()) == false) {
				bool hasSideEffects = false;

				callSite->ForEachCalledFunction([&](Function* funct) -> bool {
					hasSideEffects = FunctionMayHaveSideEffects(funct);
					return !hasSideEffects;
				});

				return hasSideEffects;
			}
		}
	}

	// Presume it has side-effects.
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SafetyInfo::FunctionMayHaveSideEffects(Function* funct) {
	DebugValidator::IsNotNull(funct);

	// Functions that don't affect the global state and don't read/write
	// from/to the pointer parameters have no side effects.
	if(funct->IsNoState()) {
		bool hasParamAccess = false;

		funct->ForEachParameterVariable([&](Variable* param, int index) -> bool {
			hasParamAccess = param->IsRead() || param->IsNoWrite();
			return hasParamAccess == false;
		});

		return hasParamAccess == false;
	}

	if(client_) {
		if(auto languageInfo = client_->LanguageInfoRequest()) {
			return languageInfo->CallMayHaveSideEffects(funct);
		}
	}

	// Presume it has side-effects.
	return true;
}

} // namespace Analysis