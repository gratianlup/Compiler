// CFGSimplifierJumpThreading.cpp
// Copyright (c) Lup Gratian
//
// Implements the CFGSimplifier pass (jump threading methods).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "CFGSimplifier.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

bool CFGSimplifier::ThreadPhiBlocksToDestination(Block* block) {
    // Check if this optimization is enabled.
    if(THREAD_PHI_BLOCKS_TO_DESTINATION == false) {
        return false;
    }

    // Here we try to perform a simple form of jump threading.
    // If this block ends with an 'if' and we have predecessors
    // for which we can determine the target block, we can force them 
    // to jump directly to the target. This happens when we have a
    // 'phi' that has constant incoming operands.
    // B1: goto B3                         B1: goto B4  *
    // B2: goto B3                     ->  B2: goto B3                  
    // B3: t1 = phi {2, B1}, {a, B2}       B3: if a, B4, B5
    //     if t1, B4, B5                       
    PhiInstr* phiInstr;
    CmpInstrBase* cmpInstr;
    IfInstr* ifInstr;
    bool changed = false;

    // The block should be small (a 'phi', an 'if' and an optional comparison).
    if(IsThreadingCandidate(block, phiInstr, cmpInstr, ifInstr) == false) {
        return false;
    }

    // If we don't have any incoming constant there is nothing to do.
    if(phiInstr->HasSingleOperand()) {
        return false;
    }

    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        auto incomingOp = phiInstr->GetOperand(i);
        BlockReference* successor = nullptr;

        if(cmpInstr || incomingOp->IsIntConstant()) {
            successor = GetThreadingSuccessor(incomingOp, cmpInstr, ifInstr);
        }

        // Skip if the final target couldn't be determined.
        if(successor == nullptr) {
            continue;
        }

        // Thread all edges from the block that provides the constant
        // to its new target block.
        auto constOpBlock = phiInstr->GetOperandBlock(i);

        for(int j = 0; j < constOpBlock->SuccessorCount(); j++) {
            auto constOpSucc = constOpBlock->SuccessorAt(j);

            if(constOpSucc == block) {
                // Thread the edge.
                constOpBlock->ReplaceSuccessor(j, successor->Target());                    
                PatchSuccessor(successor->Target(), block, constOpBlock);
            }
        }

        // Remove the operand that was incoming. If the 'phi'
        // remains without operands (meaning that all predecessors
        // could be threaded) replace it with 'undef', the block
        // will be removed later anyway.
        phiInstr->RemoveOperand(i);
        i--;
        changed = true;
    }

    if(phiInstr->OperandCount() == 0) {
        auto unit = block->ParentFunction()->ParentUnit();
        auto type = phiInstr->ResultOp()->GetType();
        auto undef = unit->Constants().GetUndefined(type);
        phiInstr->ResultOp()->ReplaceWith(undef);
        phiInstr->RemoveFromBlock(true);
    }

    if(changed) BlockSimplified(block, 1);
    return changed;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFGSimplifier::IsThreadingCandidate(Block* block, PhiInstr*& phiInstr,
                                         CmpInstrBase*& cmpInstr,
                                         IfInstr*& ifInstr) {
    // We try to thread only if the block ends with an 'if'
    // that depends on the 'phi' instruction, or on a comparison
    // that tests the 'phi'. Any other instructions are not allowed.
    if(block->HasPhi() == false) {
        return false;
    }
    else if(block->InstructionCount() > 3) {
        return false;
    }

    // Make sure that the 'phi' is not used outside this block,
    // because we can't handle these cases (it requires the reconstruction
    // of the SSA form, something that we can't do here).
    phiInstr = block->FirstInstruction()->As<PhiInstr>();
    auto phiResult = phiInstr->ResultOp();
    
    for(int i = 0; i < phiResult->UserCount(); i++) {
        if(phiResult->GetUser(i)->ParentBlock() != block) {
            return false;
        }
    }
    
    // Check that the block ends with an 'if'.
    ifInstr = block->BranchInstruction()->As<IfInstr>();

    if(ifInstr == nullptr) {
        return false;
    }

    if(auto definingInstr = ifInstr->ConditionOp()->DefiningInstruction()) {
        if((definingInstr == phiInstr) && (block->InstructionCount() == 2)) {
            cmpInstr = nullptr;
            return true;
        }
        else if(cmpInstr = definingInstr->As<CmpInstrBase>()) {
            if(cmpInstr->RightOp()->IsConstant() &&
               // compares the 'phi' result?
               (cmpInstr->LeftOp()->DefiningInstruction() == phiInstr) &&
               // the only instruction in the block? (except 'if' and 'phi')
               (block->InstructionCount() == 3) &&
               // used only in this block? (by the 'if')
               cmpInstr->ResultOp()->HasSingleUser()) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* CFGSimplifier::GetThreadingSuccessor(Operand* op, CmpInstrBase* cmpInstr,
                                                     IfInstr* ifInstr) {
    // The successor depends on the value of the operand,
    // or on the result of the comparison, if it's the case.
    Operand* result = op;

    if(cmpInstr) result = folder_.FoldCompare(cmpInstr->GetOpcode(), op,
                                              cmpInstr->RightOp(), 
                                              cmpInstr->Order(),
                                              cmpInstr->ParentBlock());
    // It's possible that the comparison couldn't be folded
    // (it's not a constant, value-range information couldn't help).
    if(result == nullptr) {
        return nullptr;
    }
    else return result->IsZeroInt() ? ifInstr->FalseTargetOp() :
                                      ifInstr->TrueTargetOp();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::ThreadToDestination(Block* block) {
    // Check if this optimization is enabled.
    if(THREAD_BLOCKS_TO_DESTINATION == false) {
        return false;
    }

	// This implements an optimization known as Jump Threading.
	// An example in C:
	// if(a == 2) { f(); }   ->   if(a == 2) { f(); goto after_aEQ3; }
	// if(a == 3) { g(); }        else if(a == 3) { g(); }
	//                            after_aEQ3: ...
    // The implementation is very aggressive and fairly complicated,
    // the rest of the code in this file is dedicated only to it.

	// Check if all conditions hold and identify the target
	// and controlling blocks and their associated condition operands.
	if(block->SuccessorCount() > MAX_THREADING_SUCCESORS) { 
		return false;
	}

	BlockIndexList threadedBlocks;

    // Try to find the end destination for each successor.
	for(int i = 0; i < block->SuccessorCount(); i++) {
		if(auto newTargetBlock = TryJumpThreading(block, block->SuccessorAt(i), i)) {
            BlockThreaded(block, block->SuccessorAt(i), newTargetBlock, i);
			block->ReplaceSuccessor(i, newTargetBlock);
			threadedBlocks.Add(i);
		}
	}

	// If a clone of the skipped block was created it might interfere
	// with further jump threading opportunities. In many cases
	// these blocks are actually not needed and can be eliminated.
	for(int i = 0; i < threadedBlocks.Count(); i++) {
		int successorIndex = threadedBlocks[i];
		auto newTargetBlock = block->SuccessorAt(successorIndex);

		if(auto replacementBlock = TryEliminateBlock(newTargetBlock, block)) {
			BlockUtils::ReplacePhiOperandsBlock(replacementBlock, newTargetBlock, block);
			block->ReplaceSuccessor(successorIndex, replacementBlock);
			IRPrinter(block->ParentFunction()).Dump();
		}
	}

    return threadedBlocks.IsNotEmpty();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::TryEliminateBlock(Block* block, Block* newPredecessorBlock) {
	if(block->HasSinglePredecessor() &&
	   block->HasSingleSuccessor()) {
		// Delete all instructions that are dead.
		auto safetyInfo = GetSafetyInfo();
		auto instr = block->BranchInstruction()->PreviousInstruction();

		while(instr) {
			auto prevInstr = instr->PreviousInstruction();

			if(safetyInfo->IsDefinitelyDead(instr)) {
				instr->RemoveFromBlock(true /* free */);
			}

			instr = prevInstr;
		}

		// If only the branching instruction remains
		// return the unique successor, but only if there are no
		// operands already incoming from the new predecessor.
		if(block->InstructionCount() == 1) {
			auto successorBlock = block->SuccessorAt(0);

            if(HasIncomingFromBlock(successorBlock, newPredecessorBlock) == false) {
                block->BranchInstruction()->RemoveFromBlock(/* free */);
				return successorBlock;
			}
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::TryJumpThreading(Block* threadedBlock, Block* targetBlock,
                                       int targetBlockSuccessorIndex) {
    ControllingBlockList controllingBlocks;
	Operand* targetCondition;
    Block* newTargetBlock = nullptr;
    
	if(FindThreadingMembers(threadedBlock, targetBlock, 
                            controllingBlocks, targetCondition)) {
		// Check if statically it can be determined that the 'true' or 'false'
		// targets are definitely taken in the target block. 
        newTargetBlock = TryCorrelatedJumpThreading(threadedBlock, 
                                                    targetBlock,
                                                    targetCondition, 
                                                    targetBlockSuccessorIndex, 
                                                    controllingBlocks);

        // If a new target could not be determined check if it
        // can be determined without using a correlated condition
        // (uses the incoming 'phi' operands from the threaded block).
        if(newTargetBlock == nullptr) {
            newTargetBlock = TryNonCorrelatedJumpThreading(threadedBlock, 
                                                           targetBlock,
                                                           targetCondition,
                                                           controllingBlocks);
        }
	}

    return newTargetBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::TryCorrelatedJumpThreading(Block* threadedBlock, 
                                                 Block* targetBlock,
                                                 Operand* targetCondition, 
                                                 int targetBlockSuccessorIndex,
                                                 ControllingBlockList& controllingBlocks) {
    // The correlated test cannot be performed if the target block
    // ends with a 'switch' instruction.
    if(targetBlock->BranchInstruction()->IsSwitch()) {
        return nullptr;
    }

    // Check if statically it can be determined that the 'true' or 'false'
	// targets are definitely taken in the target block. 
	// If true then the block can jump directly to that target.
    Block* newTargetBlock = nullptr;

    controllingBlocks.ForEach([&, this](ControllingBlockPair& pair) -> bool {
        // The controlling block can end either with an 'if'
        // or with a 'switch'. For 'switch' only a few cases are handled.
        auto branchingInstr = pair.ControllingBlock->BranchInstruction();

        if(auto switchInstr = branchingInstr->As<SwitchInstr>()) {
            newTargetBlock = TrySwitchJumpThreading(targetCondition, switchInstr,
                                                    threadedBlock, targetBlock,
                                                    targetBlockSuccessorIndex);
        }
        else if(auto ifInstr = branchingInstr->As<IfInstr>()) {
            newTargetBlock = TryIfJumpThreading(targetCondition, ifInstr,
                                                threadedBlock, targetBlock, 
                                                pair.ControlledOnTruePath);
        }
        else DebugValidator::Unreachable();

        return newTargetBlock == nullptr;
    });

    return newTargetBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::TrySwitchJumpThreading(Operand* targetCondition, SwitchInstr* switchInstr,
                                             Block* threadedBlock, Block* targetBlock,
                                             int targetBlockSuccessorIndex) {
    // The threading is done based on the value of the operand
    // which is the 'switch' condition. If the target block is the default
    // block nothing is known about the condition and threading is not attempted.
    if(switchInstr->DefaultTargetBlock() == targetBlock) {
        return nullptr;
    }

    TrueFalseResult testResult = TrueFalseResult::Unknown;
    auto switchCase = switchInstr->GetCase(targetBlockSuccessorIndex);

    // For simplicity consider only a comparison controlling condition.
    // More tests like for the 'if' case can ba added later.
    if(auto cmpInstrBase = targetCondition->DefiningInstrAs<CmpInstrBase>()) {
        if(cmpInstrBase->IsFcmp()) {
            return nullptr;
        }

        if((cmpInstrBase->LeftOp() == switchInstr->ConditionOp()) &&
           cmpInstrBase->RightOp()->IsIntConstant()) {
            testResult = CompareWithValue(cmpInstrBase, switchCase.Value);
        }
    }

    return SelectThreadingSuccessor(testResult, threadedBlock, targetBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
CFGSimplifier::TrueFalseResult 
CFGSimplifier::CompareWithValue(CmpInstrBase* instr, __int64 value) {
    auto intConst = instr->RightOp()->As<IntConstant>();
    auto intKind = intConst->GetType()->GetSubtype();
    
    switch(instr->Order()) {
        case OrderType::Equal: {
            return IA::AreEqual(intConst->Value(), value, intKind) ? 
                   TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
        }
        case OrderType::NotEqual: {
            return IA::AreEqual(intConst->Value(), value, intKind) ? 
                   TrueFalseResult::AlwaysFalse : TrueFalseResult::AlwaysTrue;
        }
        case OrderType::LessOrEqual: {
            if(instr->IsCmp()) {
                return IA::IsSmallerOrEqual(intConst->Value(), value, intKind) ? 
                       TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
            }
            else return IA::IsSmallerOrEqualUnsigned(intConst->Value(), value, intKind) ? 
                        TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
            
        }
        case OrderType::GreaterOrEqual: {
            if(instr->IsCmp()) {
                return IA::IsLargerOrEqual(intConst->Value(), value, intKind) ? 
                       TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
            }
            else return IA::IsLargerOrEqualUnsigned(intConst->Value(), value, intKind) ? 
                        TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
        }
        case OrderType::Less: {
            if(instr->IsCmp()) {
                return IA::IsSmaller(intConst->Value(), value, intKind) ? 
                       TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
            }
            else return IA::IsSmallerUnsigned(intConst->Value(), value, intKind) ? 
                        TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
        }
        case OrderType::Greater: {
            if(instr->IsCmp()) {
                return IA::IsLarger(intConst->Value(), value, intKind) ? 
                       TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
            }
            else return IA::IsLargerUnsigned(intConst->Value(), value, intKind) ? 
                        TrueFalseResult::AlwaysTrue : TrueFalseResult::AlwaysFalse;
        }
        default: DebugValidator::Unreachable();
    }

    return TrueFalseResult::Unknown;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::TryIfJumpThreading(Operand* targetCondition, IfInstr* ifInstr,
                                         Block* threadedBlock, Block* targetBlock,
                                         bool controlledOnTruePath) {
    auto ifCondition = ifInstr->ConditionOp();

    if(controlledOnTruePath) {
        if(IsTrueCorrelatedCondition(targetCondition, ifCondition, threadedBlock)) {
		    return ThreadOverBlock(threadedBlock, targetBlock, 
								   targetBlock->SuccessorAt(0) /* new target */);
	    }
	    else if(IsFalseCorrelatedCondition(targetCondition, ifCondition, threadedBlock)) {
		    return ThreadOverBlock(threadedBlock, targetBlock, 
								   targetBlock->SuccessorAt(1) /* new target */);
	    }
        else return nullptr;
    }
    else return TryFalsePathCorrelatedJumpThreading(targetCondition, ifCondition, 
                                                    threadedBlock, targetBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::TryFalsePathCorrelatedJumpThreading(Operand* correlatedOp, 
                                                          Operand* otherOp, 
											              Block* threadedBlock, 
                                                          Block* targetBlock) {
    TrueFalseResult testResult = TrueFalseResult::Unknown;

    // For simplicity consider only a comparison controlling condition.
    // More tests like for the 'true' path can ba added later.
    if(auto cmpInstrBase = otherOp->DefiningInstrAs<CmpInstrBase>()) {
        if(cmpInstrBase->IsFcmp()) {
            return nullptr;
        }

        // Temporarily invert the comparison order to account 
        // for the 'false' path, then use the same methods
        // as for the 'true' path to do the tests.
        cmpInstrBase->InvertOrder(false /* invertOperands */);
        auto cmpResultOp = cmpInstrBase->ResultOp();

        if(IsTrueCorrelatedCondition(correlatedOp, cmpResultOp, threadedBlock)) {
            testResult = TrueFalseResult::AlwaysTrue;
        }
        else if(IsFalseCorrelatedCondition(correlatedOp, cmpResultOp, threadedBlock)) {
            testResult = TrueFalseResult::AlwaysFalse;
        }

        // Restore the original order!
        cmpInstrBase->InvertOrder(false /* invertOperands */);
    }

    return SelectThreadingSuccessor(testResult, threadedBlock, targetBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::TryNonCorrelatedJumpThreading(Block* threadedBlock, 
                                                    Block* targetBlock,
                                                    Operand* targetCondition,
                                                    ControllingBlockList& controllingBlocks) {
    // For 'switch' we're interested only if the condition operand
    // can be evaluated to a constat that is a 'case' value.
    if(targetBlock->BranchInstruction()->IsSwitch()) {
        return TryNonCorrelatedSwitchJumpThreading(threadedBlock, targetBlock, 
                                                   targetCondition, controllingBlocks);
    }

    // Check if it's certain that the condition is definitely 'false' or 'true'. 
    TrueFalseResult testResult = TrueFalseResult::Unknown;

    if(auto cmpInstrBase = targetCondition->DefiningInstrAs<CmpInstrBase>()) {
        // The most common case is a single comparison.
        if(cmpInstrBase->IsFcmp()) {
            return nullptr;
        }

        testResult = IsAlwaysTrueOrFalse(cmpInstrBase, threadedBlock,
                                         &controllingBlocks);
    }
    else {
        // Check if there are more comparisons that are
        // unified using an 'and' or 'or' operation.
        OperandList andOperands;
        OperandList orOperands;

        if(CollectAndOperands(targetCondition, andOperands)) {
            testResult = TryNonCorrelatedJumpThreadingAnd(threadedBlock, andOperands,
                                                          controllingBlocks);
        }
        else if(CollectOrOperands(targetCondition, orOperands)) {
            testResult = TryNonCorrelatedJumpThreadingOr(threadedBlock, orOperands,
                                                         controllingBlocks);
        }
    }

    return SelectThreadingSuccessor(testResult, threadedBlock, targetBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::TryNonCorrelatedSwitchJumpThreading(Block* threadedBlock, 
                                                          Block* targetBlock,
                                                          Operand* targetCondition,
                                                          ControllingBlockList& controllingBlocks) {
    // Check if the 'switch' condition is a constant when incoming
    // from the threaded block and thread to the destination. For example,
    // t1 = phi {1, B1}, {x, B2}
    // t2 = phi {2, B1}, {y, B2}
    // t3 = add t1, t2    -> evaluated as 'add 1, 2' -> 3
    // switch t3, B3 {       if control is flowing from B1
    //     1 : B4
    //     2 : B5
    //     3 : B6         -> B1 can jump directly to B6
    // }
    auto switchInstr = targetBlock->BranchInstruction()->As<SwitchInstr>();
    auto newTargetCondition = ThreadOperand(targetCondition, threadedBlock,
                                            &controllingBlocks);
    
    if(auto intConst = newTargetCondition->As<IntConstant>()) {
        auto newTargetBlock = switchInstr->GetTargetForValue(intConst->Value(),
                                                             true /* defaultIfNotFound */);
        return ThreadOverBlock(threadedBlock, targetBlock, newTargetBlock);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
CFGSimplifier::TrueFalseResult 
CFGSimplifier::TryNonCorrelatedJumpThreadingAnd(Block* threadedBlock, OperandList andOperands,
                                                ControllingBlockList& controllingBlocks) {
    TrueFalseResult testResult = TrueFalseResult::Unknown;

    andOperands.ForEach([&, this](Operand* op) -> bool {
        if(auto cmpInstrBase = op->DefiningInstrAs<CmpInstrBase>()) {
            if(cmpInstrBase->IsFcmp()) {
                testResult = TrueFalseResult::Unknown;
                return false;
            }

            // Check if the result of the comparison is definitive
            // if control is incoming from the threaded block.
            auto cmpTestResult = IsAlwaysTrueOrFalse(cmpInstrBase, threadedBlock,
                                                     &controllingBlocks);

            if(testResult == TrueFalseResult::Unknown) {
                cmpTestResult = testResult; // First time.
            }
            else if(cmpTestResult != testResult) {
                testResult = TrueFalseResult::Unknown; // Not as the previous.
                return false;
            }

            // None of the results should be unknown.
            return testResult != TrueFalseResult::Unknown;
        }

        return false;
    });

    return testResult;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
CFGSimplifier::TrueFalseResult 
CFGSimplifier::TryNonCorrelatedJumpThreadingOr(Block* threadedBlock, OperandList orOperands,
                                               ControllingBlockList& controllingBlocks) {
    TrueFalseResult testResult = TrueFalseResult::Unknown;

    orOperands.ForEach([&, this](Operand* op) -> bool {
        if(auto cmpInstrBase = op->DefiningInstrAs<CmpInstrBase>()) {
            if(cmpInstrBase->IsFcmp()) {
                testResult = TrueFalseResult::Unknown;
                return false;
            }

            // Check if the result of the comparison is definitive
            // if control is incoming from the threaded block.
            auto cmpTestResult = IsAlwaysTrueOrFalse(cmpInstrBase, threadedBlock,
                                                     &controllingBlocks);

            if(testResult == TrueFalseResult::Unknown) {
                cmpTestResult = testResult; // First time.
            }
            else if(cmpTestResult != testResult) {
                testResult = TrueFalseResult::Unknown; // Not as the previous.
                return false;
            }

            return true;
        }

        return false;
    });

    return testResult;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::SelectThreadingSuccessor(TrueFalseResult testResult, 
                                               Block* threadedBlock, Block* targetBlock) {
    if(testResult == TrueFalseResult::AlwaysTrue) {
        return ThreadOverBlock(threadedBlock, targetBlock, 
                               targetBlock->SuccessorAt(0) /* new target */);
    }
    else if(testResult == TrueFalseResult::AlwaysFalse) {
        return ThreadOverBlock(threadedBlock, targetBlock, 
                               targetBlock->SuccessorAt(1) /* new target */);
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::FindThreadingMembers(Block* threadedBlock, Block* targetBlock, 
										 ControllingBlockList& controllingBlocks,
										 Operand*& targetCondition) {
	// The following are required:
	// 1. The target block should end with an 'if' or 'switch'.
	// 2. The controlling block should end with an 'if' or 'switch'.
    if((targetBlock->BranchInstruction()->IsIf() ||
        targetBlock->BranchInstruction()->IsSwitch()) == false) {
        return false;
    }
	else targetCondition = targetBlock->BranchInstruction()->GetSourceOp(0);
	
	// Try to find the controlling block (it is the basic block that decides
	// if this block is executed or not, we're interested in the condition).
	// If it is the target block then it is a loop header and for safety reasons 
	// we ignore it (it also maintains the loop structure).
	FindControllingBlock(threadedBlock, controllingBlocks);

    controllingBlocks.RemoveAll([&](ControllingBlockPair& pair) -> bool {
        return pair.ControllingBlock == targetBlock;
    });
	
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::ThreadOverBlock(Block* threadedBlock, Block* skippedBlock,
									  Block* targetBlock) {
    // Threading cannot be done if the target block
    // already has operands incoming from the threaded block
    // (there would be a 'phi' conflict where two different operands
    //  incoming from the same block, which is invalid SSA).
    if(HasIncomingFromBlock(threadedBlock, targetBlock)) {
        return nullptr;
    }

	// Control should jump from 'threadedBlock' directly to 'targetBlock'
	// without going anymore through 'targetBlock'. There are two cases:
	// 1. The skipped block has no instruction that is used outside it
	//    or has a side effect. This case is very simple, requiring 
	//    only patching the 'phi' instructions in the target.
	// 2. Otherwise the skipped block must be cloned, control flowing like
	//    threadedBlock -> skippedBlockClone -> targetBlock
	//    If the instructions that escape the skipped block have users
	//    that are not 'phi' instruction in the target block the SSA
	//    form must be reconstructed for them.
	bool hasNonPhiUsers;

	if(NeedsThreadingClone(skippedBlock, targetBlock, hasNonPhiUsers)) {
		// Create the clone. This also patches the 'phi' instructions
		// found in 'targetBlock'. The ones found in the skipped block
		// and the sibling of the target block must be matched manually.
        auto skippedBlockClone = CloneAndLinkWithBlock(skippedBlock, targetBlock);

        // Reconstruct the SSA form for each users of the instructions
        // defined in the skipped block if required.
		if(hasNonPhiUsers) {
			ReconstructSSA(skippedBlock, skippedBlockClone, targetBlock);
		}

		BlockUtils::RemoveIncomingFrom(skippedBlock, threadedBlock);
		TryRemovePhis(skippedBlock);

#if 0
		IRPrinter(threadedBlock->ParentFunction()).Dump();
#endif

		// The clone has a single predecessor, so any 'phi' instruction
		// is actually unnecessary and can be removed now.
		BlockUtils::ReplacePhisWithIncomingFrom(skippedBlockClone, threadedBlock);
		return skippedBlockClone;
	}
	else {
		// If no clone is needed patching is much easier.
		// The only thing to take care of is that for any 'phi'
		// in 'targetBlock' the incoming operand from 'skippedBlock' 
		// should be incoming from 'block' too.
		BlockUtils::InsertSameIncoming(targetBlock, skippedBlock, threadedBlock);
        BlockUtils::RemoveIncomingFrom(skippedBlock, threadedBlock);
		return targetBlock;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Block* CFGSimplifier::CloneAndLinkWithBlock(Block* block, Block* targetBlock) {
    BlockCloner cloner;
    auto cloneBlock = cloner.CloneSingleBlock(block);

    // When the block is cloned the branching instruciton
    // is cloned too and 'phi' operands are inserted in all
    // successors that required them. These incoming operands
    // must be removed before the block will be linked to a single target.
    cloneBlock->ForEachSuccessor([&, this](Block* successorBlock, int index) -> bool {
        if(successorBlock != targetBlock) {
            BlockUtils::RemoveIncomingFrom(successorBlock, cloneBlock);
            TryRemovePhis(successorBlock);
        }

        return true;
    });

    cloneBlock->LinkWith(targetBlock);
    return cloneBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void CFGSimplifier::ReconstructSSA(Block* skippedBlock, Block* skippedBlockClone,
								   Block* targetBlock) {
	DebugValidator::AreNotEqual(skippedBlock, skippedBlockClone);
	DebugValidator::AreEqual(skippedBlock->InstructionCount(),
							 skippedBlockClone->InstructionCount());

    IRPrinter(skippedBlock->ParentFunction()).Dump();

	// Reconstruct the SSA form for each instruction defined
	// in 'skippedBlock' and used outside it.
	auto blockInstr = skippedBlock->FirstInstruction();
	auto blockCloneInstr = skippedBlockClone->FirstInstruction();

	while(blockInstr) {
		// Ignore instructions without a destination operand.
		if(blockInstr->HasDestinationOp()) {
			DebugValidator::IsTrue(blockCloneInstr->HasDestinationOp());

			ReconstructSSA(blockInstr->GetDestinationOp(), skippedBlock,
						   blockCloneInstr->GetDestinationOp(), skippedBlockClone,
						   targetBlock);
		}

		blockInstr = blockInstr->NextInstruction();
		blockCloneInstr = blockCloneInstr->NextInstruction();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void CFGSimplifier::ReconstructSSA(Temporary* skippedOp, Block* skippedBlock, 
								   Temporary* skippedOpClone, Block* skippedBlockClone,
								   Block* targetBlock) {
    SSAReconstruction ssaReconstruction;

	skippedOp->ForEachUser([&, this](Instruction* user, int index) -> bool {
		if((user->ParentBlock() != skippedBlock) &&
		   ((user->IsPhi() == false) || (user->ParentBlock() != targetBlock))) {
			// 'skippedOpClone' must be made available at the end of
            // 'skippedBlockClone' for the reconstruction helper to do its job.
            ssaReconstruction.MakeOperandIncomingFromBlock(skippedOpClone,
                                                           skippedBlockClone);
            ssaReconstruction.ReconstructUser(user, skippedOp->DefiningInstruction());
            ssaReconstruction.Reset();

            IRPrinter(skippedBlock->ParentFunction()).Dump();
		}

		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::NeedsThreadingClone(Block* block, Block* targetBlock,
										bool& hasNonPhiUsers) {
	// A clone of the block over which we thread is required
	// if it contains any instructions that are used outside the block
	// or any of the instructions may have side effects (for load/store/call).
	bool required = false;
	hasNonPhiUsers = false;

	block->ForEachInstruction([&, this](Instruction* instr) -> bool {
		if(MayHaveSideEffects(instr)) {
			required = true;
		}

		if(instr->HasDestinationOp()) {
			auto destOp = instr->GetDestinationOp();

			destOp->ForEachUser([&](Instruction* user, int index) -> bool {
				if(user->ParentBlock() != block) {
					required = true;
				}

				// If the users outside the block are only 'phi' instructions
				// found in the target block some steps can be skipped.
				if((user->ParentBlock() != block) && // Ignore users in same block.
				   ((user->ParentBlock() != targetBlock) || (user->IsPhi() == false))) {
					hasNonPhiUsers = true;
				}

				return true;;
			});
		}

		return true;
	});

	return required;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::MayHaveSideEffects(Instruction* instr) {
	if(instr->IsStore()) {
		return true;
	}
	else if(auto loadInstr = instr->As<LoadInstr>()) {
		return GetSafetyInfo()->IsSafeToEliminateLoad(loadInstr);
	}
	if(auto callInstr = instr->As<CallInstr>()) {
		return GetSafetyInfo()->CanBeSpeculated(callInstr);
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::FindControllingBlock(Block* startBlock, ControllingBlockList& blocks, int level) {
	if((level > MAX_CONTROLLING_BLOCK_LEVEL) ||
       (blocks.Count() > MAX_CONTROLLING_BLOCK_COUNT)) {
		return blocks.IsNotEmpty();
	}

	// If the start block ends with a 'switch' it is itself
    // a controlling block for the successors. 
    if(startBlock->BranchInstruction()->IsSwitch()) {
        // Make sure it wasn't added already by one of the succesors.
        if(! blocks.ContainsMatching([&](ControllingBlockPair pair) -> bool {
            return pair.ControllingBlock == startBlock;
        })) {
            blocks.Add(ControllingBlockPair(startBlock, false));
        }
    }

	if(startBlock->HasSinglePredecessor()) {
        // If the block has a single predecessor its execution
	    // is controlled by it, so add it to the list.
		auto candidateBlock = startBlock->PredecessorAt(0);

		if(candidateBlock->BranchInstruction()->IsIf() ||
           candidateBlock->BranchInstruction()->IsSwitch()) {
            // 'onTruePath' is used only for 'if'.
			bool onTruePath = candidateBlock->SuccessorAt(0) == startBlock;
		    blocks.Add(ControllingBlockPair(candidateBlock, onTruePath));
		}

        // Walk up to collect more controlling blocks.
        return FindControllingBlock(candidateBlock, blocks, level + 1);
	}
    else if(startBlock->PredecessorCount() == 2) {
	    // If the block has two predecessors it might be the continuation
	    // of an 'if-then-else' or 'if-then' construct. The block that contains
	    // the 'if' doesn't control the execution, but it's parent might.
	    //
	    //   [A]                 [A]       <- controlling block
	    //  /   \               /   \
	    // [+]  [B]           [+]   [B]    <- 'if' block, continue from here
	    //     /   \               /   \
	    //    [+]  [+]            |    [+]
	    //     \    /              \   /
	    //      [C]                 [C]     <- current block
		// Test for 'if-then' first, it's more common.
		auto predecessor0 = startBlock->PredecessorAt(0);
		auto predecessor1 = startBlock->PredecessorAt(1);

		if(predecessor0->SuccessorCount() == 2) {
			if(((predecessor0->SuccessorAt(0) == startBlock) &&
			    (predecessor0->SuccessorAt(1) == predecessor1) 
				   ||
				(predecessor0->SuccessorAt(0) == predecessor1) &&
				(predecessor0->SuccessorAt(1) == startBlock))) {
				return FindControllingBlock(predecessor0, blocks, level + 1);
			}
		}
		else if(predecessor1->SuccessorCount() == 2) {
			if(((predecessor1->SuccessorAt(0) == startBlock) &&
				(predecessor1->SuccessorAt(1) == predecessor0) 
				   ||
				(predecessor1->SuccessorAt(0) == predecessor0) &&
				(predecessor1->SuccessorAt(1) == startBlock))) {
				return FindControllingBlock(predecessor1, blocks, level + 1);
			}
		}
		
		// Test for 'if-then-else' first.
		if(predecessor0->HasSingleSuccessor()   &&
		   predecessor1->HasSingleSuccessor()   &&
		   predecessor0->HasSinglePredecessor() &&
		   predecessor1->HasSinglePredecessor()) {
			auto blockA = predecessor0->PredecessorAt(0);
			auto blockB = predecessor1->PredecessorAt(0);

			if(blockA == blockB) {
				return FindControllingBlock(blockA, blocks, level + 1);
			}
		}
	}

	//! TODO: On maximum optimization level the Control Dependence Graph 
	// could be used, but it's quite expensive to compute (maybe use it
	// only on paths that are "hot" or for frequently called functions? )
    return blocks.IsNotEmpty();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsTrueCorrelatedCondition(Operand* correlatedOp, Operand* otherOp, 
											  Block* incomingBlock, bool testAndOr) {
    // Value Numbering makes this test in many cases really simple.
    if(correlatedOp == otherOp) {
        return true;
    }

    // There are some situations in which it is clear that 'correlatedOp'
    // evaluates to 'true' if 'otherOp' evaluates to 'true'.
    // 1. Comparisons 
	auto correlatedCmpInstr = correlatedOp->DefiningInstrAs<CmpInstrBase>();
	auto otherCmpInstr = otherOp->DefiningInstrAs<CmpInstrBase>();

	if((correlatedCmpInstr && correlatedCmpInstr->IsFcmp()) ||
	   (otherCmpInstr && otherCmpInstr->IsFcmp())) {
		return false;
	}

    // After threading the source operands of the correlated
    // comparison instruction it is possible that both are constants.
    // t1 = phi {2, B1}, {x, B2}
    // t2 = phi {1, B1}, {y, B2}
    // t3 = add t1, t2
    // t4 = cmp gt t3, 0  -> evaluated as 'cmp gt 3, 0' -> 1
    // if t4, B3, B4
    // If the threaded block is 'B1' only block 'B3' can be the target.
    if(correlatedCmpInstr && IsAlwaysTrue(correlatedCmpInstr, incomingBlock)) {
        return true;
    }

    if(correlatedCmpInstr && otherCmpInstr) {
        auto correlatedRightOp = ThreadOperand(correlatedCmpInstr->RightOp(), incomingBlock);
        auto correlatedIntConst = correlatedRightOp->As<IntConstant>();
        auto otherIntConst = otherCmpInstr->RightOp()->As<IntConstant>();

        if((correlatedIntConst && otherIntConst) &&
           (otherCmpInstr->LeftOp() == correlatedCmpInstr->LeftOp())) {
            return IsTrueCorrelatedConditionConst(correlatedIntConst, otherIntConst, 
                                                  correlatedCmpInstr, otherCmpInstr);
        }
        
        return IsTrueCorrelatedConditionInt(correlatedCmpInstr, otherCmpInstr);
    }
    
    if(testAndOr) {
        return IsTrueCorrelatedConditionAndOr(correlatedOp, otherOp, incomingBlock);
    }
    else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsTrueCorrelatedConditionConst(IntConstant* correlatedIntConst,
                                                   IntConstant* otherIntConst,
                                                   CmpInstrBase* correlatedCmpInstr, 
                                                   CmpInstrBase* otherCmpInstr) {
	// a == 5 -> a == 5 true
	// This case usually doesn't appear because of Value Numbering.
	if(otherCmpInstr->IsEqual() && correlatedCmpInstr->IsEqual()) {
		return correlatedIntConst == otherIntConst;
	}

	// a == 6 -> a != 5 true
	if(otherCmpInstr->IsNotEqual() && correlatedCmpInstr->IsEqual()) {
		return IA::AreNotEqual(correlatedIntConst, otherIntConst);
	}

    // a > 5 -> a >= 5 true, a > 4 true, a != 0 true
    if(otherCmpInstr->IsGreater() && (correlatedCmpInstr->IsGreaterOrEqual() ||
                                      correlatedCmpInstr->IsGreater()        ||
                                      correlatedCmpInstr->IsNotEqual())) {
        return correlatedCmpInstr->IsUcmp() ? 
			   IA::IsSmallerOrEqualUnsigned(correlatedIntConst, otherIntConst) : 
               IA::IsSmallerOrEqual(correlatedIntConst, otherIntConst);
    }
                                                      
    // a < 4 -> a <= 4 true, a < 5 true, a != 0 true
    if(otherCmpInstr->IsLess() && (correlatedCmpInstr->IsLessOrEqual() ||
                                   correlatedCmpInstr->IsLess()        ||
                                   correlatedCmpInstr->IsNotEqual())) {
        return correlatedCmpInstr->IsUcmp() ?
			   IA::IsLargerOrEqualUnsigned(correlatedIntConst, otherIntConst) : 
               IA::IsLargerOrEqual(correlatedIntConst, otherIntConst);
    }

    // a >= 5 -> a > 4 true, a != 0 true iff C positive
    if(otherCmpInstr->IsGreaterOrEqual() && (correlatedCmpInstr->IsGreater() ||
                                             correlatedCmpInstr->IsNotEqual())) {
        return correlatedCmpInstr->IsUcmp() ? 
			   IA::IsSmallerUnsigned(correlatedIntConst, otherIntConst) : 
               IA::IsSmaller(correlatedIntConst, otherIntConst);
    }

    // a <= 4 -> a <= 5 true, a != 0 true iff C is negative
    if(otherCmpInstr->IsGreaterOrEqual() && (correlatedCmpInstr->IsGreater() ||
                                             correlatedCmpInstr->IsNotEqual())) {
        return correlatedCmpInstr->IsUcmp() ? 
			   IA::IsLargerUnsigned(correlatedIntConst, otherIntConst) : 
               IA::IsLarger(correlatedIntConst, otherIntConst);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsTrueCorrelatedConditionInt(CmpInstrBase* correlatedCmpInstr, 
                                                 CmpInstrBase* otherCmpInstr) {
    // a </> b -> a != b true
    if((otherCmpInstr->IsLess() || otherCmpInstr->IsGreater()) &&
        correlatedCmpInstr->IsNotEqual()) {
        return SameEqualityOperands(correlatedCmpInstr, otherCmpInstr);
    }
    // a ==/!= b-> a ==/!= b true
    else if(otherCmpInstr->IsEquality() && 
       correlatedCmpInstr->HasSameOrder(otherCmpInstr)) {
        return SameEqualityOperands(correlatedCmpInstr, otherCmpInstr);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsTrueCorrelatedConditionAndOr(Operand* correlatedOp, Operand* otherOp,
                                                   Block* incomingBlock) {
    // The condition might hold if it is found inside the other one.
    // For example, 'a' holds if the other condition is 'a && b && c'.
    // Collect the operands on which AND is applied from each condition
    // and verify if OPS(correlatedOp) is included in OPS(otherOp).
    OperandList correlatedAndOperands;
    OperandList otherAndOperands;

    if(CollectAndOperands(correlatedOp, correlatedAndOperands) &&
       CollectAndOperands(otherOp, otherAndOperands)) {
        return IsTrueCorrelatedConditionAndAnd(correlatedAndOperands, 
                                               otherAndOperands, incomingBlock);   
    }

    // Another case involves the OR operation. For example,
    // condition 'a || b || c' holds if conditions 'a', 'a && b' or 'a || b' hold.
    OperandList correlatedOrOperands;

    if(CollectOrOperands(correlatedOp, correlatedOrOperands)) {
        // Test the case where the other condition is of the form
        // 'a && b && c...'. In this case it is enough that one of the operands
        // appears in the condition for it to be correlated.
        OperandList otherAndOperands;

        if(CollectAndOperands(otherOp, otherAndOperands)) {
            if(IsTrueCorrelatedConditionOrAnd(correlatedOrOperands, 
                                              otherAndOperands, incomingBlock)) {
                return true;
            }
        }
        
        // The other form is 'a || b || c...'. In this case all operands
        // must appear in the other condition for the condition to be correlated.
        OperandList otherOrOperands;

        if(CollectOrOperands(otherOp, otherOrOperands)) {
            if(IsTrueCorrelatedConditionOrOr(correlatedOrOperands, 
                                             otherOrOperands, incomingBlock)) {
				return true;
			}
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsTrueCorrelatedConditionAndAnd(OperandList& correlatedAndOperands,
                                                    OperandList& otherAndOperands,
                                                    Block* incomingBlock) {
    bool correlated = true;

    if(UseCorrelationTest()) {
        // For higher optimization levels use more complex tests that handle
        // cases like 'a > 4 && b > 5' true if 'a > 6 && b > 8' is true.
        correlatedAndOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
            correlated = otherAndOperands.ContainsMatching([&, this](Operand* otherOp)-> bool {
                return IsTrueCorrelatedCondition(correlatedOp, otherOp, 
                                                 incomingBlock, false /* testAndOr */);
            });

            return correlated;
        });
    }
    else {
        // For lower optimization levels use exact matching.
        correlatedAndOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
            correlated = otherAndOperands.Contains(correlatedOp);
            return correlated;
        });
    }

    return correlated;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsTrueCorrelatedConditionOrAnd(OperandList& correlatedOrOperands,
                                                   OperandList& otherAndOperands,
                                                   Block* incomingBlock) {
    bool correlated = false;

    if(UseCorrelationTest()) {
        // For higher optimization levels use more complex tests that handle
        // cases like 'a > 4 || b < 3' true if 'a > 6 && b > 8' is true.
        correlatedOrOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
            correlated = otherAndOperands.ContainsMatching([&, this](Operand* otherOp)-> bool {
                return IsTrueCorrelatedCondition(correlatedOp, otherOp, 
                                                 incomingBlock, false /* testAndOr */);
            });

            return correlated == false;
        });
    }
    else {
        // For lower optimization levels use exact matching.
        correlatedOrOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
            correlated = otherAndOperands.Contains(correlatedOp);
            return correlated == false;
        });
    }

    return correlated;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsTrueCorrelatedConditionOrOr(OperandList& correlatedOrOperands,
                                                  OperandList& otherOrOperands,
                                                  Block* incomingBlock) {
    bool correlated = true;

    if(UseCorrelationTest()) {
        // For higher optimization levels use more complex tests that handle
        // cases like 'a > 4 || b > 3' true if 'a > 6 || b > 8' is true.
        correlatedOrOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
            correlated = otherOrOperands.ContainsMatching([&, this](Operand* otherOp)-> bool {
                return IsTrueCorrelatedCondition(correlatedOp, otherOp,
                                                 incomingBlock, false /* testAndOr */);
            });

            return correlated;
        });
    }
    else {
        // For lower optimization levels use exact matching.
        correlatedOrOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
            correlated = otherOrOperands.Contains(correlatedOp);
            return correlated;
        });
    }

    return correlated;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsFalseCorrelatedCondition(Operand* correlatedOp, Operand* otherOp, 
											   Block* incomingBlock, bool testAndOr) {
    if(correlatedOp == otherOp) {
        return false;
    }

    // There are some situations in which it is clear that 'correlatedOp'
    // evaluates to 'false' if 'otherOp' evaluates to 'true'.
    // 1. Comparisons 
	auto correlatedCmpInstr = correlatedOp->DefiningInstrAs<CmpInstrBase>();
	auto otherCmpInstr = otherOp->DefiningInstrAs<CmpInstrBase>();

	if((correlatedCmpInstr && correlatedCmpInstr->IsFcmp()) ||
	   (otherCmpInstr && otherCmpInstr->IsFcmp())) {
		return false;
	}

    // After threading the source operands of the correlated
    // comparison instruction it is possible that both are constants.
    // t1 = phi {2, B1}, {x, B2}
    // t2 = phi {1, B1}, {y, B2}
    // t3 = add t1, t2
    // t4 = cmp lt t3, 0  -> evaluated as 'cmp lt 3, 0' -> 0
    // if t4, B3, B4
    // If the threaded block is 'B1' only block 'B4' can be the target.
    if(correlatedCmpInstr && IsAlwaysFalse(correlatedCmpInstr, incomingBlock)) {
        return true;
    }

	if(correlatedCmpInstr && otherCmpInstr) {
        // 1.1 Pointer comparisons
        // a == nullptr -> a != nullptr false
        // a != nullptr -> a == nullptr false
        if(otherCmpInstr->IsUcmp() && correlatedCmpInstr->IsUcmp()) {
            if(otherCmpInstr->IsEqual() && correlatedCmpInstr->IsNotEqual() &&
               otherCmpInstr->RightOp()->IsNullConstant() &&
               correlatedCmpInstr->RightOp()->IsNullConstant()) {
                return correlatedCmpInstr->LeftOp() == otherCmpInstr->LeftOp();
            }
            else if(otherCmpInstr->IsNotEqual() && correlatedCmpInstr->IsEqual() &&
                    otherCmpInstr->RightOp()->IsNullConstant() &&
                    correlatedCmpInstr->RightOp()->IsNullConstant()) {
                return correlatedCmpInstr->LeftOp() == otherCmpInstr->LeftOp();
            }
        }

        // 1.2 Constant comparisons
        auto correlatedAndOp = ThreadOperand(correlatedCmpInstr->RightOp(), incomingBlock);
        auto correlatedIntConst = correlatedAndOp->As<IntConstant>();
        auto otherIntConst = otherCmpInstr->RightOp()->As<IntConstant>();

        if((correlatedIntConst && otherIntConst) &&
           (otherCmpInstr->LeftOp() == correlatedCmpInstr->LeftOp())) {
            return IsFalseCorrelatedConditionConst(correlatedIntConst, otherIntConst, 
                                                   correlatedCmpInstr, otherCmpInstr);
        }
        
        return IsFalseCorrelatedConditionInt(correlatedCmpInstr, otherCmpInstr,
                                             incomingBlock);
	}

    // a -> a == 0 false
    if(otherCmpInstr && otherCmpInstr->IsEquality() &&
       otherCmpInstr->RightOp()->IsZeroInt() &&
       (otherCmpInstr->LeftOp() == correlatedOp)) {
        return true;
    }
    
    if(testAndOr) {
        return IsFalseCorrelatedConditionAndOr(correlatedOp, otherOp, incomingBlock);
    }
    else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsFalseCorrelatedConditionConst(IntConstant* correlatedIntConst,
                                                    IntConstant* otherIntConst,
                                                    CmpInstrBase* correlatedCmpInstr, 
                                                    CmpInstrBase* otherCmpInstr) {
    // a == C1 -> a !=/</> C1 false
    if(otherCmpInstr->IsEqual() && (correlatedCmpInstr->IsNotEqual() ||
									correlatedCmpInstr->IsLess()     ||
									correlatedCmpInstr->IsGreater())) {
        return correlatedIntConst == otherIntConst;
    }

    // a == C1 -> a == C2 false
    if(otherCmpInstr->IsEqual() && correlatedCmpInstr->IsEqual()) {
        return correlatedIntConst != otherIntConst;
    }

    // a < 5 -> a >/>=/== 5 false, a >/>=/== 7 false
    if(otherCmpInstr->IsLess() && (correlatedCmpInstr->IsGreater() ||
								   correlatedCmpInstr->IsGreaterOrEqual() ||
                                   correlatedCmpInstr->IsEqual())) {
        return correlatedCmpInstr->IsUcmp() ? 
			   IA::IsLargerOrEqualUnsigned(correlatedIntConst, otherIntConst) :
               IA::IsLargerOrEqual(correlatedIntConst, otherIntConst);
    }

    // a > 5 -> a </<=/== 5 false, a </<=/== 3 false
    if(otherCmpInstr->IsGreater() && (correlatedCmpInstr->IsLess()        ||
                                      correlatedCmpInstr->IsLessOrEqual() ||
                                      correlatedCmpInstr->IsEqual())) {
        return correlatedCmpInstr->IsUcmp() ? 
			   IA::IsSmallerOrEqualUnsigned(correlatedIntConst, otherIntConst) :
               IA::IsSmallerOrEqual(correlatedIntConst, otherIntConst);
    }

    // a <= 5 -> a > 5 false, a > 7 false
    if(otherCmpInstr->IsLessOrEqual() && correlatedCmpInstr->IsGreater()) {
        return correlatedCmpInstr->IsUcmp() ? 
			   IA::IsLargerOrEqualUnsigned(correlatedIntConst, otherIntConst) :
               IA::IsLargerOrEqual(correlatedIntConst, otherIntConst);
    }

    // a >= 5 -> a < 5 false, a < 3 false
    if(otherCmpInstr->IsGreaterOrEqual() && correlatedCmpInstr->IsLess()) {
        return correlatedCmpInstr->IsUcmp() ? 
			   IA::IsSmallerOrEqualUnsigned(correlatedIntConst, otherIntConst) :
               IA::IsSmallerOrEqual(correlatedIntConst, otherIntConst);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsFalseCorrelatedConditionInt(CmpInstrBase* correlatedCmpInstr, 
                                                  CmpInstrBase* otherCmpInstr,
                                                  Block* incomingBlock) {
    // a == b -> a !=/</> b false
	if(otherCmpInstr->IsEqual() && (correlatedCmpInstr->IsNotEqual() || 
									correlatedCmpInstr->IsLess()     || 
									correlatedCmpInstr->IsGreater())) {
		return correlatedCmpInstr->IsNotEqual() ?
			   SameEqualityOperands(otherCmpInstr, correlatedCmpInstr) :
			   SameOperands(otherCmpInstr, correlatedCmpInstr);
	}

    // a != b -> a == b false
    if(otherCmpInstr->IsNotEqual() && correlatedCmpInstr->IsEqual()) {
        return SameEqualityOperands(otherCmpInstr, correlatedCmpInstr);
    }

	// a < b -> a >/== b false
    if(otherCmpInstr->IsLess() && (correlatedCmpInstr->IsGreater() ||
								   correlatedCmpInstr->IsEqual())) {
        return correlatedCmpInstr->IsEqual() ?
			   SameEqualityOperands(otherCmpInstr, correlatedCmpInstr) :
			   SameOperands(otherCmpInstr, correlatedCmpInstr);
    }

    // a > b -> a </== b false
    if(otherCmpInstr->IsGreater() && (correlatedCmpInstr->IsLess() ||
									  correlatedCmpInstr->IsEqual())) {
        return correlatedCmpInstr->IsEqual() ?
               SameEqualityOperands(correlatedCmpInstr, otherCmpInstr) :
			   SameOperands(correlatedCmpInstr, otherCmpInstr);
    }
    
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsFalseCorrelatedConditionAndOr(Operand* correlatedOp, Operand* otherOp,
                                                    Block* incomingBlock) {
    // The condition might hold if it is found inside the other one.
    // 1. a && b -> !a || !b false (a AND b should appear)
    //    a == 5 && b == 3 -> a != 5 || b != 3 false
    {
        OperandList correlatedOrOperands;
        OperandList otherAndOperands;

        if(CollectOrOperands(correlatedOp, correlatedOrOperands) &&
           CollectAndOperands(otherOp, otherAndOperands)) {
            return IsFalseCorrelatedConditionOrAnd(correlatedOrOperands, 
                                                   otherAndOperands, incomingBlock);
        }
    }

    // 2. a && b -> !a && !b false (a OR b should appear)
    //    a == 5 && b == 3 -> a != 5 && b != 3 false
    {
        OperandList correlatedAndOperands;
        OperandList otherAndOperands;

        if(CollectAndOperands(correlatedOp, correlatedAndOperands) &&
           CollectAndOperands(otherOp, otherAndOperands)) {
            return IsFalseCorrelatedConditionAndAnd(correlatedAndOperands, 
                                                    otherAndOperands, incomingBlock);
        }
    }

    // 3. a || b -> !a && !b false (a OR b should appear)
    //    a == 5 || b == 3 -> a != 5 && b != 3 false
    {
        OperandList correlatedAndOperands;
        OperandList otherOrOperands;

        if(CollectAndOperands(correlatedOp, correlatedAndOperands) &&
           CollectOrOperands(otherOp, otherOrOperands)) {
            return IsFalseCorrelatedConditionAndOr(correlatedAndOperands, 
                                                   otherOrOperands, incomingBlock);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsFalseCorrelatedConditionOrAnd(OperandList& correlatedOrOperands, 
                                                    OperandList& otherAndOperands,
                                                    Block* incomingBlock) {
    // a && b -> !a || !b false (a AND b should appear)
    // a == 5 && b == 3 -> a != 5 || b != 3 false
    bool correlated = true;

    if(UseCorrelationTest()) {
        // For higher optimization levels use more complex tests that handle
        // cases like 'a != 4 || b < 3' false if 'a == 5 && b == 3' is true.
		correlatedOrOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
			correlated = otherAndOperands.ContainsMatching([&, this](Operand* otherOp) -> bool {
				return IsFalseCorrelatedCondition(correlatedOp, otherOp, 
												  incomingBlock, false /* testAndOr */);
			});

			return correlated;
		});
    }
    else {
        // For lower optimization levels consider only simple comparisons.
		correlatedOrOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
			auto correlatedCmpInstr = correlatedOp->DefiningInstrAs<CmpInstrBase>();

			if(correlatedCmpInstr == nullptr) {
				return false;
			}

			correlated = otherAndOperands.ContainsMatching([&, this](Operand* otherOp) -> bool {
				auto otherCmpInstr = otherOp->DefiningInstrAs<CmpInstrBase>();
				return otherCmpInstr &&
					   IsNegatedOrder(otherCmpInstr, correlatedCmpInstr) &&
					   SameOperands(otherCmpInstr, correlatedCmpInstr);
			});

			return correlated;
		});
    }

    return correlated;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsFalseCorrelatedConditionAndAnd(OperandList& correlatedAndOperands, 
                                                     OperandList& otherAndOperands,
                                                     Block* incomingBlock) {
    // a && b -> !a && !b false (a OR b should appear)
    // a == 5 && b == 3 -> a != 5 && b != 3 false
    bool correlated = false;

    if(UseCorrelationTest()) {
        // For higher optimization levels use more complex tests that handle
        // cases like 'a < 4 && b < 3' false if 'a == 5' is true.
		correlatedAndOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
			correlated = otherAndOperands.ContainsMatching([&, this](Operand* otherOp) -> bool {
				return IsFalseCorrelatedCondition(correlatedOp, otherOp,
												  incomingBlock, false /* testAndOr */);
			});

			return correlated == false;
		});
    }
    else {
        // For lower optimization levels consider only simple comparisons.
        correlatedAndOperands.ForEach([&, this](Operand* correlatedOp) -> bool {
			auto correlatedCmpInstr = correlatedOp->DefiningInstrAs<CmpInstrBase>();

			if(correlatedCmpInstr == nullptr) {
				return false;
			}

			correlated = otherAndOperands.ContainsMatching([&, this](Operand* otherOp) -> bool {
				auto otherCmpInstr = otherOp->DefiningInstrAs<CmpInstrBase>();
				return otherCmpInstr &&
					   IsNegatedOrder(otherCmpInstr, correlatedCmpInstr) &&
					   SameOperands(otherCmpInstr, correlatedCmpInstr);
			});

			return correlated == false;
		});
    }

    return correlated;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsFalseCorrelatedConditionAndOr(OperandList& correlatedAndOperands, 
                                                    OperandList& otherOrOperands,
                                                    Block* incomingBlock) {
	// a || b -> !a && !b false (a AND b should appear)
	// a == 5 || b == 3 -> a != 5 && b != 3 false
    bool correlated = true;

    if(UseCorrelationTest()) {
        // For higher optimization levels use more complex tests that handle
        // cases like 'a != 4 && b < 3' false if 'a == 5 || b == 3' is true.
        otherOrOperands.ForEach([&, this](Operand* otherOp) -> bool {
            correlated = correlatedAndOperands.ContainsMatching([&, this]
                                               (Operand* correlatedOp)-> bool {
                return IsFalseCorrelatedCondition(correlatedOp, otherOp,
                                                  incomingBlock, false /* testAndOr */);
            });
            
			return correlated;
        });
    }
    else {
		// For lower optimization levels consider only simple comparisons.
		otherOrOperands.ForEach([&, this](Operand* otherOp) -> bool {
			auto otherCmpOp = otherOp->DefiningInstrAs<CmpInstrBase>();

			if(otherCmpOp == nullptr) {
				return false;
			}

			correlated = correlatedAndOperands.ContainsMatching([&, this]
                                               (Operand* correlatedOp) -> bool {
				auto correlatedCmpInstr = correlatedOp->DefiningInstrAs<CmpInstrBase>();
				return correlatedCmpInstr &&
					   IsNegatedOrder(correlatedCmpInstr, otherCmpOp) &&
					   SameOperands(correlatedCmpInstr, otherCmpOp);
			});

			return correlated;
		});
    }

    return correlated;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::CollectAndOperands(Operand* op, OperandList& andOperands) {
    // Walk the chain of 'and' instructions and collect all operands
    // that are not defined by 'and' instructions themselves.
    // For 'a && b && (c || d)' the operands are 'a', 'b' and 'c || d'.
    if(auto andInstr = op->DefiningInstrAs<AndInstr>()) {
        return CollectAndOperands(andInstr->LeftOp(), andOperands) &&
               CollectAndOperands(andInstr->RightOp(), andOperands);
    }
    else if(op->DefiningInstrIs<OrInstr>() == false) {
        if(andOperands.Count() < MAX_AND_OPERANDS) {
            andOperands.Add(op);
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::CollectOrOperands(Operand* op, OperandList& orOperands) {
    // Walk the chain of 'and' instructions and collect all operands
    // that are not defined by 'or' instructions themselves.
    // For 'a || b || (c && d)' the operands are 'a', 'b' and 'c && d'.
    if(auto orInstr = op->DefiningInstrAs<OrInstr>()) {
        return CollectOrOperands(orInstr->LeftOp(), orOperands) &&
               CollectOrOperands(orInstr->RightOp(), orOperands);
    }
    else if(op->DefiningInstrIs<AndInstr>() == false) {
        if(orOperands.Count() < MAX_AND_OPERANDS) {
            orOperands.Add(op);
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* CFGSimplifier::ThreadOperand(Operand* op, Block* incomingBlock, 
                                      ControllingBlockList* controllingBlocks, int level) {
    // Constants and references don't need to be threaded.
    // Don't thread also when a maximum level of recursion has been reached.
    if(op->IsConstant() || op->IsReference() ||
       (level > MAX_OPERAND_THREADING_LEVEL)) {
        return op;
    }

    if(auto definingInstr = op->DefiningInstruction()) {
        if(auto phiInstr = definingInstr->As<PhiInstr>()) {
            // t1 = phi {1, B1}, {x, B2}
            // If the incoming block is 'B1' -> t1 = 1.
            if(auto incomingOp = phiInstr->GetOperandFromBlock(incomingBlock)) {
                return incomingOp;
            }
        }
        else if(auto arithmeticInstr = definingInstr->As<ArithmeticInstr>()) {
            if(auto result = ThreadArithmetic(arithmeticInstr, incomingBlock, 
                                              controllingBlocks, level)) {
                return result;
            }
        }
        else if(auto questInstr = definingInstr->As<QuestionInstr>()) {
            if(auto result = ThreadQuestion(questInstr, incomingBlock, 
                                            controllingBlocks, level)) {
                return result;
            }
        }
    }

    return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* CFGSimplifier::ThreadArithmetic(ArithmeticInstr* arithInstr, Block* incomingBlock,
                                         ControllingBlockList* controllingBlocks, int level) {
    // t1 = phi {1, B1}, {x, B2}
    // t2 = phi {3, B1}, {y, B2}
    // t3 = add t1, t2
    // If the incoming block is 'B1' -> t1 = 1, t2 = 3
    // and t3 can be evaluated as being 4.
    auto newLeftOp = ThreadOperand(arithInstr->LeftOp(), incomingBlock, 
                                   controllingBlocks, level + 1);
    auto newRightOp = ThreadOperand(arithInstr->RightOp(), incomingBlock, 
                                    controllingBlocks, level + 1);

    if((newLeftOp != arithInstr->LeftOp()) ||
       (newRightOp != arithInstr->RightOp())) {
        // Try to constant-fold the new operands
        // and return the resulting constant if possible.
        IRGenerator irGen(incomingBlock->ParentFunction()->ParentUnit());
        ConstantFolder folder(&irGen, GetTarget());

        if(auto result = folder.FoldBinary(arithInstr->GetOpcode(),
                                           newLeftOp, newRightOp, 
                                           arithInstr->ParentBlock())) {
            return result;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* CFGSimplifier::ThreadQuestion(QuestionInstr* questInstr, Block* incomingBlock, 
                                       ControllingBlockList* controllingBlocks, int level) {
    // There are two cases handled here:
    // 1. The condition operand can be threaded and turned
    //    into a constant so that selected operand is known.
    //    t1 = phi {1, B1}, {x, B2}
    //    t2 = quest t1, 4, 5
    //    If the incoming block is 'B1' -> t1 = 1, t2 = 4
    auto newConditionOp = ThreadOperand(questInstr->ConditionOp(), incomingBlock,
                                        controllingBlocks);
    if(newConditionOp->IsIntConstant()) {
        return ThreadQuestionOperand(questInstr, newConditionOp->IsZeroInt(),
                                     incomingBlock, controllingBlocks, level);
    }

    // 2. The condition operand is the same as the condition
    //    that controlls the execution of the incoming block.
    //
    //    For example, if the incoming block is on the 'true' path
    //    of one of the controlling blocks and the 'quest' condition
    //    is the same as the one in the controlling block, then the 
    //    'true' operand will definitely be selected.
    if(controllingBlocks &&  controllingBlocks->IsNotEmpty()) {
        bool found = false;
        bool onTruePath = false;

        controllingBlocks->ForEach([&, this](ControllingBlockPair& pair) -> bool {
            auto ifInstr = pair.ControllingBlock->BranchInstruction()->As<IfInstr>();
            found = ifInstr && (ifInstr->ConditionOp() == questInstr->ConditionOp());
            onTruePath = pair.ControlledOnTruePath;
            return found == false;
        });

        if(found) {
            return ThreadQuestionOperand(questInstr, onTruePath == false,
                                         incomingBlock, controllingBlocks, level);
        }
    }

    // This is a particulare case for 2, appears sometimes
    // after other optimization run before.
    auto ifInstr = incomingBlock->BranchInstruction()->As<IfInstr>();

    if(ifInstr && (ifInstr->ConditionOp() == questInstr->ConditionOp())) {
        bool selectTrueOp = ifInstr->TrueTargetBlock() == questInstr->ParentBlock();
        return ThreadQuestionOperand(questInstr, selectTrueOp,
                                     incomingBlock, controllingBlocks, level);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* CFGSimplifier::ThreadQuestionOperand(QuestionInstr* questInstr, 
                                              bool threadFalseOp, Block* incomingBlock, 
                                              ControllingBlockList* controllingBlocks, int level) {
    auto selectedOp = threadFalseOp ? questInstr->FalseOp() : questInstr->TrueOp();
    return ThreadOperand(selectedOp, incomingBlock, controllingBlocks, level + 1);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
CFGSimplifier::TrueFalseResult 
CFGSimplifier::IsAlwaysTrueOrFalse(CmpInstrBase* cmpInstr, Block* incomingBlock,
                                   ControllingBlockList* controllingBlocks) {
    // After threading the operands of the compare it's possible
    // that they both become constants and the result is always known:
    // t1 = phi {2, B1}, {x, B2}
    // t2 = phi {1, B1}, {y, B2}
    // t3 = add t1, t2
    // t4 = cmp gt t3, 0  -> evaluated as 'cmp gt 3, 0' -> 1
    // 't4' always true if the incoming block is 'B1'.
    auto newLeftOp = ThreadOperand(cmpInstr->LeftOp(), incomingBlock, controllingBlocks);
    auto newRightOp = ThreadOperand(cmpInstr->RightOp(), incomingBlock, controllingBlocks);

    if((newLeftOp != cmpInstr->LeftOp()) ||
       (newRightOp != cmpInstr->RightOp())) {
        if(newLeftOp->IsIntConstant() && newRightOp->IsIntConstant()) {
            // Constant-folding definitely possible.
            IRGenerator irGen(incomingBlock->ParentUnit());
            ConstantFolder folder(&irGen, GetTarget());

            auto result = folder.FoldCompare(cmpInstr->GetOpcode(), newLeftOp, newRightOp, 
                                             cmpInstr->Order(), cmpInstr->ParentBlock());
            DebugValidator::IsNotNull(result);
            return result->IsOneInt() ? TrueFalseResult::AlwaysTrue :
                                        TrueFalseResult::AlwaysFalse;
        }
        else if(newRightOp->IsNullConstant()) {
            // Handle the comparison of a pointer with 'nullptr',
            // in many cases it's obvious it is not null.
            if(cmpInstr->IsEquality()) {
                // cmp neq p, nullptr
                //    true if 'p' definitely not null
                //    false if 'p' definitely null,
                // 
                // cmp eq p, nullptr
                //    true if 'p' definitely null,
                //    false if 'p' definitely not null
                OperandInfo opInfo(incomingBlock->ParentUnit(), GetTarget());

                if(opInfo.IsPointerNotNull(newLeftOp, cmpInstr->ParentBlock())) {
                    return cmpInstr->IsEqual() ? TrueFalseResult::AlwaysFalse :
                                                 TrueFalseResult::AlwaysTrue;
                }
                else if(opInfo.IsPointerNull(newLeftOp, cmpInstr->ParentBlock())) {
                    return cmpInstr->IsEqual() ? TrueFalseResult::AlwaysTrue :
                                                 TrueFalseResult::AlwaysFalse;
                }
            }
        }
    }

    return TrueFalseResult::Unknown;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::IsNegatedOrder(OrderType order, OrderType negatedOrder) {
    switch(order) {
        case OrderType::Equal:          return negatedOrder == OrderType::NotEqual;
        case OrderType::NotEqual:       return negatedOrder == OrderType::Equal;
        case OrderType::Less:           return negatedOrder == OrderType::GreaterOrEqual;
        case OrderType::LessOrEqual:    return negatedOrder == OrderType::Greater;
        case OrderType::Greater:        return negatedOrder == OrderType::LessOrEqual;
        case OrderType::GreaterOrEqual: return negatedOrder == OrderType::Less;
        default: DebugValidator::Unreachable();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::SameEqualityOperands(CmpInstrBase* cmpInstrA, 
                                         CmpInstrBase* cmpInstrB) {
    // 'a ==/!= b' and 'b ==/!= a' are the same.
    return ((cmpInstrB->LeftOp() == cmpInstrA->LeftOp()) &&
            (cmpInstrB->RightOp() == cmpInstrB->RightOp())) 
              ||
		   ((cmpInstrB->RightOp() == cmpInstrA->LeftOp()) &&
			(cmpInstrB->LeftOp() == cmpInstrB->RightOp()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::SameOperands(CmpInstrBase* cmpInstrA, 
                                 CmpInstrBase* cmpInstrB) {
    return (cmpInstrB->LeftOp() == cmpInstrA->LeftOp()) &&
           (cmpInstrB->RightOp() == cmpInstrB->RightOp());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool CFGSimplifier::ThreadOverSameConditionBlock(Block* block) {
	// Check if this optimization is enabled.
	if(THREAD_OVER_SAME_CONDITION_BLOCK == false) {
		return false;
	}

	// There are cases where the same condition is tested
	// on the 'false' branch of an 'if'. Because the condition
	// is the same it's obvious that the new 'false' branch is going
	// to be taken, so the first 'if' could jump directly to it.
	//     if sameCond, TB1, FB1    ->   if cond, TB1, FB2
	// label falseBlock
	//     if sameCond, TB2, FB2
	if(block->InstructionCount() == 0) {
		return false;
	}

	auto ifInstr = block->BranchInstruction()->As<IfInstr>();

	if(ifInstr == nullptr) {
		return false;
	}

	// The block should contain only an 'if' instruction
	// branching on the same condition.
	auto falseSuccBlock = ifInstr->FalseTargetBlock();

	if(falseSuccBlock->InstructionCount() != 1) {
		return false;
	}

	auto falseIfinstr = falseSuccBlock->BranchInstruction()->As<IfInstr>();

	if((falseIfinstr == nullptr) ||
	   (falseIfinstr->ConditionOp() != ifInstr->ConditionOp()) &&
	   (falseIfinstr->TrueTargetOp() != falseIfinstr->FalseTargetOp())) {
		return false;
	}

	// The new false target should not have 'block' already as a predecessor
	// or at least is should not contain 'phi' instructions.
	auto newFalseTarget = falseIfinstr->FalseTargetBlock();

	if((newFalseTarget->HasPredecessor(block) == false) ||
	   (newFalseTarget->HasPhi() == false)) {
		BlockUtils::RemoveIncomingFrom(newFalseTarget, falseSuccBlock);
		ifInstr->SetFalseTargetOp(newFalseTarget->GetReference());
		return true;
	}

	return false;
}

} // namespace Optimization