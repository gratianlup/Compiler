// ConstantEstimator.cpp
// Copyright (c) Lup Gratian
//
// Implements the ConstantEstimator class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantEstimator.hpp"

namespace Analysis {

ConstantEstimationResult ConstantEstimator::Estimate(Block* block, Block* incomingBlock) {
    DebugValidator::IsNotNull(block);
    DebugValidator::AreNotEqual(block, incomingBlock);

    // Initialize and replace the 'phi' instructions
    // with the incoming operands if requested.
    Initialize(block->ParentUnit());
    totalInstrs_ = block->InstructionCount();

    if(incomingBlock) {
        ReplacePhisWithIncoming(block, incomingBlock);
        phiInstrs_ = block->PhiInstructionCount();
    }

    // Try to evaluate each instruction and update
    // the operand map with the constants if the case.
    block->ForEachNonPhiInstruction([&, this](Instruction* instr) -> bool {
        if(instr->HasDestinationOp() ) {
            if(auto result = FoldInstruction(instr)) {
                operandMap_.Add(instr->GetDestinationOp(), result);
                constantInstrs_++; 
            }
        }

        return true;
    });

    // Check if the branching instruction uses a constant now.
    Operand* branchingConst = nullptr;

    if(auto branchInstr = block->BranchInstruction()) {
        auto branchOp = branchInstr->GetSourceOp(0);
        branchingConst = TryGetFromMap(branchOp)->As<Constant>();
    }

    return ConstantEstimationResult(totalInstrs_, constantInstrs_, 
                                    phiInstrs_, branchingConst);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEstimator::AddConstantOperand(Operand* op, Constant* constantOp) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsNotNull(constantOp);
    DebugValidator::IsFalse(operandMap_.ContainsKey(op));
    DebugValidator::IsFalse(op->IsConstant() || op->IsReference());

    operandMap_.Add(op, constantOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEstimator::Reset() {
    operandMap_.Clear();
    totalInstrs_ = 0;
    constantInstrs_ = 0;
    phiInstrs_ = 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEstimator::Initialize(Unit* unit) {
    irGen_ = IRGenerator(unit);
    folder_ = ConstantFolder(&irGen_, target_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantEstimator::FoldInstruction(Instruction* instr) {
    if(instr->IsArithmetic() || instr->IsLogical()) {
        auto newLeftOp = TryGetFromMap(instr->GetSourceOp(0));
        auto newRightOp = TryGetFromMap(instr->GetSourceOp(1));

        return folder_.FoldBinary(instr->GetOpcode(), newLeftOp, newRightOp, 
                                  instr->ParentBlock());
    }
    else if(auto cmpInstr = instr->As<CmpInstrBase>()) {
        auto newLeftOp = TryGetFromMap(cmpInstr->GetSourceOp(0));
        auto newRightOp = TryGetFromMap(cmpInstr->GetSourceOp(1));

        return folder_.FoldCompare(cmpInstr->GetOpcode(), newLeftOp, newRightOp,
                                   cmpInstr->Order(), instr->ParentBlock());
    }
    else if(auto conversionInstr = instr->As<ConversionInstr>()) {
        auto newTargetop = TryGetFromMap(conversionInstr->GetSourceOp(0));
        return folder_.FoldConversion(conversionInstr->GetOpcode(), newTargetop,
                                      conversionInstr->CastType(), instr->ParentBlock());
    }
    else if(auto questInstr = instr->As<QuestionInstr>()) {
        auto newConditionOp = TryGetFromMap(questInstr->ConditionOp());
        Operand* selectedOp = nullptr;

        if(newConditionOp->IsIntConstant()) {
            if(newConditionOp->IsZeroInt()) {
                selectedOp = TryGetFromMap(questInstr->FalseOp());
            }
            else selectedOp = selectedOp = TryGetFromMap(questInstr->TrueOp());
        }

        if(selectedOp && (selectedOp->IsConstant() || selectedOp->IsReference())) {
            return selectedOp;
        }
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        if((callInstr->ArgumentCount() >= MIN_CALL_ARGUMENT_COUNT) &&
           (callInstr->ArgumentCount() <= MAX_CALL_ARGUMENT_COUNT)) {
            // Make a list with the new arguments and try to fold
            // the call. Useful especially for math operations.
            CallInstr::ArgumentList newArgs(callInstr->ArgumentCount());

            callInstr->ForEachArgument([&, this](Operand* argument, int index) -> bool {
                newArgs.Add(TryGetFromMap(argument));
                return true;
            });

            return folder_.FoldCall(callInstr, &newArgs);
        }
    }
    else if(auto loadInstr = instr->As<LoadInstr>()) {
        auto newSourceOp = TryGetFromMap(loadInstr->SourceOp());
        return folder_.FoldLoad(newSourceOp);
    }
    else if(instr->IsBranching()) {
        auto newConditionOp = TryGetFromMap(instr->GetSourceOp(0));
        return folder_.FoldBranching(instr, newConditionOp);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEstimator::ReplacePhisWithIncoming(Block* block, Block* incomingBlock) {
    DebugValidator::IsNotNull(incomingBlock);

    // Replace each 'phi' result with the incoming operand.
    block->ForEachPhiInstruction([&, this](PhiInstr* instr) -> bool {
        if(instr->HasDestinationOp()) {
            DebugValidator::IsTrue(instr->HasOperandFromBlock(incomingBlock));

            operandMap_.Add(instr->GetDestinationOp(),
                            instr->GetOperandFromBlock(incomingBlock));
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantEstimator::TryGetFromMap(Operand* op) {
    // Fast case for constants and references,
    // they can't be in the map.
    if(op->IsConstant() || op->IsReference()) {
        return op;
    }

    Operand* opFromMap;

    if(operandMap_.TryGetValue(op, &opFromMap)) {
        return opFromMap;
    }
    else return op;
}

} // namespace Analysis