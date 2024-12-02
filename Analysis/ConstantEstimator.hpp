// ConstantEstimator.hpp
// Copyright (c) Lup Gratian
//
// Estimates the number of instructions in a block that would turn
// into constants after certain operands are replaced.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_CONSTANT_ESTIMATOR_HPP
#define PC_ANALYSIS_CONSTANT_ESTIMATOR_HPP

#include "ConstantFolder.hpp"
#include "../IR/Block.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Base/Log.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Targets/TargetInfo.hpp"
using namespace IR;
using namespace Base;
using namespace Target;

namespace Analysis {

// Stores the result of the estimation.
class ConstantEstimationResult {
private:
    int totalInstrs_;
    int constantInstrs_;
    int phiInstrs_;
    Operand* branchingConst_;

public:
    ConstantEstimationResult() :
            totalInstrs_(0), constantInstrs_(0),
            phiInstrs_(0), branchingConst_(nullptr) {}

    ConstantEstimationResult(int totalInstrs, int constantInstrs,
                             int phiInstrs = 0, Operand* branchingConst = nullptr) :
            totalInstrs_(totalInstrs), constantInstrs_(constantInstrs),
            phiInstrs_(phiInstrs), branchingConst_(branchingConst) {
        DebugValidator::IsSmallerOrEqual(constantInstrs, totalInstrs);
        DebugValidator::IsSmallerOrEqual(phiInstrs, totalInstrs);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    int TotalInstructions() const {
        return totalInstrs_;
    }

    int ConstantInstructions() const {
        return constantInstrs_;
    }

    int HasConstantInstructions() const {
        return constantInstrs_ > 0;
    }

    int PhiInstructionsReplaced() const {
        return phiInstrs_;
    }

    Operand* BranchingConstant() const {
        return branchingConst_;
    }

    bool HasBranchingOnConstant() const {
        return branchingConst_ != nullptr;
    }

    float ConstantInstructionsPercent(bool ignorePhi = false) const {
        int instrs = totalInstrs_ - (ignorePhi ? phiInstrs_ : 0);

        if(instrs == 0) {
            return 0;
        }
        else return (float)constantInstrs_ / (float)instrs;
    }

    int RemainingInstructions(bool ignorePhi = false) {
        return totalInstrs_ - constantInstrs_ - 
               (ignorePhi ? 0 : phiInstrs_);
    }
};


class ConstantEstimator {
private:
    typedef List<Block*> BlockList;
    typedef Dictionary<Operand*, Operand*> OperandMap;

    static const int MIN_CALL_ARGUMENT_COUNT = 1;
    static const int MAX_CALL_ARGUMENT_COUNT = 4;

    IRGenerator irGen_;
    ConstantFolder folder_;
    TargetInfo* target_;
    OperandMap operandMap_;
    int totalInstrs_;
    int constantInstrs_;
    int phiInstrs_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    void Initialize(Unit* unit);

    Operand* FoldInstruction(Instruction* instr);

    void ReplacePhisWithIncoming(Block* block, Block* incomingBlock);

    Operand* TryGetFromMap(Operand* op);

public:
    ConstantEstimator(TargetInfo* target) : target_(target) {
        DebugValidator::IsNotNull(target);
        Reset();
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Tries to evaluate (constant-fold) the instruction in the specified block.
    // Note that no change is made to the actual instructions.
    // If 'incomingBlock' is specified the 'phi' instructions in 'block'
    // are replaced with the corresponding incoming operands.
    ConstantEstimationResult Estimate(Block* block, Block* incomingBlock = nullptr);

    // Forces the evaluator to use the specified constant 
    // instead of the original operand. Note that the replaced 
    // operand cannot be a constant or a reference.
    void AddConstantOperand(Operand* op, Constant* constantOp);

    // Brings the evaluator into a clean state. Should be used
    // before analyzing another block with the same evaluator.
    void Reset();
};

} // namespace Analysis
#endif