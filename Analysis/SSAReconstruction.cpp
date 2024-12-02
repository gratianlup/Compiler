// SSAReconstruction.cpp
// Copyright (c) Lup Gratian
//
// Implements the SSAReconstruction class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SSAReconstruction.hpp"

namespace Analysis {

void SSAReconstruction::MakeOperandIncomingFromBlock(Operand* incomingOp, 
                                                     Block* incomingBlock) {
    DebugValidator::IsNotNull(incomingOp);
    DebugValidator::IsNotNull(incomingBlock);
    DebugValidator::IsFalse(availableOps_.ContainsKey(incomingBlock));

    availableOps_.Add(incomingBlock, incomingOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAReconstruction::ReconstructUsers(Instruction* rewrittenInstr) {
    DebugValidator::IsNotNull(rewrittenInstr);
    DebugValidator::IsTrue(rewrittenInstr->HasDestinationOp());

    // Reconstruct the SSA form for each user
    // and update its source operands if necessary.
    auto rewrittenOp = rewrittenInstr->GetDestinationOp();

    rewrittenOp->ForEachUser([&, this](Instruction* user, int index) -> bool {
        ReconstructSingleUser(user, rewrittenInstr);
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAReconstruction::ReconstructUser(Instruction* user, 
                                        Instruction* rewrittenInstr) {
    DebugValidator::IsNotNull(user);
    DebugValidator::IsNotNull(rewrittenInstr);
    DebugValidator::IsTrue(rewrittenInstr->HasDestinationOp());
    
    ReconstructSingleUser(user, rewrittenInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAReconstruction::ReconstructSingleUser(Instruction* user, 
                                              Instruction* rewrittenInstr) {
    // Reconstruct the SSA form for the user
    // and update its source operands if necessary.
    if(auto phiInstr = user->As<PhiInstr>()) {
        ReconstructPhiUser(phiInstr, rewrittenInstr);
    }
    else {
        auto rewrittenOp = rewrittenInstr->GetDestinationOp();
        auto replacementOp = GetIncomingFromBlock(user->ParentBlock(), rewrittenInstr);

        if(replacementOp != rewrittenOp) {
            user->ReplaceSourceOp(rewrittenOp, replacementOp);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAReconstruction::ReconstructPhiUser(PhiInstr* phiUser, 
                                           Instruction* rewrittenInstr) {
    // In case of 'phi' instruction users the operands
    // need to be searched in the incoming blocks.
    auto rewrittenOp = rewrittenInstr->GetDestinationOp();

    for(int i = 0; i < phiUser->OperandCount(); i++) {
        if(phiUser->GetOperand(i) == rewrittenOp) {
            auto replacementOp = GetIncomingFromBlock(phiUser->GetOperandBlock(i), 
                                                      rewrittenInstr);
            if(replacementOp != rewrittenOp) {
                phiUser->ReplaceOperand(i, replacementOp);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAReconstruction::Reset() {
    availableOps_.Clear();
    loopBlocks_.Clear();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SSAReconstruction::GetIncomingFromBlock(Block* block, 
                                                 Instruction* rewrittenInstr) {
    // First check if the users forces a certain operand
    // to replace the rewritten instruction.
    Operand* replacementOp;

    if(availableOps_.TryGetValue(block, &replacementOp)) {
        return replacementOp;
    }

    // If the rewritten instruction is defined in the block
    // just return its result operand, otherwise search for it
    // in the predecessor blocks, inserting 'phi' instructions if required.
    if(rewrittenInstr->ParentBlock() == block) {
        DebugValidator::IsTrue(rewrittenInstr->HasDestinationOp());
        return rewrittenInstr->GetDestinationOp();
    }
    else {
        // Make the replacement operand available at the end of this block,
        // it eliminates the search in future requests.
        auto replacementOp = GetIncomingFromBlockPredecessors(block, rewrittenInstr);
        availableOps_.Add(block, replacementOp);
        return replacementOp;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SSAReconstruction::GetIncomingFromBlockPredecessors(Block* block, 
                                                             Instruction* rewrittenInstr) {
    // The instruction is not defined in 'block', but in its predecessors. 
    // Collect the operands that are incoming from each predecessor 
    // and insert a 'phi' instruction if at least two don't match.
    if(loopBlocks_.Contains(block)) {
        // The block is part of a loop and it was reached again
        // through a predecessor; no value is incoming (yet) from it.
        return nullptr;
    }

    Operand* replacementOp;
    loopBlocks_.Add(block);

    if(block->HasSinglePredecessor()) {
        // For a single predecessor no 'phi' is needed.
        replacementOp = GetIncomingFromBlock(block->PredecessorAt(0), rewrittenInstr);
    }
    else {
        OperandList incomingOps;
        
        block->ForEachPredecessor([&, this](Block* predecessorBlock, int index) -> bool {
            incomingOps.Add(GetIncomingFromBlock(predecessorBlock, rewrittenInstr));
            return true;
        });

        if(IsPhiRequired(incomingOps, replacementOp)) {
            replacementOp = CreatePhi(incomingOps, block);
        }
    }

    loopBlocks_.Remove(block);
    return replacementOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SSAReconstruction::IsPhiRequired(OperandList& incomingOps, Operand*& singleOp) {
    // A 'phi' instruction is required if only if
    // at least two different operands are incoming
    // (the undefined constant and null operands are ignored).
    singleOp = nullptr;
    bool required = false;

    incomingOps.ForEach([&](Operand* incomingOp) -> bool {
        if((incomingOp == nullptr) ||
            incomingOp->IsUndefinedConstant()) {
            return true; // Skip.
        }
        else if(singleOp == nullptr) {
            singleOp = incomingOp;
        }
        else if(singleOp != incomingOp) {
            required = true;
        }

        return true;
    });

    // At least one usefull operand should be incoming.
    DebugValidator::IsNotNull(singleOp);
    return required;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* SSAReconstruction::CreatePhi(OperandList& incomingOps, Block* block) {
    DebugValidator::IsLarger(incomingOps.Count(), 1);
    auto phiResultOp = Temporary::GetTemporary(incomingOps[0]->GetType());
    auto phiInstr = PhiInstr::GetPhi(phiResultOp, incomingOps.Count());

    for(int i = 0; i < incomingOps.Count(); i++) {
        auto incomingOp = incomingOps[i] ? incomingOps[i] : phiResultOp;
        phiInstr->AddOperand(incomingOp, block->PredecessorAt(i));
    }

    block->InsertInstructionFirst(phiInstr);
    return phiResultOp;
}

} // namespace Analysis