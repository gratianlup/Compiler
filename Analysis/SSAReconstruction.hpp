// SSAReconstruction.hpp
// Copyright (c) Lup Gratian
//
// Implements a helper that reconstructs the SSA form for the users
// of an instruction when complicated CFG changes have been performed.
// Usefull for optimizations like jump-threading or loop-unrolling.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SSA_RECONTRUCTION_HPP
#define PC_ANALYSIS_SSA_RECONTRUCTION_HPP

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class SSAReconstruction {
private:
    typedef Dictionary<Block*, Operand*> BlockOperandMap;
    typedef StaticList<Block*, 16> BlockList;
    typedef StaticList<Operand*, 4> OperandList;

    BlockOperandMap availableOps_;
    BlockList loopBlocks_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Reconstructs the SSA form for the specified user that has 
    // the result of the rewritten instruction as a source operand.
    void ReconstructSingleUser(Instruction* user, Instruction* rewrittenInstr);

    // Special reconstruction for 'phi' instruction.
    void ReconstructPhiUser(PhiInstr* phiUser, Instruction* rewrittenInstr);

    // Returns the operand that is incoming from the specified block.
    // If a defining instruction is not found in the block the predecessors 
    // are queried by calling 'GetIncomingFromBlockPredecessors'.
    Operand* GetIncomingFromBlock(Block* block, Instruction* rewrittenInstr);

    // Returns the operand incoming into the block from the predecessors.
    // If different operands are incoming a 'phi' instruction is inserted.
    Operand* GetIncomingFromBlockPredecessors(Block* block, Instruction* rewrittenInstr);

    // Returns 'true' if a 'phi' instruction is really required
    // to merge the incoming operands. If all operands are the same
    // it is assigned to 'singleOp' and 'false' is returned.
    bool IsPhiRequired(OperandList& incomingOps, Operand*& singleOp);

    // Creates the 'phi' that merges the operands from the list
    // and inserts it at the beginning of the block.
    Operand* CreatePhi(OperandList& incomingOps, Block* block);

public:
    // Reconstructs the SSA format for all users of the specified instruction.
    // Use 'MakeOperandIncomingFromBlock' to specify the additional incoming operands.
    void ReconstructUsers(Instruction* rewrittenInstr);

    // Reconstructs the SSA format only for the specified user
    // that has the result of 'rewrittenInstr' as a source operand.
    // Use 'MakeOperandIncomingFromBlock' to specify the additional incoming operands.
    void ReconstructUser(Instruction* user, Instruction* rewrittenInstr);

    // Forces the specified operand to be incoming from the block.
    // Should be used to specify the additional incoming operands 
    // before calling 'ReconstructUsers' to do the actual reconstruction.
    void MakeOperandIncomingFromBlock(Operand* incomingOp, Block* incomingBlock);

    // Brings the reconstruction helper in the initial state.
    // Should be called when reconstructing the SSA for multiple instructions.
    void Reset();
};

} // namespace Analysis
#endif