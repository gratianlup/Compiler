// Loop.hpp
// Copyright (c) Lup Gratian
//
// Defines the class used to represent a Natural Loop and the Loop Tree.
// Besides knowing which blocks are part of the loop, the class
// can respond to questions like "Does the loop execute at least once?".
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_LOOP_HPP
#define PC_ANALYSIS_LOOP_HPP

#include "SparseBitVector.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Optimization/BlockUtils.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;
using namespace Optimization;

namespace Analysis {

class Loop {
private:
    typedef StaticList<Loop*, 2> ChildrenList;
    typedef shared<ChildrenList> ChildrenListHolder;

    Loop* parentLoop_;              // 'nullptr' if the loop is the topmost one.
    ChildrenListHolder childLoops_; // The loops embedded into this one.
    unsigned headerBlockId_;        // The Id of the loop header block.
    unsigned preheaderBlockId_;     // The Id of the loop preheader, or 0 if doesn't exist.

    SparseBitVector loopBlocks_;        // The Ids of the blocks part of the loop.
    SparseBitVector exitBlocks_;        // The Ids of the first blocks after the loop.
    SparseBitVector backedgeBlocks_;    // The Ids of the blocks which form a backedge.
    FunctionReference* parentFunction_; // The associated function.
    Block* testBlock_;                  // Test block created by the Loop Inversion pass.
    mutable unsigned loopDepth_;        // The depth in the Loop Tree, -1 if not computed.
    bool isInverted_;                   // 'true' if the Loop Inversion pass has been run.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Loop(const Loop& other);             // Should not be copied.
    Loop& operator= (const Loop& other); // Should not be assigned.

    void ComputeLoopDepth() const;

    void AddNestedLoopImpl(Loop* loop);

    void RemoveNestedLoopImpl(Loop* loop);

    Block* GetBlockFromId(int id) {
        auto& symbols = parentFunction_->Target()->Symbols();
        return static_cast<Block*>(symbols.Get(id));
    }

    const Block* GetBlockFromId(int id) const {
        auto& symbols = parentFunction_->Target()->Symbols();
        return static_cast<Block*>(symbols.Get(id));
    }

public:
    Loop(Block* headerBlock, Loop* parent = nullptr);

    ~Loop() {
        parentFunction_->Free();
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the parent's function reference.
    FunctionReference* ParentFunctionReference() {
        return parentFunction_;
    }

    // Returns the parent function.
    Function* ParentFunction() {
        return parentFunction_->Target();
    }

    // Returns the parent loop of the current one,
    // or 'nullptr' if this is the topmost loop.
    Loop* ParentLoop() {
        return parentLoop_;
    }

    const Loop* ParentLoop() const {
        return parentLoop_;
    }

    void SetParent(Loop* parent) {
        parentLoop_ = parent;
    }

    // Returns 'true' if this is a top level loop,
    // meaning it has no parent.
    bool IsTopLevel() const {
        return parentLoop_ == nullptr;
    }

    // Returns 'true' if this loop is nested inside another one.
    bool IsNestedLoop() const {
        return parentLoop_;
    }

    // Returns the depth of this loop in the Loop Tree.
    // A topmost loop is considered to have depth 0.
    int LoopDepth() const {
        if(parentLoop_ == nullptr) {
            return 0;
        }
        else if(loopDepth_ == -1) {
            // Not yet computed, do it now.
            ComputeLoopDepth();
        }

        return loopDepth_;
    }

    // Adds the specified block to the list of blocks part of the loop.
    void AddBlock(Block* block) {
        DebugValidator::IsNotNull(block);
        loopBlocks_.SetBit(block->Id());

        if(parentLoop_) {
            parentLoop_->AddBlock(block);
        }
    }

    // Returns the number of blocks that are part of the loop.
    // Note that this excludes the preheader, if one was created.
    int BlockCount() const {
        return loopBlocks_.SetBitsCount();
    }

    // Returns 'true' if the specified block is part of the loop
    // Note that this excludes the preheader, if one was created.
    bool HasBlock(Block* block) const {
        DebugValidator::IsNotNull(block);
        return loopBlocks_.IsSet(block->Id());
    }

    // Removes the specified block from the list of block part of the loop.
    void RemoveBlock(Block* block) {
        DebugValidator::IsNotNull(block);
        DebugValidator::IsTrue(HasBlock(block));
        loopBlocks_.ResetBit(block->Id());
    }

    // Performs the specified action on each block in the loop.
    // Note that the preheader is excluded, if it was created.
    // bool Predicate(Block* loopBlock)
    template <class Predicate>
    void ForEachBlock(Predicate action) {
        auto& symbolTable = parentFunction_->Target()->Symbols();

        loopBlocks_.ForEachSetBit([&symbolTable, action](int index) -> bool {
            auto block = reinterpret_cast<Block*>(symbolTable.Get(index));
            return action(block);
        });
    }

    // Performs the specified action on each block in the loop.
    // Note that the preheader is excluded, if it was created.
    // bool Predicate(const Block* loopBlock) const
    template <class Predicate>
    void ForEachBlock(Predicate action) const {
        auto& symbolTable = parentFunction_->Target()->Symbols();

        loopBlocks_.ForEachSetBit([&symbolTable, action](int index) -> bool {
            auto block = reinterpret_cast<const Block*>(symbolTable.Get(index));
            return action(block);
        });
    }

    // Returns the block that acts as the loop header
    // (the first block in the loop that is entered).
    Block* GetLoopHeader() {
        return GetBlockFromId(headerBlockId_);
    }

    const Block* GetLoopHeader() const {
        return GetBlockFromId(headerBlockId_);
    }

    // Returns the loop preheader, or 'nullptr' if one has not been created.
    Block* GetLoopPreheader() {
        if(preheaderBlockId_ == 0) {
            return nullptr;
        }
        else return GetBlockFromId(preheaderBlockId_);
    }

    const Block* GetLoopPreheader() const {
        if(preheaderBlockId_ == 0) {
            return nullptr;
        }
        else return GetBlockFromId(preheaderBlockId_);
    }

    // Returns 'true' if the specified block is the header of the loop.
    bool BlockIsHeader(Block* block) const {
        DebugValidator::IsNotNull(block);
        return headerBlockId_ == block->Id();
    }

    // Returns 'true' if the specified block is the preheader of the loop,
    // or 'false' if the loop has no preheader.
    bool BlockIsPreheader(Block* block) const {
        DebugValidator::IsNotNull(block);

        if(preheaderBlockId_ == 0) {
            return false;
        }
        else return preheaderBlockId_ == block->Id();
    }

    // Returns 'true' if a preheader has been created for the loop.
    bool HasPreheader() const {
        return preheaderBlockId_ != 0;
    }

    // Adds the specified loop as a nested (child) loop of this one.
    void AddNestedLoop(Loop* loop) {
        DebugValidator::IsNotNull(loop);
        AddNestedLoopImpl(loop);
    }

    // Returns 'true' if the loop has at least one nested (child) loop.
    bool HasNestedLoops() const {
        return childLoops_ && childLoops_->Count() > 0;
    }

    // Returns the number of  loops nested inside this one.
    int NestedLoopsCount() const {
        if(childLoops_) {
            return childLoops_->Count();
        }
        else return 0;
    }

    // Returns the nested loop found at the specified position.
    Loop* GetNestedLoop(int index) {
        DebugValidator::IsNotNull(childLoops_.Raw());
        return (*childLoops_)[index];
    }

    const Loop* GetNestedLoop(int index) const {
        DebugValidator::IsNotNull(childLoops_.Raw());
        return (*childLoops_)[index];
    }

    // Removes the nested loop found at the specified position.
    void RemoveNestedLoop(int index) {
        DebugValidator::IsTrue(HasNestedLoops());
        RemoveNestedLoopImpl((*childLoops_)[index]);
    }

    // Removes the specified nested loop.
    void RemoveNestedLoop(Loop* loop) {
        DebugValidator::IsNotNull(loop);
        DebugValidator::IsTrue(HasNestedLoops());
        RemoveNestedLoopImpl(loop);
    }

    // Performs the specified action on each directly nested loop.
    // bool Predicate(Loop* nestedLoop);
    template <class Predicate>
    void ForEachNestedLoop(Predicate action) {
        if(childLoops_) {
            for(int i = 0; i < childLoops_->Count(); i++) {
                if(action((*childLoops_)[i]) == false) {
                    return;
                }
            }
        }
    }

    // Performs the specified action on each directly nested loop.
    // bool Predicate(const Loop* nestedLoop) const;
    template <class Predicate>
    void ForEachNestedLoop(Predicate action) const {
        if(childLoops_) {
            for(int i = 0; i < childLoops_->Count(); i++) {
                if(action((*childLoops_)[i]) == false) {
                    return;
                }
            }
        }
    }

    // Marks the specified block as being a loop exit block.
    void AddExitBlock(Block* block) {
        DebugValidator::IsNotNull(block);
        exitBlocks_.SetBit(block->Id());
    }

    // Returns the number of exit blocks of the loop.
    int ExitBlocksCount() const {
        return exitBlocks_.SetBitsCount();
    }

    // Returns 'true' if the specified block is a loop exit block.
    bool IsLoopExitBlock(Block* block) const {
        DebugValidator::IsNotNull(block);
        return exitBlocks_.IsSet(block->Id());
    }

    // Returns 'true' if the CFG edge formed by the specified blocks
    // leads to a block that is an exit for the loop.
    bool IsLoopExitEdge(Block* fromBlock, Block* toBlock) const {
        DebugValidator::IsNotNull(fromBlock);
        DebugValidator::IsNotNull(toBlock);

        return loopBlocks_.IsSet(fromBlock->Id()) &&
               exitBlocks_.IsSet(toBlock->Id());
    }

    // Removes the specified block from the list of loop exit blocks.
    void RemoveLoopExitBlock(Block* block) {
        DebugValidator::IsNotNull(block);
        exitBlocks_.ResetBit(block->Id());
    }

    // Performs the specified action on each block that is an exit block.
    // bool Predicate(Block* exitBlock);
    template <class Predicate>
    void ForEachExitBlock(Predicate action) {
        auto& symbolTable = parentFunction_->Target()->Symbols();

        exitBlocks_.ForEachSetBit([&symbolTable, action](int index) -> bool {
            auto block = reinterpret_cast<Block*>(symbolTable.Get(index));
            return action(block);
        });
    }

    // Performs the specified action on each block that is an exit block.
    // bool Predicate(const Block* exitBlock) const;
    template <class Predicate>
    void ForEachExitBlock(Predicate action) const {
        auto& symbolTable = parentFunction_->Target()->Symbols();

        exitBlocks_.ForEachSetBit([&symbolTable, action](int index) -> bool {
            auto block = reinterpret_cast<const Block*>(symbolTable.Get(index));
            return action(block);
        });
    }

    // Marks the specified block as being a loop backedge block.
    void AddBackedgeBlock(Block* block) {
        DebugValidator::IsNotNull(block);
        backedgeBlocks_.SetBit(block->Id());
    }

    // Returns the number of backedge blocks of the loop.
    int BackedgeBlocksCount() const {
        return backedgeBlocks_.SetBitsCount();
    }

    // Returns 'true' if the specified block is a loop backedge block.
    bool IsLoopBackedgeBlock(Block* block) const {
        DebugValidator::IsNotNull(block);
        return backedgeBlocks_.IsSet(block->Id());
    }

    // Returns 'true' if the CFG edge formed by the specified blocks
    // is a backedge that leads to the loop header.
    bool IsBackedge(Block* fromBlock, Block* toBlock) const {
        DebugValidator::IsNotNull(fromBlock);
        DebugValidator::IsNotNull(toBlock);

        return loopBlocks_.IsSet(toBlock->Id()) &&
               backedgeBlocks_.IsSet(fromBlock->Id());
    }

    // Removes the specified block from the list of loop backedge blocks.
    void RemoveLoopBackedgeBlock(Block* block) {
        DebugValidator::IsNotNull(block);
        backedgeBlocks_.ResetBit(block->Id());
    }

    // Performs the specified action on each block that is an backedge block.
    // bool Predicate(Block* backedgeBlock);
    template <class Predicate>
    void ForEachBackedgeBlock(Predicate action) {
        auto& symbolTable = parentFunction_->Target()->Symbols();

        backedgeBlocks_.ForEachSetBit([&symbolTable, action](int index) -> bool {
            auto block = reinterpret_cast<Block*>(symbolTable.Get(index));
            return action(block);
        });
    }

    // Performs the specified action on each block that is an backedge block.
    // bool Predicate(const Block* backedgeBlock) const;
    template <class Predicate>
    void ForEachBackedgeBlock(Predicate action) const {
        auto& symbolTable = parentFunction_->Target()->Symbols();

        backedgeBlocks_.ForEachSetBit([&symbolTable, action](int index) -> bool {
            auto block = reinterpret_cast<const Block*>(symbolTable.Get(index));
            return action(block);
        });
    }

    // Returns 'true' if the specified instruction is created
    // in a block part of the loop. Note that the preheader is ignored.
    bool IsDefinedInLoop(Instruction* instr) const {
        DebugValidator::IsNotNull(instr);
        return loopBlocks_.IsSet(instr->ParentBlock()->Id());
    }

    // Returns 'true' if the specified operand is created
    // in a block part of the loop. Note that the preheader is ignored.
    bool IsDefinedInLoop(Operand* op) const {
        if(auto instr = op->DefiningInstruction()) {
            return IsDefinedInLoop(instr);
        }
        else return true;
    }

    // Creates a preheader and redirects all predecessors that
    // branch to the loop header to branch to the preheader.
    Block* CreatePreheader();

    // Removes the preheader if it is empty (contains only a branch
    // to the loop header). Redirects all predecessors to the header.
    bool RemovePreheaderIfEmpty() {
        return false;
    }

    // Returns an operand that represents the number of times
    // the loop iterates until it is exited.
    Operand* ComputeTripCount() const {
        return nullptr;
    }

    // Returns an integer constant that represents the exact number 
    // of times the loop iterates until it is exited.
    IntConstant* ComputeConstantTripCount() const {
        return nullptr;
    }

    // Returns 'true' if it is known that the loop iterates at least once.
    bool ExecutesAtLeastOnce() const {
        return false;
    }

    // Returns the common ancestor of the specified loops,
    // or 'nullptr' if no such loop could be found.
    static Loop* FindCommonAncestor(Loop* loopA, Loop* loopB);

    // Returns the common ancestor between this loop and the specified one,
    // or 'nullptr' if no such loop could be found.
    Loop* FindCommonAncestor(Loop* otherLoop) {
        DebugValidator::IsNotNull(otherLoop);
        return FindCommonAncestor(this, otherLoop);
    }

    // Returns 'true' if there is the specified loops have a common ancestor.
    static bool HaveCommonAncestor(Loop* loopA, Loop* loopB) {
        DebugValidator::IsNotNull(loopA);
        DebugValidator::IsNotNull(loopB);
        return FindCommonAncestor(loopA, loopB) != nullptr;
    }

    // Returns 'true' if this loop and the specified one have a common ancestor.
    bool HaveCommonAncestor(Loop* otherLoop) {
        DebugValidator::IsNotNull(otherLoop);
        return HaveCommonAncestor(this, otherLoop);
    }

    // Returns 'true' if it is known that the loop iterates at least
    // the required number of times.
    bool ExecutesAtLeast(int times) const {
        DebugValidator::IsLarger(times, 0);
        return false;
    }

    string ToString() const;

    void Dump();
};

} // namespace Analysis
#endif