// Loop.cpp
// Copyright (c) Lup Gratian
//
// Implements the Loop class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Loop.hpp"
#include "LoopTag.hpp"

namespace Analysis {

Loop::Loop(Block* headerBlock, Loop* parent) : 
        parentLoop_(parent), headerBlockId_(0), preheaderBlockId_(0),
        parentFunction_(nullptr), loopDepth_(-1), 
        testBlock_(nullptr), isInverted_(false) {
    DebugValidator::IsNotNull(headerBlock);

    headerBlockId_ = headerBlock->Id();
    parentFunction_ = headerBlock->ParentFunction()->GetReference();
    parentFunction_->AddUser();
    AddBlock(headerBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Loop::ComputeLoopDepth() const {
    Loop* loop = parentLoop_;
    loopDepth_ = 0;

    while(parentLoop_) {
        loopDepth_++;
        loop = loop->ParentLoop();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Loop::AddNestedLoopImpl(Loop* loop) {
    // The children list is allocated only on demand.
    if(childLoops_ == nullptr) {
        childLoops_ = new ChildrenList();
    }

    childLoops_->Add(loop);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Loop::RemoveNestedLoopImpl(Loop* loop) {
    DebugValidator::IsNotNull(loop);
    DebugValidator::IsTrue(HasNestedLoops());

    childLoops_->Remove(loop);

    // Remove the list if no nested loops remain.
    if(childLoops_->Count() == 0) {
        childLoops_ = nullptr;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* Loop::CreatePreheader() {
    // If a preheader was already created just return it.
    if(preheaderBlockId_ != 0) {
        auto& symbols = parentFunction_->Target()->Symbols();
        return static_cast<Block*>(symbols.Get(preheaderBlockId_));
    }

    // A preheader is a block that is placed right before 
    // the header of a loop, becoming the new block which enters the loop.
    // Find the predecessor which enters the block (all other ones
    // must be backedges) and place a new block between it and the header.
    auto headerBlock = GetLoopHeader();
    Block* enteringBlock = nullptr;

    for(int i = 0; i < headerBlock->PredecessorCount(); i++) {
        auto predecessor = headerBlock->PredecessorAt(i);
        
        if(IsLoopBackedgeBlock(predecessor) == false) {
            enteringBlock = predecessor;
            break;
        }
    }

    DebugValidator::IsNotNull(enteringBlock);

    // The loop is probably stored inside a Loop Tag and creating
    // the preheader doesn't actually invalidate it.
    auto loopTag = parentFunction_->Target()->GetTag<LoopTag>();
    bool validLoopTag = loopTag ? loopTag->IsSafeToUse() : false;

    // Create the preaheader block, insert it before the header,
    // then make the entering block branch to the preheader
    // and patch the incoming 'phi' operands.
    auto& symbols = parentFunction_->Target()->Symbols();
    string name = BlockUtils::CreateUniqueName("#preheader", symbols);
    Block* preheaderBlock = Block::GetBlock(name);

    parentFunction_->Target()->InsertBlockBefore(preheaderBlock, headerBlock);
    enteringBlock->ReplaceSuccessor(headerBlock, preheaderBlock);

    preheaderBlock->LinkWith(headerBlock);
    BlockUtils::ReplacePhiOperandsBlock(headerBlock, enteringBlock, preheaderBlock);

    // Make the Loop Tag valid to use again.
    if(validLoopTag) {
        loopTag->SetIsSafeToUse(true);
    }

    // If this loop is nested inside another one
    // the preheader becomes part of the parent loop body.
    if(parentLoop_) {
        parentLoop_->AddBlock(preheaderBlock);
    }

    preheaderBlockId_ = preheaderBlock->Id();
    return preheaderBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Loop* Loop::FindCommonAncestor(Loop* loopA, Loop* loopB) {
    DebugValidator::IsNotNull(loopA);
    DebugValidator::IsNotNull(loopB);

    while(loopA && loopB) {
        while(loopA && (loopA->LoopDepth() > loopB->LoopDepth())) {
            loopA = loopA->ParentLoop();
        }

        while(loopB && (loopB->LoopDepth() > loopA->LoopDepth())) {
            loopB = loopB->ParentLoop();
        }

        if(loopA == loopB) {
            // Found common ancestor.
            return loopA;
        }
        else {
            // Move up one level in both loop tress.
            if(loopA) loopA = loopA->ParentLoop();
            if(loopB) loopB = loopB->ParentLoop();
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Loop::ToString() const {
    auto& symbols = parentFunction_->Target()->Symbols();
    StringBuilder sb;
    
    auto header = reinterpret_cast<Block*>(symbols.Get(headerBlockId_));
    sb.AppendLine("Header: " + *header->Name());

    if(HasPreheader()) {
        auto preheader = reinterpret_cast<Block*>(symbols.Get(preheaderBlockId_));    
        sb.AppendLine("Preheader: " + *preheader->Name());
    }

    sb.AppendFormat(L"Exit blocks (%d): ", ExitBlocksCount());
    ForEachExitBlock([&sb](const Block* block) -> bool {
        sb.Append(*block->Name() + ", ");
        return true;
    });

    sb.AppendFormat(L"\nBackedge blocks (%d): ", BackedgeBlocksCount());
    ForEachBackedgeBlock([&sb](const Block* block) -> bool {
        sb.Append(*block->Name() + ", ");
        return true;
    });

    sb.AppendFormat(L"\nBlocks (%d): ", BlockCount());
    ForEachBlock([&sb](const Block* block) -> bool {
        sb.Append(*block->Name() + ", ");
        return true;
    });

    sb.AppendFormat(L"\nNested loops (%d): ", childLoops_ ? childLoops_->Count() : 0);
    ForEachNestedLoop([&sb](Loop* loop) -> bool {
        sb.Append(*loop->GetLoopHeader()->Name() + ", ");
        return true;
    });

    return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Loop::Dump() {
    ObjectDumper(ToString(), "Loop").Dump();
}

} // namespace Analysis