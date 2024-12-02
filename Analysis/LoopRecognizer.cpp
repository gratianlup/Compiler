// LoopRecognizer.cpp
// Copyright (c) Lup Gratian
//
// Implements the LoopRecognizer class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "LoopRecognizer.hpp"
#include "LoopTreePrinter.hpp"

namespace Analysis {

void LoopRecognizer::Execute(Function* function) {
    FindLoops(function);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoopRecognizer::Execute(Unit* unit) {
    // Find the loops in all defined functions.
    for(auto function = unit->Functions().First(); function;
        function = function->Next) {
        if(function->Value->IsDefinition()) {
            Execute(function->Value);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoopRecognizer::FindLoops(Function* function) {
    // The Dominator Tree is created only if a backedge is found.
    // This is a speed optimization for functions without loops.
    local<IRDominatorTree> dominatorTree;
    LoopTag* loopTag = nullptr;

    // Compute the list of blocks in postorder and the type
    // of CFG edges (we're interested in checking for back-edges).
    IRCFGInfo cfgInfo(function, true /* edgeInfoNeeded */);
    auto& postorderList = cfgInfo.PostorderList();

    // If a block that has a predecessor dominated by the block 
    // is found it means a new loop starts having the block as its header.
    for(int i = 0; i < postorderList.Count(); i++) {
        auto block = const_cast<Block*>(postorderList[i]);

        for(int j = 0; j < block->PredecessorCount(); j++) {
            auto predecessorBlock = block->PredecessorAt(j);

            if(cfgInfo.IsBackwardEdge(predecessorBlock, block) == false) {
                continue;
            }

            // Create the Dominator Tree if it wasn't already created
            // and check if the block dominates the predecessor.
            if(dominatorTree == nullptr) {
                dominatorTree = new IRDominatorTree(function);
                dominatorTree->Build();
            }
            
            if(dominatorTree->Dominates(block, predecessorBlock)) {
                // A new loop was found, now find the blocks which form
                // its body and the exit blocks.
                if(loopTag == nullptr)  {
                    loopTag = GetOrCreateLoopTag(function);
                }

                shared<Loop> loop = new Loop(block);
                FindLoopBody(block, loop, dominatorTree, loopTag);
                FindExitBlocks(loop);
                loopTag->AddLoop(loop);
#if 0
                loop->Dump();
#endif
            }
        }
    }

#if 0
    LoopTreePrinter("J:\\test\\loopTree.dot").Print(function);
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoopRecognizer::FindLoopBody(Block* headerBlock, Loop* loop,
                                  IRDominatorTree* dominatorTree, LoopTag* loopTag) {
    // A Natural Loop has a single entry point (the loop header)
    // which dominates all other blocks of the loop body.
    // Perform a reverse graph reachability transitive closure starting
    // with the blocks that branch to the loop header.
    StaticList<Block*, 16> worklist;

    for(int i = 0; i < headerBlock->PredecessorCount(); i++) {
        auto predecessorBlock = headerBlock->PredecessorAt(i);

        if(dominatorTree->Dominates(headerBlock, predecessorBlock)) {
            loop->AddBackedgeBlock(predecessorBlock);
            worklist.Add(predecessorBlock);
        }
    }

    while(worklist.IsNotEmpty()) {
        auto block = worklist.RemoveLast();
        loop->AddBlock(block);

        // Check if it is the header of a nested loop;
        // if it is add that loop as a child of the current one.
        if(auto nestedLoop = loopTag->GetLoop(block)) {
            if(nestedLoop->ParentLoop() == nullptr) {
                nestedLoop->SetParent(loop);
                loop->AddNestedLoop(nestedLoop);
            }
        }

        // Add to the worklist all predecessors that are not
        // already part of loop body.
        for(int i = 0; i < block->PredecessorCount(); i++) {
            auto predecessorBlock = block->PredecessorAt(i);

            if((loop->HasBlock(predecessorBlock) == false) &&
               (worklist.Contains(predecessorBlock) == false)) {
                worklist.Add(predecessorBlock);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LoopRecognizer::FindExitBlocks(Loop* loop) {
    // An exit block is a block that is executed after the loop
    // stops iterating. A loop can have multiple exit blocks because
    // of high-level language constructs like 'break'.
    loop->ForEachBlock([loop](Block* block) -> bool {
        // An exit block is a block outside the loop which has
        // at least a predecessor inside the loop.
        for(int i = 0; i < block->SuccessorCount(); i++) {
            auto successorBlock = block->SuccessorAt(i);

            if(loop->HasBlock(successorBlock) == false) {
                loop->AddExitBlock(successorBlock);
            }
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LoopTag* LoopRecognizer::GetOrCreateLoopTag(Function* function) {
    LoopTag* tag = function->GetTag<LoopTag>();

    if(tag == nullptr) {
        tag = LoopTag::GetLoop();
        function->AddTag(tag);
    }

    return tag;
}

} //namespace Analysis