// LoopRecognizer.hpp
// Copyright (c) Lup Gratian
//
// Defines the class that recognizes natural loops
// and creates the Loop objects used to represent them.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_LOOP_RECOGNIZER_HPP
#define PC_ANALYSIS_LOOP_RECOGNIZER_HPP

#include "Loop.hpp"
#include "LoopTag.hpp"
#include "CFGWalker.hpp"
#include "IRDominators.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class LoopRecognizer {
private:
    typedef CFGInfo<Block, Function> IRCFGInfo;

    // Returns the tag containing information about the loops
    // found in the specified function.
    LoopTag* GetOrCreateLoopTag(Function* function);

    // Scans the CFG of the specified function
    // identifying every loop and their relationship.
    void FindLoops(Function* function);

    // Finds all blocks that are part of the loops
    // which starts with the specified header.
    void FindLoopBody(Block* headerBlock, Loop* loop, 
                      IRDominatorTree* dominatorTree, LoopTag* loopTag);

    // Finds the blocks that are executed after the loop stops iterating.
    void FindExitBlocks(Loop* loop);

public:
    void Execute(Function* function);

    void Execute(Unit* unit);
};

} // namespace Analysis
#endif