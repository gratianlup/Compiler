// LoopTreePrinter.hpp
// Copyright (c) Lup Gratian
//
// Prints a Loop Tree to a Graphviz DOT file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_LOOP_TREE_PRINTER_HPP
#define PC_ANALYSIS_LOOP_TREE_PRINTER_HPP

#include "Loop.hpp"
#include "LoopTag.hpp"
#include "DotPrinterBase.hpp"
#include "../IR/Unit.hpp"
#include "../Base/String.hpp"
#include "../Base/SharedPointer.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class LoopTreePrinter : public DotPrinterBase {
public:
    LoopTreePrinter(const string& file) : DotPrinterBase(file) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void Print(Function* function, bool details = true) {
        if(auto loopTag = function->GetTag<LoopTag>()) {
            loopTag->ForEachLoop([this, details](Loop* loop) -> bool {
                if(loop->IsTopLevel()) {
                    Print(loop, details);
                }

                return true;
            });
        }
    }

    void Print(Loop* loop, bool details = true) {
        NewNode(loop, *loop->GetLoopHeader()->Name(), 
                COLOR_LIGHT_BLUE, SHAPE_RECT);

        if(details) {
            loop->ForEachBackedgeBlock([this, loop](Block* block) -> bool {
                NewNode(block, *block->Name(), COLOR_LIGHT_GREEN, SHAPE_PARALLELOGRAM);
                Link(block, loop, "Backedge", COLOR_GREEN);
                return true;
            });

            loop->ForEachExitBlock([this, loop](Block* block) -> bool {
                NewNode(block, *block->Name(), COLOR_LIGHT_RED, SHAPE_PARALLELOGRAM);
                Link(loop, block, "Exit", COLOR_RED);
                return true;
            });
        }

        loop->ForEachNestedLoop([this, loop](Loop* nestedLoop) -> bool {
            Print(nestedLoop);
            Link(loop, nestedLoop);
            return true;
        });
    }               
};

} // namespace Analysis
#endif