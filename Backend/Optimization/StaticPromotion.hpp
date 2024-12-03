// StaticPromotion.hpp
// Copyright (c) Lup Gratian
//
// Tries to promote global functions/variables that are
// visible externally to 'static' (internal to the unit)
// by searching for the program entry point.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_STATIC_PROMOTION_HPP
#define PC_OPTIMIZATION_STATIC_PROMOTION_HPP

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Analysis/CallGraph.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class StaticPromotion : public Pass {
private:
    // Tries to find the unique entry point of the program
    // as defined by the available language information.
    Function* FindUniqueEntryPoint(Unit* unit);

    // Promotes all functions (except the entry point) to 'static'.
    void PromoteFunctionsToStatic(CallGraph* callGraph, Function* entryPoint);

    // Promotes all functions (except the entry point) to 'static'.
    // This version doesn't update the Call Graph.
    void PromoteFunctionsToStatic(Unit* unit, Function* entryPoint);

    // Promotes the specified function to 'static' and removes
    // the potential call from the External node in the Call Graph.
    void PromoteFunctionToStatic(CallNode* callNode, Function* entryPoint);

    // Promotes all variables in the unit to 'static'.
    void PromoteVariablesToStatic(Unit* unit);

public:
    void Execute(Unit* unit, CallGraph* callGraph);

    void Execute(Unit* unit);
};

} // namespace Optimization 
#endif