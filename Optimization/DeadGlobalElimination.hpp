// DeadGlobalElimination.hpp
// Copyright (c) Lup Gratian
//
// Implements an aggressive algorithm that deletes dead (unused) 
// functions and global variables.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_DEAD_GLOBAL_ELIMINATION_HPP
#define PC_OPTIMIZATION_DEAD_GLOBAL_ELIMINATION_HPP

#include "../Analysis/GlobalUsersTag.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
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

class DeadGlobalElimination : public Pass {
private:
    typedef StaticList<Symbol*, 32> SymbolList;

    SymbolList worklist_;
    SparseBitVector inWorklist_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns 'true' if the specified symbol is not used
    // in the unit where it is defined and in other external units.
    bool IsDefinitelyDead(Symbol* symbol);
    
    // Finds the functions and global variables that
    // are definitely dead and adds them to the worklist.
    bool FindInitialCandidates(Unit* unit);

    // Adds to the worklist any function or global variable used
    // because they might become dead after the function is removed.
    void AddUsedGlobalSymbols(Function* function);

    // Adds to the worklist any function or global variable used
    // as part of the initializer because they might become dead 
    // after the global variable is removed.
    void AddUsedGlobalSymbols(GlobalVariable* variable);

    // Adds to the worklist any function or global variable used
    // by the initializer or by any of its children.
    void AddUsedGlobalSymbols(Initializer* initializer);

    // Adds the specified symbol to the worklist, 
    // in case it's not already there.
    void AddSymbolToWorklist(Symbol* symbol);

public:
    void Execute(Unit* unit);
};

} // namespace Optimization
#endif