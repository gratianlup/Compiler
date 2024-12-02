// DeadGlobalElimination.hpp
// Copyright (c) Lup Gratian
//
// Implements the DeadGlobalElimination class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DeadGlobalElimination.hpp"

namespace Optimization {

void DeadGlobalElimination::Execute(Unit* unit) {
    // Build a list of all functions and global variables that are
    // definitely dead. Until the worklist is empty, take a symbol
    // from the worklist and if it is dead add all global symbols 
    // used by it to the worklist (because they might become dead too),
    // then delete the symbol.
    if(FindInitialCandidates(unit) == false) {
        return;
    }

    while(worklist_.IsNotEmpty()) {
        auto symbol = worklist_.RemoveLast();
        inWorklist_.ResetBit(symbol->Id());

        if(IsDefinitelyDead(symbol)) {
            if(auto function = symbol->As<Function>()) {
                AddUsedGlobalSymbols(function);
                unit->RemoveFunction(function);
            }
            else if(auto variable = symbol->As<GlobalVariable>()) {
                AddUsedGlobalSymbols(variable);
                unit->RemoveVariable(variable);
            }
            else DebugValidator::Unreachable();

            symbol->Free();
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadGlobalElimination::FindInitialCandidates(Unit* unit) {
    // Add to the worklist any function and global variable
    // that is definitely not used anymore.
    unit->ForEachFunction([this](Function* function) -> bool {
        if(IsDefinitelyDead(function)) {
            worklist_.Add(function);
            inWorklist_.SetBit(function->Id());
        }

        return true;
    });

    unit->ForEachGlobalVariable([this](GlobalVariable* variable) -> bool {
        if(IsDefinitelyDead(variable)) {
            worklist_.Add(variable);
            inWorklist_.SetBit(variable->Id());
        }

        return true;
    });

    return worklist_.IsNotEmpty();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadGlobalElimination::IsDefinitelyDead(Symbol* symbol) {
    // Symbols visible to external units might still be used.
    if(symbol->IsExtern()) {
        return false;
    }

    // If there is a Global Users tag attached use it,
    // else look at the number of users the reference has.
    GlobalUsersTag* usersTag = nullptr; 
    Reference* reference = nullptr;

    if(auto function = symbol->As<Function>()) {
        usersTag = function->GetTag<GlobalUsersTag>();

        if(usersTag == nullptr) {
            reference = function->GetReference();
        }
    }
    else if(auto variable = symbol->As<GlobalVariable>()) {
        usersTag = variable->GetTag<GlobalUsersTag>();

        if(usersTag == nullptr) {
            reference = variable->GetReference();
        }
    }
    else return false;

    if(usersTag) {
        return usersTag->HasNoUsers();
    }
    
    // Use the reference to answer otherwise.
    if(reference->UserCount() == 0) {
        reference->Free(); // Not used anywhere, free it.
        return true;
    }
    else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadGlobalElimination::AddUsedGlobalSymbols(Function* function) {
    // The function is going to be deleted, creating potentially 
    // new dead functions and global variables. Add to the worklist
    // any global symbol used by the function.
    if(function->IsDeclaration()) {
        return;
    }

    function->ForEachInstruction([this](Instruction* instr) -> bool {
        // Arithmetic and logical instructions can't use
        // references directly, ignore them.
        if(instr->IsArithmetic() || instr->IsLogical()) {
            return true;
        }

        for(int i = 0; i < instr->SourceOpCount(); i++) {
            if(auto reference = instr->GetSourceOp(i)->As<Reference>()) {
                AddSymbolToWorklist(reference->GetSymbol());
            }
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadGlobalElimination::AddUsedGlobalSymbols(GlobalVariable* variable) {
    // A global variable that has an initializer might
    // use the address of some functions or other global variables,
    // which might become dead after this variable is removed.
    if((variable->HasInitializer() == false) ||
       variable->HasZeroInitializer()) {
        return;
    }

    AddUsedGlobalSymbols(variable->GetInitializer());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadGlobalElimination::AddUsedGlobalSymbols(Initializer* initializer) {
    if(initializer->IsInitializerList()) {
        auto list = static_cast<InitializerList*>(initializer);

        for(int i = 0; i < list->Count(); i++) {
            AddUsedGlobalSymbols((*list)[i]);
        }
    }
    else if(auto reference = initializer->Value()->As<Reference>()) {
        AddSymbolToWorklist(reference->GetSymbol());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadGlobalElimination::AddSymbolToWorklist(Symbol* symbol) {
    // If the referred symbol is one of interest
    // add it to the worklist, if not already there.
    if(symbol->IsFunction() || symbol->IsGlobalVariable()) {
        if(inWorklist_.IsNotSet(symbol->Id())) {
            worklist_.Add(symbol);
            inWorklist_.SetBit(symbol->Id());
        }
    }
}

} // namespace Optimization