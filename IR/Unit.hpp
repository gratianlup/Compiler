// Unit.hpp
// Copyright (c) Lup Gratian
//
// An unit is a collection of variables and functions, 
// usually originating from the same source code file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_UNIT_HPP
#define PC_IR_UNIT_HPP

#include "Function.hpp"
#include "Block.hpp"
#include "SymbolTable.hpp"
#include "TypeTable.hpp"
#include "Symbols.hpp"
#include "Tagged.hpp"
#include "ConstantTable.hpp"
#include "ReferenceTable.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/LinkedList.hpp"
using namespace Base;

namespace IR {

// Base class for all unit observers.
class UnitObserver {
public:
	virtual ~UnitObserver() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual void FunctionAdded(Function* funct) {}

	virtual void FunctionRemoved(Function* funct) {}

	virtual void GlobalVariableAdded(GlobalVariable* variable) {}

	virtual void GlobalVariableRemoved(GlobalVariable* variable) {}
};


class Unit : public Tagged<Tag> {
public:
	typedef LinkedList<Function*> FunctionList;
	typedef LinkedList<GlobalVariable*> VariableList;
	typedef LinkedList<Symbol*> TypenameList;
	typedef LinkedList<Symbol*> SymbolList;
    typedef Dictionary<unsigned, Symbol*> SymbolIdMap;

	typedef SymbolList::TNode SymbolNode;
	typedef VariableList::TNode VariableNode;
	typedef FunctionList::TNode FunctionNode;
	typedef TypenameList::TNode TypenameNode;

private:
	IntrinsicTable* intrinsics_;    // The table with all available intrinsics.
	TypeTable* types_;              // Creates and manages types.
	ConstantTable* consts_;         // The table with all the constant of the program.
	SymbolTable* symbols_;          // The symbol table for all global symbols.
	ReferenceTable* refs_;          // The table with all references in the unit.
	SymbolList decls_;              // The list with all symbol declarations.
	FunctionList functions_;        // The list with all function definitions and declarations.
	VariableList variables_;        // The list with all global variables.
	TypenameList typenames_;        // The list with all type names.
	List<UnitObserver*> observers_; // The list of symbol add/remove event observers.
	shared<string> name_;           // The name of the unit (optional).
    unsigned nextSymbolId_;         // The Id of the next global symbol.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Unit(TypeTable* types, ConstantTable* consts,
		 IntrinsicTable* intrinsic, shared<string> name);

    int GetNextSymbolId() {
        return ++nextSymbolId_;
    }

	void NotifyFunctionAdded(Function* funct) {
		observers_.ForEach([funct](UnitObserver* observer) -> bool {
			observer->FunctionAdded(funct);
			return true;
		});
	}

	void NotifyFunctionRemoved(Function* funct) {
		observers_.ForEach([funct](UnitObserver* observer) -> bool {
			observer->FunctionRemoved(funct);
			return true;
		});
	}

	void NotifyGlobalVariableAdded(GlobalVariable* variable) {
		observers_.ForEach([variable](UnitObserver* observer) -> bool {
			observer->GlobalVariableAdded(variable);
			return true;
		});
	}

	void NotifyGlobalVariableRemoved(GlobalVariable* variable) {
		observers_.ForEach([variable](UnitObserver* observer) -> bool {
			observer->GlobalVariableRemoved(variable);
			return true;
		});
	}

public:
	// Creates a new Unit having the specified helper objects.
	static Unit* GetUnit(TypeTable* types, ConstantTable* consts,
						 IntrinsicTable* intrinsics, shared<string> name = nullptr);
	~Unit();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns the name of this unit.
    shared<string> Name() {
        return name_;
    }

    const shared<string> Name() const {
        return name_;
    }

    void SetName(shared<string> value) {
    	name_ = value;
    }  
    
    // Returns the table used to store all symbols.
	SymbolTable& Symbols() {
		return *symbols_;
	}

	const SymbolTable& Symbols() const {
		return *symbols_;
	}

	// Returns the list with all symbol declarations.
	SymbolList& Declarations() {
		return decls_;
	}

	const SymbolList& Declarations() const {
		return decls_;
	}

	// Returns the module that creates and manages types.
	TypeTable& Types() {
		return *types_;
	}

	const TypeTable& Types() const {
		return *types_;
	}

	// Returns the table that stores all the constant in the program.
	ConstantTable& Constants() {
		return *consts_;
	}

	const ConstantTable& Constants() const {
		return *consts_;
	}

	ReferenceTable& References() {
		return *refs_;
	}

	const ReferenceTable& References() const {
		return *refs_;
	}

	// Returns the table that contains the registered intrinsics.
	IntrinsicTable& Intrinsics() {
		return *intrinsics_;
	}

	const IntrinsicTable& Intrinsics() const {
		return *intrinsics_;
	}

	// Returns the list with all defined and declared functions.
	FunctionList& Functions() {
		return functions_;
	}

	const FunctionList& Functions() const {
		return functions_;
	}

	// Returns the list with all declared global variables.
	VariableList& Variables() {
		return variables_;
	}

	const VariableList& Variables() const {
		return variables_;
	}

	// Returns the list with all declared type names.
	TypenameList& Typenames() {
		return typenames_;
	}

	const TypenameList& Typenames() const {
		return typenames_;
	}

	// Adds the specified type name to the symbol and type tables.
	Symbol* AddTypename(Symbol* typeName, TypenameNode* typenameRef = nullptr, 
						SymbolNode* symbolRef = nullptr, 
						TypenameNode** outTypenameRef = nullptr,
						SymbolNode** outSymbolRef = nullptr);

	// Returns the number of declared type names.
	int TypenameCount() const {
		return typenames_.Count();
	}

	// Adds the specified function symbol to the unit.
	void AddFunction(Function* value, FunctionNode* functionRef = nullptr, 
					 SymbolNode* symbolRef = nullptr, 
					 FunctionNode** outFunctRef = nullptr,
					 SymbolNode** outSymbolRef = nullptr);

	// Adds the specified global variable symbol to the unit.
	void AddVariable(GlobalVariable* value,  VariableNode* variableRef = nullptr, 
					 SymbolNode* symbolRef = nullptr, 
					 VariableNode** outVariableRef = nullptr,
					 SymbolNode** outSymbolRef = nullptr);

	// Returns the number of function definitions and declarations.
	int FunctionCount() const {
		return functions_.Count();
	}

	// Returns the number of global variable declarations.
	int VariableCount() const {
		return variables_.Count();
	}

	// Returns the number of declared functions.
	int DeclaredFunctions() const;

	// Returns the number of defined functions.
	int DefinedFunctions() const {
		return functions_.Count() - DeclaredFunctions();
	}

    // Returns the symbol associated with the specified Id.
    Symbol* GetSymbolWithId(int id) {
        return symbols_->Get(id);
    }

    const Symbol* GetSymbolWithId(int id) const {
        return symbols_->Get(id);
    }

    // Removes the specified function from the unit.
    void RemoveFunction(Function* function);

    // Removes the specified global variable from the unit.
    void RemoveVariable(GlobalVariable* variable);

    // Performs the specified action on each function in the unit.
    // bool Predicate(Function* function)
    template <class Predicate>
    void ForEachFunction(Predicate action) {
        functions_.ForEach(action);
    }

    // Performs the specified action on each function in the unit.
    // bool Predicate(const Function* function) const
    template <class Predicate>
    void ForEachFunction(Predicate action) const {
        functions_.ForEach(action);
    }

    // Performs the specified action on each function 
    // that has a definition found in this unit.
    // bool Predicate(Function* function)
    template <class Predicate>
    void ForEachFunctionDefinition(Predicate action) {
        for(auto function = functions_.First(); function; function = function->Next) {
            if(function->Value->IsDefinition()) {
                if(action(function->Value) == false) {
                    return;
                }
            }
        }
    }

    // Performs the specified action on each function 
    // that has a definition found in this unit.
    // bool Predicate(const Function* function) const
    template <class Predicate>
    void ForEachFunctionDefinition(Predicate action) const {
        for(auto function = functions_.First(); function; function = function->Next) {
            if(function->Value->IsDefinition()) {
                if(action(function->Value) == false) {
                    return;
                }
            }
        }
    }

    // Performs the specified action on each function 
    // that has only a declaration in this unit.
    // bool Predicate(Function* function)
    template <class Predicate>
    void ForEachFunctionDeclaration(Predicate action) {
        for(auto function = functions_.First(); function; function = function->Next) {
            if(function->Value->IsDeclaration()) {
                if(action(function->Value) == false) {
                    return;
                }
            }
        }
    }

    // Performs the specified action on each function 
    // that has only a declaration in this unit.
    // bool Predicate(const Function* function) const
    template <class Predicate>
    void ForEachFunctionDeclaration(Predicate action) const {
        for(auto function = functions_.First(); function; function = function->Next) {
            if(function->Value->IsDeclaration()) {
                if(action(function->Value) == false) {
                    return;
                }
            }
        }
    }

    // Performs the specified action on each global variable in the unit.
    // bool Predicate(GlobalVariable* variable)
    template <class Predicate>
    void ForEachGlobalVariable(Predicate action) {
        variables_.ForEach(action);
    }

    // Performs the specified action on each global variable in the unit.
    // bool Predicate(const GlobalVariable* variable) const
    template <class Predicate>
    void ForEachGlobalVariable(Predicate action) const {
        variables_.ForEach(action);
    }

	// Associates the specified observer object with the unit.
	void AddObserver(UnitObserver* observer) {
		DebugValidator::IsNotNull(observer);
		observers_.Add(observer);
	}

	// Returns the number of observers associated with the unit.
	int ObserverCount() const {
		return observers_.Count();
	}

	// Returns the observer found at the specified position.
	UnitObserver* GetObserver(int index) {
		return observers_[index];
	}

	const UnitObserver* GetObserver(int index) const {
		return observers_[index];
	}

	// Removes the specified observer from the list.
	void RemoveObserver(UnitObserver* observer) {
		DebugValidator::IsNotNull(observer);
		observers_.Remove(observer);
	}

	// Removes the observer found at the specified position from the list.
	void RemoveObserver(int index) {
		observers_.RemoveAt(index);
	}

	// Removes all asociated observers.
	void ClearObserver() {
		observers_.Clear();
	}
};

} // namespace IR
#endif