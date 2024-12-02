#include "Unit.hpp"

namespace IR {

Unit::Unit(TypeTable* types, ConstantTable* consts,
		   IntrinsicTable* intrinsic, shared<string> name) : 
		types_(types), consts_(consts), name_(name), intrinsics_(intrinsic),
		symbols_(new SymbolTable(this)), refs_(new ReferenceTable()), nextSymbolId_(0) {
	DebugValidator::IsNotNull(types);
	DebugValidator::IsNotNull(consts);
	DebugValidator::IsNotNull(intrinsic);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unit* Unit::GetUnit(TypeTable* types, ConstantTable* consts, 
					IntrinsicTable* intrinsics, shared<string> name) {
	return new Unit(types, consts, intrinsics, name);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unit::~Unit() {
	// In order to properly free the memory we need to:
	// 1. free all the functions
	// 2. remove the initializers from the global variables
	// 3. free all remaining references
	// 4. free all global variables and typenames
	while(functions_.Count() > 0) {
		auto function = functions_.First()->Value;
		functions_.RemoveFirst();
		function->Free();
	}

	for(VariableNode* variable = variables_.First(); 
        variable; variable = variable->Next) {
		variable->Value->SetInitializer(nullptr);
	}

	while(variables_.Count() > 0) {
		auto variable = variables_.First()->Value;
		variables_.RemoveFirst();
		variable->Free();
	}

	delete refs_;
	symbols_->Clear(false /* freeSymbols */);
	delete symbols_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Symbol* Unit::AddTypename(Symbol* typeName, TypenameNode* typenameRef, 
						  SymbolNode* symbolRef, TypenameNode** outTypenameRef,
						  SymbolNode** outSymbolRef) {
	symbols_->Add(typeName);
    typeName->SetId(GetNextSymbolId());
	types_->AddNamed(typeName->Name(), typeName->GetType());

	TypenameNode* typenameNode;
	SymbolNode* symbolNode;
	
	if(typenameRef) {
        typenameNode = typenames_.AddBefore(typenameRef, typeName);
    }
	else typenameNode = typenames_.Add(typeName);

	if(symbolRef) {
        symbolNode = decls_.AddBefore(symbolRef, typeName);
    }
	else symbolNode = decls_.Add(typeName);

	if(outTypenameRef) {
        *outTypenameRef = typenameNode;
    }

	if(outSymbolRef) {
        *outSymbolRef = symbolNode;
    }

	return typeName;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Unit::AddFunction(Function* value, FunctionNode* functionRef, SymbolNode* symbolRef,
					   FunctionNode** outFunctRef, SymbolNode** outSymbolRef) {
	value->SetParentUnit(this);
	value->SetParentTable(symbols_);
    value->SetId(GetNextSymbolId());
	symbols_->Add(value);

	FunctionNode* functNode;
	SymbolNode* symbolNode;
	
	if(functionRef) {
        functNode = functions_.AddBefore(functionRef, value);
    }
	else functNode = functions_.Add(value);

	if(symbolRef) {
        symbolNode = decls_.AddBefore(symbolRef, value);
    }
	else symbolNode = decls_.Add(value);

	if(outFunctRef) {
        *outFunctRef = functNode;
    }

	if(outSymbolRef) {
        *outSymbolRef = symbolNode;
    }

	NotifyFunctionAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Unit::AddVariable(GlobalVariable* value,  VariableNode* variableRef,
                       SymbolNode* symbolRef, VariableNode** outVariableRef,
                       SymbolNode** outSymbolRef) {
	value->SetParentTable(symbols_);
    value->SetId(GetNextSymbolId());
	symbols_->Add(value);
		
	VariableNode* varNode;
	SymbolNode* symbolNode;

	if(variableRef) {
        varNode = variables_.AddBefore(variableRef, value);
    }
	else varNode = variables_.Add(value);

	if(symbolRef) {
        symbolNode = decls_.AddBefore(symbolRef, value);
    }
	else symbolNode = decls_.Add(value);

	if(outVariableRef) {
        *outVariableRef = varNode;
    }

	if(outSymbolRef) {
        *outSymbolRef = symbolNode;
    }

	NotifyGlobalVariableAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Unit::RemoveFunction(Function* function) {
    DebugValidator::IsNotNull(function);

	NotifyFunctionRemoved(function);
    functions_.Remove(function);
    symbols_->Remove(function);
    decls_.Remove(function);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Unit::RemoveVariable(GlobalVariable* variable) {
    DebugValidator::IsNotNull(variable);

	NotifyGlobalVariableRemoved(variable);
    variables_.Remove(variable);
    symbols_->Remove(variable);
    decls_.Remove(variable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Unit::DeclaredFunctions() const {
    // Count how many functions have no definition.
	int ct = 0;
		
	for(FunctionList::TNode* p = functions_.First(); p; p = p->Next) {
		if(p->Value->IsDefinition() == false) {
			ct++;
		}
	}

	return ct;
}

} // namespace IR