// SymbolTable.cpp
// Copyright (c) Lup Gratian
//
// Implements the SymbolTable class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SymbolTable.hpp"
#include "Variable.hpp"
#include "Function.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include <iostream>
using namespace Base;

namespace IR {

SymbolTable::~SymbolTable() {
	// Free the memory used by all symbols.
	Clear();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Symbol* SymbolTable::Add(Symbol* symbol) {
	DebugValidator::IsNotNull(symbol);
	
    if(symbol->Id() != 0) {
	    if(symbol->HasName()) {
		    nameTable_.Add(symbol->Name(), symbol);
	    }

        idTable_.Add(symbol->Id(), symbol);
    }

	return symbol;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool  SymbolTable::Contains(const string* name) const {
	DebugValidator::IsNotNull(name);
	return nameTable_.ContainsKey(name);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Symbol* SymbolTable::Get(const string* name) {
	DebugValidator::IsNotNull(name);
	Symbol* result;

	if(nameTable_.TryGetValue(name, &result)) {
		return result;
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Symbol* SymbolTable::Get(const string* name) const {
	DebugValidator::IsNotNull(name);
	Symbol* result;

	if(nameTable_.TryGetValue(name, &result)) {
		return result;
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Symbol* SymbolTable::Get(int id) {
    Symbol* result;

    if(idTable_.TryGetValue(id, &result)) {
        return result;
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Symbol* SymbolTable::Get(int id) const {
    Symbol* result;

    if(idTable_.TryGetValue(id, &result)) {
        return result;
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolTable::Remove(const string* name) {
	DebugValidator::IsNotNull(name);
	DebugValidator::IsTrue(nameTable_.ContainsKey(name));

    Symbol* symbol = nameTable_[name];
	nameTable_.Remove(name);
    idTable_.Remove(symbol->Id());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolTable::Remove(int id) {
    DebugValidator::IsTrue(idTable_.ContainsKey(id));

    Symbol* symbol = idTable_[id];
    idTable_.Remove(id);

    if(symbol->HasName()) {
        nameTable_.Remove(symbol->Name());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolTable::Remove(Symbol* symbol) {
	DebugValidator::IsNotNull(symbol);
    DebugValidator::IsTrue(idTable_.ContainsKey(symbol->Id()));
	
	if(symbol->HasName()) {
		nameTable_.Remove(symbol->Name());
	}
	
    idTable_.Remove(symbol->Id());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SymbolTable::Clear(bool freeSymbols) {
	if(freeSymbols) {
		idTable_.ForEachValue([](Symbol* symbol) -> bool {
			// We need to free the memory used by the symbol.
			symbol->Free();
			return true;
		});
	}

	idTable_.Clear();
	nameTable_.Clear();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string SymbolTable::Dump(bool showParams, int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendFormat(L"Symbol table: %d", Count());

	nameTable_.ForEachValue([&sb, level, showParams](Symbol* symbol) -> bool {
		if(!showParams && symbol->IsLocalVariable()) {
			auto temp = symbol->As<Variable>();
			if(temp->IsParameter()) return true; // Skip over it.
		}

		sb.Append('\t', level + 1).AppendLine(symbol->ToString(level + 1));
		sb.AppendLine();
		return true;
	});

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void SymbolTable::Dump() const {
	std::wcout<<Dump(true, 0).Chars();
}

} // namespace IR