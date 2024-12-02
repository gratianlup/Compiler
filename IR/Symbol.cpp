// Symbol.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Symbol.hpp"
#include "SymbolTable.hpp"
#include "../Base/DebugValidator.hpp"
#include <iostream>
using namespace Base;

namespace IR {

Symbol::Symbol(Kind kind, const Type* type, shared<string> name, 
			   SymbolTable* parent, SymbolVisibility visibility) :
		kind_((unsigned char)kind), type_(type), name_(name), parentTable_(parent), 
		visibility_((unsigned char)visibility), alignment_(0), other_(0),
		dllVisibility_((unsigned char)DllVisibility::None), addressTaken_(1) {
    // By default global variables and function are presumed
    // to have their address taken.
	if(parent && ((kind != Kind::Block) && (kind != Kind::Function))) {
		parent->Add(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Symbol* Symbol::GetTypename(const Type* type, const string& name, SymbolTable* parent) {
	return new Symbol(Kind::TypeName, type, new string(name), parent);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Symbol::MoveTo(SymbolTable* other) {
	DebugValidator::IsNotNull(other);
	DebugValidator::IsNotNull(parentTable_);
	DebugValidator::IsNotNull(name_.Raw());
	DebugValidator::IsFalse(other->Contains(name_));
	// Remove the symbol from the current table and place it into the other one.
	parentTable_->Remove(this);
	other->Add(this);
	parentTable_ = other;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Symbol::RemoveFromParent() {
	DebugValidator::IsNotNull(parentTable_);
	parentTable_->Remove(this);
	parentTable_ = nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void Symbol::Dump() const {
	std::wcout<<ToString(0).Chars();
}

} // namespace IR