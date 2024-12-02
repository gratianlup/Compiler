// GlobalVariable.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "GlobalVariable.hpp"
#include "SymbolTable.hpp"
#include "References.hpp"
#include "Unit.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

Initializer::Initializer(Operand* value, __int64 adjust, InitConversion conversion,
						 const Type* conversionType) :
		value_(value), adjustment_(adjust), 
        conversion_(conversion), convType_(conversionType) {
}

Initializer::Initializer() : 
		value_(nullptr), adjustment_(0), convType_(nullptr), 
		conversion_(InitConversion::None) {}

Initializer::~Initializer() {
	// The value has been freed by 'DisassociateFrom'
	// if it was a reference.
	if(value_) {
		value_->Free();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Initializer> 
Initializer::GetInitializer(Operand* value, __int64 adjust, InitConversion conversion,
							const Type* conversionType) {
	return new Initializer(value, adjust, conversion, conversionType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Initializer::AssociateWith(GlobalVariable* variable) {
	if(auto reference = value_->As<Reference>()) {
		reference->AddUser(variable);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Initializer::DisassociateFrom(GlobalVariable* variable) {
	if(auto reference = value_->As<Reference>()) {
		reference->Free(variable);
		value_ = nullptr;
	}
}

// ######################################################################################
// InitializerList
// ######################################################################################
void InitializerList::AssociateWith(GlobalVariable* variable) {
	ForEach([variable](Initializer* init) -> bool {
		init->AssociateWith(variable);
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InitializerList::DisassociateFrom(GlobalVariable* variable) {
	ForEach([variable](Initializer* init) -> bool {
		init->DisassociateFrom(variable);
		return true;
	});
}

// ######################################################################################
// GlobalVariable
// ######################################################################################
GlobalVariable::GlobalVariable(const Type* type, const string& name, SymbolTable* parent,
							   SymbolVisibility visibility, shared<Initializer> initializer, 
							   bool listInit, bool zeroInit) :
		Variable(Kind::GlobalVariable, type, new string(name), 0, parent, visibility),
		initializer_(initializer) {
	DebugValidator::IsTrue(!zeroInit || !initializer);
	SetBit(ZERO_INIT_BIT, zeroInit);
	if(parent) parent->Add(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GlobalVariable* 
GlobalVariable::GetGlobal(const Type* type, const string& name, 
						  shared<Initializer> initializer, SymbolTable* parent, 
						  SymbolVisibility visibility) {
	return new GlobalVariable(type, name, parent, visibility, initializer, 
							  false /* listInit */, false /* zeroInit */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GlobalVariable*
GlobalVariable::GetGlobalAggregate(const Type* type, const string& name, 
								   shared<InitializerList> initList, SymbolTable* parent, 
								   SymbolVisibility visibility) {
	return new GlobalVariable(type, name, parent, visibility, initList, 
							  true /* listInit */, false /* zeroInit */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GlobalVariable*
GlobalVariable::GetZeroInitialized(const Type* type, const string& name, 
								   SymbolTable* parent, SymbolVisibility visibility) {
	return new GlobalVariable(type, name, parent, visibility, nullptr, 
							  false /* listInit */, true /* zeroInit */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VariableReference* GlobalVariable::GetReference() {
    auto parentUnit = reinterpret_cast<Unit*>(parentTable_->Parent());
    auto type = parentUnit->Types().GetPointer(GetType());
    return parentUnit->References().GetGlobalVariableRef(this, type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string GlobalVariable::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append("GlobalVar:").AppendLine(*Name());
	if(HasInitializer()) sb.AppendLine("- has initializer");

	sb.Append('\t', level).AppendLine(type_->ToString(level + 1));
	return sb.ToString();
}

} // namespace IR