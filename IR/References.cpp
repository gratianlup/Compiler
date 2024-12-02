// References.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Operand.hpp"
#include "References.hpp"
#include "IRType.hpp"
#include "Unit.hpp"
#include "ReferenceTable.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

void Reference::AddUser(Instruction* instr) {
	parent_->UserAdded(this, instr);
	users_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Reference::AddUser(Symbol* symbol) {
	parent_->UserAdded(this, symbol);
	users_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Reference::AddUser() {
	parent_->UserAdded(this);
	users_++;
}

// ######################################################################################
// VariableReference
// ######################################################################################
VariableReference::VariableReference(Variable* variable, const Type* type,
                                     ReferenceTable* parent) :
		Reference(type, (int)Kind::VariableRef, variable, parent) {}

VariableReference::VariableReference(GlobalVariable* variable, const Type* type, 
                                     ReferenceTable* parent) :
		Reference(type, (int)Kind::GlobalVariableRef, variable, parent) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableReference::FreeImpl() {
	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseVariableRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableReference::FreeImpl(Instruction* user) {
	parent_->UserRemoved(this, user);

	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseVariableRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void VariableReference::FreeImpl(Symbol* user) {
	parent_->UserRemoved(this, user);

	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseVariableRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string VariableReference::ToStringImpl(int level) const {
    return "VariableRef - " + *GetSymbol()->Name();
}

// ######################################################################################
// BlockReference
// ######################################################################################
BlockReference::BlockReference(Block* block, ReferenceTable* parent) : 
		Reference(nullptr /* type */, (int)Kind::BlockRef, block, parent) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockReference::FreeImpl() {
	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseBlockRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockReference::FreeImpl(Instruction* user) {
	parent_->UserRemoved(this, user);

	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseBlockRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void BlockReference::FreeImpl(Symbol* user) {
	parent_->UserRemoved(this, user);

	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseBlockRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string BlockReference::ToStringImpl(int level) const {
	return "BlockRef - " + *Target()->Name();
}

// ######################################################################################
// FunctionReference
// ######################################################################################
FunctionReference::FunctionReference(Function* function, const Type* type, 
									 ReferenceTable* parent) : 
		Reference(type, (int)Kind::FunctionRef, function, parent) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionReference::FreeImpl() {
	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseFunctionRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionReference::FreeImpl(Instruction* user) {
	parent_->UserRemoved(this, user);

	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseFunctionRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionReference::FreeImpl(Symbol* user) {
	parent_->UserRemoved(this, user);

	if((users_ == 0) || (--users_ == 0)) {
		parent_->ReleaseFunctionRef(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string FunctionReference::ToStringImpl(int level) const {
	return "FunctionRef - " + *Target()->Name();
}

} // namespace IR