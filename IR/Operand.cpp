// Operands.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Operand.hpp"
#include "Constants.hpp"
#include "References.hpp"
#include "Variable.hpp"
#include "GlobalVariable.hpp"
#include "Function.hpp"
#include "Temporary.hpp"
#include "IRType.hpp"
#include "Unit.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

Operand::Operand(const Type* type, int kind, int other) : 
		type_(type), kind_(kind), other_(other), isBool_(false) {}

Operand::Operand(const Type* type, int kind, Instruction* definingInstr, int other) : 
		type_(type), kind_(kind), other_(other), 
		defInstr_(definingInstr), isBool_(0) {}

Operand::Operand(const Type* type, int kind, Symbol* symbol, int other) : 
		type_(type), kind_(kind), other_(other), 
		symbol_(symbol), isBool_(0) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Operand::IsZeroInt() const {
	if(auto temp = As<IntConstant>()) {
		return temp->IsZero();
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Operand::IsOneInt() const {
	if(auto temp = As<IntConstant>()) {
        return temp->IsOne();
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Operand::IsMinusOneInt() const {
	if(auto temp = As<IntConstant>()) {
		return temp->IsMinusOne();
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Operand::IsBoolean() const {
	return isBool_ || IsOneInt() || IsZeroInt();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Operand::IsIntConstant() const {
	return IsConstant() && Is<IntConstant>();
}

bool Operand::IsFloatingConstant() const {
	return IsConstant() && Is<FloatConstant>();
}

bool Operand::IsStringConstant() const {
	return IsConstant() && Is<StringConstant>();
}

bool Operand::IsNullConstant() const {
	return IsConstant() && Is<NullConstant>();
}

bool Operand::IsUndefinedConstant() const {
	return IsConstant() && Is<UndefinedConstant>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Operand::HasSingleUser() const {
	return IsTemporary() && (As<Temporary>()->UserCount() == 1);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Operand::ToStringImpl(int level) const {
	if(IsTemporary()) {
		return "temp";
	}
	else if(IsVariableReference() && HasSymbol() && GetSymbol()->HasName()) {
		return "variable - " + *GetSymbol()->Name();
	}
	else if(IsGlobalVariableRef() && HasSymbol() && GetSymbol()->HasName()) {
		return "global - " + *GetSymbol()->Name();
	}
	else return "";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void Operand::Dump() const {
    ObjectDumper(ToString(0).Chars(), "Operand");
}

} // namespace IR