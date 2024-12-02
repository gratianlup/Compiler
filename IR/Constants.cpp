// Constants.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Operand.hpp"
#include "Constants.hpp"
#include "IRType.hpp"
#include "Unit.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

Constant::Constant(ConstantKind kind, const Type* type) :
		Operand(type, (int)Kind::Constant, (int)kind) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsigned Constant::GetHashCode() const {
	// Combine the operand kind with the constant kind and the type.
	// Uses the FNV hash algorithm. 
	// Taken from 'http://www.eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx'
	unsigned hash = 2166136261;
	hash = (hash * 16777619) ^ kind_;
	hash = (hash * 16777619) ^ other_;
	hash = (hash * 16777619) ^ (unsigned)type_;

	if(sizeof(Type*) == 8) {
		// For 64 bit pointers.
		hash = (hash * 16777619) ^ (unsigned)((size_t)type_ >> 32);
	}

	return hash;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Constant::IsEqualTo(const Constant& other) const {
	// The operand type, constant type, and IR Type must be the same.
	if(kind_ != other.kind_) return false;
	if(other_ != other.other_) return false;
	if(type_ != other.type_) return false;

	return true;
}

// ######################################################################################
// IntConstant
// ######################################################################################
IntConstant::IntConstant(const Type* type, __int64 value) :
		Constant(ConstantKind::Integer, type), value_(value) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsigned IntConstant::GetHashCode() const {
	unsigned hash = 2166136261;
	hash = (hash ^ 16777619) + Constant::GetHashCode();
	hash = (hash ^ 16777619) + (unsigned)value_;
	hash = (hash ^ 16777619) + (unsigned)(value_ >> 32);
	return hash;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IntConstant::IsEqualTo(const Constant& other) const {
	if(Constant::IsEqualTo(other) == false) return false;

	// The values should be the same.
	auto otherIntConst = static_cast<const IntConstant*>(&other);
	return value_ == otherIntConst->value_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string IntConstant::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.AppendFormat(L"IntConst - %d", value_);
	return sb.ToString();
}

// ######################################################################################
// FloatConstant
// ######################################################################################
FloatConstant::FloatConstant(const Type* type, double value) :
		Constant(ConstantKind::Floating, type), value_(value) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsigned FloatConstant::GetHashCode() const {
	unsigned hash = 2166136261;
	hash = (hash * 16777619) ^ Constant::GetHashCode();
	hash = (hash * 16777619) ^ (unsigned)value_;
	hash = (hash * 16777619) ^ (unsigned)((__int64)value_ >> 32);
	return hash;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FloatConstant::IsEqualTo(const Constant& other) const {
	if(Constant::IsEqualTo(other) == false) return false;

	// The values should be exactly the same.
	auto otherFloatConst = static_cast<const FloatConstant*>(&other);
	return value_ == otherFloatConst->value_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string FloatConstant::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.AppendFormat(L"FloatConst - %f", value_);
	return sb.ToString();
}

// ######################################################################################
// StringConstant
// ######################################################################################
StringConstant::StringConstant(const Type* type, const StringBuffer& value) :
		Constant(ConstantKind::String, type), value_(value) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsigned StringConstant::GetHashCode() const {
	// The two values to be combined are hash code, so use a simple XOR.
	return Constant::GetHashCode() ^ value_.GetHashCode();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StringConstant::IsEqualTo(const Constant& other) const {
	if(Constant::IsEqualTo(other) == false) return false;

	// The values should be exactly the same (do not approximate).
	auto otherStrConst = static_cast<const StringConstant*>(&other);
	return value_ == otherStrConst->value_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string StringConstant::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.AppendFormat(L"StringConstant - %s", value_.Data());
	return sb.ToString();
}

// ######################################################################################
// NullConstant
// ######################################################################################
unsigned NullConstant::GetHashCode() const {
	return Constant::GetHashCode();
}

// ######################################################################################
// UndefinedConstant
// ######################################################################################
unsigned UndefinedConstant::GetHashCode() const {
	return Constant::GetHashCode();
}

} // namespace IR