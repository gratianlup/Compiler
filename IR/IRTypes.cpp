// Types.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IRTypes.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

const shared<IntegerType> IntegerType::types_[4] = {
	new IntegerType(IRIntegerKind::Int8),
	new IntegerType(IRIntegerKind::Int16),
	new IntegerType(IRIntegerKind::Int32),
	new IntegerType(IRIntegerKind::Int64)
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string IntegerType::ToStringImpl(int level) const {
	string s = string('\t', level);

	switch((IRIntegerKind)other_) {
		case IRIntegerKind::Int8:  { s += "int8"; break;  }
		case IRIntegerKind::Int16: { s += "int16"; break; }
		case IRIntegerKind::Int32: { s += "int32"; break; }
		case IRIntegerKind::Int64: { s += "int64"; break; }
	}

	return s;
}

// ######################################################################################
// FloatingType
// ######################################################################################
const shared<FloatingType> FloatingType::types_[2] = {
	new FloatingType(IRFloatingKind::Float),
	new FloatingType(IRFloatingKind::Double)
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string FloatingType::ToStringImpl(int level) const {
	string s = string('\t', level);

	switch((IRFloatingKind)other_) {
		case IRFloatingKind::Float:  { s += "float"; break;  }
		case IRFloatingKind::Double: { s += "double"; break; }
	}

	return s;
}

// ######################################################################################
// VoidType
// ######################################################################################
const shared<VoidType> VoidType::instance_ = new VoidType();

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string VoidType::ToStringImpl(int level) const {
	string s = string('\t', level);
	return s + "void";
}

// ######################################################################################
// PointerType
// ######################################################################################
string PointerType::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));

    if(pointee_->IsRecord()) {
        sb.AppendLine("Pointer: RECORD");
    }
    else {
	    sb.AppendLine("Pointer: ");
	    sb.Append(pointee_->ToString(level + 1));
    }
	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash PointerType::GetHashCode(const Type* pointee) {
	return ObjectHash().Add(Kind::Pointer).AddObject(pointee);
}

// ######################################################################################
// ArrayType
// ######################################################################################
string ArrayType::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));

	sb.AppendLine("Array: ");
	sb.AppendLine(elemType_->ToString(level + 1));
	sb.Append('\t', level);
	sb.AppendFormat(L"Size: %d", size_);
	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash ArrayType::GetHashCode(const Type* elementType, __int64 size) {
	return ObjectHash().Add(Kind::Array).AddObject(elementType).Add(size);
}

// ######################################################################################
// RecordType
// ######################################################################################
string RecordType::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));

	sb.AppendLine("Record: ");

	for(int i = 0; i < fields_.Count(); i++) {
		sb.Append('\t', level + 1);
		sb.AppendFormat(L"Field #%d:", i);
		sb.Append('\t', level + 2).AppendLine("Type: ");
		sb.Append(fields_[i].FieldType->ToString(level + 2));
		sb.Append('\t', level + 2).AppendFormat(L"Offset: %d", fields_[i].FieldOffset);

	}

	return sb.ToString();
}

// ######################################################################################
// FunctionType
// ######################################################################################
string FunctionType::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));

	sb.Append("Function: ");

	if(IsVarargs()) sb.Append("varargs");
	sb.AppendLine();

	sb.Append('\t', level + 1).AppendLine("Return type: ");
	sb.AppendLine(returnType_->ToString(level + 2));

	for(int i = 0; i < params_.Count(); i++) {
		sb.Append('\t', level + 1);
		sb.AppendFormat(L"Param #%d:", i).AppendLine();
		sb.AppendLine(params_[i]->ToString(level + 2));
	}

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash FunctionType::GetHashCode(const Type* returnType, const Type** parameters, 
								     int paramCount, bool isVarargs) {
	ObjectHash hash;
	hash.Add(Kind::Function);
	hash.AddObject(returnType);
	hash.Add(paramCount);
	hash.Add(isVarargs);

	for(int i = 0; i < paramCount; i++) {
		hash.AddObject(parameters[i]);
	}

	return hash;
}

}// namespace IR