// Type.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IRType.hpp"
#include "IRTypes.hpp"
#include <iostream>

namespace IR {

bool Type::IsInt8() const {
	if(auto temp = As<IntegerType>()) {
		return temp->IsInt8();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsInt16() const {
	if(auto temp = As<IntegerType>()) {
		return temp->IsInt16();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsInt32() const {
	if(auto temp = As<IntegerType>()) {
		return temp->IsInt32();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsInt64() const {
	if(auto temp = As<IntegerType>()) {
		return temp->IsInt64();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsFloat() const {
	if(auto temp = As<FloatingType>()) {
		return temp->IsFloat();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsDouble() const {
	if(auto temp = As<FloatingType>()) {
		return temp->IsDouble();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void Type::Dump() const {
	std::wcout<<ToString(0).Chars();
}

} // namespace IR