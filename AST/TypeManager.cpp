// TypeManager.cpp
// Copyright (c) Lup Gratian
//
// Implement the 'TypeManager' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeManager.hpp"
#include "Types.hpp"
#include "Expression.hpp"
#include "../Base/ObjectHash.hpp"
using namespace Common;

namespace AST {

TypeManager::~TypeManager() {
	// Destroy all types.
	ptrTypes_.ForEachValue([](Type* type)            { delete type; });
	arrayTypes_.ForEachValue([](ArrayType* type)     { delete type; });
	functTypes_.ForEachValue([](FunctionType* type)  { delete type; });
	qualTypes_.ForEachValue([](QType* type)          { delete type; });
	typedefTypes_.ForEachValue([](TypedefType* type) { delete type; });

	for(int i = 0; i < otherTypes_.Count(); i++) {
		delete otherTypes_[i];
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PointerType* TypeManager::GetPointer(const Type* pointee) {
	DebugValidator::IsNotNull(pointee);
	
	// See if the pointer type already exists.
	ObjectHash hash = PointerType::GetHashCode(pointee);

	if(ptrTypes_.ContainsKey(hash)) {
		return ptrTypes_[hash];
	}
	
	// Create a new pointer type and store it.
	PointerType* pointerType = new PointerType(pointee);
	ptrTypes_.Add(hash, pointerType);
	return pointerType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FunctionType* TypeManager::GetEmptyFunction(const Type* returnType, bool isVarargs) {
	DebugValidator::IsNotNull(returnType);
	
	// See if the empty function type already exists.
	ObjectHash hash = FunctionType::GetHashCode(returnType, isVarargs, nullptr);
	
	if(functTypes_.ContainsKey(hash)) {
		return functTypes_[hash];
	}
	
	// Create a new empty function type and store it.
	FunctionType* functionType = new FunctionType(returnType, isVarargs);
	functTypes_.Add(hash, functionType);
	return functionType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FunctionType* TypeManager::GetFunction(const Type* returnType, bool isVarargs, 
									   const List<const Type*>& parameters) {
	DebugValidator::IsNotNull(returnType);
	
	// See if the empty function type already exists.
	ObjectHash hash = FunctionType::GetHashCode(returnType, isVarargs, &parameters);
	
	if(functTypes_.ContainsKey(hash)) {
		return functTypes_[hash];
	}
	
	// Create a new function type and store it.
	FunctionType* functionType = new FunctionType(returnType, parameters, isVarargs);
	functTypes_.Add(hash, functionType);
	return functionType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ArrayType* TypeManager::GetIncompleteArray(const Type* elementType, bool isStatic,
                                           Qualifier qual) {
	DebugValidator::IsNotNull(elementType);
	
	// See if the empty function type already exists.
	ObjectHash hash = ArrayType::GetHashCode(elementType, true /* incomplete */,
										     ArrayType::INVALID_SIZE, isStatic, qual);
	if(arrayTypes_.ContainsKey(hash)) {
		return arrayTypes_[hash];
	}
	
	// Create a new empty function type and store it.
	ArrayType* arrayType = new ArrayType(elementType, qual, true /* incomplete */,
										 ArrayType::INVALID_SIZE, isStatic);
	arrayTypes_.Add(hash, arrayType);
	return arrayType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ArrayType* TypeManager::GetFinalizedArray(const ArrayType* arrayType, __int64 size) {
	DebugValidator::IsNotNull(arrayType);
	DebugValidator::IsTrue(arrayType->IsIncomplete());
	
	// See if the empty function type already exists.
	ObjectHash hash = ArrayType::GetHashCode(arrayType->ElementType(), 
                                             false /* incomplete */,
										     size, arrayType->IsStatic(), 
										     arrayType->Qualifiers());
	if(arrayTypes_.ContainsKey(hash)) {
		return arrayTypes_[hash];
	}

	// Create a new array that is the finalized version of the given one.
	ArrayType* finType = new ArrayType(arrayType->ElementType(), 
                                       arrayType->Qualifiers(),
									   false /* incomplete */, size, 
                                       arrayType->IsStatic());
	arrayTypes_.Add(hash, finType);
	return finType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ArrayType* TypeManager::GetArray(const Type* elementType, __int64 size, bool isStatic, 
								 Qualifier qual) {
	DebugValidator::IsNotNull(elementType);
	
	// See if the empty function type already exists.
	ObjectHash hash = ArrayType::GetHashCode(elementType, false /* incomplete */,
										   size, isStatic, qual);
	if(arrayTypes_.ContainsKey(hash)) {
		return arrayTypes_[hash];
	}
	
	// Create a new empty function type and store it.
	ArrayType* arrayType = new ArrayType(elementType, qual, false /* incomplete */,
									     size, isStatic);
	arrayTypes_.Add(hash, arrayType);
	return arrayType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VarArrayType* TypeManager::GetVarArray(const Type* elementType, shared<Expression> sizeExpr,
									   bool isStatic, Qualifier qual) {
	DebugValidator::IsNotNull(elementType);
	
	// A new type is created for each variable array.
	return new VarArrayType(elementType, sizeExpr, qual, isStatic);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
QType* TypeManager::GetQualified(const Type* type, Qualifier qual) {
	DebugValidator::IsNotNull(type);
	
	// See if the empty function type already exists.
	ObjectHash hash = QType::GetHashCode(type, qual);
	
	if(qualTypes_.ContainsKey(hash)) {
		return qualTypes_[hash];
	}
	
	// Create a new empty function type and store it.
	QType* qualType = new QType(type, qual);
	qualTypes_.Add(hash, qualType);
	return qualType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TypedefType* TypeManager::GetTypedef(const Type* parent) {
	DebugValidator::IsNotNull(parent);
	
	// See if the empty function type already exists.
	ObjectHash hash = TypedefType::GetHashCode(parent);
	
	if(typedefTypes_.ContainsKey(hash)) {
		return typedefTypes_[hash];
	}
	
	// Create a new empty function type and store it.
	TypedefType* tdefType = new TypedefType(parent);
	typedefTypes_.Add(hash, tdefType);
	return tdefType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Type* TypeManager::GetElementPointer(const ArrayType* type) {
	DebugValidator::IsNotNull(type);
	
	// Construct a pointer type that points either to the element type
	// or to the qualified version of the element type.
	// C99:6.7.3.8 says that the qualifiers should be applied to the element,
	// not to the pointer type.
	if(type->Qualifiers().HasNone()) {
		return GetPointer(type->ElementType());
	}
	else return GetPointer(GetQualified(type->ElementType(), type->Qualifiers()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
EnumType* TypeManager::GetEnum(const BasicType* constType) {
	// A separate type is created for each enum.
	EnumType* enumType = new EnumType(constType);
	otherTypes_.Add(enumType);
	return enumType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StructType* TypeManager::GetStruct() {
	// A separate type is created for each enum.
	StructType* structType = new StructType();
	otherTypes_.Add(structType);
	return structType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UnionType* TypeManager::GetUnion() {
	// A separate type is created for each enum.
	UnionType* unionType = new UnionType();
	otherTypes_.Add(unionType);
	return unionType;
}

} // namespace AST