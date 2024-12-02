// TypeTable.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeTable.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

TypeTable::~TypeTable() {
	// Free the memory used by all types.
	types_.ForEachValue([](const Type* type) {
		delete type;
	});

	// Free the memory of all record types.
	for(int i = 0; i < recordTypes_.Count(); i++) {
		delete recordTypes_[i];
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const PointerType* TypeTable::GetPointer(const Type* pointee) {
	DebugValidator::IsNotNull(pointee);

	// See if the pointer type already exists.
	ObjectHash hash = PointerType::GetHashCode(pointee);

	if(types_.ContainsKey(hash)) {
		return static_cast<const PointerType*>(types_[hash]);
	}
	
	// Create a new pointer type and store it.
	PointerType* pointerType = new PointerType(pointee);
	types_.Add(hash, pointerType);
	return pointerType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const ArrayType* TypeTable::GetArray(const Type* elementType, __int64 size) {
	DebugValidator::IsNotNull(elementType);
	
	// See if the pointer type already exists.
	ObjectHash hash = ArrayType::GetHashCode(elementType, size);

	if(types_.ContainsKey(hash)) {
		return static_cast<const ArrayType*>(types_[hash]);
	}
	
	// Create a new pointer type and store it.
	ArrayType* arrayType = new ArrayType(elementType, size);
	types_.Add(hash, arrayType);
	return arrayType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const FunctionType* TypeTable::GetFunction(const Type* returnType, 
                                           const Type** parameters,
										   int paramCount, bool isVarargs) {
	DebugValidator::IsNotNull(returnType);
	DebugValidator::IsTrue((paramCount == 0) || (parameters != nullptr));
	DebugValidator::IsTrue((isVarargs == false) || (paramCount > 0));
	
	// See if the pointer type already exists.
	ObjectHash hash = FunctionType::GetHashCode(returnType, parameters, 
                                                paramCount, isVarargs);

	if(types_.ContainsKey(hash)) {
		return static_cast<const FunctionType*>(types_[hash]);
	}
	
	// Create a new pointer type and store it.
	FunctionType* functionType = new FunctionType(returnType, parameters, 
                                                  paramCount, isVarargs);
	types_.Add(hash, functionType);
	return functionType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const RecordType* TypeTable::GetRecord(const List<RecordField>& fields) {
	const RecordType* recordType = new RecordType(fields);
	recordTypes_.Add(recordType);
	return recordType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const RecordType* TypeTable::GetRecord(int fieldCount) {
	const RecordType* recordType = new RecordType(fieldCount);
	recordTypes_.Add(recordType);
	return recordType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* TypeTable::AddNamed(string* name, const Type* type) {
	DebugValidator::IsNotNull(name);
	DebugValidator::IsNotNull(type);
	DebugValidator::IsFalse(typenames_.ContainsKey(name));
	typenames_.Add(name, type);
	return type;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* TypeTable::GetNamed(string* name) {
	DebugValidator::IsNotNull(name);
	const Type* type;

	if(typenames_.TryGetValue(name, &type)) {
		return type;
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TypeTable::ContainsNamed(string* name) {
	DebugValidator::IsNotNull(name);
	return typenames_.ContainsKey(name);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeTable::RemoveNamed(string* name) {
	DebugValidator::IsNotNull(name);
	DebugValidator::IsTrue(typenames_.ContainsKey(name));
	typenames_.Remove(name);
	delete name;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeTable::ClearNamed() {
	typenames_.Clear();
}

} // namespace IR