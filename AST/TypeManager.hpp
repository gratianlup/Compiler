// TypeManager.hpp
// Copyright (c) Lup Gratian
//
// Creates, stores and frees the types used in the AST.
// Basic types are should be obtained directly from the 'BasicType' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_TYPE_MANAGER_HPP
#define PC_AST_TYPE_MANAGER_HPP

#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/List.hpp"
#include "../Base/ObjectHash.hpp"
#include "Qualifier.hpp"
using namespace Base;

namespace AST {

// Forward declarations.
class Type;
class BasicType;
class PointerType;
class ArrayType;
class VarArrayType;
class FunctionType;
class QType;
class TypedefType;
class EnumType;
class StructType;
class UnionType;
class Expression;

class TypeManager {
private:
	Dictionary<ObjectHash, PointerType*> ptrTypes_;
	Dictionary<ObjectHash, ArrayType*> arrayTypes_;
	Dictionary<ObjectHash, FunctionType*> functTypes_;
	Dictionary<ObjectHash, QType*> qualTypes_;
	Dictionary<ObjectHash, TypedefType*> typedefTypes_;
	List<Type*> otherTypes_; // Used for struct/union/enum.

public:
	~TypeManager();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns a pointer type that points to the specified pointee.
	PointerType* GetPointer(const Type* pointee);

	// Returns a function type with no parameters that returns the specified type.
	FunctionType* GetEmptyFunction(const Type* returnType, bool isVarargs);

	// Returns a function type with parameters that returns the specified type.
	FunctionType* GetFunction(const Type* returnType, bool isVarargs,
							  const List<const Type*>& parameters);

	// Returns an incomplete array type with the specified element type.
	ArrayType* GetIncompleteArray(const Type* elementType, bool isStatic, Qualifier qual);
	
	// Returns an array type that is the finalized version of the specified array.
	// Used when an array is finalized by an initializer, like in 'int a[] = {1,2};'.
	ArrayType* GetFinalizedArray(const ArrayType* arrayType, __int64 size);

	// Returns a fixed-length array type having the specified element type and size.
	ArrayType* GetArray(const Type* elementType, __int64 size, bool isStatic, Qualifier qual);

	// Returns a variable-length array type  the specified element type and size expression.
	VarArrayType* GetVarArray(const Type* elementType, shared<Expression> sizeExpr,
						      bool isStatic, Qualifier qual);

	// Returns a type representing the qualified version of the specified type.
	QType* GetQualified(const Type* type, Qualifier qual);

	// Returns a type name type for the specified type.
	TypedefType* GetTypedef(const Type* parent);

	// Returns a pointer to the array element, with any qualifiers applied.
	Type* GetElementPointer(const ArrayType* type);

	// Returns an enum type that uses the specified type to store it's constants.
	EnumType* GetEnum(const BasicType* constType);

	// Returns a new struct type.
	StructType* GetStruct();

	// Returns a new union type.
	UnionType* GetUnion();
};

} // namespace AST
#endif