// TypeCombiner.hpp
// Copyright (c) Lup Gratian
//
// Defines methods for combining types with the same kind.
// Used to combine multiple declarations and test for compatibility.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_TYPE_COMBINER_HPP
#define PC_AST_TYPE_COMBINER_HPP

#include "../Base/SharedPointer.hpp"
#include "../Common/Diagnostic.hpp"
#include "Types.hpp"
#include "TypeManager.hpp"
using namespace Base;
using namespace Common;

namespace AST {

// Used to report the cause of failure when combining functions.
enum class FunctionCombineResult {
	OK, 
	IncompatibleReturn,
	ParamNumber,
	ParamsIncompatible,
	Varargs,
	Promotion
};


class TypeCombiner {
private:
	TypeManager* types_;

public:
	TypeCombiner(TypeManager* manager) : types_(manager) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Tries to combine two enum's or an enum and a basic type.
	Type* CombineEnums(const EnumType* a, const Type* b);

	// Tries to combine two array types (and their element types).
	Type* CombineArrays(const ArrayType* a, const ArrayType* b);

	// Tries to combine two function types (return types and parameters).
	Type* CombineFunctions(const FunctionType* a, const FunctionType* b, 
						   FunctionCombineResult* result = nullptr);
	
	// Tries to combine two functions that have a prototype.
	Type* CombineFunctionsWithProto(const FunctionType* a, const FunctionType* b, 
									const Type* newRet, 
                                    FunctionCombineResult* result = nullptr);

	// Tries to combine two types (handles all cases).
	Type* Combine(const Type* a, const Type* b);
};

} // namespace AST
#endif