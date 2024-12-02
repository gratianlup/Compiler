// TypeCombiner.cpp
// Copyright (c) Lup Gratian
//
// Implements the TypeCombiner class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeCombiner.hpp"

namespace AST {

Type* TypeCombiner::CombineEnums(const EnumType* a, const Type* b) {
	// The types can be combined if either both are enums
	// or the first is an enum and the second one a BasicType.
	if(auto temp = b->As<EnumType>()) {
		// The type used to represent the enums should be compatible.
		return Combine(a->ConstType(), temp->ConstType());
	}

	const BasicType* basic = b->As<BasicType>();

	if(basic == nullptr) {
		// The other type is not a basic type, so no match is possible.
		return nullptr;
	}

	auto enumConst = a->ConstType()->As<BasicType>();

	if(enumConst->IsChar()      && basic->IsChar()) return BasicType::GetChar();
	else if(enumConst->IsInt()  && basic->IsInt())  return BasicType::GetInt();
	else if(enumConst->IsUInt() && basic->IsUInt()) return BasicType::GetUInt();
	else return nullptr; // Other promotions are not valid (C99:6.2.5.17).
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Type* TypeCombiner::CombineArrays(const ArrayType* a, const ArrayType* b) {
	// The element types are combined first.
	Type* result = nullptr;
	Type* newElement = Combine(a->ElementType(), b->ElementType());

	if(newElement == nullptr) {
		// Could not combine, give up;
		return nullptr;
	}

	// C99:6.2.7.3: if one type is a constant array, the result
	// is the constant array. Else if it's an VLA, the result is the VLA.
	// The size of the arrays must match if they are constant.
	if(a->IsVariable() == false) {
		// a has a fixed size.
		if(b->IsVariable()) {
			// const + var = const.
			result = types_->GetArray(newElement, a->Size(),
                                      a->IsStatic(), a->Qualifiers());
		}
		else {
			// const + const = const. The length should match.
			// If one is incomplete the other one is considered.
			if(b->IsIncomplete() || (a->Size() == b->Size())) {
				result = types_->GetArray(newElement, a->Size(), 
                                          a->IsStatic(), a->Qualifiers());
			}
			else return nullptr; // The length doesn't match.
		}
	}
	else {
		// b has fixed size.
		if(a->IsVariable()) {
			// const + var = const.
			result = types_->GetArray(newElement, b->Size(), 
                                      b->IsStatic(), b->Qualifiers());
		}
		else {
			// const + const = const. The length should match. 
			// If one is incomplete the other one is considered.
			if(a->IsIncomplete() || (a->Size() == b->Size())) {
				result = types_->GetArray(newElement, b->Size(), 
                                          b->IsStatic(), b->Qualifiers());
			}
			else return nullptr; // The length doesn't match.
		}
	}
	
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Type* TypeCombiner::CombineFunctions(const FunctionType* a, const FunctionType* b,
									 FunctionCombineResult* combinedResult) {
    // If both types are the same there is nothing to combine.
    if(a == b) {
        if(combinedResult) {
			*combinedResult = FunctionCombineResult::OK;
		}
			
        return const_cast<FunctionType*>(a);
    }

	// Combine the return types (they should be compatible).
	Type* newReturn = Combine(a->ReturnType(), b->ReturnType());

	if(newReturn == nullptr) {
		if(combinedResult) {
			*combinedResult = FunctionCombineResult::IncompatibleReturn;
		}
		
		return nullptr;
	}

	// There are separate rules depending on the presence of the prototype.
	// If both functions have prototypes the parameters are combined.
	if(a->HasPrototype() && b->HasPrototype()) {
		return CombineFunctionsWithProto(a, b, newReturn, combinedResult);
	}

	// If both functions don't have prototype we're done.
	if(a->HasPrototype() == b->HasPrototype()) {
        if(combinedResult) {
			*combinedResult = FunctionCombineResult::OK;
		}
		
		return types_->GetEmptyFunction(newReturn, a->IsVarargs());
	}

	// Figure out which is the function with the prototype and validate it's parameters.
	const FunctionType* proto = a->HasPrototype() ? a : b;
	auto& parameters = proto->Parameters();

	// The prototype should not be varargs ('f(int, ...)' not compatible with 'f()').
	if(proto->IsVarargs()) {
		if(combinedResult) {
			*combinedResult = FunctionCombineResult::Varargs;
		}
		
		return nullptr;
	}

	// Each parameter must be compatible with it's default promotion (C99:6.7.5.3.15).
	// If the type is 'float' or it can't be promoted to 'int' the functions
	// can't be combined. Special check for 'enum'.
	for(int i = 0; i < parameters.Count(); i++) {
		const Type* parameterType = parameters[i];

		if(auto temp = parameterType->As<EnumType>()) {
			parameterType = temp->ConstType();
		}

		if(parameterType->CanPromoteToInt() ||
		  (parameterType->IsFloating() && parameterType->As<BasicType>()->IsFloat())) {
			if(combinedResult) {
				*combinedResult = FunctionCombineResult::Varargs;
			}
			
			return nullptr;
		}
	}

	// Create the combined function type.
	if(combinedResult) {
		*combinedResult = FunctionCombineResult::OK;
	}
	
	return types_->GetFunction(newReturn, false /* isVarargs */, proto->Parameters());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Type* TypeCombiner::CombineFunctionsWithProto(const FunctionType* a, 
                                              const FunctionType* b,
											  const Type* newReturn, 
                                              FunctionCombineResult* combinedResult) {
	// If the previous declaration is Varargs this should be too.
	if(a->IsVarargs() != b->IsVarargs()) {
		if(combinedResult) {
			*combinedResult = FunctionCombineResult::Varargs;
		}
			
		return nullptr;
	}
	
	// The number of parameters should be equal (C99:6.7.5.3.15).
	if(a->ParameterCount() != b->ParameterCount()) {
		if(combinedResult) {
			*combinedResult = FunctionCombineResult::ParamNumber;
		}
		
		return nullptr;
	}
	
	// Try to combine the parameters.
	List<const Type*> parameters(a->ParameterCount());
	auto& firstParams  = a->Parameters();
	auto& secondParams = b->Parameters();

	for(int i = 0; i < firstParams.Count(); i++) {
		const Type* result = Combine(firstParams[i], secondParams[i]);

		if(result == nullptr) {
			// The parameter types could not be combined.
			if(combinedResult) {
				*combinedResult = FunctionCombineResult::ParamsIncompatible;
			}
			
			return nullptr;
		}

		// Replace the type of the parameter with the new one.
		parameters.Add(result);
	}

	if(combinedResult) {
		*combinedResult = FunctionCombineResult::OK;
	}
	
	return types_->GetFunction(newReturn, a->IsVarargs(), parameters);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Type* TypeCombiner::Combine(const Type* a, const Type* b) {
	// See C99:6.2.7.3 for all the rules.
	// The types should be equally qualified.
	const QType* qa = a->As<QType>();
	const QType* qb = b->As<QType>();

	if(qa != qb) {
		// One of the types has no qualifier.
		return nullptr;
	}
	else if(qa) {
		if(qa->GetQualifiers() != qb->GetQualifiers()) {
			// The qualifiers are not the same.
			return nullptr;
		}

		// Qualifiers no longer needed.
		a = qa->Base();
		b = qa->Base();
	}

	// The types are either not qualified or equally-qualified, continue.
	// Check if the types are identical.
	if(a->Equals(b)) {
		return const_cast<Type*>(a);
	}

	// The types should be in the same category.
	if(a->IsSameKind(b) == false) {
		// Enums are allowed to promote to integers. 
		// Check if the integer representation matches the other type.
		Type* result = nullptr;

		if(auto temp = a->As<EnumType>()) {
			result = CombineEnums(temp, b);
		}
		else if(auto temp = b->As<EnumType>()) {
			result = CombineEnums(temp, a);
		}

		if(result) {
			if(qa) return new QType(result, qa->GetQualifiers());
			else return result;
		}
		else return nullptr;
	}

	// Check each type category.
	if(auto temp = a->As<PointerType>()) {
		auto temp2 = b->As<PointerType>();
		// Make a pointer to the merged types.
		// Ex: a: *[], b: *[3]  =>  *[3]
		Type* result = Combine(temp->PointeeType(), temp2->PointeeType());

		if(result == nullptr) {
			return nullptr;
		}

		result = types_->GetPointer(result);

		// Apply the qualifiers, if any.
		if(qa) return types_->GetQualified(result, qa->GetQualifiers());
		else return result;
	}
	else if(a->Is<ArrayType>()) {
		// Apply the qualifiers, if any.
		Type* result = CombineArrays(a->As<ArrayType>(), b->As<ArrayType>());

		if(qa) return types_->GetQualified(result, qa->GetQualifiers());
		else return result;
	}
	else if(auto temp = a->As<FunctionType>()) {
		Type* result = CombineFunctions(a->As<FunctionType>(), b->As<FunctionType>());

		if(qa) return types_->GetQualified(result, qa->GetQualifiers());
		else return result;
	}

	return nullptr; // All other types cannot be merged.
}

} // namespace AST