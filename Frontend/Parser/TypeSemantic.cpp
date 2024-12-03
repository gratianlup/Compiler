// TypeSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements semantic analysis method for types (basic types, arrays and functions).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DeclarationSemantic.hpp"
#include "StatementSemantic.hpp"
#include "SemanticHolder.hpp"
#include "../AST/StructLayout.hpp"
#include "../AST/TypeCombiner.hpp"
#include "../AST/TypeString.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;
using namespace AST;

namespace Parsing {

bool DeclarationSemantic::HandleFunctionParam(DI::ParameterInfo* parameter,
                                              shared<DeclarationContext> context) {
	bool invalid = false;

	// If the parameter has an identifier it should be unique.
	if(parameter->HasName()) {
		Identifier* name = parameter->Declarator->GetName();
		DeclarationContext* temp;

		if(context->Find(name, &temp, false)) {
			diag_->Report(Error::PARAMETER_REDEFINITION)<<*name;
			invalid = true;
		}
	}

	// C99:6.7.5.3.2: only 'register' is allowed as a storage-class specifier.
	auto info = parameter->Info;

	if(info.Auto || info.Extern || info.Static ||
       info.Typedef || info.Inline) {
		diag_->Report(Error::INVALID_PARAMETER_SPECIFIER)<<parameter->Location;
		invalid = true;
	}

	return invalid == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* DeclarationSemantic::TypeFromSpecifiers(SpecifierInfo& info) {
	// Extract a type based on the flags in the specifier.
	// For struct, union, enum and typedef the type is already known.
	const Type* type = nullptr;

	if(info.Enum)           type = info.Enum->DeclarationType();
	else if(info.Struct)    type = info.Struct->DeclarationType();
	else if (info.Union)    type = info.Union->DeclarationType();
	else if (info.TypedefT) type = info.TypedefT->Inner();
	
	if(type == nullptr) {
		// Test for the possible combinations according to C99:6.7.2.2.
		if(info.Void) type = BasicType::GetVoid();
		else if(info.Char) {
			if(info.Unsigned) type = BasicType::GetUChar();
			else type = BasicType::GetChar();
		}
		else if(info.WChar) type = BasicType::GetWChar();
		else if(info.Short) {
			// Just 'short'.
			if(info.Unsigned) type = BasicType::GetUShort();
			else type = BasicType::GetShort();
		}
		else if(info.Int) {
			if(info.LongCount == 1) {
				// This is 'long int'.
				if(info.Unsigned) type = BasicType::GetULong();
				else type = BasicType::GetLong();
			}
			else if(info.LongCount == 2) {
				// This is 'long long int'.
				if(info.Unsigned) type = BasicType::GetULongLong();
				else type = BasicType::GetLongLong();
			}
			else {
				// This is 'int' or invalid (long long long, for ex.).
				if(info.Unsigned) type = BasicType::GetUInt();
				else type = BasicType::GetInt();
			}
		}
		else if(info.LongCount == 1) {
			// 'long' without 'int'. It could also be 'long double'.
			if(info.Double) type = BasicType::GetDouble();
			else if(info.Unsigned) type = BasicType::GetULong();
			else type = BasicType::GetLong();
		}
		else if(info.LongCount == 2) {
			// 'long long' without 'int'.
			if(info.Unsigned) type = BasicType::GetULongLong();
			else type = BasicType::GetLongLong();
		}
		else if(info.Signed) {
			// Just 'signed'.
			type = BasicType::GetInt();
		}
		else if(info.Unsigned) {
			// Just 'unsigned'.
			type = BasicType::GetUInt();
		}
		else if(info.Float)  type = BasicType::GetFloat();
		else if(info.Double) type = BasicType::GetDouble();
		else if(info.Bool)   type = BasicType::GetBool();

		// Check the number of 'long' (it should be max. 2).
		if(info.LongCount > 2) {
			diag_->Report(Error::DUPLICATE_DECLARATION_SPECIFIERS)
                          <<RangeInfo(info.Start, info.End);
			return nullptr;
		}
	}

	// Apply qualifiers, if any. Wrap a 'QType' around the type.
	if(info.Qual.HasNone() == false) {
		// Verify that the qualifiers can be applied to the type.
		// C99:6.7.3.2: Types other than pointer types derived from object 
		// or incomplete types shall not be restrict-qualified.
		if(info.Qual.HasRestrict()) {
			if((type->IsPointer() == false) && (type->IsIncomplete() == false)) {
				// 'restrict' not valid here, but build the type anyway to recover.
				diag_->Report(Error::RESTRICT_QUALIFIED_NOT_POINTER)<<info.Start;
			}
			
			// This is a pointer. Check that it doesn't point to a function type.
			auto pointee = type->As<PointerType>()->PointeeType();

			if(pointee->IsFunction()) {
				// 'restrict' not valid here, but build the type anyway to recover.
				diag_->Report(Error::RESTRICT_POINTER_TO_FUNCTION)<<info.Start;
			}
		}

		// C99:6.7.3.8: Function types are not allowed to have qualifiers.
		if(type->IsFunction()) {
			// Continue to recover (the error isn't serious, anyway).
			diag_->Report(Error::QUALIFIER_ON_FUNCTION)<<info.Start;
		}

		// Apply the qualifiers to the type.
		type = types_->GetQualified(type, info.Qual);
	}

	return type;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* DeclarationSemantic::AdjustType(const Type* type) {
	// Array should be converted to "qualified pointer to array".
	// The qualifiers are taken from the array declarator ('[const 12]').
	// A function type should be converted to "pointer to function".
	// See C99:6.7.5.3.7 and 8 for details.
	if(type->IsArray()) {
		const ArrayType* arrayType = type->As<ArrayType>();
		return types_->GetElementPointer(arrayType);
	}
	else if(type->IsFunction()) {
		return types_->GetPointer(type);
	}
	else return type; // Other types are not touched.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StorageType DeclarationSemantic::StorageFromSpec(SpecifierInfo& info) {
	if(info.Extern)   return StorageType::Extern;
	if(info.Auto)     return StorageType::Auto;
	if(info.Static)   return StorageType::Static;
	if(info.Register) return StorageType::Register;
	
	return StorageType::None;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* DeclarationSemantic::MakePointer(const Type* pointee, Qualifier& qual,
									         shared<DeclarationContext> context) {
	// Create a pointer type. If qualifiers are present create a 'QType'.
	const PointerType* pointerType = types_->GetPointer(pointee);

	if(qual.HasNone() == false) {
		return types_->GetQualified(pointerType, qual);
	}
	else return pointerType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* DeclarationSemantic::MakeArray(const Type* elementType, 
                                           DI::ArrayInfo& info, LocationInfo location, 
                                           shared<DeclarationContext> context, bool first) {
	// The element type should not be incomplete (like 'void' or 'struct s;')
	// or a function type (C99:6.7.5.2.1). We continue even if it's an error.
	if(elementType->IsIncomplete()) {
		// Emit a different error for 'void a[...]'.
		bool handled = false;

		if(auto temp = elementType->As<BasicType>()) {
			if(temp->IsVoid()) {
				diag_->Report(Error::ARRAY_VOID_ELEMENT_TYPE)<<location;
				handled = true;
			}
		}

		if(handled == false) {
			// Emit the same error for all other cases.
			diag_->Report(Error::ARRAY_INCOMPLETE_ELEMENT_TYPE)<<location;
		}
	}
	else if(elementType->IsFunction()) {
		diag_->Report(Error::ARRAY_FUNCTION_ELEMENT_TYPE)<<location;
	}

	// If the element type is a struct/union it should not contain
	// a flexible array (C99:6.7.2.1.2). We continue even if it's an error.
	if(elementType->IsStruct()) {
		if(elementType->As<StructType>()->HasFlexArray(true)) {
			diag_->Report(Error::ARRAY_FLEX_ARRAY)<<location;
		}
	}
	else if(elementType->IsUnion()) {
		if(elementType->As<UnionType>()->HasFlexArray(true)) {
			diag_->Report(Error::ARRAY_FLEX_ARRAY)<<location;
		}
	}

	// 'static' and type qualifiers can appear only if the array is a
	// function parameter; and then only in the outermost position (C99:6.7.5.2.1).
	// Ex: 'int a[2][static 3]' is not valid.
	if((info.Info.Static || (info.Info.Qual.HasNone() == false)) &&
		((context->IsFunctProtoScope() == false) || (first == false))) {
		diag_->Report(Error::ARRAY_STATIC_NOT_OUTERMOST)<<location;
	}

	// Now we need to choose between a fixed-length and a variable-length array.
	// An incomplete array is a standard one.
	if(info.Incomplete) {
		return types_->GetIncompleteArray(elementType, info.HasStatic, 
                                          info.Info.Qual);
	}

	// See if it's *. * denotes a variable-length array
	// that can appear only in function-prototype scope (C99:6.7.5.2.4).
	if(info.HasStar) {
		if(info.FunctProto == false) {
			diag_->Report(Error::ARRAY_STAR_NOT_IN_FUNCTION_PROTOTYPE)<<location;
		}

		return types_->GetVarArray(elementType, nullptr, info.HasStatic, 
                                   info.Info.Qual);
	}

	// If the expression evaluates to a constant it's a fixed array (C99:6.7.5.2.4).
	EvaluationInfo eval = info.Value->EvaluateAsICE(context_, false /* warn */);

	if(eval.IsConstant()) {
		// The value must be an integer constant(C99:6.7.5.2.1).
		if(eval.IsFloatConstant()) {
			// Not valid, but continue by making an array that has one element;
			diag_->Report(Error::ARRAY_INDEX_FLOATING)<<location;
			return types_->GetArray(elementType, 1, info.HasStatic, 
                                    info.Info.Qual);
		}
		else {
			// The evaluated size should be > 0 (C99:6.7.5.2.1).
			// Emit separate errors for zero and negative.
			if(eval.Value().IntValue < 0) {
				diag_->Report(Error::ARRAY_INDEX_NEGATIVE)<<location;
			}
			else if(eval.Value().IntValue == 0) {
				diag_->Report(Error::ARRAY_INDEX_ZERO)<<location;
			}
			
			// Create a fixed-size array.
			return types_->GetArray(elementType, eval.IntValue(), 
									info.HasStatic, info.Info.Qual);
		}
	}
	else {
		// This is a variable-length array.
		// If we are in function-prototype scope the array is converted to [*].
		// Else the value must be evaluated each time (C99:6.7.5.2.5).
		if(context->IsFunctProtoScope()) {
			return types_->GetVarArray(elementType, nullptr, 
                                       info.HasStatic, info.Info.Qual);
		}
		else {
            info.Value->EvaluateAsICE(context_, false /* warn */);
            return types_->GetVarArray(elementType, info.Value, 
                                       info.HasStatic, info.Info.Qual);
        }
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* DeclarationSemantic::MakeFunction(const Type* returnType, DI* info, 
									          shared<DeclarationContext> context) {
	// Verify the return type. It should not be a function or array type.
	// If errors occur they are reported, but we still continue to recover.
	if(returnType->IsFunction()) {
		diag_->Report(Error::FUNCTION_RETURNS_FUNCTION)<<info->Location;
	}
	else if(returnType->IsArray()) {
		diag_->Report(Error::FUNCTION_RETURNS_ARRAY)<<info->Location;
	}

	// Check for 'f(void)'. This marks a function without parameters.
	if(info->Parameters.Count() > 0) {
		DI::ParameterInfo* parameter = info->Parameters[0];

		if(auto parameterType = MakeType(parameter->Declarator, parameter->Info, context)) {
			if(parameterType->IsVoid()) {
				// 'void' found. Now make sure nothing is after it.
				if(info->Parameters.Count() > 1) {
					diag_->Report(Error::PARAMETER_AFTER_VOID)
                                  <<info->Parameters[1]->Location;
				}

				// No qualifiers should be applied to 'void'.
				if(parameter->Info.Qual.HasNone() == false) {
					diag_->Report(Error::PARAMETER_VOID_QUAL)<<parameter->Location;
				}

				// Create a function type with no parameters.
				return types_->GetEmptyFunction(returnType, false /* isVarargs */);
			}
		}
	}

	List<const Type*> parameters;

	// All parameters must be checked after they are adjusted.
	for(int i = 0; i < info->Parameters.Count(); i++) {
		DI::ParameterInfo* parameter = info->Parameters[i];

		// The only storage-class specifier allowed is 'register' (C99:6.7.5.3).
		if(parameter->Info.HasStorage() && 
           (parameter->Info.Auto   || 
            parameter->Info.Static ||
			parameter->Info.Extern)) {
			diag_->Report(Error::PARAMETER_INVALID_STORAGE_SPECIFIER)<<parameter->Location;
		}

		// Create the parameter type from the declarator and adjust it.
		// The return type should not be 'void'. If it is, it's an error
		// because single 'void' is already treated above.
		const Type* parameterType = MakeType(parameter->Declarator, parameter->Info, context);
		const Type* adjustedType = AdjustType(parameterType);

		if(auto temp = adjustedType->As<BasicType>()) {
			if(temp->IsVoid()) {
				// Found 'void'.
				diag_->Report(Error::PARAMETER_VOID_TYPE)<<parameter->Location;
			}
		}

		// C99:6.7.5.3.4: after adjustment, the type should not be incomplete.
		if(adjustedType->IsIncomplete()) {
			diag_->Report(Error::PARAMETER_INCOMPLETE_TYPE)<<parameter->Location;
		}

		// Add the parameter to the function type.
		parameters.Add(adjustedType);
	}

	return types_->GetFunction(returnType, info->IsVarargs, parameters);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* DeclarationSemantic::MakeType(shared<DI> declaration, SpecifierInfo& info, 
								          shared<DeclarationContext> context) {
	// Obtain the type from the specifiers and apply all declarator
	// sequences to it to obtain the final type.
	// int *abc[3] => ArrayType -> PointerType -> BasicType(int)
	//				  array[3] of pointer to integer
	// int (*abc())[3] => FunctionType -> PointerType -> ArrayType -> BasicType(int)
	//                    function returning pointer to array[3] of integer
	const Type* type = TypeFromSpecifiers(info);
	DI* current = declaration;
	bool arrayCt = 0;

	// The parts are already arranged in the right precedence by the Parser.
	while(current) {
		if(current->IsPointer()) {
			type = MakePointer(type, current->Info.Qual, context);
			current = current->Next;
		}
		else if(current->IsArray()) {
			// The parsed array is in the form [2][3][4]. The type must be built
			// from the end in this case.  The correct order of the types is:
			// ArrayType(2) -> ArrayType(3) -> ArrayType(4).
			List<DI*> arrayDecl(8);

			while(current && (current->IsArray())) {
				arrayDecl.Add(current);
				current = current->Next;
			}

			for(int i = arrayDecl.Count() - 1; i >= 0; i--) {
				type = MakeArray(type, arrayDecl[i]->ArrayDim, 
                                 arrayDecl[i]->Location, context, arrayCt == 0);
				arrayCt++;
			}
		}
		else if(current->IsFunction()) {
			type = MakeFunction(type, current, context);
			current = current->Next;
		}
		else {
			// The declarator part with only the identifier is ignored.
			current = current->Next;
		}
	}

	return type;
}

} // namespace Parsing