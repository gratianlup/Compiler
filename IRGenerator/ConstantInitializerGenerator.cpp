// ConstantInitializerGen.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "UnitGenerator.hpp"
#include "../AST/ASTDotPrinter.hpp"

namespace IRGenerator {

const Expression* UnitGenerator::FindBaseObject(const Expression* expr, bool expectInt) {
	// Traverse the tree formed by the expression until the base object is found,
	// or until it is certain that such an object doesn't exists.
	if(expr->IsDeclarationExpr()) {
		return expr;
	}
	else if(expr->IsStringConst()) {
		return expr;
	}
	else if(expr->IsCompoundExpr()) {
		return expr;
	}
	else if(auto unaryOp = expr->As<UnaryOperator>()) {
		return FindBaseObject(unaryOp->Value(), expectInt);
	}
	else if(auto binaryOp = expr->As<BinaryOperator>()) {
		auto left = FindBaseObject(binaryOp->LeftValue(), expectInt);
		
        if(left) {
            return left;
        }
		else return FindBaseObject(binaryOp->RightValue(), expectInt);
	}
	else if(auto castExpr = expr->As<CastExpression>()) {
		return FindBaseObject(castExpr->Target(), expectInt);
	}
	else if(auto subscriptExpr = expr->As<SubscriptExpression>()) {
		// If we expect an integer the 'subscript' operator can appear only in
		// cases like 'int a = &p[2]-&p[1]', where we don't care about the variables.
		if(expectInt) {
            return nullptr;
        }
        else return FindBaseObject(subscriptExpr->Base(), expectInt);
	}
	else if(auto memberExpr = expr->As<MemberExpression>()) {
		// Same as above.
		if(expectInt) {
            return nullptr;
        }
		else return FindBaseObject(memberExpr->Object(), expectInt);
	}
	
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<IR::Initializer> 
UnitGenerator::GetStringInitializer(const StringConstant* str, 
                                    const ArrayType* targetType) {
	// The following conversion is made:
	// 'char* a = L"abcd";' -> 'var a int8* = "abcd"'
	auto stringConst = GetStringConstant(str, targetType);
	return IR::Initializer::GetInitializer(stringConst);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::StringConstant*
UnitGenerator::GetStringConstant(const StringConstant* str, 
                                 const ArrayType* targetType) {
	// If 'target' is set, it means that an array is initialized with this string.
	// If the array is smaller that the string, we use only 'requiredLength' characters.
	// If it's larger, the rest of the elements must have the value '\0'.
	IR::IRGenerator gen(irUnit_);

	if(targetType) {
		__int64 strLength = str->Value().Value.Length();
		int requiredLength = (int)targetType->Size();
		auto irStrType = typeGen_->GetType(targetType);

		if(strLength > requiredLength) {
			// Use only a part of the original string.
			StringBuffer newValue(str->Value().Value, requiredLength);
			return irUnit_->Constants().GetString(irStrType, newValue);
		}
		else {
			// Append '\0' until the string has the length of the array.
			StringBuffer newValue(requiredLength);
			newValue.Append(str->Value().Value);
			newValue.Append(L'\0', (int)(requiredLength - strLength));
			return irUnit_->Constants().GetString(irStrType, newValue);
		}
	}
	else {
		// Nothing needs to be changed; use the original string.
		auto irStrType = typeGen_->GetType(str->ResultType());
		return irUnit_->Constants().GetString(irStrType, str->Value().Value);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<IR::Initializer> 
UnitGenerator::GetIntegerInitializer(const Type* destType, EvaluationInfo& eval, 
									 const Expression* base) {
	// An integer can be initialized with an integer constant or
	// with the address of a variable/string constant.
	auto intType = typeGen_->GetType(destType)->As<IR::IntegerType>();

	if(base == nullptr) {
		// Initialized with an integer constant or other scalar converted to integer.
		// 'int a = 5;', 'int b = 2.5;', 'int c = 'p';'.
		auto constValue = irUnit_->Constants().GetInt(intType, eval.IntValue());
		return IR::Initializer::GetInitializer(constValue);
	}
	else if(auto objectRef = base->As<DeclarationExpression>()) {
		// The address of a variable converted to an integer, like in
		// 'int a; int b = (int)&a;'. Note that it can be adjusted.
		IR::Operand* constValue;

		if(auto variableRef = objectRef->Object()->As<VariableDeclaration>()) {
			constValue = GetVariableReference(variableRef);
		}
		else constValue = GetFunctionReference(objectRef->Object()->As<FunctionDeclaration>());

		return IR::Initializer::GetInitializer(constValue, eval.IntValue(), 
											   IR::InitConversion::PointerToInt, intType);
	}
	else if(auto stringConst = base->As<StringConstant>()) {
		// The address of a string constant, with optional adjustment, like in
		// 'int a = (int)("abcd" - 2);'.
		auto strInit = GetStringInitializer(stringConst);
		strInit->SetAdjustment(eval.IntValue());
		strInit->SetConversion(IR::InitConversion::PointerToInt);
		strInit->SetConversionType(intType);
		return strInit;
	}
	
	DebugValidator::Unreachable(); // Should not be reached.
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<IR::Initializer> 
UnitGenerator::GetFloatingInitializer(const Type* destType, EvaluationInfo& eval, 
									  const Expression* base) {
	// Floating variables can be initialized only by floating constants.
	IR::Operand* constValue;
	auto floatType = typeGen_->GetType(destType);

	// Select the appropriate floating type ('float' or 'double').
	// Note that 'long double' is not supported.
	if(floatType->IsFloat()) {
		constValue = irUnit_->Constants().GetFloat(eval.FloatValue());
	}
	else constValue = irUnit_->Constants().GetDouble(eval.FloatValue());

	// Create the initializer. Note that no adjustment is possible.
	return IR::Initializer::GetInitializer(constValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<IR::Initializer> 
UnitGenerator::GetPointerInitializer(const Type* destType, EvaluationInfo& eval, 
									 const Expression* base) {
	// A pointer can be initialized with a null-pointer constant, with
	// the address of another variable (with adjustment), with the address
	// of a string (with adjustment) or with an integer.
	auto pointerType = typeGen_->GetType(destType);
	const IR::Type* conversionType = nullptr;
	IR::InitConversion conversion = IR::InitConversion::None;
	IR::Operand* constValue;
	__int64 adjustment = 0;

	if(base == nullptr) {
		// This is an integer converted to a pointer. A null pointer is a special case.
		if(eval.IntValue() == 0) {
			constValue = irUnit_->Constants().GetNull(pointerType);
		}
		else {
			// We cast the integer to the appropriate pointer type.
			auto targetPtrInt = TypeGenerator::GetTypeFromSize(target_->GetPointerSize());
			constValue = irUnit_->Constants().GetInt(targetPtrInt, eval.IntValue());
			conversionType = pointerType;
			conversion = IR::InitConversion::IntToPointer;
		}
	}
	else if(auto stringConst = base->As<StringConstant>()) {
		auto strType = typeGen_->GetType(stringConst->ResultType());
		constValue = irUnit_->Constants().GetString(strType, stringConst->Value().Value);
		adjustment = eval.IntValue();
		
		// A conversion may be needed if we have something like 'int* a = "abc"'.
		// It's not done if the pointers match, like in 'char* a = "abc";'.
		if(strType != pointerType) {
			conversionType = pointerType;
			conversion = IR::InitConversion::PointerToPointer;
		}
	}
	else if(auto objectRef = base->As<DeclarationExpression>()) {
		// Reference to a variable or a function. 
        // We must match the type of the destination in cases like
        // 'int a; char* p = (char*)&a;'.
		if(auto variableRef = objectRef->Object()->As<VariableDeclaration>()) {
			constValue = GetVariableReference(variableRef);
		}
		else constValue = GetFunctionReference(objectRef->Object()->As<FunctionDeclaration>());

		auto irObjectType = constValue->GetType();
		adjustment = eval.IntValue();

		if(irObjectType != pointerType) {
			conversionType = pointerType;
			conversion = IR::InitConversion::PointerToPointer;
		}
	}
	else if(auto compoundExpr = base->As<CompoundExpression>()) {
		// The pointer is initialized with a compound expression, like in
		// 'int* p = (int[]){1,2,3};' and 'struct ABC* q = &(struct ABC){4,5,6};'.
		auto compoundType = compoundExpr->ResultType()->WithoutQualifiers();
		auto irCompoundType = typeGen_->GetType(compoundType);

		// Generate the global variable; mark it 'static' and 'const'.
		string name = nameGen_.GetName(&irUnit_->Symbols(), "#compound");
		auto compoundVar = IR::GlobalVariable::GetGlobal(irCompoundType, name, nullptr,
														 &irUnit_->Symbols(),
														 IR::SymbolVisibility::Static);
		compoundVar->SetIsConstant(true);
		irUnit_->AddVariable(compoundVar);

		// Add the initializer of the compound expression. 
		// The pointer will be initialized with the address of the variable.
		AddInitializer(compoundVar, compoundExpr->InitList(), compoundType);
		constValue = GetVariableReference(compoundVar);

		if(irCompoundType != pointerType) {
			conversionType = pointerType;
			conversion = IR::InitConversion::PointerToPointer;
		}
	}

	return IR::Initializer::GetInitializer(constValue, adjustment, 
                                           conversion, conversionType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<IR::Initializer> 
UnitGenerator::GetArrayStringInitializer(const Type* destType, EvaluationInfo& eval, 
								   const Expression* base) {
	DebugValidator::IsNotNull(base);
	DebugValidator::IsTrue(base->IsStringConst());
	
	// An array can be also initialized with a string.
	return GetStringInitializer(base->As<StringConstant>(), destType->As<ArrayType>());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::CreateBitfield(__int64 value, const IR::Type* type,
							 IR::InitializerList* irInitList) {
	auto bitfield = irUnit_->Constants().GetInt(type, value);
	auto bitfieldInit = IR::Initializer::GetInitializer(bitfield);
	irInitList->Add(bitfieldInit);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::GetRecordInitializer(const StructUnionType* destType, 
										 const InitializerListExpression* initList, 
										 IR::InitializerList* irInitList) {
	auto& exprList = initList->InitList();
	auto layout = layouts_->GetOrCreate(destType);
	int fieldPos = 0;          // Position in the field list.
	int prevIndex = -1;        // The index of the previous field unit.
	bool wasBitfield = false;  // 'true' if the previous field was a bitfield.
	__int64 bitfieldValue = 0; // The accumulated value of all bitfields in the current unit.
	const IR::Type* bitfieldType = nullptr;

	// If the type is an 'union' we emit only the first initializer.
	int maxCount = destType->IsUnion() ? 1 : exprList.Count();

	for(int i = 0; i < maxCount; i++) {
		// Bitfields are a special case among the fields. We need to skip over unnamed
		// bitfields and combine the values of adjacent bitfields.
		auto field = destType->Fields()[fieldPos];
		fieldPos++;

		if(field->IsUnnamedBitfield()) {
			field = destType->Fields()[fieldPos];
			fieldPos++;
		}

		if(field->IsBitfield()) {
			// Make sure the initializer is an integer that doesn't involve
			// a variable or a string, like in 'int b; struct A {int a:2} t = {&a};'.
			// We continue generating code even if we have an error.
			if(FindBaseObject(exprList[i], true /* expectInt */)) {
				context_->Diagnostic().Report(Error::BITFIELD_NOT_SCALAR_INIT)<<
											  *field->Name()<<exprList[i]->Location();
			}

			// 'mask' is used to restrict the value based on the bit width.
			auto& info = layout->GetFieldInfo(field->Name());
			__int64 mask = ((__int64)1 << info.Size()) - 1;
			EvaluationInfo eval = exprList[i]->Evaluate(context_, false /* warn */);

			if(prevIndex != info.Index()) {
				// This is the first bitfield in the unit, or a new unit is beginning.
				if(wasBitfield) {
					CreateBitfield(bitfieldValue, bitfieldType, irInitList);
				}

				wasBitfield = true;
				bitfieldValue = eval.IntValue() & mask;
				bitfieldType = typeGen_->GetType(field->DeclarationType());
				prevIndex = info.Index();
			}
			else {
				// The value must be brought in the right position by shifting
                // to the left and combined with the values of the previous bitfields.
				__int64 value = eval.IntValue() & mask;
				bitfieldValue |= value << info.UnitOffset();
			}
		}
		else {
			// This is a normal field.
			// If the field was preceded by bitfields we need to emit their values
			// before the value for the current field.
			if(wasBitfield) {
				wasBitfield = false;
				CreateBitfield(bitfieldValue, bitfieldType, irInitList);
			}

			// Now add the value of the current field.
			auto fieldType = field->DeclarationType()->WithoutQualifiers();
			irInitList->Add(GetInitializer(exprList[i], fieldType));
		}
	}

	// If the last field was a bitfield we need to create it now.
	if(wasBitfield) {
		CreateBitfield(bitfieldValue, bitfieldType, irInitList);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<IR::Initializer> UnitGenerator::GetInitializer(const Expression* initializer, 
													  const Type* destType) {
	// The initializer can be a simple constant (number/string), the address
	// of a variable, a null-pointer constant or a list of initializers.
	if(auto initList = initializer->As<InitializerListExpression>()) {
		// For arrays, 'struct' and 'union'.
		auto irInitList = IR::InitializerList::GetList();

		if(auto arrayType = destType->As<ArrayType>()) {
			auto& exprList = initList->InitList();
			auto elementType = arrayType->ElementType()->WithoutQualifiers();

			for(int i = 0; i < exprList.Count(); i++) {
				irInitList->Add(GetInitializer(exprList[i], elementType));
			}
		}
		else GetRecordInitializer(destType->As<StructUnionType>(), 
                                  initList, irInitList);

		return irInitList;
	}
	else {
		// Use the constant evaluator for all other cases.
		EvaluationInfo eval = initializer->Evaluate(context_, false /* warn */);

		if(destType->IsInteger()) {
			auto base = FindBaseObject(initializer, true /* expectInt */);
			return GetIntegerInitializer(destType, eval, base);
		}
		else if(destType->IsFloating()) {
			auto base = FindBaseObject(initializer);
			return GetFloatingInitializer(destType, eval, base);
		}
		else if(destType->IsPointer()) {
			auto base = FindBaseObject(initializer);
			return GetPointerInitializer(destType, eval, base);
		}
		else if(destType->IsArray()) {
			auto base = FindBaseObject(initializer);
			return GetArrayStringInitializer(destType, eval, base);
		}
	}

	DebugValidator::Unreachable();
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::AddInitializer(IR::GlobalVariable* irVariable, 
                                   const VariableDeclaration* variable) {
	AddInitializer(irVariable, variable->Initializer(), 
                   variable->DeclarationType()->WithoutQualifiers());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::AddInitializer(IR::GlobalVariable* irVariable, 
                                   const Expression* initExpr,
								   const Type* variableType) {
	bool initSet = false;

	// If the list contains only 0 we generate the special zero-initializer.
	if(auto listInit = initExpr->As<InitializerListExpression>()) {
		if(listInit->IsAllZero(context_)) {
			irVariable->SetHasZeroInitializer(true);
			initSet = true;
		}
	}
	
	// If it wasn't a zero initializer we generate it now.
	if(initSet == false) {
        auto initializer = GetInitializer(initExpr, variableType->WithoutQualifiers());
		irVariable->SetInitializer(initializer);
	}

	// Mark the value as a constant if it's the case.
	// 'char* const a = "abc";' is marked as a constant, 
    // while 'const char* b = "abc";' is not.
	if(auto qualType = variableType->As<QType>()) {
		irVariable->SetIsConstant(qualType->HasConst());
	}
}

} // namespace IRGenerator