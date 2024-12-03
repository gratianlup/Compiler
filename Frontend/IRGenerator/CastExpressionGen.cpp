// ExpressionGenerator.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

void ExpressionGenerator::Visit(const CastExpression* expr) {
	CastType cast = expr->Type();
	
	// OPTIMIZATION:
	// Try to eliminate some unnecessary cast 
    // introduced by various rules in the C standard.
	if(GenerateCastOptimized(expr)) {
        return;
    }

	// Many casts involve constants. For example, 'double a = 2' has an
	// 'integer to floating' cast. Instead of generating an instruction 
    // for the cast we can simply emit the converted constant (2.0000).
	// Note that for a 'FunctionToPtr' case the constant generator 
    // shall not be used!!!
	if((cast != CastType::FunctionToPtr) &&
       GenerateConstant(expr)) {
       return;
    }

	switch(expr->Type()) {
		case CastType::IntToFloat: { 
            GenerateIntToFloat(expr);   
            break; 
        }
		case CastType::FloatToInt: { 
            GenerateFloatToInt(expr);   
            break; 
        }
		case CastType::IntToInt: { 
            GenerateIntToInt(expr);     
            break; 
        }
		case CastType::FloatToFloat: { 
            GenerateFloatToFloat(expr); 
            break; 
        }
		case CastType::PointerToInt: { 
            GeneratePointerToInt(expr); 
            break; 
        }
		case CastType::IntToPointer:
		case CastType::ArrayToPtr:
		case CastType::ToPointer: { 
            GenerateToPointer(expr);    
            break; 
        }
		case CastType::FunctionToPtr:
		case CastType::ToVoid:
		case CastType::RemoveQual: {
			// Nothing is going to change in this cases.
            Visit(expr->Target());
			break;
		}
		case CastType::Unknown: {
			// We need to figure out what 'CastType::Unknown' actually means.
			auto castType = expr->ResultType()->WithoutQualifiers();
			auto targetType = expr->Target()->ResultType()->WithoutQualifiers();

			if(targetType->IsInteger()) {
				// Integers can be converted to floating, 
                // integer and pointer types. Note that converting 
                // to pointer type involves no changes.
				if(castType->IsInteger()) { 
                    GenerateIntToInt(expr);   
                    break; 
                }
				else if(castType->IsFloating()) { 
                    GenerateIntToFloat(expr); 
                    break; 
                }
				else { 
                    GenerateToPointer(expr); 
                    break; 
                }
			}
			else if(targetType->IsFloating()) {
				if(castType->IsFloating()) {
                    GenerateFloatToFloat(expr); 
                    break; 
                }
				else {
                    GenerateFloatToInt(expr); 
                    break; 
                }
			}
			else if(castType->IsPointer()) {
				GenerateToPointer(expr); 
				break;
			}
            else if(targetType->IsRecord()) {
                // This kind of cast appears when a copy of a constant is done.
                // Nothing is going to change in this cases.
                Visit(expr->Target());
			    break;
            }
			else {
				DebugValidator::IsTrue(castType->IsInteger());
				GeneratePointerToInt(expr); 
				break;
			}
		}
		default: DebugValidator::Unreachable();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateIntToFloat(const CastExpression* expr) {
	// Convert a signed or unsigned integer to a floating point number.
	auto targetType = expr->Target()->ResultType()->WithoutQualifiers();
	auto irCastType = GetIRType(expr->ResultType()->WithoutQualifiers());
	IR::Instruction* instr;

	// Generate code for the target.
	auto targetOp = GenerateExpression(expr->Target());
	result_ = irGen_->GetTemporary(irCastType);

	// Take in consideration signed numbers.
	if(targetType->IsSigned()) {
		instr = irGen_->GetItof(targetOp, irCastType, 
                                result_, activeBlock_);
	}
	else instr = irGen_->GetUitof(targetOp, irCastType, 
                                  result_, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateFloatToInt(const CastExpression* expr) {
	// Convert a floating point number to a signed or unsigned integer number.
	auto castType = expr->ResultType()->WithoutQualifiers();
	auto irCastType = GetIRType(castType);
	IR::Instruction* instr;

	// Generate code for the target.
	auto targetOp = GenerateExpression(expr->Target());
	result_ = irGen_->GetTemporary(irCastType);
	
	// Take in consideration signed numbers.
	if(castType->IsSigned()) {
		instr = irGen_->GetFtoi(targetOp, irCastType, 
                                result_, activeBlock_);
	}
	else instr = irGen_->GetFtoui(targetOp, irCastType,
                                  result_, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateIntToInt(const CastExpression* expr) {
	// Converts between different ranks of integer types.
	// If we convert to smaller rank:
	//		truncate (the sign is preserved in two's complement arithmetic)
	// If we convert to a higher rank:
	//		sign extend, if the target number is signed
	//      zero extend, for unsigned numbers
	auto castIntType = GetBasicType(expr->ResultType()->WithoutQualifiers());
	auto targetIntType = GetBasicType(expr->Target()->ResultType()
                                                    ->WithoutQualifiers());
	auto irCastType = GetIRType(castIntType);
	auto irTargetType = GetIRType(targetIntType);

	// Some 'integer to integer' convert between integers with the same 
    // rank, but who have different signedness, or between integers 
    // that have the same IR rank (like 'int' and 'long' on most targets).
    // Don't emit the conversion in these cases.
	if(irCastType == irTargetType) {
        GenerateExpression(expr->Target());
		return;
	}

	// Generate code for the target.
	auto targetOp = GenerateExpression(expr->Target());
	result_ = irGen_->GetTemporary(irCastType);
    IR::Instruction* instr;

	// Generate the right conversion instruction.
	if(castIntType->RankBelow(targetIntType)) {
		instr = irGen_->GetTrunc(targetOp, irCastType, 
                                 result_, activeBlock_);
	}
	else if(targetIntType->IsSigned()) {
		instr = irGen_->GetSext(targetOp, irCastType, 
                                result_, activeBlock_);
	}
	else instr = irGen_->GetZext(targetOp, irCastType, 
                                 result_, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateFloatToFloat(const CastExpression* expr) {
	// Conversion between 'float' and 'double'.
	// For 'float -> double' we do an extension, for 'double -> float' a truncation.
	auto castFloatType = GetBasicType(expr->ResultType()->WithoutQualifiers());
	auto targetFloatType = GetBasicType(expr->Target()->ResultType()
                                                      ->WithoutQualifiers());
	auto irCastType = GetIRType(castFloatType);
	IR::Instruction* instr;

	// Generate code for the target.
    auto targetOp = GenerateExpression(expr->Target());

    // It's possible that the cast and target type are the same, 
    // and no cast is needed. This happens when a 'const float' 
    // is converted to a 'float', for example.
    if(castFloatType != targetFloatType) {
	    result_ = irGen_->GetTemporary(irCastType);

	    if(castFloatType->IsFloat()) {
		    instr = irGen_->GetFtrunc(targetOp, irCastType, 
                                      result_, activeBlock_);
	    }
	    else instr = irGen_->GetFext(targetOp, irCastType, 
                                     result_, activeBlock_);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateToPointer(const CastExpression* expr) {
	// The instructions that need to be generated, based on the cast type:
	// CastType::IntToPointer  - itop
	// CastType::ArrayToPtr    - addr array, 0
	// CastType::ToPointer     - ptop
	auto targetType = expr->Target()->ResultType()->WithoutQualifiers();
	auto castType = expr->ResultType()->WithoutQualifiers();
	auto irCastType = GetIRType(castType);
	
	// Generate code for the target.
	bool isArrayToPtr = (expr->Type() == CastType::ArrayToPtr) || 
                        targetType->IsArray();
	if(isArrayToPtr) {
		// We must take the address of the array, not load it.
		SetLvalue();
	}

	auto targetOp = GenerateExpression(expr->Target(), true /* resetFlags */ , 
                                       true /* ignoreVla */);

	if(isArrayToPtr) {
		// Array to pointer case. Note that it's possible 
        // to have a conversion to a pointer pointing to a type 
        // that is different from the element of the array.
		// For example, 'int a[10]; void* b = a;' has an 'array-to-void*' cast.
		auto arrayType = targetType->As<ArrayType>();
        auto elementType = GetIRType(arrayType->ElementType());
		auto irElementPtrType = irGen_->GetPointer(elementType);
		bool sameTypes = irCastType == irElementPtrType;

		// If the array is a VLA we don't need to address the first element
		// (because the IR type of the variable is a pointer, not an array).
		if(arrayType->IsVariable() == false) {
			// Get the address of the first element of the array.
			auto zeroConst = irGen_->GetIntConst(irGen_->GetInt32(), 0);

			result_ = irGen_->GetTemporary(irElementPtrType);
			irGen_->GetIndex(targetOp, zeroConst, 
                             result_, activeBlock_);
		}

		if(sameTypes == false) {
			// A 'ptop' conversion is needed.
			auto elemPtrOp = result_;
			result_ = irGen_->GetTemporary(irCastType);
			irGen_->GetPtop(elemPtrOp, irCastType, 
                            result_, activeBlock_);
		}
	}
	else if(irCastType->IsPointer() && targetType->IsInteger()) {
		// 'integer-to-pointer' case.
		// OPTIMIZATION:
		// If the target of the expression is the number 0 
        // (with integer type) we can use the 'nullptr' constant 
        // instead (for example, 'char* s; if(s == 0) {}').
		if(auto numberConst = expr->Target()->As<NumberConstant>()) {
			if(targetType->IsInteger() && 
               (numberConst->Value().IntValue == 0)) {
				result_ = irGen_->GetNullConst(irCastType);
				return;
			}
		}

		result_ = irGen_->GetTemporary(irCastType);
		irGen_->GetItop(targetOp, irCastType, 
                        result_, activeBlock_);
	}
    else if(castType->IsPointer() && targetType->IsPointer()) {
		// 'pointer-to-pointer' case.
		// Many libraries define 'NULL' as '(void*)0'.
        // When used in a comparison, like 'if(p == NULL)', 
        // another cast from 'void*' to the type of 'p' is introduced 
        // if 'p' is not 'void*'. We can eliminate this cast in this situation.
		if(expr->Target()->IsNullPointer(functGen_->GetContext())) {
			DebugValidator::IsTrue(targetOp->GetType()->IsPointer());
            result_ = PatchOperandType(targetOp, irCastType);
			return;
		}

		// If the target type, without qualifiers, it's the same 
        // don't emit the instruction. Handles cases like 
        // 'int* a; const int* b; b = a;'.
		auto castPointeeType = castType->As<PointerType>()
									   ->PointeeType()->WithoutQualifiers();
		auto targetPointeeType = targetType->As<PointerType>()
										   ->PointeeType()->WithoutQualifiers();

		if(castPointeeType != targetPointeeType) {
			result_ = irGen_->GetTemporary(irCastType);
			irGen_->GetPtop(targetOp, irCastType, 
                            result_, activeBlock_);
		}		
	}
    else {
        // Function to pointer when the function is not compatible
        // with the variable/parameter.
        result_ = irGen_->GetTemporary(irCastType);
		irGen_->GetPtop(targetOp, irCastType, 
                        result_, activeBlock_);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GeneratePointerToInt(const CastExpression* expr) {
	// We simply generate the 'pointer to integer' instruction.
	// An exception is when the result is stored into a 'bool',
    // because when we truncate something like 0xFF00 we get 
    // 0x00 (false), but instead we should set 1 (true).
	auto castType = GetBasicType(expr->ResultType()->WithoutQualifiers());
	auto irCastType = GetIRType(castType);
	IR::Instruction* instr;

	// Generate code for the target.
    auto targetOp = GenerateExpression(expr->Target());
	result_ = irGen_->GetTemporary(irCastType);

	if(castType->IsBool()) {
		// We store the result of a 'not-equal' comparison to 0.
		// Explanation: suppose we have the following code:
		// 'bool b; char* p = malloc(N); b = p; if(b) {...}'.
		// If 'p' would be 0x123400, truncating the pointer 
        // to a 1 byte integer would result in the value 0, 
        // and the 'if' statement would not be taken anymore.
		auto nullConst = irGen_->GetNullConst(targetOp->GetType());
		instr = irGen_->GetUcmpNEQ(targetOp, nullConst, 
                                   result_, activeBlock_);
	}
	else instr = irGen_->GetPtoi(targetOp, irCastType, 
                                 result_, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::GenerateCastOptimized(const CastExpression* expr) {
	// Consider the following code: 'char a, b, c; a = b + c;'. 
	// The C language rules forces us to insert two 
    // 'char-to-integer' casts, and a 'integer-to-char' cast, 
    // which are not necessarily at all (generates two 'sext' 
    // and one 'trunc' instructions in the IR that have no effect).
	auto finalType = expr->ResultType()->WithoutQualifiers();
    auto origType = expr->Target()->ResultType()->WithoutQualifiers();

	if(auto binOp = expr->Target()->As<BinaryOperator>()) {
		if(binOp->IsAssignment() || binOp->IsLogical()) {
            return false;
        }

		auto leftCastExpr = binOp->LeftValue()->As<CastExpression>();
		auto rightCastExpr = binOp->RightValue()->As<CastExpression>();

		if((leftCastExpr && rightCastExpr) == false) {
            return false;
        }

		// We can apply the optimization only if the casts cancel themselves.
		auto leftTargetType = leftCastExpr->Target()->ResultType()
                                          ->WithoutQualifiers();
		auto rightTargetType = rightCastExpr->Target()->ResultType()
                                            ->WithoutQualifiers();

		if((leftTargetType != finalType) ||
		   (rightTargetType != finalType)) {
            return false;
        }

        auto leftOp = GenerateExpression(leftCastExpr->Target());
        auto rightOp = GenerateExpression(rightCastExpr->Target());

        GenerateBinaryOp(binOp, leftOp, rightOp, finalType);
		return true;
	}
	else if(finalType->IsInteger() && origType->IsInteger()) {
		// An unnecessary 'integer-to-integer' cast is introduced
        // in code like the following: 'char a; a = 5;' 
        // (a 'integer-to-char' cast in this case).
		if(auto numberConst = expr->Target()->As<NumberConstant>()) {
			auto irResultType = GetIRType(finalType);
			result_ = irGen_->GetIntConst(irResultType, numberConst->Value().IntValue);
			return true;
		}
	}

	return false;
}

} // namespace IRGenerator