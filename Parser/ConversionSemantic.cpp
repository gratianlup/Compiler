// ConversionSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements semantic analysis for conversion between
// the types of the values used in expressions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionSemantic.hpp"
#include "DeclarationSemantic.hpp"
#include "StatementSemantic.hpp"
#include "SemanticHolder.hpp"
#include "../AST/TypeString.hpp"
using namespace AST;

namespace Parsing {

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::UsualConversion(shared<Expression> expr, 
                                    bool removeQualifiers) {
	// Conversions that are performed on most expressions.
	bool changed;
	expr = IntegerPromotion(expr, &changed);
	
	if(changed == false) {
		expr = PromoteFunctionArrayExpr(expr, removeQualifiers);
	}

	return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionSemantic::UsualBinaryConversion(shared<Expression>& left, 
										       shared<Expression>& right,
                                               bool changeLeft) {
	// Used by most binary operator expressions.
	// Determines the common type of the specified ones (int + float = float, for ex.).
	// The usual conversions are applied first. If 'changeLeft' is not set
	// the conversions are not applied on 'left' because it's on the
	// left of a compound assignment operator (+=, /=, etc.).
	if(changeLeft) left = UsualConversion(left);
	right = UsualConversion(right);

	// Work on the unqualified variant of the types (so that 'const int' = 'int').
	const Type* a = left->ResultType()->WithoutQualifiers();
	const Type* b = right->ResultType()->WithoutQualifiers();
	const Type* common;

	// If the types are the same we're done. Make sure that the qualifiers
	// are removed from both sides of the expression.
	if(a->Equals(b)) {
		if(left->ResultType()->IsQualified()) {
			left = CreateImplicitCast(left, a, CastType::RemoveQual);
		}

		if(right->ResultType()->IsQualified()) {
			right = CreateImplicitCast(right, b, CastType::RemoveQual);
		}

		return true;
	}

	// If the types are not both arithmetic give up.
	if((a->IsArithmetic() == false) || (b->IsArithmetic() == false)) {
		return false;
	}

	// The expressions could be references to bitfields,
	// perform the usual promotions first.
	if(auto temp = left->GetBitfield()) {
		a = PromoteToInt(temp->DeclarationType());
	}

	if(auto temp = right->GetBitfield()) {
		b = PromoteToInt(temp->DeclarationType());
	}

	// Rules from C99:6.3.1.8 (note that 'long double' is treated as 'double'):
	// double + anything = double
	// float  + anything = float
	// Integers are handled according to the sign and the rank.
	const BasicType* basicA = a->As<BasicType>();
	const BasicType* basicB = b->As<BasicType>();

	if(basicA->IsDouble() || basicB->IsDouble()) {
		common = BasicType::GetDouble();
	}
	else if(basicA->IsFloat() || basicB->IsFloat()) {
		common = BasicType::GetFloat();
	}
	else {
		// The integer promotions are performed first.
		if(basicA->CanPromoteToInt()) {
			basicA = static_cast<const BasicType*>(PromoteToInt(basicA));
		}

		if(basicB->CanPromoteToInt()) {
			basicB = static_cast<const BasicType*>(PromoteToInt(basicB));
		}

		// If both types are signed or unsigned, the common type
		// is the one with higher rank.
		if(basicA->IsSigned() == basicB->IsSigned()) {
			common = basicA->Rank() > basicB->Rank() ? basicA : basicB;
		}
		else if(basicA->IsUnsigned() && 
               (basicA->Rank() > basicB->Rank())) {
			// The signed type is converted to unsigned if the unsigned
			// type has a higher rank.
			common = basicA;
		}
		else if(basicB->IsUnsigned() && 
               (basicB->Rank() > basicA->Rank())) {
			common = basicB;
		}
		else if(basicA->IsSigned() && 
               (basicB->Rank() < basicA->Rank())) {
			// The unsigned type is converted to signed if the value
			// can be represented (unsigned rank < signed rank).
			common = basicA;
		}
		else if(basicB->IsSigned() && 
               (basicA->Rank() < basicB->Rank())) {
			common = basicB;
		}
		else {
			// The common type is the unsigned correspondent of the signed type.
			if(basicA->IsSigned()) common = UnsignedFromSigned(basicA);
			else common = basicA;
		}
	}

	// Now apply the conversions. If 'left' is the left part of an assignment
	// operator (like in 'left += right') the conversion is not applied to it.
	if(changeLeft) left = CreateImplicitCast(left, common, CastType::Unknown);
	right = CreateImplicitCast(right, common, CastType::Unknown);
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* 
ExpressionSemantic::PromoteToInt(const Type* type) {
	// The type should be an integer type or 'enum'.
	type = type->WithoutQualifiers();

	if(type->CanPromoteToInt()) {
		// If 'int' can represent all values 'int' is returned. Else 'unsigned'.
		if(type->IsSigned()) {
			return BasicType::GetInt();
		}
		else {
			const BasicType* uType = nullptr;

			if(auto basicType = type->As<BasicType>()) {
				uType = basicType;
			}
			else if(auto enumType = type->As<EnumType>()) {
				uType = enumType->ConstType();
			}

			if(uType->IsBool()) {
				// Extension: allow booleans to be promoted.
				return BasicType::GetInt();
			}
			else if(uType->RankBelow(BasicType::GetInt())) {
				return BasicType::GetInt();
			}
			else return BasicType::GetUInt();
		}
	}

	return type;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::IntegerPromotion(shared<Expression> expr, bool* changed) {
	// C99:6.3.1.1.2: in place of 'int' or 'unsigned' the following can be used:
	// - an integer type with rank below that of 'int'.
	// - a bitfield of type '_Bool', 'int', or 'unsigned'.
	// Extension: allow all integer types.
	if(auto temp = expr->GetBitfield()) {
		const Type* bitType = temp->DeclarationType();

		if(auto basicType = bitType->WithoutQualifiers()->As<BasicType>()) {
			if(changed) *changed = true;
			return CreateImplicitCast(expr, PromoteToInt(bitType), 
                                      CastType::IntToInt);
		}
	}
	else if(expr->ResultType()->WithoutQualifiers()->CanPromoteToInt()) {
		if(changed) *changed = true;
		return CreateImplicitCast(expr, PromoteToInt(expr->ResultType()),
                                  CastType::IntToInt);
	}

	// Other types are not converted.
	if(changed) *changed = false;
	return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression>
ExpressionSemantic::PromoteArgumentDefault(shared<Expression> expr) {
	// C99:6.5.2.2.6: if the function has no prototype the default
	// promotions are performed - integer promotion and 'float' -> 'double'.
	if(auto temp = expr->ResultType()->As<BasicType>()) {
		if(temp->IsFloat()) {
			// Convert it to 'double'.
			return CreateImplicitCast(expr, BasicType::GetDouble(), 
									  CastType::FloatToFloat);
		}
	}

	// For all other cases perform integer/array/function promotion.
	return UsualConversion(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const BasicType* ExpressionSemantic::UnsignedFromSigned(const BasicType* type) {
	if(type->IsChar())     return BasicType::GetUChar();
	if(type->IsShort())    return BasicType::GetUShort();
	if(type->IsInt())      return BasicType::GetUInt();
	if(type->IsLong())     return BasicType::GetULong();
	if(type->IsLongLong()) return BasicType::GetULongLong();

	DebugValidator::Unreachable(); // Should not be reached.
	return type;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression>
ExpressionSemantic::PromoteFunctionArrayExpr(shared<Expression> expr, 
                                             bool removeQualifiers, bool* changed) {
	// Select the appropriate conversion. If the expression has a type that is not
	// an array or a function, nothing is done. An exception applies to qualified
	// types that are in a lvalue position (see below for details).
	const Type* exprType = expr->ResultType()->WithoutQualifiers();

	if(exprType->IsFunction()) {
		if(changed) *changed = true;
		expr = PromoteFunctionExpr(expr);
	}
	else if(exprType->IsArray()) {
		if(changed) *changed = true;
		expr = PromoteArrayExpr(expr);
	}
	else if(changed) {
		*changed = false;
	}

	// C99:6.3.2.1.2: if the expression is an lvalue that is not an array,
	// and the type has qualifiers, remove them if it's allowed
	// (not allowed for 'sizeof', &, ++, --, . and assignment operators)
	if(removeQualifiers && exprType->IsQualified()) {
		if((exprType->WithoutQualifiers()->IsArray() == false) && 
            IsLValue(expr)) {
			// Don't remove for an 'array-to-pointer' cast.
			if(auto castExpr = expr->As<CastExpression>()) {
				if(castExpr->Type() == CastType::ArrayToPtr) {
					return expr;
				}
			}
			
			// Remove the qualifiers.
			return CreateImplicitCast(expr, exprType->WithoutQualifiers(),
                                      CastType::RemoveQual);
		}
	}

	return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::PromoteArrayExpr(shared<Expression> expr) {
	// An 'array of type' is converted to 'pointer to type' (C99:6.3.2.1.3).
	// If this is already a pointer to an array element nothing is done.
	// This should not be called for 'sizeof' and &!
    bool hasQualifiers = false;
    Qualifier qualifiers;
    const Type* exprType = expr->ResultType();

    if(auto qualType = exprType->As<QType>()) {
        // If we have a constant array, the result will be 
        // a constant pointer to the element type.
        hasQualifiers = true;
        qualifiers = qualType->GetQualifiers();
        exprType = qualType->Base();
    }

	if(auto arrayType = exprType->As<ArrayType>()) {
		const Type* result = types_->GetElementPointer(arrayType);

        // Apply the qualifiers, if required.
        if(hasQualifiers) {
            result = types_->GetQualified(result, qualifiers);
        }

		return CreateImplicitCast(expr, result, CastType::ArrayToPtr);

	}
	else return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::PromoteFunctionExpr(shared<Expression> expr) {
	// A function is converted to 'pointer to function returning type' (C99:6.3.2.1.4).
	// If this is already a pointer to function nothing is done.
	// This should not be called for 'sizeof' and &.
    bool hasQualifiers = false;
    Qualifier qualifiers;
    const Type* exprType = expr->ResultType();

    if(auto qualType = exprType->As<QType>()) {
        // If we have a constant array, the result will be 
        // a constant pointer to the element type.
        hasQualifiers = true;
        qualifiers = qualType->GetQualifiers();
        exprType = qualType->Base();
    }

	if(exprType->IsFunction()) {
		const Type* result = types_->GetPointer(expr->ResultType());

        // Apply the qualifiers, if required.
        if(hasQualifiers) {
            result = types_->GetQualified(result, qualifiers);
        }

		return CreateImplicitCast(expr, result, CastType::FunctionToPtr);
	}
	
    return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::CreateImplicitCast(shared<Expression> expr, 
                                       const Type* newType, CastType castType) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(expr)) {
		return expr;
	}

	// The conversion is created only if the types aren't the same.
	// If 'expr' is an implicit cast it's type is changed with the new one
	// (prevents a chain of implicit casts).
	if(expr->ResultType()->Equals(newType)) {
		return expr;
	}
	
	if(auto castExpr = expr->As<CastExpression>()) {
		if(castExpr->IsImplicit()) {
			// Change the type.
			castExpr->SetType(castType);
			castExpr->SetResultType(newType);
			return expr;
		}
	}

	// Create a new implicit cast.
	return new CastExpression(castType, expr, newType, 
                              false /* isExplicit */, expr->Location());
}

} // namespace Parsing