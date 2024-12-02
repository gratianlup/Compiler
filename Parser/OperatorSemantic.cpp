// OperatorSemantic.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that perform semantic analysis on expression operators.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionSemantic.hpp"
#include "DeclarationSemantic.hpp"
#include "../AST/TypeCombiner.hpp"
using namespace AST;

namespace Parsing {

shared<Expression> 
ExpressionSemantic::HandleIncDecOp(shared<Expression> target, bool isIncrement, 
							       bool isPostfix, shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
        return target;
    }

	// C99:6.5.3.1.1: the operand shall have real or pointer type 
	// (qualified or not), and should be a modifiable lvalue.
	const Type* resultType = target->ResultType()->WithoutQualifiers();
	
	if(resultType->WithoutQualifiers()->IsArithmetic()); // OK
	else if(auto temp = resultType->WithoutQualifiers()->As<PointerType>()) {
		// Diagnose pointer to void and function.
		if(temp->PointeeType()->WithoutQualifiers()->IsVoid()) {
			diag_->Report(Error::POINTEE_VOID_ARITHMETIC_OPERATION)<<target->Location();
			return new InvalidExpression();
		}
		else if(temp->PointeeType()->WithoutQualifiers()->IsFunction()) {
			diag_->Report(Error::POINTEE_FUNCTION_ARITHMETIC_OPERATION)<<target->Location();
			return new InvalidExpression();
		}
	}

	ValueType valueType;
	if(IsModifiableLValue(target, &valueType) == false) {
		// This can't be modified.
		EmitLValueDiagnostic(valueType, target->Location());
		return new InvalidExpression();
	}

	// Create the corresponding unary operator expression.
	UnaryOpType op = isIncrement ? UnaryOpType::Inc : UnaryOpType::Dec;
	return new UnaryOperator(op, target, resultType, isPostfix, target->Location());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandlePrefixAddSubOp(shared<Expression> target, bool isAdd, 
								         shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
        return target;
    }

	// C99:6.5.3.3.1: the target of the + and - operators shall have arithmetic type.
	UsualConversion(target);

	if(target->ResultType()->WithoutQualifiers()->IsArithmetic() == false) {
		if(isAdd) { 
			diag_->Report(Error::PREFIX_ADD_NOT_ON_ARITHMETIC_TYPE)<<target->Location();
		}
		else diag_->Report(Error::PREFIX_SUB_NOT_ON_ARITHMETIC_TYPE)<<target->Location();

		return new InvalidExpression();
	}

	// Create the unary operator expression.
	UnaryOpType op = isAdd ? UnaryOpType::Add : UnaryOpType::Sub; 
	return new UnaryOperator(op, target, target->ResultType(), false /* postfix */,
					   target->Location());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleComplementOp(shared<Expression> target, 
                                       shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
        return target;
    }

	// C99:6.5.3.3.1: the target of the ~ operator shall have integer type.
	UsualConversion(target);

	if(target->ResultType()->WithoutQualifiers()->IsInteger() == false) {
		diag_->Report(Error::COMPLEMENT_NOT_ON_INTEGER_TYPE)<<target->Location();
		return new InvalidExpression();
	}

	// Create the unary operator expression.
	return new UnaryOperator(UnaryOpType::Complement, target, target->ResultType(), 
					   false /* postfix */, target->Location());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleNotOp(shared<Expression> target, 
                                shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
        return target;
    }

	// C99:6.5.3.3.1: the target of the ! operator shall have scalar type.
	// The result type is 'int'. Only array/function promotions are applied.
	target = PromoteFunctionArrayExpr(target);

	if(target->ResultType()->WithoutQualifiers()->IsScalar() == false) {
		diag_->Report(Error::PREFIX_NOT_EXPECTED_SCALAR_TYPE)<<target->Location();
		return new InvalidExpression();
	}

	// Create the unary operator expression.
	return new UnaryOperator(UnaryOpType::Not, target, BasicType::GetInt(), 
					   false /* postfix */, target->Location());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type*
ExpressionSemantic::HandleMultiplicativeOp(shared<Expression>& left,
                                           shared<Expression>& right,
									       BinaryOpType opType, bool inAssignment,
									       shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// C99:6.5.5.2: The operands shall have arithmetic type.
	UsualBinaryConversion(left, right, inAssignment == false /* changeLeft */);

	if((left->ResultType()->WithoutQualifiers()->IsArithmetic() == false) ||
	   (right->ResultType()->WithoutQualifiers()->IsArithmetic() == false)) {
		DiagnoseBinaryOp(left, right, Error::INVALID_OPERANDS, opType);
		return nullptr;
	}

	// % should be applied only on integer types.
	if((opType == BinaryOpType::Mod) &&
		((left->ResultType()->WithoutQualifiers()->IsInteger() == false) ||
		 (left->ResultType()->WithoutQualifiers()->IsInteger() == false))) {
		DiagnoseBinaryOp(left, right, Error::INVALID_OPERANDS, opType);
		return nullptr;
	}

	// Warn if this is division/modulo and the right side is 0.
	if(opType != BinaryOpType::Mul) {
		EvaluationInfo eval = right->Evaluate(context_, false /* warn */);
	
		if((eval.IsIntConstant() && (eval.IntValue() == 0)) ||
		   (eval.IsFloatConstant() && (eval.FloatValue() == 0))) {
			eval = right->Evaluate(context_, false /* warn */);
			diag_->Report(Warning::DIVISION_BY_ZERO)<<right->Location();
		}
	}

	// Create the binary operator expression.
	return left->ResultType();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type*
ExpressionSemantic::HandleAdditiveOp(shared<Expression>& left, 
                                     shared<Expression>& right,
							         BinaryOpType opType, bool inAssignment,
							         shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// The following pairs of operands are allowed:
	// C99:6.5.6.2: arithmetic + arithmetic, pointer + integer
	// C99:6.5.6.3: arithmetic - arithmetic, pointer - pointer, pointer - integer
	UsualBinaryConversion(left, right, inAssignment == false /* changeLeft */);

	// The simple case, both operands of arithmetic type.
	if(left->ResultType()->WithoutQualifiers()->IsArithmetic() &&
	   right->ResultType()->WithoutQualifiers()->IsArithmetic()) {
		return left->ResultType();
	}

    // Test for 'void' type ('(void)a + 4' is not valid).
    if(left->ResultType()->WithoutQualifiers()->IsVoid() ||
       right->ResultType()->WithoutQualifiers()->IsVoid()) {
        DiagnoseBinaryOp(left, right, Error::POINTER_ARITHMETIC_EXPECTED_INTEGER, opType);
        return nullptr;
    }

	// Handle the cases that involve pointers. Figure out which expression
	// is a pointer and which is the integer (both can be pointers, too).
	const PointerType* ptr1 = nullptr;
	const PointerType* ptr2 = nullptr;
	const Type* intType     = nullptr;
	
    if(left->ResultType()->WithoutQualifiers()->IsPointer()) {
		ptr1 = left->ResultType()->WithoutQualifiers()->As<PointerType>();
	}
	else intType = left->ResultType(); // 'left' should be an integer.

	if(right->ResultType()->WithoutQualifiers()->IsPointer()) {
		// If the operator is + 'left' should not be a pointer.
		if(ptr1 && (opType == BinaryOpType::Add)) {
			DiagnoseBinaryOp(left, right, Error::ADD_POINTERS_INVALID, opType);
			return nullptr;
		}

		ptr2 = right->ResultType()->WithoutQualifiers()->As<PointerType>();
	}
	else intType = right->ResultType(); // 'right' is an integer.

	// Check that the pointer(s) point to object types (not functions).
	if(ptr1 && ptr1->PointeeType()->WithoutQualifiers()->IsFunction()) {
		DiagnoseBinaryOp(left, right, Error::POINTEE_FUNCTION_ARITHMETIC_OPERATION, opType);
		return nullptr;
	}

	if(ptr2 && ptr2->PointeeType()->WithoutQualifiers()->IsFunction()) {
		DiagnoseBinaryOp(left, right, Error::POINTEE_FUNCTION_ARITHMETIC_OPERATION, opType);
		return nullptr;
	}

	// Handle pointer - pointer.
	if(ptr1 && ptr2) {
		// The pointers should point to qualified or unqualified
		// versions of compatible types (C99:6.5.6.3).
		if(AreTypesCompatible(ptr1->PointeeType()->WithoutQualifiers(), 
							  ptr2->PointeeType()->WithoutQualifiers())) {
			// The resulting type is 'ptrdiff_t' (C99:6.5.6.9).
			return BasicType::GetPtrDiffT();
		}
	}

	// pointer +/- integer otherwise (C99:6.5.6.8). 
	// If it's - the pointer is allowed to be only the left operand.
	// The other operand should be an integer (float not allowed).
	// The result type is the pointer type.
	if(ptr2 && (opType == BinaryOpType::Sub)) {
		DiagnoseBinaryOp(left, right, Error::INVALID_OPERANDS, opType);
		return nullptr;
	}

	if(intType->WithoutQualifiers()->IsInteger() == false) {
		DiagnoseBinaryOp(left, right, Error::POINTER_ARITHMETIC_EXPECTED_INTEGER, opType);
		return nullptr;
	}

	return ptr1 ? ptr1 : ptr2;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type*
ExpressionSemantic::HandleShiftOp(shared<Expression>& left, 
                                  shared<Expression>& right,
							      BinaryOpType opType, bool inAssignment,
							      shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// If we're in an assignment (like 'E1 <<= E2'), E1 is not converted.
	if(inAssignment == false) left = IntegerPromotion(left);
	right = IntegerPromotion(right);

	// C99:6.5.7.2: both operands shall have integer type. The only promotions
	// performed on the types are the integer promotions.
	if((left->ResultType()->WithoutQualifiers()->IsInteger() == false) ||
	   (right->ResultType()->WithoutQualifiers()->IsInteger() == false)) {
		DiagnoseBinaryOp(left, right, Error::SHIFT_OPERANDS_NOT_INTEGER, opType);
		return nullptr;
	}

	// Check if the right operand is negative or greater than the width
	// (in bits) of the left operand (something like 'a << 40', where 'a' is 'int').
	// This check is made only if the expression is an ICE.
	EvaluationInfo eval = right->Evaluate(context_, false /* warn */);

	if(eval.IsIntConstant()) {
		// Make this check only for constants.
		if(eval.IntValue() < 0) {
			DiagnoseBinaryOp(left, right, Warning::SHIFT_NEGATIVE, opType);
		}
		else {
			const BasicType* temp = left->ResultType()->WithoutQualifiers()->As<BasicType>();
			int leftWidth = 8 * temp->Size(context_->Target());

			if(eval.IntValue() > leftWidth) {
				DiagnoseBinaryOp(left, right, Warning::SHIFT_TOO_LARGE, opType);
			}
		}
	}

	// The type of the result is the type of the left operand.
	return left->ResultType();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type*
ExpressionSemantic::HandleComparisonOp(shared<Expression>& left, 
                                       shared<Expression>& right,
								       BinaryOpType opType, 
                                       shared<DeclarationContext> context,
								       LocationInfo operandLocation) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// If both operands have arithmetic types the usual binary conversions are applied.
	// Else we use the ones that are applied for unary operators.
	// The result type is 'int'.
	bool isEquality = (opType == BinaryOpType::EqEq) || (opType == BinaryOpType::NotEq);
	const Type* leftType = left->ResultType();
	const Type* rightType = right->ResultType();

	if(leftType->WithoutQualifiers()->IsArithmetic() && 
	   rightType->WithoutQualifiers()->IsArithmetic()) {
		UsualBinaryConversion(left, right);
		leftType = left->ResultType();
		rightType = right->ResultType();
	}
	else {
		left = UsualConversion(left);
		right = UsualConversion(right);
		leftType = left->ResultType();
		rightType = right->ResultType();
	}

    // Continue without qualifiers from now on.
    leftType = leftType->WithoutQualifiers();
    rightType = rightType->WithoutQualifiers();

	// For all operators (C99:6.5.8.2/C99:6.5.9.2): 
	// - both operands should have arithmetic type, or
	// - both op. are pointers to qualified/unqualified of compatible types.
	//   If not an equality operator, pointers to function are not allowed.
	//
	// Only for == and !=:
	// - one op. is a pointer, the other one is a null pointer constant, or
	// - one op. is a pointer to object, the other one is a pointer to 'void'.
	if(leftType->IsArithmetic() && rightType->IsArithmetic()) {
		return BasicType::GetInt();
	}

	bool isLeftNull  = left->IsNullPointer(context_);
	bool isRightNull = right->IsNullPointer(context_);
	
	if(isEquality && (isLeftNull != isRightNull)) {
		// pointer + null pointer constant
		if(isLeftNull && rightType->IsPointer()) {
			left = CreateImplicitCast(right, right->ResultType(), CastType::ToPointer);
			return BasicType::GetInt();
		}
		else if(leftType->IsPointer()) {
			right = CreateImplicitCast(right, left->ResultType(), CastType::ToPointer);
			return BasicType::GetInt();
		}

		return nullptr; // Both 0 or one not a pointer.
	}
	
	if(leftType->IsPointer() && rightType->IsPointer()) {
		// The pointed types should be compatible.
		const Type* leftPointee = leftType->As<PointerType>()
                                          ->PointeeType()->WithoutQualifiers();
		const Type* rightPointee = rightType->As<PointerType>()
                                            ->PointeeType()->WithoutQualifiers();
		
		if(AreTypesCompatible(leftPointee, rightPointee)) {
			// Check for pointers to function, because these aren't objects.
			// These are accepted if this is an equality operator (==, !=).
			if(leftPointee->IsFunction() && (isEquality == false)) {
				DiagnoseBinaryOp(left, right, Error::COMPARISON_FUNCTION_POINTERS, opType);
				return nullptr;
			}
		}
		else if(leftPointee->IsVoid() || rightPointee->IsVoid()) {
			// C99:6.5.9.5: a pointer to 'void' is converted to the type
			// of the other operand. Don't allow pointers to functions,
			// because these are allowed to be compared only to null pointer constants.
			if(leftPointee->IsFunction() || rightPointee->IsFunction()) {
				DiagnoseBinaryOp(left, right, Error::COMPARISON_FUNCTION_WITH_VOID, opType);
				return nullptr;
			}

			if(leftPointee->IsVoid()) {
				left = CreateImplicitCast(right, right->ResultType(), CastType::ToPointer);
			}
			else right = CreateImplicitCast(right, left->ResultType(), CastType::ToPointer);
		}
		else {
			// Pointer to types that are not compatible.
			DiagnoseBinaryOp(left, right, Error::COMPARISON_DISTINCT_POINTERS, opType);
			return nullptr;
		}

		return BasicType::GetInt();
	}
	
	// All other cases are invalid.
	DiagnoseBinaryOp(left, right, Error::COMPARISON_DISTINCT_TYPES, opType);
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type*
ExpressionSemantic::HandleBitwiseOp(shared<Expression>& left, 
                                    shared<Expression>& right,
							        BinaryOpType opType, bool inAssignment,
							        shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// Both operands shall have integer type.
	// The usual binary conversions are performed.
	UsualBinaryConversion(left, right, inAssignment == false /* changeLeft */);

	if((left->ResultType()->IsInteger() == false) ||
	   (right->ResultType()->IsInteger() == false)) {
		DiagnoseBinaryOp(left, right, Error::BITWISE_OPERANDS_NOT_INTEGER, opType);
		UsualBinaryConversion(left, right, inAssignment == false);
		return nullptr;
	}

	return left->ResultType();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type*
ExpressionSemantic::HandleLogicalOp(shared<Expression>& left, 
                                    shared<Expression>& right,
							        BinaryOpType opType, 
                                    shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// C99:6.5.13.2: Both operands should have scalar type. The usual conversions
	// are performed before testing. The result is an 'int' (1 - true, 0 - false). 
	// The same rules apply to both && and ||.
	left = UsualConversion(left);
	right= UsualConversion(right);

	if((left->ResultType()->WithoutQualifiers()->IsScalar() == false) ||
		(left->ResultType()->WithoutQualifiers()->IsScalar() == false)) {
		DiagnoseBinaryOp(left, right, Error::LOGICAL_OPERANDS_NOT_SCALAR, opType);
		return nullptr;
	}

	return BasicType::GetInt();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression>
ExpressionSemantic::HandleConditionalOp(shared<Expression>& condition,
                                        shared<Expression>& left,
								        shared<Expression>& right, 
                                        LocationInfo operandLocation) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(condition) || Expression::IsInvalid(left) || 
	   Expression::IsInvalid(right)) {
		return nullptr;
	}

	// C99:6.5.15.2: the first operand should have scalar type.
	// The second and third operands should both have:
	// - arithmetic types
	// - the same struct/union type
	// - void type (functions returning void, for example)
	// - pointers to compatible types (with/without qualifiers)
	// - one op. is a pointer, the other one is a null pointer constant
	// - one op. is a pointer to an object/incomplete type, the other one a pointer
	//   to void (with/without qualifiers). Note that this doesn't accept functions.
	const Type* resultType = nullptr; // The type of the result.

	// Perform the usual conversions first.
	condition = UsualConversion(condition);
	left      = UsualConversion(left);
	right     = UsualConversion(right);

	if(condition->ResultType()->WithoutQualifiers()->IsScalar() == false) {
		diag_->Report(Error::CONDITIONAL_NOT_ON_SCALAR)<<operandLocation;
		return nullptr;
	}

	// The types without qualifiers are used.
	const Type* leftType  = left->ResultType()->WithoutQualifiers();
	const Type* rightType = right->ResultType()->WithoutQualifiers();
	bool isLeftNull  = left->IsNullPointer(context_);
	bool isRightNull = right->IsNullPointer(context_);

	// Test for common case, both arithmetic.
	if(leftType->IsArithmetic() && rightType->IsArithmetic()) {
		// The resulting type is the common type after the usual arithmetic
		// conversions (C99:6.5.15.5).
		UsualBinaryConversion(left, right);
		resultType = left->ResultType();
	}
	else if(leftType->IsStruct() || leftType->IsUnion()) {
		// struct and union are treated together.
		// First check that the other type is the same.
		if(leftType->IsStruct() && (rightType->IsStruct() == false)) {
			diag_->Report(Error::CONDITIONAL_RIGHT_NOT_STRUCT)<<operandLocation;
			return nullptr;
		}

		if(leftType->IsUnion() && (rightType->IsUnion() == false)) {
			diag_->Report(Error::CONDITIONAL_RIGHT_NOT_UNION)<<operandLocation;
			return nullptr;
		}

		// Both types should be complete and the same (not only compatible).
		if(leftType->IsIncomplete() || rightType->IsIncomplete()) {
			diag_->Report(Error::CONDITIONAL_INCOMPLETE_TYPES)<<operandLocation;
			return nullptr;
		}

		if(leftType->Equals(rightType) == false) {
			// The types are not the same.
			diag_->Report(Error::CONDITIONAL_DISTINCT_TYPES)<<operandLocation;
			return nullptr;
		}

		// The resulting type is the struct/union type, without any qualifiers.
		resultType = left->ResultType()->WithoutQualifiers();
	}
	else if(leftType->IsVoid()) {
		if(rightType->IsVoid() == false) {
			// The right type is not void.
			diag_->Report(Error::CONDITIONAL_RIGHT_NOT_VOID)<<operandLocation;
			return nullptr;
		}

		// If both operands have 'void' type, the resulting type is 'void'.
		// Note that this removes any qualifiers.
		resultType = BasicType::GetVoid();
		left  = CreateImplicitCast(left, resultType, CastType::ToVoid);
		right = CreateImplicitCast(right, resultType, CastType::ToVoid);
	}
	else if(isLeftNull != isRightNull) {
		// '0 : pointer' or 'pointer : 0'.
		// The resulting type is the type that is not the null pointer.
		// The null is promoted to a pointer (in case it's not one already).
		if(isLeftNull) {
			resultType = right->ResultType();
			left  = CreateImplicitCast(left, resultType, CastType::IntToPointer);
		}
		else {
			resultType = left->ResultType();
			right = CreateImplicitCast(right, resultType, CastType::IntToPointer);
		}
	}
	else if(leftType->IsPointer() && rightType->IsPointer()) {
		resultType = HandleConditionalPointers(leftType, rightType, left, 
                                               right, operandLocation);
	}

	if(resultType == nullptr) {
		// The types are not the same.
		diag_->Report(Error::CONDITIONAL_DISTINCT_TYPES)<<operandLocation;
		return nullptr;
	}
	else return new ConditionalOperator(condition, left, right, resultType, operandLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* 
ExpressionSemantic::HandleConditionalPointers(const Type* leftType, 
                                              const Type* rightType, 
									          shared<Expression>& left, 
                                              shared<Expression>& right, 
										      LocationInfo operandLocation) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// See C99:6.5.15.6 for details.
	const Type* resultType;
	const Type* leftP  = leftType->As<PointerType>()->PointeeType();
	const Type* rightP = rightType->As<PointerType>()->PointeeType();
	const Type* leftPNQual  = leftP->WithoutQualifiers();
	const Type* rightPNQual = rightP->WithoutQualifiers();

	// Handle 'void* : pointer' and 'pointer : void*' first, it's simpler.
	// Test the non-qualified versions because 'void' can be qualified.
	// Both pointers pointing to 'void' is not allowed.
	const Type* voidType  = nullptr;
	const Type* otherType  = nullptr;

	if(leftPNQual->IsVoid()) {
		voidType = leftPNQual;
		otherType = rightPNQual;
	}

	if(rightPNQual->IsVoid()) {
		if(voidType) {
			// Both types are pointers to void, not allowed.
			diag_->Report(Error::CONDITIONAL_BOTH_POINTERS_TO_VOID)<<operandLocation;
			return nullptr;
		}

		voidType  = rightPNQual;
		otherType = leftPNQual;
	}

	if(voidType) {
		// The other type needs to be an object, so functions are not allowed.
		if(otherType->IsFunction()) {
			diag_->Report(Error::CONDITIONAL_FUNCTION_POINTER)<<operandLocation;
			return nullptr;
		}

		// The resulting type is a pointer to the qualified version of 'void'
		// with the qualifiers from the other type.
		resultType = BasicType::GetVoid();
	}
	else if(AreTypesCompatible(leftPNQual, rightPNQual)) {
		// The resulting type is a pointer to the composite type of the
		// pointed types, with their qualifiers combined.
		// Note that this can't fail because we know the types are compatible.
		resultType = typeComb_.Combine(leftPNQual, rightPNQual);
	}
	else {
		// The types are not compatible, report and fail.
		diag_->Report(Error::CONDITIONAL_DISTINCT_TYPES)<<operandLocation;
		return nullptr;
	}
	
	// We have a type. Apply the combined qualifiers (if any).
	Qualifier combQuals;
				
	if(leftP->IsQualified()) {
		combQuals = leftP->As<QType>()->GetQualifiers();
	}

	if(rightP->IsQualified()) {
		combQuals = combQuals.Combine(rightP->As<QType>()->GetQualifiers());
	}

	// If there are qualifiers create a 'QType'.
	// All this is wrapped into a 'PointerType'.
	if(combQuals.HasNone() == false) {
		resultType = types_->GetQualified(resultType, combQuals);
	}

	// Apply the cast to the left and right expressions.
	resultType = types_->GetPointer(resultType);
	left  = CreateImplicitCast(left, resultType, CastType::ToPointer);
	right = CreateImplicitCast(right, resultType, CastType::ToPointer);

	return resultType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type*
ExpressionSemantic::HandleAssignmentOp(shared<Expression>& left, 
                                       shared<Expression>& right,
								       BinaryOpType opType, 
                                       shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// If this is a compound assignment handle first the operation,
	// and if valid, continue to validate the assignment.
	const Type* compType = nullptr;

	if(opType != BinaryOpType::Eq) {
		switch(opType) {
			case BinaryOpType::AddEq: {
				compType = HandleAdditiveOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::SubEq: {    
				compType = HandleAdditiveOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::MulEq: {
				compType = HandleMultiplicativeOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::DivEq: {   
				compType = HandleMultiplicativeOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::ModEq: {
				compType = HandleMultiplicativeOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::AndEq: {
				compType = HandleBitwiseOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::OrEq: {
				compType = HandleBitwiseOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::XorEq: {
				compType = HandleBitwiseOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::ShiftLEq: {
				compType = HandleShiftOp(left, right, opType, true, context);
				break;
			}
			case BinaryOpType::ShiftREq: {
				compType = HandleShiftOp(left, right, opType, true, context);
				break;
			}
		}

		if(compType == nullptr) return nullptr;
	}
	
	// The left expression should be a modifiable lvalue.
	ValueType valueType;

	if(IsModifiableLValue(left, &valueType) == false) {
		EmitLValueDiagnostic(valueType, left->Location());
		return nullptr;
	}

	// Typecheck the assignment.
	if(IsSimpleAssignmentValid(left->ResultType(), right, compType)) {
		// Assignment is valid, now convert the right expression
		// to the left (if the types are the same no conversion will be done).
		right = CreateImplicitCast(right, left->ResultType(), CastType::Unknown);

		// The resulting type is the left type, without qualifiers (6.5.16.3).
		// Warn if any of the qualifiers are dropped during assignment.
		WarnQualifierDrop(left->ResultType(), right->ResultType(), right, 
						  nullptr /* name */, false /* isArg */, right->Location());

		return left->ResultType()->WithoutQualifiers();
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionSemantic::AreQualifiersCompatible(const Type* a, const Type* b) {
	// If both types are not qualified we're done.
	if((a->IsQualified() == false) && (b->IsQualified()) == false) {
		return true;
	}

	// If only one is qualified it's OK as long as the other has 'volatile' or 'const'.
	const QType* aQual = a->As<QType>();
	const QType* bQual = b->As<QType>();

	// If the second has no qualifiers it's OK.
	if(bQual == nullptr) return true;

	if(bQual->HasVolatile() && 
	  ((aQual == nullptr) || (aQual->HasVolatile() == false))) {
		return false;
	}

	if(bQual->HasConst() &&
	   ((aQual == nullptr) || (aQual->HasConst() == false))) {
		return a->IsChar();
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool
ExpressionSemantic::IsSimpleAssignmentValid(const Type* left, shared<Expression>& right,
									        const Type* compoundType) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(right)) {
		return true;
	}

	// This method is also used for type-checking function arguments.
	// Apply the function/array conversion first (only on the right expression).
	// All checks are made on the unqualified version of the types.
	right = PromoteFunctionArrayExpr(right);
	const Type* leftType = left->WithoutQualifiers();
	const Type* rightType = compoundType ?
							compoundType->WithoutQualifiers() :
							right->ResultType()->WithoutQualifiers();

	// Check all constraints from C99:6.5.16.1:
	// arithmetic = arithmetic
	if(leftType->IsArithmetic() && rightType->IsArithmetic()) {
		return true;
	}

	// struct/union = compatible struct/union
	if(leftType->IsStruct() && AreTypesCompatible(leftType, rightType)) {
		return true;
	}
	else if(leftType->IsUnion() && AreTypesCompatible(leftType, rightType)) {
		return true;
	}

	if(leftType->IsPointer()) {
		if(rightType->IsPointer()) {
			// pointer = pointer. The pointed type should be compatible
			// and the qualifiers (if any) should be the same.
			const Type* leftPointee = leftType->As<PointerType>()->PointeeType();
			const Type* rightPointee = rightType->As<PointerType>()->PointeeType();

			if(AreQualifiersCompatible(leftPointee, rightPointee) == false) {
				// The types don't have the same (or compatible) qualifiers.
				// EXTENSION:
				// Allow the assignment, but warn about the qualifier drop.
				DiagnoseBinaryOp(left, right, Warning::ASSIGNMENT_QUALIFIERS_DROPPED, 
                                 BinaryOpType::Eq);
			}
			
			// Continue without the qualifiers.
			leftPointee = leftPointee->WithoutQualifiers();
			rightPointee = rightPointee->WithoutQualifiers();

			// pointer = compatible pointer or pointer = void*
			if(AreTypesCompatible(leftPointee, rightPointee)) {
				return true;
			}
			else if(leftPointee->IsVoid() || rightPointee->IsVoid()) {
                // A pointer and void*.
				return true;
			}
			else {
				// EXTENSION:
				// Pointer to types that are not compatible.
				// Instead of an error we emit just a warning (this is the behavior
				// of both GCC and VC); allows things like 'void f(int* a); int t[2][3]; f(t);'
				DiagnoseBinaryOp(left, right, Warning::ASSIGNMENT_INCOMPATIBLE_POINTERS, 
								 BinaryOpType::Eq);
				return true;
			}
		}
		else if((compoundType == nullptr) && right->IsNullPointer(context_)) {
			// pointer = 0. This is disabled for compound assignments.
			return true;
		}
		else if(auto temp = rightType->As<BasicType>()) {
			// EXTENSION:
			// Allow a pointer to take the value of any integer, but warn.
			if(temp->IsInteger()) {
				DiagnoseBinaryOp(left, right, Warning::INT_ASSIGNED_TO_POINTER, 
                                 BinaryOpType::Eq);
				return true;
			}
		}
	}
	else if(auto temp = leftType->As<BasicType>()) {
		// _Bool = pointer
		if(temp->IsBool() && rightType->IsPointer()) {
			return true;
		}

		// As an extension allow the assignment of a pointer to any of the integer types.
		if(temp->IsInteger() && rightType->IsPointer()) {
			DiagnoseBinaryOp(left, right, Warning::POINTER_ASSIGNED_TO_INT, BinaryOpType::Eq);
			return true;
		}
	}

	// All other cases are invalid.
	DiagnoseBinaryOp(left, right, Error::INVALID_OPERANDS, BinaryOpType::Eq);
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type*
ExpressionSemantic::HandleCommaOp(shared<Expression>& left, shared<Expression>& right,
							      BinaryOpType opType,shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(left) || Expression::IsInvalid(right)) {
		return nullptr;
	}

	// The result is the type of the right expression (C99:6.5.17.2).
	right = PromoteFunctionArrayExpr(right);
	return right->ResultType();
}

} // namespace Parsing