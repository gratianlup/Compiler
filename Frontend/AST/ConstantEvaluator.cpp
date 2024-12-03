// ConstantEvaluator.cpp
// Copyright (c) Lup Gratian
//
// Implements the constant evaluator.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantEvaluator.hpp"
#include "../Abstraction/Platform.hpp"

namespace AST {

ConstantEvaluator::ConstantEvaluator(const Expression* expr, bool expectICE, 
									 const Context* context, bool allowVars, 
                                     bool warn, bool ice, bool markConst) : 
		expectICE_(expectICE), invalid_(false), warn_(warn), allowVars_(allowVars),
		context_(context), markConst_(markConst) {
	expr->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::MarkConstantExpression(const Expression* expr) {
	if(markConst_) {
		bool isConst = invalid_ == false;
		const_cast<Expression*>(expr)->SetIsConstant(isConst);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const SubscriptExpression* expr) {
	// The base should have pointer type and index should be an integer.
	// The result is the offset, in bytes, from the 0 index.
	expr->Base()->Accept(this);
	EvaluationResult base = result_;
	if(invalid_) return;

	expr->Index()->Accept(this);
	EvaluationResult index = result_;
	if(invalid_) return;

	// Compute the offset and push it on the stack.
	auto resultType = base.ResultType->WithoutQualifiers()
                                     ->As<PointerType>()->PointeeType();
	ValueUnion resultValue = TypeSize(resultType, context_).Size() * 
						   index.Value.IntValue;

	result_ = EvaluationResult(resultValue, resultType, true /* isInt */, 
                               base.HasVariable, base.BaseVariable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const MemberExpression* expr) {
	// The member can be accessed with . or ->.
	expr->Object()->Accept(this);
	if(invalid_) return;

	// Compute the layout of the struct/union and take the offset of the member.
	const Type* type;

	if(expr->IsPointer()) {
		type = expr->Object()->ResultType()->WithoutQualifiers()
                   ->As<PointerType>()->PointeeType();
	}
	else type = expr->Object()->ResultType()->WithoutQualifiers();

	StructLayout layout(type->WithoutQualifiers()->As<StructUnionType>(), context_);
	auto& info = layout.GetFieldInfo(expr->Member()->Name());

	result_ = EvaluationResult(ValueUnion(info.ByteOffset()), 
                               expr->Member()->DeclarationType(), 
						       true /* isInt */, result_.HasVariable, 
                               result_.BaseVariable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const DeclarationExpression* expr) {
	// If it's a reference to a enumeration constant we use it's value.
	// Else we're interested only in the type of the object.
	if(auto temp = expr->Object()->As<EnumConstDeclaration>()) {
		// The enum constant should be an integer too.
        __int64 enumValue;

        if(temp->ValueExpr()) {
		    EvaluationInfo info = temp->ValueExpr()->EvaluateAsICE(context_, true);
            enumValue = info.IntValue();
		    
            if(info.IsIntConstant() == false) {
			    invalid_ = true;
			    return;
		    }
        }
        else {
            // The 'enum' constant has no expression assigned to it yet.
            // This can happen when we're parsing an 'enum' and other
            // 'enum' constants reference this one.
            enumValue = 0;
        }

		result_ = EvaluationResult(ValueUnion(enumValue), expr->ResultType());
	}
	else {
		if(allowVars_ == false) {
			// Variables are not allowed in the expression.
			invalid_ = true;
			return;
		}

		result_ = EvaluationResult(ValueUnion(0LL), expr->ResultType(),
						     false /* isInt */, true /* isVariable */, expr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const NumberConstant* expr) {
	// For numbers we just take the value.
	const NumberInfo& info = expr->Value();
	
	if(info.IsInteger) {
		result_ = EvaluationResult(ValueUnion(info.IntValue), 
                                   expr->ResultType());
	}
	else {
		result_ = EvaluationResult(ValueUnion(info.FloatValue), 
                             expr->ResultType(), false /* isInt */);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const CharConstant* expr) {
	// Take the integer representation of the character.
	const CharInfo& info = expr->Value();
	result_ = EvaluationResult(ValueUnion((__int64)info.Value),
                               expr->ResultType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const StringConstant* expr) {
	// If we're expecting an ICE reject it. Else consider that the value is offset 0.
	if(expectICE_) {
		invalid_ = true;
		return;
	}

	result_ = EvaluationResult(ValueUnion(0LL), expr->ResultType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const CallExpression* expr) {
	// Calls are not permitted.
	invalid_ = true;
	hasSideffects_ = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const BinaryOperator* expr) {
	// This method handles some invalid cases, but acts more like a dispatch
	// mechanism to the appropriate handling method based on the operator type.
	switch(expr->Operator()) {
		case BinaryOpType::Add:
		case BinaryOpType::Sub: {
			EvaluateAdditiveOp(expr);
			break;
		} 
		case BinaryOpType::Mul:
		case BinaryOpType::Div:
		case BinaryOpType::Mod: {
			EvaluateMultiplicativeOp(expr);
			break;
		} 
		case BinaryOpType::Eq:      		
		case BinaryOpType::AddEq:    		
		case BinaryOpType::SubEq:    		
		case BinaryOpType::MulEq:    		
		case BinaryOpType::ModEq:    		
		case BinaryOpType::DivEq:    		
		case BinaryOpType::AndEq:    		
		case BinaryOpType::OrEq:
		case BinaryOpType::ShiftLEq: 		
		case BinaryOpType::ShiftREq:
		case BinaryOpType::XorEq: {
			// Assignments can't appear in a constant expression.
			invalid_ = true;
			hasSideffects_ = true;
			break;
		}
		case BinaryOpType::EqEq: 
		case BinaryOpType::NotEq:
		case BinaryOpType::Less: 
		case BinaryOpType::LessEq: 
		case BinaryOpType::Greater: 
		case BinaryOpType::GreaterEq: {
			EvaluateComparisonOp(expr);
			break;
		}
		case BinaryOpType::AndAnd:
		case BinaryOpType::OrOr: {
			EvaluateLogicalOp(expr);
			break;
		}
		case BinaryOpType::And: 
		case BinaryOpType::Or: 
		case BinaryOpType::Xor: 
		case BinaryOpType::ShiftR: 
		case BinaryOpType::ShiftL: {
			EvaluateBitwiseOp(expr);
			break;
		}
		case BinaryOpType::Comma: {
			// Comma can't appear, except as a subexpression of 'sizeof'
			// (but we wouldn't be here if it were that exception).
			invalid_ = true;
			break;
		}
	}

	MarkConstantExpression(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::EvaluateAdditiveOp(const BinaryOperator* expr) {
	// Evaluate the left and right subexpressions.
	expr->LeftValue()->Accept(this);
	EvaluationResult left = result_;
	if(invalid_) return;

	expr->RightValue()->Accept(this);
	EvaluationResult right = result_;
	if(invalid_) return;

	// Handle operations involving pointers.
	if(left.ResultType->WithoutQualifiers()->IsPointer() || 
	   right.ResultType->WithoutQualifiers()->IsPointer()) {
		// This can be the addition of two pointers or of a pointer with an integer.
		// Let another method handle these cases.
		EvaluatePointerAdditiveOp(expr, left, right);
		return;
	}
	
    // The operands should not be references to variables.
    if(left.HasVariable || right.HasVariable) {
        invalid_ = true;
        return;
    }

	bool hasOverflow = false;

	if(left.ResultType->WithoutQualifiers()->IsInteger()) {
		// The operands should not be references to variables.
		if(left.HasVariable || right.HasVariable) {
			invalid_ = true;
			return;
		}

		auto target = context_->Target();
		auto leftType = left.ResultType->WithoutQualifiers()->As<BasicType>();
		auto rightType = left.ResultType->WithoutQualifiers()->As<BasicType>();
		auto diag = const_cast<Context*>(context_)->Diagnostic();

		if(expr->Operator() == BinaryOpType::Add) {
			// The operands need to be added.
			ValueUnion value = ValueUnion(left.Value.IntValue + right.Value.IntValue);
			result_ = EvaluationResult(value, left.ResultType);

			// We're done if we don't need to check for overflow.
			if((warn_ == false) && (expectICE_ == false)) {
                return;
            }

			// Check overflow. If both operands have different signs or the sign
			// of the first operand corresponds with the sign of the result
			// there is no overflow. For unsigned, the result should not be smaller
			// than the first operand.
			__int64 resultValue = target->Truncate(value.IntValue, leftType->GetKind());
            hasOverflow = AdditionOveflowed(expr, resultValue, left, right);
		}
		else {
			// The operands need to be subtracted.
			ValueUnion value = ValueUnion(left.Value.IntValue - right.Value.IntValue);
			result_ = EvaluationResult(value, left.ResultType);

			// We're done if we don't need to check for overflow.
			if((warn_ == false) && (expectICE_ == false)) {
                return;
            }

			// Check overflow. If both operands have the same signs or the sign
			// of the first operand corresponds with the sign of the result
			// there is no overflow. For unsigned, the result should not be bugger
			// than the first operand.
			__int64 resultValue = target->Truncate(value.IntValue, leftType->GetKind());
            hasOverflow = SubtractionOveflowed(expr, resultValue, left, right);
		}
	}
	else if(expr->Operator() == BinaryOpType::Add) {
		// Floating-point addition. 
		// Don't do it if one of the operands is NaN or Infinity.
		if(Abstraction::FloatInfo::IsNaN(left.Value.FloatValue)      ||
		   Abstraction::FloatInfo::IsNaN(right.Value.FloatValue)     ||
		   Abstraction::FloatInfo::IsInfinity(left.Value.FloatValue) ||
		   Abstraction::FloatInfo::IsInfinity(right.Value.FloatValue)) {
			invalid_ = true;
			return;
		}
		
		ValueUnion value = ValueUnion(left.Value.FloatValue + right.Value.FloatValue);
		result_ = EvaluationResult(value, left.ResultType, false /* isInt */);
	}
	else {
		// Floating-point subtraction.
		// Don't do it if one of the operands is NaN or Infinity.
		if(Abstraction::FloatInfo::IsNaN(left.Value.FloatValue)      ||
		   Abstraction::FloatInfo::IsNaN(right.Value.FloatValue)     ||
		   Abstraction::FloatInfo::IsInfinity(left.Value.FloatValue) ||
		   Abstraction::FloatInfo::IsInfinity(right.Value.FloatValue)) {
			invalid_ = true;
			return;
		}
		
		ValueUnion value = ValueUnion(left.Value.FloatValue - right.Value.FloatValue);
		result_ = EvaluationResult(value, left.ResultType, false /* isInt */);
	}

	// If we're expecting an ICE and there was overflow give up.
	if(expectICE_ && hasOverflow) {
		invalid_ = true;
		return;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::AdditionOveflowed(const BinaryOperator* expr, __int64 result, 
                                          EvaluationResult& left, EvaluationResult& right) {
    // Check overflow. If both operands have the same signs or the sign
	// of the first operand corresponds with the sign of the result
	// there is no overflow. For unsigned, the result should not be bugger
	// than the first operand.
    auto diag = const_cast<Context*>(context_)->Diagnostic();
    auto leftType = left.ResultType->WithoutQualifiers()->As<BasicType>();

	if(left.ResultType->WithoutQualifiers()->IsSigned()) {
		__int64 mask = context_->Target()->GetSignMask(leftType->GetKind());
		bool sameSigns1 = ((left.Value.IntValue ^ right.Value.IntValue) & mask) != 0;
		bool sameSigns2 = ((left.Value.IntValue ^ result) & mask) == 0;

		if((sameSigns1 || sameSigns2) == false) {
			// There was signed overflow.
			if(warn_) {
                diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
            }

			return true;
		}
	}
	else if((unsigned __int64)result > (unsigned __int64)left.Value.IntValue) {
		// There was unsigned overflow.
		if(warn_) {
            diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
        }

		return true;
	}

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::SubtractionOveflowed(const BinaryOperator* expr, __int64 result, 
                                             EvaluationResult& left, EvaluationResult& right) {
    // Check overflow. If both operands have the same signs or the sign
	// of the first operand corresponds with the sign of the result
	// there is no overflow. For unsigned, the result should not be bugger
	// than the first operand.
    auto diag = const_cast<Context*>(context_)->Diagnostic();
    auto leftType = left.ResultType->WithoutQualifiers()->As<BasicType>();

	if(left.ResultType->WithoutQualifiers()->IsSigned()) {
		__int64 mask = context_->Target()->GetSignMask(leftType->GetKind());
		bool sameSigns1 = ((left.Value.IntValue ^ right.Value.IntValue) & mask) != 0;
		bool sameSigns2 = ((left.Value.IntValue ^ result) & mask) == 0;

		if((sameSigns1 || sameSigns2) == false) {
			// There was signed overflow.
			if(warn_) {
                diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
            }

			return true;
		}
	}
	else if((unsigned __int64)result > (unsigned __int64)left.Value.IntValue) {
		// There was unsigned overflow.
		if(warn_) {
            diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
        }

		return true;
	}

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::ContainsReference(const Expression* expr) {
	expr = expr->WithoutCasts();

	if(auto binaryOp = expr->As<BinaryOperator>()) {
		return ContainsReference(binaryOp->LeftValue()) ||
			   ContainsReference(binaryOp->RightValue());
	}
	else if(auto unaryOp = expr->As<UnaryOperator>()) {
		if(unaryOp->Operator() == UnaryOpType::Address) {
			return false;
		}
		else return ContainsReference(unaryOp->Value());
	}
	else if(auto conditionalOp = expr->As<ConditionalOperator>()) {
		return ContainsReference(conditionalOp->Condition());
	}

	return expr->IsDeclarationExpr();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::EvaluatePointerAdditiveOp(const BinaryOperator* expr, 
												  EvaluationResult left, 
                                                  EvaluationResult right) {
	// Figure out which is the pointer and which is the integer
	// (both can be pointers, too).
	EvaluationResult* intValue = nullptr;
	EvaluationResult* ptrValue1 = nullptr;
	EvaluationResult* ptrValue2 = nullptr;

	if(left.ResultType->WithoutQualifiers()->IsPointer()) {
        ptrValue1 = &left;
    }
	else intValue = &left;

	if(right.ResultType->WithoutQualifiers()->IsPointer()) {
		if(ptrValue1 == nullptr) {
            ptrValue1 = &right;
        }
		else ptrValue2 = &right;
	}
	else intValue = &right;
	
	// Make sure we don't have references on both sides , like in 'a - b'.
	if(ContainsReference(expr->LeftValue()) && 
	   ContainsReference(expr->RightValue())) {
		invalid_ = true;
		return;
	}

	const Type* pointee = left.ResultType->WithoutQualifiers()->
                          As<PointerType>()->PointeeType();
	__int64 pointeeSize = TypeSize(pointee, context_).Size();

	if(ptrValue2) {
        // We have the 'pointer - pointer' case. 
        // We need to make sure that the base variable is the same.
        // For example, '&a[4] - &b[2]' is not a constant, because we don't know
        // the relationship between the address of 'a' and 'b'.
        if(((left.BaseVariable && right.BaseVariable) == false) ||
            left.BaseVariable->Object() != right.BaseVariable->Object()) {
            invalid_ = true;
            return;
        }

		// Note the operator can't be '+' because it
		// isn't allowed by the semantic analysis step.
		ValueUnion value = ValueUnion((left.Value.IntValue - right.Value.IntValue) /
									   pointeeSize);

		// The result is a 'ptrdiff_t' that represents the difference in elements.
		// We don't allow the result to be negative (like for '&a[1] - &a[3]').
		if(value.IntValue < 0) invalid_ = true;
		else result_ = EvaluationResult(value, BasicType::GetPtrDiffT());
		return;
	}

	// The 'pointer +/- integer' case. The result type is the pointer type.
	// The offset is adjusted with 'value * pointeeSize' bytes.
	ValueUnion value;

	if(expr->Operator() == BinaryOpType::Add) {
		value = ValueUnion(ptrValue1->Value.IntValue + 
                           (intValue->Value.IntValue * pointeeSize));
	}
	else value = ValueUnion(ptrValue1->Value.IntValue - 
                            (intValue->Value.IntValue * pointeeSize));

	result_ = EvaluationResult(value, ptrValue1->ResultType, true /* isInt */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::EvaluateBitwiseOp(const BinaryOperator* expr) {
	// Evaluate the left and right subexpressions.
	expr->LeftValue()->Accept(this);
	EvaluationResult left = result_;
	if(invalid_) return;

	expr->RightValue()->Accept(this);
	EvaluationResult right = result_;
	if(invalid_) return;

	// The operands should not be references to variables.
	if(left.HasVariable || right.HasVariable) {
		invalid_ = true;
		return;
	}

	ValueUnion value;
	__int64 leftValue = left.Value.IntValue;
	__int64 rightValue = right.Value.IntValue;
	bool hasOverflow = false;

	switch(expr->Operator()) {
		case BinaryOpType::And:    { value = ValueUnion(leftValue &  rightValue); break; }
		case BinaryOpType::Or:     { value = ValueUnion(leftValue |  rightValue); break; }
		case BinaryOpType::Xor:    { value = ValueUnion(leftValue ^  rightValue); break; }
		case BinaryOpType::ShiftR: {
			// If the left operand is unsigned we need to force the insertion of
			// 0 from the right (else the sign bit would be inserted).
			if(left.ResultType->WithoutQualifiers()->IsUnsigned()) {
				value = ValueUnion((__int64)((unsigned __int64)leftValue >> rightValue));
			}
			else value = ValueUnion(leftValue >> rightValue);

			break;
		}
		case BinaryOpType::ShiftL: { 
			value = ValueUnion(leftValue << rightValue);

			// Warn about overflow if requested.
			if((warn_ == false) && (expectICE_ == false)) {
				result_ = EvaluationResult(value, left.ResultType);
				return;
			}

			// If the sign of the first operand corresponds with the sign
			// of the result there is no overflow. For unsigned, the result 
			// should not be smaller than the first operand.
			auto target = context_->Target();
			auto leftType = left.ResultType->WithoutQualifiers()->As<BasicType>();

			__int64 resultValue = target->Truncate(value.IntValue, leftType->GetKind());
            hasOverflow = ShiftLeftOveflowed(expr, resultValue, left, right);
		}
	}

	// If we're expecting an ICE and there was overflow give up.
	if(expectICE_ && hasOverflow) {
		invalid_ = true;
		return;
	}

	result_ = EvaluationResult(value, left.ResultType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::ShiftLeftOveflowed(const BinaryOperator* expr, __int64 result, 
                                           EvaluationResult& left, EvaluationResult& right) {
    // If the sign of the first operand corresponds with the sign
		// of the result there is no overflow. For unsigned, the result 
		// should not be smaller than the first operand.
    auto diag = const_cast<Context*>(context_)->Diagnostic();
    auto leftType = left.ResultType->WithoutQualifiers()->As<BasicType>();

	if(left.ResultType->WithoutQualifiers()->IsSigned()) {
		__int64 mask = context_->Target()->GetSignMask(leftType->GetKind());
		bool sameSigns = ((left.Value.IntValue ^ result) & mask) == 0;

		if(sameSigns == false) {
			// There was signed overflow.
			if(warn_) {
                diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
            }

			return true;
		}
	}
	else if((unsigned __int64)result < (unsigned __int64)left.Value.IntValue) {
		// There was unsigned overflow.
		if(warn_) {
            diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
        }

		return true;
	}

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::EvaluateMultiplicativeOp(const BinaryOperator* expr) {
	// Evaluate the left and right subexpressions.
	expr->LeftValue()->Accept(this);
	EvaluationResult left = result_;
	if(invalid_) return;

	expr->RightValue()->Accept(this);
	EvaluationResult right = result_;
	if(invalid_) return;

	// The operands should not be references to variables.
	if(left.HasVariable || right.HasVariable) {
		invalid_ = true;
		return;
	}

	auto target = context_->Target();
	auto diag = const_cast<Context*>(context_)->Diagnostic();
	bool hasOverflow = false;

	if(left.ResultType->WithoutQualifiers()->IsInteger() == false) {
		// Both operands are floating-point values. 
		// Only multiplication and division can appear here.
		ValueUnion value;

		// Don't do it if one of the operands is NaN or Infinity.
		if(Abstraction::FloatInfo::IsNaN(left.Value.FloatValue) ||
		   Abstraction::FloatInfo::IsNaN(right.Value.FloatValue) ||
		   Abstraction::FloatInfo::IsInfinity(left.Value.FloatValue) ||
		   Abstraction::FloatInfo::IsInfinity(right.Value.FloatValue)) {
			invalid_ = true;
			return;
		}

		if(expr->Operator() == BinaryOpType::Mul) {
			value = ValueUnion(left.Value.FloatValue * right.Value.FloatValue);
		}
		else {
			// Give up if we must divide by 0.
			if(right.Value.FloatValue == 0.0) {
				if(warn_) diag.Report(Warning::DIVISION_BY_ZERO)<<expr->Location();
				invalid_ = true;
				return;
			}

			value = ValueUnion(left.Value.FloatValue / right.Value.FloatValue);
		}

		return;
	}
	
	// Both operands are integers.
	auto leftType  = left.ResultType->WithoutQualifiers()->As<BasicType>();
	auto rightType = left.ResultType->WithoutQualifiers()->As<BasicType>();

	if(expr->Operator() == BinaryOpType::Mul) {
		ValueUnion value = ValueUnion(left.Value.IntValue * right.Value.IntValue);
		result_ = EvaluationResult(value, left.ResultType);

		// We're done if we don't need to check for overflow.
		if((warn_ == false) && (expectICE_ == false)) {
            return;
        }

		// For both signed and unsigned, there wasn't overflow if by dividing the result
		// by one of the operands we obtain the other operand.
		// For signed we need to check for: 0x8000 / 0xFFFF = 0x8000 (ex. for 16-bit).
		__int64 resultValue = target->Truncate(value.IntValue, leftType->GetKind());
        hasOverflow = MultiplicationOveflowed(expr, resultValue, left, right);
		return;
	}

	// Either division or remainder, check for division by zero first.
	if(right.Value.IntValue == 0) {
		if(warn_) diag.Report(Warning::DIVISION_BY_ZERO)<<expr->Location();
		invalid_ = true;
		return;
	}

	if(expr->Operator() == BinaryOpType::Div) {
		ValueUnion value;
		
		// Division doesn't behave the same for signed and unsigned numbers.
		// For signed, a special case is when we divide by -1, because
		// -MAX / -1 = -MAX, not MAX as it should be.
		if(left.ResultType->WithoutQualifiers()->IsSigned()) {
			if(right.Value.IntValue == -1) {
				value = ValueUnion(-left.Value.IntValue);
				unsigned __int64 max = target->GetSignMask(leftType->GetKind());

				if((warn_ || expectICE_) && (left.Value.IntValue == max)) {
					// There was signed overflow.
					if(warn_) diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)
                                          <<expr->Location();
					hasOverflow = true;
				}
			}
			else value = ValueUnion(left.Value.IntValue / right.Value.IntValue);
		}
		else value = ValueUnion((__int64)((unsigned __int64)left.Value.IntValue / 
										  (unsigned __int64)right.Value.IntValue));
		result_ = EvaluationResult(value, left.ResultType);
	}
	else {
		ValueUnion value;
		
		// Remainder doesn't behave the same for signed and unsigned numbers.
		// For signed, a special case is when the right operand is -1, 
		// because -MAX % -1 = 0. We don't warn in this case.
		if(left.ResultType->WithoutQualifiers()->IsSigned()) {
			if(right.Value.IntValue == -1) {
				value = ValueUnion(0LL);
			}
			else value = ValueUnion(left.Value.IntValue % right.Value.IntValue);
		}
		else value = ValueUnion((__int64)((unsigned __int64)left.Value.IntValue % 
									      (unsigned __int64)right.Value.IntValue));
		result_ = EvaluationResult(value, left.ResultType);
	}

	// If we're expecting an ICE and there was overflow give up.
	if(expectICE_ && hasOverflow) {
		invalid_ = true;
		return;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::MultiplicationOveflowed(const BinaryOperator* expr, __int64 result, 
                                                EvaluationResult& left, 
                                                EvaluationResult& right) {
    // For both signed and unsigned, there wasn't overflow if by dividing the result
	// by one of the operands we obtain the other operand.
	// For signed we need to check for: 0x8000 / 0xFFFF = 0x8000 (ex. for 16-bit).
    auto diag = const_cast<Context*>(context_)->Diagnostic();
    auto target = context_->Target();
    auto leftType = left.ResultType->WithoutQualifiers()->As<BasicType>();

	if(leftType->IsSigned()) {
		bool overflow = (result / right.Value.IntValue) != left.Value.IntValue;

		if(overflow || ((result == target->GetSignMask(leftType->GetKind())) &&
		   (left.Value.IntValue == target->GetSignMask(leftType->GetKind())) &&
		   (right.Value.IntValue == target->GetMaxValue(leftType->GetKind())))) {
			// There was signed overflow.
			if(warn_) {
                diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
            }

			return true;
		}
	}
	else if(((unsigned __int64)result / (unsigned __int64)right.Value.IntValue) !=
			 (unsigned __int64)left.Value.IntValue) {
		// There was signed overflow.
		if(warn_) {
            diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
        }

		return true;
	}

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::EvaluateComparisonOp(const BinaryOperator* expr) {
	// Evaluate the left and right subexpressions.
	expr->LeftValue()->Accept(this);
	EvaluationResult left = result_;
	if(invalid_) return;

	expr->RightValue()->Accept(this);
	EvaluationResult right = result_;
	if(invalid_) return;

	// The operands should not be references to variables.
	if(left.HasVariable || right.HasVariable) {
		invalid_ = true;
		return;
	}

	// Note that comparisons involving pointers have been already rejected,
	// so we're left with only comparisons of integer or floating types.
	// The operands can be either integer/integer or floating/floating, but not
	// integer/floating because the usual binary conversions have been applied
	// and both operands have been converted to a common type.
	auto commonType = expr->RightValue()->ResultType()->WithoutQualifiers();
	ValueUnion value;

	if(commonType->IsFloating()) {
		// The floating/floating case.
		double leftValue = left.Value.FloatValue;
		double rightValue = right.Value.FloatValue;
		double result;

		switch(expr->Operator()) {
		case BinaryOpType::EqEq:      { result = (leftValue == rightValue) ? 1 : 0; break; }
		case BinaryOpType::NotEq:     { result = (leftValue != rightValue) ? 1 : 0; break; }
		case BinaryOpType::Less:      { result = (leftValue < rightValue)  ? 1 : 0; break; }
		case BinaryOpType::LessEq:    { result = (leftValue <= rightValue) ? 1 : 0; break; }
		case BinaryOpType::Greater:   { result = (leftValue > rightValue)  ? 1 : 0; break; }
		case BinaryOpType::GreaterEq: { result = (leftValue >= rightValue) ? 1 : 0; break; }
		}

		value.FloatValue = result;
	}
	else if(commonType->IsUnsigned()) {
		// The integer/integer case, with unsigned numbers.
		unsigned __int64 leftValue = left.Value.IntValue;
		unsigned __int64 rightValue = right.Value.IntValue;
		unsigned __int64 result;

		switch(expr->Operator()) {
		case BinaryOpType::EqEq:      { result = (leftValue == rightValue) ? 1 : 0; break; }
		case BinaryOpType::NotEq:     { result = (leftValue != rightValue) ? 1 : 0; break; }
		case BinaryOpType::Less:      { result = (leftValue < rightValue)  ? 1 : 0; break; }
		case BinaryOpType::LessEq:    { result = (leftValue <= rightValue) ? 1 : 0; break; }
		case BinaryOpType::Greater:   { result = (leftValue > rightValue)  ? 1 : 0; break; }
		case BinaryOpType::GreaterEq: { result = (leftValue >= rightValue) ? 1 : 0; break; }
		}

		value.IntValue = (__int64)result;
	}
	else {
		// The integer/integer case, with signed numbers.
		__int64 leftValue = left.Value.IntValue;
		__int64 rightValue = right.Value.IntValue;
		__int64 result;

		switch(expr->Operator()) {
		case BinaryOpType::EqEq:      { result = (leftValue == rightValue) ? 1 : 0; break; }
		case BinaryOpType::NotEq:     { result = (leftValue != rightValue) ? 1 : 0; break; }
		case BinaryOpType::Less:      { result = (leftValue < rightValue)  ? 1 : 0; break; }
		case BinaryOpType::LessEq:    { result = (leftValue <= rightValue) ? 1 : 0; break; }
		case BinaryOpType::Greater:   { result = (leftValue > rightValue)  ? 1 : 0; break; }
		case BinaryOpType::GreaterEq: { result = (leftValue >= rightValue) ? 1 : 0; break; }
		}

		value.IntValue = result;
	}

	result_ = EvaluationResult(value, expr->ResultType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::EvaluateLogicalOp(const BinaryOperator* expr) {
	// Note that we must handle the logical operators in short-circuit fashion.
	expr->LeftValue()->Accept(this);
	EvaluationResult left = result_;
	if(invalid_) return;

	// The operands should not be references to variables.
	if(left.HasVariable) {
		invalid_ = true;
		return;
	}

	// We're done if the result is 'true' and the operator is ||, 
	// or the result is 'false' and the operator is &&.
	__int64 leftValue = left.Value.IntValue;

	if((leftValue == 0) && (expr->Operator() == BinaryOpType::AndAnd)) {
		result_ = EvaluationResult(ValueUnion(0LL), BasicType::GetInt());
		return;
	}
	else if((leftValue != 0) && (expr->Operator() == BinaryOpType::OrOr)) {
		result_ = EvaluationResult(ValueUnion(1ll), BasicType::GetInt());
		return;
	}

	// Evaluate the right part and combine the results.
	expr->RightValue()->Accept(this);
	EvaluationResult right = result_;
	if(invalid_) return;

	// The operands should not be references to variables.
	if(left.HasVariable) {
		invalid_ = true;
		return;
	}

	// 'true' takes the value 1, 'false' 0.
	__int64 rightValue = right.Value.IntValue;
	__int64 resultValue;
	
	if(expr->Operator() == BinaryOpType::AndAnd) {
		resultValue = ((leftValue != 0) && (rightValue != 0)) ? 1 : 0;
	}
	else {
		resultValue = ((leftValue != 0) || (rightValue != 0)) ? 1 : 0;
	}

	result_ = EvaluationResult(resultValue, BasicType::GetInt());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const SizeofOperator* expr) {
	// We don't need to evaluate the expression 'sizeof' targets.
	// Doing so could discover expressions that are not constant, but the standard
	// specifies that they are allowed inside 'sizeof'.
	const Type* type = expr->Target()->ResultType()->WithoutQualifiers();

	// The target type should not depend on a VLA.
	if(type->IsVariable()) {
		invalid_ = true;
		return;
	}

	// Use 'size_t' as the result type.
	__int64 size = TypeSize(type, context_).Size();
	result_ = EvaluationResult(ValueUnion(size), BasicType::GetSizeT());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const ConditionalOperator* expr) {
	// Only one side of the conditional operator is evaluated, depending on the
	// result of the condition expression.
	expr->Condition()->Accept(this);
	EvaluationResult condition = result_;

	if(invalid_) {
		MarkConstantExpression(expr);
		return;
	}

	// The operands should not be references to variables.
	if(condition.HasVariable) {
		invalid_ = true;
		return;
	}

	if(condition.IsInt) {
		if(condition.Value.IntValue == 0) {
			expr->Right()->Accept(this);
		}
		else expr->Left()->Accept(this);
	}
	else {
		if(condition.Value.FloatValue == 0.0) {
			expr->Right()->Accept(this);
		}
		else expr->Left()->Accept(this);
	}

	MarkConstantExpression(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const CastExpression* expr) {
	// We care only about conversions involving integer, floating and pointer types.
	auto targetType = expr->Target()->ResultType()->WithoutQualifiers();
	auto castType = expr->ResultType()->WithoutQualifiers();

	switch(expr->Type()) {
		case CastType::IntToFloat:   { CastIntToFloat(expr);   return; }
		case CastType::FloatToInt:   { CastFloatToInt(expr);   return; }
		case CastType::IntToInt:     { CastIntToInt(expr);     return; }
		case CastType::FloatToFloat: { CastFloatToFloat(expr); return; }
		case CastType::Unknown: {
			if(targetType->IsInteger() || targetType->IsEnum()) {
				// Integers can be converted to floating, integer and pointer types.
				// Note that converting to pointer type involves no changes.
				if(castType->IsInteger() || castType->IsEnum()) {
					CastIntToInt(expr);
					return;
				}
				else if(castType->IsFloating()) {
					CastIntToFloat(expr);
					return;
				}
			}
			else if(targetType->IsFloating()) {
				if(castType->IsFloating()) {
					CastFloatToFloat(expr);
					return;
				}
				else {
					CastFloatToInt(expr);
					return;
				}
			}
			else {
				// This is an invalid case.
				invalid_ = true;
				return;
			}

			break;
		}
	}

	// All other cases don't need any conversion.
	expr->Target()->Accept(this);
	if(invalid_) {
		MarkConstantExpression(expr);
		return;
	}

	result_.ResultType = castType;
	result_.IsInt = castType->IsInteger();
	MarkConstantExpression(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::CastIntToFloat(const CastExpression* expr) {
	expr->Target()->Accept(this);
	if(invalid_) {
		MarkConstantExpression(expr);
		return;
	}

	auto castType = expr->ResultType()->WithoutQualifiers()->As<BasicType>();
	ValueUnion value;

	if(castType->IsFloat()) {
		value = ValueUnion((double)((float)result_.Value.IntValue));
	}
	else value = ValueUnion((double)(result_.Value.IntValue));

	result_ = EvaluationResult(value, castType, false /* isInt */, 
							   result_.HasVariable, result_.BaseVariable);
	MarkConstantExpression(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::CastIntToInt(const CastExpression* expr) {
	expr->Target()->Accept(this);
	if(invalid_) {
		MarkConstantExpression(expr);
		return;
	}

	// We check for overflow when truncating. Overflow occurred if the value,
	// after truncation, is smaller than the original one.
	auto targetType = expr->Target()->ResultType()->WithoutQualifiers()->AsIntegerType();
	auto castType = expr->ResultType()->WithoutQualifiers()->AsIntegerType();
	bool isTrunct = castType->RankBelow(targetType);

	auto target = context_->Target();
	__int64 prevValue = result_.Value.IntValue;
	ValueUnion value = ValueUnion(target->Truncate(prevValue, castType->GetKind()));

	// Warn about overflow if it's the case.
	if(isTrunct && (value.IntValue < prevValue)) {
		// There was overflow.
		if(warn_) {
			auto diag = const_cast<Context*>(context_)->Diagnostic();
			diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
		}

		// If we're expecting an ICE and there was overflow give up.
		if(expectICE_) {
			invalid_ = true;
			MarkConstantExpression(expr);
			return;
		}
	}

	result_ = EvaluationResult(value, castType, true /* isInt */, 
                         result_.HasVariable, result_.BaseVariable);
	MarkConstantExpression(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::CastFloatToFloat(const CastExpression* expr) {
	expr->Target()->Accept(this);
	if(invalid_) {
		MarkConstantExpression(expr);
		return;
	}

	auto castType = expr->ResultType()->WithoutQualifiers()->As<BasicType>();
	ValueUnion value;

	if(castType->IsFloat()) {
		value = ValueUnion((double)((float)result_.Value.FloatValue));
	}
	else value = ValueUnion(result_.Value.FloatValue);
	
	result_ = EvaluationResult(value, castType, false /* isInt */, 
                         result_.HasVariable, result_.BaseVariable);
	MarkConstantExpression(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::CastFloatToInt(const CastExpression* expr) {
	expr->Target()->Accept(this);
	if(invalid_) {
		MarkConstantExpression(expr);
		return;
	}

	// We warn about overflow in this case. Overflow will occur if the value
	// to be converted is larger than the maximum representable value using
	// the resulting type.
	auto targetType = expr->Target()->ResultType()->WithoutQualifiers()->As<BasicType>();
	auto castType = expr->ResultType()->WithoutQualifiers()->AsIntegerType();
	auto target = context_->Target();
	__int64 maxValue = target->GetMaxValue(castType->GetKind());

	// If the number is signed reset the highest bit ('GetMaxValue' returns
	// a value adequate for unsigned integers only).
	if(castType->IsSigned()) {
		maxValue &= ~(1 << target->SizeInBits(castType->GetKind()));
	}

	if(result_.Value.FloatValue > maxValue) {
		// There was overflow.
		if(warn_) {
			auto diag = const_cast<Context*>(context_)->Diagnostic();
			diag.Report(Warning::INTEGER_CONSTANT_OVERFLOW)<<expr->Location();
		}

		// If we're expecting an ICE and there was overflow give up.
		if(expectICE_) {
			invalid_ = true;
			MarkConstantExpression(expr);
			return;
		}
	}

	// Convert to integer (even if it was overflow).
	ValueUnion value = target->Truncate((__int64)result_.Value.FloatValue,
										castType->GetKind());
	result_ = EvaluationResult(value, castType, true /* isInt */, 
                         result_.HasVariable, result_.BaseVariable);
	MarkConstantExpression(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::IsVariableDeclaration(const Expression* expr) {
	// Skip all casts first, then check that it's a reference
	// to a variable declaration declared at file scope.
	if(auto declExpr = expr->WithoutCasts()->As<DeclarationExpression>()) {
		if(auto variableDecl = declExpr->Object()->As<VariableDeclaration>()) {
			return variableDecl->IsGlobal();
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::IsIntegerBase(const Expression* expr) {
	// Skip all implicit casts first, then check that it's a number constant
	// with integer type or an enumeration constant.
	const Expression* exprNoCasts = expr->WithoutCasts();

	if(auto number = exprNoCasts->As<NumberConstant>()) {
		return number->IsInteger();
	}
	else if(auto declExpr = exprNoCasts->As<DeclarationExpression>()) {
		return declExpr->Object()->IsEnumConstDecl();
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::IsStringBase(const Expression* expr) {
	// Skip all implicit casts first, then check that it's a string constant.
	const Expression* exprNoCasts = expr->WithoutCasts();
	return exprNoCasts->IsStringConst();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const UnaryOperator* expr) {
	// The address & operator is a special case.
	if(expr->Operator() == UnaryOpType::Address) {
		// It's valid as long as the target is a subscript expression, a struct/union
		// member-access expression, a global declaration or a compound literal.
		// The result is the offset of the component, or 0 for the declaration case.
		const Expression* target = expr->Value();
        bool mayBeOffsetOf = false;

		if(auto castExpr = target->As<CastExpression>()) {
			// It's common to have code like '(char*)&((struct ABC*)0)->field - (char*)0'. 
			if(castExpr->ResultType()->WithoutQualifiers()->IsPointer()) {
				target = castExpr->Target();
			}
		}

		if(auto subscript = target->As<SubscriptExpression>()) {
			// The base of the subscript must be a reference to
			// a global variable declaration of an array (ex. '&a[3]') or a string.
			invalid_ = (IsVariableDeclaration(subscript->Base()) ||
					    IsIntegerBase(subscript->Base()) ||
					    IsStringBase(subscript->Base())) == false;
		}
		else if(auto member = target->As<MemberExpression>()) {
			// The object expression should be a variable declaration
			// of a struct/union object (something like '&a.member' or '&a->member').
			invalid_ = (IsVariableDeclaration(member->Object()) == false ||
					    IsIntegerBase(member->Object()) ||
					    IsStringBase(member->Object())) == false;
            mayBeOffsetOf = member->Object()->IsNullPointer(context_, 
                                                            true /* relaxed */);
		}
		else if(auto compound = target->As<CompoundExpression>()) {
			invalid_ = false;
		}
		else invalid_ = IsVariableDeclaration(expr->Value()) == false;

		if(invalid_ == false) {
			// This is a valid situation, compute the offset.
			expr->Value()->Accept(this);
			if(invalid_) return;

			result_ = EvaluationResult(ValueUnion(result_.Value.IntValue), 
                                 expr->ResultType(), true /* isInt */, 
                                 mayBeOffsetOf == false /* hasVariable */, 
                                 result_.BaseVariable);
			MarkConstantExpression(expr);
			return;
		}

		MarkConstantExpression(expr);
		return;
	}

	// First evaluate the target.
	expr->Value()->Accept(this);
	if(invalid_) {
		MarkConstantExpression(expr);
		return;
	}

	// All the operators expect to work on constants, not variables
	// (something like 'int a = -b;' is invalid).
	if(result_.HasVariable) {
		invalid_ = true;
		hasSideffects_ = true;
		MarkConstantExpression(expr);
		return;
	}

	switch(expr->Operator()) {
		case UnaryOpType::Inc:
		case UnaryOpType::Dec: { 
			// Increment and decrement operators are not allowed in constant expressions.
			invalid_ = true;
			hasSideffects_ = true; // We presume it has.
			MarkConstantExpression(expr);
			return;
		}
		case UnaryOpType::Indirection: {
			// Not allowed because it accesses the value.
			invalid_ = true;
			hasSideffects_ = true;
			MarkConstantExpression(expr);
			break; 
		}
		case UnaryOpType::Add: {
			// The result isn't changed.
			MarkConstantExpression(expr);
			break;
		}
		case UnaryOpType::Sub: {
			// The number is negated.
			if(result_.IsInt) {
				result_.Value.IntValue = -result_.Value.IntValue;
				MarkConstantExpression(expr);
			}
			else {
				// If we're expecting an ICE floating types are not allowed here.
				if(expectICE_) {
					invalid_ = true;
					MarkConstantExpression(expr);
					return;
				}

				result_.Value.FloatValue = -result_.Value.FloatValue;
				MarkConstantExpression(expr);
			}

			break; 
		}
		case UnaryOpType::Complement: {
			// The binary complements is computed.
			result_.Value.IntValue = ~result_.Value.IntValue;
			MarkConstantExpression(expr);
			break;
		}
		case UnaryOpType::Not: {
			// If the number is 0 the result is 1 (true). Else it's 0 (false).
			// The result type is an integer.
			if(result_.IsInt) {
				result_.Value.IntValue = result_.Value.IntValue == 0 ? 1 : 0;
				MarkConstantExpression(expr);
			}
			else {
				// If we're expecting an ICE floating types are not allowed here.
				if(expectICE_) {
					invalid_ = true;
					MarkConstantExpression(expr);
					return;
				}

				result_.Value.FloatValue = result_.Value.FloatValue == 0.0 ? 1.0 : 0.0;
				MarkConstantExpression(expr);
			}

			// If the target was an integer use it's type.
			result_.IsInt = true;
			result_.ResultType = result_.IsInt ? result_.ResultType : 
                                                 BasicType::GetInt();
			MarkConstantExpression(expr);
			break; 
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const CompoundExpression* expr) {
	// A compound expression is constant if all the expression in the associated
	// initializer list are constant.
	if(expectICE_) {
		invalid_ = true;
		return;
	}

	auto& initList = expr->InitList()->InitList();

	for(int i = 0; i < initList.Count(); i++) {
		initList[i]->Accept(this);
		if(invalid_) break;
	}

	result_ = EvaluationResult(ValueUnion(0LL), expr->ResultType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ConstantEvaluator::Visit(const InvalidExpression* expr) {
	invalid_ = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantEvaluator::GetResult(EvaluationResult& result, bool& hasSideffects, 
								  bool& hasVariable) {
	if(invalid_) {
		hasSideffects = hasSideffects_;
		return false;
	}

	result = result_;
	hasSideffects = false;
	hasVariable = result_.HasVariable;
	return true;
}

} // namespace AST