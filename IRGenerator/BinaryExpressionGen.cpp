// ExpressionGenerator.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

void ExpressionGenerator::Visit(const BinaryOperator* op) {
	// We can have simple cases, like 'integer + integer'
    // or 'floating + floating', or more complex ones, involving pointers.
	// First we try to evaluate the expression as a constant.
	if(GenerateConstant(op)) {
        return;
    }

	if(op->IsAssignment()) {
		switch(op->Operator()) {
			case BinaryOpType::Eq: {
				GenerateAssignment(op);
				break;
			}
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
				GenerateCompoundAssignment(op);
				break;
			}
		}
	}
	else if(op->IsLogical()) {
		GenerateLogical(op);
	}
	else {
		// For all other operators.
		IR::Operand* leftOp;
		IR::Operand* rightOp;

		// Generate the left and right subexpressions.
		auto leftValue = op->LeftValue();
		auto rightValue = op->RightValue();
		const Type* newLeftType = nullptr;
		const Type* newRightType = nullptr;

		if(op->IsLogical() || op->IsRelational() || op->IsEquality()) {
			// Try to eliminate some unnecessary casts that involve
			// the logical and comparison operators.
			SimplifyBinaryOpCasts(leftValue, rightValue, 
                                  &newLeftType, &newRightType);
		}

		// Generate code for the left subexpression.
		ResetLvalue(); // Load the value.
        leftOp = GenerateExpression(leftValue);

        // Patch the left operand, if needed.
		if(newLeftType) {
            leftOp = PatchOperandType(leftOp, GetIRType(newLeftType));
        }

		// Generate code for the right subexpression.
		ResetLvalue(); // Load the value.
		rightOp = GenerateExpression(rightValue);

        // Patch the right operand, if needed.
		if(newRightType) {
            rightOp = PatchOperandType(rightOp, GetIRType(newRightType));
        }
        
		// Now generate the appropriate operation.
		GenerateBinaryOp(op, leftOp, rightOp, 
                         op->ResultType()->WithoutQualifiers());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateBinaryOp(const BinaryOperator* op, IR::Operand* leftOp,
										   IR::Operand* rightOp, const Type* resultType) {
	// Dispatch to the methods that handle specialized cases.
	switch(op->Operator()) {
		case BinaryOpType::Add:
		case BinaryOpType::Sub: {
			bool onPointers = op->LeftValue()->ResultType()->WithoutQualifiers()->IsPointer() ||
							  op->RightValue()->ResultType()->WithoutQualifiers()->IsPointer();
			// The operands can be any scalar values.
			if(onPointers == false) {
				GenerateAdditiveOp(op, leftOp, rightOp, resultType);
			}
			else GeneratePointerAdditiveOp(op, leftOp, rightOp, resultType);

			break;
		}
		case BinaryOpType::Mul:
		case BinaryOpType::Div:
		case BinaryOpType::Mod: {
			GenerateMultiplicativeOp(op, leftOp, rightOp, resultType);
			break;
		}
		case BinaryOpType::EqEq: 
		case BinaryOpType::NotEq:
		case BinaryOpType::Less: 
		case BinaryOpType::LessEq: 
		case BinaryOpType::Greater:
		case BinaryOpType::GreaterEq: {
			GenerateComparisonOp(op, leftOp, rightOp, resultType);
			break;
		}
		case BinaryOpType::AndAnd:
		case BinaryOpType::OrOr: {
			GenerateLogical(op);
			break;
		}
		case BinaryOpType::And: 
		case BinaryOpType::Or: 
		case BinaryOpType::Xor: 
		case BinaryOpType::ShiftR: 
		case BinaryOpType::ShiftL: {
			GenerateBitwiseOp(op, leftOp, rightOp, resultType);
			break;
		}
		case BinaryOpType::Comma: {
			// For comma nothing needs to be done, because the left and right
			// subexpressions have already been generated, and the value of
			// the right one is in 'result_', as imposed by the standard.
			break;
		}
		default: DebugValidator::Unreachable();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Instruction* ExpressionGenerator::SetOverflowFlag(IR::Instruction* instr) {
	DebugValidator::IsTrue(instr->IsArithmetic());
	DebugValidator::IsFalse(instr->IsFloatArithmetic());

	if(functGen_->GetContext()->Options().IsOverflowUndefined()) {
		instr->SetHasUndefinedOverflow(true);
	}

	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Instruction* ExpressionGenerator::SetFloatingMode(IR::Instruction* instr) {
	//! TODO: this should look for float attribute attached to the function
	DebugValidator::IsTrue(instr->IsFloatArithmetic());
	auto arithInstr = instr->As<IR::ArithmeticInstr>();

	switch(functGen_->GetContext()->Options().GetFloatingPointMode()) {
		case FPMode_Exact: {
			arithInstr->SetFPMode(IR::FloatMode::Exact);
			break;
		}
		case FPMode_Safe: {
			arithInstr->SetFPMode(IR::FloatMode::Safe);
			break;
		}
		case FPMode_Fast: {
			arithInstr->SetFPMode(IR::FloatMode::Fast);
			break;
		}
	}

	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateAdditiveOp(const BinaryOperator* op, IR::Operand* leftOp,
											 IR::Operand* rightOp, const Type* resultType) {
	// We can have 'integer OP integer' or 'floating OP floating'.
	// Note that if we arrived here we know for sure the operands are not pointers.
	auto irResultType = GetIRType(resultType);
	result_ = irGen_->GetTemporary(irResultType);

	if(irResultType->IsInteger()) {
		if((op->Operator() == BinaryOpType::Add) || 
           (op->Operator() == BinaryOpType::AddEq)) {
			SetOverflowFlag(irGen_->GetAdd(leftOp, rightOp, 
                                           result_, activeBlock_));
		}
		else SetOverflowFlag(irGen_->GetSub(leftOp, rightOp, 
                                            result_, activeBlock_));
	}
	else {
		// Floating operands.
		if((op->Operator() == BinaryOpType::Add) || 
           (op->Operator() == BinaryOpType::AddEq)) {
			SetFloatingMode(irGen_->GetFadd(leftOp, rightOp, 
                                            result_, activeBlock_));
		}
		else SetFloatingMode(irGen_->GetFsub(leftOp, rightOp, 
                                             result_, activeBlock_));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GeneratePointerAdditiveOp(const BinaryOperator* op, 
													IR::Operand* leftOp,
													IR::Operand* rightOp, 
													const Type* resultType) {
	// Based on the type of the operands we generate the following code:
	// 'pointer + integer' - addr ptr, value   (the same for 'integer + pointer')
	// 'pointer - integer' - addr ptr, -value
	// 'pointer - pointer' - t1 = ptoi p1, int64     t2 = ptoi p2, int64
	//                       t3 = sub t1, t2         t4 = div t3, sizeof(pointee)
	// Note that 'integer - pointer' and 'pointer + pointer' are not valid.
	auto leftType = op->LeftValue()->ResultType()->WithoutQualifiers();
	auto rightType = op->RightValue()->ResultType()->WithoutQualifiers();
	const Type* ptrType1 = nullptr;
	const Type* ptrType2 = nullptr;
	const Type* intType  = nullptr;

	// Figure out the case that must be handled.
	if(leftType->IsPointer()) {
		ptrType1 = leftType;
	}
	else intType = leftType;

	if(rightType->IsPointer()) {
		// Even if it's a pointer type, it can be part of a compound assignment
		// that promoted the 'int' to 'pointer'. In this case we want it to be
		// considered an 'int', else invalid code will be generated.
		if(IsImplicitIntToPtrCast(op->RightValue(), &intType)) {
            // Nothing else to do here.
        }
		else if(ptrType1) {
            ptrType2 = rightType;
        }
		else ptrType1 = rightType;
	}
	else intType = rightType;

	if((op->Operator() == BinaryOpType::Add) || 
       (op->Operator() == BinaryOpType::AddEq)) {
		// 'pointer + integer' or 'integer + pointer' case.
		GeneratePointerIntAdd(op, leftOp, rightOp, ptrType1, 
                              intType, resultType);
	}
	else if(intType) {
		// 'pointer - integer' case.
		GeneratePointerIntSub(op, leftOp, rightOp, ptrType1, 
                              intType, resultType);
	}
	else {
		// 'pointer - pointer' case.
		GeneratePointerPtrSub(op, leftOp, rightOp, ptrType1, 
                              ptrType2, resultType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GeneratePointerIntAdd(const BinaryOperator* op, 
                                                IR::Operand* leftOp,
												IR::Operand* rightOp, 
                                                const Type* pointerType,
												const Type* intType, 
												const Type* resultType) {
	DebugValidator::IsNotNull(pointerType);
	DebugValidator::IsNotNull(intType);
	
	// It's possible that we have a pointer to a VLA.
    auto pointee = pointerType->As<PointerType>()->PointeeType();

	if(auto arrayType = pointee->As<ArrayType>()) {
		if(arrayType->IsVariable()) {
			GenerateVLAPointerIntAdd(op, leftOp, rightOp, pointerType, 
									 intType, resultType, arrayType);
			return;
		}
	}

	// This is an usual operation.
	auto leftType = op->LeftValue()->ResultType()->WithoutQualifiers();
	auto irResultType = GetIRType(resultType);
	result_ = irGen_->GetTemporary(irResultType);

	if(intType == leftType) {
		// The adjustment value is in 'leftOp'.
		irGen_->GetAddress(rightOp, leftOp, result_, activeBlock_);
	}
	else irGen_->GetAddress(leftOp, rightOp, result_, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GeneratePointerIntSub(const BinaryOperator* op, 
                                                IR::Operand* leftOp,
                                                IR::Operand* rightOp, 
                                                const Type* pointerType,
												const Type* intType, 
												const Type* resultType) {
	DebugValidator::IsNotNull(pointerType);
	DebugValidator::IsNotNull(intType);
	
	// It's possible that we have a pointer to a VLA.
    auto pointee = pointerType->As<PointerType>()->PointeeType();

	if(auto arrayType = pointee->As<ArrayType>()) {
		if(arrayType->IsVariable()) {
			GenerateVLAPointerIntSub(op, leftOp, rightOp, pointerType, 
									 intType, resultType, arrayType);
			return;
		}
	}

	// The value in the integer operand needs to be negated.
	auto irIntType = GetIRType(intType);
	IR::Operand* negatedValue;

	// If the operand is a constant integer we negate the value directly,
	// instead of generating a 'negValue = sub 0, rightOp' instruction.
	if(auto constIntOp = rightOp->As<IR::IntConstant>()) {
		negatedValue = irGen_->GetIntConst(constIntOp->GetType(), 
                                           -constIntOp->Value());
	}
	else {
		// Generate 'negValue = sub 0, rightOp'.
		negatedValue = irGen_->GetTemporary(irIntType);
		auto zeroConst = irGen_->GetIntConst(irIntType, 0);
		SetOverflowFlag(irGen_->GetSub(zeroConst, rightOp, 
                                       negatedValue, activeBlock_));
	}

	// Generate the address of the element.
	auto irResultType = GetIRType(resultType);
	result_ = irGen_->GetTemporary(irResultType);
	irGen_->GetAddress(leftOp, negatedValue, result_, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GeneratePointerPtrSub(const BinaryOperator* op, 
                                                IR::Operand* leftOp, 
                                                IR::Operand* rightOp, 
                                                const Type* ptrType1,
												const Type* ptrType2, 
												const Type* resultType) {
	DebugValidator::IsNotNull(ptrType1);
	DebugValidator::IsNotNull(ptrType2);
	
	int ptrSize = functGen_->GetTarget()->GetPointerSize();
	auto ptrIntType = TypeGenerator::GetTypeFromSize(ptrSize)->As<IR::IntegerType>();
	auto ptoi1 = irGen_->GetTemporary(ptrIntType);
	auto ptoi2 = irGen_->GetTemporary(ptrIntType);

	irGen_->GetPtoi(leftOp, ptrIntType, ptoi1, activeBlock_);
	irGen_->GetPtoi(rightOp, ptrIntType, ptoi2, activeBlock_);

	// Now do the subtraction.
	auto subOp = irGen_->GetTemporary(ptrIntType);
	SetOverflowFlag(irGen_->GetSub(ptoi1, ptoi2, subOp, activeBlock_));

	// Divide by the size of the pointed object. If it's a VLA we need to
	// query the computed size, but no other difference is there.
	IR::Operand* sizeOp = nullptr;
	auto pointeeType = ptrType1->As<PointerType>()->PointeeType();

	if(auto arrayType = pointeeType->As<ArrayType>()) {
		if(arrayType->IsVariable()) {
			// Get the size of the VLA element, and make sure it has the right type.
			sizeOp = functGen_->GetVLAElemSizeOperand(arrayType);

			if(auto varArrayType = arrayType->As<VarArrayType>()) {
				auto sizeExprType = varArrayType->SizeExpression()->ResultType();
				GenerateIntToIntCast(sizeOp, ptrIntType, sizeExprType);
			}
			else GenerateIntToIntCast(sizeOp, ptrIntType, BasicType::GetInt());
		}
	}

	if(sizeOp == nullptr) {
		// This is a standard pointer.
		__int64 pointeeSize = TypeSize(pointeeType, functGen_->GetContext()).Size();
		sizeOp = irGen_->GetIntConst(ptrIntType, pointeeSize);
	}
	
	// Divide the result of the subtraction by the size of the element.
	// The result will be the number of elements between the two pointers.
	result_ = irGen_->GetTemporary(ptrIntType);
    auto divInstr = irGen_->GetDiv(subOp, sizeOp, result_, activeBlock_);
	SetOverflowFlag(divInstr);

    // We know that the division will not generate any remainder, so we mark
    // the instruction with this fact. This improves simplifications in the backend.
    divInstr->SetHasNoRemainder(true);

	// Convert back to the pointer type if the result is a pointer.
	// Happens in cases like 'int *p, *q, *t; t = q - p;'.
	if(resultType->IsPointer()) {
		auto irResultType = GetIRType(resultType);
		auto divOp = result_;
		result_ = irGen_->GetTemporary(irResultType);
		irGen_->GetItop(divOp, irResultType, result_, activeBlock_);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::IsImplicitIntToPtrCast(const Expression* expr, 
												 const Type** targetType) {
	if(expr->ResultType()->WithoutQualifiers()->IsPointer() == false) {
		return false;
	}

	if(auto castExpr = expr->As<CastExpression>()) {
		auto type = castExpr->Target()->ResultType()->WithoutQualifiers();
		if(castExpr->IsImplicit() && type->IsInteger()) {
			if(targetType) *targetType = type;
			return true;
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateMultiplicativeOp(const BinaryOperator* op, 
												   IR::Operand* leftOp, 
												   IR::Operand* rightOp, 
												   const Type* resultType) {
	// We can have 'integer OP integer' or 'floating OP floating'.
	// Division and modulo for unsigned integers use separate instructions.
	resultType = resultType->WithoutQualifiers(); // Strip qualifiers.
	auto irResultType = GetIRType(resultType);
	bool isSigned = resultType->IsInteger() || resultType->IsChar();
	result_ = irGen_->GetTemporary(irResultType);

	if((op->Operator() == BinaryOpType::Mul) || 
       (op->Operator() == BinaryOpType::MulEq)) {
		if(irResultType->IsInteger()) {
			// The same instruction is used for signed and unsigned.
			SetOverflowFlag(irGen_->GetMul(leftOp, rightOp, 
                                           result_, activeBlock_));
		}
		else SetFloatingMode(irGen_->GetFmul(leftOp, rightOp, 
                                             result_, activeBlock_));
	}
	else if((op->Operator() == BinaryOpType::Div) || 
            (op->Operator() == BinaryOpType::DivEq)) {
		if(irResultType->IsInteger()) {
			// Select the instruction based on the presence of the sign.
			if(isSigned) SetOverflowFlag(irGen_->GetDiv(leftOp, rightOp, 
														result_, activeBlock_));
			else irGen_->GetUdiv(leftOp, rightOp, 
                                 result_, activeBlock_);
		}
		else SetFloatingMode(irGen_->GetFdiv(leftOp, rightOp, 
                                             result_, activeBlock_));
	}
	else {
		// Modulo can be applied only to integers.
		// Select the instruction based on the presence of the sign.
		if(isSigned) SetOverflowFlag(irGen_->GetMod(leftOp, rightOp, 
													result_, activeBlock_));
		else irGen_->GetUmod(leftOp, rightOp, 
                             result_, activeBlock_);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateBitwiseOp(const BinaryOperator* op, 
                                            IR::Operand* leftOp, 
                                            IR::Operand* rightOp, 
                                            const Type* resultType) {
	// Binary operations can be applied only on integers.
	// We don't care about the sign, except for right shift >>.
	auto irResultType = GetIRType(resultType);

	switch(op->Operator()) {
		case BinaryOpType::And: 
		case BinaryOpType::AndEq: {
			result_ = irGen_->GetTemporary(irResultType);
			irGen_->GetAnd(leftOp, rightOp, result_, activeBlock_);
			break;
		}
		case BinaryOpType::Or:
		case BinaryOpType::OrEq: {
			result_ = irGen_->GetTemporary(irResultType);
			irGen_->GetOr(leftOp, rightOp, result_, activeBlock_);
			break;
		}
		case BinaryOpType::Xor:
		case BinaryOpType::XorEq: {
			result_ = irGen_->GetTemporary(irResultType);
			irGen_->GetXor(leftOp, rightOp, result_, activeBlock_);
			break;
		}
		case BinaryOpType::ShiftR:
		case BinaryOpType::ShiftREq: {
			// The usual binary conversion is not performed on shift operators,
			// so we can have different types for the left and right parts.
			// For example, in 'long long a; a = a << 32;' '32' still has 'int' type.
			// We must convert the right operand so that it has the type of the left one.
			irResultType = GenerateCastToLeft(leftOp, op->LeftValue()->ResultType(),
											  rightOp, op->RightValue()->ResultType());
			result_ = irGen_->GetTemporary(irResultType);

			// For signed integers we need to use arithmetic right shift.
			bool isSigned = op->ResultType()->WithoutQualifiers()->IsSigned();
			
			if(isSigned) irGen_->GetShr(leftOp, rightOp, 
                                        result_, activeBlock_);
			else irGen_->GetUshr(leftOp, rightOp, 
                                 result_, activeBlock_);
			break;
		}
		case BinaryOpType::ShiftL:
		case BinaryOpType::ShiftLEq: {
			// The same as for right shift above.
			irResultType = GenerateCastToLeft(leftOp, op->LeftValue()->ResultType(),
											  rightOp, op->RightValue()->ResultType());
			result_ = irGen_->GetTemporary(irResultType);

			// Shifting left is the same for both signed and unsigned integers.
			irGen_->GetShl(leftOp, rightOp, result_, activeBlock_);
			break;
		}
		default: DebugValidator::Unreachable();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IR::Type*
ExpressionGenerator::GenerateCastToLeft(IR::Operand*& leftOp, const Type* oldLeftType,
										IR::Operand*& rightOp, const Type* oldRightType) {
	auto irOldLeftType = GetIRType(oldLeftType)->As<IR::IntegerType>();
	auto irOldRightType = GetIRType(oldRightType)->As<IR::IntegerType>();

	// If the types are the same nothing needs to be done.
	if(irOldLeftType == irOldRightType) return irOldLeftType;
	
	// Else convert the operand with smaller type to one with the larger type.
	GenerateIntToIntCast(rightOp, irOldLeftType, oldRightType);
	return irOldLeftType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateComparisonOp(const BinaryOperator* op,
                                               IR::Operand* leftOp, IR::Operand* rightOp, 
											   const Type* resultType) {
	// The operands can be of integer, floating or pointer type.
	auto irResultType = GetIRType(resultType);
	auto leftType = op->LeftValue()->ResultType()->WithoutQualifiers();
	auto rightType = op->LeftValue()->ResultType()->WithoutQualifiers();

	// Mark the result as having boolean type (might improve some optimizations).
	result_ = irGen_->GetTemporary(irResultType);
	result_->SetIsBoolean(true);

	// Determine the comparison to perform.
	IR::OrderType order;

	switch(op->Operator()) {
		case BinaryOpType::Less:      { order = IR::OrderType::Less;           break; }
		case BinaryOpType::LessEq:    { order = IR::OrderType::LessOrEqual;    break; }
		case BinaryOpType::Greater:   { order = IR::OrderType::Greater;        break; }
		case BinaryOpType::GreaterEq: { order = IR::OrderType::GreaterOrEqual; break; }
		case BinaryOpType::NotEq:     { order = IR::OrderType::NotEqual;       break; }
		case BinaryOpType::EqEq:      { order = IR::OrderType::Equal;          break; }
	}

	// Select the right instruction, based on the type of the operands.
	bool onPointers = leftType->IsPointer() || rightType->IsPointer();

	if(onPointers) {
		// Compare pointers as if they were unsigned integers.
		// It's possible that one of them is a 'null-constant' integer, which
		// is represented by the 'NullConstant' IR object.
		if(leftType->IsInteger()) {
			auto nullConst = irGen_->GetNullConst(rightOp->GetType());
			irGen_->GetUcmp(order, nullConst, rightOp, 
                            result_, activeBlock_);
		}
		else if(rightType->IsInteger()) {
			auto nullConst = irGen_->GetNullConst(leftOp->GetType());
			irGen_->GetUcmp(order, leftOp, nullConst, 
                            result_, activeBlock_);
		}
		else irGen_->GetUcmp(order, leftOp, rightOp,
                             result_, activeBlock_);
	}
	else if(leftType->IsInteger()) {
		// Comparison of integer values.
		bool isSigned = leftType->IsSigned();

		if(isSigned) irGen_->GetCmp(order, leftOp, rightOp, 
                                    result_, activeBlock_);
		else irGen_->GetUcmp(order, leftOp, rightOp, 
                             result_, activeBlock_);
	}
	else if(leftType->IsFloating()) {
		// Comparison of floating values.
		irGen_->GetFcmp(order, leftOp, rightOp, 
                        result_, activeBlock_);
	}
	else DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::SimplifyBinaryOpCasts(shared<Expression>& leftValue, 
												shared<Expression>& rightValue,
												const Type** newLeftType, 
												const Type** newRightType) {
	// Consider the following case: 'char a, b; int c; c = a > b;'.
	// In this case 'a' and 'b' have each a 'char-to-integer' cast that is not necessary.
	if(auto leftCastExpr = leftValue->As<CastExpression>()) {
		if(auto rightCastExpr = rightValue->As<CastExpression>()) {
			auto leftCastType = leftCastExpr->ResultType()->WithoutQualifiers();
			auto rightCastType = rightCastExpr->ResultType()->WithoutQualifiers();

			auto leftTargetType = leftCastExpr->Target()->ResultType()->WithoutQualifiers();
			auto rightTargetType = rightCastExpr->Target()->ResultType()->WithoutQualifiers();

			// The simplification is valid only if the result of both casts is the same
			// integer type, and we cast from the same integer type.
			if(((leftCastType == rightCastType) && leftCastType->IsInteger()) &&
			   ((leftTargetType == rightTargetType) && leftTargetType->IsInteger())) {
				// Both cast to the same integer type; we can eliminate them.
				leftValue = leftCastExpr->Target();
				rightValue = rightCastExpr->Target();
				return;
			}
		}
	}

	// The following optimizations presume changing the type of the result operands.
	if((newLeftType && newRightType) == false) return;

	// Consider the following case: 'char a; if(a > 0) {...}'.
	// In this case an unnecessary 'char-to-integer' cast is introduced for 'a'.
	if(SimplifyBinaryOpCastNumber(rightValue, leftValue, newRightType) == false) {
		SimplifyBinaryOpCastNumber(leftValue, rightValue, newLeftType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::SimplifyBinaryOpCastNumber(shared<Expression>& numbValue,
													 shared<Expression>& otherValue,
													 const Type** newNumbType) {
	DebugValidator::IsNotNull(newNumbType);
	
	// If we have a number constant, and an expression that is cast to the type of
	// the number, we can change the number so that it has the type of the expression,
	// before applying the cast that can now be eliminated.
	// Note that this can be done only if the number remains the same when converted
	// ('char a; if(a < 1000) {...}' cannot be converted, because 1000 > 255).
	if(auto castExpr = otherValue->As<CastExpression>()) {
		auto castType = castExpr->ResultType()->WithoutQualifiers();
		auto targetType = castExpr->Target()->ResultType()->WithoutQualifiers();

		// This applies only for integer types.
		if((castType->IsInteger() && targetType->IsInteger())== false) {
            // Don't touch int/float combinations.
            return false;
        }

		// See if the expression is a constant with integer type.
		auto numbType = numbValue->ResultType()->WithoutQualifiers();
		EvaluationInfo eval;

		if(IsConstant(numbValue, eval)) {
			if(auto basicType = targetType->As<BasicType>()) {
				// Check that the number isn't truncated when converting the type.
				auto target = functGen_->GetTarget();
                auto kind = basicType->GetKind();
				__int64 maxValue = functGen_->GetTarget()->GetMaxValue(kind);

				if(eval.IntValue() <= maxValue) {
					// The number is small enough, do the optimization.
					*newNumbType = targetType;
					otherValue = castExpr->Target();
					return true;
				}
			}
		}
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateStore(IR::Operand* destOp, IR::Operand* sourceOp, 
										const Type* destType, const Type* sourceType,
										FieldDeclaration* bitfield) {
	// Generate the store and mark it as 'volatile' if it's the case.
	IR::Instruction* storeInstr = nullptr;

	if(bitfield) {
		// A special case arises for bitfields, because we must mask the value to be
		// stored, so that the adjacent values are not changed.
		auto record = bitfield->Parent()->DeclarationType();
		auto layout = functGen_->GetLayouts()->GetOrCreate(record);
		auto& bitfieldInfo = layout->GetFieldInfo(bitfield->Name());

		storeInstr = StoreToBitfield(destOp, sourceOp, 
                                     bitfield, bitfieldInfo);
	}
	else if(destType->IsRecord()) {
		GenerateRecordAssignment(destOp, sourceOp, destType);
	}
	else {
        functGen_->BeforeStore(destOp, currentExpr_, destType);
        storeInstr = irGen_->GetStore(destOp, sourceOp, activeBlock_);
        functGen_->AfterStore(storeInstr->As<IR::StoreInstr>(), 
                              currentExpr_, destType);
    }

	// If a single 'store' instruction was used (not the case for record assignment)
	// mark it as 'volatile', if it's the case.
	if(storeInstr) {
		SetVolatile(storeInstr, destType, true /* testPointer */);

		// Note that in this case we also need to reload the value and mark
		// the 'load' instruction as 'volatile'.
		if(IsVolatile(destType, true /* testPointer */)) {
			ReloadVolatileValue(destOp, destType);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateAssignment(const BinaryOperator* op) {
	// If the assignment involves records the right value should not be loaded,
	// only it's address must be computed. For all other types we load the value.
	bool loadRight = op->LeftValue()->ResultType()
					   ->WithoutQualifiers()->IsRecord() == false;

	// We generate a store of the source operand in the destination operand.
	// If the destination is a 'volatile' variable we mark the store as 'volatile'.
	// Note that the destination operand is not loaded, we only want it's address.
	SetLvalue();
	auto destOp = GenerateExpression(op->LeftValue());
	
	// OPTIMIZATION:
	// If the value we're storing is a record returned by a call we can
	// use the target variable directly, instead of using a temporary and then
	// copying into it. Applies to something like 'a = f()', where 'a' has record type.
	if((loadRight == false) && op->RightValue()->IsCallExpr()) {
		recordReturnTarget_ = destOp;
        GenerateExpression(op->RightValue());
		recordReturnTarget_ = nullptr; // Not valid anymore.
		return;
	}

    // Generate code for the source part.
	if(loadRight == false) {
        SetLvalue();
    }

	auto sourceOp = GenerateExpression(op->RightValue());

	// We must use a different method to store into a bitfield.
	FieldDeclaration* bitfield = nullptr;

	if(auto memberExpr = op->LeftValue()->As<MemberExpression>()) {
		if(memberExpr->Member()->IsBitfield()) {
			bitfield = memberExpr->Member();
		}
	}

	GenerateStore(destOp, sourceOp, op->LeftValue()->ResultType(), 
				  op->RightValue()->ResultType(), bitfield);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateCompoundAssignment(const BinaryOperator* op) {
	// First generate the code that computes the right side of the assignment,
	// then code for the left part, and then code to perform the assignment operation.
	// Note that the computed value is stored in the left side 
	// without generating code for it again (C99:6.5.16.2.3).
	ResetLvalue(); // Always load the value on the right.

	// If the operation is += or -=, and 'LeftValue' has pointer type, it's possible
	// that 'RightValue' has an unnecessary 'integer-to-pointer' cast that would
	// generate invalid code (for example 'char* p; p += 5;').
    IR::Operand* rightValueOp;

	if(((op->Operator() == BinaryOpType::AddEq) || 
        (op->Operator() == BinaryOpType::SubEq)) &&
		IsImplicitIntToPtrCast(op->RightValue())) {
		auto castExpr = op->RightValue()->As<CastExpression>();
		rightValueOp = GenerateExpression(castExpr->Target());
	}
	else rightValueOp = GenerateExpression(op->RightValue());

	// We want both the address and the value of the left side.
	// First get the address, then load the value; bitfields must be handled with care.
	SetLvalue();
    IR::Operand* leftAddrOp = GenerateExpression(op->LeftValue());

	auto irLeftType = GetIRType(op->LeftValue()->ResultType());
	FieldDeclaration* bitfield = nullptr;
	FieldInfo bitfieldInfo; // Used only if the left side is a bitfield.
	IR::Operand* leftValueOp = nullptr;

	// Check if the left side is a bitfield.
	if(auto memberExpr = op->LeftValue()->As<MemberExpression>()) {
		if(memberExpr->Member()->IsBitfield()) {
			bitfield = memberExpr->Member();
			auto recordType = bitfield->Parent()->DeclarationType();

			auto recordLayout = functGen_->GetLayouts()->GetOrCreate(recordType);
			bitfieldInfo = recordLayout->GetFieldInfo(bitfield);
			
			LoadFromBitfield(leftAddrOp, memberExpr->Member(), bitfieldInfo);
			leftValueOp = result_;
		}
	}

	if(bitfield == nullptr) {
		// This means the left side is not a bitfield and the value can
		// be loaded using a simple 'load' instruction.
		leftValueOp = irGen_->GetTemporary(GetIRType(op->LeftValue()->ResultType()));

        functGen_->BeforeLoad(leftAddrOp, currentExpr_);
		auto loadInstr = irGen_->GetLoad(leftAddrOp, leftValueOp, activeBlock_);
        functGen_->AfterLoad(loadInstr, currentExpr_);
	}

	// Perform the required operation.
	switch(op->Operator()) {
		case BinaryOpType::AddEq: 
		case BinaryOpType::SubEq: {
			bool onPointers = op->LeftValue()->ResultType()->IsPointer() ||
							  op->RightValue()->ResultType()->IsPointer();
			if(onPointers) {
				GeneratePointerAdditiveOp(op, leftValueOp, rightValueOp, 
										  op->ResultType());
			}
			else GenerateAdditiveOp(op, leftValueOp, rightValueOp, 
                                    op->ResultType());
			break;
		}   		
		case BinaryOpType::MulEq:    		
		case BinaryOpType::ModEq:    		
		case BinaryOpType::DivEq: {
			GenerateMultiplicativeOp(op, leftValueOp, rightValueOp, 
                                     op->ResultType());
			break;
		}		
		case BinaryOpType::AndEq:    		
		case BinaryOpType::OrEq:
		case BinaryOpType::ShiftLEq: 		
		case BinaryOpType::ShiftREq:
		case BinaryOpType::XorEq: {
			GenerateBitwiseOp(op, leftValueOp, rightValueOp, 
                              op->ResultType());
			break;
		}
		default: DebugValidator::Unreachable();
	}

	// Now store the value; storing into a bitfield needs special instructions.
	// If the value is not a bitfield we mark the 'store' as 'volatile'.
	if(bitfield) {
		StoreToBitfield(leftAddrOp, result_, bitfield, bitfieldInfo);
	}
	else {
        functGen_->BeforeStore(leftAddrOp, currentExpr_, op->LeftValue()->ResultType());

		auto storeInstr = irGen_->GetStore(leftAddrOp, result_, activeBlock_);
		SetVolatile(storeInstr, op->ResultType(), true /* testPointer */);

        functGen_->AfterStore(storeInstr, currentExpr_, op->LeftValue()->ResultType());

		// Note that in this case we also need to reload the value and mark
		// the 'load' instruction as 'volatile'.
		if(IsVolatile(op->ResultType(), true /* testPointer */)) {
			ReloadVolatileValue(leftAddrOp, op->ResultType());
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateLogical(const BinaryOperator* op) {
	// OPTIMIZATION:
	// If we can prove, at compile time, that the expression
    // always evaluates to 'true' or 'false' we can select 
    // the right result without creating any jumps.
	if(GenerateLogicalOptimized(op)) {
        return;
    }

	// Create a variable where the result is stored, then two blocks
	// where the variable gets it's values. Based on the result of the expression
	// we jump to one of the blocks, and from there to the continuation block.
	// In the continuation block we load the value of the variable.
	StatementGenerator statementGen(functGen_);
	auto irResultType = GetIRType(op->ResultType());

	IR::Block* trueBlock = functGen_->CreateBlock(BlockName::LogicalTrue);
	IR::Block* falseBlock = functGen_->CreateBlock(BlockName::LogicalFalse);
	IR::Block* contBlock = functGen_->CreateBlock(BlockName::LogicalCont);

	// Generate the expression and the 'if' instruction that jumps to the blocks.
	statementGen.GenerateIf(op, trueBlock, falseBlock);

	// Create the variable and add it to the function. Then generate the following code:
	// 'label logical_true:
	//		store &logical_var, 1
	//      goto logical_cont'
	// and the same (except 0 instead of 1) for block 'logical_false'.
    string varName = functGen_->GetNameGen()->GetLogicalVarName();
	auto logicalVariableRef = functGen_->AddVariable(irResultType, varName);
	auto oneConst = irGen_->GetIntConst(irResultType, 1);
	auto zeroConst = irGen_->GetIntConst(irResultType, 0);

	// Mark the values as having boolean type; can improve some optimizations.
	functGen_->InsertAndMakeActive(trueBlock);
    functGen_->BeforeStore(logicalVariableRef, currentExpr_);

	auto storeInstr1 = irGen_->GetStore(logicalVariableRef, oneConst, trueBlock);
    functGen_->AfterStore(storeInstr1, currentExpr_);
	irGen_->GetGoto(contBlock, trueBlock);

	functGen_->InsertAndMakeActive(falseBlock);
    functGen_->BeforeStore(logicalVariableRef, currentExpr_);

	auto storeInstr2 = irGen_->GetStore(logicalVariableRef, zeroConst, falseBlock);
    functGen_->AfterStore(storeInstr2, currentExpr_);
	irGen_->GetGoto(contBlock, falseBlock);

	// Insert the continuation block, then load the variable.
	functGen_->InsertAndMakeActive(contBlock);
	activeBlock_ = contBlock; // It has changed.

	// Mark the values as having boolean type; can improve some optimizations.
	result_ = irGen_->GetTemporary(irResultType);
	result_->SetIsBoolean(true);

    functGen_->BeforeLoad(logicalVariableRef, currentExpr_);
	auto loadInstr = irGen_->GetLoad(logicalVariableRef, result_, contBlock);
    functGen_->AfterLoad(loadInstr, currentExpr_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::GenerateLogicalOptimized(const BinaryOperator* op) {
	StatementGenerator statementGen(functGen_);
	auto irResultType = GetIRType(op->ResultType());

	// Mark the values as having boolean type; can improve some optimizations.
	if(statementGen.IsAlwaysTrue(op)) {
		// Use 1 for 'true'.
		result_ = irGen_->GetIntConst(irResultType, 1);
		result_->SetIsBoolean(true);
		return true;
	}
	else if(statementGen.IsAlwaysFalse(op)) {
		// Use 0 for 'false'.
		result_ = irGen_->GetIntConst(irResultType, 0);
		result_->SetIsBoolean(true);
		return true;
	}
	else return false;
}

} // namespace IRGenerator