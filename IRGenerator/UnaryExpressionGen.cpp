// ExpressionGenerator.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

void ExpressionGenerator::Visit(const UnaryOperator* expr) {
	// First we try to evaluate the expression as a constant.
	if(GenerateConstant(expr)) return;

	switch(expr->Operator()) {
		case UnaryOpType::Add: break; // Nothing to do in this case.
		case UnaryOpType::Sub:         { GenerateNegation(expr);    break; } // -
		case UnaryOpType::Complement:  { GenerateComplement(expr);  break; } // ~
		case UnaryOpType::Inc:
		case UnaryOpType::Dec:         { GenerateIncDec(expr);      break; }
		case UnaryOpType::Address:     { GenerateAddress(expr);     break; } // &
		case UnaryOpType::Indirection: { GenerateIndirection(expr); break; } // *
		case UnaryOpType::Not:         { GenerateNot(expr);         break; } // !
		default: DebugValidator::Unreachable();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateNegation(const UnaryOperator* op) {
	// We change the sign by subtracting the operand from 0.
	// Generate code for the target expression first.
	auto targetOp = GenerateExpression(op->Value());
	auto irResultType = GetIRType(op->ResultType());
	result_ = irGen_->GetTemporary(irResultType);

	if(irResultType->IsInteger()) {
		auto zero = irGen_->GetIntConst(irResultType, 0);
		irGen_->GetSub(zero, targetOp, result_, activeBlock_);
	}
	else {
		// Operand having floating type.
		auto zero = irGen_->GetFloatingConst(irResultType, 0);
		irGen_->GetFsub(zero, targetOp, result_, activeBlock_);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateComplement(const UnaryOperator* op) {
	// We generate the complement by doing an XOR with 1111...111 (-1).
	auto intType = GetBasicType(op->ResultType()->WithoutQualifiers());
	auto irResultType = GetIRType(intType);
	auto target = functGen_->GetTarget();

	// t = xor result_, -1
	auto targetOp = GenerateExpression(op->Value());

	result_ = irGen_->GetTemporary(irResultType);
	auto minusOneConst = irGen_->GetIntConst(irResultType, -1);
	auto instr = irGen_->GetXor(targetOp, minusOneConst, 
                                result_, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateNot(const UnaryOperator* op) {
	// !a should return 1 if 'a' is 0 and 0 vice versa, so we generate
	// a comparison for equality with 0 and take it's result.
	auto targetType = op->Value()->ResultType()->WithoutQualifiers();
	auto irTargetType = GetIRType(targetType);
	auto irResultType = GetIRType(op->ResultType());
	IR::Instruction* instr;

	// The operand can have integer, floating or pointer type.
	auto targetOp = GenerateExpression(op->Value());

	// Mark the result as having boolean type; can improve some optimizations.
	result_ = irGen_->GetTemporary(irResultType);
	result_->SetIsBoolean(true);

	if(targetType->IsInteger()) {
		// Note that when comparing to 0 it doesn't matter
        // if the number is signed or not.
		auto zeroConst = irGen_->GetIntConst(irTargetType, 0);
		instr = irGen_->GetCmpEQ(targetOp, zeroConst,
                                 result_, activeBlock_);
	}
	else if(targetType->IsFloating()) {
		auto zeroConst = irGen_->GetFloatingConst(irTargetType, 0);
		instr = irGen_->GetFcmpEQ(targetOp, zeroConst, 
                                  result_, activeBlock_);
	}
	else {
		// If it's a pointer we compare with the null-pointer constant.
		auto zeroConst = irGen_->GetNullConst(irTargetType);
		instr = irGen_->GetUcmpEQ(targetOp, zeroConst, 
                                  result_, activeBlock_);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateIncDec(const UnaryOperator* op) {
	// Increment/decrement behave differently if in a prefix position
    // than in a postfix one. For prefix, we take the value after 
    // it has been changed, while for postfix we take the value before it's modified.
	auto irResultType = GetIRType(op->ResultType());

	// Generate code for the target. Note that we want only 
    // the address of the target expression, so we make it think 
    // it's in a lvalue position.
	SetLvalue();
	auto targetOp = GenerateExpression(op->Value());

	// If the target is a bitfield we need to use a different
    // (sadly much more complex) method.
	FieldDeclaration* bitfield = nullptr;
	FieldInfo* bitfieldInfo = nullptr;

	if(auto memberExpr = op->Value()->As<MemberExpression>()) {
		if(memberExpr->Member()->IsBitfield()) {
			bitfield = memberExpr->Member();
			auto record = bitfield->Parent()->DeclarationType();
			auto layout = functGen_->GetLayouts()->GetOrCreate(record);
			bitfieldInfo = &layout->GetFieldInfo(bitfield->Name());
		}
	}

	if(irResultType->IsInteger() || irResultType->IsFloating()) {
		// 'int a; a++' is converted to: (t1 is the taken value)
		// var a int32    t1 = load a    t2 = add t1, 1     store a, t2
		bool isIncrement = op->Operator() == UnaryOpType::Inc;
		bool isPostfix = op->IsPostfix();
		bool isInt = irResultType->IsInteger();
        
		// Load the value. For bitfields we use the specialized method.
		auto t1 = irGen_->GetTemporary(irResultType);

		if(bitfield) {
            LoadFromBitfield(targetOp, bitfield, *bitfieldInfo);
        }
		else { 
            functGen_->BeforeLoad(targetOp, currentExpr_, op->ResultType());

            auto loadInstr = irGen_->GetLoad(targetOp, t1, activeBlock_);
            SetVolatile(loadInstr, op->Value()->ResultType(), true /* testPointer */);

            functGen_->AfterLoad(loadInstr, currentExpr_, op->ResultType());
        }

		// Generate the instruction to increment/decrement.
		auto t2 = irGen_->GetTemporary(irResultType);

		if(isInt) {
			auto oneConst = irGen_->GetIntConst(irResultType, 1);
			
            if(isIncrement) {
                SetOverflowFlag(irGen_->GetAdd(t1, oneConst, t2, activeBlock_));
            }
			else SetOverflowFlag(irGen_->GetSub(t1, oneConst, t2, activeBlock_));
		}
		else {
            // Floating numbers.
			auto oneConst = irGen_->GetFloatingConst(irResultType, 1.0);

			if(isIncrement) {
                SetOverflowFlag(irGen_->GetFadd(t1, oneConst, t2, activeBlock_));
            }
			else SetOverflowFlag(irGen_->GetFsub(t1, oneConst, t2, activeBlock_));
		}

		// Store the result and select the taken value.
		if(bitfield) {
            StoreToBitfield(targetOp, t2, bitfield, *bitfieldInfo);
        }
		else { 
            functGen_->BeforeStore(targetOp, currentExpr_, op->Value()->ResultType());

            auto storeInstr = irGen_->GetStore(targetOp, t2, activeBlock_);
		    SetVolatile(storeInstr, op->Value()->ResultType(), true /* testPointer */);
            
            functGen_->AfterStore(storeInstr, currentExpr_, op->Value()->ResultType());
        }

		// Select the value used as a result.
		result_ = isPostfix ? t1 : t2;
	}
	else {
		// Increment/decrement of a pointer.
		GeneratePointerIncDec(op, targetOp);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GeneratePointerIncDec(const UnaryOperator* op, 
                                                IR::Operand* targetOp) {
	// 'char* p; --p' is converted to: (t2 is the taken value)
	// var p int8*    t1 = load p    t2 = addr t1, -1    store p, t2
	bool isIncrement = op->Operator() == UnaryOpType::Inc;
	bool isPostfix = op->IsPostfix();

	// It's possible that the operand to be incremented is a pointer to VLA.
	// In this case we use a pointer to the inner type.
	auto resultType = op->ResultType()->As<PointerType>();

	if(auto arrayType = resultType->PointeeType()->As<ArrayType>()) {
		if(arrayType->IsVariable()) {
			GenerateVLAPointerIncDec(op, targetOp, arrayType);
			return;
		}
	}

	// Load the value.
	// t1 - value before, t2 - value after
    auto irResultType = GetIRType(op->ResultType());
    functGen_->BeforeLoad(targetOp, currentExpr_, op->ResultType());

    auto t1 = irGen_->GetTemporary(irResultType);
	auto loadInstr = irGen_->GetLoad(targetOp, t1, activeBlock_);
    SetVolatile(loadInstr, op->Value()->ResultType(), true /* testPointer */);
    
    functGen_->AfterLoad(loadInstr, currentExpr_, op->ResultType());

	// Generate the instruction to increment/decrement
	auto t2 = irGen_->GetTemporary(irResultType);

	if(isIncrement) {
		auto oneConst = irGen_->GetInt32Const(1);
		irGen_->GetAddress(t1, oneConst, t2, activeBlock_);
	}
	else {
		auto minusOneConst = irGen_->GetInt32Const(-1);
		irGen_->GetAddress(t1, minusOneConst, t2, activeBlock_);
	}

	// Store the result and select the taken value.
    functGen_->BeforeStore(targetOp, currentExpr_, op->ResultType());
	
    auto storeInstr = irGen_->GetStore(targetOp, t2, activeBlock_);
    SetVolatile(storeInstr, op->Value()->ResultType(), true /* testPointer */);
    
    functGen_->AfterStore(storeInstr, currentExpr_, op->ResultType());

	result_ = isPostfix ? t1 : t2;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateAddress(const UnaryOperator* op) {
	// To take the address we simply generate code for the target 
    // expression with loading disabled. We also enable 'address_' 
    // so that we take the address of something like '&a[1]', not its value.
	SetLvalue();
	SetAddress();
	SetMemberAddress();
	Visit(op->Value());
	ResetFlags();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateIndirection(const UnaryOperator* op) {
	// We need to generate a load instruction that targets the result returned
	// by the subexpression. This also works for cases like '*(p + 2)'.
	// If the expression is in a lvalue position or the type of the result 
	// is a record or function the pointed value is not loaded.
	auto resultType = op->ResultType()->WithoutQualifiers();
	bool resultRecord = resultType->IsRecord();
	bool resultFunct = resultType->IsFunction();
	bool shouldLoad = ShouldLoad() && 
                      ((resultRecord || resultFunct) == false);

    // The pointer must always be loaded (not for ptr. to record/function).
	ResetLvalue(); 
	Visit(op->Value());
	auto targetOp = result_;
	ResetFlags();

	if(shouldLoad) {
		// 'int* p; *p' -> t1 = load &p     result_ = load t1
		auto irResultType = GetIRType(op->ResultType());
		result_ = irGen_->GetTemporary(irResultType);

        functGen_->BeforeLoad(targetOp, currentExpr_, op->ResultType());

        auto loadInstr = irGen_->GetLoad(targetOp, result_, activeBlock_);
		SetVolatile(loadInstr, op->Value()->ResultType(), true /* testPointer */);
        
        functGen_->AfterLoad(loadInstr, currentExpr_, op->ResultType());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const SubscriptExpression* expr) {
	// A subscript expression always has a base that is a pointer. If the base
	// is a pointer to an array we don't need to generate the conversion
	// to a pointer; instead we directly use the array (via an 'index' instruction).
	auto baseType = expr->Base()->ResultType()->WithoutQualifiers();
	bool shouldLoad = ShouldLoad() && ShouldLoadAddress();

	bool castToPointer = false; // 'true' if the base is an 'array-to-pointer' cast.
	const VarArrayType* baseVLAType = nullptr; // IsValid only if base is a VLA.

	// Generate code for the base.
	auto baseOp = GenerateSubscriptBase(expr, castToPointer, baseVLAType);

	// Compute the address according to the index expression.
	// The address has type 'pointer to element type'.
	Visit(expr->Index());
	ResetFlags(true /* ignoreVLA */);
	auto indexOp = result_;

	if(HasVLA()) {
		// If the base is a VLA we need to multiply the index 
        // by the size of the base array.
		auto pointeeType = baseType->As<PointerType>()->PointeeType();

		if(auto arrayType = pointeeType->As<ArrayType>()) {
			indexOp = GenerateVLAOffset(arrayType, indexOp, 
                                        expr->Index()->ResultType());
		}
	}

	// The type of the address computation is the result of the expression
	// if the type is not a VLA. If it's a VLA it's a pointer
    // to the type of the base.
	const IR::Type* addrType;
	
	if(HasVLA()) {
		auto pointeeType = baseType->As<PointerType>()->PointeeType();
		const Type* innerType;

		if(auto arrayType = pointeeType->As<ArrayType>()) {
			innerType = functGen_->GetArrayInnerType(arrayType);
		}
		else innerType = pointeeType;

		// A pointer to the inner type is required.
		addrType = irGen_->GetPointer(GetIRType(innerType));
	}
	else addrType = GetIRType(expr->Base()->ResultType());

	// The instruction used to compute the address depends on the base:
	// array: 'index'       
	// pointer: 'addr'
	result_ = irGen_->GetTemporary(addrType);
	
	if(castToPointer && (HasVLA() == false)) {
		irGen_->GetIndex(baseOp, indexOp, result_, activeBlock_);
	}
	else irGen_->GetAddress(baseOp, indexOp, result_, activeBlock_);

	// If this is the top-level subscript expression ([3] in 'a[1][2][3]')
	// and we're not in an lvalue position (like in 'b = a[1][2][3]'), 
    // we need to load the value pointed by the computed address.
	if(shouldLoad == false) {
        return;
    }

	auto addressOp = result_;
	result_ = irGen_->GetTemporary(GetIRType(expr->ResultType()));

    functGen_->BeforeLoad(addressOp, currentExpr_, expr->ResultType());
	
    auto loadInstr = irGen_->GetLoad(addressOp, result_, activeBlock_);
	SetVolatile(loadInstr, expr->ResultType(), true /* testPointer */);

    functGen_->AfterLoad(loadInstr, currentExpr_, expr->ResultType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* 
ExpressionGenerator::GenerateSubscriptBase(const SubscriptExpression* expr, 
                                           bool& castToPointer,
										   const VarArrayType*& baseVLAType) {
	auto baseType = expr->Base()->ResultType()->WithoutQualifiers();

	if(auto castExpr = expr->Base()->As<CastExpression>()) {
		if(castExpr->Type() == CastType::ArrayToPtr) {
			auto castTargetType = castExpr->Target()->ResultType()
                                          ->WithoutQualifiers();

			// VLAs should not be treated like regular arrays.
			if(castTargetType->IsVariable()) {
				SetVLA();
			}
			
			castToPointer = true;
			SetLvalue(); // We don't want the array to be loaded.
			SetAddress();
			SetResetVLAAllowed(false);
			Visit(castExpr->Target());
			SetResetVLAAllowed(true);
		}
	}
	
	// Load the base now if it hasn't been loaded already.
	if(castToPointer == false) {
		if(expr->Base()->IsDeclarationExpr() || 
           baseType->IsPointer()) {
			// If the base is a variable or a pointer we must load it.
			ResetLvalue();
			ResetAddress();
			ResetMemberAddress();
		}
		else {
			// The base must not be loaded (for example, 'f()[3]').
			SetLvalue();
			SetAddress();
			SetMemberAddress();
		}
		
		SetResetVLAAllowed(false);
		Visit(expr->Base());
		SetResetVLAAllowed(true);
	}

	ResetFlags(true /* ignoreVLA */); // Resets all flags set above.
	return result_;
}

} // namespace IRGenerator