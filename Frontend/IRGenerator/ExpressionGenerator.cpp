// ExpressionGenerator.cpp
// Copyright (c) Lup Gratian
//
// Implements the expression generator.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionGenerator.hpp"
#include "StatementGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

ExpressionGenerator::ExpressionGenerator(FunctionGenerator* functGen) :
		functGen_(functGen), 
        typeGen_(functGen_->GetTypeGen()), 
		irGen_(functGen_->GetIRGen()), 
        activeBlock_(functGen->ActiveBlock()),
		address_(false), 
        lvalue_(false), 
        membAddress_(false), 
        currentExpr_(nullptr),
		recordReturnTarget_(nullptr), 
        vla_(false), 
        resetVLAAllowed_(true),
        markFunctionAddressTaken_(true),
		namedTemp_(functGen->GetContext()->Options().ShouldNameTemporaries()) {
	DebugValidator::IsNotNull(functGen);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const Expression* expr) {
    currentExpr_ = expr;
	expr->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IR::Type* ExpressionGenerator::GetIRType(const Type* type) {
	return typeGen_->GetType(type, functGen_->GetIRFunction());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const BasicType* ExpressionGenerator::GetBasicType(const Type* type) {
	// If the type is an 'enum' we return the type used to store its value.
	if(auto enumType = type->As<EnumType>()) {
		return enumType->ConstType();
	}
	else return type->As<BasicType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::IsVolatile(const Type* type, bool testPointer) {
	// Return 'true' if the type is 'volatile' qualified, 
	// or we have a pointer to a 'volatile' qualified type.
	const QType* qualType = type->As<QType>();

	if((qualType == nullptr) && testPointer) {
		if(auto pointerType = type->As<PointerType>()) {
			qualType = pointerType->PointeeType()->As<QType>();
		}
	}

	return qualType && qualType->HasVolatile();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::SetVolatile(IR::Instruction* instr, const Type* type, 
									  bool testPointer) {
	DebugValidator::IsTrue(instr->IsLoad() || instr->IsStore());

	// Mark a 'load' or 'store' instruction as 'volatile' if the type is
	// 'volatile' qualified, or we have a pointer to a 'volatile' qualified type.
	if(IsVolatile(type, testPointer)) {
		if(auto loadInstr = instr->As<IR::LoadInstr>()) {
			loadInstr->SetIsVolatile(true);
		}
		else instr->As<IR::StoreInstr>()->SetIsVolatile(true);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::ReloadVolatileValue(IR::Operand* valueAddrOp, 
											  const Type* valueType) {
	// Mark the result as having boolean type; can improve some optimizations.
	valueType = valueType->WithoutQualifiers();
	result_ = irGen_->GetTemporary(GetIRType(valueType));
	result_->SetIsBoolean(valueType->IsBool());

    functGen_->BeforeLoad(valueAddrOp, currentExpr_, valueType);
	
    auto loadInstr = irGen_->GetLoad(valueAddrOp, result_, activeBlock_);
	loadInstr->SetIsVolatile(true);
    
    functGen_->AfterLoad(loadInstr, currentExpr_, valueType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* ExpressionGenerator::Generate(const Expression* expr) {
	// If the result of the expression is not used and it has
    // record or array type don't load the value, compute only 
    // its address (else invalid IR is generated).
	if(expr->ResultType()->WithoutQualifiers()->IsAggregate()) {
		SetLvalue();
	}

	// Generate the whole expression and return the result.
    Visit(expr);
	return result_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* ExpressionGenerator::LoadSimpleValue(IR::Operand* addressOp, 
                                                  const Type* type) {
	type = type->WithoutQualifiers();
	auto irResultType = GetIRType(type);

	// Mark the result as having boolean type.
    // This can improve some optimizations.
	result_ = irGen_->GetTemporary(irResultType);
	result_->SetIsBoolean(type->IsBool());

    functGen_->BeforeLoad(addressOp, currentExpr_);

    auto loadInstr = irGen_->GetLoad(addressOp, result_, activeBlock_);
	SetVolatile(loadInstr, type, false /* testPointer */);

    functGen_->AfterLoad(loadInstr, currentExpr_);
    return result_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* ExpressionGenerator::LoadSimpleValue(IR::Operand* addressOp) {
    DebugValidator::IsTrue(addressOp->GetType()->IsPointer());

    auto irResultType = addressOp->GetType()->As<IR::PointerType>()->PointeeType();
	result_ = irGen_->GetTemporary(irResultType);

    functGen_->BeforeLoad(addressOp, currentExpr_);
	auto loadInstr = irGen_->GetLoad(addressOp, result_, activeBlock_);

    functGen_->AfterLoad(loadInstr, currentExpr_);
    return result_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateIntToIntCast(IR::Operand*& op, 
											   const IR::IntegerType* newType,
									           const Type* oldType) {
	DebugValidator::IsTrue(oldType->WithoutQualifiers()->IsInteger());
	oldType = oldType->WithoutQualifiers();
	auto irOldType = op->GetType()->As<IR::IntegerType>();
	DebugValidator::IsNotNull(irOldType); // The size operand should be an integer.

	// If the operand has the right type nothing needs to be done.
	if(irOldType == newType) return;
	
	// If the operand is a 'IntConstant' just change its type.
	if(auto intConst = op->As<IR::IntConstant>()) {
		op = irGen_->GetIntConst(newType, intConst->Value());
		return;
	}

	// Select the right type of conversion to perform.
	auto newOp = irGen_->GetTemporary(newType);
	IR::Instruction* instr;

	if(irOldType->RankBelow(newType)) {
		// An extension is needed. Based on the fact that the type
        // of the size expression is signed or unsigned, 
        // choose the right extension instruction.
		if(oldType->IsSigned()) {
			instr = irGen_->GetSext(op, newType, newOp, activeBlock_);	
		}
		else instr = irGen_->GetZext(op, newType, newOp, activeBlock_);
	}
	else {
		// A truncation is needed. This shouldn't actually happen, because
		// 'size_t' is supposed to be the largest integer type.
		instr = irGen_->GetTrunc(op, newType, newOp, activeBlock_);
	}

    DebugValidator::AreNotEqual(instr->GetSourceOp(0)->GetType(), newType);
	op = newOp;	
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* ExpressionGenerator::PatchOperandType(IR::Operand* op, 
                                                   const IR::Type* newType) {
    // We can't change the type of a constant, because they are unique
    // (the type for all constants with the current type would change).
    if(auto intConst = op->As<IR::IntConstant>()) {
        return irGen_->GetIntConst(newType, intConst->Value());
    }
    else if(auto floatConst = op->As<IR::FloatConstant>()) {
        return irGen_->GetFloatingConst(newType, floatConst->Value());
    }
    else if(auto nullConst = op->As<IR::NullConstant>()) {
        return irGen_->GetNullConst(newType);
    }
    else if(auto undefConst = op->As<IR::UndefinedConstant>()) {
        return irGen_->GetUndefinedConst(newType);
    }

    // For temporaries we can change the type directly.
    op->SetType(newType);
    return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::StoreInitializer(IR::Variable* irVariable, 
                                           const Expression* initializer) {
    // Should not be used for array/record types.
	DebugValidator::IsFalse(initializer->IsInitListExpr());
	
	// Generate a store instruction that initializes the variable
	// with the specified expression. We generate code for the expression, 
    // then 'store' the result in the variable.
	Visit(initializer);
	auto variableRef = irGen_->GetVariableRef(irVariable);

    functGen_->BeforeStore(variableRef, currentExpr_, 
                           nullptr, initializer->ResultType());

	auto storeInstr = irGen_->GetStore(variableRef, result_,activeBlock_);
    
    functGen_->AfterStore(storeInstr, currentExpr_, 
                          nullptr, initializer->ResultType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::StoreInitializer(IR::Operand* destOp, 
                                           const Expression* initializer) {
    // Should not be used for array/record types.
	DebugValidator::IsFalse(initializer->IsInitListExpr()); 
	Visit(initializer);

    functGen_->BeforeStore(destOp, currentExpr_, 
                           nullptr, initializer->ResultType());

	auto storeInstr = irGen_->GetStore(destOp, result_, activeBlock_);
    
    functGen_->AfterStore(storeInstr, currentExpr_, 
                          nullptr, initializer->ResultType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::IsConstant(const Expression* expr, EvaluationInfo& eval) {
	if(expr->TryEvaluateConstant(functGen_->GetContext(), 
                                 &eval, false /* allowVars */)) {
		// It's a constant, create the required type.
		// We don't accept constants that involve other variables or pointers.
		return (eval.HasVariable() || eval.IsOtherConstant()) == false;
	}
	
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::GenerateConstant(const Expression* expr) {
	// We try to emit the expression as a constant instead of generating 
	// the instructions. Applicable for integer, floating and pointer types.
	EvaluationInfo eval;

	if(IsConstant(expr, eval)) {
		auto resultType = expr->ResultType()->WithoutQualifiers();
		auto irResultType = GetIRType(resultType);

		if(irResultType->IsInteger()) {
			// Mark the result as having boolean type.
            // This can improve some optimizations.
			result_ = irGen_->GetIntConst(irResultType, eval.IntValue());
			return true;
		}
		else if(irResultType->IsFloating()) {
			result_ = irGen_->GetFloatingConst(irResultType, eval.FloatValue());
			return true;
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const NumberConstant *expr) {
	// Convert the constant to a 'IntConstant' object
    // having the corresponding type.
	auto resultType = expr->ResultType()->WithoutQualifiers();
	auto irResultType = GetIRType(resultType);
	
	if(irResultType->IsInteger()) {
		result_ = irGen_->GetIntConst(irResultType, expr->Value().IntValue);
	}
    else if(irResultType->IsFloating()) {
        result_ = irGen_->GetFloatingConst(irResultType, expr->Value().FloatValue);
    }
    else {
        // This must be a pointer otherwise.
        DebugValidator::IsTrue(irResultType->IsPointer());
        DebugValidator::AreEqual(expr->Value().IntValue, 0);
        result_ = irGen_->GetNullConst(irResultType);
    }

	// Mark the result as having boolean type. 
    // This can improve some optimizations.
	result_->SetIsBoolean(resultType->IsBool());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const CharConstant *expr) {
	// Character constants are converted to a 'IntConstant' object.
	auto irResultType = GetIRType(expr->ResultType());
	result_ = irGen_->GetIntConst(irResultType, (__int64)expr->Value().Value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const StringConstant *expr) {
	// String constants are saved as global variables with a unique name
	// and here we create only a reference to them.
	// For example, 'void f() { char* p = "abcd"; }' must be converted to:
	// 'var $const_f_str1 [5 int8] = "abcd" // what follows is inside 'f'
	//  var p int8*     t1 = addr $const_f_str, 0     store p, t2
	result_ = functGen_->AddStringConstant(expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const SizeofOperator* op) {
	// The 'sizeof' operator is converted to an 'IntConstant' 
    // having the size of the object. The target expression is not generated.
	// A complication arises if the target is a VLA or pointer to a VLA.
	// If the result should not be taken then we don't have to do anything.
	auto targetType = op->Target()->ResultType()->WithoutQualifiers();

	if(targetType->IsVariable() == false) {
		auto irResultType = GetIRType(op->ResultType()); // size_t
		__int64 size = op->Evaluate(functGen_->GetContext(), 
                                    false /* warn */).IntValue();
		result_ = irGen_->GetIntConst(irResultType, size);
	}
	else {
		// Compute the size based on the dynamic values.
		GenerateVLASizeof(op);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const DeclarationExpression* expr) {
	// A reference can point to a variable, function or enumeration constant.
	if(auto variable = expr->Object()->As<VariableDeclaration>()) {
		GenerateVariableDeclarationExpression(expr, variable);
	}
	else if(auto enumConst = expr->Object()->As<EnumConstDeclaration>()) {
		// Enumeration constants are represented by constant integers.
		auto irResultType = GetIRType(enumConst->DeclarationType());
		EvaluationInfo eval = enumConst->ValueExpr()->Evaluate(functGen_->GetContext(), 
                                                               false /* warn */);
		result_ = irGen_->GetIntConst(irResultType, eval.IntValue());
	}
	else if(auto function = expr->Object()->As<FunctionDeclaration>()) {
        auto functionRef = functGen_->GetUnitGen()->GetFunctionReference(function);
		result_ = functionRef;

        // If the functions reference is used in any other way
        // than a call target be conservative and mark it
        // as being address taken.
        if(ShouldMarkFunctionAddressTaken()) {
            functionRef->Target()->SetIsAddresTaken(true);
        }
	} 
	else DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::
     GenerateVariableDeclarationExpression(const DeclarationExpression* expr,
                                           const VariableDeclaration* variable) {
    // Make a reference to the variable, loading its contents if requested.
    // We don't load it if it's on the left side of an assignment or
    // if the type is a 'struct' or 'union'.
    auto variableRef = functGen_->GetVariableReference(variable);
    auto variableType = expr->ResultType()->WithoutQualifiers();
    bool recordType = variableType->IsRecord();

    // Records that are function parameters were converted 
    // to 'pointer to record', but the uses must believe they
    // still working directly with the record.
    // In this case insert a instruction that loads the pointer.
    if(recordType) {
        if(auto pointerType = variableRef->GetType()->As<IR::PointerType>()) {
            // If it's a parameter the pointee type is 'pointer', not 'record'.
            if(pointerType->PointeeType()->IsPointer()) {
                auto loadedRecord = irGen_->GetTemporary(pointerType->PointeeType());
                result_ = loadedRecord;

                functGen_->BeforeLoad(variableRef, currentExpr_, variableType);
                auto loadInstr = irGen_->GetLoad(variableRef, loadedRecord, activeBlock_);
                SetVolatile(loadInstr, expr->ResultType(), false /* testPointer */);

                functGen_->AfterLoad(loadInstr, currentExpr_, variableType);
                return;
            }
        }
    }
    else if(variableType->IsVariable() && 
        (variableType->IsPointer() == false)) {
            // This is a VLA and its IR type is not array, 
            // instead its pointer to the innermost element type 
            // -> we need to load the pointer.
            auto irVarPtrType = variableRef->GetVariable()->GetType()
                                ->As<IR::PointerType>();
            result_ = irGen_->GetTemporary(irVarPtrType);
            functGen_->BeforeLoad(variableRef, currentExpr_, variableType);

            auto loadInstr = irGen_->GetLoad(variableRef, result_, activeBlock_);
            SetVolatile(loadInstr, expr->ResultType(), false /* testPointer */);

            functGen_->AfterLoad(loadInstr, currentExpr_, variableType);
            return;
    }
    else if(ShouldLoad()) {
        // Generate 'result_ = load variableRef'.
        // We must take care if we are loading from a volatile variable.
        // Note that if the variable is a pointer we don't mark this 'load'
        // as 'volatile', because we don't load the pointed value here, 
        // we load just the pointer instead.
        auto irResultType = GetIRType(expr->ResultType());

        // Mark the result as having boolean type.
        // This can improve some optimizations.
        result_ = irGen_->GetTemporary(irResultType);
        result_->SetIsBoolean(variableType->IsBool());

        functGen_->BeforeLoad(variableRef, currentExpr_, variableType);

        auto loadInstr = irGen_->GetLoad(variableRef, result_, activeBlock_);
        SetVolatile(loadInstr, expr->ResultType(), false /* testPointer */);

        functGen_->AfterLoad(loadInstr, currentExpr_, variableType);
        return;
    }

    // Use the reference to the variable directly for all other cases.
    result_ = variableRef;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const ConditionalOperator* op) {
	StatementGenerator statementGen(functGen_);

	// OPTIMIZATION:
    // Try to simplify if we work with constants.
    if(GenerateConditionalOptimized(op)) {
        return;
    }

	// If the expression has a type different than 'void' we need to store
	// the result generated in the 'true'/'false' blocks in a temporary variable.
	auto resultType = op->ResultType()->WithoutQualifiers();
	bool needsTemp = resultType->IsVoid() == false;
	IR::VariableReference* tempVariableRef = nullptr;

	if(needsTemp) {
		auto irType = GetIRType(op->ResultType());
        string name = functGen_->GetNameGen()->GetCondTempName();
		tempVariableRef = functGen_->AddVariable(irType, name);
	}

	// Generate the condition expression in the current block.
    // Then create two blocks, one for the 'true' expression, 
    // and one for the 'false' one, and a continuation block.
	IR::Block* trueBlock = functGen_->CreateBlock(BlockName::CondTrue);
	IR::Block* falseBlock = functGen_->CreateBlock(BlockName::CondFalse);
	IR::Block* contBlock = functGen_->CreateBlock(BlockName::CondCont);

	statementGen.GenerateIf(op->Condition(), trueBlock, falseBlock);
	GenerateConditionalBlock(trueBlock, op->Left(), 
                            contBlock, tempVariableRef);
	GenerateConditionalBlock(falseBlock, op->Right(), 
                             contBlock, tempVariableRef);
	
	// Make the continuation block active, then generate a 'load' of the value.
	// If the result has 'record' type the operand is a reference to the temp. variable.
	functGen_->InsertAndMakeActive(contBlock);
	activeBlock_ = contBlock; // The active block has changed.

	if(needsTemp) {
		auto irType = GetIRType(op->ResultType());

		if(irType->IsRecord()) {
			result_ = tempVariableRef;
		}
		else LoadSimpleValue(tempVariableRef, op->ResultType());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::GenerateConditionalOptimized(const ConditionalOperator *op) {
    // If the condition expression is a constant we can select the result
    // without using any jumps to blocks.
    StatementGenerator statementGen(functGen_);

    if(statementGen.IsAlwaysTrue(op->Condition())) {
        Generate(op->Left());
        return true;
    }
    else if(statementGen.IsAlwaysFalse(op->Condition())) {
        Generate(op->Right());
        return true;
    }

    // If both the true and false expression are constants
    // emit a 'question' instruction, helps reducing the number of blocks.
    IR::Operand* leftConst;
    IR::Operand* rightConst;
    IR::Operand* conditionOp;

    if(GenerateConstant(op->Left())) {
        leftConst = result_;

        if(GenerateConstant(op->Right())) {
            rightConst = result_;

            // Generate 'quest conditionOp, leftConst, rightConst'.
            Visit(op->Condition());
            conditionOp = result_;

            auto irResultType = GetIRType(op->ResultType());
            result_ = irGen_->GetTemporary(irResultType);
            irGen_->GetQuestion(conditionOp, leftConst, rightConst, 
                                result_, activeBlock_);
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateConditionalBlock(IR::Block* block, 
												   const Expression* expr,
												   IR::Block* contBlock,	
												   IR::VariableReference* tempVariableRef) {
	functGen_->InsertAndMakeActive(block);
	activeBlock_ = block; // The active block has changed.
	auto trueSourceOp = Generate(expr);

	// Store the result of the expression in a temporary variable, if needed.
	// Assignments involving records are treated separately.
	if(tempVariableRef) {
		if(expr->ResultType()->WithoutQualifiers()->IsRecord()) {
			GenerateRecordAssignment(tempVariableRef, trueSourceOp, 
                                     expr->ResultType());
		}
		else { 
            functGen_->BeforeStore(tempVariableRef, currentExpr_, 
                                   nullptr, expr->ResultType());

            auto storeInstr = irGen_->GetStore(tempVariableRef, trueSourceOp, 
                                               activeBlock_);
            
            functGen_->AfterStore(storeInstr, currentExpr_, 
                                  nullptr, expr->ResultType());
        }
	}
	
	// Connect the block with its continuation.
	irGen_->GetGoto(contBlock, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const CompoundExpression* expr) {
	// Compound expressions are generated as if they were 
    // ordinary variables. For example, '(int[]){1,2,3}' 
    // produces a variable like 'var #compound [3 int32]' 
    // and code to initialize it in the current block.
	auto irType = GetIRType(expr->ResultType());
    string name = functGen_->GetNameGen()->GetCompoundName();
	auto compoundVariableRef = functGen_->AddVariable(irType, name);

	// Apply the initializer to the variable.
	functGen_->InitializeVariable(compoundVariableRef->GetVariable(), 
                                  expr->InitList(), expr->ResultType(), 
                                  activeBlock_);

	// The result of the expression is the reference 
    // to the variable.
	result_ = compoundVariableRef;
}

} // namespace IRGenerator