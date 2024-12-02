// VLAGenerator.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionGenerator.hpp"
#include "StatementGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

void FunctionGenerator::GenerateVLATypedef(const TypedefDeclaration* typedefDecl) {
	// We have something like 'typedef int ABC[E];', where 'E' is not a constant.
	// Generate code that computes expression 'E' and store the operand
	// with the result in a map, so it can be used by a future variable declaration
	// that uses this 'typedef' (for example, 'ABC someArray;').
	auto type = typedefDecl->DeclarationType()->InnerType()->WithoutQualifiers();

	// Skip all pointers until we get to the array.
	while(auto pointerType = type->As<PointerType>()) {
		type = pointerType->PointeeType();
	}

	// Generate the expression and multiply it with the size of the element.
	auto vlaType = type->As<ArrayType>();
	DebugValidator::IsNotNull(vlaType);

	auto elementType = GetArrayInnerType(vlaType)->WithoutQualifiers();
	auto vlaSizeOp = GenerateVLASize(vlaType, elementType);

	AddVLATypedef(VLATypedefInfo(vlaSizeOp, vlaType));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::AddVLAVariable(const VariableDeclaration* variableDecl) {
	// A variable that has VLA type needs to allocate space from the stack, at runtime.
	// The first time such a variable is added in the current context we create
	// a variable (with 'int8*' type) that holds the address of the top of the stack.
	// This is used to restore the state of the stack when the context is destroyed.
	auto variableType = variableDecl->DeclarationType()->WithoutQualifiers();

	// Determine the type of the IR variable and create it. For example,
	// 'int a[n][m];' generates 'var a int32*'.
	// Note that it's possible that we have a 'pointer-to-VLA' (like 'int (*p)[n][m]'),
	// in which case we count how many pointer types are before the VLA is reached.
	int pointerCount = 0;
	const PointerType* pointerType = variableType->As<PointerType>();
	const ArrayType* vlaType =  variableType->As<ArrayType>();
	
	while(pointerType) {
		pointerCount++;
		auto pointeeType = pointerType->PointeeType()->WithoutQualifiers();
		pointerType = pointeeType->As<PointerType>();

		if(pointerType == nullptr) {
			vlaType = pointeeType->As<ArrayType>();
		}
	}

	// A VLA is replaced by a pointer to the base element type.
	auto elementType = GetArrayInnerType(vlaType)->WithoutQualifiers();
	auto irElemType = GetIRType(elementType);
	auto irElemPtrType = irGen_.GetPointer(irElemType);

	if(pointerCount > 0) {
		// Pointer to VLA case; no space needs to be allocated.
		GenerateVLAPointer(variableDecl, pointerCount, vlaType, irElemPtrType);
		return;
	}

	// Generate the variable and get a reference to it, and store into it
	// the address of the first element.
	VLAContext* vlaContext = GetOrCreateVLAContext();
	auto vlaVariableRef = AddVariable(irElemPtrType, variableDecl->Name()->Name());
	auto vlaVariable = vlaVariableRef->GetVariable();

	// If this is the first variable in the context we can use the operand in which
	// the address of the top of the stack was returned (when the context was created).
	// Else call the 'stackTop' intrinsic to obtain the current stack top address:
	IR::Operand* vlaBaseAddrOp;

	if(vlaContext->VLACount == 0) {
		vlaBaseAddrOp = vlaContext->StackTopOp;
	}
	else {
		// Generate 'vlaBaseAddrOp = call intrinsic stackTop' in this case.
		vlaBaseAddrOp = irGen_.GetTemporary(irGen_.GetInt8Pointer());
		irGen_.GetStackTopCall(vlaBaseAddrOp, activeBlock_);
	}

	// The base address has type 'int8*', convert it to the type of the array.
	// 't1 = ptop stackTopOp, irElemPtrType     store vlaVariableRef, t1'.
	auto vlaBaseConvAddrOp = irGen_.GetTemporary(irElemPtrType);
	irGen_.GetPtop(vlaBaseAddrOp, irElemPtrType, vlaBaseConvAddrOp, activeBlock_);
	irGen_.GetStore(vlaVariableRef, vlaBaseConvAddrOp, activeBlock_);

	// Generate the expression that represents the size of the VLA and increment
	// the pointer that indicates the top of the stack.
	auto byteSizeOp = GenerateVLASize(vlaType);
	UpdateNextVLAAddress(byteSizeOp);

	// Add the variable to the map; increase the number of VLAs in the context.
	vlaVariables_.Add(variableDecl, VLAInfo(byteSizeOp, vlaVariable));
	localVars_.Add(variableDecl, vlaVariable);
	vlaContext->VLACount++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::GenerateVLAPointer(const VariableDeclaration* variableDecl,
                                           int pointerCount, const ArrayType* vlaType, 
										   const IR::Type* irElemPtrType) {
	// If we have a pointer to VLA no space needs to be allocated.
	// Just generate the size expressions and create the variable.
	// Generate the type of the IR variable.
	auto irVarType = typeGen_->GetIRArrayInnerType(variableDecl->DeclarationType());
	auto vlaVariableRef = AddVariable(irVarType, variableDecl->Name()->Name());
	auto irVariable = vlaVariableRef->GetVariable();

	localVars_.Add(variableDecl, irVariable);
	GenerateVLASize(vlaType);

	// A pointer to VLA could be initialized with an address.
	if(variableDecl->Initializer()) {
		// Generate code for the initializer store it into the variable.
		auto initOp = ExpressionGenerator(this).Generate(variableDecl->Initializer());
		irGen_.GetStore(vlaVariableRef, initOp, activeBlock_);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* FunctionGenerator::GenerateVLASize(const ArrayType* vlaType, 
												bool addToMap, bool searchMap) {
	// For 'int a[E1][E2]...[En];' the size of the allocated array is
	// 'E1 * E2 * ... * En * sizeof(int)' bytes.
	return GenerateVLASizeImpl(vlaType, addToMap, searchMap);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* FunctionGenerator::GenerateVLASizeImpl(const Type* type, bool addToMap, 
                                                    bool searchMap) {
	IR::Operand* elemSizeOp = nullptr;
	IR::Operand* elemNumOp = nullptr;
	const Type* sizeExprType;

	if(HasVLATypedef(type)) {
		// If the variable array originated from a 'typedef' the size has been
		// already computed. It's a requirement of the C standard 
		// that the size from where the 'typedef' was declared to be considered.
		return GetVLATypedef(type).SizeOperand;
	}
	else if(auto vlaArrayType = type->As<VarArrayType>()) {
		elemSizeOp = GenerateVLASizeImpl(vlaArrayType->ElementType()->WithoutQualifiers(),
										 addToMap, searchMap);

		// If we're allowed, see if the computation has been already done.
		sizeExprType = vlaArrayType->SizeExpression()->ResultType()->WithoutQualifiers();

		if(searchMap && HasVLASizeExprOperand(vlaArrayType)) {
			elemNumOp = GetVLASizeExprOperand(vlaArrayType);
		}
		else {
            auto sizeExpr = vlaArrayType->SizeExpression();
            elemNumOp = ExpressionGenerator(this).Generate(sizeExpr);
        }

		// Add the computed value to the map, if requested.
		if(addToMap) {
            AddVLASizeExprOperand(vlaArrayType, elemNumOp);
        }
	}
	else if(auto arrayType = type->As<ArrayType>()) {
		// The size is a compile-time constant.
		elemSizeOp = GenerateVLASizeImpl(arrayType->ElementType()->WithoutQualifiers(),
										 addToMap, searchMap);
		sizeExprType = BasicType::GetInt();
		elemNumOp = irGen_.GetIntConst(GetIRType(sizeExprType), arrayType->Size());
	}
	else {
		// This is the base type of the VLA; no other type can follow.
		auto irType = type->IsInteger() ? GetIRType(type) : 
                                          GetIRType(BasicType::GetSizeT());
		__int64 size = TypeSize(type, context_).Size();
		elemNumOp = irGen_.GetIntConst(irType, size);
	}

	// If the type was an array, multiply the number of elements by the element size.
	if(elemSizeOp) {
		// We conservatively use 'int64'.
        auto commonType = IR::IntegerType::GetInt64();

		// Cast the operands to the common type.		
        if(elemSizeOp->GetType() != commonType) {
			auto castOp = irGen_.GetTemporary(commonType);
			irGen_.GetZext(elemSizeOp, commonType, castOp, activeBlock_);
			elemSizeOp = castOp;
		}

        if(elemNumOp->GetType() != commonType) {
			auto castOp = irGen_.GetTemporary(commonType);
			irGen_.GetZext(elemNumOp, commonType, castOp, activeBlock_);
			elemNumOp = castOp;
		}

		auto newSizeOp = irGen_.GetTemporary(commonType);
		irGen_.GetMul(elemSizeOp, elemNumOp, newSizeOp, activeBlock_);

		AddVLAElemSizeOperand(type->As<ArrayType>(), newSizeOp);
		return newSizeOp;
	}
	else return elemNumOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FunctionGenerator::GetVLAContext(VLAContext** vlaContext) {
	if(HasVLAContext()) {
		auto& context = TopVLAContext();
		auto parentStatement = ParentStatement();

		if(context.Parent == parentStatement) {
			*vlaContext = &context;
			return true;
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VLAContext* FunctionGenerator::GetOrCreateVLAContext() {
	// If the variable has been created, return a reference to it.
	// Else we need to create it now, and initialize it.
	VLAContext* vlaContext = nullptr;

	if(GetVLAContext(&vlaContext)) {
		return vlaContext;
	}

	// Create a new variable to hold the address of the top of the stack
	// when the context has been created. When the context is destroyed
	// it's used to restore the state of the stack.
	auto irPtrType = irGen_.GetInt8Pointer();
	auto vlaTopVariableRef = AddVariable(irPtrType, "#vla_top");

	// Generate the following code in the current block.
	// 'var #vla_top int8*          t1 = load #vla_top
	//  t2 = call intrinsic stackTop     store t1, t2
	auto stackTopOp = irGen_.GetTemporary(irPtrType);
	irGen_.GetStackTopCall(stackTopOp, activeBlock_);
	irGen_.GetStore(vlaTopVariableRef, stackTopOp, activeBlock_);

	// Create the context and push it on the stack.
    auto vlaVariable = vlaTopVariableRef->GetVariable();
	VLAContext context(ParentStatement(), vlaVariable, stackTopOp);
	PushVLAContext(context);

	return &TopVLAContext();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* FunctionGenerator::GetArrayInnerType(const ArrayType* vlaType) {
	return typeGen_->GetArrayInnerType(vlaType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::UpdateNextVLAAddress(IR::Operand* sizeOp) {
	// A vector has been allocated on the stack, now the top of the stack
	// pointer needs to be updated, by increasing it by the allocated size.
	// Make a call to the 'incStackTop' intrinsic with 'sizeOp' as the parameter.
	auto requiredType = IR::IntegerType::GetInt64();

	if(sizeOp->GetType() != requiredType) {
		// The operand has not the right type, insert a 'zext' conversion instruction.
		auto castOp = irGen_.GetTemporary(requiredType);
		irGen_.GetZext(sizeOp, requiredType, castOp, activeBlock_);
		sizeOp = castOp;
	}

	irGen_.GetIncStackTopCall(sizeOp, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::RestoreStackState() {
	DebugValidator::IsTrue(HasVLAContext());
	
	// The current context is destroyed, restore the stack to the state in which
	// it was before any VLA was allocated in the context. Generate the code:
	// 't1 = load #vla_top      call intrinsic restoreStack, t1'
	GenerateStackRestoreCode(TopVLAContext());
	vlaContexts_.Pop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::GenerateStackRestoreCode(VLAContext& vlaContext) {
    // Load the variable that contains the stack pointer
    // at the moment the VLA was generated and restore it
    // doing a call to the 'restoreStackTop' intrinsic.
    auto vlaTopOp = irGen_.GetTemporary(irGen_.GetInt8Pointer());
    auto vlaTopVariableRef = GetVariableReference(vlaContext.TopStackVariable);

    BeforeLoad(vlaTopVariableRef, nullptr);
    auto loadInstr = irGen_.GetLoad(vlaTopVariableRef, vlaTopOp, activeBlock_);
    AfterLoad(loadInstr, nullptr);
    irGen_.GetRestoreStackTopCall(vlaTopOp, activeBlock_);
}

// ######################################################################################
// VLA expressions
// ######################################################################################
void ExpressionGenerator::GenerateVLASizeof(const SizeofOperator* op) {
	auto vlaType = op->Target()->ResultType()->WithoutQualifiers()->As<VarArrayType>();
	DebugValidator::IsNotNull(vlaType);

	if(auto declExpr = op->Target()->As<DeclarationExpression>()) {
		// Use the size computed when the variable was declared ('int a[E]; sizeof(a);').
		auto variableDecl = declExpr->Object()->As<VariableDeclaration>();
		result_ = functGen_->GetVLAInfo(variableDecl).SizeOperand;
	}
	else if(functGen_->HasVLATypedef(vlaType)) {
		// The type originates from a 'typedef' ('typedef int ABC[E]; sizeof(ABC);').
		result_ = functGen_->GetVLATypedef(vlaType).SizeOperand;
	}
	else if(functGen_->HasVLASizeExprOperand(vlaType)) {
		// The size is obtained from an intermediate computation
		// of the final size of the VLA (for example, 'int a[E1][E2]; sizeof(a[2]);').
		result_ = functGen_->GenerateVLASize(vlaType, false /* addToMap */, 
                                             true /* searchMap */);
	}
	else {
		// The size must be computed now (for example, 'sizeof(int [E1])').
		result_ = functGen_->GenerateVLASize(vlaType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* ExpressionGenerator::GenerateVLAOffset(const ArrayType* vlaType, 
												    IR::Operand* indexOp,
													const Type* indexType) {
	// Example: for 'int a[E1][E2][E3]; a[1][2][3] = 5;',
	// the offset is '1*(E2*E3) + 2*E3 + 3'  ->  '(1*E2 + 2) * E3 + 3'.
	// Generate '(elemSizeOp / baseSizeOp) * indexOp'.
	auto elemSizeOp = functGen_->GetVLAElemSizeOperand(vlaType);
	auto elemSizeOpType = elemSizeOp->GetType()->As<IR::IntegerType>();

	// Divide the size of the element by the size of the base type of the VLA.
	auto baseType = functGen_->GetArrayInnerType(vlaType);
	__int64 baseSize = TypeSize(baseType, functGen_->GetContext()).Size();
	auto baseSizeConst = irGen_->GetIntConst(elemSizeOpType, baseSize);
	
	auto elemIndexOp = irGen_->GetTemporary(elemSizeOpType);
	irGen_->GetUdiv(elemSizeOp, baseSizeConst, elemIndexOp, activeBlock_);

	// Multiply the index by the offset, if provided.
	if(indexOp) {
		// Make sure 'indexOp' has the type of 'elemSizeOp'.
		auto mulOp = irGen_->GetTemporary(elemSizeOpType);
		GenerateIntToIntCast(indexOp, elemSizeOpType, indexType);

		irGen_->GetMul(elemIndexOp, indexOp, mulOp, activeBlock_);
		return mulOp;
	}
	
    return elemIndexOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* ExpressionGenerator::GenerateNegatedOperand(IR::Operand* op) {
	// Negate the offset by subtracting from 0.
	auto zeroConst = irGen_->GetIntConst(op->GetType(), 0);
	auto negConst = irGen_->GetTemporary(op->GetType());

	irGen_->GetSub(zeroConst, op, negConst, activeBlock_);
	return negConst;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateVLAPointerIncDec(const UnaryOperator* op, 
                                                   IR::Operand* targetOp,
												   const ArrayType* vlaType) {
	bool isDecrement = op->Operator() == UnaryOpType::Dec;
	bool isPostfix = op->IsPostfix();
	auto innerType = functGen_->GetArrayInnerType(vlaType);
	auto irResultType = irGen_->GetPointer(GetIRType(innerType));

	// Load the value.
	// t1 - value before, t2 - value after
	auto t1 = irGen_->GetTemporary(irResultType);
    functGen_->BeforeLoad(targetOp, currentExpr_);
	
    auto loadInstr = irGen_->GetLoad(targetOp, t1, activeBlock_);
    SetVolatile(loadInstr, op->Value()->ResultType(), false /* testPointer */);
   
    functGen_->AfterLoad(loadInstr, currentExpr_);

	// Generate the instruction to increment/decrement. Adjust by the size of the VLA,
	// divided by the size of the inner type.
	auto offsetOp = GenerateVLAOffset(vlaType);

	if(isDecrement) {
		// If we're decrementing negate the operand.
		offsetOp = GenerateNegatedOperand(offsetOp);
	}

	// Store the result and select the taken value.
	auto t2 = irGen_->GetTemporary(irResultType);
	irGen_->GetAddress(t1, offsetOp, t2, activeBlock_);

    functGen_->BeforeStore(targetOp, currentExpr_);
	auto storeInstr = irGen_->GetStore(targetOp, t2, activeBlock_);
    SetVolatile(storeInstr, op->Value()->ResultType(), false /* testPointer */);

    functGen_->AfterStore(storeInstr, currentExpr_);
	result_ = isPostfix ? t1 : t2;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateVLAPointerIntCommon(const BinaryOperator* op, 
                          IR::Operand* leftOp, IR::Operand* rightOp, 
                          const Type* intType, const ArrayType* vlaType, 
						  IR::Operand*& baseOp, IR::Operand*& offsetOp) {
	auto leftType = op->LeftValue()->ResultType()->WithoutQualifiers();

	if(leftType == intType) {
		// The adjustment value is in 'leftOp'.
		offsetOp = GenerateVLAOffset(vlaType, leftOp, intType);
		baseOp = rightOp;
	}
	else {
		offsetOp = GenerateVLAOffset(vlaType, rightOp, intType);
		baseOp = leftOp;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateVLAPointerIntAdd(const BinaryOperator* op, 
                          IR::Operand* leftOp, IR::Operand* rightOp, 
                          const Type* pointerType, const Type* intType, 
                          const Type* resultType, const ArrayType* vlaType) {   
	// Generate the offset from the base of the VLA.
	IR::Operand* baseOp;
	IR::Operand* offsetOp;
	GenerateVLAPointerIntCommon(op, leftOp, rightOp, intType, 
                                vlaType, baseOp, offsetOp);

	// Generate the new address.
	auto irResultType = GetIRType(resultType);
	result_ = irGen_->GetTemporary(irResultType);
	irGen_->GetAddress(baseOp, offsetOp, result_, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateVLAPointerIntSub(const BinaryOperator* op, 
                          IR::Operand* leftOp, IR::Operand* rightOp, 
                          const Type* pointerType, const Type* intType, 
                          const Type* resultType, const ArrayType* vlaType) {   
	// Generate the offset from the base of the VLA.
	IR::Operand* baseOp;
	IR::Operand* offsetOp;
	GenerateVLAPointerIntCommon(op, leftOp, rightOp, intType, 
                                vlaType, baseOp, offsetOp);
	
	// Because this is a subtraction the offset operand needs to be negated.
	offsetOp = GenerateNegatedOperand(offsetOp);

	// Generate the new address.
	auto irResultType = GetIRType(resultType);
	result_ = irGen_->GetTemporary(irResultType);
	irGen_->GetAddress(baseOp, offsetOp, result_, activeBlock_);
}

} // namespace IRGenerator