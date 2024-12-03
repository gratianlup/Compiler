// FunctionGenerator.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"
#include "../AST/TypeSize.hpp"
#include "../IR/FlowDotPrinter.hpp"

namespace IRGenerator {

FunctionGenerator::FunctionGenerator(UnitGenerator* unitGen) :
		irUnit_(unitGen->GetIRUnit()), 
        context_(unitGen->GetContext()),
		target_(unitGen->GetTarget()), 
        typeGen_(unitGen->GetTypeGen()),
		layouts_(unitGen->GetLayouts()), 
        unitGen_(unitGen), 
        unit_(unitGen->GetUnit()),
		irGen_(unitGen->GetIRUnit(), 
        unitGen->GetContext()->Options().ShouldNameTemporaries()),
		namedTemp_(unitGen->GetContext()->Options().ShouldNameTemporaries()) {
	DebugValidator::IsNotNull(unitGen);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::Generate(const FunctionDeclaration* function, 
                                 IR::Function* irFunct,
								 List<ExpandedField>& expandedFields) {
	DebugValidator::IsNotNull(function);
	DebugValidator::IsNotNull(irFunct);
	DebugValidator::IsTrue(function->IsDefinition());
	
	funct_ = function;
	irFunct_ = irFunct;

    // Create the entry block; this block shall exist
    // even if the function is empty.
    InsertAndMakeActive(CreateBlock("#entry"));

	// If the function has records as parameters it's possible 
    // that the fields were/ expanded into individual parameters. 
    // We need to add all these fields into a map, so that when a field 
    // is accessed the associated parameter is returned.
	for(int i = 0; i < expandedFields.Count(); i++) {
		expandedFields_.Add(expandedFields[i],
                            expandedFields[i].Parameter);
	}

	// Add the parameters to the map with local variables.
	List<VariableDeclaration*> expandedParams;
	GenerateParameters(expandedFields, expandedParams);
	
	// Initialize the expanded parameters, if there are any.
	InitializeExpandedParameters(expandedParams);

    // Notify the observers that the function 
    // generation process starts now.
    if(auto events = unitGen_->GetEvents()) {
        events->FunctionBegin(GetGeneratorContext());
    }

	// Generate code for each component of the body 
    // (statement/expression/declaration).
	StatementGenerator statementGen(this);
	statementGen.Generate(funct_->Body());

	// Make sure there is a branching instruction 
    // in the last block. This may introduce dead code, but it's
    // cleaned afterwards by the optimizer.
	InsertLastReturn();

    // Notify the observers that the function was generated.
    if(auto events = unitGen_->GetEvents()) {
        events->FunctionEnd(GetGeneratorContext());
    }

#if 0
	IR::FlowDotPrinter("d:\\flow.dot", true, false).Print(irFunct_);
#endif
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InsertLastReturn() {
	// If no branching instruction is found at the end 
    // of the last block add a 'ret' now.
	IR::Block* lastBlock = ActiveBlock();
	auto returnType = funct_->DeclarationType()->ReturnType()
                                               ->WithoutQualifiers();
	
	if(lastBlock->LastInstruction() && 
       lastBlock->LastInstruction()->IsBranching()) {
		// There is already a branching instruction in the last block.
		return;
	}

	// If the function returns 'void' or a record, insert 
    // a 'ret void' instruction. If it's a simple record return "zero".
	if(returnType->IsVoid() || returnType->IsRecord()) {
		irGen_.GetVoidReturn(lastBlock);
	}
	else {
		// Insert a return with the "undefined" value.
		auto irReturnType = unitGen_->GetTypeGen()->GetType(returnType);
		auto undefConst = irGen_.GetUndefinedConst(irReturnType);
		irGen_.GetReturn(undefConst, lastBlock);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::GenerateParameters(List<ExpandedField>& expandedFields,
										   List<VariableDeclaration*>& expandedParams) {
	// Add every parameter to the local map (so they can be found 
    // when referenced in expressions). Note that parameters 
    // with 'struct' type that have been expanded into parameters 
    // (one for each field) need special treatment.
	int expandedPos = 0;
	int irParamPos = 0;
	auto& parameters = funct_->Parameters();
	auto& irParams = irFunct_->Parameters();

	for(int i = 0; i < parameters.Count(); i++) {
		VariableDeclaration* parameter = parameters[i];

		if(unitGen_->IsExpandableStruct(parameter->DeclarationType())) {
			// This parameter was expanded; skip over the field parameters.
			while((expandedPos < expandedFields.Count()) && 
				  (expandedFields[expandedPos].Variable == parameter)) {
				expandedPos++;
				irParamPos++;
			}

			// A variable having record type needs to be added, 
            // and the expanded fields must be copied into it.
			expandedParams.Add(parameter);
		}
        else if(parameter->DeclarationType()->WithoutQualifiers()->IsRecord()) {
            // This is a large record that should be copied
            // to a local variable before it's used.
            ExpressionGenerator exprGen(this);
            auto recordType = parameter->DeclarationType()->WithoutQualifiers()
                                               ->As<StructUnionType>();
            
            auto slotRef = GetRecordSlot(recordType);
            auto paramRef = GetVariableReference(irParams[irParamPos]);
            auto loadedParam = exprGen.LoadSimpleValue(paramRef);
            exprGen.GenerateRecordAssignment(slotRef, loadedParam, recordType);
            localVars_.Add(parameter, slotRef->GetLocalVariable());
        }
		else {
			// This is a standard parameter.
			localVars_.Add(parameter, irParams[irParamPos]);
			irParamPos++;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InitializeExpandedParameters(List<VariableDeclaration*>& expandedParams) {
	// If there were expanded parameters (with 'struct' type)
	// we need to generate here code that copies the values 
    // of the fields from the parameters into the record. For example:
	// 'void f(struct ABC x)' becomes 
	// 'function f(var x_0 int32, var x_1 int32) : void {
	//		var x <ABC>
	// label entry:
	//		t1 = addr x, 0     t2 = load x_0     store t1, t2
	//		t3 = addr x, 1     t4 = load x_1     store t3, t4 }'
	if(expandedParams.Count() == 0) {
        return;
    }

	ExpressionGenerator exprGen(this);

	for(int i = 0; i < expandedParams.Count(); i++) {
		auto structParam = expandedParams[i];
		AddVariable(structParam, activeBlock_ /* entry */);

		auto structRef = GetVariableReference(structParam);
		exprGen.InitializeExpandedStruct(structRef, structParam);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IR::Type* FunctionGenerator::GetIRType(const Type* type) {
	return typeGen_->GetType(type, GetIRFunction());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FunctionGenerator::IsSimpleArray(const ArrayType* arrayType, 
                                      InitializerListExpression* initList) {
	// An array is considered "simple" if it is initialized with few values,
	// and if the element type is a record it should also be "simple".
	if(GetInitListValueCount(initList) > AGGREGATE_CONSTANT_LIMIT) {
        return false;
    }
	
	auto elementType = arrayType->ElementType()->WithoutQualifiers();

	if(auto recordElemType = elementType->As<StructUnionType>()) {
		return unitGen_->IsSimpleRecord(recordElemType, AGGREGATE_CONSTANT_LIMIT,
										SIMPLE_RECORD_MAX_LEVELS);
	}
	else return true; // For all other types.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FunctionGenerator::HasCompoundExpression(const Expression* expr) {
	if(expr->IsCompoundExpr()) {
		// Found a compound literal expression!
		return true;
	}
	else if(auto temp = expr->As<UnaryOperator>()) {
		return HasCompoundExpression(temp->Value());
	}
	else if(auto temp = expr->As<BinaryOperator>()) {
		bool hasInLeft = HasCompoundExpression(temp->LeftValue());

		if(hasInLeft) {
            return true;
        }
		else return HasCompoundExpression(temp->RightValue());
	}
	else if(auto temp = expr->As<CastExpression>()) {
		return HasCompoundExpression(temp->Target());
	}
	else if(auto temp = expr->As<SubscriptExpression>()) {
		bool hasInBase = HasCompoundExpression(temp->Base());
		
        if(hasInBase) {
            return true;
        }
		else return HasCompoundExpression(temp->Index());
	}
	else if(auto temp = expr->As<MemberExpression>()) {
		return HasCompoundExpression(temp->Object());
	}
	else if(auto temp = expr->As<InitializerListExpression>()) {
		auto& initList = temp->InitList();

		for(int i = 0; i < initList.Count(); i++) {
			if(HasCompoundExpression(initList[i])) {
                return true;
            }
		}
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* FunctionGenerator::GetZeroConstant(const IR::Type* type) {
	if(type->IsInteger()) {
        return irGen_.GetIntConst(type, 0);
    }
	else if(type->IsFloat()) {
        return irGen_.GetFloatConst(0);
    }
	else if(type->IsDouble()) {
        return irGen_.GetDoubleConst(0);
    }
	else return irGen_.GetNullConst(type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* FunctionGenerator::GetConstant(const IR::Type* type, const Expression* value, 
                                            IR::Block* block) {
	EvaluationInfo eval = value->Evaluate(context_, false /* warn */);

	if(type->IsInteger()) {
        return irGen_.GetIntConst(type, eval.IntValue());
    }
	else if(type->IsFloat()) {
        return irGen_.GetFloatConst(eval.FloatValue());
    }
	else if(type->IsDouble()) {
        return irGen_.GetDoubleConst(eval.FloatValue());
    }
	else { 
		if(auto stringConst = value->As<StringConstant>()) {
			// This is a pointer initialized with a string.
			return GeneratePointerToString(stringConst, type, block);
		}
		else return irGen_.GetNullConst(type);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int FunctionGenerator::GetInitListValueCount(InitializerListExpression* initList) {
	DebugValidator::IsNotNull(initList);
	int count = 0;

	for(int i = 0; i < initList->Count(); i++) {
		Expression* item = initList->InitList()[i];

		if(auto nestedList = item->As<InitializerListExpression>()) {
			count += GetInitListValueCount(nestedList);
		}
		else count++;
	}

	return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InitializeAggregate(IR::Operand* destOp, const Type* destType, 
                                            InitializerListExpression* initList,
                                            FieldInitializerType initType,
											IR::Block* block, 
                                            ExpressionGenerator* exprGen) {
	if(auto arrayType = destType->As<ArrayType>()) {
		auto elementType = arrayType->ElementType()->WithoutQualifiers();
		auto irElemType = typeGen_->GetType(elementType);
		auto irElemPtrType = irGen_.GetPointer(irElemType);

		// Generate code for each element of the array.
		for(__int64 i = 0; i < arrayType->Size(); i++) {
			// Generate code to initialize the field.
			auto elemAddr = GenerateElementAddress(i, destOp, irElemPtrType, 
                                                   block, false);
			InitializeElement(elemAddr, i, elementType, irElemType, 
                              initList, initType, block, exprGen);
		}

		return;
	}

	// This is a 'struct'/'union' otherwise.
	auto recordType = destType->As<StructUnionType>();
	DebugValidator::IsNotNull(recordType);

	// If we have an 'union' only it's first element needs to be initialized.
	int initLimit = recordType->IsUnion() ? 1 : recordType->FieldCount();
	int unnamedBitfields = 0;

	for(int i = 0; i < initLimit; i++) {
		auto field = recordType->Fields()[i];
		auto fieldType = field->DeclarationType()->WithoutQualifiers();
		auto irFieldType = typeGen_->GetType(fieldType);
		auto irFieldPtrType = irGen_.GetPointer(irFieldType);

		// Generate code to initialize the field.  Note that 'initIndex' must take 
		// into account unnamed bitfields, who have no initializers.
		auto elemAddr = GenerateElementAddress(i, destOp, irFieldPtrType,
                                               block, true);
		int initIndex = i - unnamedBitfields;

		// If it's a bitfield special code needs to be generated.
		if(field->IsBitfield() == false) {
			// The field is not a bitfield.
			InitializeElement(elemAddr, initIndex, fieldType, irFieldType, 
                              initList, initType, block, exprGen);
		}
		else {
			// Unnamed bitfields need to be skipped.
			if(field->IsUnnamedBitfield()) {
				unnamedBitfields++;
				continue;
			}

			DebugValidator::IsNotNull(exprGen);
			auto value = exprGen->Generate(initList->InitList()[initIndex]);
            auto layout = layouts_->GetOrCreate(recordType);
			auto fieldInfo = layout->GetFieldInfo(field->Name());

			exprGen->StoreToBitfield(elemAddr, value, 
                                     field, fieldInfo);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* FunctionGenerator::GenerateElementAddress(__int64 index, IR::Operand* baseAddr,
													   const IR::Type* irElemPtrType, 
													   IR::Block* block, bool onRecord) {
	auto elemAddr = irGen_.GetTemporary(irElemPtrType);

    // Try to use an 'int32' for the index constant.
    auto elemIndexConst = index < 0x7FFFFFFF ? irGen_.GetInt32Const(index) :
                                               irGen_.GetInt64Const(index);

	// If the base is a record use the 'field' instruction, else 'index'.
	if(onRecord) irGen_.GetField(baseAddr, elemIndexConst, elemAddr, block);
	else irGen_.GetIndex(baseAddr, elemIndexConst, elemAddr, block);

	return elemAddr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InitializeElement(IR::Operand* elemAddr, __int64 index, 
										  const Type* elementType, 
										  const IR::Type* irElemType, 
                                          InitializerListExpression* initList,
										  FieldInitializerType initType, 
                                          IR::Block* block,
										  ExpressionGenerator* exprGen) {
	// We need to check if the element to initialize is an aggregate.
	// If it is let 'InitAggregateWithConst' handle it.
	auto initExpr = initList->InitList()[(int)index];
    bool handled = false;
    IR::Operand* value;

	if(auto nestedInitList = initExpr->As<InitializerListExpression>()) {
		// The element to initialize has aggregate type.
		DebugValidator::IsTrue(elementType->IsAggregate());
		InitializeAggregate(elemAddr, elementType, nestedInitList,
                            initType, block, exprGen);
	}
	else {
		// The destination has basic or pointer type.
		if(initType == FieldInitializerType::Zero) {
			value = GetZeroConstant(irElemType);
		}
		else if(initType == FieldInitializerType::Constant) {
			// Here we can have 4 cases:
			// - array initialized with a string: 'char a[] = "abc";'
			// - pointer initialized with a string: 'char* p = "abc";'
            // - pointer initialized with the address of a variable: '&var'
			// - a simple constant.
			if(auto stringConst = initExpr->As<StringConstant>()) {
				if(auto arrayType = elementType->As<ArrayType>()) {
					InitArrayWithString(elemAddr, arrayType, 
                                        stringConst, block);
					return;
				}
			}
            else if(auto unaryOp = initExpr->As<UnaryOperator>()) {
                if(unaryOp->Operator() == UnaryOpType::Address) {
                    DebugValidator::IsNotNull(exprGen);
			        value = exprGen->Generate(initExpr);
                    handled = true;
                }
            }
			
            if(handled == false) {
			    value = GetConstant(irElemType, initExpr, block);
            }
		}
		else {
			// Generate code that emits the value that is not a constant.
			DebugValidator::IsNotNull(exprGen);
			value = exprGen->Generate(initExpr);
		}

		// Now store the value.
		irGen_.GetStore(elemAddr, value, block);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InitLargeAggregateWithZero(IR::VariableReference* variableRef, 
												   InitializerListExpression* initList, 
												   const Type* destType,
                                                   bool withZero, 
												   IR::Block* block) {
	// Make a cast to 'int8*' and call the 'setMemory' intrinsic.
	auto memsetType = irGen_.GetPointer(IR::IntegerType::GetInt8());
	auto castOp = irGen_.GetTemporary(memsetType);
	irGen_.GetPtop(variableRef, memsetType, castOp, block);

	auto lengthOp = irGen_.GetInt64Const(TypeSize(destType, context_).Size());
	auto valueOp = irGen_.GetInt8Const(0);
	
	// Call set memory with value 0.
	// funct setMemory(var dest int8*, var value int8, var len int64) : void
	irGen_.GetSetMemoryCall(castOp, valueOp, lengthOp, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InitLargeAggregateWithConst(IR::Variable* irVariable, 
													InitializerListExpression* initList, 
													const Type* destType,
													IR::Block* block) {
	// Initialization with a large list. Make a global variable initialized
	// appropriately and make a call to 'copyMemory' to copy the values to the stack.
	auto variableRef = GetVariableReference(irVariable);
	auto constVariableRef = unitGen_->AddConstant(irVariable, initList, irFunct_);
	CopyGlobalToStack(variableRef, constVariableRef, destType, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::CopyGlobalToStack(IR::Operand* destOp, 
                                          IR::Operand* sourceOp,
										  const Type* destType, 
                                          IR::Block* block) {
	// For 'int a[5] = {1,2,3,4,5}' we generate, at the function level:
	// dest = ptop variableRef, int8*     
	// src  = ptop constVariableRef,  int8*
	// call intrinsic copyMemory, dst2, src2, 20
	auto copyMemType = irUnit_->Types().GetPointer(IR::IntegerType::GetInt8());
	
	// Cast the addresses to int8* (required by the intrinsic).
	auto destCastOp = irGen_.GetTemporary(copyMemType);
	irGen_.GetPtop(destOp, copyMemType, destCastOp, block);
	auto srcCastOp = irGen_.GetTemporary(copyMemType);
	irGen_.GetPtop(sourceOp, copyMemType, srcCastOp, block);
	auto lengthOp = irGen_.GetInt64Const(TypeSize(destType, context_).Size());

	// Create the operand with the size of the data to copy.
	// funct copyMemory(var dest int8*, var src int8*, var len int64) : void
	irGen_.GetCopyMemoryCall(destCastOp, srcCastOp, lengthOp, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InitializeWithConstant(IR::Variable* irVariable, 
                                               InitializerListExpression* initList, 
											   const Type* destType, 
                                               bool withZero, 
											   IR::Block* block) {
	// If there are at most 'AGGREGATE_CONSTANT_LIMIT' values 
    // that must be set we emit the instructions. Note that we do this 
    // optimization only for simple types. Else we emit a call to 
    // the 'setMemory' or 'copyMemory' intrinsic.
	auto variableRef = GetVariableReference(irVariable);
	FieldInitializerType initType = withZero ? FieldInitializerType::Zero :
                                               FieldInitializerType::Constant;
    ExpressionGenerator exprGen(this);
    bool handled = false;

	if(auto arrayType = destType->As<ArrayType>()) {
		// The array can have almost any element type, as long as the number
		// of values to initialize doesn't exceed the limit.
		if(IsSimpleArray(arrayType, initList)) {
			handled = true;
			InitializeAggregate(variableRef, arrayType, initList,
                                initType, block, &exprGen);
		}
	}
	else if(auto recordType = destType->As<StructUnionType>()) {
		// The total number of fields in the record (this includes
        // any nested record) should not exceed the limit. Also, 
        // the fields should not have array or function type or be bitfields.
		if(unitGen_->IsSimpleRecord(recordType, AGGREGATE_CONSTANT_LIMIT, 
									SIMPLE_RECORD_MAX_LEVELS)) {
			handled = true;
			InitializeAggregate(variableRef, recordType, initList,
                                initType, block, &exprGen);
		}
	}

	if(handled == false) {
		if(withZero) {
			InitLargeAggregateWithZero(variableRef, initList, destType,  
                                   	   withZero, block);
		}
		else InitLargeAggregateWithConst(irVariable, initList, destType, block);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InitializeVariable(IR::Variable* irVariable, 
                                           Expression* initializer, 
										   const Type* destType, 
                                           IR::Block* block) {
	// Use the version without qualifiers.
	destType = destType->WithoutQualifiers();

	if(auto initList = initializer->As<InitializerListExpression>()) {
		// We try to optimize for constant and zero initializers.
		// Arrays and structures with at most 8 members are 
        // initialized directly. For all other cases a global variable 
        // with the values from the initializer is created and the 
        // 'copyMemory' intrinsic is used to copy it to the stack.
		if(initList->IsAllZero(context_)) {
			InitializeWithConstant(irVariable, initList, destType, 
                                   true /* withZero */, block);
		}
		else if(initList->IsAllConstant(context_) && 
			    (HasCompoundExpression(initList) == false)) {
			InitializeWithConstant(irVariable, initList, destType, 
                                   false /* withZero */, block);
		}
		else {
			// At least one of the items in the initializer list 
            // is not constant. In this case we need to emit code 
            // that computes the values.
			auto variableRef = GetVariableReference(irVariable);
			ExpressionGenerator exprGen(this);

			InitializeAggregate(variableRef, destType, initList,
                                FieldInitializerType::Variable, block, &exprGen);
		}
	}
	else if(auto arrayType = destType->As<ArrayType>()) {
		// We have an array initialized with a string 
        // ('char a[] = "test";', for example).
		auto stringConst = initializer->As<StringConstant>();
		DebugValidator::IsNotNull(stringConst);

		auto irVariableRef = GetVariableReference(irVariable);
		InitArrayWithString(irVariableRef, arrayType, stringConst, block);
	}
    else if(destType->IsStruct() || destType->IsUnion()) {
        // A 'struct'/'union' initialized with the result of an expression
        // (a function call, for example).
        ExpressionGenerator exprGen(this);
        auto irVariableRef = GetVariableReference(irVariable);

        if(initializer->WithoutCasts()->IsCallExpr()) {
            exprGen.SetRecordReturnTarget(irVariableRef);
            exprGen.Generate(initializer);
        }
        else {
            auto expr = exprGen.Generate(initializer);
            exprGen.GenerateRecordAssignment(irVariableRef, expr, destType);
        }
    }
	else {
		// The variable is simple (not an array or record).
        // Generate the instruction that initializes it. 
        // For example, 'int a = 5;'  ->  'var a int32   store a, 5'.
		if(auto stringConst = initializer->As<StringConstant>()) {
			// This must be a pointer initialized by a string constant.
			// A global constant with the string will be generated, and it has
			// '[N intType]' type, where 'intType' is 'int8' for 'char'.
			auto irPtrType = GetIRType(destType);
			auto stringConstAddr = GeneratePointerToString(initializer, irPtrType, block);

			auto irVariableRef = GetVariableReference(irVariable);
			irGen_.GetStore(irVariableRef, stringConstAddr, block);
		}
		else ExpressionGenerator(this).StoreInitializer(irVariable, initializer);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* FunctionGenerator::GeneratePointerToString(const Expression* stringConst, 
														const IR::Type* destType, 
														IR::Block* block) {
    ExpressionGenerator exprGen(this);
	auto stringConstRef = exprGen.Generate(stringConst);

	// We need to cast from the array type to 'pointer to char' ('int8*').
	auto indexConst = irGen_.GetInt32Const(0);
	auto elemAddrOp = irGen_.GetTemporary(destType);

	irGen_.GetIndex(stringConstRef, indexConst, elemAddrOp, block);
	return elemAddrOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InitArrayWithString(IR::Operand* destOp, 
											const ArrayType* arrayType, 
											const StringConstant* stringConst, 
											IR::Block* block) {
	// If it's a small string initialize each element of the array,
	// else generate a global string constant and use the 'copyMemory' intrinsic.
	auto elementType = arrayType->ElementType()->WithoutQualifiers();
	auto irElemType = GetIRType(elementType);
	auto irElemPtrType = irGen_.GetPointer(irElemType);

	if(arrayType->Size() <= AGGREGATE_CONSTANT_LIMIT) {
		// If an array is larger than the string, the rest of the elements
		// shall be initialized with zero (the '\0' character).
		// 'char a[6] = "abc"' - 'a[3]'-'a[5]' = '\0'.
		auto stringValue = stringConst->Value().Value;
		int stringLimit = stringValue.Length();
		int arraySize = (int)arrayType->Size();
		
		for(int i = 0; i < arraySize; i++) {
			IR::Operand* elemValue;
			auto elemAddrOp = GenerateElementAddress(i, destOp, irElemPtrType, 
													 activeBlock_, false /* onRecord */);

			// Select the value to use (character from the string or '\0').
			if(i < stringLimit) {
				elemValue = irGen_.GetIntConst(irElemType, stringValue[i]);
			}
			else elemValue = irGen_.GetIntConst(irElemType, 0);

			// Store the value into the generated element address.
			irGen_.GetStore(elemAddrOp, elemValue, activeBlock_);
		}
	}
	else {
		// Generate a global variable with the string. We need to copy
        // from this variable to the stack, using the 'copyMemory' intrinsic.
		auto stringConstRef = AddStringConstant(stringConst, arrayType);
		CopyGlobalToStack(destOp, stringConstRef, 
                          arrayType, block);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
FunctionGenerator::AddStringConstant(const StringConstant* stringConst,
                                     const ArrayType* targetType) {
	// Create a global variable that holds the string constant;
    // We return a reference to it.
	auto symbolTable = &irUnit_->Symbols();
	auto irResultType = targetType ? GetIRType(targetType) : 
									 GetIRType(stringConst->ResultType());
	auto irStrConst = unitGen_->GetStringConstant(stringConst, targetType);
	auto strInit = irGen_.GetInitializer(irStrConst);
	
	// Create an unique name, of the form '#const_functname_str'.
    string name = nameGen_.GetName(symbolTable, "str", *irFunct_->Name(), "const");
	auto stringConstVar = irGen_.GetGlobalSymbol(irResultType, name, strInit, 
											     nullptr /* parent */,
                                                 IR::SymbolVisibility::Static);
	stringConstVar->SetIsConstant(true);
	return unitGen_->AddConstant(stringConstVar);	
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* FunctionGenerator::GetVariableReference(IR::Variable* irVariable) {
	auto irVarPtrType = irUnit_->Types().GetPointer(irVariable->GetType());
	return irUnit_->References().GetVariableRef(irVariable, irVarPtrType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Block* FunctionGenerator::CreateBlock(const string& name, bool inLoop) {
	auto block = IR::Block::GetBlock(name);
    block->SetId(irFunct_->GetNextBlockId());
    block->SetLoopDepth(LoopDepth() + (inLoop ? 1 : 0));
	return block;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Block* FunctionGenerator::CreateBlock(BlockName name, bool inLoop) {
    return CreateBlock(nameGen_.GetBlockName(name), inLoop);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::InsertAndMakeActive(IR::Block* block) {
	DebugValidator::IsNotNull(block);
	
	// The block is inserted on the last position and made active.
	// We first make sure that the name is unique.
	string uniqueName = nameGen_.GetName(&irFunct_->Symbols(), *block->Name());
	block->SetName(new string(uniqueName));
	irFunct_->InsertBlock(block);
	activeBlock_ = block;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::AddVariable(const VariableDeclaration* variable, IR::Block* block) {
	DebugValidator::IsNotNull(variable);
	DebugValidator::IsNotNull(block);
	
	// Create the variable and add it to the function. If the variable is marked
	// 'static' we emit it at global scope, and if it's 'extern' we don't have
	// anything to do. The initializer (if any) is added in the specified block
	// (except when the variable is 'static').
	if(variable->Linkage() == LinkageType::None) {
		// 'auto' variable, the usual case.
		auto irVarType = typeGen_->GetType(variable->DeclarationType());
		string uniqueName = nameGen_.GetName(&irFunct_->Symbols(), 
										     variable->Name()->Name());

		IR::Variable* irVariable = IR::Variable::GetVariable(irVarType, uniqueName);
		irFunct_->AddVariable(irVariable);
		localVars_.Add(variable, irVariable);

		// Create code for the initializer, if any.
		if(variable->Initializer()) {
			InitializeVariable(irVariable, variable->Initializer(), 
                               variable->DeclarationType(), block);
		}

		// Add the attributes, if any. Also add location info, if requested.
		unitGen_->AddAttributes(irVariable, variable);
		unitGen_->AddVariableLocation(irVariable, variable);
        irVariable->SetIsUnsigned(variable->DeclarationType()->IsUnsigned());

		// Mark the variable as 'restrict' if it's the case; may help alias analysis.
		if(auto qualType = variable->DeclarationType()->As<QType>()) {
			if(qualType->HasRestrict()) {
                irVariable->SetIsRestrict(true);
            }
		}
	}
	else if(variable->Linkage() == LinkageType::Internal) {
		// The variable is declared as 'static'.
        // Emit the variable at global scope.
		unitGen_->AddVariable(variable);
	}
	else {
		// The variable is marked with 'extern'.
		// We emit the variable only if it can't be found at global scope.
		// It's found if there was a previous declaration.
		if(unitGen_->HasVariable(variable) == false) {
			return unitGen_->AddVariable(variable);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
FunctionGenerator::AddVariable(const IR::Type* Type, const string& name) {
	// Make sure the name is unique.
    string varName = nameGen_.GetName(&irFunct_->Symbols(), name);
	IR::Variable* irVariable = irGen_.GetVariableSymbol(Type, varName);
    irFunct_->AddVariable(irVariable);
	return GetVariableReference(irVariable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference*
FunctionGenerator::GetVariableReference(const VariableDeclaration* variable) {
	DebugValidator::IsNotNull(variable);
	
	// We search the IR variable in the function if the variable 
    // is local, else we look at the global scope.
	IR::Variable* irVariable;

	if(localVars_.TryGetValue(variable, &irVariable)) {
		return GetVariableReference(irVariable);
	}
	else return unitGen_->GetVariableReference(variable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
FunctionGenerator::GetVariableReference(const DeclarationExpression* expr) {
	DebugValidator::IsNotNull(expr);
	DebugValidator::IsTrue(expr->Object()->IsVariableDecl());
	return GetVariableReference(expr->Object()->As<VariableDeclaration>());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
FunctionGenerator::GetFieldReference(const FieldDeclaration* field, 
                                     const VariableDeclaration* variable) {
	DebugValidator::IsNotNull(field);
	DebugValidator::IsFalse(field->IsBitfield());

	ExpandedField key(field, variable);
	IR::Variable* fieldVar;

	if(expandedFields_.TryGetValue(key, &fieldVar)) {
		return GetVariableReference(fieldVar);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
FunctionGenerator::GetRecordSlot(const StructUnionType* recordType) {
	// First try to get an unused slot. 
    // If no one is available we create one now.
	IR::Variable* slotVar = nullptr;

	if(recordSlots_.ContainsKey(recordType)) {
		auto& slotQueue = recordSlots_[recordType];

		if(slotQueue.Count() > 0) {
			auto slot = slotQueue.Dequeue();
			usedRecordSlots_.Add(slot);
			slotVar = slot.IRVariable;
		}
	}

	if(slotVar == nullptr) {
		// No slot found, create one.
		RecordSlot slot = CreateRecordSlot(recordType);
		usedRecordSlots_.Add(slot);
		slotVar = slot.IRVariable;
	}

	return GetVariableReference(slotVar);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RecordSlot FunctionGenerator::CreateRecordSlot(const StructUnionType* recordType) {
	// Create a variable having the specified type. 
	// The name has the form '$slot_n0'.
	auto irRecordType = typeGen_->GetType(recordType);
    string name = nameGen_.GetRecordSlotName();
	
    auto variable = irGen_.GetVariableSymbol(irRecordType, name, irFunct_);
    irFunct_->AddVariable(variable);
	return RecordSlot(recordType, variable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::ReleaseRecordSlots() {
	for(int i = 0; i < usedRecordSlots_.Count(); i++) {
		auto slot = usedRecordSlots_[i];
		auto key = slot.RecordType;

		if(recordSlots_.ContainsKey(key)) {
			// The queue was created before.
			recordSlots_[key].Enqueue(slot);
		}
		else {
			// The queue must be created now.
			recordSlots_.Add(key, Queue<RecordSlot>());
			recordSlots_[key].Enqueue(slot);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::AddLabeledBlock(const LabelStatement* statement, 
                                        IR::Block* block) {
	if(labeledBlocks_.ContainsKey(statement) == false) {
		labeledBlocks_.Add(statement, block);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Block* FunctionGenerator::GetLabeledBlock(const LabelStatement* statement) {
	IR::Block* block;

	if(labeledBlocks_.TryGetValue(statement, &block)) {
		return block;
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::PopParentStatement() {
	RestoreVLAStack();
    parentStatements_.Pop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::RestoreVLAStack(bool popAllArrays, bool popLoopArrays) {
    DebugValidator::IsFalse(popAllArrays && popLoopArrays);

    // If the parent of the top VLA context (if any) is the statement
    // that was removed, generate code that restores the state of the stack.
    while(HasVLAContext()) {
        if(TopVLAContext().Parent == parentStatements_.Peek()) {
            RestoreStackState();
        }
        else break;
    }

    // If we're in a loop and have a 'break' or 'continue'
    // statement we need to restore the stack for all variable
    // arrays declared inside the loop until this point.
    if(popLoopArrays && HasVLAContext()) {
        DebugValidator::IsLarger(LoopDepth(), 0);

        List<const Statement*> loopStatements;
        auto& currentLoop = CurrentLoop();
        
        // Collect all parent statements part of the current loop.
        for(int i = 0; i < parentStatements_.Count(); i++) {
            auto statement = ParentStatement(i);
            loopStatements.Add(statement);

            if(statement == currentLoop.BodyStatement) {
                break;
            }
        }

        vlaContexts_.ForEach([this, &loopStatements](VLAContext& vlaContext) -> bool {
            if(loopStatements.Contains(vlaContext.Parent)) {
                GenerateStackRestoreCode(vlaContext);
            }

            return true;
        });
    }

    // We have a 'return', in which case code to restore the stack
    // for all variable arrays must be generated, but without 
    // popping them from the stack.
    if(popAllArrays && HasVLAContext()) {
        vlaContexts_.ForEach([this](VLAContext& vlaContext) -> bool {
            GenerateStackRestoreCode(vlaContext);
            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GeneratorContext FunctionGenerator::GetGeneratorContext(const Expression* expr) {
	return GeneratorContext(unit_, funct_, this, expr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::BeforeLoad(IR::Operand* op, const Expression* expr,
                                   const Type* sourceType) {
	if(auto events = unitGen_->GetEvents()) {
		events->BeforeLoad(GetGeneratorContext(expr), op, sourceType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::AfterLoad(IR::LoadInstr* instr, const Expression* expr,
                                  const Type* sourceType) {
	if(auto events = unitGen_->GetEvents()) {
		events->AfterLoad(GetGeneratorContext(expr), instr, sourceType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::BeforeStore(IR::Operand* op, const Expression* expr,
                                    const Type* destType, const Type* sourceType) {
	if(auto events = unitGen_->GetEvents()) {
		events->BeforeStore(GetGeneratorContext(expr), op, destType, sourceType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::AfterStore(IR::StoreInstr* instr, const Expression* expr,
                                   const Type* destType, const Type* sourceType) {
	if(auto events = unitGen_->GetEvents()) {
		events->AfterStore(GetGeneratorContext(expr), instr, destType, sourceType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::BeforeCall(IR::Operand* op, const Expression* expr) {
	if(auto events = unitGen_->GetEvents()) {
		events->BeforeCall(GetGeneratorContext(expr), op);
	}
}

void FunctionGenerator::AfterCall(IR::CallInstr* instr, const Expression* expr) {
	if(auto events = unitGen_->GetEvents()) {
		events->AfterCall(GetGeneratorContext(expr), instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::BeforeIf(IR::Operand* op) {
	if(auto events = unitGen_->GetEvents()) {
		events->BeforeIf(GetGeneratorContext(), op);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::AfterIf(IR::IfInstr* instr, IR::Block* trueBlock, 
								IR::Block* falseBlock) {
	if(auto events = unitGen_->GetEvents()) {
		events->AfterIf(GetGeneratorContext(), instr, trueBlock, falseBlock);
	}
}

void FunctionGenerator::BeforeLoop(LoopKind loopKind) {
	if(auto events = unitGen_->GetEvents()) {
		events->BeforeLoop(GetGeneratorContext(), loopKind);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FunctionGenerator::AfterLoop(LoopKind loopKind, IR::Block* headerBlock,
				IR::Block* bodyBlock, IR::Block* incrementBlock) {
	if(auto events = unitGen_->GetEvents()) {
		events->AfterLoop(GetGeneratorContext(), loopKind, headerBlock,
						  bodyBlock, incrementBlock);
	}
}

} // namespace IRGenerator