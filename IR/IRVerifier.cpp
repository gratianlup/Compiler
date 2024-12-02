// IRVerifier.cpp
// Copyright (c) Lup Gratian
//
// Implements the IR Verifier.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IRVerifier.hpp"
#include "IRPrinter.hpp"
#include <iostream>

namespace IR {

IRVerifier::IRVerifier(Unit* unit, VerifierErrorHandler* handler, bool debugBreak) :
		unit_(unit), handler_(handler), errorCount_(0), debugBreak_(debugBreak) {
	DebugValidator::IsNotNull(unit);
	DebugValidator::IsNotNull(handler);
	
	// Verify all components of the unit.
	for(auto type = unit->Typenames().First(); type; type = type->Next) {
		type->Value->Accept(this);
	}

	for(auto variable = unit->Variables().First(); variable; variable = variable->Next) {
		variable->Value->Accept(this);
	}

	for(auto function = unit->Functions().First(); function; function= function->Next) {
		function->Value->Accept(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IRVerifier::IRVerifier(Function* function, VerifierErrorHandler* handler, bool debugBreak) :
        unit_(function->ParentUnit()), handler_(handler), 
        errorCount_(0), debugBreak_(debugBreak) {
    DebugValidator::IsNotNull(function);
    DebugValidator::IsNotNull(handler);

    function->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRVerifier::CheckFunction(bool cond, int error, void* obj, const string* function) {
	if(cond == false) {
		handler_->Handle(VerifierError(Error_Function, error, obj, function));
		if(debugBreak_) DebugValidator::Unreachable();
	}

	return cond == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRVerifier::CheckBlock(bool cond, int error, void* obj, 
							const string* function, const string* block) {
	if(cond == false) {
		handler_->Handle(VerifierError(Error_Block, error, obj, currentFunct_, block));
		if(debugBreak_) DebugValidator::Unreachable();
	}

	return cond == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRVerifier::CheckGlobal(bool cond, int error, void* obj, const string* name) {
	if(cond == false) {
		handler_->Handle(VerifierError(true, error, obj, name));
		if(debugBreak_) DebugValidator::Unreachable();
	}

	return cond == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRVerifier::CheckVariable(bool cond, int error, void* obj, const string* name) {
	if(cond == false) {
		handler_->Handle(VerifierError(false, error, obj, name, currentFunct_));
		if(debugBreak_) DebugValidator::Unreachable();
	}

	return cond == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRVerifier::CheckInstruction(bool cond, int error, void* obj) {
	if(cond == false) {
		handler_->Handle(VerifierError(Error_Instruction, error, obj, currentFunct_, 
									   currentBlock_->Name(), instrIndex_));
		if(debugBreak_) DebugValidator::Unreachable();
	}

	return cond == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRVerifier::CheckArgument(bool cond, int error, void* obj, int argIndex) {
	if(cond == false) {
		handler_->Handle(VerifierError(Error_Argument, error, obj, currentFunct_, 
									   currentBlock_->Name(), instrIndex_, argIndex));
		if(debugBreak_) DebugValidator::Unreachable();
	}

	return cond == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(Variable* symbol) {
	// The type of the symbol shall be set.
	CheckVariable(symbol->GetType(), Error::VARIABLE_NO_TYPE,
				  symbol, symbol->Name());

	if(symbol->GetType()) {
		// The type shall not be 'void' or a function type.
		CheckVariable(symbol->GetType()->IsVoid() == false, Error::VARIABLE_VOID_TYPE,
					  symbol, symbol->Name());

		CheckVariable(symbol->GetType()->IsFunction() == false, Error::VARIABLE_FUNCT_TYPE,
					  symbol, symbol->Name());
	}

	// The symbol shall have a name.
	CheckVariable(symbol->Name(), Error::VARIABLE_UNNAMED,
				  symbol, symbol->Name());

	// The parent table shall be set.
	CheckVariable(symbol->ParentTable(), Error::VARIABLE_NO_TABLE,
				  symbol, symbol->Name());

	// The parent table shall not be the global (unit) table.
	CheckVariable(symbol->ParentTable() != &unit_->Symbols(), Error::VARIABLE_UNIT_LEVEL,
				  symbol, symbol->Name());

	// The alignment shall not be negative.
	CheckVariable(symbol->Alignment() >= 0, Error::VARIABLE_ALIGNMENT,
				  symbol, symbol->Name());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::CheckInitializer(const Type* type, Initializer* initializer,
                                  Symbol* symbol) {
	if(type->IsArray() || type->IsRecord()) {
		// If it's an array it can also be initialized with a string.
		if(type->IsArray() && initializer->IsInitializerList() == false) {
			CheckGlobal(initializer->Value()->IsStringConstant() &&
						initializer->Value()->As<StringConstant>()->GetType() == type,
						Error::INIT_VALUE_OP, symbol, symbol->Name());
		}
		else {
			// There should be a list of initializers.
			if(CheckGlobal(initializer->IsInitializerList(), Error::INIT_VALUE_INVALID, 
						   symbol, symbol->Name()) == false) {
				return;
			}

			// Check each initializer in the list.
			CheckInitializerList(type, static_cast<InitializerList*>(initializer), symbol);
		}
	}
	else {
		// The initializer shall have a value.
		CheckGlobal(initializer->Value(), Error::INIT_VALUE_OP, symbol, symbol->Name());
		
		// The type of the value shall be the same as of the symbol.
		// If the type of the initializer is converted to the destination type
		// we use it instead of the original type.
		auto initType = initializer->Conversion() != InitConversion::None ?
						initializer->ConversionType() : initializer->Value()->GetType();

		CheckGlobal(initializer->Value() && type == initType,
					Error::INIT_VALUE_OP, symbol, symbol->Name());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::CheckInitializerList(const Type* type, InitializerList* initList,
									  Symbol* symbol) {
	if(auto arrayType = type->As<ArrayType>()) {
		// The number of elements shall match.
		if(CheckGlobal(initList->Count() == arrayType->Size(),
					   Error::INIT_INVALID_NUMBER, symbol, symbol->Name()) == false) {
			return;
		}

		for(int i = 0; i < arrayType->Size(); i++) {
			CheckInitializer(arrayType->ElementType(), (*initList)[i], symbol);
		}
	}
	else if(auto recordType = type->As<RecordType>()) {
		// The number of elements shall match.
		CheckGlobal(initList->Count() == recordType->FieldCount(),
					Error::INIT_INVALID_NUMBER, symbol, symbol->Name());

		auto& fields = recordType->Fields();
		
		for(int i = 0; i < fields.Count(); i++) {
			CheckInitializer(fields[i].FieldType, (*initList)[i], symbol);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(GlobalVariable* symbol) {
	if(*symbol->Name() == "aBuiltinFunc") {
		auto list1 = symbol->GetInitializerList();
		for(int i = 0; i < list1->Count(); i++) {
			auto list2 = (IR::InitializerList*)((*list1)[i].Raw());

			for(int j = 0; j < list2->Count(); j++){
				if((*list2)[j]->IsInitializerList()) {
					assert(0);
				}
				else (*list2)[j]->Value()->GetType();
			}
		}
	}

	// The type of the symbol shall be set.
	CheckGlobal(symbol->GetType(), Error::VARIABLE_NO_TYPE,
				symbol, symbol->Name());

	if(symbol->GetType()) {
		// The type shall not be 'void' or a function type.
		CheckGlobal(symbol->GetType()->IsVoid() == false, Error::VARIABLE_VOID_TYPE,
					symbol, symbol->Name());
		CheckGlobal(symbol->GetType()->IsFunction() == false, Error::VARIABLE_FUNCT_TYPE,
					symbol, symbol->Name());
	}

	// The symbol shall have a name.
	CheckGlobal(symbol->Name(), Error::VARIABLE_UNNAMED,
				symbol, symbol->Name());

	// The parent table shall be set and be the global (unit) table.
	CheckGlobal(symbol->ParentTable() == &unit_->Symbols(), Error::VARIABLE_NO_TABLE,
				symbol, symbol->Name());

	// The alignment shall not be negative.
	CheckVariable(symbol->Alignment() >= 0, Error::VARIABLE_ALIGNMENT,
				  symbol, symbol->Name());

	// A constant variable shall have an initializer.
	CheckGlobal((symbol->IsConstant() == false) || symbol->HasInitializer(), 
				Error::GLOBAL_CONST_NO_INIT, symbol, symbol->Name());

	// Check the initializers (if any).
	if(symbol->HasInitializer() == false) {
        return;
    }

	if(symbol->HasSimpleInitializer()) {
		// If the type of the initializer is converted to the destination type
		// we use it instead of the original type.
		auto initializer = symbol->GetInitializer();
		auto initType = initializer->Conversion() != InitConversion::None ?
						initializer->ConversionType() : initializer->Value()->GetType();

		// Value initializers shall not be used for arrays/records, except when
		// we have an array initialized by a string constant.
		CheckGlobal(symbol->GetType() && (symbol->GetType()->IsRecord() == false),
					Error::INIT_VALUE_INVALID, symbol, symbol->Name());

		CheckGlobal(symbol->GetType() && (symbol->GetType()->IsArray() == false ||
					initializer->Value()->IsStringConstant()),
					Error::INIT_VALUE_INVALID, symbol, symbol->Name());

		// The initializer shall have a value.
		CheckGlobal(initializer->Value(), Error::INIT_VALUE_OP, symbol, symbol->Name());
		
		// The type of the value shall be the same as of the symbol.
		CheckGlobal(initializer->Value() && symbol->GetType() == initType,
					Error::INIT_VALUE_OP, symbol, symbol->Name());
	}
	else if(symbol->HasInitializerList()) {
		// The list can be used only on arrays/records.
		CheckGlobal(symbol->GetType() && 
				   (symbol->GetType()->IsRecord() || symbol->GetType()->IsArray()),
					Error::INIT_LIST_INVALID, symbol, symbol->Name());

		// The list shall not be empty.
		CheckGlobal(symbol->GetInitializerList()->Count() > 0,
					Error::INIT_LIST_INVALID, symbol, symbol->Name());

		CheckInitializerList(symbol->GetType(), symbol->GetInitializerList(), symbol);		
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(Symbol* symbol) {
	// This is a typename. It must have a type and a name.
	CheckGlobal(symbol->GetType(), Error::TYPENAME_NO_TYPE, symbol, symbol->Name());

	CheckGlobal(symbol->HasName(), Error::TYPENAME_NO_NAME, symbol, symbol->Name());

	// Arrays shall have size greater than zero.
	const Type* type = symbol->GetType();
	
    if(type == nullptr) {
        return;
    }

	if(auto temp = type->As<RecordType>()) {
		for(int i = 0; i < temp->FieldCount(); i++) {
			auto field = temp->Fields()[i];

			// The field shall have a type.
			CheckGlobal(field.FieldType, Error::FIELD_NO_TYPE, 
						symbol, symbol->Name());

			// The field type shall not be 'void' or a function type.
			CheckGlobal(field.FieldType && field.FieldType->IsVoid() == false, 
						Error::FIELD_VOID_TYPE, symbol, symbol->Name());

			CheckGlobal(field.FieldType && field.FieldType->IsFunction() == false, 
						Error::FIELD_FUNCT_TYPE, symbol, symbol->Name());

			// The field offset shall be positive.
			CheckGlobal(field.FieldOffset >= 0, Error::FIELD_OFFSET_NEGATIVE, 
						symbol, symbol->Name());

			// The field offset shall be greater or equal to the previous one.
			CheckGlobal(i == 0 || field.FieldOffset >= temp->Fields()[i - 1].FieldOffset,
						Error::FIELD_OFFSET_SMALLER, symbol, symbol->Name());
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(Block* block) {
	currentBlock_ = block;
	string* functionName = block->ParentFunction() ? 
						   block->ParentFunction()->Name() : nullptr;

	// The block shall have a name.
	CheckBlock(block->HasName(), Error::BLOCK_UNNAMED,
			   block, functionName, block->Name());

	// The block shall be linked to the parent function.
	CheckBlock(block->ParentFunction(), Error::BLOCK_INVALID_PARENT, 
			   block, functionName, block->Name());

	// The block shall have at least one instruction.
	CheckBlock(block->InstructionCount() > 0, Error::BLOCK_NO_INSTR, 
			   block, functionName, block->Name());

	// The last instruction shall be a control instruction (if, goto, etc.).
	CheckBlock(block->LastInstruction() && block->LastInstruction()->IsBranching(),
			   Error::BLOCK_END_NOT_BRANCH_INSTR, block, functionName, block->Name());

	// Verify each instruction in the block.
	branchInstrCount_ = 0;
	instrIndex_ = 0;

	for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
		instr->Accept(this);
		instrIndex_++;
	}

	// There must be exactly one branch instruction in the block.
	CheckBlock(branchInstrCount_ < 2, Error::BLOCK_DUPLICATE_BRANCH_INSTR, 
			   block, functionName, block->Name());
	currentBlock_ = nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(Function* function) {
	currentFunct_ = function;

	// The function shall have a name.
	CheckFunction(function->HasName(), Error::FUNCT_UNNAMED, function, function->Name());

	// The function shall be linked with the parent unit.
	CheckFunction(function->ParentUnit(), Error::FUNCT_NO_PARENT, 
				  function, function->Name());

	// The function shall be linked with the parent symbol table.
	CheckFunction(function->ParentTable(), Error::FUNCT_NO_TABLE, 
				  function, function->Name());

	// The function shall have at least one block if it's a definition.
	CheckFunction(!function->IsDefinition() || function->BlockCount() > 0,
				  Error::FUNCT_NO_BLOCKS, function, function->Name());

	// The type shall be a function type.
	CheckFunction(function->GetType() && function->GetType()->IsFunction(),
				  Error::FUNCT_TYPE_NOT_FUNCT, function, function->Name());

	// The function shall have a return type.
	CheckFunction(function->ReturnType(),
				  Error::FUNCT_NO_RETURN_TYPE, function, function->Name());

	// The return type shall not be a function.
	CheckFunction(function->ReturnType()->IsFunction() == false,
				  Error::FUNCT_RETURN_TYPE_FUNCT, function, function->Name());

	// Return type and parameters shall match function type.
	const FunctionType* type = function->GetType() ? 
							   function->GetType()->As<FunctionType>() : nullptr;
	CheckFunction(!type || ((type->ReturnType() ==  function->ReturnType()) &&
				  (type->ParameterCount() == function->ParameterCount())),
				  Error::FUNCT_PARAM_COUNT, function, function->Name());

	if(type && type->ParameterCount() == function->ParameterCount()) {
		for(int i = 0; i < type->ParameterCount(); i++) {
			CheckFunction(type->Parameters()[i] == function->Parameters()[i]->GetType(),
						  Error::FUNCT_PARAM_TYPE, function, function->Name());

			// All parameters shall have 'auto' storage.
			CheckFunction(function->Parameters()[i]->IsAuto(),
						  Error::FUNCT_PARAM_NOT_AUTO, function, function->Name());

			// Parameters cannot be global variables.
			CheckFunction(function->Parameters()[i]->IsLocalVariable(),
						  Error::FUNCT_GLOBAL_VAR_PARAM, function, function->Name());
		}
	}

	// Check properties for all variables.
	int notAuto = 0;
	int varCount = 0;
	int globalCount = 0;
	IRVerifier* verifier = this;

	function->Symbols().ForEach([&notAuto, &varCount, &globalCount, verifier](Symbol* symbol) {
		if(auto temp = symbol->As<Variable>()) {
			varCount++;

			if(temp->IsAuto() == false) {
				notAuto++;
			}
		}
		else if(symbol->IsGlobalVariable()) {
			globalCount++;
		}

		symbol->Accept(verifier);
	});

	// All variables shall have 'auto' storage.
	CheckFunction(notAuto == 0, Error::FUNCT_PARAM_NOT_AUTO, function, function->Name());

	// No global variables shall be declared in the function.
	CheckFunction(globalCount == 0, Error::FUNCT_VAR_GLOBAL, function, function->Name());

	// If it's a declaration it shall have no body.
	CheckFunction(function->IsDefinition() || (function->BlockCount() == 0),
				  Error::FUNCT_DECL_HAS_BODY, function, function->Name());
	currentFunct_ = nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(Temporary* op) {
	const string* name = nullptr;

	// If the temporary has a name use it for diagnostics.
    if(auto nameTag = op->GetTag<NameTag>()) {
        name = &nameTag->Name();
	}

	// The temporary shall have a type.
	CheckVariable(op->GetType(), Error::TEMP_NO_TYPE, op, name);
	
    if(op->GetType() == nullptr) {
        return;
    }

	// The temporary shall not have void, function, array or record type.
	CheckVariable(!(op->GetType()->IsVoid() || op->GetType()->IsFunction() ||
					op->GetType()->IsArray() || op->GetType()->IsRecord()),
					Error::TEMP_INVALID_TYPE, op, name);

	// The temporary should be linked to it's defining instruction.
	CheckVariable(op->HasDefiningInstruction(), 
                  Error::TEMP_NO_DEFINING_INSTR, op, name);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(VariableReference* op) {
	// The operand shall have a symbol.
	CheckVariable(op->GetSymbol(), Error::OP_INVALID_SYMBOL, op, nullptr);

	// The symbol shall be a variable.
	if(op->IsLocalVariableRef()) {
		CheckVariable(op->GetSymbol()->IsLocalVariable(),
						Error::OP_INVALID_SYMBOL, op, nullptr);
	}
	else {
		CheckVariable(op->GetSymbol()->IsGlobalVariable(),
						Error::OP_INVALID_SYMBOL, op, nullptr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(FunctionReference* op) {
	// The target function shall be provided.
	CheckVariable(op->Target(), Error::FUNCT_REF_NO_BLOCK, op, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(BlockReference* op) {
	// The target function shall be provided.
	CheckVariable(op->Target(), Error::FUNCT_REF_NO_BLOCK, op, nullptr);

	// If we're inside a function the operand shall point to a block
	// from the current function only.
	if(currentFunct_) {
		CheckVariable(!op->Target() || (op->Target()->ParentFunction() == currentFunct_), 
					  Error::BLOCK_REF_INVALID_FUNCT, op, nullptr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(IntConstant* op) {
	// This shall have integer type.
	CheckVariable(op->GetType() && op->GetType()->IsInteger(),
				  Error::INT_CONST_NOT_INT, op, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(FloatConstant* op) {
	// This shall have integer type.
	CheckVariable(op->GetType() && op->GetType()->IsFloating(),
				  Error::FLOAT_CONST_NOT_FLOAT, op, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(StringConstant* op) {
	// This shall have array type with integer element.
	CheckVariable(op->GetType() && op->GetType()->IsArray() && 
				  op->GetType()->As<ArrayType>()->ElementType()->IsInteger(),
				  Error::STRING_CONST_NOT_ARRAY, op, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(NullConstant* op) {
	// This shall have pointer type.
	CheckVariable(op->GetType() && op->GetType()->IsPointer(),
				  Error::NULL_CONST_NOT_POINTER, op, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(UndefinedConstant* op) {
	// It should have a type that is not an array, record or function.
	CheckVariable(op->GetType() && ((op->GetType()->IsArray() ||
				  op->GetType()->IsRecord() || op->GetType()->IsFunction()) == false),
				  Error::UNDEF_CONST_INVALID_TYPE, op, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(ArithmeticInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// Source1 and source2 operands shall be set and have the same type.
	CheckInstruction(instr->LeftOp() && instr->RightOp(),
					 Error::INSTR_NO_OPERAND, instr);

	// Verify the operand type.
	if(instr->LeftOp() && instr->RightOp()) {
		instr->LeftOp()->Accept(this);
		instr->RightOp()->Accept(this);

		auto leftType = instr->LeftOp()->GetType();
		auto rightType = instr->RightOp()->GetType();

		// The types of the source operands shall be the same.
		CheckInstruction(leftType == rightType, 
						 Error::ARITH_OPERANDS_NOT_SAME_TYPE, instr);

		if(instr->IsIntArithmetic()) {
			CheckInstruction(!leftType || leftType->IsInteger(),
							 Error::ARITH_OPERAND_INVALID_TYPE, instr);
		}
		else {
			CheckInstruction(!leftType || leftType->IsFloating(),
							 Error::ARITH_OPERAND_INVALID_TYPE, instr);
		}

		// The result type, if present, shall have the same type.
		if(instr->ResultOp()) {
			CheckInstruction(instr->ResultOp()->GetType() == leftType,
							 Error::INSTR_RESULT_INVALID_TYPE, instr);

			CheckInstruction((instr->ResultOp()->GetType()->IsArray() ||
							 instr->ResultOp()->GetType()->IsRecord()) == false,
							 Error::INSTR_RESULT_COMPLEX_TYPE, instr);

			CheckInstruction(instr->ResultOp()->IsVariableReference() == false, 
						 Error::ADDRESS_SOURCE_INVALID, instr);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(LogicalInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// Source1 and source2 operands shall be set and have the same type.
	CheckInstruction(instr->LeftOp() && instr->RightOp(),
					 Error::INSTR_NO_OPERAND, instr);

	// Verify the operand type.
	if(instr->LeftOp() && instr->RightOp()) {
		instr->LeftOp()->Accept(this);
		instr->RightOp()->Accept(this);

		// The target operand shall not be a variable.
		CheckInstruction(!(instr->LeftOp()->IsVariableReference() || 
                           instr->RightOp()->IsVariableReference()),
						 Error::INSTR_VARIABLE_OPERAND, instr);

		auto leftType = instr->LeftOp()->GetType();
		auto rightType = instr->RightOp()->GetType();

		// The types of the source operands shall be the same, and integer.
		CheckInstruction(leftType == rightType, 
						 Error::LOGICAL_OPERANDS_NOT_SAME_TYPE, instr);

		CheckInstruction(!leftType || leftType->IsInteger(),
						 Error::LOGICAL_OPERAND_INVALID_TYPE, instr);

		// The result type, if present, shall have the same type.
		if(instr->ResultOp()) {
			CheckInstruction(instr->ResultOp()->GetType() == leftType,
							 Error::INSTR_RESULT_INVALID_TYPE, instr);

			CheckInstruction((instr->ResultOp()->GetType()->IsArray() ||
							 instr->ResultOp()->GetType()->IsRecord()) == false,
							 Error::INSTR_RESULT_COMPLEX_TYPE, instr);

			CheckInstruction(instr->ResultOp()->IsVariableReference() == false, 
						 Error::ADDRESS_SOURCE_INVALID, instr);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(ConversionInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// Target and type operands shall be set.
	CheckInstruction(instr->TargetOp() && instr->CastType(),
					 Error::INSTR_NO_OPERAND, instr);

	if(instr->TargetOp() && instr->CastType()) {
		instr->TargetOp()->Accept(this);

		auto targetType = instr->TargetOp()->GetType();
		
        if(targetType == nullptr) {
            return;
        }
		
        auto castType = instr->CastType();

		switch(instr->GetOpcode()) {
			case Opcode::Trunc: {
				// int -> int
				CheckInstruction(targetType->IsInteger(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsInteger(), Error::CONV_CAST_TYPE, instr);

				// The cast type shall have a lower or equal rank.
				if(auto targetInt = targetType->As<IntegerType>()) {
					if(auto castInt = castType->As<IntegerType>()) {
						CheckInstruction(castInt->Rank() <= targetInt->Rank(),
										 Error::CONV_CAST_TYPE, instr);
					}
				}

				break;
			}
			case Opcode::Zext:
			case Opcode::Sext: {
				// int -> int
				CheckInstruction(targetType->IsInteger(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsInteger(), Error::CONV_CAST_TYPE, instr);

				// The cast type shall have a greater or equal rank.
				if(auto targetInt = targetType->As<IntegerType>()) {
					if(auto castInt = castType->As<IntegerType>()) {
						CheckInstruction(castInt->Rank() >= targetInt->Rank(),
										 Error::CONV_CAST_TYPE, instr);
					}
				}

				break;
			}
			case Opcode::Ftoi:
			case Opcode::Ftoui: {
				// floating -> int
				CheckInstruction(targetType->IsFloating(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsInteger(), Error::CONV_CAST_TYPE, instr);
				break;
			}
			case Opcode::Itof:
			case Opcode::Uitof: {
				// int -> floating
				CheckInstruction(targetType->IsInteger(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsFloating(), Error::CONV_CAST_TYPE, instr);
				break;
			}
			case Opcode::Ftrunc: {
				// double -> float
				CheckInstruction(targetType->IsDouble(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsFloat(), Error::CONV_CAST_TYPE, instr);
				break;
			}
			case Opcode::Fext: {
				// float -> double
				CheckInstruction(targetType->IsFloat(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsDouble(), Error::CONV_CAST_TYPE, instr);
				break;
			}
			case Opcode::Ptoi: {
				// pointer -> int
				CheckInstruction(targetType->IsPointer(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsInteger(), Error::CONV_CAST_TYPE, instr);
				break;
			}
			case Opcode::Itop: {
				// int -> pointer
				CheckInstruction(targetType->IsInteger(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsPointer(), Error::CONV_CAST_TYPE, instr);
				break;
			}
			case Opcode::Ptop: {
				// pointer -> pointer
				CheckInstruction(targetType->IsPointer(), Error::CONV_INVALID_TYPE, instr);
				CheckInstruction(castType->IsPointer(), Error::CONV_CAST_TYPE, instr);
				break;
			}
		}

		if(instr->ResultOp()) {
			CheckInstruction(instr->ResultOp()->GetType() == castType,
							 Error::INSTR_RESULT_INVALID_TYPE, instr);

			CheckInstruction((instr->ResultOp()->GetType()->IsArray() ||
							  instr->ResultOp()->GetType()->IsRecord()) == false,
							 Error::INSTR_RESULT_COMPLEX_TYPE, instr);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(AddressInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// Target and index operands shall be set.
	CheckInstruction(instr->BaseOp() && instr->IndexOp(),
					 Error::INSTR_NO_OPERAND, instr);

	// The target operand shall have pointer type.
	if(instr->BaseOp()) {
		auto type = instr->BaseOp()->GetType();

		CheckInstruction(!type || type->IsPointer(), 
						 Error::ADDRESS_SOURCE_INVALID, instr);
	}

	// The index operand shall have integer type and not be a variable.
	if(instr->IndexOp()) {
		auto type = instr->IndexOp()->GetType();

		CheckInstruction(!type || type->IsInteger(), 
						 Error::ADDRESS_INDEX_INVALID, instr);

		CheckInstruction(instr->IndexOp()->IsVariableReference() == false, 
						 Error::ADDRESS_SOURCE_INVALID, instr);
	}

	// The result operand (if any) shall have the type pointed by the base.
	if(instr->ResultOp() && instr->BaseOp()) {
		instr->ResultOp()->Accept(this);
		instr->BaseOp()->Accept(this);

		auto type = instr->BaseOp()->GetType();
		
        if(type == nullptr) {
            return;
        }

		if(auto pointerType = type->As<PointerType>()) {
				// 'int32* -> int32*', for example.
			CheckInstruction(pointerType == instr->ResultOp()->GetType(), 
							 Error::ADDRESS_RESULT_INVALID, instr);
		}

		if(instr->ResultOp()->GetType() == nullptr) {
            return;
        }

		CheckInstruction((instr->ResultOp()->GetType()->IsArray() ||
						  instr->ResultOp()->GetType()->IsRecord()) == false,
						  Error::INSTR_RESULT_COMPLEX_TYPE, instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(IndexInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// Target and index operands shall be set.
	CheckInstruction(instr->BaseOp() && instr->IndexOp(),
					 Error::INSTR_NO_OPERAND, instr);

	// The target operand shall have pointer type.
	if(instr->BaseOp()) {
		auto type = instr->BaseOp()->GetType();

		CheckInstruction(!type || type->IsPointer(), 
						 Error::INDEX_SOURCE_INVALID, instr);

		// It should be a pointer to array or to record.
		if(auto pointerType = type->As<PointerType>()) {
			CheckInstruction(pointerType->PointeeType()->IsArray(), 
							 Error::INDEX_SOURCE_INVALID, instr);
		}
	}

	// The index operand shall have integer type and not be a variable.
	if(instr->IndexOp()) {
		auto type = instr->IndexOp()->GetType();

		CheckInstruction(!type || type->IsInteger(), 
						 Error::INDEX_INDEX_INVALID, instr);

		CheckInstruction(instr->IndexOp()->IsVariableReference() == false, 
						 Error::INDEX_SOURCE_INVALID, instr);
	}

	// The result operand (if any) shall have the type pointed by the base.
	if(instr->ResultOp() && instr->BaseOp()) {
		instr->ResultOp()->Accept(this);
		instr->BaseOp()->Accept(this);

		auto type = instr->BaseOp()->GetType();

		if(type == nullptr) {
            return;
        }

		if(auto pointerType = type->As<PointerType>()) {
			// If the base pointee is an array we expect a pointer to the
			// element type as the result operand type. 
            // The same applies to record types.
			if(auto arrayType = pointerType->PointeeType()->As<ArrayType>()) {
				// [N int32*] -> int32*
				auto resultType = instr->ResultOp()->GetType();
				CheckInstruction(resultType->Is<PointerType>() &&
									(resultType->As<PointerType>()->PointeeType() == 
									arrayType->ElementType()), 
									Error::INDEX_RESULT_INVALID, instr);
			}
		}

		if(instr->ResultOp()->GetType() == nullptr) {
            return;
        }

		CheckInstruction((instr->ResultOp()->GetType()->IsArray() ||
						  instr->ResultOp()->GetType()->IsRecord()) == false,
						  Error::INSTR_RESULT_COMPLEX_TYPE, instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(FieldInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// Target and index operands shall be set.
	CheckInstruction(instr->BaseOp() && instr->IndexOp(),
					 Error::INSTR_NO_OPERAND, instr);

	// The target operand shall have pointer type.
	if(instr->BaseOp()) {
		auto type = instr->BaseOp()->GetType();

		CheckInstruction(!type || type->IsPointer(), 
						 Error::ELEMENT_SOURCE_INVALID, instr);

		// It should be a pointer to record.
		if(auto pointerType = type->As<PointerType>()) {
			CheckInstruction(pointerType->PointeeType()->IsRecord(),
							 Error::ELEMENT_SOURCE_INVALID, instr);
		}
	}

	// The index operand shall have integer type and not be a variable.
	if(instr->IndexOp()) {
		auto type = instr->IndexOp()->GetType();

		CheckInstruction(!type || type->IsInteger(), 
						 Error::ELEMENT_INDEX_INVALID, instr);

		CheckInstruction(instr->IndexOp()->IsVariableReference() == false, 
						 Error::ELEMENT_SOURCE_INVALID, instr);
	}

	// The result operand (if any) shall have the type pointed by the base.
	if(instr->ResultOp() && instr->BaseOp()) {
		instr->ResultOp()->Accept(this);
		instr->BaseOp()->Accept(this);

		auto type = instr->BaseOp()->GetType();
		
        if(type == nullptr) {
            return;
        }

		if(auto pointerType = type->As<PointerType>()) {
			// If the base pointee is a record we expect a pointer to the
			// element type as the result operand type.
			if(auto recordType = pointerType->PointeeType()->As<RecordType>()) {
				// The index of the field must be a constant integer
				// that refers to a existing field.
				if(CheckInstruction(instr->IndexOp()->IsIntConstant(), 
								    Error::ELEMENT_FIELD_INDEX_NOT_CONST, instr)) {
					// Condition not met.
					return;
				}

				__int64 index = instr->IndexOp()->As<IntConstant>()->Value();
				auto resultType = instr->ResultOp()->GetType();

				// Index must be between 0 and FieldCount - 1.
				if(CheckInstruction((index >= 0) && (index < recordType->FieldCount()), 
								    Error::ELEMENT_FIELD_INDEX_INVALID, instr)) {
					return;
				}

				CheckInstruction(resultType->Is<PointerType>() &&
								 (resultType->As<PointerType>()->PointeeType() == 
								 recordType->Fields()[(int)index].FieldType), 
								 Error::ELEMENT_RESULT_INVALID, instr);
			}
		}

		if(instr->ResultOp()->GetType() == nullptr) {
            return;
        }

		CheckInstruction((instr->ResultOp()->GetType()->IsArray() ||
						  instr->ResultOp()->GetType()->IsRecord()) == false,
						  Error::INSTR_RESULT_COMPLEX_TYPE, instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(LoadInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// The source and result operands shall be set and have a type.
	CheckInstruction(instr->SourceOp(), Error::INSTR_NO_OPERAND, instr);

	if(instr->SourceOp() == nullptr) {
        return;
    }
	
	// The source operand needs to have pointer type.
	auto sourceType = instr->SourceOp()->GetType();
	CheckInstruction(sourceType, Error::LOAD_SOURCE_INVALID, instr);
	
    if(sourceType == nullptr) {
        return;
    }

	CheckInstruction(sourceType->IsPointer(), Error::LOAD_SOURCE_INVALID, instr);

	// The result operand needs to have the type pointer by the source operand.
	if(sourceType->IsPointer() == false) {
        return;
    }

	if(instr->ResultOp()) {
		instr->ResultOp()->Accept(this);
		auto resultType = instr->ResultOp()->GetType();
		auto pointeeType = sourceType->As<PointerType>()->PointeeType();

		CheckInstruction(resultType == pointeeType,
						 Error::LOAD_RESULT_INVALID, instr);

		CheckInstruction((instr->ResultOp()->GetType()->IsArray() ||
							 instr->ResultOp()->GetType()->IsRecord()) == false,
							 Error::INSTR_RESULT_COMPLEX_TYPE, instr);

		CheckInstruction(instr->ResultOp()->IsVariableReference() == false, 
						 Error::ADDRESS_SOURCE_INVALID, instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(StoreInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// The destination and source operands shall be set.
	CheckInstruction(!((instr->SourceOp() == nullptr) || 
                       (instr->DestinationOp() == nullptr)),
					 Error::INSTR_NO_OPERAND, instr);

	if(instr->DestinationOp() == nullptr) {
        return;
    }

	// The destination operand needs to have pointer type.
	auto destType = instr->DestinationOp()->GetType();
	CheckInstruction(destType, Error::LOAD_SOURCE_INVALID, instr);

	if(destType == nullptr) {
        return;
    }
	
    CheckInstruction(destType->IsPointer(), Error::STORE_DEST_INVALID, instr);

	// The source operand needs to have the type pointer by the source operand.
	if(destType->IsPointer() == false) {
        return;
    }

	if(instr->SourceOp()) {
		instr->SourceOp()->Accept(this);
		auto sourceType = instr->SourceOp()->GetType();
		auto pointeeType = destType->As<PointerType>()->PointeeType();

		CheckInstruction(sourceType == pointeeType,
						 Error::STORE_SOURCE_INVALID, instr);

		// Source operand can't have array/record type.
		CheckInstruction((instr->SourceOp()->GetType()->IsArray() ||
						  instr->SourceOp()->GetType()->IsRecord()) == false,
						  Error::INSTR_RESULT_COMPLEX_TYPE, instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(IfInstr* instr) {
	branchInstrCount_++;

	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// The conditional, true and false operands shall be set.
	CheckInstruction(!((instr->ConditionOp() == nullptr)  || 
                       (instr->TrueTargetOp() == nullptr) ||
					   (instr->FalseTargetOp() == nullptr)),
					 Error::INSTR_NO_OPERAND, instr);

	// The conditional operand needs to have integer type.
	if(instr->ConditionOp()) {
		instr->ConditionOp()->Accept(this);
		auto type = instr->ConditionOp()->GetType();

		CheckInstruction(type && type->IsInteger(), Error::IF_COND_NOT_INT, instr);
	}	
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(GotoInstr* instr) {
	branchInstrCount_++;

	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// The target shall be a block reference.
	CheckInstruction(instr->TargetOp() && 
					 (instr->TargetOp()->ParentFunction() == currentFunct_),
					 Error::GOTO_INVALID_TARGET, instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(CallInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// The target shall be set and have function type.
	CheckInstruction(instr->TargetOp(), Error::CALL_TARGET_NULL, instr);

	if(instr->TargetOp()) {
		instr->TargetOp()->Accept(this);

		auto type = instr->TargetOp()->GetType();
		
        if(type == nullptr) {
            return;
        }

		auto functionType = type->IsPointer() ? 
                            type->As<PointerType>()->PointeeType() : nullptr;
		CheckInstruction(functionType && functionType->IsFunction(), 
						 Error::CALL_TARGET_NOT_FUNCT, instr);
	}

	// Validate the arguments (number and type).
	if(instr->TargetOp() == nullptr) {
        return;
    }

	auto type = instr->TargetOp()->GetType();
	if(type == nullptr) {
        return;
    }

	const FunctionType* functionType = nullptr;
	
	if(auto pointerType = type->As<PointerType>()) {
		if(auto pointee = pointerType->PointeeType()) {
			CheckInstruction(pointee->IsFunction(), Error::CALL_TARGET_NOT_FUNCT, instr);
			functionType = pointee->As<FunctionType>();
		}
	}

	// If the return type is 'void' there shall be no result operand;
	// otherwise, the type of the operand shall be the return type.
	if(functionType == nullptr) {
        return;
    }

	if(functionType->IsVoid()) {
		CheckInstruction(instr->ResultOp() == nullptr, 
						 Error::CALL_VOID_RESULT_OP, instr);
	}
	else {
		CheckInstruction((instr->ResultOp() == nullptr) ||
						 (instr->ResultOp()->GetType() == functionType->ReturnType()),
						 Error::CALL_RESULT_TYPE, instr);
	}

	// Validate the arguments. The number must match (exception for varargs),
	// and the type of the arguments must be the same as the type of the parameters.
	int argCount = instr->ArgumentCount();
	bool argCountValid = (argCount == functionType->ParameterCount()) ||
						 ((argCount >= functionType->ParameterCount()) && 
                           functionType->IsVarargs());

	CheckInstruction(argCountValid, Error::ARGUMENT_NUM_MISMATCH, instr);
	
    if(argCountValid == false) {
        return;
    }

	// Validate as much parameters as possible.
	argCount = std::min(argCount, functionType->ParameterCount());

	for(int i = 0; i < argCount; i++) {
		auto argument = (*instr->Arguments())[i];
		auto parameterType = functionType->Parameters()[i];
		argument->Accept(this);

		CheckInstruction(argument->GetType() == parameterType, 
						 Error::ARGUMENT_TYPE_MISMATCH, instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(ReturnInstr* instr) {
	branchInstrCount_++;

	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);
	
    if(currentFunct_->ReturnType() == nullptr) {
        return;
    }

	// The operand can be nullptr only if the function has return type 'void'.
	if(currentFunct_->ReturnType()->IsVoid()) {
		CheckInstruction(instr->IsVoid(), Error::RETURN_OP_FOR_VOID, instr);
	}
	else {
		CheckInstruction(instr->IsVoid() == false, Error::RETURN_OP_INCOMPATIBLE, instr);
		
        if(instr->IsVoid()) {
            return;
        }

		// The type of the returned operand shall match the function return type.
		instr->ReturnedOp()->Accept(this);
		CheckInstruction(instr->ReturnedOp()->GetType() == currentFunct_->ReturnType(),
						 Error::RETURN_OP_INCOMPATIBLE, instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::CheckCompare(CmpInstrBase* instr, bool isInt) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// The operands shall not be null.
	CheckInstruction(instr->LeftOp() && instr->RightOp(),
					 Error::INSTR_NO_OPERAND, instr);

	// Verify the operand type.
	if(instr->LeftOp() && instr->RightOp()) {
		instr->LeftOp()->Accept(this);
		instr->RightOp()->Accept(this);

		auto leftType = instr->LeftOp()->GetType();
		auto rightType = instr->RightOp()->GetType();

		// The types of the source operands shall be the same.
		CheckInstruction(leftType == rightType, 
						 Error::COMPARE_OPERANDS_NOT_SAME_TYPE, instr);

		// The operands shall have integer type.
		// An exception is for 'ucmp', which can have pointers as operands.
		if(instr->Is<UcmpInstr>()) {
			CheckInstruction(leftType && ((leftType->IsArray() || leftType->IsRecord() ||
							 leftType->IsFunction()) == false),
							 Error::ARITH_OPERAND_INVALID_TYPE, instr);
		}
		else {
			CheckInstruction(leftType && (isInt ? leftType->IsInteger() : 
                                                  leftType->IsFloating()),
							 Error::ARITH_OPERAND_INVALID_TYPE, instr);
		}

		// The result type, if present, shall have integer type.
		if(instr->ResultOp()) {
			CheckInstruction(instr->ResultOp()->GetType()->IsInteger(),
							 Error::COMPARE_RESULT_NOT_INT, instr);

			CheckInstruction(instr->ResultOp()->IsVariableReference() == false, 
						 Error::ADDRESS_SOURCE_INVALID, instr);
		}	
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(CmpInstr* instr) {
	CheckCompare(instr, true);
}

void IRVerifier::Visit(UcmpInstr* instr) {
	CheckCompare(instr, true);
}

void IRVerifier::Visit(FcmpInstr* instr) {
	CheckCompare(instr, false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRVerifier::Visit(SwitchInstr* instr) {
	// The instruction parent block shall be set.
	CheckInstruction(instr->ParentBlock(), Error::INSTR_NO_PARENT, instr);

	// The condition operand shall be set and have integer type.
	CheckInstruction(instr->ConditionOp(), Error::SWITCH_COND_OP_NULL, instr);
	CheckInstruction((instr->ConditionOp() == nullptr) || 
					  instr->ConditionOp()->GetType()->IsInteger(),
					 Error::SWITCH_COND_OP_NOT_INT, instr);

	// Verify that each case is unique and has a target block.
	Dictionary<__int64, __int64> found;
	auto& list = instr->CaseList();

	for(int i = 0; i < list.Count(); i++) {
		CheckInstruction(found.ContainsKey(list[i].Value) == false, 
						 Error::SWITCH_CASE_DUPLICATE, instr);			

		CheckInstruction(list[i].Target, Error::SWITCH_CASE_TARGET_NULL, instr);
		found.Add(list[i].Value, list[i].Value);
	}
}

// ######################################################################################
// DefaultVerifierHandler
// ######################################################################################
DefaultVerifierHandler::DefaultVerifierHandler() {
	#define errorMessage(TYPE, MESSAGE) verifierMessages_.Add(Error::TYPE, MESSAGE);
	#include "IRVerifierMessages.def"
	#undef errorMessage
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DefaultVerifierHandler::Handle(VerifierError message) {
	Console::SetTextColor(ConsoleColor::Red);
	std::wcout<<"VERIFIER ERROR:\n ";
	Console::SetTextColor(ConsoleColor::White);
	std::wcout<<"\t"<<verifierMessages_[message.Error].Chars()<<"\n";

	Console::SetTextColor(ConsoleColor::Gray);
	if(message.ParentFunction) std::wcout<<"\tFunction: "<<message.ParentFunction->Name()->Chars()<<"\n";
	if(message.BlockName) std::wcout<<"\tBlock: "<<message.BlockName->Chars()<<"\n";
	if(message.SymbolName) std::wcout<<"\tSymbol: "<<message.SymbolName->Chars()<<"\n";
	if(message.InstructionIndex != -1) std::wcout<<"\tInstruction: "<<message.InstructionIndex<<"\n";
	std::wcout<<"\n";
    
#if 0
    if(message.ParentFunction) {
        IRPrinter printer(const_cast<Function*>(message.ParentFunction), true);
        printer.Dump();
    }
#endif

#if 0
    if(message.Object) {
        auto instr = (Instruction*)message.Object;
        IRPrinter printer(const_cast<Block*>(instr->ParentBlock()), true);
        printer.Dump();
    }
#endif
}

} // namespace IR