// Interpreter.cpp
// Copyright (c) Lup Gratian
//
// Implements the IR interpreter.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Interpreter.hpp"
#include "IRPrinter.hpp"
#include "../Base/StringBuilder.hpp"
#include <cmath>
#include <iostream>
#include <cstdio>
#include <Windows.h>
using namespace Base;

namespace IR {

Value* Value::Get(const Type* type, bool temp, bool free) {
	return new Value(type, Size(type), temp, free);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Value::Size(const Type* type) {
	DebugValidator::IsFalse(type->IsVoid());
	DebugValidator::IsFalse(type->IsFunction());
	
	if(type->IsInteger()) {
		switch(type->As<IntegerType>()->GetSubtype()) {
			case IRIntegerKind::Int8:  return 1;
			case IRIntegerKind::Int16: return 2;
			case IRIntegerKind::Int32: return 4;
			case IRIntegerKind::Int64: return 8;
		}
	}
	else if(type->IsFloating()) {
		if(type->IsFloat()) return 4;
		else return 8;
	}
	else if(type->IsPointer()) {
		return sizeof(char*);
	}
	else if(auto temp = type->As<ArrayType>()) {
		return (int)temp->Size() * Size(temp->ElementType());
	}
	else if(auto temp = type->As<RecordType>()) {
		// The size of the record is the maximum offset +
		// the size of the field with maximum size at that offset.
		auto& fields = temp->Fields();
		__int64 maxOffset = fields[fields.Count() - 1].FieldOffset;
		__int64 maxSize = -1;

		for(int i = 0; i < fields.Count(); i++) {
			if((fields[i].FieldOffset == maxOffset) &&
				(Size(fields[i].FieldType) > maxSize)) {
				maxSize = Size(fields[i].FieldType);
			}
		}

		return (int)(maxOffset + maxSize);
	}

	DebugValidator::Unreachable();
	return 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__int64 Value::AsInteger(char* data, const Type* type) {  
	DebugValidator::IsTrue(type->IsInteger());
	
	switch(type->As<IntegerType>()->GetSubtype()) {
		case IRIntegerKind::Int8:  { return *((char*)data);    }
		case IRIntegerKind::Int16: { return *((short*)data);   }
		case IRIntegerKind::Int32: { return *((int*)data);     }
		case IRIntegerKind::Int64: { return *((__int64*)data); }
	}

	return 0; // To silence warning.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
double Value::AsFloating(char* data, const Type* type) {
	DebugValidator::IsTrue(type->IsFloating());
	if(type->IsFloat()) return *((float*)data);
	else return *((double*)data);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
char* Value::AsPointer(char* data, const Type* type) {
	DebugValidator::IsTrue(type->IsPointer());
	return (char*)(*((size_t*)data));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Value::SetInteger(char* data, const Type* type, __int64 value) {
	DebugValidator::IsTrue(type->IsInteger());
	
	switch(type->As<IntegerType>()->GetSubtype()) {
		case IRIntegerKind::Int8:  { *((char*)data)    = (char)value;  break; }
		case IRIntegerKind::Int16: { *((short*)data)   = (short)value; break; }
		case IRIntegerKind::Int32: { *((int*)data)     = (int)value;   break; }
		case IRIntegerKind::Int64: { *((__int64*)data) = value; break; }
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Value::SetFloating(char* data, const Type* type, double value) {
	DebugValidator::IsTrue(type->IsFloating());
	if(type->IsFloat()) *((float*)data) = (float)value;
	else *((double*)data) = value;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Value::SetData(char* data, void* other, int size) {
	DebugValidator::IsNotNull(data);
	DebugValidator::IsNotNull(other);
	memcpy(data, other, size);
}

// ######################################################################################
// Interpreter
// ######################################################################################
void Interpreter::ApplyInitializer(char* data, const Type* type, 
                                   Initializer* initializer, bool isInitList) {
	DebugValidator::IsNotNull(data);
	DebugValidator::IsNotNull(initializer);
	
	if(type->IsInteger()) {
		DebugValidator::IsFalse(isInitList);
		Value::SetInteger(data, type, initializer->Value()->As<IntConstant>()->Value());
	}
	else if(type->IsFloating()) {
		DebugValidator::IsFalse(isInitList);
		Value::SetFloating(data, type, initializer->Value()->As<FloatConstant>()->Value());
	}
	else if(type->IsPointer()) {
		DebugValidator::IsFalse(isInitList);

		// A pointer can be initialized with a null constant,
		// with the address of another global variable or with an integer.
		if(initializer->Value()->IsGlobalVariableRef()) {
			Symbol* variable = initializer->Value()->GetSymbol();
			Value::SetData(data, globalVars_[variable]->Data(), Value::Size(type));
		}
		else if(auto intConst = initializer->Value()->As<IntConstant>()) {
			__int64 value = intConst->Value();
			Value::SetData(data, &value, Value::Size(type));
		}
		else {
			// nullptr is equivalent to 0.
			__int64 zero = 0;
			Value::SetData(data, &zero, Value::Size(type));
		}
	}
	else if((initializer->IsInitializerList() == false) && 
             initializer->Value()->IsStringConstant()) {
		auto stringConst = initializer->Value()->As<StringConstant>();
		auto arrayType = type->As<ArrayType>();
		auto value = stringConst->Value();

		for(int i = 0; i < value.Length(); i++) {
			Value::SetInteger(data, arrayType->ElementType(), value[i]);
			data += Value::Size(arrayType->ElementType());
		}

		Value::SetInteger(data, arrayType->ElementType(), 0);
	}
	else if(auto arrayType = type->As<ArrayType>()) {
		auto initList = static_cast<InitializerList*>(initializer);
		auto elementType = arrayType->ElementType();
		DebugValidator::IsTrue(isInitList);
		DebugValidator::IsTrue(initList->Count() == arrayType->Size());

		// Initialize each element of the array.
		for(int i = 0; i < arrayType->Size(); i++) {
			ApplyInitializer(data, elementType, (*initList)[i], 
							 (*initList)[i]->IsInitializerList());
			data += Value::Size(elementType);
		}
	}
	else if(auto recordType = type->As<RecordType>()) {
		auto initList = static_cast<InitializerList*>(initializer);
		auto& fields = recordType->Fields();
		DebugValidator::IsTrue(isInitList);
		DebugValidator::IsTrue(initList->Count() == recordType->Fields().Count());

		// Initialize each field. The address of the field is given by the
		// offset of the field, not by it's size.
		for(int i = 0; i < recordType->FieldCount(); i++) {
			ApplyInitializer(data + fields[i].FieldOffset, fields[i].FieldType,
							 (*initList)[i], (*initList)[i]->IsInitializerList());
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::ApplyZeroInitializer(char* data, const Type* type) {
    DebugValidator::IsNotNull(data);
    std::memset(data, 0, Value::Size(type));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::ApplyInitializer(Value* value, GlobalVariable* variable) {
	if(variable->HasInitializer()) {
        if(variable->HasZeroInitializer()) {
            ApplyZeroInitializer(value->Data(), value->GetType());
        }
        else {
		    ApplyInitializer(value->Data(), value->GetType(), 
						     variable->GetInitializer(), 
                             variable->HasInitializerList());
        }
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::InitializeWithMagic(Value* value) {
	// Initialize the value with some "magic" numbers so that uninitialized
	// variables are easier to recognize.
	__int64 magic = MagicMarker();
	auto type = value->GetType();

	if(type->IsInteger()) {
		value->SetInteger(magic);
	}
	else if(type->IsFloating()) {
		value->SetFloating((double)magic);
	}
	else if(type->IsPointer()) {
		value->SetData(&magic, Value::Size(value->GetType()));
	}
	else if(type->IsArray() || type->IsRecord()) {
		// For arrays and records we set all the used memory to the magic value.
        std::memset(value->Data(), (char)magic, Value::Size(type));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::PushContext() {
	frameStack_.Push(StackFrame(currentFunct_, currentBlock_, 
                                previousBlock_, currentInstr_));
	localVars_.Push(ValueDict());
	tempValues_.Push(TemporaryDict());
    paramValues_.Push(ParameterDict());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::PopContext() {
	frameStack_.Pop();

	if(frameStack_.Count() > 0) {
		StackFrame old = frameStack_.Peek();
		currentFunct_  = old.FunctionObj;
		currentBlock_  = old.BlockObj;
        previousBlock_ = old.PreviousBlockObj;
		currentInstr_  = old.InstructionObj;
	}

	// Free the memory used by the values.
	localVars_.Peek().ForEachValue([](Value* value) {
		value->Free();
	});
	
	tempValues_.Peek().ForEachValue([](Value* value){
		value->Free();
	});

    paramValues_.Peek().ForEachValue([](Value* value){
        value->Free();
    });

	localVars_.Pop();
	tempValues_.Pop();
    paramValues_.Pop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::UpdateContext() {
	StackFrame& frame = frameStack_.Peek();
	frame.FunctionObj      = currentFunct_;
	frame.BlockObj         = currentBlock_;
    frame.PreviousBlockObj = previousBlock_;
	frame.InstructionObj   = currentInstr_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Interpreter::IsMagic(__int64 value, const IntegerType* type) {
	switch(type->GetSubtype()) {
		case IRIntegerKind::Int8:  return ((char)value  ^ (char)MagicMarker())  == 0;
		case IRIntegerKind::Int16: return ((short)value ^ (short)MagicMarker()) == 0;
		case IRIntegerKind::Int32: return ((int)value   ^ (int)MagicMarker())   == 0;
		case IRIntegerKind::Int64: return (value        ^ MagicMarker())        == 0;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::FreeValue(Value* value) {
	// The value is freed only if it's not one of the temporaries.
	if(value->IsTemporary() == false) {
		value->Free();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(IntConstant* op) {
	Value* value = Value::Get(op->GetType());
	value->SetInteger(op->Value());
	PushOperand(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(FloatConstant* op) {
	Value* value = Value::Get(op->GetType());
	value->SetFloating(op->Value());
	PushOperand(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(NullConstant* op) {
	Value* value = Value::Get(op->GetType());
	value->SetInteger(0);
	PushOperand(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(UndefinedConstant* op) {
	Value* value = Value::Get(op->GetType());
	value->SetInteger(MagicMarker());
	PushOperand(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(FunctionReference* op) {
	Value* value = Value::Get(op->GetType());
	Function* target = op->Target();
	value->SetData((char*)&target, Value::Size(op->GetType()));
	PushOperand(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(BlockReference* op) {
	Value* value = Value::Get(op->GetType());
	Block* target = op->Target();
	value->SetData((char*)&target, sizeof(Block*));
	PushOperand(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(Operand* op) {
	if(op->IsLocalVariableRef()) {
		// Search the variable in the local table.
		DebugValidator::IsTrue(localVars_.Peek().ContainsKey(op->GetSymbol()));
		Value* varHolder = Value::Get(op->GetType(), false /* temp */,
                                      false /* free* */);
		Value* variable = localVars_.Peek()[op->GetSymbol()];

		varHolder->SetData(variable->DataAddress(), 
                           Value::Size(op->GetType()));
		PushOperand(varHolder);
	}
	else if(op->IsGlobalVariableRef()) {
		// Search the variable in the global table.
		DebugValidator::IsTrue(globalVars_.ContainsKey(op->GetSymbol()));
		Value* varHolder = Value::Get(op->GetType(), false /* temp */, 
                                      false /* free* */);
		Value* variable = globalVars_[op->GetSymbol()];

		varHolder->SetData(variable->DataAddress(), 
                           Value::Size(op->GetType()));
		PushOperand(varHolder);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(Temporary* op) {
	// See if the temporary has already a value.
	if(tempValues_.Peek().ContainsKey(op)) {
		PushOperand(tempValues_.Peek()[op]);
	}
	else {
		// Create a new temporary and place it in the map.
		Value* temp = Value::Get(op->GetType(), true /* temp */);
		InitializeWithMagic(temp);
		PushOperand(temp);
		tempValues_.Peek().Add(op, temp);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(Parameter* op) {
    // See if the parameter has already a value.
    if(paramValues_.Peek().ContainsKey(op)) {
        PushOperand(paramValues_.Peek()[op]);
    }
    else {
        // Create a new parameter, copy the value from the variable
        // and then place it in the map.
        auto parameterType = op->GetType();
        Value* parameterValue = Value::Get(parameterType, true /* temp */);

        op->GetVariable()->GetReference()->Accept(this);
        auto paramVariableValue = PopOperand();

        char* data = paramVariableValue->AsPointer();
        auto resultType = paramVariableValue->GetType();

        if(resultType->IsInteger()) {
            parameterValue->SetInteger(Value::AsInteger(data, resultType));
        }
        else if(resultType->IsFloating()) {
            parameterValue->SetFloating(Value::AsFloating(data, resultType));
        }
        else parameterValue->SetData(data, Value::Size(resultType));

        FreeValue(paramVariableValue);
        PushOperand(parameterValue);
        paramValues_.Peek().Add(op, parameterValue);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(ArithmeticInstr* instr) {
	// Evaluate the operands first, then apply the required operation.
	if(instr->ResultOp()) {
        instr->ResultOp()->Accept(this);
    }

	instr->RightOp()->Accept(this);
	instr->LeftOp()->Accept(this);

	Value* leftValue = PopOperand();
	Value* rightValue = PopOperand();

	if(instr->IsIntArithmetic()) {
		__int64 left = leftValue->AsInteger();
		__int64 right = rightValue->AsInteger();
		__int64 result = 0;

		switch(instr->GetOpcode()) {
			case Opcode::Add: { result = left + right; break; }
			case Opcode::Sub: { result = left - right; break; }
			case Opcode::Mul: { result = left * right; break; }
			case Opcode::Div: { result = left / right; break; }
			case Opcode::Udiv: { 
				result = (__int64)(unsigned __int64)left / (unsigned __int64)right; 
				break; 
			}
			case Opcode::Mod: { result = left % right; break; }
			case Opcode::Umod: {
				result = (__int64)(unsigned __int64)left % (unsigned __int64)right; 
				break; 
			}
		}

		if(instr->ResultOp()) {
			PopOperand()->SetInteger(result);
		}
	}
	else {
		double right = PopOperand()->AsFloating();
		double left = PopOperand()->AsFloating();
		double result = 0;
		
		switch(instr->GetOpcode()) {
			case Opcode::Fadd: { result = left + right; break; }
			case Opcode::Fsub: { result = left - right; break; }
			case Opcode::Fmul: { result = left * right; break; }
			case Opcode::Fdiv: { result = left / right; break; }
		}

		if(instr->ResultOp()) {
			PopOperand()->SetFloating(result);
		}
	}

	// The values can be freed now.
	FreeValue(leftValue);
	FreeValue(rightValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(LogicalInstr* instr) {
	// Evaluate the operands first, then apply the logical operation.
	if(instr->ResultOp()) {
        instr->ResultOp()->Accept(this);
    }

	instr->RightOp()->Accept(this);
	instr->LeftOp()->Accept(this);

	Value* leftValue = PopOperand();
	Value* rightValue = PopOperand(); 
	__int64 left = leftValue->AsInteger();
	__int64 right = rightValue->AsInteger();
	__int64 result = 0;

	switch(instr->GetOpcode()) {
		case Opcode::And:  { result = left &  right; break; }
		case Opcode::Or:   { result = left |  right; break; }
		case Opcode::Xor:  { result = left ^  right; break; }
		case Opcode::Shl:  { result = left << right; break; }
		case Opcode::Shr:  { result = left >> right; break; }
		case Opcode::Ushr: {
			result = (__int64)((unsigned __int64)left >> (unsigned __int64)right);
			break;
		}
	}

	// The values can be freed now.
	FreeValue(leftValue);
	FreeValue(rightValue);

	if(instr->ResultOp()) {
		PopOperand()->SetInteger(result);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(ConversionInstr* instr) {
	// Evaluate the operand first, than apply the conversion.
	if(instr->ResultOp()) {
        instr->ResultOp()->Accept(this);
    }

	instr->TargetOp()->Accept(this);
	Value* targetValue = PopOperand();

	switch(instr->GetOpcode()) {
		case Opcode::Zext: {
			// Nothing needs to be done in this cases.
			if(instr->ResultOp()) {
				__int64 result= targetValue->AsInteger();
				PopOperand()->SetInteger(result);
			}

			break;
		}
		case Opcode::Sext: {
			// We perform sign extension by shifting the value to the left,
			// than to the right with sign extension enabled.
			auto intType = instr->CastType()->As<IntegerType>();
			__int64 result = 0;
			__int64 shiftLeft = 0;
			__int64 bitWidth = sizeof(__int64) * 8;

			switch(intType->GetSubtype()) {
				case IRIntegerKind::Int8:  { shiftLeft = bitWidth - 8;  break; }
				case IRIntegerKind::Int16: { shiftLeft = bitWidth - 16; break; }
				case IRIntegerKind::Int32: { shiftLeft = bitWidth - 32; break; }
			}

			result = (targetValue->AsInteger() << shiftLeft) >> shiftLeft;
			if(instr->ResultOp()) {
				PopOperand()->SetInteger(result);
			}

			break;
		}
		case Opcode::Trunc: {
			// We truncate by applying a mask to the target operand.
			auto intType = instr->CastType()->As<IntegerType>();
			__int64 result = 0;
			__int64 shiftLeft = 0;
			__int64 bitWidth = sizeof(__int64) * 8;
			__int64 mask;

			switch(intType->GetSubtype()) {
				case IRIntegerKind::Int8:  { shiftLeft = bitWidth - 8;  mask = 0xFF;     break; }
				case IRIntegerKind::Int16: { shiftLeft = bitWidth - 16; mask = 0xFFFF;   break; }
				case IRIntegerKind::Int32: { shiftLeft = bitWidth - 32; mask = 0xFFFFFF; break; }
			}

			// Note that this is needed to preserve the sign.
			result = ((targetValue->AsInteger() << shiftLeft) >> shiftLeft) & mask;

			if(instr->ResultOp()) {
				PopOperand()->SetInteger(result);
			}

			break;
		}
		case Opcode::Ftoi: {
			// The value is converted to an integer and a mask is applied 
			// to restrict the domain.
			auto intType = instr->CastType()->As<IntegerType>();
			double target = targetValue->AsFloating();
			__int64 result = 0;

			switch(intType->GetSubtype()) {
				case IRIntegerKind::Int8:  { result = (__int64)target & 0xFF;             break; }
				case IRIntegerKind::Int16: { result = (__int64)target & 0xFFFF;           break; }
				case IRIntegerKind::Int32: { result = (__int64)target & 0xFFFFFF;         break; }
				case IRIntegerKind::Int64: { result = (__int64)target & 0xFFFFFFFFFFFFFF; break; }
			}

			if(instr->ResultOp()) {
				PopOperand()->SetInteger(result);
			}

			break;
		}
		case Opcode::Ftoui: {
			// The value is converted to an integer and a mask is applied 
			// to restrict the domain.
			auto intType = instr->CastType()->As<IntegerType>();
			double target = targetValue->AsFloating();
			unsigned __int64 result = 0;

			switch(intType->GetSubtype()) {
				case IRIntegerKind::Int8:  { result = (unsigned __int64)target & 0xFF;             break; }
				case IRIntegerKind::Int16: { result = (unsigned __int64)target & 0xFFFF;           break; }
				case IRIntegerKind::Int32: { result = (unsigned __int64)target & 0xFFFFFF;         break; }
				case IRIntegerKind::Int64: { result = (unsigned __int64)target & 0xFFFFFFFFFFFFFF; break; }
			}

			if(instr->ResultOp()) {
				PopOperand()->SetInteger((__int64)result);
			}

			break;
		}
		case Opcode::Itof: {
			// We truncate to the appropriate floating type.
			double result = 0;

			if(instr->CastType()->IsDouble()) {
				result = (double)targetValue->AsInteger();
			}
			else result = (float)targetValue->AsInteger();

			if(instr->ResultOp()) {
				PopOperand()->SetFloating(result);
			}

			break;
		}
		case Opcode::Uitof: {
			double result = 0;

			if(instr->CastType()->IsDouble()) {
				result = (double)(unsigned __int64)targetValue->AsInteger();
			}
			else result = (float)(unsigned __int64)targetValue->AsInteger();

			if(instr->ResultOp()) {
				PopOperand()->SetFloating(result);
			}

			break;
		}
		case Opcode::Ftrunc: {
			double result = targetValue->AsFloating();
			PopOperand()->SetFloating((float)result);
		}
		case Opcode::Ptoi: {
			// Convert the pointer to an integer and truncate to the appropriate size.
			auto intType = instr->CastType()->As<IntegerType>();
			__int64 target = *((__int64*)targetValue->Data());
			__int64 result = 0;

			switch(intType->GetSubtype()) {
				case IRIntegerKind::Int8:  { result = (unsigned __int64)target & 0xFF;             break; }
				case IRIntegerKind::Int16: { result = (unsigned __int64)target & 0xFFFF;           break; }
				case IRIntegerKind::Int32: { result = (unsigned __int64)target & 0xFFFFFF;         break; }
				case IRIntegerKind::Int64: { result = (unsigned __int64)target & 0xFFFFFFFFFFFFFF; break; }
			}

			if(instr->ResultOp()) {
				PopOperand()->SetInteger((__int64)result);
			}

			break;
		}
		case Opcode::Itop: {
			__int64 target = targetValue->AsInteger();
			PopOperand()->SetData(&target, sizeof(void*));
			break;
		}
		case Opcode::Ptop: {
			char* ptr = targetValue->Data();
			PopOperand()->SetData(ptr, sizeof(void*));
			break;
		}
	}

	// The value can be freed now.
	FreeValue(targetValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(CmpInstr* instr) {
	// Evaluate the operand first, than apply the conversion.
	if(instr->ResultOp()) {
        instr->ResultOp()->Accept(this);
    }

	instr->RightOp()->Accept(this);
	instr->LeftOp()->Accept(this);

	Value* leftValue = PopOperand();
	Value* rightValue = PopOperand();
	__int64 left = leftValue->AsInteger();
	__int64 right = rightValue->AsInteger();
	__int64 result = 0;

	switch(instr->Order()) {
		case OrderType::Less:           { result = left <  right; break; }
		case OrderType::LessOrEqual:    { result = left <= right; break; }
		case OrderType::Greater:        { result = left >  right; break; }
		case OrderType::GreaterOrEqual: { result = left >= right; break; }
		case OrderType::Equal:          { result = left == right; break; }
		case OrderType::NotEqual:       { result = left != right; break; }
	}

	if(instr->ResultOp()) {
		Value* resultValue = PopOperand();
		resultValue->SetInteger(result);
		FreeValue(resultValue);
	}

	// The values can be freed now.
	FreeValue(leftValue);
	FreeValue(rightValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(UcmpInstr* instr) {
	// Evaluate the operand first, than apply the conversion.
	// Note that the operands can also be pointers in this case.
	if(instr->ResultOp()) instr->ResultOp()->Accept(this);
	instr->RightOp()->Accept(this);
	instr->LeftOp()->Accept(this);

	Value* leftValue = PopOperand();
	Value* rightValue = PopOperand();
	unsigned __int64 left = leftValue->AsInteger();
	unsigned __int64 right = rightValue->AsInteger();
	__int64 result = 0;

	if(instr->LeftOp()->IsPointer()) {
		left  = (__int64)leftValue->AsPointer();
		right = (__int64)rightValue->AsPointer();
	}
	else {
		left = leftValue->AsInteger();
		right = rightValue->AsInteger();
	}

	switch(instr->Order()) {
		case OrderType::Less:           { result = left <  right; break; }
		case OrderType::LessOrEqual:    { result = left <= right; break; }
		case OrderType::Greater:        { result = left >  right; break; }
		case OrderType::GreaterOrEqual: { result = left >= right; break; }
		case OrderType::Equal:          { result = left == right; break; }
		case OrderType::NotEqual:       { result = left != right; break; }
	}

	if(instr->ResultOp()) {
		PopOperand()->SetInteger(result);
	}

	// The values can be freed now.
	FreeValue(leftValue);
	FreeValue(rightValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(FcmpInstr* instr) {
	// Evaluate the operand first, than apply the conversion.
	if(instr->ResultOp()) instr->ResultOp()->Accept(this);
	instr->RightOp()->Accept(this);
	instr->LeftOp()->Accept(this);

	Value* leftValue = PopOperand();
	Value* rightValue = PopOperand();
	double left = leftValue->AsFloating();
	double right = rightValue->AsFloating();
	__int64 result = 0;

	switch(instr->Order()) {
		case OrderType::Less:           { result = left <  right; break; }
		case OrderType::LessOrEqual:    { result = left <= right; break; }
		case OrderType::Greater:        { result = left >  right; break; }
		case OrderType::GreaterOrEqual: { result = left >= right; break; }
		case OrderType::Equal:          { result = left == right; break; }
		case OrderType::NotEqual:       { result = left != right; break; }
	}

	if(instr->ResultOp()) {
		PopOperand()->SetInteger(result);
	}

	// The values can be freed now.
	FreeValue(leftValue);
	FreeValue(rightValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(LoadInstr* instr) {
	// Do the load only if the result is used.
	if(instr->ResultOp()) {
		instr->ResultOp()->Accept(this);
		instr->SourceOp()->Accept(this);

		Value* sourceValue = PopOperand();
		Value* resultValue = PopOperand();
		char* data = sourceValue->AsPointer();

		// The only types that can be loaded are integer, floating and pointers.
		auto resultType = resultValue->GetType();

		if(resultType->IsInteger()) {
			resultValue->SetInteger(Value::AsInteger(data, resultType));
		}
		else if(resultType->IsFloating()) {
			resultValue->SetFloating(Value::AsFloating(data, resultType));
		}
		else resultValue->SetData(data, Value::Size(resultType));

		// Source value no longer needed.
		FreeValue(sourceValue);
		FreeValue(resultValue);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(StoreInstr* instr) {
	// Evaluate the source and destination operands.
	instr->DestinationOp()->Accept(this);
	instr->SourceOp()->Accept(this);

	Value* sourceValue = PopOperand();
	Value* destValue = PopOperand();
	char* data = destValue->AsPointer();

	// The only types that can be stored are integer, floating and pointers.
	Value::SetData(data, sourceValue->Data(), Value::Size(sourceValue->GetType()));
	FreeValue(sourceValue);
	FreeValue(destValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(AddressInstr* instr) {
	// A pointer to an array is a special case, because the index operand
	// refers to the element of the array.
	instr->ResultOp()->Accept(this);
	instr->IndexOp()->Accept(this);
	instr->BaseOp()->Accept(this);

	Value* baseValue = PopOperand();
	Value* indexValue = PopOperand();

	auto baseType = baseValue->GetType()->As<PointerType>();
	char* data = baseValue->AsPointer();
	__int64 index = indexValue->AsInteger();

	// result = base_address + (index * pointee_size)
	__int64 size = Value::Size(baseType->PointeeType());
	char* element = data + (index * size);
	PopOperand()->SetData((char*)&element, Value::Size(baseType));

	// The values can be freed now.
	FreeValue(baseValue);
	FreeValue(indexValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(IndexInstr* instr) {
	// A pointer to an array is a special case, because the index operand
	// refers to the element of the array.
	instr->ResultOp()->Accept(this);
	instr->IndexOp()->Accept(this);
	instr->BaseOp()->Accept(this);

	Value* baseValue = PopOperand();
	Value* indexValue = PopOperand();

	auto baseType = baseValue->GetType()->As<PointerType>();
	char* data = baseValue->AsPointer();
	__int64 index = indexValue->AsInteger();

	if(auto arrayType = baseType->PointeeType()->As<ArrayType>()) {
		// result = base_address + (index * elem_size)
		char* element = data + (index * Value::Size(arrayType->ElementType()));
		PopOperand()->SetData((char*)&element, Value::Size(baseType));
	}

	// The values can be freed now.
	FreeValue(baseValue);
	FreeValue(indexValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(FieldInstr* instr) {
	// A pointer to an array is a special case, because the index operand
	// refers to the element of the array.
	instr->ResultOp()->Accept(this);
	instr->IndexOp()->Accept(this);
	instr->BaseOp()->Accept(this);

	Value* baseValue = PopOperand();
	Value* indexValue = PopOperand();

	auto baseType = baseValue->GetType()->As<PointerType>();
	char* data = baseValue->AsPointer();
	__int64 index = indexValue->AsInteger();

	if(auto recordType = baseType->PointeeType()->As<RecordType>()) {
		// result = base_address + field_offset
		__int64 offset = recordType->Fields()[(int)index].FieldOffset;
		char* field = data + offset;
		PopOperand()->SetData((char*)&field, Value::Size(baseType));
	}

	// The values can be freed now.
	FreeValue(baseValue);
	FreeValue(indexValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(Block* block) {
	// We execute each instruction in the block. The last instruction should be
	// a branching instruction that transfers control to another block.
	if(debug_ && breakOnBlock_) {
		handler_->NewBlock(block);
	}

    previousBlock_ = currentBlock_;
	currentBlock_ = block;
	currentInstr_ = block->FirstInstruction();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(IfInstr* instr) {
	instr->ConditionOp()->Accept(this);
	Value* condition = PopOperand();

	if(condition->AsInteger() != 0) {
		// Take the 'true' branch.
		instr->TrueTargetBlock()->Accept(this);
	}
	else {
		// Take the 'false' branch.
		instr->FalseTargetBlock()->Accept(this);
	}

	FreeValue(condition);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(GotoInstr* instr) {
	// Change the block unconditionally.
	instr->TargetOp()->Target()->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::EnterFunction(Function* function, List<Value*>& arguments) {
	// We execute only the first block; the branching instructions at the end
	// of the blocks determine the execution of other blocks.
	currentFunct_ = function;
	currentBlock_ = function->FirstBlock();
	currentInstr_ = currentBlock_->FirstInstruction();
	PushContext();

	// Add the parameters and the variables to the variable table.
	// The parameters are initialized with the argument values.
	auto parameters = function->Parameters();

	for(int i = 0; i < parameters.Count(); i++) {
		auto parameter = parameters[i];
		auto parameterType = parameter->GetType();
		Value* parameterValue = Value::Get(parameterType);

		// If the argument value is integer or floating we make a copy.
		if(parameterType->IsInteger()) {
			parameterValue->SetInteger(arguments[i]->AsInteger());
		}
		else if(parameterType->IsFloating()) {
			parameterValue->SetFloating(arguments[i]->AsFloating());
		}
		else parameterValue->SetData(arguments[i]->Data(), 
                                     Value::Size(parameterType));

		// Add the parameter to the local table.
		localVars_.Peek().Add(parameter, parameterValue);
	}

	// Initialize the local variables. 
	// All variables are initialized with the "magic" value.
	auto& variables = function->Variables();

	for(int i = 0; i < variables.Count(); i++) {
		auto variable = variables[i];
		Value* varValue = Value::Get(variable->GetType());
		InitializeWithMagic(varValue);
		localVars_.Peek().Add(variable, varValue);
	}

	// Notify the handler a new function is beginning.
	if(debug_ && breakOnFunct_) {
		handler_->NewFunction(function);
	}

	if(debug_ && breakOnBlock_) {
		handler_->NewBlock(currentBlock_);
	}

	// Now start executing.
	while(currentInstr_) {
		// Check if there are any breakpoints that must be handled.
		bool handled = HandleBreakpoints();

		// Notify the handler about the new instruction (if it requests so).
		// If a breakpoint was already issued for this instruction we do nothing.
		if(debug_ && breakOnInstr_ && (handled == false)) {
			handler_->NewInstruction(currentInstr_);
		}

		if(exit_) {
			// We were requested to stop execution.
			return;
		}

		// Execute the current instruction. We increment the "instruction pointer"
		// before we execute the instruction because when a call is made the next
		// instruction after 'call' should be put in the context.
		int prevDepth = frameStack_.Count();
		Instruction* instr = currentInstr_;
		currentInstr_ = currentInstr_->NextInstruction();

		instr->Accept(this);

		if(frameStack_.Count() != prevDepth) {
			// The instruction was a return and we must end this function.
			return;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::ExitFunction() {
	PopContext();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Interpreter::HandleBuiltin(CallInstr* instr, Function* function,
                                List<Value*>& arguments) {
	// If the function is defined in this unit it can't be built-in.
	if(function->IsDefinition()) {
		return false;
	}

	// Check if there is a built-in function with the name of the called function.
	shared<BuiltinFunction> builtin;

	if(builtinFunct_.TryGetValue(function->Name(), &builtin)) {
		if(builtin->IsCompatible(function) == false) {
			return false;
		}

		// The function is compatible, execute it.
		returnValue_ = builtin->Execute(function, arguments);
		return true;
	}

    // Handle the usual intrinsics.
    if(auto intrinsic = instr->GetIntrinsic()) {
        return HandleIntrinsic(intrinsic, instr, function, arguments);
    }

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Interpreter::HandleIntrinsic(Intrinsic* intrinsic, CallInstr* instr,
                                  Function* function, List<Value*>& arguments) {
    if(intrinsic->Is<CopyMemoryIntr>()) {
        std::memcpy(arguments[0]->AsPointer(), arguments[1]->AsPointer(),
                    (size_t)arguments[2]->AsInteger());
        return true;
    }
    else if(intrinsic->Is<SetMemoryIntr>()) {
        std::memset(arguments[0]->AsPointer(), (int)arguments[1]->AsInteger(),
                    (size_t)arguments[2]->AsInteger());
        return true;
    }
    else if(intrinsic->Is<PrefetchIntr>()) {
        // Nothing needs to be done.
        return true;
    }
    else if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
        switch(mathIntrinsic->GetMathKind()) {
            case MathIntrinsicKind::Min8: {
                if(arguments[2]->AsInteger() != 0) {
                    __int64 a = arguments[0]->AsInteger() & 0xFF;
                    __int64 b = arguments[1]->AsInteger() & 0xFF;
                    returnValue_->SetInteger(std::min(a, b));
                }
                else {
                    unsigned __int64 a = arguments[0]->AsInteger() & 0xFF;
                    unsigned __int64 b = arguments[1]->AsInteger() & 0xFF;
                    returnValue_->SetInteger(std::min(a, b));
                }
                break;
            }
            case MathIntrinsicKind::Min16: {
                if(arguments[2]->AsInteger() != 0) {
                    __int64 a = arguments[0]->AsInteger() & 0xFFFF;
                    __int64 b = arguments[1]->AsInteger() & 0xFFFF;
                    returnValue_->SetInteger(std::min(a, b));
                }
                else {
                    unsigned __int64 a = arguments[0]->AsInteger() & 0xFFFF;
                    unsigned __int64 b = arguments[1]->AsInteger() & 0xFFFF;
                    returnValue_->SetInteger(std::min(a, b));
                }
                break;
            }
            case MathIntrinsicKind::Min32: {
                if(arguments[2]->AsInteger() != 0) {
                    __int64 a = arguments[0]->AsInteger() & 0xFFFFFF;
                    __int64 b = arguments[1]->AsInteger() & 0xFFFFFF;
                    returnValue_->SetInteger(std::min(a, b));
                }
                else {
                    unsigned __int64 a = arguments[0]->AsInteger() & 0xFFFFFF;
                    unsigned __int64 b = arguments[1]->AsInteger() & 0xFFFFFF;
                    returnValue_->SetInteger(std::min(a, b));
                }
                break;
            }
            case MathIntrinsicKind::Min64: {
                if(arguments[2]->AsInteger() != 0) {
                    __int64 a = arguments[0]->AsInteger() & 0xFFFFFFFFFFFFFF;
                    __int64 b = arguments[1]->AsInteger() & 0xFFFFFFFFFFFFFF;
                    returnValue_->SetInteger(std::min(a, b));
                }
                else {
                    unsigned __int64 a = arguments[0]->AsInteger() & 0xFFFFFFFFFFFFFF;
                    unsigned __int64 b = arguments[1]->AsInteger() & 0xFFFFFFFFFFFFFF;
                    returnValue_->SetInteger(std::min(a, b));
                }
                break;
            }
            case MathIntrinsicKind::Max8: {
                if(arguments[2]->AsInteger() != 0) {
                    __int64 a = arguments[0]->AsInteger() & 0xFF;
                    __int64 b = arguments[1]->AsInteger() & 0xFF;
                    returnValue_->SetInteger(std::max(a, b));
                }
                else {
                    unsigned __int64 a = arguments[0]->AsInteger() & 0xFF;
                    unsigned __int64 b = arguments[1]->AsInteger() & 0xFF;
                    returnValue_->SetInteger(std::max(a, b));
                }
                break;
            }
            case MathIntrinsicKind::Max16: {
                if(arguments[2]->AsInteger() != 0) {
                    __int64 a = arguments[0]->AsInteger() & 0xFFFF;
                    __int64 b = arguments[1]->AsInteger() & 0xFFFF;
                    returnValue_->SetInteger(std::max(a, b));
                }
                else {
                    unsigned __int64 a = arguments[0]->AsInteger() & 0xFFFF;
                    unsigned __int64 b = arguments[1]->AsInteger() & 0xFFFF;
                    returnValue_->SetInteger(std::max(a, b));
                }
                break;
            }
            case MathIntrinsicKind::Max32: {
                if(arguments[2]->AsInteger() != 0) {
                    __int64 a = arguments[0]->AsInteger() & 0xFFFFFF;
                    __int64 b = arguments[1]->AsInteger() & 0xFFFFFF;
                    returnValue_->SetInteger(std::max(a, b));
                }
                else {
                    unsigned __int64 a = arguments[0]->AsInteger() & 0xFFFFFF;
                    unsigned __int64 b = arguments[1]->AsInteger() & 0xFFFFFF;
                    returnValue_->SetInteger(std::max(a, b));
                }
                break;
            }
            case MathIntrinsicKind::Max64: {
                if(arguments[2]->AsInteger() != 0) {
                    __int64 a = arguments[0]->AsInteger() & 0xFFFFFFFFFFFFFF;
                    __int64 b = arguments[1]->AsInteger() & 0xFFFFFFFFFFFFFF;
                    returnValue_->SetInteger(std::max(a, b));
                }
                else {
                    unsigned __int64 a = arguments[0]->AsInteger() & 0xFFFFFFFFFFFFFF;
                    unsigned __int64 b = arguments[1]->AsInteger() & 0xFFFFFFFFFFFFFF;
                    returnValue_->SetInteger(std::max(a, b));
                }
                break;
            }
            case MathIntrinsicKind::Abs8: {
                returnValue_->SetInteger(std::abs(arguments[0]->AsInteger() & 0xFF));
                break;
            }
            case MathIntrinsicKind::Abs16: {
                returnValue_->SetInteger(std::abs(arguments[0]->AsInteger() & 0xFFFF));
                break;
            }
            case MathIntrinsicKind::Abs32: {
                returnValue_->SetInteger(std::abs(arguments[0]->AsInteger() & 0xFFFFFF));
                break;
            }
            case MathIntrinsicKind::Abs64: {
                returnValue_->SetInteger(std::abs(arguments[0]->AsInteger()));
                break;
            }
            case MathIntrinsicKind::Fabs: {
                returnValue_->SetFloating(std::fabs(arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Atan: {
                returnValue_->SetFloating(std::atan(arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Exp: {
                returnValue_->SetFloating(std::exp(arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Pow: {
                returnValue_->SetFloating(std::pow(arguments[0]->AsFloating(),
                                                   arguments[1]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Log10: {
                returnValue_->SetFloating(std::log10(arguments[0]->AsFloating()));
                break;
                               }
            case MathIntrinsicKind::Sqrt: {
                returnValue_->SetFloating(std::sqrt(arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Sqrtf: {
                returnValue_->SetFloating(std::sqrtf((float)arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Log: {
                returnValue_->SetFloating(std::log(arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Sin: {
                returnValue_->SetFloating(std::sin(arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Tan: {
                returnValue_->SetFloating(std::tan(arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Cos: {
                returnValue_->SetFloating(std::cos(arguments[0]->AsFloating()));
                break;
            }
            case MathIntrinsicKind::Atan2: {
                returnValue_->SetFloating(std::atan2(arguments[0]->AsFloating(),
                                                     arguments[1]->AsFloating()));
                break;
            }
        }

        return true;
    }

    // Not a recognized intrinsic.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(CallInstr* instr) {
	// First obtain the function to be called.
	instr->TargetOp()->Accept(this);
	Value* functionValue = PopOperand();
	Function* function = reinterpret_cast<Function*>(functionValue->AsPointer());

	if(instr->ResultOp()) {
		instr->ResultOp()->Accept(this);
	}

	// Create a new context and push the arguments.
	List<Value*> arguments(instr->ArgumentCount());
	auto callArguments = instr->Arguments();

	if(callArguments) {
		for(int i = 0; i < callArguments->Count(); i++) {
			(*callArguments)[i]->Accept(this);
			arguments.Add(PopOperand());
		}
	}

	// Call the function, then retrieve the returned value (if any).
	// First we verify if it's a built-in function.
	if(HandleBuiltin(instr, function, arguments) == false) {
		if(function->IsDefinition() == false) {
			if(debug_) handler_->UndefinedFunctionCall(function);
			else exit(0);

			return;
		}
		else {
			UpdateContext();
			EnterFunction(function, arguments);
		}
	}

	if(instr->ResultOp()) {
		if(returnValue_->GetType()->IsInteger()) {
			PopOperand()->SetInteger(returnValue_->AsInteger());
		}
		else if(returnValue_->GetType()->IsFloating()) {
			PopOperand()->SetFloating(returnValue_->AsFloating());
		}
		else PopOperand()->SetData(returnValue_->Data(), 
                                   Value::Size(returnValue_->GetType()));
	}

	// Free the argument values and target.
	for(int i = 0; i < arguments.Count(); i++) {
		FreeValue(arguments[i]);
	}
	
	if(returnValue_) {
		returnValue_->Free();
		returnValue_ = nullptr;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(ReturnInstr* instr) {
	// If a value is returned we evaluate the operand.
	if(instr->IsVoid()) {
		returnValue_ = nullptr;
	}
	else {
		// Make a copy of the value. The caller should free the value.
		instr->ReturnedOp()->Accept(this);
		Value* result = PopOperand();
		returnValue_ = Value::Get(result->GetType());
		returnValue_->SetData(result->Data(), Value::Size(result->GetType()));
	}

	// Exit from the function.
	ExitFunction();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(SwitchInstr* instr) {
	// Evaluate the condition operand first.
	instr->ConditionOp()->Accept(this);
	Value* condValue = PopOperand();
	__int64 cond = condValue->AsInteger();

	// Find the target to be used. If no one is found the default target is used.
	auto list = instr->CaseList();
	BlockReference* target = nullptr;

	for(int i = 0; i < list.Count(); i++) {
		if(list[i].Value == cond) {
			target = list[i].Target;
			break;
		}
	}

	// Use the default target if needed.
	if(target == nullptr) {
		target = instr->DefaultTargetOp();
	}

	// Jump to the target block.
	target->Target()->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(QuestionInstr* instr) {
    if(instr->ResultOp()) {
        instr->ResultOp()->Accept(this);
    }

    // Evaluate the condition operand.
    instr->ConditionOp()->Accept(this);
    Value* condValue = PopOperand();
	__int64 cond = condValue->AsInteger();

    // Select the value based on the condition.
    Value* result;

    if(cond) {
        // True operand.
        instr->TrueOp()->Accept(this);
        result = PopOperand();
    }
    else {
        // False operand.
        instr->FalseOp()->Accept(this);
        result = PopOperand();
    }

    if(instr->ResultOp()) {
        SetResult(result);
    }

    FreeValue(result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Visit(PhiInstr* instr) {
    // Get the operand that is incoming based on the block
    // from which we obtained control.
    auto incomingOp = instr->GetOperandFromBlock(previousBlock_);

    if(incomingOp) {
        incomingOp->Accept(this);
        Value* incomingValue = PopOperand();

        if(instr->ResultOp()) {
            instr->ResultOp()->Accept(this);
            SetResult(incomingValue);
        }

        FreeValue(incomingValue);
    }
    else {
        // There is no incoming operand (the 'phi' is invalid).
        if(handler_) {
            handler_->InvalidPhi(instr);
        }
        else exit(0);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::SetResult(Value* result) {
    if(result->GetType()->IsInteger()) {
        PopOperand()->SetInteger(result->AsInteger());
    }
    else if(result->GetType()->IsFloating()) {
        PopOperand()->SetFloating(result->AsFloating());
    }
    else {
        __int64 target = result->AsInteger();
        PopOperand()->SetData(&target, sizeof(void*));
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Interpreter::HandleBreakpoints() {
	if(debug_ == false) return false;

	// Walk the list of breakpoints and notify about the first one that is hit.
	for(int i = 0; i < breakpoints_.Count(); i++) {
		auto& bp = breakpoints_[i];

		if((bp.BreakFunction != currentFunct_) ||
			(bp.BreakBlock != currentBlock_)   ||
			(bp.BreakInstruction != currentInstr_)) {
			continue;
		}

		if(bp.BreakHits == -1) {
			// This breakpoint should be always taken.
			handler_->BreakpointHit(&bp);
			return true;
		}
		else if(++bp.HitCount == bp.BreakHits) {
			// The hit count has been reached.
			handler_->BreakpointHit(&bp);
			return true;
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Interpreter::Interpreter(Unit* unit, DebugHandler* handler) : 
		unit_(unit), handler_(handler), currentFunct_(nullptr), 
        currentBlock_(nullptr), previousBlock_(nullptr),
		currentInstr_(nullptr), returnValue_(nullptr), exit_(false),
		debug_(false), breakOnInstr_(false) {
	DebugValidator::IsNotNull(unit);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Value* Interpreter::Start(Function* function, __int64 argument) {
	DebugValidator::IsNotNull(function);
	
	// Create and initialize all global variables.
	for(auto variable = unit_->Variables().First(); variable; variable = variable->Next) {
		Value* varValue = Value::Get(variable->Value->GetType());

		if(variable->Value->HasInitializer()) {
			ApplyInitializer(varValue, variable->Value);
		}
		else {
			// Initialize with a magic value 
            //(easier to recognize uninitialized variables).
			InitializeWithMagic(varValue);
		}

		globalVars_.Add(variable->Value, varValue);
	}

	// 'EnterFunction' will start a loop that executes the function.
	// If the function expects an integer argument we pass the one 
    // specified by the user.
	List<Value*> arguments;

	if((function->ParameterCount() == 1) && function->Parameters()[0]->IsInteger()) {
		Value* argumentValue = Value::Get(function->Parameters()[0]->GetType());
		argumentValue->SetInteger(argument);
		arguments.Add(argumentValue);

		EnterFunction(function, arguments);
		argumentValue->Free();
	}
	else EnterFunction(function, arguments);

	return returnValue_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::LocalVariables(List<VariableValue>& list, bool includeParams) {
	localVars_.Peek().ForEach([&list, includeParams](ValueDict::TPair& pair) {
		Symbol* symbol = reinterpret_cast<Symbol*>(pair.Key);
		auto variable = symbol->As<Variable>();

		if((variable->IsParameter() == false) || includeParams) {
			list.Add(VariableValue(variable, pair.Value));
		}
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::GlobalVariables(List<VariableValue>& list) {
	globalVars_.ForEach([&list](ValueDict::TPair& pair) {
		Symbol* symbol = reinterpret_cast<Symbol*>(pair.Key);
		list.Add(VariableValue(symbol, pair.Value));
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Temporaries(List<TemporaryValue>& list) {
	tempValues_.Peek().ForEach([&list](TemporaryDict::TPair& pair) {
		list.Add(TemporaryValue(pair.Key, pair.Value));
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::Parameters(List<ParameterValue>& list) {
    paramValues_.Peek().ForEach([&list](ParameterDict::TPair& pair) {
        list.Add(ParameterValue(pair.Key, pair.Value));
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Interpreter::StackTrace(List<StackFrame>& list) {
	// Add the current function first.
	list.Add(CurrentFrame());

	// Pop all frames and add them to the list. Then traverse the list
	// backwards and push them on the frame stack, so that nothing is lost.
	int ct = 0;

	for(int i = 0; i < frameStack_.Count(); i++) {
		list.Add(frameStack_.Peek(i));
		ct++;
	}
}

} // namespace IR