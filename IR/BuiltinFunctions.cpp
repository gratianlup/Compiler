// BuiltinFunctions.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_BUILTIN_FUNCTIONS_CPP
#define PC_IR_BUILTIN_FUNCTIONS_CPP
#include "InterpreterBuiltins.hpp"
#include "Interpreter.hpp"
#include "../Abstraction/Platform.hpp"
#include <iostream>
using namespace Abstraction;

namespace IR {

Value* ConsoleRead::Execute(Function* function, List<Value*>& arguments) {
	auto type = functType_->ReturnType();

	Console::SetTextColor(ConsoleColor::Green);
	std::wcout<<prompt_.Chars();

	if(auto temp = type->As<IntegerType>()) {
		__int64 value;
		std::wcin>>value;

		// Limit the range of the read value.
		switch(temp->GetSubtype()) {
			case IRIntegerKind::Int8:  { value = value & 0xFF;       break; }
			case IRIntegerKind::Int16: { value = value & 0xFFFF;     break; }
			case IRIntegerKind::Int32: { value = value & 0xFFFFFFFF; break; }
		}

		Value* retValue = Value::Get(temp);
		retValue->SetInteger(value);
		return retValue;
	}
	else {
		double value;
		std::wcin>>value;

		// Limit the range of the read value.
		if(type->IsFloat()) {
			value = (float)value;
		}

		Value* retValue = Value::Get(temp);
		retValue->SetFloating(value);
		return retValue;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#define MAKE_READ(TYPE) \
	Function* Read##TYPE::Register(Interpreter* inter, string prompt) { \
		DebugValidator::IsNotNull(inter); \
		Read##TYPE* function = new Read##TYPE(prompt, inter->GetUnit()); \
		Function* functSym = Function::GetFunction(function->GetType(), *function->Name(), \
												   false /* isDef */, inter->GetUnit()); \
		inter->BuiltinFunctions().Add(function->Name(), function); \
		return functSym; \
	}

MAKE_READ(Int8)
MAKE_READ(Int16)
MAKE_READ(Int32)
MAKE_READ(Int64)
MAKE_READ(Float)
MAKE_READ(Double)
#undef MAKE_READ

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ConsolePrint::ConsolePrint(string name, const Type* sourceType, 
						   string prompt, Unit* unit) :
		BuiltinFunction(name), prompt_(prompt) {
	functType_ = unit->Types().GetFunction(VoidType::GetVoid(), &sourceType, 1);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Value* ConsolePrint::Execute(Function* function, List<Value*>& arguments) {
	auto sourceType = functType_->Parameters()[0];
	Value* sourceValue = arguments[0];

	Console::SetTextColor(ConsoleColor::Green);
	std::wcout<<prompt_.Chars();

	if(sourceType->IsInteger()) {
		std::wcout<<sourceValue->AsInteger()<<"\n";
	}
	else std::wcout<<sourceValue->AsFloating()<<"\n";

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#define MAKE_PRINT_INT(TYPE) \
	Function* Print##TYPE::Register(Interpreter* inter, string prompt) { \
		DebugValidator::IsNotNull(inter); \
		Print##TYPE* function = new Print##TYPE(prompt, inter->GetUnit()); \
		Function* functSym = Function::GetFunction(function->GetType(), *function->Name(), \
												   false /* isDef */, inter->GetUnit()); \
		Variable* variable = Variable::GetVariable(IntegerType::Get##TYPE(), \
                                                   string("src"), functSym); \
		functSym->AddParameter(variable); \
		inter->BuiltinFunctions().Add(function->Name(), function); \
		return functSym; \
	}


MAKE_PRINT_INT(Int8)
MAKE_PRINT_INT(Int16)
MAKE_PRINT_INT(Int32)
MAKE_PRINT_INT(Int64)
#undef MAKE_PRINT_INT


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#define MAKE_PRINT_FLOAT(TYPE) \
	Function* Print##TYPE::Register(Interpreter* inter, string prompt) { \
		DebugValidator::IsNotNull(inter); \
		Print##TYPE* function = new Print##TYPE(prompt, inter->GetUnit()); \
		Function* functSym = Function::GetFunction(function->GetType(), *function->Name(), \
												   false /* isDef */, inter->GetUnit()); \
		Variable* variable = Variable::GetVariable(FloatingType::Get##TYPE(), \
                                                   string("src"), functSym); \
		functSym->AddParameter(variable); \
		inter->BuiltinFunctions().Add(function->Name(), function); \
		return functSym; \
	}


MAKE_PRINT_FLOAT(Float)
MAKE_PRINT_FLOAT(Double)
#undef MAKE_PRINT_FLOAT

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PrintArray::PrintArray(string name, const Type* sourceType, string prompt, Unit* unit) :
		BuiltinFunction(name), prompt_(prompt) {
	// function PrintArray(sourceType*, int32) : void
	auto& types = unit->Types();
	List<const Type*> parameters;

	parameters.Add(types.GetPointer(sourceType));
	parameters.Add(IntegerType::GetInt32());
	functType_ = unit->Types().GetFunction(VoidType::GetVoid(), 
										   parameters.GetInternal(), 2);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Value* PrintArray::Execute(Function* function, List<Value*>& arguments) {
	Console::SetTextColor(ConsoleColor::Green);
	std::wcout<<prompt_.Chars();
	
	auto pointerType = functType_->Parameters()[0]->As<PointerType>();
	auto pointeeType = pointerType->PointeeType();
	Value* sourceValue = arguments[0];
	Value* lengthValue = arguments[1];

	char* dataPtr = sourceValue->AsPointer();
	int length = (int)lengthValue->AsInteger();
	int pointeeSize = Value::Size(pointeeType);
	
	// Display all numbers.
	std::wcout<<"[";

	for(int i = 0; i < length; i++) {
		if(i > 0) std::wcout<<",";
		std::wcout<<Value::AsInteger(dataPtr, pointeeType);
		dataPtr += pointeeSize;
	}

	std::wcout<<"]\n";
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#define MAKE_PRINT_ARRAY(TYPE) \
	Function* Print##TYPE##Array::Register(Interpreter* inter, string prompt) { \
		DebugValidator::IsNotNull(inter); \
		Print##TYPE##Array* function = new Print##TYPE##Array(prompt, inter->GetUnit()); \
		Function* functSym = Function::GetFunction(function->GetType(), *function->Name(), \
												   false /* isDef */, inter->GetUnit()); \
		auto pointerType = inter->GetUnit()->Types().GetPointer(IntegerType::Get##TYPE()); \
		Variable* var1 = Variable::GetVariable(pointerType, string("src"), functSym); \
		Variable* var2 = Variable::GetVariable(IntegerType::GetInt32(), \
                                               string("ct"), functSym); \
		functSym->AddParameter(var1); \
		functSym->AddParameter(var2); \
		inter->BuiltinFunctions().Add(function->Name(), function); \
		return functSym; \
	}


MAKE_PRINT_ARRAY(Int8)
MAKE_PRINT_ARRAY(Int16)
MAKE_PRINT_ARRAY(Int32)
MAKE_PRINT_ARRAY(Int64)
#undef MAKE_PRINT_ARRAY

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PrintString::PrintString(string prompt, Unit* unit) :
		BuiltinFunction("PrintString"), prompt_(prompt) {
	// funct PrintString(int8*) : void
	auto& types = unit->Types();
	List<const Type*> parameters;

	parameters.Add(types.GetPointer(IntegerType::GetInt8()));
	functType_ = unit->Types().GetFunction(VoidType::GetVoid(),
                                           parameters.GetInternal(), 1);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* PrintString::Register(Interpreter* inter, string prompt) {
	DebugValidator::IsNotNull(inter);
	PrintString* function = new PrintString(prompt, inter->GetUnit());
	Function* functSym = Function::GetFunction(function->GetType(), *function->Name(),
											   false /* isDef */, inter->GetUnit());
	auto pointerType = inter->GetUnit()->Types().GetPointer(IntegerType::GetInt8());
	Variable* var1 = Variable::GetVariable(pointerType, string("src"), functSym);
	functSym->AddParameter(var1);
	inter->BuiltinFunctions().Add(function->Name(), function);
	return functSym;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Value* PrintString::Execute(Function* function, List<Value*>& arguments) {
	Console::SetTextColor(ConsoleColor::Green);
	std::wcout<<prompt_.Chars();
	
	auto pointerType = functType_->Parameters()[0]->As<PointerType>();
	char* dataPtr = arguments[0]->AsPointer();
	std::cout<<dataPtr;
	std::cout<<"\n";
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ReadString::ReadString(string prompt, Unit* unit) :
		BuiltinFunction("ReadString"), prompt_(prompt) {
	// funct ReadString(int8*) : int32
	List<const Type*> parameters;

	parameters.Add(unit->Types().GetPointer(IntegerType::GetInt8()));
	functType_ = unit->Types().GetFunction(IntegerType::GetInt32(), 
                                           parameters.GetInternal(), 1);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* ReadString::Register(Interpreter* inter, string prompt) {
	DebugValidator::IsNotNull(inter);
	ReadString* function = new ReadString(prompt, inter->GetUnit());
	Function* functSym = Function::GetFunction(function->GetType(), *function->Name(),
											   false /* isDef */, inter->GetUnit());

	auto pointerType = inter->GetUnit()->Types().GetPointer(IntegerType::GetInt8());
	Variable* var1 = Variable::GetVariable(pointerType, string("dst"), functSym);

	functSym->AddParameter(var1);
	inter->BuiltinFunctions().Add(function->Name(), function);
	return functSym;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Value* ReadString::Execute(Function* function, List<Value*>& arguments) {
	Console::SetTextColor(ConsoleColor::Green);
	std::wcout<<prompt_.Chars();
	
	auto pointerType = functType_->Parameters()[0]->As<PointerType>();
	char* dataPtr = arguments[0]->AsPointer();

	std::cin>>dataPtr;
	Value* retValue = Value::Get(IntegerType::GetInt32());
	retValue->SetInteger(strlen(dataPtr));
	return retValue;
}

} // namespace IR
#endif