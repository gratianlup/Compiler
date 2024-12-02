// InterpreterBuiltins.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_BUILTIN_FUNCTIONS_HPP
#define PC_IR_BUILTIN_FUNCTIONS_HPP

#include "Instructions.hpp"
#include "Symbols.hpp"
#include "Unit.hpp"
#include "Operand.hpp"
#include "Constants.hpp"
#include "References.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class Interpreter;
class Value;


// Base class for all functions that are provided by the interpreter.
class BuiltinFunction {
protected:
	string name_;

public:
	BuiltinFunction(const string& name) : name_(name) {}

	virtual ~BuiltinFunction() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Should return the name of the function.
	virtual const string* Name() const {
		return &name_;
	}

	// Should return 'true' if the specified function is compatible
	// with the built-in function and can be executed.
	virtual bool IsCompatible(Function* function) const = 0;

	// Should execute the specified function and return
	// the generated value ('nullptr' if the function returns 'void').
	virtual Value* Execute(Function* function, List<Value*>& arguments) = 0;
};


// The base class for the functions that read a simple type from the console.
// for example 'funct ReadInt32() : int32'
class ConsoleRead : public BuiltinFunction {
private:
	const FunctionType* functType_;
	string prompt_;

protected:
	ConsoleRead(string name, const Type* returnType, string prompt, Unit* unit) :
				BuiltinFunction(name), prompt_(prompt),
				functType_(unit->Types().GetFunction(returnType, nullptr, 0)) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const FunctionType* GetType() {
		return functType_;
	}

	virtual bool IsCompatible(Function* function) const override {
		return function->GetType() == functType_;
	}

	virtual Value* Execute(Function* function, List<Value*>& arguments) override;
};


#define MAKE_READ_INT(TYPE) \
	class Read##TYPE : public ConsoleRead { \
	private: \
		Read##TYPE(string prompt, Unit* unit) : \
				ConsoleRead("Read" #TYPE, IntegerType::Get##TYPE(), prompt, unit) {} \
	public: \
		static Function* Register(Interpreter* inter, string prompt = ">> enter " #TYPE ": "); \
	}


#define MAKE_READ_FLOAT(TYPE) \
	class Read##TYPE : public ConsoleRead { \
	private: \
		Read##TYPE(string prompt, Unit* unit) : \
				ConsoleRead("Read" #TYPE, FloatingType::Get##TYPE(), prompt, unit) {} \
	public: \
		static Function* Register(Interpreter* inter, string prompt = ">> enter " #TYPE ": "); \
	}

MAKE_READ_INT(Int8);
MAKE_READ_INT(Int16);
MAKE_READ_INT(Int32);
MAKE_READ_INT(Int64);
MAKE_READ_FLOAT(Float);
MAKE_READ_FLOAT(Double);
#undef MAKE_READ_INT
#undef MAKE_READ_FLOAT

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// The base class for the functions that writes a simple type to the console.
// for example 'funct PrintInt32(int32) : void'
class ConsolePrint : public BuiltinFunction {
private:
	const FunctionType* functType_;
	string prompt_;

protected:
	ConsolePrint(string name, const Type* sourceType, string prompt, Unit* unit);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const FunctionType* GetType() {
		return functType_;
	}

	virtual bool IsCompatible(Function* function) const override {
		return function->GetType() == functType_;
	}

	virtual Value* Execute(Function* function, List<Value*>& arguments) override;
};
 

#define MAKE_PRINT_INT(TYPE) \
	class Print##TYPE : public ConsolePrint { \
	private: \
		Print##TYPE(string prompt, Unit* unit) : \
				ConsolePrint("Print" #TYPE, IntegerType::Get##TYPE(), prompt, unit) {} \
	public: \
		static Function* Register(Interpreter* inter, string prompt = ">> " #TYPE " value: "); \
	}


#define MAKE_PRINT_FLOAT(TYPE) \
	class Print##TYPE : public ConsolePrint { \
	private: \
		Print##TYPE(string prompt, Unit* unit) : \
				ConsolePrint("Print" #TYPE, FloatingType::Get##TYPE(), prompt, unit) {} \
	public: \
		static Function* Register(Interpreter* inter, string prompt = ">> " #TYPE " value: "); \
	}

MAKE_PRINT_INT(Int8);
MAKE_PRINT_INT(Int16);
MAKE_PRINT_INT(Int32);
MAKE_PRINT_INT(Int64);
MAKE_PRINT_FLOAT(Float);
MAKE_PRINT_FLOAT(Double);
#undef MAKE_PRINT_INT
#undef MAKE_PRINT_FLOAT


// Prints an array in the form 'prompt [E1,E2,....En]'.
// funct PrintArray(int32*, int32) : void
class PrintArray : public BuiltinFunction {
	const FunctionType* functType_;
	string prompt_;

public:
	PrintArray(string name, const Type* sourceType, string prompt, Unit* unit);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const FunctionType* GetType() {
		return functType_;
	}

	virtual bool IsCompatible(Function* function) const override {
		return function->GetType() == functType_;
	}

	virtual Value* Execute(Function* function, List<Value*>& arguments) override;
};


#define MAKE_PRINT_ARRAY(TYPE) \
	class Print##TYPE##Array : public PrintArray { \
	private: \
		Print##TYPE##Array(string prompt, Unit* unit) : \
				PrintArray("Print" #TYPE "Array", IntegerType::Get##TYPE(), prompt, unit) {} \
	public: \
		static Function* Register(Interpreter* inter, string prompt = ">> " #TYPE " array values: "); \
	}


MAKE_PRINT_ARRAY(Int8);
MAKE_PRINT_ARRAY(Int16);
MAKE_PRINT_ARRAY(Int32);
MAKE_PRINT_ARRAY(Int64);
#undef MAKE_PRINT_ARRAY


// Prints a string of 1-byte characters (Unicode not supported).
// funct PrintString(int8*) : void
class PrintString : public BuiltinFunction {
	const FunctionType* functType_;
	string prompt_;

public:
	PrintString(string prompt, Unit* unit);

	static Function* Register(Interpreter* inter, string prompt = ">> String: ");

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const FunctionType* GetType() {
		return functType_;
	}

	virtual bool IsCompatible(Function* function) const override {
		return function->GetType() == functType_;
	}

	virtual Value* Execute(Function* function, List<Value*>& arguments) override;
};


// Reads a string of 1-byte characters (Unicode not supported) 
// and returns how many characters were read.
// funct ReadString(int8*) : int32
class ReadString : public BuiltinFunction {
	const FunctionType* functType_;
	string prompt_;

public:
	ReadString(string prompt, Unit* unit);

	static Function* Register(Interpreter* inter, string prompt = ">> Enter string: ");

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const FunctionType* GetType() {
		return functType_;
	}

	virtual bool IsCompatible(Function* function) const override {
		return function->GetType() == functType_;
	}

	virtual Value* Execute(Function* function, List<Value*>& arguments) override;
};

} // namespace IR
#endif