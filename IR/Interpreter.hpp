// Interpreter.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_INTERPRETER_HPP
#define PC_INTERPRETER_HPP

#include "Instructions.hpp"
#include "Symbols.hpp"
#include "Unit.hpp"
#include "Operand.hpp"
#include "Temporary.hpp"
#include "Constants.hpp"
#include "References.hpp"
#include "Visitor.hpp"
#include "Intrinsics.hpp"
#include "InterpreterBuiltins.hpp"
#include "IRStatistics.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/Stack.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

// Encapsulates and creates the values used in the interpreter.
class Value {
private:
	const Type* type_; // The type of the value.
	char* data_;       // Pointer to the data of the value.
	unsigned char free_ : 1;
	unsigned char temp_ : 1;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Value(const Type* type, int size, bool temp, bool free) : 
			type_(type), data_(new char[size]), temp_(temp), free_(free) {}

public:
	// Factory methods to construct value objects.
	static Value* Get(const Type* type, bool temp = false, bool free = true);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the size (in bytes) of the specified type.
	static int Size(const Type* type);

	// Frees the memory used by the value.
	void Free() {
		delete[] data_;
		data_ = nullptr;
		delete this;
	}

	// Returns the size (in bytes) of this value.
	int Size() const { 
		return Size(type_);
	}

	// Returns the internal data.
	char* Data() {
		return data_;
	}

	const char* Data() const {
		return data_;
	}

	char** DataAddress() {
		return &data_;
	}

	// Returns 'true' if the value is automatically freed.
	bool Free() const { 
		return free_; 
	}

	void SetFree(bool value) {
		free_ = value;
	}

	// Returns 'true' if the value is a temporary.
	bool IsTemporary() const {
		return temp_;
	}

	// Returns the value data interpreted as an integer, float or value.
	static __int64 AsInteger(char* data, const Type* type);
	static double AsFloating(char* data, const Type* type);
	static char* AsPointer(char* data, const Type* type);

	__int64 AsInteger() {
		return AsInteger(data_, type_);
	}

	double AsFloating() {
		return AsFloating(data_, type_);
	}

	char* AsPointer() {
		return AsPointer(data_, type_);
	}

	// Sets the data to the specified value.
	static void SetData(char* data, void* other, int size);

	void SetData(void* other, int size) {
		SetData(data_, other, size);
	}

	// Sets the data with the value of an integer, float or value.
	static void SetInteger(char* data, const Type* type, __int64 value);
	static void SetFloating(char* data, const Type* type, double value);

	void SetInteger(__int64 value) {
		SetInteger(data_, type_, value);
	}

	void SetFloating(double value) {
		SetFloating(data_, type_, value);
	}

	// Returns the type of the value.
	const Type* GetType() {
		return type_;
	}
};


// Represents a breakpoint, used to stop execution at certain points.
struct Breakpoint {
	Function* BreakFunction;
	Block* BreakBlock;
	Instruction* BreakInstruction;
	int BreakHits;  // -1 = always
	int HitCount;

	Breakpoint() : 
			BreakFunction(nullptr), BreakBlock(nullptr), BreakInstruction(nullptr) {}

	Breakpoint(const Breakpoint& other) :
			BreakFunction(other.BreakFunction), BreakBlock(other.BreakBlock),
			BreakInstruction(other.BreakInstruction), 
			BreakHits(other.BreakHits), HitCount(other.HitCount) {}

	Breakpoint(Function* function, Block* block, Instruction* instr, int hits = -1) :
			BreakFunction(function), BreakBlock(block), 
			BreakInstruction(instr), BreakHits(hits), HitCount(0) {}
};


// Base class that must be implemented by all objects who wish
// to be notified about the debugging events in the interpreter.
class DebugHandler {
public:
	// Called before a new instruction is executed.
	virtual void NewInstruction(Instruction* instr) {};

	// Called before a new block is executed.
	virtual void NewBlock(Block* block) {};

	// Called before a new function is executed.
	virtual void NewFunction(Function* function) {};

	// Called before a breakpoint is hit.
	virtual void BreakpointHit(Breakpoint* bp) {};

	// Called when a function that has only a declaration is called.
	virtual void UndefinedFunctionCall(Function* function) {}

    virtual void InvalidPhi(PhiInstr* instr) {}
};


// Used to return the list of variables and their associated values.
struct VariableValue {
	Symbol* Variable;
	Value* Data;

	VariableValue() : Variable(nullptr), Data(nullptr) {}

	VariableValue(const VariableValue& other) :
			Variable(other.Variable), Data(other.Data) {}

	VariableValue(Symbol* variable, Value* data) : 
			Variable(variable), Data(data) {}
};


// Used to return the list of temporaries and their associated values.
struct TemporaryValue {
	Temporary* Temp;
	Value* Data;

	TemporaryValue() : Temp(nullptr), Data(nullptr) {}

	TemporaryValue(const TemporaryValue& other) :
			Temp(other.Temp), Data(other.Data) {}

	TemporaryValue(Temporary* temp, Value* data) :
			Temp(temp), Data(data) {}
};


// Used to return the list of parameters and their associated values.
struct ParameterValue {
    Parameter* Param;
    Value* Data;

    ParameterValue() : Param(nullptr), Data(nullptr) {}

    ParameterValue(const ParameterValue& other) :
            Param(other.Param), Data(other.Data) {}

    ParameterValue(Parameter* param, Value* data) :
            Param(param), Data(data) {}
};


// Used to store information about a running function.
struct StackFrame {
	Function*    FunctionObj;
	Block*       BlockObj;
    Block*       PreviousBlockObj;
	Instruction* InstructionObj;

	StackFrame() : FunctionObj(nullptr), BlockObj(nullptr), 
                   PreviousBlockObj(nullptr), InstructionObj(nullptr) {}

	StackFrame(const StackFrame& other) :
			FunctionObj(other.FunctionObj), BlockObj(other.BlockObj),
            PreviousBlockObj(other.PreviousBlockObj),
			InstructionObj(other.InstructionObj) {}

	StackFrame(Function* function, Block* block, Block* previousBlock, 
               Instruction* instr) :
			   FunctionObj(function), BlockObj(block), 
               PreviousBlockObj(previousBlock), InstructionObj(instr) {}
};


class Interpreter : public Visitor {
private:
	typedef Dictionary<Symbol*, Value*> ValueDict;
	typedef Dictionary<Temporary*, Value*> TemporaryDict;
    typedef Dictionary<Parameter*, Value*> ParameterDict;
	typedef Dictionary<const string*, shared<BuiltinFunction>, true> FunctionDict;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Unit* unit_;
	ValueDict globalVars_;
	Stack<ValueDict> localVars_;
	Stack<TemporaryDict> tempValues_;
    Stack<ParameterDict> paramValues_;
	Stack<Value*> opStack_;
	Stack<StackFrame> frameStack_;
	DebugHandler* handler_;
	List<Breakpoint>  breakpoints_;
	FunctionDict builtinFunct_;
	bool exit_;
	bool debug_;
	bool breakOnInstr_;
	bool breakOnBlock_;
	bool breakOnFunct_;

	// These act as an "instruction pointer".
	Function* currentFunct_;
	Block* currentBlock_;
    Block* previousBlock_;
	Instruction* currentInstr_;
	Value* returnValue_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes the value with the specified initializer.
	void ApplyInitializer(Value* value, GlobalVariable* variable);
	
    void ApplyInitializer(char* data, const Type* type, Initializer* variable, 
						  bool isInitList = false);

    void ApplyZeroInitializer(char* data, const Type* type);

	// Writes a "magic" value so that uninitialized variables can be easily recognized.
	void InitializeWithMagic(Value* value);

	// Pushed the specified operand on the stack.
	void PushOperand(Value* value) {
		opStack_.Push(value);
	}

	// Pops an operand from the stack.
	Value* PopOperand() {
		return opStack_.Pop();
	}

    Value* PeekOperand() {
        return opStack_.Peek();
    }

	// Pushes the current execution context on the stack.
	void PushContext();

	// Pops the previous execution context from the stack and makes the active one.
	void PopContext();

	void UpdateContext();

	// Frees the memory used by the specified value.
	void FreeValue(Value* value);

	// Executes the specified function, after it prepares the local variables.
	void EnterFunction(Function* function, List<Value*>& arguments);

	// Frees the resources used by the function and restores the previous state.
	void ExitFunction();

	// Returns 'true' if any of the breakpoint was handled.
	bool HandleBreakpoints();

	// Verifies if the specified function is built-in, and executes it if it's the case.
	bool HandleBuiltin(CallInstr* instr, Function* function, 
                       List<Value*>& arguments);

    // Executes the most usual intrinsics.
    bool HandleIntrinsic(Intrinsic* intrinsic, CallInstr* instr,
                         Function* function, List<Value*>& arguments);

    void SetResult(Value* result);

	// Visitor methods used to evaluate the instructions.
	virtual void Visit(Operand*           op) override;
	virtual void Visit(Temporary*         op) override;
    virtual void Visit(Parameter*         op) override;
	virtual void Visit(FunctionReference* op) override;
	virtual void Visit(BlockReference*    op) override;
	virtual void Visit(IntConstant*       op) override;
	virtual void Visit(FloatConstant*     op) override;
	virtual void Visit(NullConstant*      op) override;
	virtual void Visit(UndefinedConstant* op) override;
	virtual void Visit(Block* function) override;

	virtual void Visit(ArithmeticInstr* instr) override;
	virtual void Visit(ConversionInstr* instr) override;
	virtual void Visit(LogicalInstr*    instr) override;
	virtual void Visit(CmpInstr*        instr) override;
	virtual void Visit(UcmpInstr*       instr) override;
	virtual void Visit(FcmpInstr*       instr) override;
	virtual void Visit(LoadInstr*       instr) override;
	virtual void Visit(StoreInstr*      instr) override;
	virtual void Visit(AddressInstr*    instr) override;
	virtual void Visit(IndexInstr*      instr) override;
	virtual void Visit(FieldInstr*      instr) override;
	virtual void Visit(IfInstr*         instr) override;
	virtual void Visit(GotoInstr*       instr) override;
	virtual void Visit(SwitchInstr*     instr) override;
	virtual void Visit(ReturnInstr*     instr) override;
	virtual void Visit(CallInstr*       instr) override;
    virtual void Visit(QuestionInstr*   instr) override;
    virtual void Visit(PhiInstr*        instr) override;

public:
	Interpreter(Unit* unit, DebugHandler* handler = nullptr);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Unit* GetUnit() {
		return unit_;
	}

	// Returns 'true' if the interpreter runs in 'debug' mode.
	bool IsDebug() const {
		return debug_;
	}

	void SetIsDebug(bool value) {
		debug_ = value;
	}

	// Returns the associated debug handler object.
	DebugHandler* Hanlder() {
		return handler_;
	}

	void SetHandler(DebugHandler* value) {
		handler_ = value;
	}

	// Start interpreting the specified function, passing the specified argument.
	// Returns the value returned by the function (must be freed by caller).
	Value* Start(Function* function, __int64 argument = 0);

	// Returns the current stack frame (function, block and instruction).
	StackFrame CurrentFrame() {
		// The first function has no stack frame on the stack, so we create one now.
		return StackFrame(currentFunct_, currentBlock_, 
                          previousBlock_, currentInstr_);
	}

	// Returns the number used to mark uninitialized values.
	__int64 MagicMarker() const { 
		return 0xBABABABABABABABA;
	}

	// Returns 'true' if the value could be the "magic" value.
	bool IsMagic(__int64 value, const IntegerType* type);

	// Returns 'true' if the interpreter breaks on each new instruction.
	bool BreakOnInstruction() const { 
		return breakOnInstr_; 
	}

	void SetBreakOnInstruction(bool value) { 
		breakOnInstr_ = value; 
	}

	// Returns 'true' if the interpreter breaks on each new block.
	bool BreakOnBlock() const { 
		return breakOnBlock_; 
	}

	void SetBreakOnBlock(bool value) { 
		breakOnBlock_ = value; 
	}

	// Returns 'true' if the interpreter breaks on each new function.
	bool BreakOnFunction() const { 
		return breakOnFunct_; 
	}

	void SetBreakOnFunction(bool value) { 
		breakOnFunct_ = value; 
	}

	// If set to 'true', the interpreter will abort execution.
	void SetExit(bool value) { 
		exit_ = value; 
	}

	// Returns the list with the active breakpoints.
	List<Breakpoint>& Breakpoints() {
		return breakpoints_;
	}

	// Returns a list with all the variables/parameter variables
    // and their value in the current function.
	void LocalVariables(List<VariableValue>& list, bool includeParams = true);

	// Returns a list with all global variables and their value.
	void GlobalVariables(List<VariableValue>& list);

	// Returns a list with all the temporaries
    // and their value in the current function.
	void Temporaries(List<TemporaryValue>& list);

    // Returns a list with all the parameters
    // and their value in the current function.
    void Parameters(List<ParameterValue>& list);

	// Returns a list with all active stack frames.
	// The current function is first in the list.
	void StackTrace(List<StackFrame>& list);

	// Returns the value associated with the specified local variable.
	Value* GetGlobalValue(Symbol* symbol) {
		Value* value;

		if(globalVars_.TryGetValue(symbol, &value)) {
			return value;
		}
		else return nullptr;
	}

	// Returns the value associated with the specified local variable.
	Value* GetLocalValue(Symbol* symbol) {
		Value* value;

		if(localVars_.Peek().TryGetValue(symbol, &value)) {
			return value;
		}
		else return nullptr;
	}

	// Returns the value associated with the specified temporary.
	Value* GetTemporaryValue(Temporary* op) {
		Value* value;

		if(tempValues_.Peek().TryGetValue(op, &value)) {
			return value;
		}
		else return nullptr;
	}

	// Returns the dictionary containing the built-in functions.
	FunctionDict& BuiltinFunctions() {
		return builtinFunct_;
	}
};

} // namespace IR
#endif