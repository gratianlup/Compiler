// InterpreterDriver.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_INTERPRETER_DRIVER_HPP
#define PC_IR_INTERPRETER_DRIVER_HPP

#include "Interpreter.hpp"
#include "IRPrinter.hpp"
#include "../Abstraction/Platform.hpp"

namespace IR {

class InterpreterDriver : public DebugHandler {
private:
	static const int PROMPT_COLOR    = Abstraction::ConsoleColor::Gray;
	static const int INSTR_COLOR     = Abstraction::ConsoleColor::White;
	static const int FUNCT_COLOR     = Abstraction::ConsoleColor::Magenta;
	static const int BLOCK_COLOR     = Abstraction::ConsoleColor::Yellow;
	static const int INSTR_VAL_COLOR = Abstraction::ConsoleColor::Gray;
	static const int VAR_LIST_COLOR  = Abstraction::ConsoleColor::Green;
	static const int TEMP_LIST_COLOR = Abstraction::ConsoleColor::Cyan;
	static const int NORMAL_COLOR    = Abstraction::ConsoleColor::Gray;
	static const int ERROR_COLOR     = Abstraction::ConsoleColor::Red;
	static const int BREAK_COLOR     = Abstraction::ConsoleColor::Red;
	static const int HIGHLIGHT_COLOR = Abstraction::ConsoleColor::White;

	Interpreter* inter_;
	bool showInstrValues_;
	bool isRunning_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void SetColor(int color) {
		Abstraction::Console::SetTextColor(color);
	}

	// The loop that waits for user commands.
	void CommandLoop();

	// Converts the specified string to an integer.
	int StringToInt(string data);
	
	string DumpValue(Value* value) {
		return DumpValueData(value->Data(), value->GetType());
	}

	// Methods to display various information about the interpreted program.
	string DumpValueData(char* data, const Type* value);
	string DumpArray(char* data, const ArrayType* type, int maxElem = 8);
	string DumpRecord(char* data, const RecordType* type, int maxFields = 8);
	string DumpOperand(Operand* op, int tempIndex = -1, const string* name = nullptr);
	string DumpFrame(Function* function, Block* block, Instruction* instr);

	void DumpBreakpointList();
	void DumpVariableList(string title, List<VariableValue>& list);
	void DumpTemporaryList(List<TemporaryValue>& list);
    void DumpParameterList(List<ParameterValue>& list);
	void DumpInstruction(Instruction* instr, IRPrinter& printer);
	void DumpInstructionValues(Instruction* instr, IRPrinter& printer);
	void DumpInstructionWithValues(Instruction* instr);

	// Prints a list with the commands accepted by the interpreter.
	void ShowHelp();

	// Displays a message indicating that an invalid command was entered.
	void InvalidCommand(string message = "");

	// Debug handler implementation.
	virtual void NewInstruction(Instruction* instr) override;
	virtual void NewBlock(Block* block) override;
	virtual void NewFunction(Function* function) override;
	virtual void BreakpointHit(Breakpoint* bp) override;

	// breakpoint
	// start(function, start_value)
	void ExecuteRun(List<string>& parts);
	void ExecuteBreakOn(List<string>& parts);
	void ExecutePrint(List<string>& parts);
	void ExecuteStack(List<string>& parts);
	void ExecuteBreak(List<string>& parts);
	void ExecuteDelete(List<string>& parts);

	void ExecuteCommand(string command);
	string ReadCommand();

public:
	InterpreterDriver(Interpreter* inter);
};

} // namespace IR
#endif