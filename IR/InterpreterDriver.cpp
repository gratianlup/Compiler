// InterpreterDriver.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "InterpreterDriver.hpp"
#include "IRGenerator.hpp"
#include "Tags.hpp"
#include <iostream>

namespace IR {

InterpreterDriver::InterpreterDriver(Interpreter* inter) :
		inter_(inter), isRunning_(false), showInstrValues_(true) {
	inter->SetHandler(this);
	inter->SetIsDebug(true);
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::CommandLoop() {
	// We wait until a command is introduced.
	string command;

	while(command.Length() == 0) {
		command = ReadCommand();
		if(isRunning_) break;
	}

	ExecuteCommand(command);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::InvalidCommand(string message) {
	SetColor(ERROR_COLOR);
	std::wcout<<"Invalid command!";

	if(message.Length() > 0) {
		std::wcout<<": "<<message.Chars();
	}

	std::wcout<<"\n\n";
	SetColor(NORMAL_COLOR);
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int InterpreterDriver::StringToInt(string data) {
	// A simple converter, that does no error checking.
	int value = 0;

	for(int i = 0; i < data.Length(); i++) {
		value = (value * 10) + (data[i] - '0');
	}

	return value;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string InterpreterDriver::ReadCommand() {
	// Show a "prompt" so that the user knows a command is expected.
	SetColor(PROMPT_COLOR);
	std::wcout<<">> ";
	std::wcout.flush();

	// Read from the console until Enter is pressed.
	wchar_t buffer[1000];
	std::wcin.getline(buffer, 1000);
	return string(buffer);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::ExecuteCommand(string command) {
	// If the command is empty:
	// running -> do nothing, return
	// not running -> read a new command
	if(command.Trim().Length() == 0) {
		if(isRunning_) return;
		else CommandLoop();
	}

	// Split the command string in it's components.
	// The first word should be the command type.
	List<string> parts;
	command.Split(parts, L" ", 1, 5, StringSplitOptions::RemoveEmptyEntries);
	string type = parts[0];

	if(type == "run") {
		ExecuteRun(parts);
	}
	else if(type == "end") {
		inter_->SetExit(true);
	}
	else if(type == "local") {
		if((isRunning_ == false) || (parts.Count() > 1)) {
			InvalidCommand();
			return;
		}

		List<VariableValue> list;
		inter_->LocalVariables(list);
		DumpVariableList("Local variables", list);
	}
	else if(type == "global") {
		if((isRunning_ == false) || (parts.Count() > 1)) {
			InvalidCommand();
			return;
		}

		List<VariableValue> list;
		inter_->GlobalVariables(list);
		DumpVariableList("Global variables", list);
	}
	else if(type == "temp") {
		if((isRunning_ == false) || (parts.Count() > 1)) {
			InvalidCommand();
			return;
		}

		List<TemporaryValue> list;
		inter_->Temporaries(list);
		DumpTemporaryList(list);
	}
    else if(type == "param") {
        if((isRunning_ == false) || (parts.Count() > 1)) {
            InvalidCommand();
            return;
        }

        List<ParameterValue> list;
        inter_->Parameters(list);
        DumpParameterList(list);
    }
	else if(type == "break") {
		ExecuteBreak(parts);
	}
	else if(type == "step") {
		// We do nothing in this case when the interpreter is running.
		if(isRunning_ == false) {
			InvalidCommand();
			return;
		}
	}
	else if(type == "delete") {
		ExecuteDelete(parts);
	}
	else if(type == "stack") {
		ExecuteStack(parts);
	}
	else if(type == "instr") {
		ExecuteBreakOn(parts);
	}
	else if(type == "block") {
		ExecuteBreakOn(parts);
	}
	else if(type == "funct") {
		ExecuteBreakOn(parts);
	}
	else if(type == "print") {
		ExecutePrint(parts);
	}
	else if(type == "help") {
		ShowHelp();
	}
	else {
		InvalidCommand("Unknown");
		return;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::ExecuteRun(List<string>& parts) {
	// If the interpreter is already running it's an invalid command.
	if(isRunning_ || (parts.Count() > 3)) {
		InvalidCommand();
		return;
	}

	// start funct_name param-opt
	// The function name should follow, then an optional integer parameter.
	// If no function name is given it's considered to be "main".
	string functionName;
	bool hasValue = false;
	int value;

	if(parts.Count() < 2) functionName = "main";
	else functionName = parts[1];

	// Check for the value.
	if(parts.Count() == 3)  {
		hasValue = true;
		value = StringToInt(parts[2]);
	}

	// Try to find the function.
	Symbol* symbol = inter_->GetUnit()->Symbols().Get(&functionName);
	
	if((symbol == nullptr) || (symbol->IsFunction() == false)) {
		// Invalid function name.
		InvalidCommand("Function not found");
		return;
	}

	// Only functions with no parameters are accepted, or with an integer parameter
	// if a value was provided.
	Function* function = symbol->As<Function>();
	Value* result;

	if((function->ParameterCount() > 0) && 
       ((hasValue == false) || (function->ParameterCount() > 1) || 
        (function->ParameterTypes()[0]->IsInteger() == false))) {
		InvalidCommand();
		return;
	}

	// Now start the interpreter!
	isRunning_ = true;

	if(hasValue == false) {
		result = inter_->Start(function);
	}
	else {
		result = inter_->Start(function, value);
	}

	// Check and display the result, it it's the case.
	if(result && (function->IsVoid() == false)) {
		std::wcout<<"\n\nRESULT: "<<result->AsInteger()<<"\n\n";
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::ExecuteBreakOn(List<string>& parts) {
	if(parts.Count() < 2) {
		InvalidCommand();
		return;
	}

	// Get the status to be set.
	bool activate;

	if(parts[1] == "on") activate = true;
	else if(parts[1] == "off") activate = false;
	else {
		InvalidCommand("Invalid switch");
		return;
	}

	if(parts[0] == "funct") {
		inter_->SetBreakOnFunction(activate);
	}
	else if(parts[0] == "block") {
		inter_->SetBreakOnBlock(activate);
	}
	else inter_->SetBreakOnInstruction(activate);

	// Read next command.
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::ExecutePrint(List<string>& parts) {
	if(isRunning_ == false) {
		InvalidCommand("Not running");
		return;
	}

	StackFrame frame = inter_->CurrentFrame();

	if(parts.Count() == 1) {
		// Print the current instruction.
		if(frame.InstructionObj == nullptr) return;
		IRPrinter printer(frame.InstructionObj);
		printer.Dump();
		std::wcout<<"\n";
	}
	else if(parts[1] == "block") {
		// Print the current block.
		IRPrinter printer(frame.BlockObj);
		printer.Dump();
	}
	else if(parts[1] == "funct") {
		// Print the current function.
		IRPrinter printer(frame.FunctionObj);
		printer.Dump();
		std::wcout<<"\n";
	}
	else if(parts[1] == "all") {
		// Print the whole unit.
		IRPrinter printer(inter_->GetUnit());
		printer.Dump();
		std::wcout<<"\n";
	}

	// Read next command.
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::ExecuteStack(List<string>& parts) {
	if((isRunning_ == false) || (parts.Count() > 1)) {
		InvalidCommand();
		return;
	}

	// Print the stack trace, starting with the current function.
	// Format: function.block: instruction_index
	List<StackFrame> frames;

	inter_->StackTrace(frames);
	SetColor(HIGHLIGHT_COLOR);
	std::wcout<<">> Stack trace:\n\n";
	SetColor(NORMAL_COLOR);

	for(int i = 0; i < frames.Count(); i++) {
		auto frame = frames[i];
		string frameStr = DumpFrame(frame.FunctionObj, frame.BlockObj, 
                                    frame.InstructionObj);
		std::wcout<<"   "<<frameStr.Chars()<<"\n";
	}

	// Read next command.
	std::wcout<<"\n";
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::ExecuteBreak(List<string>& parts) {
	if((isRunning_ == false) || (parts.Count() < 2)) {
		InvalidCommand("Expected function name");
		return;
	}

	if(parts[1] == "show") {
		// Show the list with all active breakpoints.
		DumpBreakpointList();
		return;
	}

	// Format: break funct_name funct_block instr_index hit_count
	// If only the function name is given the breakpoint is set at the
	// first block, first instruction in the function.
	// If the block is given the breakpoint is set at it's first instruction.
	string functionName = parts[1];
	string blockName = "";
	int instrIndex = -1;
	int hits = -1;

	if(parts.Count() > 2) {
		blockName = parts[2];

		if(parts.Count() > 3) {
			instrIndex = StringToInt(parts[3]);

			if(parts.Count() > 4) {
				// Minimum hit count for the breakpoint.
				hits = StringToInt(parts[4]);
			}
		}
	}

	// Get the associated objects.
	Function* function;
	Block* block;
	Instruction* instr;

	if(auto functSymbol = inter_->GetUnit()->Symbols().Get(&functionName)) {
		function = functSymbol->As<Function>();
	}
	else {
		InvalidCommand("Function not found");
		return;
	}

	// Select the block.
	if(blockName.Length() > 0) {
		// Find the block.
		if(auto blockSymbol = function->Symbols().Get(&blockName)) {
			block = blockSymbol->As<Block>();
		}
		else {
			InvalidCommand("Block not found");
			return;
		}
	}
	else {
		// Use the first block.
		block = function->FirstBlock();
	}

	// Select the instruction.
	if(instrIndex != -1) {
		// We must count the instruction in order to select the right one.
		int ct = 0;
		Instruction* p;

		for(p = block->FirstInstruction(); p && (ct < instrIndex); 
            p = p->NextInstruction(), ct++);
		
		// If we ran out of instructions the index is invalid.
		if(p) {
            instr = p;
        }
		else {
			InvalidCommand("Invalid instruction index");
			return;
		}
	}
	else {
		// Use the first instruction in the block.
		instr = block->FirstInstruction();
	}

	// Now create the breakpoint and read the next command.
	inter_->Breakpoints().Add(Breakpoint(function, block, instr, hits));
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::ExecuteDelete(List<string>& parts) {
	if((isRunning_ == false) || (parts.Count() < 2)) {
		InvalidCommand("Expected breakpoint index");
		return;
	}

	// Remove the specified breakpoint if in the valid range.
	int index = (int)StringToInt(parts[1]);
	auto& breakPoints = inter_->Breakpoints();

	if((breakPoints.Count() == 0) || (index < 0) || 
       (index >= breakPoints.Count())) {
		InvalidCommand("Invalid index");
		return;
	}
	else breakPoints.RemoveAt(index);

	// Read next command.
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::NewInstruction(Instruction* instr) {
	// Print the instruction and wait for a command.
	DumpInstructionWithValues(instr);
	std::wcout<<"\n";
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::NewBlock(Block* block) {
	// Print the name of the block and wait for a command.
	SetColor(BLOCK_COLOR);
	std::wcout<<">> BLOCK "<<block->Name()->Chars()<<"\n";
	
	// Read next command.
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::NewFunction(Function* function) {
	// Print the name of the block and wait for a command.
	SetColor(FUNCT_COLOR);
	std::wcout<<">> FUNCTION "<<function->Name()->Chars()<<"\n";
	
	// Read next command.
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::BreakpointHit(Breakpoint* bp) {
	// Print details about the breakpoint (location, number), then the instruction.
	string frame = DumpFrame(bp->BreakFunction, bp->BreakBlock, bp->BreakInstruction);
	
	SetColor(BREAK_COLOR);
	std::wcout<<">> BREAKPOINT";
	
	SetColor(NORMAL_COLOR);
	std::wcout<<" at ";

	SetColor(INSTR_COLOR);
	std::wcout<<frame.Chars()<<"\n";
	DumpInstructionWithValues(bp->BreakInstruction);

	// Read next command.
	std::wcout<<"\n\n";
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::DumpInstructionWithValues(Instruction* instr) {
	IRPrinter printer(instr);
	SetColor(INSTR_COLOR);
	DumpInstruction(instr, printer);

	// Print the value of the operands if requested.
	if(showInstrValues_) {
		DumpInstructionValues(instr, printer);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::DumpBreakpointList() {
	// Format:       N | function:block: instr_index
	auto& bps = inter_->Breakpoints();
	SetColor(BREAK_COLOR);
	std::wcout<<">> Breakpoints\n\n";

	for(int i = 0; i < bps.Count(); i++) {
		auto bp = bps[i];
		SetColor(NORMAL_COLOR);
		string frame = DumpFrame(bp.BreakFunction, bp.BreakBlock, 
                                 bp.BreakInstruction);
		std::wcout.width(16);
		std::wcout<<i<<" | ";

		SetColor(BREAK_COLOR);
		std::wcout<<frame.Chars()<<"\n";
	}

	// Read next command.
	std::wcout<<"\n";
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string InterpreterDriver::DumpFrame(Function* function, Block* block,
                                    Instruction* instr) {
	// Find the index of the instruction.
	int index = 0;
	for(auto p = block->FirstInstruction(); p && (p != instr); 
        p = p->NextInstruction(), index++);

	return string::Format(L"%s.%s: %d", function->Name()->Chars(), 
						  block->Name()->Chars(), index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::DumpVariableList(string title, List<VariableValue>& list) {
	// Format:
	//           varName | value
	//       longVarName | value
	SetColor(VAR_LIST_COLOR);
	std::wcout<<">> "<<title.Chars()<<"\n\n";

	for(int i = 0; i < list.Count(); i++) {
		SetColor(NORMAL_COLOR);
		std::wcout.width(16);
		std::wcout<<list[i].Variable->Name()->Chars()<<" | ";

		SetColor(VAR_LIST_COLOR);
		std::wcout<<DumpValue(list[i].Data).Chars()<<"\n";
	}

	// Read next command.
	std::wcout<<"\n";
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::DumpTemporaryList(List<TemporaryValue>& list) {
	// Format:
	//           t1 | value
	IRPrinter printer(inter_->CurrentFrame().FunctionObj);
	SetColor(TEMP_LIST_COLOR);
	std::wcout<<">> Temporaries\n\n";

	for(int i = 0; i < list.Count(); i++) {
		SetColor(NORMAL_COLOR);
		std::wcout.width(16);

		// Use the name of the temporary, if available.
		bool hasName = false;

        if(auto nameTag = list[i].Temp->GetTag<NameTag>()) {
		    hasName = true;
		    std::wcout<<nameTag->Name().Chars()<<" | ";
		}

		if(hasName == false) {
			std::wcout<<"t"<<printer.GetTemporaryIndex(list[i].Temp)<<" | ";
		}

		SetColor(TEMP_LIST_COLOR);
		std::wcout<<DumpValue(list[i].Data).Chars()<<"\n";
	}

	// Read next command.
	std::wcout<<"\n";
	CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::DumpParameterList(List<ParameterValue>& list) {
    // Format:
    //           p1 | value
    IRPrinter printer(inter_->CurrentFrame().FunctionObj);
    SetColor(TEMP_LIST_COLOR);
    std::wcout<<">> Parameters\n\n";

    for(int i = 0; i < list.Count(); i++) {
        SetColor(NORMAL_COLOR);
        std::wcout.width(16);

        // Use the name of the variable, if available.
        bool hasName = list[i].Param->GetVariable()->HasName();

        if(hasName) {
            std::wcout<<list[i].Param->GetVariable()->Name()->Chars()<<" | ";
        }
        else {
            std::wcout<<"p | ";
        }

        SetColor(TEMP_LIST_COLOR);
        std::wcout<<DumpValue(list[i].Data).Chars()<<"\n";
    }

    // Read next command.
    std::wcout<<"\n";
    CommandLoop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::DumpInstruction(Instruction* instr, IRPrinter& printer) {
	SetColor(INSTR_COLOR);
	std::wcout<<">> "<<printer.ToString().Chars();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::DumpInstructionValues(Instruction* instr, IRPrinter& printer) {
	// The value of each operand is printed. For variables the name is also printer,
	// and for temporaries the index is taken from the printer.
	SetColor(INSTR_VAL_COLOR);
	bool printComma = false;

	if(instr->IsGoto() == false) {
		std::wcout<<"       (";
	}

	for(int i = 0; i < instr->SourceOpCount(); i++) {
		Operand* op = instr->GetSourceOp(i);
		if(op == nullptr) continue;

		if(op->IsVariableReference()) {
			if(printComma) std::wcout<<", ";
			string result = DumpOperand(op);
			std::wcout<<result.Chars();
			printComma = result.Length() > 0;
		}
		else if(auto temp = op->As<Temporary>()) {
			if(printComma) std::wcout<<", ";
			const string* name= nullptr;
			
            if(auto nameTag = temp->GetTag<NameTag>()) { 
                name = &nameTag->Name();
			}

			int index = printer.GetTemporaryIndex(op->As<Temporary>());
			string result = DumpOperand(op, index, name);
			std::wcout<<result.Chars();
			printComma = result.Length() > 0;
		}
	}

	if(instr->IsGoto() == false) {
		std::wcout<<")";
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string InterpreterDriver::DumpOperand(Operand* op, int tempIndex, const string* name) {
	if(op == nullptr) return "";

	if(tempIndex < 0) {
		Value* value;

		if(op->IsLocalVariableRef()) {
			value = inter_->GetLocalValue(op->GetSymbol());
		}
		else value = inter_->GetGlobalValue(op->GetSymbol());

		return *op->GetSymbol()->Name() + " = " + 
			   DumpValueData(value->Data(), value->GetType());
	}
	else if(auto temp = inter_->GetTemporaryValue(op->As<Temporary>())) {
		string tempName = name ? *name : string::Format(L"t%d", tempIndex);
		return tempName + " = " + DumpValueData(temp->Data(), temp->GetType());
	}
	else return "";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string InterpreterDriver::DumpValueData(char* data, const Type* type) {
	// For integer and floating we print just the value.
	// For pointers to simple types we also print the value of the pointee.
	if(type->IsInteger()) {
		__int64 value = Value::AsInteger(data, type);

		if(inter_->IsMagic(value, type->As<IntegerType>())) {
			return "UNDEF";
		}
		else return string::Format(L"%lld", Value::AsInteger(data, type));
	}
	else if(type->IsFloating()) {
		return string::Format(L"%f", Value::AsFloating(data, type));
	}
	else if(auto pointerType = type->As<PointerType>()) {
		StringBuilder sb;
		if(inter_->IsMagic((__int64)*(size_t*)data, IntegerType::GetInt64())) {
			return "UNDEF_PTR";
		}
		else sb.AppendFormat(L"0x%X", *(size_t*)data);
		return sb.ToString();
	}
	else if(auto arrayType = type->As<ArrayType>()) {
		return DumpArray(data, arrayType);
	}
	else if(auto recordType = type->As<RecordType>()) {
		return DumpRecord(data, recordType);
	}
	else return "";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string InterpreterDriver::DumpArray(char* data, const ArrayType* type, int maxElem) {
	StringBuilder sb;
	int count = std::min((int)type->Size(), maxElem);
	auto elementType = type->ElementType();
	__int64 elemSize = Value::Size(elementType);
	
	// Format: [E1,E2] or [E1,E2,...] if not all elements should be printed.
	sb.Append("[");

	for(int i = 0; i < count; i++) {
		if(i > 0) sb.Append(",");

		sb.Append(DumpValueData(data, elementType));
		data += elemSize;
	}

	if(type->Size() > maxElem) sb.Append(",...]");
	else sb.Append("]");
	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string InterpreterDriver::DumpRecord(char* data, const RecordType* type, int maxElem) {
	StringBuilder sb;
	int count = std::min(type->FieldCount(), maxElem);
	
	// Format: {E1,E2} or {E1,E2,...} if not all fields should be printed.
	sb.Append("{");

	for(int i = 0; i < count; i++) {
		if(i > 0) sb.Append(",");

		auto& field = type->Fields()[i];
		sb.Append(DumpValueData(data + field.FieldOffset, field.FieldType));
	}

	if(type->FieldCount() > maxElem) sb.Append(",...}");
	else sb.Append("}");
	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void InterpreterDriver::ShowHelp() {
	SetColor(HIGHLIGHT_COLOR);
	std::wcout<<L">> Available commands:\n\n";
	SetColor(NORMAL_COLOR);
	std::wcout<<L"run [funct]:   start interpreting the specified function\n";
	std::wcout<<L"end:   close the interpreter\n";
	std::wcout<<L"local:   print a list with the local variables\n";
	std::wcout<<L"global:   print a list with the global variables\n";
	std::wcout<<L"temp:   print a list with the function temporaries\n";
    std::wcout<<L"param:   print a list with the function parameters\n";
	std::wcout<<L"break [funct] [block] [instr] [hitcount]:   sets a breakpoint\n";
	std::wcout<<L"\t- funct - the function where to break\n";
	std::wcout<<L"\t- block - the block where to break (optional)\n";
	std::wcout<<L"\t- instr - the instruction index in the block (optional)\n";
	std::wcout<<L"\t- hitcount - after how many hits to break (optional)\n";
	std::wcout<<L"break show:   displays a list with the active breakpoints\n";
	std::wcout<<L"delete [index]:   deletes the breakpoint having the specified index\n";
	std::wcout<<L"stack:   shows a list with all the stack frames\n";
	std::wcout<<L"print all/funct/block:   prints the current function/block, or the whole unit\n";
	std::wcout<<L"instr [on/off]:   break on each instruction\n";
	std::wcout<<L"block [on/off]:   break on each block\n";
	std::wcout<<L"funct [on/off]:   break on each function\n";

	// Read next command.
	CommandLoop();
}

} // namespace IR