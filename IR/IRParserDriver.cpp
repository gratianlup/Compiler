// IRParserDriver.cpp
// Copyright (c) Lup Gratian
//
// Implements the IR parser driver.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IRParserDriver.hpp"
#include "Intrinsics.hpp"
#include <iostream>
#include <fstream>

namespace IR {

void IRParserDriver::Handle(VerifierError message) {
	Console::SetTextColor(ConsoleColor::Red);
	std::wcout<<"VERIFIER ERROR:\n";
	Console::SetTextColor(ConsoleColor::White);
	std::wcout<<verifierMessages_[message.Error].Chars()<<"\n";

	Console::SetTextColor(ConsoleColor::Gray);
	if(message.ParentFunction) std::wcout<<"Function: "<<message.ParentFunction->Name()->Chars()<<"\n";
	if(message.BlockName)    std::wcout<<"Block: "<<message.BlockName->Chars()<<"\n";
	if(message.SymbolName)   std::wcout<<"Symbol: "<<message.SymbolName->Chars()<<"\n";
	
	if(message.InstructionIndex != -1) {
		std::wcout<<"Instruction: "<<message.InstructionIndex;
		if(message.Object) {
			std::wcout<<" (";
			std::wcout<<reinterpret_cast<Instruction*>(message.Object)->OpcodeString().Chars();
			std::wcout<<")";
		}

		std::wcout<<"\n";
	}
	
	std::wcout<<"\n";
	errorCount_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRParserDriver::Handle(ParserError message) {
	Console::SetTextColor(ConsoleColor::Red);
	std::wcout<<"ERROR: ";
	Console::SetTextColor(ConsoleColor::White);
	std::wcout<<message.Location.Line() + 1;
	std::wcout<<"\n"<<parserMessages_[message.Error].Chars();
	Console::SetTextColor(ConsoleColor::Gray);

	if(message.Text.Length() > 0) {
		std::wcout<<"\nCurrent token: "<<message.Text.Chars();
	}

	std::wcout<<"\n\n";
	errorCount_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IRParserDriver::IRParserDriver(const string& path, bool launchInterpreter) :
		errorCount_(0) {
	// Initialize the error messages for the parser and the verifier.
	#define errorMessage(TYPE, MESSAGE) parserMessages_.Add(Error::TYPE, MESSAGE);
	#include "IRParserMessages.def"
	#undef errorMessage

	#define errorMessage(TYPE, MESSAGE) verifierMessages_.Add(Error::TYPE, MESSAGE);
	#include "IRVerifierMessages.def"
	#undef errorMessage

	// Initialize the file manager and the lexer.
	Common::FileManager fileManager;
	fileManager.Initialize(nullptr);

	// Any error message from the lexer should be ignored.
	Common::CompileOptions options;
	Common::Diagnostic diag(&options);
	options.SetIsDiagnosticDisabled(true);
	
	// Add the keywords to the lexer.
	Lexing::IdentifierTable table;

	#define keyword(text, type) table.AddKeyword(text, (int)KeywordType::##type);
	#include "IRKeywords.def"
	#undef keyword

	// Load the file and start lexing.
	shared<Lexing::Lexer> lexer = new Lexing::Lexer(&table, nullptr, &fileManager, 
													&diag, false /* no preprocessor */);
	lexer->SetHasCustomChar(true);
	lexer->SetCustomChar(L'#');
	
	if(lexer->LoadStart(path) == false) {
		std::wcout<<"File not found or could not be opened.";
		return;
	}

	// Create the helper tables.
	types_ = new TypeTable();
	consts_ = new ConstantTable(types_);
	intrinsics_ = new IntrinsicTable();

	
	// Parse the file. If no error was found the created representation is verified,
	// and then the interpreter driver is started.
	unit_ = Unit::GetUnit(types_, consts_, intrinsics_);
	intrinsics_->Add(CopyMemoryIntr::GetCopyMemory(unit_));
	intrinsics_->Add(SetMemoryIntr::GetSetMemory(unit_));

	IRParser parser(lexer, this, types_);
	parser.ParseUnit(unit_);
	if(errorCount_ != 0) return;

	IRPrinter p(unit_);
	std::wofstream f("D:\\out.irl");
	f<<p.ToString().Chars();
	f.close();

	IRVerifier verifier(unit_, this, false /* debugBreak */);
	if(errorCount_ != 0) return;

	if(launchInterpreter) {
		Interpreter inter(unit_);

		// Register the built-in functions with the interpreter.
		ReadInt8::Register(&inter);
		ReadInt16::Register(&inter);
		ReadInt32::Register(&inter);
		ReadInt64::Register(&inter);
		ReadFloat::Register(&inter);
		ReadDouble::Register(&inter);

		PrintInt8::Register(&inter);
		PrintInt16::Register(&inter);
		PrintInt32::Register(&inter);
		PrintInt64::Register(&inter);
		PrintFloat::Register(&inter);
		PrintDouble::Register(&inter);
	
		PrintInt8Array::Register(&inter);
		PrintInt16Array::Register(&inter);
		PrintInt32Array::Register(&inter);
		PrintInt64Array::Register(&inter);
		PrintString::Register(&inter);
		ReadString::Register(&inter);

		// Start interpreting the parsed code.
		InterpreterDriver interDriver(&inter);
	}
}

} // namespace IR