// FlowDotPrinter.hpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_FLOW_DOT_PRINTER_HPP
#define PC_IR_FLOW_DOT_PRINTER_HPP

#include "Instructions.hpp"
#include "Block.hpp"
#include "Function.hpp"
#include "IRPrinter.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/Dictionary.hpp"
#include <iostream>
#include <fstream>
using namespace Base;

namespace IR {

class FlowDotPrinter {
private:
	Dictionary<size_t, int> blockIndex_;
	Dictionary<size_t, int> functIndex_;
	std::wofstream fout;
	int  nodes_;
	bool showInstr_;
	bool showVariables_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void Write(const string& s);
	void MakeIfBlock(Block* block);
	void MakeBlock(Block* block);
	void MakeGotoBlock(Block* block);
	void MakeSwitchBlock(Block* block);
	void MakeLink(Block* a, Block* b, string portA, string portB, 
				  const string& placeA = L"s", const string& placeB = L"n");
	void BeginGraph();
	void EndGraph();
	string GetInstructions(Block* block);

public:
	FlowDotPrinter(const string& file, bool showInstr = true, bool showVar = true);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void Print(Function* funct);
};

} // namespace IR
#endif