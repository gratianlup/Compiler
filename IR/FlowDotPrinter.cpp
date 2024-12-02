// FlowDotPrinter.cpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "FlowDotPrinter.hpp"

namespace IR {

FlowDotPrinter::FlowDotPrinter(const string& file, bool showInstr, bool showVar) :
		fout(file.Chars()), showInstr_(showInstr), showVariables_(showVar), nodes_(0) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::Write(const string& s) {
	fout<<s.Chars();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::Print(Function* funct) {
	BeginGraph();

	// First make the nodes for all blocks. 
	// Then scan the blocks a second time to create the links.
	for(auto block = funct->FirstBlock(); block; block = block->NextBlock()) {
		auto lastInstr = block->LastInstr();

		if(lastInstr && lastInstr->IsIf()) {
			MakeIfBlock(block);
		}
		else if(lastInstr && lastInstr->IsGoto()) {
			MakeGotoBlock(block);
		}
		else if(lastInstr && lastInstr->IsSwitch()) {
			MakeSwitchBlock(block);
		}
		else MakeBlock(block);
	}

	for(auto block = funct->FirstBlock(); block; block = block->NextBlock()) {
		auto lastInstr = block->LastInstr();
		
		if(auto ifInstr = lastInstr->As<IfInstr>()) {
			MakeLink(block, ifInstr->TrueTargetOp()->Target(), "true", "name");
			MakeLink(block, ifInstr->FalseTargetOp()->Target(), "false", "name");
		}
		else if(auto gotoInstr = lastInstr->As<GotoInstr>()) {
			MakeLink(block, gotoInstr->TargetOp()->Target(), "target", "name");
		}
		else if(auto switchInstr = lastInstr->As<SwitchInstr>()) {
			MakeLink(block, switchInstr->DefaultTargetOp()->Target(), "default", "name");
			auto list = switchInstr->CaseList();

			for(int i = 0; i < list.Count(); i++) {
				MakeLink(block, list[i].Target->Target(), string::Format(L"c%d", i), "name");
			}
		}
	}

	EndGraph();
	fout.close();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::MakeLink(Block* a, Block* b, string portA, string portB,
							  const string& placeA, const string& placeB) {
	int nodeA = blockIndex_[(size_t)a];
	int nodeB = blockIndex_[(size_t)b];
	string link;

	if(showInstr_) {
		link = string::Format(L"n%d:%s:%s -> n%d:%s:%s\n", 
							  nodeA, portA.Chars(), placeA.Chars(),
							  nodeB, portB.Chars(), placeB.Chars());
	}
	else {
		link = string::Format(L"n%d:%s:%s -> n%d:n\n", nodeA, portA.Chars(), placeA.Chars(), nodeB);
	}

	Write(link);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string FlowDotPrinter::GetInstructions(Block* block) {
	StringBuilder sb;

	for(auto instr = block->FirstInstr(); instr; instr = instr->NextInstr()) {
		sb.Append(IRPrinter(instr).ToString());

		if(instr->Next()) sb.Append("\\n");
	}

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::MakeIfBlock(Block* block) {
	// Print the instructions, if requested.
	string instrText;

	if(showInstr_) {
		instrText = GetInstructions(block);
	}

	// Make the 'record' for a block terminated with an 'if'.
	string record = string::Format(L"n%d[label=\"{<name> %s | <body> %s | {<true> true | <false> false}}\"]\n",
								   nodes_, block->Name()->Chars(), instrText.Chars());
	Write(record);
	blockIndex_.Add((size_t)block, nodes_);
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::MakeBlock(Block* block) {
	// Print the instructions, if requested.
	string instrText;

	if(showInstr_) {
		instrText = GetInstructions(block);
	}

	// Make the 'record' for a block terminated with a 'goto'.
	string record = string::Format(L"n%d[color=firebrick3, label=\"{<name> %s | <body> %s}}\"];\n",
								   nodes_, block->Name()->Chars(), instrText.Chars());
	Write(record);
	blockIndex_.Add((size_t)block, nodes_);
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::MakeGotoBlock(Block* block) {
	// Print the instructions, if requested.
	string instrText;

	if(showInstr_) {
		instrText = GetInstructions(block);
	}

	// Make the 'record' for a block terminated with a 'goto'.
	string record = string::Format(L"n%d[label=\"{<name> %s | <body> %s | {<target> }}\"];\n",
								   nodes_, block->Name()->Chars(), instrText.Chars());
	Write(record);
	blockIndex_.Add((size_t)block, nodes_);
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::MakeSwitchBlock(Block* block) {
	// Print the instructions, if requested.
	string instrText;

	if(showInstr_) {
		instrText = GetInstructions(block);
	}

	// Make the 'record' for a block terminated with a 'switch'.
	// We create a port for each 'case' label and for the default target.
	StringBuilder record;
	record.AppendFormat(L"n%d[label=\"<name> %s | <body> %s | {<default> default",
						nodes_, block->Name()->Chars(), instrText.Chars());
	auto switchInstr = block->LastInstr()->As<SwitchInstr>();
	auto& list = switchInstr->CaseList();

	for(int i = 0; i < list.Count(); i++) {
		record.AppendFormat(L" | <c%d> %d", i, list[i].Value);
	}

	record.AppendLine("}\"];");
	Write(record.ToString());
	blockIndex_.Add((size_t)block, nodes_);
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::BeginGraph() {
	fout<<"digraph {\nnode [shape = record];\n";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FlowDotPrinter::EndGraph() {
	fout<<"}\n";
}

} // namespace IR