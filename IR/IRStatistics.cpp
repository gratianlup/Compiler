// IRStatistics.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IRStatistics.hpp"
#include <iostream>

namespace IR {

void IRStatistics::Reset() {
	functCount_ = 0;
	blockCount_ = 0;
	instrCount_ = 0;
	opcodeCount_.Clear();

	#define instruction(TYPE, CAT, NAME) opcodeCount_.Add(Opcode::##TYPE, 0);
	#include "Instructions.def"
	#undef instruction
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRStatistics::Visit(Unit* unit) {
	// Visit all variables and functions.
	auto stat = this;

	unit->Symbols().ForEach([stat](Symbol* symbol) {
		symbol->Accept(stat);
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRStatistics::Visit(Function* function) {
	// Visit all blocks.
	auto stat = this;
	functCount_++;

	function->Symbols().ForEach([stat](Symbol* symbol) {
		symbol->Accept(stat);
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRStatistics::Visit(Block* block) {
	blockCount_++;

	// Visit all instructions in the block.
	for(auto instr = block->FirstInstruction(); instr; 
        instr = instr->NextInstruction()) {
		instr->Accept(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRStatistics::Visit(Instruction* instr) {
	// Increment the appropriate counter.
	opcodeCount_[instr->GetOpcode()]++;
	instrCount_++;

	// Visit all the operands.
	for(int i = 0; i < instr->SourceOpCount(); i++) {
		auto op = instr->GetSourceOp(i);
		if(op) op->Accept(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string IRStatistics::ToString(bool sortCount) const {
	StringBuilder sb;
	sb.AppendFormat(L"Functions: %lld\n", functCount_);
	sb.AppendFormat(L"Blocks: %lld\n", blockCount_);
	sb.AppendFormat(L"Instructions: %lld\n", instrCount_);

	List<InstructionSortHelper> list;
	opcodeCount_.ForEach([&list, sortCount](Dictionary<Opcode, __int64>::TPair& pair) {
		list.Add(InstructionSortHelper(pair.Key, pair.Value, sortCount));
	});

	if(sortCount) list.Sort();

	for(int i = 0; i < list.Count(); i++) {
		string instr = Instruction::OpcodeString(list[i].InstrOpcode).ToLower();
		sb.AppendLine();
		sb.AppendFormat(L"%s: %d", instr.Chars(), list[i].Count);
	}

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRStatistics::Dump(bool sortCount) const {
	std::wcout<<"Statistics\n\n";
	std::wcout<<ToString(sortCount).Chars();
}

} // namespace IR