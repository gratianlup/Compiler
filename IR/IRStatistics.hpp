// IRStatistics.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_STATISTICS_HPP
#define PC_IR_STATISTICS_HPP

#include "Instructions.hpp"
#include "Unit.hpp"
#include "Symbols.hpp"
#include "Visitor.hpp"
#include "../Base/String.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

// Used to sort the list of instructions based by name or count.
struct InstructionSortHelper {
	Opcode InstrOpcode;
	__int64 Count;
	bool  SortCount;

	InstructionSortHelper() {}
	InstructionSortHelper(Opcode opcode, __int64 count, bool sortCount) :
			InstrOpcode(opcode), Count(count), SortCount(sortCount) {}

	unsigned GetHashCode() const {
		return (unsigned)InstrOpcode;
	}

	bool operator() (const InstructionSortHelper& a, const InstructionSortHelper& b) const {
		if(a.SortCount) return a.operator<(b);
		else return a.operator==(b);
	}

	bool operator== (const InstructionSortHelper& other) const {
		if(SortCount) return Count == other.Count;
		else return Instruction::OpcodeString(InstrOpcode) ==
					Instruction::OpcodeString(other.InstrOpcode);
	}

	bool operator< (const InstructionSortHelper& other) const {
		if(SortCount) return Count > other.Count;
		else return Instruction::OpcodeString(InstrOpcode) >
					Instruction::OpcodeString(other.InstrOpcode);
	}
};


class IRStatistics : public Visitor {
private:
	Dictionary<Opcode, __int64> opcodeCount_;
	__int64 functCount_;
	__int64 blockCount_;
	__int64 instrCount_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void Visit(Unit* unit);
	void Visit(Block* block);
	void Visit(Function* function);
	void Visit(Instruction* instr);

public:
	IRStatistics() {
		Reset();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Clears all counters.
	void Reset();

	// Methods that count 
	void Count(Unit* unit)         { Visit(unit);  }
	void Count(Function* function) { Visit(function); }
	void Count(Block* block)       { Visit(block); }
	void Count(Instruction* instr) { Visit(instr); }

	// Methods for obtaining the number of objects.
	__int64 FucntionCount()    { return blockCount_; }
	__int64 BlockCount()       { return blockCount_; }
	__int64 InstructionCount() { return instrCount_; } 

	__int64 InstructionCount(Opcode op) { 
		return opcodeCount_[op]; 
	}

	// Count information for each instruction generated automatically.
	#define instruction(TYPE, CAT, NAME) \
		__int64 TYPE##Count() { return opcodeCount_[Opcode::##TYPE]; }
	#include "Instructions.def"
	#undef instruction

	// Returns a string with the statistics.
	string ToString(bool sortCount = true) const;

	// Writes the statistics on the console.
	void Dump(bool sortCount = true) const;
};

} // namespace IR
#endif