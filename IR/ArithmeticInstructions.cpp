// ArithmeticInstruction.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ArithmeticInstructions.hpp"
#include "Function.hpp"
#include "Block.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

ArithmeticInstr::ArithmeticInstr(Opcode opcode, Operand* left, Operand* right,
								 Operand* result, Block* parent, Instruction* previous) :
        Instruction(opcode, 0, parent, previous),
		result_(result ? result->As<Temporary>() : nullptr) {
	// Link the operands to this instruction.
	sources_[0] = left;
	sources_[1] = right;
	LinkUser(left);
	LinkUser(right);
	if(result) result->SetDefiningInstr(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ArithmeticInstr* ArithmeticInstr::GetArithmetic(Opcode opcode, Operand* left,
												Operand* right, Operand* result,
												Block* parent, Instruction* previous) {
	ArithmeticInstr* instr = new ArithmeticInstr(opcode, left, right, result, 
												 parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* ArithmeticInstr::Clone() {
	auto copy = GetArithmetic((Opcode)opcode_, sources_[0], sources_[1], nullptr);
	copy->other_ = other_;
	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ArithmeticInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append(OpcodeString().ToLower());

	if(LeftOp())   sb.Append('\t', level + 1)
				  .Append("Left: ").AppendLine(LeftOp()->ToString(level + 1));
	if(RightOp())  sb.Append('\t', level + 1)
				  .Append("Right: ").AppendLine(RightOp()->ToString(level + 1));
	if(ResultOp()) sb.Append('\t', level + 1)
				  .Append("Result: ").AppendLine(ResultOp()->ToString(level + 1));
	
	return sb.ToString();
}

} // namespace IR