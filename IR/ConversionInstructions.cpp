// ConversionInstructions.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConversionInstructions.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

ConversionInstr::ConversionInstr(Opcode opcode, Operand* target, 
								 const Type* castType, Operand* result, 
								 Block* parent, Instruction* previous) :
		Instruction(opcode, 0, parent, previous), 
		target_(target), castType_(castType), 
		result_(result ? result->As<Temporary>() : nullptr) {
	// Link the operands to this instruction.
	LinkUser(target);
	if(result) result->SetDefiningInstr(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ConversionInstr* ConversionInstr::GetConversion(Opcode opcode, Operand* target, 
												const Type* castType, Operand* result, 
												Block* parent, Instruction* previous) {
	ConversionInstr* instr = new ConversionInstr(opcode, target, castType,
												 result, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* ConversionInstr::Clone() {
	auto copy = GetConversion((Opcode)opcode_, target_, castType_, nullptr);
	copy->other_ = other_;
	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ConversionInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append(OpcodeString().ToLower());

	if(TargetOp())   sb.Append('\t', level + 1)
					.Append("Target: ").AppendLine(TargetOp()->ToString(level + 1));
	if(CastType()) sb.Append('\t', level + 1)
					.Append("Type: ").AppendLine(CastType()->ToString(level + 1));
	if(ResultOp())   sb.Append('\t', level + 1)
					.Append("Result: ").AppendLine(ResultOp()->ToString(level + 1));

	return sb.ToString();
}

} // namespace IR