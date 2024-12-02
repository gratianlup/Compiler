// SSAVerifier.cpp
// Copyright (c) Lup Gratian
//
// Implements the SSAVerifier class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SSAVerifier.hpp"

namespace Analysis {

void SSAVerifier::Execute(Function* function) {
	DebugValidator::IsNotNull(function);

	IRDominatorTree domTree(function);
	domTree.Build();
	domTree.Dump();

	function->ForEachInstruction([&, this](Instruction* instr) -> bool {
		instr->ForEachSourceOp([&, this](Operand* op, int index) -> bool {
			// Constants and references "dominate" any instruction.
			if(auto temp = op->As<Temporary>()) {
				auto tempBlock = temp->DefiningInstruction()->ParentBlock();

				if(auto phiInstr = instr->As<PhiInstr>()) {
					// The incoming operand should dominate the block
					// where it is incoming from.
					auto incomingBlock = phiInstr->GetOperandBlock(index);

					if(domTree.Dominates(tempBlock, incomingBlock) == false) {
						ReportError(instr, temp, function);
					}
				}
				else if(domTree.Dominates(tempBlock, instr->ParentBlock()) == false) {
					ReportError(instr, temp, function);
				}
			}

			return true;
		});

		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SSAVerifier::ReportError(Instruction* user, Temporary* temp, Function* function) {
	auto tempBlockName = temp->DefiningInstruction()->ParentBlock()->Name();
	auto userBlockName = user->ParentBlock()->Name();
	auto tempname = temp->DefiningInstruction()->OpcodeString();
	auto userName = user->OpcodeString();

	string text = "SSA violation in " + *function->Name() + "\n";
	text += "    Use of temporary defined in block '" + *tempBlockName + "' (" + userName + ")\n";
	text += "    in non-dominated block '" + *userBlockName + " by '" + userName + "' instruction\n";
	Log::Error(text);

	if(ASSERT_ON_ERROR) {
		DebugValidator::IsTrue(false);
	}
}

} // namespace Analysis