// IRSummaryBuilder.hpp
// Copyright (c) Lup Gratian
//
// Collects information from a function and generates a summary.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_IR_SUMMARY_BUILDER_HPP
#define PC_ANALYSIS_IR_SUMMARY_BUILDER_HPP

#include "IRSummary.hpp"
#include "SummaryBuilder.hpp"
#include "SummaryManager.hpp"
#include "../IR/Function.hpp"
#include "../IR/References.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Block.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Compilation Pass/AnalysisPass.hpp"
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

class IRSummaryBuilder : public SummaryBuilder {
private:
	IRSummary* GetOrCreateSummary(Function* funct) {
		auto functionRef = funct->GetReference();
		IRSummary* summary = Manager()->GetSummary<IRSummary>(functionRef);

		if(summary == nullptr) {
			summary = IRSummary::GetIRSummary();
			Manager()->AddSummary(summary, functionRef);
		}
		else summary->Reset();

		return summary;
	}

	bool UsesParameter(Operand* op) {
		if(op->IsParameter()) {
			return true;
		}
		else if(auto arithInstr = op->DefiningInstrAs<ArithmeticInstr>()) {
			return arithInstr->LeftOp()->IsParameter() ||
				   arithInstr->RightOp()->IsParameter();
		}
		else if(auto cmpInstr = op->DefiningInstrAs<CmpInstrBase>()) {
			return cmpInstr->LeftOp()->IsParameter() ||
				   cmpInstr->RightOp()->IsParameter();
		}
		else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
			return questInstr->TrueOp()->IsParameter() ||
				   questInstr->FalseOp()->IsParameter();
		}

		return false;
	}

	IRSummary* SummarizeFunction(Function* funct) {
		DebugValidator::IsNotNull(funct);
		DebugValidator::IsTrue(funct->IsDefinition());

		auto summary = GetOrCreateSummary(funct);
		int maxInstrsPerBlock = std::numeric_limits<int>::min();
		int instructions = 0;
		short gotoCount = 0;
		short ifCount = 0;
		short switchCount = 0;
		short returnCount = 0;
		short constReturnCount = 0;
		short paramReturnCount = 0;
		short ifOnParamCount = 0;
		short switchOnParamCount = 0;

		funct->ForEachBlock([&, this](Block* block) -> bool {
			int count = block->InstructionCount();
			maxInstrsPerBlock = std::max(maxInstrsPerBlock, count);
			instructions += count;

			switch(block->LastInstruction()->GetOpcode()) {
				case Opcode::Goto: { 
					gotoCount++;   
					break; 
				}
				case Opcode::If: { 
					ifCount++;

					if(UsesParameter(block->LastInstruction()->GetSourceOp(0))) {
						ifOnParamCount++;
					}
					break; 
				}
				case Opcode::Switch: {
					switchCount++;

					if(UsesParameter(block->LastInstruction()->GetSourceOp(0))) {
						switchOnParamCount++;
					}
					break;
				}
				case Opcode::Return: { 
					returnCount++; 

					if(block->ParentFunction()->IsVoid() == false) {
						auto returnedOp = block->LastInstruction()->GetSourceOp(0);

						if(UsesParameter(returnedOp)) {
							paramReturnCount++;
						}
						else if(returnedOp->IsConstant()) {
							constReturnCount++;
						}
					}

					break; 
				}
			}

			return true;
		});

		summary->SetBlockCount(funct->BlockCount());
		summary->SetInstructionCount(instructions);
		summary->SetVariableCount(funct->VariableCount());
		summary->SetMaxBlockSize(maxInstrsPerBlock);
		summary->SetGotoCount(gotoCount);
		summary->SetIfCount(ifCount);
		summary->SetSwitchCount(switchCount);
		summary->SetReturnCount(returnCount);
		summary->SetConstantReturnCount(constReturnCount);
		summary->SetReturnedParameterCount(paramReturnCount);
		summary->SetIfOnParameterCount(ifOnParamCount);
		summary->SetSwitchOnParameterCount(switchOnParamCount);
		summary->SetCallCount(funct->CallInstructionCount());
		return summary;
	}

public:
	virtual void Execute(Function* funct) override {
		DebugValidator::IsNotNull(funct);
		
		if(funct->IsDefinition()) {
			SummarizeFunction(funct);
		}
	}
};

} // namespace Analysis
#endif