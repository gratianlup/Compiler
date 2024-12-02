// LoopSummaryBuilder.hpp
// Copyright (c) Lup Gratian
//
// Generates a summary describing all loops in the function.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_LOOP_SUMMARY_BUILDER_HPP
#define PC_ANALYSIS_LOOP_SUMMARY_BUILDER_HPP

#include "LoopTag.hpp"
#include "LoopSummary.hpp"
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

class LoopSummaryBuilder : public SummaryBuilder {
private:
	LoopSummary* GetOrCreateSummary(Function* funct) {
		auto functionRef = funct->GetReference();
		LoopSummary* summary = Manager()->GetSummary<LoopSummary>(functionRef);

		if(summary == nullptr) {
			summary = LoopSummary::GetLoopSummary();
			Manager()->AddSummary(summary, functionRef);
		}
		else summary->Reset();

		return summary;
	}

	void SummarizeLoop(Loop* loop, short& loopCount, short& maxLoopDepth,
					   short& maxLoopBlocks, short& maxLoopChildren, short depth = 0) {
		loopCount++;
		maxLoopDepth = std::max(maxLoopDepth, depth);
		maxLoopBlocks = std::max(maxLoopBlocks, (short)loop->BlockCount());
		maxLoopChildren = std::max(maxLoopChildren, (short)loop->NestedLoopsCount());

		loop->ForEachNestedLoop([&, this](Loop* nestedLoop) -> bool {
			SummarizeLoop(nestedLoop, loopCount, maxLoopDepth,
						  maxLoopBlocks, maxLoopChildren, depth + 1);
			return true;
		});
	}

	LoopSummary* SummarizeFunction(Function* funct) {
		DebugValidator::IsNotNull(funct);
		DebugValidator::IsTrue(funct->IsDefinition());

		// Continue only the loops have been already identified.
		auto loopTag = funct->GetTag<LoopTag>();

		if(loopTag == nullptr) {
			return nullptr;
		}

		auto summary = GetOrCreateSummary(funct);
		short loopCount = 0;
		short maxLoopDepth = std::numeric_limits<short>::min();
		short maxLoopBlocks = std::numeric_limits<short>::min();
		short maxLoopChildren = std::numeric_limits<short>::min();

		loopTag->ForEachLoop([&, this](Loop* loop) -> bool {
			SummarizeLoop(loop, loopCount, maxLoopDepth,
						  maxLoopBlocks, maxLoopChildren);
			return true;
		});

		summary->SetLoopCount(loopCount);
		summary->SetMaximumLoopDepth(maxLoopDepth);
		summary->SetMaximumLoopBlocks(maxLoopBlocks);
		summary->SetMaximumLoopChildren(maxLoopChildren);
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