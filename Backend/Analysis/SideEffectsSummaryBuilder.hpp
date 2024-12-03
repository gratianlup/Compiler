// SideEffectsSummaryBuilder.hpp
// Copyright (c) Lup Gratian
//
// Generates a summary describing the side-effects
// when calling a function.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SIDE_EFFECTS_SUMMARY_BUILDER_HPP
#define PC_ANALYSIS_SIDE_EFFECTS_SUMMARY_BUILDER_HPP

#include "GlobalSideEffectsTag.hpp"
#include "SideEffectsSummary.hpp"
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

class SideEffectsSummaryBuilder : public SummaryBuilder {
private:
	SideEffectsSummary* GetOrCreateSummary(Function* funct) {
		auto functionRef = funct->GetReference();
		SideEffectsSummary* summary = Manager()->GetSummary<SideEffectsSummary>(functionRef);

		if(summary == nullptr) {
			summary = SideEffectsSummary::GetSideEffectsSummary();
			Manager()->AddSummary(summary, functionRef);
		}
		else summary->Reset();

		return summary;
	}

	SideEffectsSummary * SummarizeFunction(Function* funct) {
		DebugValidator::IsNotNull(funct);
		DebugValidator::IsTrue(funct->IsDefinition());

		auto summary = GetOrCreateSummary(funct);

		if(auto globalSideEffects = funct->GetTag<GlobalSideEffectsTag>()) {
			summary->SetReadsGlobalVariables(globalSideEffects->ReadsVariables());
			summary->SetWritesGlobalVariables(globalSideEffects->WritesVariables());
		}

		funct->ForEachParameterVariable([summary](Variable* variable, int index) -> bool {
			if(variable->IsNoRead()) {
				summary->SetParameterIsNoRead(index);
			}

			if(variable->IsNoWrite()) {
				summary->SetParameterIsNoWrite(index);
			}

			if(variable->IsNoEscape()) {
				summary->SetParameterIsNoEscape(index);
			}

			return true;
		});

		summary->SetIsDefinition(true);
		summary->SetIsNoState(funct->IsNoState());
		summary->SetIsNoIndirectRead(funct->IsNoIndirectRead());
		summary->SetIsNoIndirectWrite(funct->IsNoIndirectWrite());
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