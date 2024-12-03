// SummaryBuilder.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for all summary builders and
// the module which executes each builder.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SUMMARY_BUILDER_HPP
#define PC_ANALYSIS_SUMMARY_BUILDER_HPP

#include "SummaryManager.hpp"
#include "../IR/Function.hpp"
#include "../IR/References.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Block.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Compilation Pass/AnalysisPass.hpp"
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

// The base class for all summary builders.
class SummaryBuilder {
protected:
	SummaryManager* manager_;

public:
	virtual ~SummaryBuilder() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual SummaryManager* Manager() {
		return manager_;
	}

	virtual void SetManager(SummaryManager* manager) {
		DebugValidator::IsNotNull(manager);
		manager_ = manager;
	}

	virtual bool IsEnabled() const {
		return true;
	}

	virtual void Execute(Function* funct) = 0;
};


// Executes each summary builder found in the list.
class SummaryBuilderExecuter : public AnalysisPass {
private:
	List<shared<SummaryBuilder>> builders_;
	SummaryManager* manager_;

public:
	SummaryBuilderExecuter(SummaryManager* manager) : manager_(manager) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	void AddBuilder(shared<SummaryBuilder> builder) {
		DebugValidator::IsNotNull(builder.Raw());
		builder->SetManager(manager_);
		builders_.Add(builder);
	}

	void RemoveAllBuilders() {
		builders_.Clear();
	}

	bool Execute(Function* funct) {
		DebugValidator::IsNotNull(funct);

		for(int i = 0; i < builders_.Count(); i++) {
			builders_[i]->Execute(funct);
		}

		return true;
	}
};

} // namespace Analysis
#endif