// SSAVerifier.hpp
// Copyright (c) Lup Gratian
//
// Verifies if the SSA property that each use is dominated
// by its definition holds for all instructions in a function.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SSA_VERIFIER_HPP
#define PC_ANALYSIS_SSA_VERIFIER_HPP

#include "IRDominators.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/IRPrinter.hpp"
#include "../Base/Log.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/String.hpp"
#include "../Compilation Pass/AnalysisPass.hpp"
using namespace IR;
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

class SSAVerifier : public AnalysisPass {
private:
	//! TODO: this should be a control
	static const bool ASSERT_ON_ERROR = true;

	void ReportError(Instruction* user, Temporary* temp, Function* function);

public:
	void Execute(Function* function);
};

} // namespace Analysis
#endif