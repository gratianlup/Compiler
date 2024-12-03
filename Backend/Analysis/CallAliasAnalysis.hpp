// CallAliasAnalysis.hpp
// Copyright (c) Lup Gratian
//
// Implements alias analysis for function calls.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_CALL_ALIAS_ANALYSIS_HPP
#define PC_ANALYSIS_CALL_ALIAS_ANALYSIS_HPP

#include "AliasInfo.hpp"
#include "AliasAnalyzer.hpp"
#include "TypeClassTag.hpp"
#include "GlobalSideEffectsTag.hpp"
#include "ParameterAliasTag.hpp"
#include "../Base/DebugValidator.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class CallAliasAnalysis : public AliasAnalyzer, private AliasResultProvider {
private:
	// Checks the way the pointer parameter is accessed
	// by analyzing the variable flags.
	class ParameterInfo {
	public:
		bool WritesParameter(CallInstr* instr, Function* funct, int paramIndex) {
			return funct->GetParameterVariable(paramIndex)->IsWrite();
		}

		bool ReadsParameter(CallInstr* instr, Function* funct, int paramIndex) {
			return funct->GetParameterVariable(paramIndex)->IsWrite();
		}
	};


	// Checks the way the pointer parameter is accessed
	// using language information.
	class LanguageParameterInfo {
	private:
		LanguageInfo* languageInfo_;
		CallAliasAnalysis* parent_;

	public:
		LanguageParameterInfo(LanguageInfo* languageInfo, CallAliasAnalysis* parent) :
				languageInfo_(languageInfo), parent_(parent) {}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
		bool WritesParameter(CallInstr* instr, Function* funct, int paramIndex) {
			auto param = funct->GetParameter(paramIndex);
			return languageInfo_->CallMayWriteToAddress(instr, param, parent_);
		}

		bool ReadsParameter(CallInstr* instr, Function* funct, int paramIndex) {
			auto param = funct->GetParameter(paramIndex);
			return languageInfo_->CallMayReadFromAddress(instr, param, parent_);
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	//
	MemoryResult ComputeCallEffects(Function* funct, CallInstr* instr);

	//
	MemoryResult ComputeCallEffects(CallGraph* callGraph, CallInstr* instr);

	//
	MemoryResult ComputeCallEffects(CallNodeGroup* nodeGroup, CallInstr* instr);

	// 
	MemoryResult MergeResults(MemoryResult resultA, MemoryResult resultB);

	// 
	void ComputeGlobalEffects(Function* funct, CallInstr* instr,
							  MemoryResult& result);

	// 
	void ComputeGlobalEffectsForDecl(Function* funct, CallInstr* instr,
									 MemoryResult& result);

	// 
	void ComputeGlobalEffectsForIntrinsic(Intrinsic* intrinsic, CallInstr* instr,
										  MemoryResult& result);

	// 
	template <class Info>
	void ComputeParameterEffects(Function* funct, CallInstr* instr,
								 Info& parameterInfo, MemoryResult& result);

	// Implementation of the 'AliasResultProvider' interface.
	virtual bool MightBeAlias(Operand* a, Operand* b) override {
		return Parent()->HasAliasWithUnknownSize(a, b);
	}

	virtual bool IsDefinitelyNoAlias(Operand* a, Operand* b) override {
		return Parent()->HasNoAliasWithUnknownSize(a, b);
	}

public:
    CallAliasAnalysis(AliasInfo* parent) : AliasAnalyzer(parent) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual AliasResult ComputeAlias(AliasLocation locationA, 
                                     AliasLocation locationB) override;

	virtual MemoryResult ComputeCallEffects(CallInstr* instr) override;

	virtual bool HandlesCalls() const override {
        return true;
    }

    virtual string Name() override {
        return "Call Alias Analysis";
    }
};

} // namespace Analysis 
#endif