// CFamilyLanguageInfo.cpp
// Copyright (c) Lup Gratian
//
// Provides information about C-like languages.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_C_FAMILY_LANGUAGE_INFO_HPP
#define PC_ANALYSIS_C_FAMILY_LANGUAGE_INFO_HPP

#include "LanguageInfo.hpp"
#include "StdLibRecognizer.hpp"
#include "CFamilyTag.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace Analysis {

class CFamilyLanguageInfo : public LanguageInfo {
private:
    bool IsStdlibCall(CallInstr* instr, StdlibType& type,
                      StdlibCategory& category) const;

    bool IsStdlibCall(Function* function, StdlibType& type,
                      StdlibCategory& category) const;

    bool IsErrnoUsed() const {
        return false;
    }

public:
    virtual bool CallMayHaveSideEffects(CallInstr* instr, bool* hasIndirectWrite) 
                                        const override {
        if(auto function = instr->GetCalledFunction()) {
            return CallMayHaveSideEffects(function, hasIndirectWrite);
        }

        return true;
    }

    virtual bool CallMayHaveSideEffects(Function* function, bool* hasIndirectWrite) 
                                        const override;

    virtual bool CallMayCaptureParameters(CallInstr* instr) const override;

    virtual bool CallMayCaptureParameter(CallInstr* instr, int paramIndex,
                                         bool* paramReturned = nullptr) 
                                         const override {
        if(auto function = instr->GetCalledFunction()) {
            return CallMayCaptureParameter(function, instr,
                                           paramIndex, paramReturned);
        }

        return true;
    }

    virtual bool CallMayCaptureParameter(Function* function, CallInstr* instr, 
                                         int paramIndex, bool* paramReturned = nullptr) 
                                         const override;

    virtual bool CallMayReadFromAddress(CallInstr* instr, Operand* addressOp, 
                                        AliasResultProvider* aliasResult = nullptr) 
                                        const override;

    virtual bool CallMayWriteToAddress(CallInstr* instr, Operand* addressOp, 
                                       AliasResultProvider* aliasResult = nullptr) 
                                       const override;

	virtual bool CallMayWriteGlobals(CallInstr* instr) const override {
		if(auto function = instr->GetCalledFunction()) {
            return CallMayWriteGlobals(instr);
        }

        return true;
	}

	virtual bool CallMayWriteGlobals(Function* function) const override;

	virtual bool CallMayReadGlobals(CallInstr* instr) const override {
		if(auto function = instr->GetCalledFunction()) {
            return CallMayReadGlobals(instr);
        }

        return true;
	}

	virtual bool CallMayReadGlobals(Function* function) const override;

    virtual bool CanBeInlined(Function* function) const override;

    virtual bool IsAllocFunction(Function* function) const override;

    virtual bool IsCheckedAllocFunction(Function* function) const override;

    virtual bool IsProgramEntryPoint(Function* function) const override;

	virtual bool IsProgramExitPoint(Function* function) const override;
};

} // namespace Analysis
#endif