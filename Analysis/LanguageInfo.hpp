// LanguageInfo.cpp
// Copyright (c) Lup Gratian
//
// Base class that provides information about various
// IR aspects that are language-dependent.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_LANGUAGE_INFO_HPP
#define PC_ANALYSIS_LANGUAGE_INFO_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Operand.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Provides alias information for the language.
// Used by 'CallMayReadFromAddress' and 'CallMayWriteToAddress'.
class AliasResultProvider {
public:
    virtual ~AliasResultProvider() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Should return 'true' if there may be alias between the locations.
    virtual bool MightBeAlias(Operand* a, Operand* b) {
        return true;
    }

    // Should return 'true' if there is no alias between the locations.    
    virtual bool IsDefinitelyNoAlias(Operand* a, Operand* b) {
        return false;
    }
};


class LanguageInfo /* TODO : public Analysis */ {
public:
    virtual ~LanguageInfo() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns 'true' if the called function has any side-effects
    // (writes to global memory or trough a pointer parameter, 
    //  calls a function with side-effects, etc.).
	// If 'hasIndirectWrite' is provided it is set to true if the
	// function indirectly writes to memory (using a loaded pointer).
    virtual bool CallMayHaveSideEffects(CallInstr* instr, 
										bool* hasIndirectWrite = nullptr) const {
        return true;
    }

    // Returns 'true' if the specified function has any side-effects.
    virtual bool CallMayHaveSideEffects(Function* function, 
										bool* hasIndirectWrite = nullptr) const {
        return true;
    }

    // Returns 'true' if the specified function has any side-effects.
    virtual bool CallMayHaveSideEffects(FunctionReference* functionRef,
										bool* hasIndirectWrite = nullptr) const {
        DebugValidator::IsNotNull(functionRef);
        return CallMayHaveSideEffects(functionRef->Target(), hasIndirectWrite);
    }

    // Returns 'true' if the called function captures 
    // any of it's pointer parameters (a parameter is captured
    // if its value is written to non-local memory or returned).
    virtual bool CallMayCaptureParameters(CallInstr* instr) const {
        return true;
    }

    // Returns 'true' if the specified parameter is captured
    // by the called function. If the parameter is returned
    // 'paramReturned' is set to 'true'.
    virtual bool CallMayCaptureParameter(CallInstr* instr, int paramIndex,
                                         bool* paramReturned = nullptr) const {
        return true;
    }

    virtual bool CallMayCaptureParameter(Function* function, CallInstr* instr, 
                                         int parameterIndex, 
                                         bool* paramReturned = nullptr) const {
        return true;
    }

    // Returns 'true' if the called function may read
    // from the specified address.
    virtual bool CallMayReadFromAddress(CallInstr* instr, Operand* addressOp, 
                                        AliasResultProvider* aliasResult = nullptr) const {
        return true;
    }

    // Returns 'true' if the called function may write
    // to the specified address.
    virtual bool CallMayWriteToAddress(CallInstr* instr, Operand* addressOp, 
                                       AliasResultProvider* aliasResult = nullptr) const {
        return true;
    }

	// Returns 'true' if the called function may write global variables.
	virtual bool CallMayWriteGlobals(CallInstr* instr) const {
		return true;
	}

	// Returns 'true' if the specified function may write global variables.
	virtual bool CallMayWriteGlobals(Function* function) const {
		return true;
	}

	// Returns 'true' if the function associated with specified function
	// reference may write global variables.
	virtual bool CallMayWriteGlobals(FunctionReference* functionRef) const {
		DebugValidator::IsNotNull(functionRef);
		return CallMayWriteGlobals(functionRef->Target());
	}

	// Returns 'true' if the called function might read global variables.
	virtual bool CallMayReadGlobals(CallInstr* instr) const {
		return true;
	}

	// Returns 'true' if the specified function may read global variables.
	virtual bool CallMayReadGlobals(Function* function) const {
		return true;
	}

	// Returns 'true' if the function associated with specified function
	// reference may read global variables.
	virtual bool CallMayReadGlobals(FunctionReference* functionRef) const {
		DebugValidator::IsNotNull(functionRef);
        return CallMayReadGlobals(functionRef->Target());
	}

    // Returns 'true' if the specified function can be inlined
    // according to the language rules.
    virtual bool CanBeInlined(Function* function) const {
        return true;
    }

    // Returns 'true' if the function associated with specified 
    // function reference can be inlined according to the language rules.
    virtual bool CanBeInlined(FunctionReference* functionRef) const {
        DebugValidator::IsNotNull(functionRef);
        return CanBeInlined(functionRef->Target());
    }

    // Returns 'true' if the specified function represents
    // a memory allocation function (like 'malloc' in C).
    virtual bool IsAllocFunction(Function* function) const {
        return false;
    }

    // Returns 'true' if the function associated with specified function
    // reference represents a memory allocation function (like 'malloc' in C).
    virtual bool IsAllocFunction(FunctionReference* functionRef) const {
        DebugValidator::IsNotNull(functionRef);
        return IsAllocFunction(functionRef->Target());
    }

    // Returns 'true' if the specified function represents
    // a memory allocation function which guarantees that it returns
    // a valid (not 'nullptr') memory address (like 'alloca' in C).
    virtual bool IsCheckedAllocFunction(Function* function) const {
        return false;
    }

    // Returns 'true' if the function associated with specified function
    // reference represents  a memory allocation function which guarantees 
    // that it returns a valid (not 'nullptr') memory address (like 'alloca' in C).
    virtual bool IsCheckedAllocFunction(FunctionReference* functionRef) const {
        DebugValidator::IsNotNull(functionRef);
        return IsAllocFunction(functionRef->Target());
    }

    // Returns 'true' if the specified function is the entry point
    // of the program (the first called function, 'main' in C).
    virtual bool IsProgramEntryPoint(Function* function) const {
        return false;
    }

    // Returns 'true' if the function associated with the specified
    // function reference is the entry point of the program 
    // (the first called function, 'main' in C).
    virtual bool IsProgramEntryPoint(FunctionReference* functionRef) const {
        DebugValidator::IsNotNull(functionRef);
        return IsProgramEntryPoint(functionRef->Target());
    }

	// Returns 'true' if the specified function is an exit point
    // of the program (call to 'exit'/'abort' in C).
    virtual bool IsProgramExitPoint(Function* function) const {
        return false;
    }

    // Returns 'true' if the function associated with the specified
    // function reference is an exit poit of the program 
	// (call to 'exit'/'abort' in C).
    virtual bool IsProgramExitPoint(FunctionReference* functionRef) const {
        DebugValidator::IsNotNull(functionRef);
        return IsProgramExitPoint(functionRef->Target());
    }

	// Returns 'true' if the function called by the specified instruction
	// is an exit poit of the program (call to 'exit'/'abort' in C).
    virtual bool IsProgramExitPoint(CallInstr* instr) const {
        DebugValidator::IsNotNull(instr);

		if(auto calledFunct = instr->GetCalledFunction()) {
			return IsProgramExitPoint(calledFunct);
		}
		else return false;
    }
};

} // namespace Analysis
#endif