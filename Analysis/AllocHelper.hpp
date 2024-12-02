// AllocHelper.hpp
// Copyright (c) Lup Gratian
//
// Implements a helper class for recognizing operands that
// originate from calls to memory allocation routines.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_ALLOC_HELPER_HPP
#define PC_ANALYSIS_ALLOC_HELPER_HPP

#include "LanguageInfo.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class AllocHelper {
private:
	typedef List<CallInstr*> AllocCallList;

	LanguageInfo* languageInfo_;

public:
	AllocHelper(LanguageInfo* languageInfo = nullptr) :
			languageInfo_(languageInfo) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if the called function is an allocation function
    // (for C this would be 'malloc', for example).
    bool IsAllocCall(CallInstr* instr);

	// Returns 'true' if the operand originates from dynamically
    // allocated memory (this walks the chain of addressing instructions
    // until it finds a call to an allocation function).
    // If 'allocList' is specified the found calls are added to the list.
    bool OriginatesFromAlloc(Operand* op, AllocCallList* allocList = nullptr, 
                             int level = 0);

	// Returns 'true' if none of the allocation calls
    // from 'listA' are found in 'listB'.
    bool AreAllocCallsIndependent(AllocCallList* listA, 
                                  AllocCallList* listB);

	// Returns the operand that acts as the base
    // of a series of addressing instructions.
    Operand* GetBaseOperand(Operand* op);
};

} // namespace Analysis
#endif