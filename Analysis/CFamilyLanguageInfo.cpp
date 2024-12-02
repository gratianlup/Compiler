// CFamilyLanguageInfo.cpp
// Copyright (c) Lup Gratian
//
// Implements the CFamilyLanguageInfo analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "CFamilyLanguageInfo.hpp"

namespace Analysis {

bool CFamilyLanguageInfo::IsStdlibCall(CallInstr* instr, StdlibType& type,
                                       StdlibCategory& category) const {
    if(auto function = instr->GetCalledFunction()) {
        type = StdlibRecognizer::Recognize(function, &category);
        return type != StdlibType::None;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::IsStdlibCall(Function* function, StdlibType& type,
                                       StdlibCategory& category) const {
    type = StdlibRecognizer::Recognize(function, &category);
    return type != StdlibType::None;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayHaveSideEffects(Function* function, 
												 bool* hasIndirectWrite) const {
    DebugValidator::IsNotNull(function);
    
    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(function, type, category)) {
        // Math functions don't have side effects only if
        // 'errno' is not used by the application.
        if(((category == StdlibCategory::Math) && (IsErrnoUsed() == false)) ||
           (type == StdlibType::memcmp)  ||
           (type == StdlibType::strlen)  ||
           (type == StdlibType::strcmp)  ||
           (type == StdlibType::strncmp) ||
           (type == StdlibType::strchr)  ||
           (type == StdlibType::isdigit) ||
           (type == StdlibType::isascii)) {
			if(hasIndirectWrite) {
				*hasIndirectWrite = false;
			}

            return false;
        }
    }

	if(hasIndirectWrite) {
		*hasIndirectWrite = true;
	}

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayCaptureParameters(CallInstr* instr) const {
    DebugValidator::IsNotNull(instr);

    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(instr, type, category)) {
        // The only standard library functions that capture
        // a parameter are the ones that return it.
        switch(type) {
            case StdlibType::memcpy:
            case StdlibType::memmove:
            case StdlibType::memset:
            case StdlibType::strcpy:
            case StdlibType::strncpy:
            case StdlibType::strncat:
            case StdlibType::strchr:
            case StdlibType::strstr:
            case StdlibType::strpbrk: {
                return instr->HasDestinationOp() &&
                       instr->GetDestinationOp()->HasUsers();
            }
            default: return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayCaptureParameter(Function* function, CallInstr* instr,
                                                  int paramIndex, bool* paramReturned) const {
    DebugValidator::IsNotNull(function);

    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(function, type, category)) {
        // The only standard library functions that capture
        // a parameter are the ones that return it, and this
        // can only be the first (destination) parameter.
        switch(type) {
            case StdlibType::memcpy:
            case StdlibType::memmove:
            case StdlibType::memset:
            case StdlibType::strcpy:
            case StdlibType::strncpy:
            case StdlibType::strncat:
            case StdlibType::strchr:
            case StdlibType::strstr:
            case StdlibType::strpbrk: {
                if((paramIndex == 0) &&
                   instr->HasDestinationOp() &&
                   instr->GetDestinationOp()->HasUsers()) {
                    if(paramReturned) {
                        *paramReturned = true;
                    }

                    return true;
                }
            }
            default: return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayReadFromAddress(CallInstr* instr, Operand* addressOp,
                                                 AliasResultProvider* aliasResult) const {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(addressOp);

    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(instr, type, category)) {
        // All functions in the 'math' category don't read from user memory.
        if(category == StdlibCategory::Math) {
            return false;
        }

        // Try to take a decision based on the type 
        // of the standard library function.
        switch(type) {
            case StdlibType::memcpy:
            case StdlibType::memmove:
            case StdlibType::strcpy:
            case StdlibType::strncpy: {
                // We may prove that these memory-copying functions read
                // from a different variable than the one referenced by the address.
                if(aliasResult) {
                    auto source = instr->GetArgument(1);

                    if(aliasResult->IsDefinitelyNoAlias(addressOp, source)) {
                        return false;
                    }
                }

                return true;
            }
            case StdlibType::strcat:
            case StdlibType::strncat:
            case StdlibType::memcmp:
            case StdlibType::strcmp:
            case StdlibType::strncmp:
            case StdlibType::strstr: {
                // As above, but in this case we need to consider both arguments.
                if(aliasResult) {
                    auto first = instr->GetArgument(0);
                    auto second = instr->GetArgument(0);

                    if(aliasResult->IsDefinitelyNoAlias(addressOp, first) &&
                       aliasResult->IsDefinitelyNoAlias(addressOp, second)) {
                        return false;
                    }
                }
                
                return true;
            }
            case StdlibType::strlen:
            case StdlibType::strchr: {
                // As above, but only the first argument needs to be considered.
                if(aliasResult) {
                    auto source = instr->GetArgument(0);
                    
                    if(aliasResult->IsDefinitelyNoAlias(addressOp, source)) {
                        return false;
                    }
                }
                
                return true;
            }
            case StdlibType::isdigit:
            case StdlibType::isascii: {
                // These functions never read to user memory.
                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayWriteToAddress(CallInstr* instr, Operand* addressOp,
                                                AliasResultProvider* aliasResult) const {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(addressOp);

    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(instr, type, category)) {
        // All functions in the 'math' category don't write to user memory.
        if(category == StdlibCategory::Math) {
            return false;
        }

        // Try to take a decision based on the type 
        // of the standard library function.
        switch(type) {
            case StdlibType::memcpy:
            case StdlibType::memset:
            case StdlibType::memmove:
            case StdlibType::strcpy:
            case StdlibType::strncpy:
            case StdlibType::strcat:
            case StdlibType::strncat: {
                // We may prove that these memory-related functions
                // write to a different variable than the one from which we load.
                if(aliasResult) {
                    auto dest = instr->GetArgument(0);
                    
                    if(aliasResult->IsDefinitelyNoAlias(addressOp, dest)) {
                        return false;
                    }
                }

                return true;
            }
            case StdlibType::memcmp:
            case StdlibType::strlen:
            case StdlibType::strcmp:
            case StdlibType::strncmp:
            case StdlibType::strchr:
            case StdlibType::strstr:
            case StdlibType::isdigit:
            case StdlibType::isascii: {
                // These functions never write to user memory.
                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayWriteGlobals(Function* function) const {
	DebugValidator::IsNotNull(function);

	StdlibType type;
    StdlibCategory category;
	return IsStdlibCall(function, type, category) == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CallMayReadGlobals(Function* function) const {
	DebugValidator::IsNotNull(function);
	
	StdlibType type;
    StdlibCategory category;
    return IsStdlibCall(function, type, category) == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::CanBeInlined(Function* function) const {
    DebugValidator::IsNotNull(function);

    // A C function should not be inlined it contains a call
    // to 'alloca' (the optimized code could blow up the stack,
    // while the original code did not) or a call to 'longjmp'.
    if(auto familyTag = function->GetTag<CFamilyTag>()) {
        return familyTag->HasAlloca() ||
               familyTag->HasLongjmp();
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::IsAllocFunction(Function* function) const {
    DebugValidator::IsNotNull(function);

    // 'malloc' and 'alloca' are considered memory allocation functions.
    // We also check for functions that behave like 'malloc'.
    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(function, type, category)) {
        return (type == StdlibType::malloc) ||
               (type == StdlibType::alloca);
    }
    else if(auto familyTag = function->GetTag<CFamilyTag>()) {
        return familyTag->IsAllocLike() ||
               familyTag->IsCheckedAllocLike();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::IsCheckedAllocFunction(Function* function) const {
    DebugValidator::IsNotNull(function);

    // 'alloca' is considered to be a checked allocation function
    // because it never returns 'nullptr' (although it might blow up the stack).
    // Another type of checked allocation is a function written
    // by the user that verifies if the call to 'malloc' returned 'nullptr'
    // and in that case terminates the program by calling 'exit'/'abort'.
    StdlibType type;
    StdlibCategory category;

    if(IsStdlibCall(function, type, category)) {
        return type == StdlibType::alloca;
    }
    else if(auto familyTag = function->GetTag<CFamilyTag>()) {
        return familyTag->IsCheckedAllocLike();
    }

    return false;

}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::IsProgramEntryPoint(Function* function) const {
    // The entry point for a C language is the 'main' function.
    return function->IsExtern() && function->HasName() &&
           (*function->Name() == "main");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool CFamilyLanguageInfo::IsProgramExitPoint(Function* function) const {
    StdlibType type;
    StdlibCategory category;

	if(function->IsExtern() && 
	   IsStdlibCall(function, type, category)) {
		return (type == StdlibType::exit) ||
			   (type == StdlibType::abort);
	}

	return false;
}

} // namespace Analysis