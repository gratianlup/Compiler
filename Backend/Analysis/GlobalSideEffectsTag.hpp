// GlobalSideEffectsTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag used to store which global variables
// are read/written by a function and the functions it calls.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_GLOBAL_SIDE_EFFECTS_TAG_HPP
#define PC_ANALYSIS_GLOBAL_SIDE_EFFECTS_TAG_HPP

#include "SparseBitVector.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class GlobalSideEffectsTag : public FunctionTag {
private:
    // Each global variable is represented as follows:
    // Bit [4*Id + 0]: 1 if variable is mentioned in the function, 0 otherwise.
    // Bit [4*Id + 1]: 1 if the variable is read, 0 otherwise.
    // Bit [4*Id + 2]: 1 if the variable is written, 0 otherwise.
    // Bit [4*Id + 4]: 1 if the variable is always written, 0 otherwise.
    SparseBitVector globalInfo_;
    bool hasNoVariables_;
    bool hasUnknownEffects_;
    short alwaysWrittenCount_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    GlobalSideEffectsTag() : hasNoVariables_(true), hasUnknownEffects_(false),
                             alwaysWrittenCount_(0) {}             // Should not be created.
	GlobalSideEffectsTag(const GlobalSideEffectsTag&);             // Should not be copied.
	GlobalSideEffectsTag& operator= (const GlobalSideEffectsTag&); // Should not be assigned.
    
    void IntersectAlwaysWritten(GlobalSideEffectsTag* other);

public:
    static const int Id = 0x9a8a6104;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

    // Creates a new tag having no variable set.
    static GlobalSideEffectsTag* GetGlobalSideEffects() {
        return new GlobalSideEffectsTag();
    }

    // Adds the specified variable Id to the ones read/written by the function.
    // If it is already added the read/written flags are updated.
    void AddVariable(int variableId, bool isRead = true, 
                     bool isWritten = true, bool alwaysWritten = false);

    // Adds the specified variable to the ones read/written by the function.
    // If it is already added the read/written flags are updated.
    void AddVariable(GlobalVariable* variable, bool isRead = true, 
                     bool isWritten = true, bool alwaysWritten = false) {
        DebugValidator::IsNotNull(variable);
        AddVariable(variable->Id(), isRead, isWritten, alwaysWritten);
    }

    // Marks the specified variable as being read in the function.
    void SetIsRead(int variableId) {
        globalInfo_.SetBit(variableId * 4);
        globalInfo_.SetBit(variableId * 4 + 1);
    }

    void SetIsRead(GlobalVariable* variable) {
        DebugValidator::IsNotNull(variable);
        SetIsRead(variable->Id());
    }

    // Marks the specified variable as being written in the function.
    void SetIsWritten(int variableId) {
        globalInfo_.SetBit(variableId * 4);
        globalInfo_.SetBit(variableId * 4 + 2);
    }

    void SetIsWritten(GlobalVariable* variable) {
        DebugValidator::IsNotNull(variable);
        SetIsWritten(variable->Id());
    }

    // Marks the specified variable as being always written in the function.
    void SetIsAlwaysWritten(int variableId) {
        globalInfo_.SetBit(variableId * 4);
        globalInfo_.SetBit(variableId * 4 + 2);
        globalInfo_.SetBit(variableId * 4 + 3);
    }

    void SetIsAlwaysWritten(GlobalVariable* variable) {
        DebugValidator::IsNotNull(variable);
        SetIsAlwaysWritten(variable->Id());
    }

    // Returns 'true' if the function reads or writes 
    // the variable having the specified Id.
    bool ReadsOrWritesVariable(int variableId) const {
        return globalInfo_.IsSet(4 * variableId);
    }

    // Returns 'true' if the function reads or writes the specified variable.
    bool ReadsOrWritesVariable(GlobalVariable* variable) const {
        DebugValidator::IsNotNull(variable);
        return ReadsOrWritesVariable(variable->Id());
    }

    // Returns 'true' if the function reads
    // the variable having the specified Id.
    bool ReadsVariable(int variableId) const {
        return globalInfo_.IsSet(4 * variableId) &&
               globalInfo_.IsSet(4 * variableId + 1);
    }

    // Returns 'true' if the function reads the specified variable.
    bool ReadsVariable(GlobalVariable* variable) const {
        DebugValidator::IsNotNull(variable);
        return ReadsVariable(variable->Id());
    }

    // Returns 'true' if the function does not read the specified variable.
    bool DoesNotReadVariable(int variableId) const {
        return ReadsVariable(variableId) == false;
    }

    bool DoesNotReadVariable(GlobalVariable* variable) const {
        return ReadsVariable(variable) == false;
    }

    // Returns 'true' if the function writes
    // the variable having the specified Id.
    bool WritesVariable(int variableId) const {
        return globalInfo_.IsSet(4 * variableId) &&
               globalInfo_.IsSet(4 * variableId + 2);
    }

    // Returns 'true' if the function writes the specified variable.
    bool WritesVariable(GlobalVariable* variable) const {
        DebugValidator::IsNotNull(variable);
        return WritesVariable(variable->Id());
    }

    // Returns 'true' if the function does not read the specified variable.
    bool DoesNotWriteVariable(int variableId) const {
        return WritesVariable(variableId) == false;
    }

    bool DoesNotWriteVariable(GlobalVariable* variable) const {
        return WritesVariable(variable) == false;
    }

    // Returns 'true' if the function always writes the variable
    // having the specified Id (i.e the variable is written on all
    // paths that lead to the function return).
    bool AlwaysWritesVariable(int variableId) const {
        return WritesVariable(variableId) &&
               globalInfo_.IsSet(4 * variableId + 3);
    }

    // Returns 'true' if the function always writes the specified variable.
    bool AlwaysWritesVariable(GlobalVariable* variable) const {
        DebugValidator::IsNotNull(variable);
        return AlwaysWritesVariable(variable->Id());
    }

    // Returns 'true' if any non-address-taken global variable
    // is read or written inside the function.
    bool HasVariables() const {
        return hasNoVariables_ == false;
    }

    // Returns 'true' if at least one global variable
    // is always written in the function.
    bool HasAlwaysWrittenVariables() const {
        return alwaysWrittenCount_ > 0;
    }

    // Returns 'true' if any global variable is read by the function.
    bool ReadsVariables() const;

    // Returns 'true' if any global variable is written by the function.
    bool WritesVariables() const;

    // Returns 'true' if the effect of the function
    // on the global variables is unknown or uncertain.
    bool HasUnknownEffects() const {
        return hasUnknownEffects_;
    }

    // Marks the fact that the effect of the function is unknown.
    void MarkHasUnknownEffects() {
        hasUnknownEffects_ = true;
        globalInfo_.Clear();
    }

    // Combines the variables (including the read/write information)
    // with the ones from the specified tag. Can be used to merge
    // the effects of two potentially called functions.
    void Merge(GlobalSideEffectsTag* other);

    // Performs the specified action on each read variable.
    // bool Predicate(int variableIndex);
    template <class Predicate>
    void ForEachReadVariable(Predicate action) {
        globalInfo_.ForEachSetBit([action](int index) -> bool {
            if((index % 4) == 1) {
                if(action((index - 1) / 4) == false) {
                    return false;
                }
            }

            return true;
        });
    }

    // Performs the specified action on each read variable.
    // bool Predicate(GlobalVariable* variable);
    template <class Predicate>
    void ForEachReadVariable(Predicate action, Unit* unit) {
        DebugValidator::IsNotNull(unit);

        globalInfo_.ForEachSetBit([unit, action](int index) -> bool {
            if((index % 4) == 1) {
                auto symbol = unit->GetSymbolWithId((index - 1) / 4);

                if(action(symbol->As<GlobalVariable>()) == false) {
                    return false;
                }
            }

            return true;
        });
    }

    // Performs the specified action on each written variable.
    // bool Predicate(int variableIndex, bool always);
    template <class Predicate>
    void ForEachWrittenVariable(Predicate action) {
        globalInfo_.ForEachSetBit([action, this](int index) -> bool {
            if((index % 4) == 2) {
                if(action((index - 2) / 4, globalInfo_.IsSet(index + 1)) == false) {
                    return false;
                }
            }

            return true;
        });
    }

    // Performs the specified action on each written variable.
    // bool Predicate(GlobalVariable* variable, bool always);
    template <class Predicate>
    void ForEachWrittenVariable(Predicate action, Unit* unit) {
        DebugValidator::IsNotNull(unit);

        globalInfo_.ForEachSetBit([unit, action, this](int index) -> bool {
            if((index % 4) == 2) {
                auto symbol = unit->GetSymbolWithId((index - 2) / 4);

                if(action(symbol->As<GlobalVariable>(),
                          globalInfo_.IsSet(index + 1)) == false) {
                    return false;
                }
            }

            return true;
        });
    }

    // Performs the specified action on each always written variable.
    // bool Predicate(int variableIndex);
    template <class Predicate>
    void ForEachAlwaysWrittenVariable(Predicate action) {
        globalInfo_.ForEachSetBit([action, this](int index) -> bool {
            if((index % 4) == 3) {
                if(action((index - 3) / 4) == false) {
                    return false;
                }
            }

            return true;
        });
    }

    // Performs the specified action on each always written variable.
    // bool Predicate(GlobalVariable* variable, bool always);
    template <class Predicate>
    void ForEachAlwaysWrittenVariable(Predicate action, Unit* unit) {
        DebugValidator::IsNotNull(unit);

        globalInfo_.ForEachSetBit([unit, action, this](int index) -> bool {
            if((index % 4) == 3) {
                auto symbol = unit->GetSymbolWithId((index - 3) / 4);

                if(action(symbol->As<GlobalVariable>()) == false) {
                        return false;
                }
            }

            return true;
        });
    }

    bool operator== (const GlobalSideEffectsTag& other) const {
        return (hasNoVariables_ == other.hasNoVariables_)       &&
               (hasUnknownEffects_ == other.hasUnknownEffects_) &&
               (globalInfo_ == other.globalInfo_);
    }

    bool operator!= (const GlobalSideEffectsTag& other) const {
        return operator== (other) == false;
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::GlobalSideEffectsTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::GlobalSideEffectsTag::Id;
		}

		static Analysis::GlobalSideEffectsTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::GlobalSideEffectsTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif