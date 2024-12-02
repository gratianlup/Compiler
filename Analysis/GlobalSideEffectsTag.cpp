// GlobalSideEffectsTag.cpp
// Copyright (c) Lup Gratian
//
// Implements the GlobalSideEffectsTag tag.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "GlobalSideEffectsTag.hpp"

namespace Analysis {

void GlobalSideEffectsTag::AddVariable(int variableId, bool isRead, 
                                       bool isWritten, bool alwaysWritten) {
    globalInfo_.SetBit(4 * variableId);
    hasNoVariables_ = false;

    if(isRead) {
        globalInfo_.SetBit(4 * variableId + 1);

        // If the variable is marked as being always written
        // we reset the flag, because we don't record if the read
        // is done before or after the variable is written.
        if(globalInfo_.IsSet(4 * variableId + 3)) {
            globalInfo_.ResetBit(4 * variableId + 3);
            alwaysWrittenCount_--;
        }
    }

    if(isWritten) {
        globalInfo_.SetBit(4 * variableId + 2);
    }

    if(alwaysWritten) {
        // If the variable is also read don't set the flag,
        // because we don't record if the read is done before
        // or after the variable is written.
        if(globalInfo_.IsNotSet(4 * variableId + 1)) {
            globalInfo_.SetBit(4 * variableId + 3);
            alwaysWrittenCount_++;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffectsTag::ReadsVariables() const {
    if(hasNoVariables_) {
        return false;
    }

    // Check if any variable has the 'read' bit set.
    bool reads = false;

    globalInfo_.ForEachSetBit([&reads](int index) -> bool {
        if((index % 4) == 1) {
            reads = true;
            return false;
        }
        else return true;
    });

    return reads;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffectsTag::WritesVariables() const {
    if(hasNoVariables_) {
        return false;
    }
    else if(alwaysWrittenCount_ > 0) {
        return true;
    }

    // Check if any variable has the 'write' bit set.
    bool writes = false;

    globalInfo_.ForEachSetBit([&writes](int index) -> bool {
        if((index % 4) == 2) {
            writes = true;
            return false;
        }
        else return true;
    });

    return writes;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffectsTag::Merge(GlobalSideEffectsTag* other) {
    DebugValidator::IsNotNull(other);
    DebugValidator::AreNotEqual(other, this);

    if(HasUnknownEffects()) {
        // No reason to merge anymore.
        return;
    }
    else if(other->HasUnknownEffects()) {
        // Mark this function as having unknown side-effects too.
        MarkHasUnknownEffects();
    }
    else {
        // Merge the accessed variables. Note that this also merges
        // the performed operations (read/written).
        globalInfo_.Or(other->globalInfo_);

        // For the "always written" flag the operation is intersection.
        if(alwaysWrittenCount_ > 0) {
            IntersectAlwaysWritten(other);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffectsTag::IntersectAlwaysWritten(GlobalSideEffectsTag* other) {
    // A global variable is definitely written only if
    // it is definitely written in the other function too.
    int resetCount = 0;

    globalInfo_.ForEachSetBit([this, other, &resetCount](int index) -> bool {
        if((index % 4) == 3) {
            // If the other function doesn't always write the variable,
            // or reads it, the flag needs to be reset to be safe.
            if(other->globalInfo_.IsNotSet(index) ||
               other->globalInfo_.IsSet(index - 2)) {
                globalInfo_.ResetBit(index);
                resetCount++;
            }
        }

        return true;
    });

    alwaysWrittenCount_ -= resetCount;
}

} // namespace Analysis