// ParameterConstantsTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to associate constant values
// or ranges with a parameter. For each constant we remember 
// from which function it is incoming.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_PARAMETER_CONSTANTS_TAG_HPP
#define PC_ANALYSIS_PARAMETER_CONSTANTS_TAG_HPP

#include "RangeTag.hpp"
#include "../IR/Constants.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Create a pair formed from a constant 
// and the function from which it is incoming.
MAKE_PAIR(ConstantFunctionPair, Constant*, Value, FunctionReference*, IncomingFunction);

class ParameterConstantsTag : public Tag {
private:
    union {
        StaticList<ConstantFunctionPair, 4>* constants_;
        Range* range_;
    };
    
    bool isRange_;   // 'true' if 'range_' should be considered only.
    bool isNotZero_; // 'true' if the value is guaranteed never zero.
    bool safeToUse_; // 'true' if the information is safe to be used.
                     // 'false' if the functions is external, for example.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    explicit ParameterConstantsTag(int capacity, bool isNotZero, bool safeToUse = true) :
            constants_(new StaticList<ConstantFunctionPair, 4>(capacity)),
            isRange_(false), isNotZero_(isNotZero), safeToUse_(safeToUse) {}

    explicit ParameterConstantsTag(bool isNotZero, bool safeToUse = true) : 
            isRange_(false), isNotZero_(isNotZero), safeToUse_(safeToUse),
            constants_(nullptr) {}

    ParameterConstantsTag(Range range, bool isNotZero = false, bool safeToUse = true) :
            range_(new Range(range)), isRange_(true),
            isNotZero_(isNotZero), safeToUse_(safeToUse) {}

    ParameterConstantsTag(const ParameterConstantsTag&);             // Should not be copied.
    ParameterConstantsTag& operator= (const ParameterConstantsTag&); // Should not be assigned.

public:
    static const int Id = 0x3b70588f;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual ~ParameterConstantsTag() {
        if(isRange_) {
            delete range_;
        }
        else if(constants_) {
            constants_->ForEach([](ConstantFunctionPair& pair) -> bool {
                pair.IncomingFunction->Free();
                return true;
            });

            delete constants_;
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

    // Creates a new tag, optionally adding marking the fact
    // that the value can never be zero.
    static ParameterConstantsTag* GetIsNotZero(bool safeToUse = true) {
        return new ParameterConstantsTag(true /* isNotZero */, safeToUse);
    }

    // Creates a new tag, optionally adding marking the fact
    // that the value can never be zero.
    static ParameterConstantsTag* GetParameterConstants(int capacity, bool isNotZero, 
                                                        bool safeToUse = true) {
        return new ParameterConstantsTag(capacity, isNotZero, safeToUse);
    }

    // Creates a new tag, optionally adding marking the fact
    // that the value can never be zero.
    static ParameterConstantsTag* GetParameterRange(Range range, bool isNotZero, 
                                                    bool safeToUse = true) {
        return new ParameterConstantsTag(range, isNotZero, safeToUse);
    }

    // Returns 'true' if the associated range/constant list
    // can be used for optimizations. This might be 'false' if
    // the function is external, for example.
    bool IsSafeToUse() const {
        return safeToUse_;
    }

    void SetIsSafeToUse(bool value) {
        safeToUse_ = value;
    }

    // Returns 'true' if the parameter has an associated range,
    // not a list of constants. Note that 'IsNotZero' might still be used.
    bool IsRange() const {
        return isRange_;
    }

    // Returns 'true' if the individual constants associated 
    // with the parameter are known.
    bool IsConstantList() const {
        return isRange_ == false;
    }

    // Returns 'true' if the parameter is definitely never zero.
    bool IsNotZero() const {
        return isNotZero_;
    }

    void SetIsNotZero(bool value) {
        isNotZero_ = value;
    }

    // Transitions from a range representation to a 
    // list of all possible constants.
    void ConvertToConstantList() {
        if(isRange_) {
            isRange_ = false;
            constants_ = new StaticList<ConstantFunctionPair, 4>();
        }
    }

    // Adds a constant that is incoming from the specified function.
    void AddConstant(Constant* constant, FunctionReference* functionRef) {
        DebugValidator::IsNotNull(constant);
        DebugValidator::IsNotNull(functionRef);
        DebugValidator::IsTrue(IsConstantList());

        if(constants_ == nullptr) {
            constants_ = new StaticList<ConstantFunctionPair, 4>();
        }

        constants_->Add(ConstantFunctionPair(constant, functionRef));
        functionRef->AddUser();
    }

    // Returns the number of constants in the list.
    // Note that a constant might appear multiple times
    // if it is incoming from more than one function.
    int ConstantCount() const {
        if(isRange_ || (constants_ == nullptr)) {
            return 0;
        }
        else return constants_->Count();
    }

    // Returns the number of unique constants in the list.
    // This excludes the same constants that are incoming
    // from other functions than the first one.
    int UniqueConstantCount() const {
        if(isRange_ || (constants_ == nullptr)) {
            return 0;
        }

        // Scan backwards to check if the constant was seen before.
        // For only a few constants this is fast enough.
        int count = 0;

        for(int i = 0; i < constants_->Count(); i++) {
            bool found = false;

            for(int j = i - 1; j >= 0; j--) {
                if((*constants_)[j].Value == (*constants_)[i].Value) {
                    found = true;
                    break;
                }
            }

            if(found == false) {
                count++;
            }
        }

        return count;
    }

    // Returns the pair of a constant and the function
    // from where it is incoming found at the specified position.
    ConstantFunctionPair& GetConstantFunctionPair(int index) {
        DebugValidator::IsTrue(IsConstantList());
        return (*constants_)[index];
    }

    // Returns the constant found at the specified position.
    Constant* GetConstant(int index) {
        DebugValidator::IsTrue(IsConstantList());
        return (*constants_)[index].Value;
    }

    // Returns the incoming function associated with the constant 
    // found at the specified position.
    FunctionReference* GetIncomingFunction(int index) {
        DebugValidator::IsTrue(IsConstantList());
        return (*constants_)[index].IncomingFunction;
    }
    
    // Returns 'true' if the specified constant is associated 
    // with the parameter at least once.
    bool HasConstant(Constant* constant) const {
        DebugValidator::IsNotNull(constant);
        DebugValidator::IsTrue(IsConstantList());

        if(constants_ == nullptr) {
            return false;
        }

        for(int i = 0; i < constants_->Count(); i++) {
            if((*constants_)[i].Value == constant) {
                return true;
            }
        }

        return false;
    }

    // Returns 'true' if the specified value is associated 
    // with the parameter at least once.
    bool HasIntegerConstant(__int64 value) {
        DebugValidator::IsTrue(IsConstantList());

        if(constants_ == nullptr) {
            return false;
        }

        for(int i = 0; i < constants_->Count(); i++) {
            if(auto intConstant = (*constants_)[i].Value->As<IntConstant>()) {
                if(intConstant->Value() == value) {
                    return true;
                }
            }
        }

        return false;
    }

    // Returns 'true' if there is a constant incoming from
    // the specified function. If the list is specified all such
    // constants are added to it.
    bool HasConstantIncomingFrom(FunctionReference* functionRef, 
                                 List<Constant*>* constants = nullptr) const {
        DebugValidator::IsNotNull(functionRef);
        DebugValidator::IsTrue(IsConstantList());
        bool found = false;

        if(constants_ == nullptr) {
            return false;
        }

        for(int i = 0; i < constants_->Count(); i++) {
            if((*constants_)[i].IncomingFunction == functionRef) {
                found = true;

                if(constants) {
                    constants->Add((*constants_)[i].Value);
                }
            }
        }

        return found;
    }

    // Removes the specified constant, no matter from which
    // function it is incoming.
    void RemoveConstant(Constant* constant) {
        DebugValidator::IsNotNull(constant);
        DebugValidator::IsTrue(IsConstantList());

        if(constants_ == nullptr) {
            return;
        }

        for(int i = 0; i < constants_->Count(); i++) {
            if((*constants_)[i].Value == constant) {
                (*constants_)[i].IncomingFunction->Free();
                constants_->RemoveAt(i);
                i--;
            }        
        }
    }

    // Removes the specified constant, but only if it is
    // incoming form the specified function.
    void RemoveConstant(Constant* constant, FunctionReference* functionRef) {
        DebugValidator::IsNotNull(constant);
        DebugValidator::IsNotNull(functionRef);
        DebugValidator::IsTrue(IsConstantList());

        if(constants_ == nullptr) {
            return;
        }

        for(int i = 0; i < constants_->Count(); i++) {
            if(((*constants_)[i].Value == constant) &&
               ((*constants_)[i].IncomingFunction == functionRef)) {
                (*constants_)[i].IncomingFunction->Free();
                constants_->RemoveAt(i);
                i--;
            }        
        }
    }
    
    // Removes all constant incoming from the specified functions.
    void RemoveConstantIncomingFrom(FunctionReference* functionRef) {
        DebugValidator::IsNotNull(functionRef);
        DebugValidator::IsTrue(IsConstantList());

        if(constants_ == nullptr) {
            return;
        }

        for(int i = 0; i < constants_->Count(); i++) {
            if((*constants_)[i].IncomingFunction == functionRef) {
                (*constants_)[i].IncomingFunction->Free();
                constants_->RemoveAt(i);
                i--;
            }        
        }
    }

    // Removes all constants associated with the parameter.
    void ClearConstants() {
        constants_->Clear();
    }

    // Performs the specified action on each constant.
    // bool Predicate(Constant* constant, FunctionReference* incomingFunct)
    template <class Predicate>
    void ForEachConstant(Predicate action) {
        DebugValidator::IsTrue(IsConstantList());
        
        if(constants_ == nullptr) {
            return;
        }

        for(int i = 0; i < constants_->Count(); i++) {
            if(action((*constants_)[i].Value, 
                      (*constants_)[i].IncomingFunction) == false) {
                return;
            }
        }
    }

    // Performs the specified action on each unique constant.
    // bool Predicate(Constant* constant)
    template <class Predicate>
    void ForEachUniqueConstant(Predicate action) {
        DebugValidator::IsTrue(IsConstantList());
        StaticList<Constant*, 4> uniqueConstants;

        if(constants_ == nullptr) {
            return;
        }

        for(int i = 0; i < constants_->Count(); i++) {
            if(uniqueConstants.Contains((*constants_)[i].Value) == false) {
                uniqueConstants.Add((*constants_)[i].Value);
            }
        }

        for(int i = 0; i < uniqueConstants.Count(); i++) {
            if(action(uniqueConstants[i]) == false) {
                return;
            }
        }
    }

    // Returns the range associated with the parameter.
    // Note that a check that ranges are used should be done first.
    const Range& GetRange() const {
        DebugValidator::IsTrue(IsRange());
        return *range_;
    }

    // Associates the specified range with the parameter.
    // Note that any previously associated constant is removed.
    void SetRange(const Range& value) {
        isRange_ = true;
        *range_ = value;
        delete constants_;
    }
};

} // namespace Analysis

namespace IR {
    namespace Detail {
        // Implements support for "dynamic cast".
        template <>
        struct TagPromoter<Analysis::ParameterConstantsTag> {
            static bool Is(const Tag* tag) {
                return tag->GetId() == Analysis::ParameterConstantsTag::Id;
            }

            static Analysis::ParameterConstantsTag* As(Tag* tag) {
                return Is(tag) ? static_cast<Analysis::ParameterConstantsTag*>(tag) : nullptr;
            }
        };
    } // namespace Detail
} // namespace IR
#endif