// GlobalConstantsTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to associate constant values
// with a global variable. This can be helpful for certain heuristics.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_GLOBAL_CONSTANTS_TAG_HPP
#define PC_ANALYSIS_GLOBAL_CONSTANTS_TAG_HPP

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

// Create a pair formed from a constant and its probability to appear.
// The pairs can be compared and sorted in a descending order based on 'Probability'.
MAKE_PAIR_ORDERED_SECOND_DESC(ConstantProbabilityPair, Constant*, Value, float, Probability);

class GlobalConstantsTag : public Tag {
private:
    StaticList<ConstantProbabilityPair, 2> constants_;
    bool hasOnlyConstants_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    GlobalConstantsTag();                                      // Should not be created.
	GlobalConstantsTag(const GlobalConstantsTag&);             // Should not be copied.
	GlobalConstantsTag& operator= (const GlobalConstantsTag&); // Should not be assigned.

    GlobalConstantsTag(Constant* constA, float probabilityA, 
                       Constant* constB, float probabilityB) {
        if(constA) {
            constants_.Add(ConstantProbabilityPair(constA, probabilityA));
        }

        if(constB) {
            constants_.Add(ConstantProbabilityPair(constB, probabilityB));
        }
    }

public:
    static const int Id = 0x19bf1fcb;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

    // Creates a new tag, optionally adding up to two
    // constants with probabilities to the list.
    static GlobalConstantsTag* GetGlobalConstants(Constant* constA = nullptr, 
                                                  float probabilityA = 1.0,
                                                  Constant* constB = nullptr,
                                                  float probabilityB = 1.0) {
        return new GlobalConstantsTag(constA, probabilityA, constB, probabilityB);
    }

    // Returns the number of known constants.
    int ConstantCount() const {
        return constants_.Count();
    }

    // Adds the specified constant, with an optional probability.
    void AddConstant(Constant* constant, float probability = 1.0) {
        DebugValidator::IsNotNull(constant);
        constants_.Add(ConstantProbabilityPair(constant, probability));
    }

    // Returns the constant/probability pair found at the specified index.
    ConstantProbabilityPair GetConstantProbabilityPair(int index) const {
        return constants_[index];
    }

    // Returns the constant found at the specified index.
    Constant* GetConstant(int index) const {
        return constants_[index].Value;
    }

    // Returns the probability found at the specified index.
    float GetProbability(int index) const {
        return constants_[index].Probability;
    }

    // Returns 'true' if the specified constant is in the list.
    bool HasConstant(Constant* constant) const {
        DebugValidator::IsNotNull(constant);

        for(int i = 0; i < constants_.Count(); i++) {
            if(constants_[i].Value == constant) {
                return true;
            }
        }

        return false;
    }

    // Returns 'true' if the specified integer constant is in the list.
    bool HasIntegerConstant(__int64 value) const {
        for(int i = 0; i < constants_.Count(); i++) {
            if(auto intConstant = constants_[i].Value->As<IntConstant>()) {
                if(intConstant->Value() == value) {
                    return true;
                }
            }
        }

        return false;
    }

    // Removes the specified constant from the list.
    void RemoveConstant(Constant* constant) {
        DebugValidator::IsNotNull(constant);
        DebugValidator::IsTrue(HasConstant(constant));

        for(int i = 0; i < constants_.Count(); i++) {
            if(constants_[i].Value == constant) {
                constants_.RemoveAt(i);
                return;
            }
        }
    }

    // Returns 'true' if the associated global variable
    // has only these constants as the possible values.
    bool HasOnlyConstants() const {
        return hasOnlyConstants_;
    }

    void SetHasOnlyConstants(bool value) {
        hasOnlyConstants_ = value;
    }

    // Sorts the constants based on their probability using
    // a descending order (the most probable constant is first).
    void SortByProbability() {
        constants_.Sort();
    }

    // Performs the specified action on each constant.
    // bool Predicate(Constant* constant, float probability)
    template <class Predicate>
    void ForEachConstant(Predicate action) {
        for(int i = 0; i < constants_.Count(); i++) {
            if(action(constants_[i].Value, 
                      constants_[i].Probability) == false) {
                return;
            }
        }
    }

    unsigned GetHashCode() const {
        if(constants_.Count() == 0) {
            return 0;
        }
        else {
            unsigned hash = constants_[0].Value->GetHashCode();

            for(int i = 1; i < constants_.Count(); i++) {
                hash ^= constants_[i].Value->GetHashCode();
            }

            return hash;
        }
    }

    bool operator== (const GlobalConstantsTag& other) const {
        return constants_ == other.constants_;
    }

    bool operator!= (const GlobalConstantsTag& other) const {
        return operator==(other) == false;
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::GlobalConstantsTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::GlobalConstantsTag::Id;
		}

		static Analysis::GlobalConstantsTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::GlobalConstantsTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif