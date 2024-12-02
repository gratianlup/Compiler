// ParameterAliasTag.hpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_PARAMETER_ALIAS_TAG_HPP
#define PC_ANALYSIS_PARAMETER_ALIAS_TAG_HPP

#include "IntArithmetic.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/ObjectHash.hpp"
using namespace IR;
using namespace Base;

//! flag to mark parameters that point only to local memory
//! can be used to solve alias for (global, parameter) pairs!

namespace Analysis {

// Represents a section formed of a single element,
// which can be a constant or a parameter with an optionally
// applied operation (+, -, *, and /) and an adjustment.
// General form: a OP b + c, where OP is any of +, -, *, /.
// Some examples: p[2], p[a], p[a + 2], p[a * b], p[(a * 4) + 1].
class ElementSection {
private:
    union {
        const IntConstant* constantBase_;
        int parameterBase_;
    };

    union {
        const IntConstant* constantFactor_;
        int parameterFactor_;
    };

    const IntConstant* adjustment_;
    Opcode operation_;
    bool hasConstantBase_;
    bool hasConstantFactor_;
    bool isOperation_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    const IntConstant* Add(const IntConstant* a, const IntConstant* b, Unit* unit);
    const IntConstant* Subtract(const IntConstant* a, const IntConstant* b, Unit* unit);

public:
    ElementSection() : 
            constantBase_(nullptr), constantFactor_(nullptr),
            hasConstantBase_(false), hasConstantFactor_(false), 
            isOperation_(false), adjustment_(nullptr) {}

	// Constructor for a section of the form p[2].
    explicit ElementSection(const IntConstant* baseConst) :
            constantBase_(baseConst), constantFactor_(nullptr),
            hasConstantBase_(true), hasConstantFactor_(false), 
            isOperation_(false), adjustment_(nullptr) {}

	// Constructor for a section of the form p[a], where 'a' is a parameter.
    explicit ElementSection(int baseParamIndex) :
            parameterBase_(baseParamIndex), constantFactor_(nullptr),
            hasConstantBase_(false), hasConstantFactor_(false),
            isOperation_(false), adjustment_(nullptr) {}

	// Constructor for a section of the form p[a * 5 + 3], where 'a' is a parameter.
    explicit ElementSection(int baseParamIndex, Opcode operation, 
                            const IntConstant* factorConst, 
                            const IntConstant* adjustmentConst = nullptr) :
            parameterBase_(baseParamIndex), constantFactor_(factorConst),
            operation_(operation), hasConstantBase_(false), isOperation_(true),
            hasConstantFactor_(true), adjustment_(adjustmentConst) {}

	// Constructor for a section of the form p[a * b + 3], 
	// where 'a' and 'b' are parameters.
    explicit ElementSection(int baseParamIndex, Opcode operation, int factorParamIndex, 
                            const IntConstant* adjustmentConst = nullptr) :
            parameterBase_(baseParamIndex), parameterFactor_(factorParamIndex),
            operation_(operation), hasConstantBase_(false), isOperation_(true),
            hasConstantFactor_(false), adjustment_(adjustmentConst) {}

	// Constructor for a section of the form p[5 * a + 3], where 'a' is a parameter.
    explicit ElementSection(const IntConstant* baseConst, 
                            Opcode operation, int factorParamIndex, 
                            const IntConstant* adjustmentConst = nullptr) :
            constantBase_(baseConst), parameterFactor_(factorParamIndex),
            operation_(operation), hasConstantBase_(true), isOperation_(true),
            hasConstantFactor_(false), adjustment_(adjustmentConst) {
        // Canonicalize 'const + param' to 'param + const' if possible.
        // This reduces the number of tests that need to be done.
        if(Instruction::IsCommutative(operation)) {
            auto constant = baseConst;
            int parameter = factorParamIndex;

            constantFactor_ = constant;
            parameterBase_ = parameter;
            hasConstantBase_ = false;
            hasConstantFactor_ = true;
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    static ElementSection MakeElementSection(Operand* baseOp, Opcode operation,
                                             Operand* factorOp);
     
    // Returns 'true' if the base operand is a constant.
    bool HasConstantBase() const {
        return hasConstantBase_;
    }

    // Returns the constant base operand, 
    // or 'nullptr' if it isn't a constant.
    const IntConstant* BaseAsConstant() const {
        if(hasConstantBase_) {
            return constantBase_;
        }
        else return nullptr;
    }

    // Returns the base operand as the index of the represented parameter.
    int BaseAsParameter() const {
        DebugValidator::IsFalse(hasConstantBase_);
        return parameterBase_;
    }

    // Returns 'true' if the element represents an operation
    // applied on the base operand (i.e has a factor).
    bool IsOperation() const {
        return isOperation_;
    }

    bool IsNotOperation() const {
        return IsOperation() == false;
    }

    // Returns 'true' if the operation factor is a constant.
    bool HasConstantFactor() const {
        return IsOperation() && hasConstantFactor_;
    }

    // Returns the constant operation factor, 
    // or 'nullptr' if it isn't a constant.
    const IntConstant* FactorAsConstant() const {
        DebugValidator::IsTrue(IsOperation());

        if(hasConstantFactor_) {
            return constantFactor_;
        }
        else return nullptr;
    }

    // Returns the factor operand as the index of the represented parameter.
    int FactorAsParameter() const {
        DebugValidator::IsTrue(IsOperation());
        DebugValidator::IsFalse(hasConstantFactor_);
        return parameterFactor_;
    }

    // Returns the type of operation applied in the base operand.
    Opcode Operation() const {
        return operation_;
    }

    // Returns 'true' if the applied operation is addition with a factor.
    bool IsAddition() const {
        return IsOperation() && (operation_ == Opcode::Add);
    }

    // Returns 'true' if the applied operation is subtraction with a factor.
    bool IsSubtraction() const {
        return IsOperation() && (operation_ == Opcode::Sub);
    }

    // Returns 'true' if the applied operation is multiplication by a factor.
    bool IsMultiplication() const {
        return IsOperation() && (operation_ == Opcode::Mul);
    }

    // Returns 'true' if the applied operation is division by a factor.
    bool IsDivision() const {
        return IsOperation() && (operation_ == Opcode::Div);
    }

    // Returns 'true' if the element is a constant
    // (constant base operand and no operation applied).
    bool IsConstant() const {
        return HasConstantBase() && IsNotOperation() && 
               (HasAdjustment() == false);
    }

    // Returns 'true' if the element is a parameter
    // (parameter base operand and no operation applied).
    bool IsParameter() const {
        return (HasConstantBase() == false) && IsNotOperation() &&
               (HasAdjustment() == false);
    }

    // Returns the constant the element represents, or 'nullptr' if it isn't
    // a constant (it is a parameter with or without an applied operation).
    const IntConstant* AsConstant() const {
        if(IsConstant()) {
            return BaseAsConstant();
        }
        else return nullptr;
    }

    // Returns 'true' if the element is the zero constant.
    bool IsZero() const {
        if(auto constant = AsConstant()) {
            return constant->IsZero();
        }
        else return false;
    }

    // Returns 'true' if there is any additive adjustment.
    bool HasAdjustment() const {
        return adjustment_ != nullptr;
    }

    // Returns the additive adjustment, or 'nullptr' if there is none.
    const IntConstant* GetAdjustment() const {
        return adjustment_;
    }

    // Tries to add the other element section to this one,
    // returning 'true' if it could be done.
    bool Add(ElementSection& other, ElementSection& result, Unit* unit);

    // Tries to subtract the other element section from this one,
    // returning 'true' if it could be done.
    bool Subtract(ElementSection& other, ElementSection& result, Unit* unit);

    // Tries to evaluate the element section by replacing
    // the parameters with the actual arguments.
    ElementSection ReplaceParameters(CallInstr* instr);

    unsigned GetHashCode() const {
        ObjectHash hash;
        hash.Add(hasConstantBase_).Add(hasConstantFactor_);

        if(hasConstantBase_) {
            hash.AddObject(constantBase_);
        }
        else hash.Add(parameterBase_);

        if(isOperation_) {
            hash.Add(operation_);

            if(hasConstantFactor_) {
                hash.AddObject(constantFactor_);
            }
            else hash.Add(parameterFactor_);
        }

        return hash.GetHashCode();
    }

    bool operator== (const ElementSection& other) const {
        if(&other == this) {
            return true;
        }

        if((hasConstantBase_ != other.hasConstantBase_) ||
           (hasConstantFactor_ != other.hasConstantFactor_)) {
            return false;
        }

        if(hasConstantBase_) {
            if(constantBase_ != other.constantBase_) {
                return false;
            }
        }
        else if(parameterBase_ != other.parameterBase_) {
            return false;
        }

        if(hasConstantFactor_) {
            if(constantFactor_ != other.constantFactor_) {
                return false;
            }
        }
        else if(parameterFactor_ != other.parameterFactor_) {
            return false;
        }

        if(isOperation_ && (operation_ != other.operation_)) {
            return false;
        }

        return true;
    }

    bool operator!= (const ElementSection& other) const {
        return operator== (other) == false;
    }

    bool operator< (const ElementSection& other) const {
        return false;
    }
};


// Represents a section formed by a series of consecutive
// elements between the lower and upper bound, optionally
// having a known step which is an integer constant. Some examples:
// p[0 => 4], p[0 => n], p[a => b], p[a + 1 => b * 2]
class RangeSection {
private:
    ElementSection low_;
    ElementSection high_;
    const IntConstant* step_;

public:
    RangeSection() {}

    RangeSection(ElementSection low, ElementSection high, 
                 const IntConstant* step = nullptr) :
            low_(low), high_(high), step_(step) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the element that represents the low bound.
    ElementSection& LowBound() {
        return low_;
    }

    // Returns the element that represents the high bound.
    ElementSection& HighBound() {
        return high_;
    }

    // Returns 'true' if both bounds are constants.
    bool HasConstantBounds() const {
        return low_.IsConstant() && high_.IsConstant();
    }

    // Returns 'true' if both bounds are parameters
    // without any applied operation on them.
    bool HasParameterBounds() const {
        return low_.IsParameter() && high_.IsParameter();
    }

    // Returns 'true' if the higher bound is a parameter
    // without any operation applied and the lower one is a constant.
    bool HasParameterHighBound() const {
        return low_.IsConstant() && high_.IsParameter();
    }

    // Returns 'true' if the step of the accessed elements
    // is known and it is an integer constant.
    bool HasKnownStep() const {
        return step_ != nullptr;
    }

    // Returns the known step.
    const IntConstant* Step() const {
        return step_;
    }

    unsigned GetHashCode() const {
        return low_.GetHashCode() ^ high_.GetHashCode();
    }

    bool operator== (const RangeSection& other) const {
        if(&other == this) {
            return true;
        }

        return (low_ == other.low_) && (high_ == other.high_) &&
               (step_ == other.step_);
    }

    bool operator!= (const RangeSection& other) const {
        return operator== (other) == false;
    }

    bool operator< (const RangeSection& other) const {
        return false;
    }
};


// Represents the condition for a 'must' section certainty to hold.
// p > 0,  p1 == p2,  p1 == p2 + 4
class SectionCertainty {
private:
    int parameterIndex_;     // The compared parameter.
    ElementSection other_;   // The element the parameter is compared to.
    OrderType compareOrder_; // The used comparison order.
    bool isUnsiged_;         // 'true' if it is an unsigned comparison.

public:
    SectionCertainty() {}

    SectionCertainty(const SectionCertainty& other) :
            parameterIndex_(other.parameterIndex_), other_(other.other_),
            compareOrder_(other.compareOrder_), isUnsiged_(other.isUnsiged_) {}

    SectionCertainty(int paramIndex, OrderType order, 
                     bool isUnsiged, ElementSection other) :
            parameterIndex_(paramIndex), compareOrder_(order), 
            isUnsiged_(isUnsiged), other_(other) {}

    explicit SectionCertainty(int paramIndex, OrderType order, 
                              bool isUnsiged, const IntConstant* other) :
            parameterIndex_(paramIndex), compareOrder_(order),
            isUnsiged_(isUnsiged), other_(ElementSection(other)) {}

    explicit SectionCertainty(int paramIndex, OrderType order, 
                              bool isUnsiged, int otherParam) :
            parameterIndex_(paramIndex), compareOrder_(order),
            isUnsiged_(isUnsiged), other_(ElementSection(otherParam)) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the parameter that is compared.
    int ComparedParameter() const {
        return parameterIndex_;
    }

    // Returns the comparison order.
    OrderType CompareOrder() const {
        return compareOrder_;
    }

    // Returns the element the parameter is compared to.
    const ElementSection& ComparedToElement() const {
        return other_;
    }

    // Methods for determining the signedness of the comparison.
    bool IsUnsignedComparison() const {
        return isUnsiged_;
    }

    bool IsSignedComparison() const {
        return isUnsiged_ == false;
    }

    // Tries to evaluate the certainty condition by replacing
    // the parameters with the actual arguments and doing the comparison.
    bool Evaluate(CallInstr* callInstr, bool& result);

    bool operator== (const SectionCertainty& other) const {
        if(&other == this) {
            return true;
        }

        return (parameterIndex_ == other.parameterIndex_) &&
               (compareOrder_ == other.compareOrder_) &&
               (other_ == other.other_) &&
               (isUnsiged_ == other.isUnsiged_);
    }

    bool operator!= (const SectionCertainty& other) const {
        return operator== (other) == false;
    }
};


// Describes the actions performed on a section (read/write)
// and if they may/definitely are performed.
class SectionProperties {
private:
    SectionCertainty* conditionedCertainty_; // Only if 'isMay_ == true'.
    unsigned char isMay_   : 1; // 1 - may, 0 - must
    unsigned char isRead_  : 1; // 0 - definitely no read
    unsigned char isWrite_ : 1; // 0 - definitely no write
    unsigned char isByte_  : 1; // 1 - the positions are expressed in bytes

public:
    SectionProperties() : conditionedCertainty_(nullptr) {}

    SectionProperties(const SectionProperties& other) :
            conditionedCertainty_(nullptr), isMay_(other.isMay_),
            isRead_(other.isRead_), isWrite_(other.isWrite_), isByte_(other.isByte_) {
        if(other.conditionedCertainty_) {
            conditionedCertainty_ = new SectionCertainty(*other.conditionedCertainty_);
        }
    }

    SectionProperties(SectionProperties&& other) : // Move constructor.
            conditionedCertainty_(other.conditionedCertainty_), isMay_(other.isMay_),
            isRead_(other.isRead_), isWrite_(other.isWrite_), isByte_(other.isByte_) {
        other.conditionedCertainty_ = nullptr;
    }

    explicit SectionProperties(bool isRead, bool isWrite = false, 
                               bool isMay = true, bool isByte = false) : 
            isRead_(isRead), isWrite_(isWrite), isMay_(isMay), isByte_(isByte),
            conditionedCertainty_(nullptr) {}

    explicit SectionProperties(bool isRead, bool isWrite, 
                               SectionCertainty* certainty = nullptr, 
                               bool isByte = false) : 
            isRead_(isRead), isWrite_(isWrite), isMay_(true), isByte_(isByte),
            conditionedCertainty_(certainty) {}

    ~SectionProperties() {
        if(conditionedCertainty_) {
            delete conditionedCertainty_;
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns 'true' if the section may be read/written.
    bool IsMay() const {
        return isMay_;
    }

    // Returns 'true' if the section is definitely read/written.
    bool IsMust() const {
        return isMay_ == 0;
    }

    bool IsRead() const {
        return isRead_;
    }

    void SetIsRead(bool value) {
        isRead_ = value;
    }

    bool IsWritten() const {
        return isWrite_;
    }

    void SetIsWritten(bool value) {
        isWrite_ = value;
    }

    bool IsReadOrWritten() const {
        return isRead_ || isWrite_;
    }

    // Returns 'true' if the section is definitely read/written
    // only if a certain condition involving the parameters is true.
    bool IsConditioned() const {
        return isMay_ && conditionedCertainty_;
    }

    // Returns the condition that needs to be true
    // for the section to be definitely read/written.
    SectionCertainty* GetCondition() {
        return conditionedCertainty_;
    }

    // Returns 'true' if the positions of the sections
    // are expressed in bytes, not in array/record elements.
    bool HasBytePositions() const {
        return isByte_;
    }

    // Returns 'true' if the positions of the sections
    // are expressed in in array/record elements (not in bytes).
    bool HasElementPositions() const {
        return isByte_ == 0;
    }

    // Tries to merge the properties with the other properties.
    bool Merge(const SectionProperties& other, SectionProperties& result);

    bool operator== (const SectionProperties& other) const {
        if(&other == this) {
            return true;
        }

        if((isMay_ != other.isMay_)   ||
           (isRead_ != other.isRead_) ||
           (isWrite_ != other.isWrite_)) {
            return false;
        }

        if(conditionedCertainty_ && other.conditionedCertainty_) {
            return *conditionedCertainty_ == *other.conditionedCertainty_;
        }
        
        return conditionedCertainty_ && other.conditionedCertainty_;
    }

    bool operator!= (const SectionProperties& other) const {
        return operator== (other) == false;
    }

    SectionProperties& operator= (const SectionProperties& other) {
        if(&other != this) {
            if(conditionedCertainty_) {
                if(other.conditionedCertainty_) {
                    *conditionedCertainty_ = *other.conditionedCertainty_;
                }
                else {
                    delete conditionedCertainty_;
                    conditionedCertainty_ = nullptr;
                }
            }
            else if(other.conditionedCertainty_) {
                conditionedCertainty_ = new SectionCertainty(*other.conditionedCertainty_);
            }

            isMay_ = other.isMay_;
            isRead_ = other.isRead_;
            isWrite_ = other.isWrite_;
            isByte_ = other.isByte_;
        }

        return *this;
    }
};


// Defines two pairs of section/properties objects.
MAKE_PAIR(ElementSectionPropertiesPair, ElementSection, Section, SectionProperties, Properties);
MAKE_PAIR(RangeSectionPropertiesPair, RangeSection, Section, SectionProperties, Properties);


// Represents the sections associated with a parameter.
class ParameterSections {
private:
    typedef StaticList<ElementSectionPropertiesPair, 2> SectionList;
    typedef StaticList<RangeSectionPropertiesPair, 2> RangeSectionList;

    SectionList sections_;
    RangeSectionList rangeSections_;
    bool hasUnknownSections_;

public:
    ParameterSections() : hasUnknownSections_(false) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    SectionList& ElementSections() {
        return sections_;
    }

    RangeSectionList& RangeSections() {
        return rangeSections_;
    }

    //
    ElementSectionPropertiesPair* FindElementSection(const ElementSection& value) {
        for(int i = 0; i < sections_.Count(); i++) {
            if(sections_[i].Section == value) {
                return &sections_[i];
            }
        }

        return nullptr;
    }

    // 
    RangeSectionPropertiesPair* FindRangeSection(const RangeSection& value) {
        for(int i = 0; i < rangeSections_.Count(); i++) {
            if(rangeSections_[i].Section == value) {
                return &rangeSections_[i];
            }
        }

        return nullptr;
    }

    bool HasUnknownSections() const {
        return hasUnknownSections_;
    }

    void MarkHasUnknownSections() {
        hasUnknownSections_ = true;
        sections_.Clear();
        rangeSections_.Clear();
    }
};

//
class ParameterAliasTag : public Tag {
private:
    StaticList<unsigned char, 2> aliasedParams_;
    ParameterSections sections_;
    bool aliasesAll_;  // 'true' if it aliases all other parameters.
    bool aliasesNone_; // 'true' if it definitely doesn't alias any other parameter.
	bool isNotGlobal_; // 'true' if the parameter cannot point to a global variable.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    ParameterAliasTag() : 
            aliasesAll_(false), aliasesNone_(false), isNotGlobal_(true) {}

    ParameterAliasTag(bool aliasesAll, bool aliasesNone) : 
            aliasesAll_(aliasesAll), aliasesNone_(aliasesNone), isNotGlobal_(true) {}

public:
    static const int Id = 0x535e3af4;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

    // Creates a new parameter alias tag.
    static ParameterAliasTag* GetParameterAlias() {
        return new ParameterAliasTag();
    }

    // Creates a new parameter alias tag that indicates that
    // none of the other parameters alias the associated one.
    static ParameterAliasTag* GetNoneParameterAlias() {
        return new ParameterAliasTag(false /* aliasesAll */, true /* aliasesNone */);
    }

    // Creates a new parameter alias tag that indicates that
    // all of the other parameters alias the associated one.
    static ParameterAliasTag* GetAllParameterAlias() {
        return new ParameterAliasTag(true /* aliasesAll */, false /* aliasesNone */);
    }

    // Returns 'true' if all other parameters alias the associated one.
    bool AliasesAllParameters() const {
        return aliasesAll_;
    }

    void SetAliasesAllParameters(bool value) {
        aliasesAll_ = value;

        if(value) {
            aliasesNone_ = false;
            aliasedParams_.Clear();
        }
    }

    // Returns 'true' if none of the other parameters alias the associated one.
    bool DoesNotAliasParameters() const {
        return aliasesNone_;
    }

    void SetDoesNotAliasParameters(bool value) {
        aliasesNone_ = value;

        if(value) {
            aliasesAll_ = false;
            aliasedParams_.Clear();
        }
    }

    // Returns 'true' if the aliased parameters are not known exactly.
    bool AreParameterAliasesUnknown() const {
        return (aliasesAll_ == false) &&
               (aliasesNone_ == false) &&
                aliasedParams_.IsEmpty();
    }

    // Returns 'true' if this parameter aliases any other one.
    bool AliasesParameters() const {
        return aliasesAll_ || aliasedParams_.IsNotEmpty();
    }

    // Returns the number of parameters that alias the associated one.
    int AliasedParameterCount() const {
        return aliasedParams_.Count();
    }

    // Returns 'true' if this parameter may alias the specified one.
    bool AliasesParameter(int parameterIndex) const {
        return aliasedParams_.Contains(parameterIndex);
    }

    // Returns the aliased parameter found at the specified position.
    int GetAliasedParameter(int index) const {
        return aliasedParams_[index];
    }

    // Adds a new aliased parameter if it isn't already in the list.
    void AddAliasedParameter(int parameterIndex) {
        if(aliasedParams_.Contains(parameterIndex) == false) {
            aliasedParams_.Add(parameterIndex);
        }
    }

    // Removes the specified parameter from the list of aliased ones.
    void RemoveAliasedParameter(int parameterIndex) {
        DebugValidator::IsTrue(aliasedParams_.Contains(parameterIndex));
        aliasedParams_.Remove(parameterIndex);
    }

	// Returns 'true' if the parameter may point to a global variable
	// in any of the possible instances.
	bool MayPointToGlobalVariables() const {
		return isNotGlobal_ == false;
	}

	void SetMayPointToGlobalVariables(bool value) {
		isNotGlobal_ = !value;
	}

    // Returns the sections associated with the parameter.
    ParameterSections& Sections() {
        return sections_;
    }

    const ParameterSections& Sections() const {
        return sections_;
    }

    // Associates the specified element section with the parameter.
    void AddElementSection(ElementSection section, SectionProperties properties);

    // Associates the specified range section with the parameter.
    void AddRangeSection(RangeSection section, SectionProperties properties);

    // Performs the specified action on each element section.
    // bool Predicate(const ElementSection& section);
    template <class Predicate>
    void ForEachElementSection(Predicate action) const {
        auto& elementSections = sections_.ElementSections();

        for(int i = 0; i < elementSections.Count(); i++) {
            if(action(elementSections[i]) == false) {
                return;
            }
        }
    }

    // Performs the specified action on each element section.
    // bool Predicate(const RangeSection& section);
    template <class Predicate>
    void ForEachRangeSection(Predicate action) const {
        auto& rangeSections = sections_.RangeSections();

        for(int i = 0; i < rangeSections.Count(); i++) {
            if(action(rangeSections[i]) == false) {
                return;
            }
        }
    }

    // Performs the specified action on each aliased parameter.
    // bool Predicate(int parameterIndex);
    template <class Predicate>
    void ForEachAliasedParameter(Predicate action) const {
        for(int i = 0; i < aliasedParams_.Count(); i++) {
            if(action(aliasedParams_[i]) == false) {
                return;
            }
        }
    }
};

} // namespace Analysis

namespace IR {
    namespace Detail {
        // Implements support for "dynamic cast".
        template <>
        struct TagPromoter<Analysis::ParameterAliasTag> {
            static bool Is(const Tag* tag) {
                return tag->GetId() == Analysis::ParameterAliasTag::Id;
            }

            static Analysis::ParameterAliasTag* As(Tag* tag) {
                return Is(tag) ? static_cast<Analysis::ParameterAliasTag*>(tag) : nullptr;
            }
        };
    } // namespace Detail
} // namespace IR
#endif