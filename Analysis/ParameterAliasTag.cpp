// ParameterAliasTag.cpp
// Copyright (c) Lup Gratian
//
// Implements the ParameterAliasTag tag.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ParameterAliasTag.hpp"

namespace Analysis {

// a + 2 (+) a + 3 -> a*2 + 5
// a - 2 (+) a + 3 -> a*2 + 1
// a + b (+) 2 -> a + b + 2
// a*2 + 1 (+) a*4 + 2 -> a*6 + 3
// a + 5 (+) b + 2 -> a + b + 7
//? a + b + C1 (+) a - b + C2 -> a*2 + (C1 + C2)

bool ElementSection::Add(ElementSection& other, ElementSection& result, Unit* unit) {
    // Adding two element sections is not always possible,
    // we handle only the following five cases:
    // C1 (+) C2 -> C1 + C2
    // a (+) b -> a + b
    if(IsConstant() && other.IsConstant()) {
        result = ElementSection(Add(BaseAsConstant(), other.BaseAsConstant(), unit));
        return true;
    }
    else if(IsParameter() && other.IsParameter()) {
        result = ElementSection(BaseAsParameter(), Opcode::Add, 
                                other.BaseAsParameter());
        return true;
    }
    
    // If we have two operations we don't continue.
    if(IsConstant() || other.IsConstant()) {
        // Canonicalize by moving the constant on the right.
        auto& constSection = IsConstant() ? *this : other;
        auto& nonConstSection = IsConstant() ? other : *this;

        // a (+) C -> a + C
        if(nonConstSection.IsParameter()) {
            result = ElementSection(BaseAsParameter(), Opcode::Add, 
                                    other.BaseAsConstant());
            return true;
        }
        // a + C1 (+) C2 -> a + (C1 + C2)
        else if(nonConstSection.IsAddition() && nonConstSection.HasConstantFactor()) {
            auto constant = Add(FactorAsConstant(), other.BaseAsConstant(), unit);
            result = ElementSection(BaseAsParameter(), Opcode::Add, constant);
            return true;
        }
        // a - C1 (+) C2 -> a - (C1 - C2)
        else if(nonConstSection.IsSubtraction() && nonConstSection.HasConstantFactor()) {
            auto constant = Subtract(FactorAsConstant(), other.BaseAsConstant(), unit);
            result = ElementSection(BaseAsParameter(), Opcode::Sub, constant);
            return true;
        }
    }

    return false;
}

// a + b (-) 3 -> a + b + -3
// a - b + 5 (-) 3 -> a - b + 2
// a + 5 (-) b + 3 -> a - b + 2
// a*3 + 5 (-) a + 3 -> a*2 + 2
//? a OP b + C1 (-) a OP b + C2 -> C1 - C2

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ElementSection::Subtract(ElementSection& other, ElementSection& result, Unit* unit) {
    // Subtracting two element sections is not always possible,
    // we handle only the following six cases:
    // C1 (-) C2 -> C1 - C2
    // a (-) b -> a - b
    if(IsConstant() && other.IsConstant()) {
        result = ElementSection(Subtract(BaseAsConstant(), other.BaseAsConstant(), unit));
        return true;
    }
    else if(IsParameter() && other.IsParameter()) {
        result = ElementSection(BaseAsParameter(), Opcode::Sub, 
                                other.BaseAsParameter());
        return true;
    }

    // If we have two operations we don't continue.
    if(other.IsConstant()) {
        // a (-) C -> a - C
        if(IsParameter()) {
            result = ElementSection(BaseAsParameter(), Opcode::Sub, 
                                    other.BaseAsConstant());
            return true;
        }
        // a + C1 (-) C2 -> a + (C1 - C2)
        else if(IsAddition() && HasConstantFactor()) {
            auto constant = Subtract(FactorAsConstant(), other.BaseAsConstant(), unit);
            result = ElementSection(BaseAsParameter(), Opcode::Add, constant);
            return true;
        }
        // a - C1 (-) C2 -> a - (C1 + C2)
        else if(IsSubtraction() && HasConstantFactor()) {
            auto constant = Add(FactorAsConstant(), other.BaseAsConstant(), unit);
            result = ElementSection(BaseAsParameter(), Opcode::Sub, constant);
            return true;
        }
        // C1 - a (-) C2 -> (C1 - C2) - a
        else if(IsSubtraction() && HasConstantBase()) {
            auto constant = Subtract(BaseAsConstant(), other.BaseAsConstant(), unit);
            result = ElementSection(constant, Opcode::Sub, FactorAsParameter());
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ElementSection ElementSection::ReplaceParameters(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);
    return ElementSection(0);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SectionCertainty::Evaluate(CallInstr* callInstr, bool& result) {
    DebugValidator::IsNotNull(callInstr);


    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SectionProperties::Merge(const SectionProperties& other, 
                              SectionProperties& result) {
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterAliasTag::AddElementSection(ElementSection section, 
                                          SectionProperties properties) {
    // Check if this element is already added. If it is we don't add it again,
    // but try to update the properties (read + write -> read-write, for example).
    if(auto previousSection = sections_.FindElementSection(section)) {
        SectionProperties mergedProperties;

        if(previousSection->Properties.Merge(properties, mergedProperties)) {
            previousSection->Properties = mergedProperties;
        }
        else sections_.MarkHasUnknownSections();
    }
    else {
        // The first time the element is added.
        auto pair = ElementSectionPropertiesPair(section, properties);
        sections_.ElementSections().Add(pair);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterAliasTag::AddRangeSection(RangeSection section, 
                                        SectionProperties properties) {
    // Check if this range is already added. If it is we don't add it again,
    // but try to update the properties (read + write -> read-write, for example).
    if(auto previousSection = sections_.FindRangeSection(section)) {
        SectionProperties mergedProperties;

        if(previousSection->Properties.Merge(properties, mergedProperties)) {
            previousSection->Properties = mergedProperties;
        }
        else sections_.MarkHasUnknownSections();
    }
    else {
        // The first time the element is added.
        auto pair = RangeSectionPropertiesPair(section, properties);
        sections_.RangeSections().Add(pair);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IntConstant* ElementSection::Add(const IntConstant* a, 
                                       const IntConstant* b, Unit* unit) {
    return unit->Constants().GetInt(a->GetType(), IntArithmetic::Add(a, b));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IntConstant* ElementSection::Subtract(const IntConstant* a, 
                                            const IntConstant* b, Unit* unit) {
    return unit->Constants().GetInt(a->GetType(), IntArithmetic::Sub(a, b));
}

} // namespace Analysis