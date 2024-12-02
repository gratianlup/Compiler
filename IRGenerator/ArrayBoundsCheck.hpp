// ArrayBoundsCheck.hpp
// Copyright (c) Lup Gratian
//
// Emits array range check code.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_ARRAY_BOUNDS_CHECK_HPP
#define PC_IR_GENERATOR_ARRAY_BOUNDS_CHECK_HPP

#include "GeneratorEvents.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Operand.hpp"
#include "../IR/IRGenerator.hpp"
#include "../IR/OtherIntrinsics.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
using namespace Base;

namespace IRGenerator {

class ArrayBoundsCheck : public GeneratorObserver {
private:
    typedef StaticList<IR::Operand*, 8> TList;

    Dictionary<string*, IR::VariableReference*, true> names_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    IR::IndexInstr* GetIndex(IR::Operand* op);

    string* GetVariableName(IR::Operand* op);

    IR::VariableReference* GetOrInsertName(string* name, FunctionGenerator* functGen);

    void EmitIndexChecks(IR::IndexInstr* indexInstr, TList& testOps, 
                         IR::IRGenerator* irGen, FunctionGenerator* functGen);

    void EmitBoundsCheck(GeneratorContext& context, IR::Operand* op);

public:
    virtual bool IsInterestedIn(GeneratorEvent genEvent) override {
        return (genEvent == GeneratorEvent::Load) || 
               (genEvent == GeneratorEvent::Store);
    }

    virtual void BeforeLoad(GeneratorContext& context, IR::Operand* op,
                            const AST::Type* sourceType) override {
        EmitBoundsCheck(context, op);
    }

    virtual void BeforeStore(GeneratorContext& context, IR::Operand* op,
                             const AST::Type* destType, 
                             const AST::Type* sourceType) override {
        EmitBoundsCheck(context, op);
    }
};

} // namespace IRGenerator
#endif