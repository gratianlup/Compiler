// TypeClassAnnotator.hpp
// Copyright (c) Lup Gratian
//
// Annotate load/store operands with information about the language type.
// Used by Type Based Alias Analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_TYPE_CLASS_ANNOTATOR_HPP
#define PC_IR_GENERATOR_TYPE_CLASS_ANNOTATOR_HPP

#include "GeneratorEvents.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Tag.hpp"
#include "../IR/Tagged.hpp"
#include "../Analysis/TypeClassTag.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/SharedPointer.hpp"
using namespace Base;

namespace IRGenerator {

typedef Analysis::TypeClassTag TypeClassTag;

// Implements a table that holds and manages
// the type class tags for the entire application.
class TypeClassTable {
private:
	MAKE_PAIR_ORDERED_SECOND(TypeConstantPair, const AST::Type*, ClassType, bool, IsConstant);
    Dictionary<TypeConstantPair, TypeClassTag*> tags_;

public:
    ~TypeClassTable();

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    TypeClassTag* GetTypeClass(const AST::Type* type, bool isConstant = false,
                               TypeClassTag* parent = nullptr);

    TypeClassTag* GetUniversalClass(const AST::Type* type);
};


// Observer that annotates the loaded/stored pointers
// with information about the language type.
class TypeClassAnnotator : public GeneratorObserver {
private:
    shared<TypeClassTable> typeClasses_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns the alias class tag associated with the specified type.
    TypeClassTag* GetTypeClassTag(const AST::Type* languageType);

    // Annotates the operand with the appropriate alias class tag.
    void AnnotateWithTypeClass(IR::Operand* op, const AST::Type* languageType);

public:
    TypeClassAnnotator(shared<TypeClassTable> typeClasses) :
            typeClasses_(typeClasses) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual bool IsInterestedIn(GeneratorEvent genEvent) {
        return (genEvent == GeneratorEvent::Load) || 
               (genEvent == GeneratorEvent::Store);
    }

    virtual void AfterLoad(GeneratorContext& context, IR::LoadInstr* instr,
                           const AST::Type* sourceType) {
        // Annotate the source operand of the 'load'.
        if(sourceType) {
            AnnotateWithTypeClass(instr->SourceOp(), sourceType);
        }
    }

    virtual void AfterStore(GeneratorContext& context, IR::StoreInstr* instr,
                            const AST::Type* destType, const AST::Type* sourceType) {
        // Annotate the destination operand of the 'store'.
        if(destType) {
            AnnotateWithTypeClass(instr->DestinationOp(), destType);
        }
    }
};

} // namespace IRGenerator
#endif