// TypeClassAnotator.cpp
// Copyright (c) Lup Gratian
//
// Implements the TypeClassAnotator class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeClassAnnotator.hpp"

namespace IRGenerator {

TypeClassTable::~TypeClassTable() {
    // Remove all allocated tags.
    tags_.ForEachValue([](TypeClassTag* tag) -> bool {
        delete tag;
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TypeClassTag* TypeClassTable::GetTypeClass(const AST::Type* type, bool isConstant,
                                           TypeClassTag* parent) {
    // Check if the tag was already created.
	TypeConstantPair pair(type, isConstant);
    Analysis::TypeClassTag* tag;

    if(tags_.TryGetValue(pair, &tag)) {
        return tag;
    }

    tag = Analysis::TypeClassTag::GetTypeClass(isConstant, parent);
    tags_.Add(pair, tag);
    return tag;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TypeClassTag* TypeClassTable::GetUniversalClass(const AST::Type* type) {
    // Check if the tag was already created.
	TypeConstantPair pair(type, false /* isConstant */);
    Analysis::TypeClassTag* tag;

    if(tags_.TryGetValue(pair, &tag)) {
        return tag;
    }

    tag = Analysis::TypeClassTag::GetUniversalClass();
    tags_.Add(pair, tag);
    return tag;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
TypeClassTag* TypeClassAnnotator::GetTypeClassTag(const AST::Type* languageType) {
    // In C 'char' and 'unsigned char' can alias
    // any other type, so we use the universal tag for them.
	bool isConstant = false;

	if(auto qualType = languageType->As<AST::QType>()) {
		isConstant = qualType->HasConst();
		languageType = qualType->Base();
	}

    if(auto basicType = languageType->As<AST::BasicType>()) {
        if(basicType->IsChar() || basicType->IsUChar()) {
            return typeClasses_->GetUniversalClass(languageType);
        }
    }

	// Create the tag. If the type is marked 'const' create
	// another tag that marks this fact; the old tag becomes its child.
    auto typeClass = typeClasses_->GetTypeClass(languageType);

	if(isConstant) {
		typeClass = typeClasses_->GetTypeClass(languageType, true, typeClass);
	}

	return typeClass;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeClassAnnotator::AnnotateWithTypeClass(IR::Operand* op, 
                                               const AST::Type* languageType) {
    if(op->IsConstant() == false) {
        // If we have a variable reference annotate the variable directly, 
        // else the tag is lost during SSA conversion.
        IR::Tagged<IR::Tag>* taggedOp;

        if(auto variableRef = op->As<IR::VariableReference>()) {
            taggedOp = static_cast<IR::Tagged<IR::Tag>*>(variableRef->GetVariable());
        }
        else if(auto temp = op->As<IR::Temporary>()) {
            taggedOp = static_cast<IR::Tagged<IR::Tag>*>(temp);
        }
        else if(auto functionRef = op->As<FunctionReference>()) {
            taggedOp = static_cast<IR::Tagged<IR::Tag>*>(functionRef);
        }
        else DebugValidator::Unreachable();

        // Make sure we don't annotate twice.
        if(taggedOp->HasTag<TypeClassTag>() == false) {
            taggedOp->AddTag(GetTypeClassTag(languageType));
        }
    }
}

} // namespace IRGenerator