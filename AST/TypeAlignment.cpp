// TypeAlignment.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'TypeAlignment' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeAlignment.hpp"
#include "StructLayout.hpp"

namespace AST {

int TypeAlignment::GetAlignment() {
	type_->Accept(this);
	return alignment_;
}

void TypeAlignment::Visit(const QType* type) {
	// Use the alignment of the type without qualifiers.
	type->Base()->Accept(this);
}

void TypeAlignment::Visit(const BasicType *type) {
	alignment_ = type->GetAlignment(context_->Target());
}

void TypeAlignment::Visit(const PointerType* type) {
	alignment_ = context_->Target()->GetPointerAlignment();
}

void TypeAlignment::Visit(const ArrayType* type) {
	// Use the alignment of the element type.
	type->ElementType()->Accept(this);
}

void TypeAlignment::Visit(const VarArrayType* type) {
	// Use the alignment of the element type.
	type->ElementType()->Accept(this);
}

void TypeAlignment::Visit(const FunctionType* type) {
	// The alignment of a function is irrelevant.
	alignment_ = 0;
}

void TypeAlignment::Visit(const EnumType* type) {
	// Use the alignment of the type used to store the constants.
	type->ConstType()->Accept(this);
}

void TypeAlignment::Visit(const StructType* type) {
	// Build the layout of the structure and use it's alignment.
	alignment_ = StructLayout(type, context_).Alignment();
}

void TypeAlignment::Visit(const UnionType* type) {
	// Build the layout of the union and use it's alignment.
	alignment_ = StructLayout(type, context_).Alignment();
}

void TypeAlignment::Visit(const TypedefType* type) {
	// Use the alignment of the inner type.
	type->Inner()->Accept(this);
}

} // namespace AST