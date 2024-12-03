// TypeSize.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'TypeSize' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeSize.hpp"
#include "StructLayout.hpp"

namespace AST {

__int64 TypeSize::Size() {
	type_->Accept(this);
	return size_;
}

void TypeSize::Visit(const QType* type) {
	// Use the size of the type without qualifiers.
	type->Base()->Accept(this);
}

void TypeSize::Visit(const BasicType *type) {
	size_ = type->Size(context_->Target());
}

void TypeSize::Visit(const PointerType* type) {
	size_ = context_->Target()->GetPointerSize();
}

void TypeSize::Visit(const ArrayType* type) {
	// If the array is incomplete we don't know it's size yet.
	if(type->IsIncomplete()) {
		size_ = 0;
	}
	else {
		// Multiply the number of elements with the size of one of them.
		type->ElementType()->Accept(this);
		size_ *= type->Size();
	}
}

void TypeSize::Visit(const VarArrayType* type) {
	// We don't know the size yet for VLA's.
	size_ = 0;
}

void TypeSize::Visit(const FunctionType* type) {
	// The size of functions is irrelevant.
	size_ = 0;
}

void TypeSize::Visit(const EnumType* type) {
	// Use the size of the type used to store the constants.
	type->ConstType()->Accept(this);
}

void TypeSize::Visit(const StructType* type) {
	// Build the layout of the structure and use it's size.
	size_ = StructLayout(type, context_).Size();
}

void TypeSize::Visit(const UnionType* type) {
	// Build the layout of the union and use it's size.
	size_ = StructLayout(type, context_).Size();
}

void TypeSize::Visit(const TypedefType* type) {
	// Use the size of the inner type.
	type->Inner()->Accept(this);
}

} // namespace AST