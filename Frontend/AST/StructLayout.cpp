// StructLayout.cpp
// Copyright (c) Lup Gratian
//
// Implements the classes used to build the layout of struct/union.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StructLayout.hpp"

namespace AST {

StructLayout::StructLayout(const StructUnionType* type, const Context* context) : 
		type_(type), offset_(0), size_(0), alignment_(0),
		isUnion_(type->IsUnion()), context_(context), unitOffset_(0),
		prevSize_(0), remainingBits_(0), index_(-1), hasPack_(false),
		fields_(type->FieldCount()), fieldMap_(type->FieldCount()) {
	// Check if the declaration has a pack attribute.
	// If it does all members are aligned according to the pack value.
	if(auto packAttr = type->ParentDeclaration()->AttributeAs<PackAttribute>()) {
		hasPack_ = true;
		packValue_ = packAttr->Value();
	}

	// Compute the layout in the constructor.
	Layout();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StructLayout::EnsureAlignment(int value) {
	// This makes sure that the current offset is aligned to the specified value.
	if(value && (offset_ % value != 0)) {
		__int64 oldOffset = offset_;
		offset_ = (offset_ + (value - 1)) & ~(value - 1);
		size_ += offset_ - oldOffset;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StructLayout::UpdateAlignment(int value) {
	// The alignment of the whole record is the maximum between
	// the current one and the specified value (the new one).
	alignment_ = std::max(alignment_, value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StructLayout::UpdateSize(__int64 value) {
	// The size of a 'struct' is increased with the specified value,
	// while the size of an 'union' is the maximum between the previous size
	// and the specified value.
	if(isUnion_) {
		size_ = std::max(size_, value);
	}
	else {
		size_ += value;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StructLayout::Layout() {
	// Assign an offset value for each field. Bitfields are treated separately.
	auto& fields = type_->Fields();

	for(int i = 0; i < fields.Count(); i++) {
		const FieldDeclaration* field = fields[i];

		if(field->IsBitfield()) {
			LayoutBitfield(field);
		}
		else LayoutField(field);
	}

	// The size of the struct/union must be a multiple of it's alignment.
	int alignment = alignment_;

	// The size can be further changed if there is an alignment attribute
	// applied on the struct/union declaration (note that a 'pack' and 'align'
	// attribute can be applied at the same time, but don't affect each other).
	auto alignAttr = type_->ParentDeclaration()->AttributeAs<AlignmentAttribute>();

	if(alignAttr) {
		alignment = std::max(alignment, alignAttr->Value());
	}

	if(alignment && ((size_ % alignment) != 0)) {
		size_ = (size_ + alignment - 1) & ~(alignment - 1);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StructLayout::LayoutField(const FieldDeclaration* field) {
	// Set the offset so that it's a multiple of the alignment for the
	// specified type, or a multiple of the 'pack' value.
	int fieldAlign = TypeAlignment(field->DeclarationType(), context_).GetAlignment();

	// If the field is a flexible array it has no size, but must be properly aligned.
	__int64 fieldSize;

	if(field->DeclarationType()->IsArray() && 
       field->DeclarationType()->IsIncomplete()) {
		fieldSize = 0;
	}
	else fieldSize = TypeSize(field->DeclarationType(), context_).Size() * 8;

	// The alignment can be overridden by the 'pack' of the struct/union.
	if(hasPack_) fieldAlign = std::min(packValue_, fieldAlign);

	// If an alignment attribute is applied to the field it overrides 
	// all other alignment options (at least under VC).
	if(auto alignAttr = field->AttributeAs<AlignmentAttribute>()) {
		fieldAlign = alignAttr->Value();
	}

	// Ensure the field will be properly aligned and create the wrapper.
	fieldAlign *= 8;    // All values are expressed in bits.
	remainingBits_ = 0; // A future bitfield can't use bits from here.
	prevSize_ = 0;      // This is relevant only for bitfields.
	EnsureAlignment(fieldAlign);

	index_++;
	unitOffset_ = 0; // The offset of the field in the current unit.
	shared<FieldInfo> fieldInfo = new FieldInfo(field, offset_ , fieldSize, index_);
	fields_.Add(fieldInfo);
	fieldMap_.Add(field->Name(), fieldInfo);

	// The offset is changed from 0 only if it's not an 'union'.
	if(isUnion_ == false) {
		offset_ += fieldSize;
	}

	UpdateSize(fieldSize);
	UpdateAlignment(fieldAlign);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StructLayout::LayoutBitfield(const FieldDeclaration* field) {
	// If enough space remained from the previous bitfield this one
	// is placed in the same unit. Else the next unit is chosen.
	int size =  field->Bitfield();

	// Zero-sized bitfields have a special meaning: the next bitfield
	// should not be placed in the same unit as the previous one.
    // As an extension (to be compatible with GCC), we allow here any size,
    // as long as no identifier was provided for the field.
	if((size == 0) || (field->Name() == nullptr)) {
		remainingBits_ = 0;
		return;
	}

	// Get the size of the field, in bits.
	__int64 fieldSize = TypeSize(field->DeclarationType(), context_).Size() * 8;

	if((remainingBits_ < size) || (prevSize_ != fieldSize)) {
		// There is not enough space in the current unit, or the alignment
		// of the current bitfield is not the same as that of the previous one.
		int fieldAlign = TypeAlignment(field->DeclarationType(), context_).GetAlignment();

		// The alignment can be overridden by the 'pack' of the struct/union.
		if(hasPack_) {
			fieldAlign = std::min(packValue_, fieldAlign);
		}

		// If an alignment attribute is applied to the field it overrides 
		// all other alignment options (at least under VC - applies to bitfields too).
		if(auto alignAttr = field->AttributeAs<AlignmentAttribute>()) {
			fieldAlign = alignAttr->Value();
		}

		// Ensure the field will be properly aligned.
		fieldAlign *= 8;   // All values are expressed in bits.
		offset_ += remainingBits_;
		EnsureAlignment(fieldAlign);
		UpdateAlignment(fieldAlign);
		remainingBits_ = (int)fieldSize;
		unitOffset_ = 0;
		index_++;
	}

	// Place the bitfield in the current unit.
	shared<FieldInfo> fieldInfo = new FieldInfo(field, offset_ , size, 
                                                index_, unitOffset_);
	fields_.Add(fieldInfo);
	fieldMap_.Add(field->Name(), fieldInfo);
	remainingBits_ -= size;
	prevSize_ = fieldSize;

	// The offset is changed from 0 only if it's not an 'union'.
	if(isUnion_ == false) {
		offset_ += size;
		unitOffset_ += size;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string StructLayout::ToString() const {
	StringBuilder sb;

	sb.AppendLine("Struct layout: ");
	if(type_->ParentDeclaration()) {
		sb.AppendLine("\tName: " + type_->ParentDeclaration()->Name()->Name());
	}

	sb.AppendFormat(_T("\tFields: %d\n"), fields_.Count());

	for(int i = 0; i < fields_.Count(); i++) {
		auto& field = fields_[i];
		sb.AppendFormat(_T("\tField %d:\n"), i);
		if(field->Field()->Name()) {
			sb.AppendLine(_T("\t\tName: ") + field->Field()->Name()->Name());
		}

		sb.AppendFormat(_T("\t\tOffset: %d\n"), field->Offset());
		sb.AppendFormat(_T("\t\tSize: %d\n"), field->Size());
		sb.AppendLine();
	}

	return sb.ToString();
}

// ######################################################################################
// LayoutCache
// ######################################################################################
void LayoutCache::AddNewLayout(shared<StructLayout> layout) {
	UsageInfo* info = new UsageInfo(layout);
	heap_.Add(info);
	cache_.Add(layout->Type(), info);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LayoutCache::Add(shared<StructLayout> layout) {
	if((heap_.Count() + 1) == CACHE_SIZE) {
		// Something needs to be removed; remove the layout with the fewest uses.
		heap_.Extract();
	}

	AddNewLayout(layout);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<StructLayout> LayoutCache::GetOrCreate(const StructUnionType* type) {
	UsageInfo* info;

	if(cache_.TryGetValue(type, &info)) {
		info->Usage++;
		return info->Layout;
	}

	if((heap_.Count() + 1) == CACHE_SIZE) {
		// Something needs to be removed; remove the layout with the fewest uses.
		auto victim = heap_.Extract();
		cache_.Remove(victim->Layout->Type());
	}

	// If the layout is not in the cache create it now.
	shared<StructLayout> layout = new StructLayout(type, context_);
	AddNewLayout(layout);
	return layout;
}

} // namespace AST