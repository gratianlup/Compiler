// StructLayout.hpp
// Copyright (c) Lup Gratian
//
// Defines the class used to build the layout of a struct/union type.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_STRUCT_LAYOUT_HPP
#define PC_AST_STRUCT_LAYOUT_HPP

#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Heap.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Common/Context.hpp"
#include "Types.hpp"
#include "Declarations.hpp"
#include "Attributes.hpp"
#include "TypeSize.hpp"
#include "TypeAlignment.hpp"
#include <algorithm>
using namespace Base;
using namespace Common;

namespace AST {

// Wrapper that adds offset information to a field declaration.
class FieldInfo {
private:
	const FieldDeclaration* field_; // The associated field declaration.
	__int64 offset_;     // The offset from the beginning of the record (in bits).
	__int64 unitOffset_; // The offset from the beginning of the unit (in bits).
	__int64 size_;       // The size of the field (in bits).
	int  index_;         // The number of the unit in which the field is found.
						 // It's the same number for bitfields packed in the same unit.
public:
	FieldInfo() : field_(nullptr), offset_(0), size_(0), 
                  index_(0), unitOffset_(0) {}

	FieldInfo(const FieldDeclaration* field, __int64 offset = 0, __int64 size = 0, 
			  int index = 0, __int64 unitOffset = 0) :
			field_(field), offset_(offset), size_(size),
			index_(index), unitOffset_(unitOffset) {}

	FieldInfo(const FieldInfo& other) :
			field_(other.field_), offset_(other.offset_), size_(other.size_),
			index_(other.index_), unitOffset_(other.unitOffset_) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated field declaration.
	const FieldDeclaration* Field() const {
		return field_;
	}

	// Returns the offset from the start of the record, in bits.
	__int64 Offset() const {
		return offset_;
	}

	void SetOffset(__int64 value) {
		offset_ = value;
	}

	// Returns the offset from the start of the record, in bits.
	// Different from 0 only for bitfields.
	__int64 UnitOffset() const {
		return unitOffset_;
	}

	void SetUnitOffset(__int64 value) {
		unitOffset_ = value;
	}

	// Returns the offset from the start of the record, in bytes.
	__int64 ByteOffset() const {
		return offset_ / 8;
	}

	// Returns 'true' if the type is aligned at a byte offset.
	bool IsByteAlligned() const {
		return (offset_ % 8) == 0;
	}

	// Returns the size of the type, in bits.
	__int64 Size() const {
		return size_;
	}

	void SetSize(__int64 value) {
		size_ = value;
	}

	// Returns the size of the type, in bytes.
	__int64 ByteSize() const {
		return size_ / 8;
	}

	// Return 'true' if the size of the type is a byte multiple.
	bool IsByteSize() const {
		return (size_ % 8) == 0;
	}

	// Returns the position of the field in the struct/union.
	// Bitfields that are packed in the same unit have the same index number.
	int Index() const {
		return index_;
	}
};


// Computes the offsets for each field of the specified struct/union.
// Also computes the final size and alignment.
class StructLayout {
private:
	typedef Dictionary<const Identifier*, FieldInfo*, true> FieldMap;
	typedef List<shared<FieldInfo>> FieldList;

	const StructUnionType* type_; // The type for which the layout is computed.
	const Context* context_;      // Used to obtain alignment information.
	__int64 offset_;              // The current offset, in bits.
	__int64 unitOffset_;          // The offset relative to the start of the current unit.
	__int64 size_;                // The size of this struct/union.
	int remainingBits_;           // The number of bits that remained unused in the last byte.
	int alignment_;               // The alignment of this struct/union, as computed.
	__int64 prevSize_;            // The size of the previous field.
	bool isUnion_;                // 'true' if the type is an union.
	int index_;                   // The current field index.
	FieldList fields_;            // The list of fields with offset/size information.
	FieldMap fieldMap_;           // Mapping between a field and it's details.
	bool hasPack_;                // 'true' if the struct/union has the pack attribute.
	int packValue_;               // IsValid only if 'hasPackAttr_' is set.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Aligns the offset to the specified value.
	void EnsureAlignment(int value);

	// Updates the alignment of the whole struct/union so that the value is
	// the maximum alignment of all fields.
	void UpdateAlignment(int value);

	// Updates the size of the whole struct/union.
	void UpdateSize(__int64 value);

	// Computes the layout for the whole struct/union.
	void Layout();

	// Computes the layout for the specified field.
	void LayoutField(const FieldDeclaration* field);

	// Computes the layout for the specified bitfield.
	// Handles zero-sized (unnamed) bitfields too.
	void LayoutBitfield(const FieldDeclaration* field);

public:
	StructLayout(const StructUnionType* type, const Context* context);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated type.
	const StructUnionType* Type() const {
		return type_;
	}

	// Returns the alignment of the struct/union, in bytes.
	int Alignment() const {
		return alignment_ / 8;
	}

	// Returns the size of the struct/union in bytes.
	__int64 Size() const {
		return size_ / 8;
	}

	// Returns the list of fields with offset/size information.
	const FieldList& Fields() const {
		return fields_;
	}

	// Returns the offset/size information for the field with the specified name.
	FieldInfo& GetFieldInfo(const Identifier* name) const {
		DebugValidator::IsNotNull(name);
		DebugValidator::IsTrue(fieldMap_.ContainsKey(name));
		return *fieldMap_[name];
	}

	// Returns the offset/size information for the specified field.
	FieldInfo& GetFieldInfo(const FieldDeclaration* field) const {
		DebugValidator::IsNotNull(field);
		DebugValidator::IsTrue(fieldMap_.ContainsKey(field->Name()));
		return *fieldMap_[field->Name()];
	}

	// Creates a listing with information about all fields.
	string ToString() const;
};


// Acts as a cache for the most recent layouts of struct/union types.
class LayoutCache {
private:
	struct UsageInfo {
		int Usage;
		shared<StructLayout> Layout;

		//-------------------------------------------------------------------------------
		UsageInfo() : Usage(0) {}

		UsageInfo(shared<StructLayout> layout) :
				Layout(layout), Usage(1) {}

		UsageInfo(const UsageInfo& other) :
				Layout(other.Layout), Usage(other.Usage) {}

		UsageInfo& operator= (const UsageInfo& other) {
			if(&other != this) {
				Usage = other.Usage;
				Layout = other.Layout;
			}

			return *this;
		}

		bool operator== (const UsageInfo& other) {
			return Usage == other.Usage;
		}

		bool operator< (const UsageInfo& other) {
			return Usage >= other.Usage; // Make a min-heap.
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const int CACHE_SIZE = 2048;

	Heap<shared<UsageInfo>> heap_;
	Dictionary<const StructUnionType*, UsageInfo*> cache_;
	Context* context_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds a new layout to the cache.
	void AddNewLayout(shared<StructLayout> layout);

public:
	LayoutCache(Context* context) : context_(context) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds the specified layout to the cache. 'GetOrCreate' should be preferred.
	void Add(shared<StructLayout> layout);

	// Returns the layout information for the specified type.
	// If the layout is not in the cache it's created now.
	shared<StructLayout> GetOrCreate(const StructUnionType* type);

	// Removes all cached layouts.
	void Clear() {
		cache_.Clear();
		heap_.Clear();
	}

	// Returns the number of cached layouts.
	int Count() {
		return cache_.Count();
	}
};

} // namespace AST
#endif