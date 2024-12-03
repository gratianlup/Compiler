// TypeClassTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to partition pointer operands
// into type classes based on the original language type.
// This is useful for Type Based Alias Analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_TYPE_CLASS_TAG_HPP
#define PC_ANALYSIS_TYPE_CLASS_TAG_HPP

#include "../IR/Tag.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class TypeClassTag : public Tag {
private:
    TypeClassTag* parent_; // The parent tag.
    bool isUniversal_;     // 'true' if the type can alias any other type.
	bool isConstant_;      // 'true' if the pointed location is not written.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	TypeClassTag(const TypeClassTag&);             // Should not be copied.
	TypeClassTag& operator= (const TypeClassTag&); // Should not be assigned.

    TypeClassTag(TypeClassTag* parent = nullptr, bool isConstant = false, 
				 bool isUniversal = false) :
            parent_(parent), isConstant_(isConstant), isUniversal_(isUniversal) {}

public:
    static const int Id = 0xceb8ae74;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

    virtual void Free() override {
        // The tags are freed by the tag table.
    }

    // Creates a new type class tag associated with the specified type,
    // optionally being linked to a type class parent.
    static TypeClassTag* GetTypeClass(bool isConstant = false, 
									  TypeClassTag* parent = nullptr) {
        return new TypeClassTag(parent, isConstant);
    }

    // Creates a new type class tag that represents a type
    // that can alias any other type in the language type system.
    // For C-like languages this would be 'char' and 'unsigned char'.
    static TypeClassTag* GetUniversalClass(bool isConstant = false) {
        return new TypeClassTag(nullptr, isConstant, true /* isUniversal */);
    }

    TypeClassTag* Parent() {
        return parent_;
    }

    TypeClassTag* Parent() const {
        return parent_;
    }

    bool HasParent() const {
        return parent_ != nullptr;
    }

    bool IsUniversalType() const {
        return isUniversal_;
    }

	bool IsConstant() const {
		return isConstant_;
	}

    unsigned GetHashCode() const {
        // Uses the FNV hash algorithm. 
		// Taken from 'http://www.eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx'
		unsigned hash = 2166136261;
		hash = (hash * 16777619) ^ (unsigned)parent_;

		if(sizeof(parent_) == 8) {
			hash = (hash * 16777619) ^ (unsigned)((__int64)parent_ >> 32);
		}

		hash = (hash * 16777619) ^ (unsigned)isUniversal_;
		hash = (hash * 16777619) ^ (unsigned)isConstant_;
		return hash;
    }

    bool operator== (const TypeClassTag& other) const {
        return (parent_ == other.parent_) &&
               (isUniversal_ == other.isUniversal_) &&
			   (isConstant_ == other.isConstant_);
    }

    bool operator!= (const TypeClassTag& other) const {
        return operator==(other) == false;
    }

    bool operator< (const TypeClassTag& other) const {
        return false; // Required by Dictionary.
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::TypeClassTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::TypeClassTag::Id;
		}

		static Analysis::TypeClassTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::TypeClassTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif