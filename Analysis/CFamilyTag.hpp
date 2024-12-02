// CFamilyTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to associate with objects
// information specific to the C-family languages.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_C_FAMILY_TAG_HPP
#define PC_ANALYSIS_C_FAMILY_TAG_HPP

#include "../IR/Tag.hpp"
#include "../IR/Tagged.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class CFamilyTag : public FunctionTag {
private:
    enum class Flags {
        None               = 0,
        IsAllocLike        = 1 << 0,
        IsCheckedAllocLike = 1 << 1,
        HasSetjmp          = 1 << 2,
        HasLongjmp         = 1 << 3,
        HasAlloca          = 1 << 4
    };

    Flags flags_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    CFamilyTag();                              // Should not be created.
	CFamilyTag(const CFamilyTag&);             // Should not be copied.
	CFamilyTag& operator= (const CFamilyTag&); // Should not be assigned.

    CFamilyTag(Flags flags) : flags_(flags) {}

    bool HasFlag(Flags testFlag) const {
        return ((int)flags_ & (int)testFlag) != 0;
    }

    void SetFlag(Flags flag, bool state) {
        if(state) flags_ = (Flags)((int)flags_ | (int)flag);
        else flags_ = (Flags)((int)flags_ & (~(int)flag));
    }

public:
    static const int Id = 0xbcf434be;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

    static CFamilyTag* GetCFamily() {
        return new CFamilyTag(Flags::None);
    }

    static CFamilyTag* GetIsAllocLike() {
        return new CFamilyTag(Flags::IsAllocLike);
    }

    static CFamilyTag* GetIsCheckedAllocLike() {
        return new CFamilyTag(Flags::IsCheckedAllocLike);
    }

    static CFamilyTag* GetHasSetjmp() {
        return new CFamilyTag(Flags::HasSetjmp);
    }

    static CFamilyTag* GetHasLongjmp() {
        return new CFamilyTag(Flags::HasLongjmp);
    }

    bool IsAllocLike() const {
        return HasFlag(Flags::IsAllocLike) ||
               HasFlag(Flags::IsCheckedAllocLike);
    }

    void SetIsAllocLike(bool state) {
        SetFlag(Flags::IsAllocLike, state);
    }

    bool IsCheckedAllocLike() const {
        return HasFlag(Flags::IsCheckedAllocLike);
    }

    void SetIsCheckedAllocLike(bool state) {
        SetFlag(Flags::IsCheckedAllocLike, state);
    }

    bool HasSetjmp() const {
        return HasFlag(Flags::HasSetjmp);
    }

    void SetHasSetjmp(bool state) {
        SetFlag(Flags::HasSetjmp, state);
    }

    bool HasLongjmp() const {
        return HasFlag(Flags::HasLongjmp);
    }

    void SetHasLongjmp(bool state) {
        SetFlag(Flags::HasLongjmp, state);
    }

    bool HasAlloca() const {
        return HasFlag(Flags::HasAlloca);
    }

    void SetHasAlloca(bool state) {
        SetFlag(Flags::HasAlloca, state);
    }

    unsigned GetHashCode() const {
        return (unsigned)flags_;
    }

    bool operator== (const CFamilyTag& other) const {
        return flags_ == other.flags_;
    }

    bool operator!= (const CFamilyTag& other) const {
        return operator==(other) == false;
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::CFamilyTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::CFamilyTag::Id;
		}

		static Analysis::CFamilyTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::CFamilyTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif