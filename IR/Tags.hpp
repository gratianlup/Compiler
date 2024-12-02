// Tags.hpp
// Copyright (c) Lup Gratian
//
// Defines the tags that can be used to annotate symbols/instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_TAGS_HPP
#define PC_IR_TAGS_HPP

#include "Tag.hpp"
#include "../Base/String.hpp"
using namespace Base;

namespace IR {

// Represents a name that ca be given to a temporary operand. 
class NameTag : public InstructionTag {
private:
	string name_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	NameTag(const string& name) : name_(name) {}
    NameTag(const NameTag& other);
    NameTag& operator= (const NameTag& other);

	virtual string ToStringImpl(int level) const override {
		return "NameTag: " + name_;
	}

public:
    static const int Id = 0xe8a9ccd8;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	static NameTag* GetName(const string& name) {
		return new NameTag(name);
	}

	virtual ~NameTag() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    virtual int GetId() const override {
        return Id;
    }

	// Returns the name of the temporary.
	const string& Name() const {
		return name_;
	}

	void SetName(const string& value) {
		name_ = value;
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<NameTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == NameTag::Id;
		}

		static NameTag* As(Tag* tag) {
			return Is(tag) ? static_cast<NameTag*>(tag) : nullptr;
		}
	};
} // namespace Detail


// Represents the location of an instruction in the original source code.
class InstructionLocation {
private:
    shared<string> file_;
    int line_;

public:
    InstructionLocation() {}

    InstructionLocation(shared<string> file, int line) :
            file_(file), line_(line) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    const string* FilePath() const {
        return file_;
    }

    int Line() const {
        return line_;
    }
};

// Represents a tag that the describes the origin of a symbol (file and source line).
class LocationTag : public InstructionTag {
private:
	InstructionLocation location_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	LocationTag(InstructionLocation location) : 
            location_(location) {}

	virtual string ToStringImpl(int level) const override {
		return string::Format(L"LocationTag: %d, %d", 
                              location_.FilePath()->Chars(), 
							  location_.Line());
	}

public:
    static const int Id = 0x60c4416b;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	static LocationTag* GetLocation(InstructionLocation location) {
		return new LocationTag(location);
	}

	virtual ~LocationTag() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

	const InstructionLocation& Location() const {
		return location_;
	}

	void SetLocation(InstructionLocation value) {
		location_ = value;
	}
};


namespace Detail {
    // Implements support for "dynamic cast".
    template <>
    struct TagPromoter<LocationTag> {
        static bool Is(const Tag* tag) {
            return tag->GetId() == LocationTag::Id;
        }

        static LocationTag* As(Tag* tag) {
            return Is(tag) ? static_cast<LocationTag*>(tag) : nullptr;
        }
    };
} // namespace Detail


// Represents a tag that marks thread synchronization
// regions attached to 'load' and 'store' instructions.
class SynchronizationTag : public InstructionTag {
private:
    SynchronizationTag();
    SynchronizationTag(const SynchronizationTag& other);
    SynchronizationTag& operator= (const SynchronizationTag& other);

    virtual string ToStringImpl(int level) const override {
        return "SynchronizationTag: ";
    }

public:
    static const int Id = 0x8b1ee7e6;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    static SynchronizationTag* GetSynchronization() {
        return new SynchronizationTag();
    }

    virtual ~SynchronizationTag() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    virtual int GetId() const override {
        return Id;
    }
};


namespace Detail {
    // Implements support for "dynamic cast".
    template <>
    struct TagPromoter<SynchronizationTag> {
        static bool Is(const Tag* tag) {
            return tag->GetId() == SynchronizationTag::Id;
        }

        static SynchronizationTag* As(Tag* tag) {
            return Is(tag) ? static_cast<SynchronizationTag*>(tag) : nullptr;
        }
    };
} // namespace Detail

} // namespace IR
#endif