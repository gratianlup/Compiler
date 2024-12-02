// Intrinsic.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_INTRINSIC_HPP
#define PC_IR_INTRINSIC_HPP

#include "Function.hpp"
#include "../Base/String.hpp"
#include "../Base/Dictionary.hpp"
using namespace Base;

namespace IR {

// Represents the categories of intrinsics.
enum class IntrinsicKind {
	Atomic,
	Memory,
	Stack,
    Math,
    Bitwise,
	Helper,
    BoundsCheck
};


class Intrinsic : public Function {
protected:
	IntrinsicKind intrKind_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Intrinsic(IntrinsicKind intrKind, const FunctionType* type, 
              shared<string> name, Unit* parent) :
			Function(type, name, false /* isDef */, parent, 
					 SymbolVisibility::Extern, CallConventionType::Auto, 
					 true /* isIntr */), intrKind_(intrKind) {}

public:
	virtual ~Intrinsic() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the kind of the intrinsic.
	IntrinsicKind Kind() {
		return intrKind_;
	}

	// Methods to test the category of the intrinsic.
	bool IsAtomicIntrinsic()      const { return intrKind_ == IntrinsicKind::Atomic;      }
	bool IsMemoryIntrinsic()      const { return intrKind_ == IntrinsicKind::Memory;      }
	bool IsStackIntrinsic()       const { return intrKind_ == IntrinsicKind::Stack;       }
    bool IsMathIntrinsic()        const { return intrKind_ == IntrinsicKind::Math;        }
    bool IsBitwiseIntrinsic()     const { return intrKind_ == IntrinsicKind::Bitwise;     }
	bool IsHelperIntrinsic()      const { return intrKind_ == IntrinsicKind::Helper;      }
    bool IsBoundsCheckIntrinsic() const { return intrKind_ == IntrinsicKind::BoundsCheck; }

    // Should return 'true' if the intrinsic writes to user memory
    // (through a parameter or to global variables).
    virtual bool DoesWriteToMemory() const {
        return true;
    }

    // Should return 'true' if the intrinsic reads from user memory
    // (through a parameter or from global variables).
    virtual bool DoesReadFromMemory() const {
        return true;
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<Intrinsic> {
		static bool Is(const Symbol* op) {
			if((Symbol::Kind)op->kind_ == Symbol::Kind::Function) {
				return static_cast<const Function*>(op)->IsIntrinsic();
			}

			return false;
		}

		static Intrinsic* As(Symbol* op) {
			return Is(op) ? static_cast<Intrinsic*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents the table used to lookup instrinsics based on their name.
class IntrinsicTable {
private:
	Dictionary<const string*, Intrinsic*, true> intrs_;

public:
	~IntrinsicTable() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds the specified intrinsic to the table.
	void Add(Intrinsic* intrinsic) {
		intrs_.Add(intrinsic->Name(), intrinsic);
	}

	// Returns 'true' if an intrinsic having the specified name is found.
	bool Contains(const string& name) const {
		return intrs_.ContainsKey(&name);
	}

	// Returns the intrinsic with the specified name,
	// or 'nullptr' if it's not found.
	Intrinsic* Get(const string& name) {
		if(intrs_.ContainsKey(&name)) {
			return intrs_[&name];
		}
		else return nullptr;
	}
};

} // namespace IR
#endif