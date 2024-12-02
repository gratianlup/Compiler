// MemoryIntrinsics.hpp
// Copyright (c) Lup Gratian
//
// Defines the intrinsics that manipulate memory (copy/set memory).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_MEMORY_INTRINSICS_HPP
#define PC_IR_MEMORY_INTRINSICS_HPP

#include "Intrinsic.hpp"
#include "Unit.hpp"
#include "IRTypes.hpp"
#include "Constants.hpp"
#include "Function.hpp"
#include "Variable.hpp"
#include "ControlInstructions.hpp"

namespace IR {

// Base class for all intrinsics that manipulate memory.
class MemoryIntrinsic : public Intrinsic {
protected:
	enum class Kind {
		CopyMemory,
		SetMemory,
        Prefetch
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Kind intrKind_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	MemoryIntrinsic(Kind kind, const FunctionType* type, shared<string> name, Unit* parent) :
			Intrinsic(IntrinsicKind::Memory, type, name, parent), intrKind_(kind) {}

public:
	virtual ~MemoryIntrinsic() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this is a 'CopyMemoryIntr' intrinsic.
	bool IsCopyMemoryIntr() const {
		return intrKind_ == Kind::CopyMemory;
	}

	// Returns 'true' if this is a 'SetMemoryIntr' intrinsic.
	bool IsSetMemoryIntr() const {
		return intrKind_ == Kind::SetMemory;
	}

    // Returns 'true' if this is a 'Prefetch' intrinsic.
    bool IsPrefetchIntr() const {
        return intrKind_ == Kind::Prefetch;
    }

    virtual bool DoesWriteToMemory() const override {
        // We consider that 'prefetch' doesn't write to memory.
        return IsPrefetchIntr() == false;
    }

    virtual bool DoesReadFromMemory() const override {
        // We consider that 'prefetch' doesn't read from memory.
        return IsPrefetchIntr() == false;
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<MemoryIntrinsic> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<Intrinsic>()) {
				return temp->IsMemoryIntrinsic();
			}

			return false;
		}

		static MemoryIntrinsic* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<MemoryIntrinsic*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// funct copyMemory(var dest int8*, var src int8*, var len int64) : void
// Copies 'len' bytes from 'src' to 'dest' (equivalent to 'memcpy').
class CopyMemoryIntr : public MemoryIntrinsic {
protected:
	CopyMemoryIntr(const FunctionType* type, shared<string> name, Unit* parent) :
			MemoryIntrinsic(Kind::CopyMemory, type, name, parent) {}

public:
	// Returns the unique instance of the intrinsic.
	static CopyMemoryIntr* GetCopyMemory(Unit* unit);

    static Operand* GetDestination(CallInstr* instr) {
        return instr->GetArgument(0);
    }

    static Operand* GetSource(CallInstr* instr) {
        return instr->GetArgument(1);
    }

    static Operand* GetLength(CallInstr* instr) {
        return instr->GetArgument(2);
    }

    static IntConstant* GetConstantLength(CallInstr* instr) {
        return GetLength(instr)->As<IntConstant>();
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<CopyMemoryIntr> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<MemoryIntrinsic>()) {
				return temp->IsCopyMemoryIntr();
			}

			return false;
		}

		static CopyMemoryIntr* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<CopyMemoryIntr*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// funct setMemory(var dest int8*, var val int8, var len int64) : void
// Sets the first 'len' bytes in 'dest' to 'val' (equivalent to 'memset').
class SetMemoryIntr : public MemoryIntrinsic {
protected:
	SetMemoryIntr(const FunctionType* type, shared<string> name, Unit* parent) :
			MemoryIntrinsic(Kind::SetMemory, type, name, parent) {}

public:
	// Returns the unique instance of the intrinsic.
	static SetMemoryIntr* GetSetMemory(Unit* unit);

    static Operand* GetDestination(CallInstr* instr) {
        return instr->GetArgument(0);
    }

    static Operand* GetSource(CallInstr* instr) {
        return instr->GetArgument(1);
    }

    static Operand* GetLength(CallInstr* instr) {
        return instr->GetArgument(2);
    }

    static IntConstant* GetConstantLength(CallInstr* instr) {
        return GetLength(instr)->As<IntConstant>();
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<SetMemoryIntr> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<MemoryIntrinsic>()) {
				return temp->IsSetMemoryIntr();
			}

			return false;
		}

		static SetMemoryIntr* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<SetMemoryIntr*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// Defines the type of hints the 'prefetch' intrinsic can take.
// Note that the behavior is highly implementation defined, 
// even for CPUs from the same vendor.
namespace Prefetch {
    static const int HINT_T0  = 0; // The data may be placed into L1 cache.
    static const int HINT_T1  = 1; // The data may be placed into L2 cache.
    static const int HINT_T2  = 2; // The data may be placed into a higher-level cache.
    static const int HINT_NTA = 3; // Non-temporal aligned hint.
}

// funct prefetch(var addr int8*, var hint int32) : void
// Tries to prefetch the memory at the specified address.
// 'type' specifies where the memory should be prefetched (cache L1, L2, etc.).
class PrefetchIntr : public MemoryIntrinsic {
protected:
	PrefetchIntr(const FunctionType* type, shared<string> name, Unit* parent) :
			MemoryIntrinsic(Kind::Prefetch, type, name, parent) {}

public:
	// Returns the unique instance of the intrinsic.
	static PrefetchIntr* GetPrefetch(Unit* unit);

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<PrefetchIntr> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<MemoryIntrinsic>()) {
				return temp->IsPrefetchIntr();
			}

			return false;
		}

		static PrefetchIntr* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<PrefetchIntr*>(symbol) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif