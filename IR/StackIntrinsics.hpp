// StackIntrinsics.hpp.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_STACK_INTRINSICS_HPP
#define PC_STACK_INTRINSICS_HPP

#include "Intrinsic.hpp"
#include "Unit.hpp"
#include "IRTypes.hpp"
#include "Function.hpp"
#include "Variable.hpp"
#include "ControlInstructions.hpp"

namespace IR {

class StackIntrinsic : public Intrinsic {
protected:
	enum class Kind {
		StackTop,
		IncStackTop,
		RestoreStackTop
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Kind intrKind_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StackIntrinsic(Kind kind, const FunctionType* type, shared<string> name, Unit* parent) :
			Intrinsic(IntrinsicKind::Stack, type, name, parent), intrKind_(kind) {}

public:
	virtual ~StackIntrinsic() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this is a 'StackTopIntr' object.
	bool IsStackTopIntr() const {
		return (Kind)kind_ == Kind::StackTop;
	}

	// Returns 'true' if this is a 'IncStackTopIntr' object.
	bool IsIncStackTopIntr() const {
		return (Kind)kind_ == Kind::IncStackTop;
	}

	// Returns 'true' if this is a 'RestoreStackTopIntr' object.
	bool IsRestoreStackTopIntr() const {
		return (Kind)kind_ == Kind::RestoreStackTop;
	}

    virtual bool DoesWriteToMemory() const override {
        // Not to user memory at least.
        return false;
    }

    virtual bool DoesReadFromMemory() const override {
        // Not from user memory at least.
        return false;
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<StackIntrinsic> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<Intrinsic>()) {
				return temp->IsStackIntrinsic();
			}

			return false;
		}

		static StackIntrinsic* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<StackIntrinsic*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// funct stackTop() : int8*
// Returns the address of the top the current stack frame.
class StackTopIntr : public StackIntrinsic {
protected:
	StackTopIntr(const FunctionType* type, shared<string> name, Unit* parent) :
			StackIntrinsic(Kind::StackTop, type, name, parent) {}

public:
	// Returns the unique instance of the intrinsic.
	static StackTopIntr* GetStackTop(Unit* unit);

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<StackTopIntr> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<StackIntrinsic>()) {
				return temp->IsStackTopIntr();
			}

			return false;
		}

		static StackTopIntr* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<StackTopIntr*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// funct incStackTop(var val int64) : void
// Increments the address of the top of the current stack frame.
class IncStackTopIntr : public StackIntrinsic {
protected:
	IncStackTopIntr(const FunctionType* type, shared<string> name, Unit* parent) :
			StackIntrinsic(Kind::IncStackTop, type, name, parent) {}

public:
	// Returns the unique instance of the intrinsic.
	static IncStackTopIntr* GetIncStackTop(Unit* unit);

    Operand* GetValue(CallInstr* instr) {
        return instr->GetArgument(0);
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<IncStackTopIntr> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<StackIntrinsic>()) {
				return temp->IsIncStackTopIntr();
			}

			return false;
		}

		static IncStackTopIntr* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<IncStackTopIntr*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// funct restoreStackTop(var val int8*) : void
class RestoreStackTopIntr : public StackIntrinsic {
protected:
	RestoreStackTopIntr(const FunctionType* type, shared<string> name, Unit* parent) :
			StackIntrinsic(Kind::RestoreStackTop, type, name, parent) {}

public:
	// Returns the unique instance of the intrinsic.
	static RestoreStackTopIntr* GetRestoreStackTop(Unit* unit);

    Operand* GetValue(CallInstr* instr) {
        return instr->GetArgument(0);
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<RestoreStackTopIntr> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<StackIntrinsic>()) {
				return temp->IsRestoreStackTopIntr();
			}

			return false;
		}

		static RestoreStackTopIntr* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<RestoreStackTopIntr*>(symbol) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif