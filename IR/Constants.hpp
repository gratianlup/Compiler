// Constants.hpp
// Copyright (c) Lup Gratian
//
// Defines the constant operands (integer, floating, string,
// null and undefined constants).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_CONSTANTS_HPP
#define PC_IR_CONSTANTS_HPP

#include "Operand.hpp"
#include "IRTypes.hpp"
#include "../Base/StringBuffer.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class ConstantTable;


// Represents the type of a constant operand.
enum class ConstantKind {
	Integer,
	Floating,
	String,
	Null,
	Undefined
};


// Represents a value that is constant. 
// Can be an integer, a floating value or a character string.
class Constant : public Operand {
private:
	// Disallow calling of some methods from 'Operand'.
	void SetSymbol(Symbol* value) {}
	void SetDefiningInstr(Instruction* value) {}
	void SetType(const Type* value) {}
	void SetIsBoolean(bool value) {}

protected:
	Constant(ConstantKind kind, const Type* type);

	// Shall return 'true' if the object is the same with this object.
	virtual bool IsEqualTo(const Constant& other) const;

	virtual void FreeImpl() {
		// We never free a constant once it has been allocated.
		// Constant are freed by 'ConstantTable' only when the whole program is freed.
	}

public:
	friend class ConstantTable; // Give it access to the constructor.

	virtual ~Constant() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the type of the constant operand.
	ConstantKind Kind() const {
		return (ConstantKind)other_;
	}

	// Returns 'true' if this is a integer constant.
	bool IsIntConstant() const {
		return Kind() == ConstantKind::Integer;
	}

	// Returns 'true' if this is a floating constant.
	bool IsFloatConstant() const {
		return Kind() == ConstantKind::Floating;
	}

	// Returns 'true' if this is a string constant.
	bool IsStringConstant() const {
		return Kind() == ConstantKind::String;
	}

	// Returns 'true' if this is a null-pointer constant.
	bool IsNullConstant() const {
		return Kind() == ConstantKind::Null;
	}

	// Returns 'true' if this is an undefined constant.
	bool IsUndefinedConstant() const {
		return Kind() == ConstantKind::Undefined;
	}

	// Methods for implementing hashing.
	unsigned GetHashCode() const;

	bool operator== (const Constant& other) const {
		return IsEqualTo(other);
	}

	bool operator< (const Constant& other) {
		return false; // Needed by 'DefaultComparer'.
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<Constant> {
		static bool Is(const Operand* op) {
			return (Operand::Kind)op->kind_ == Operand::Kind::Constant;
		}

		static Constant* As(Operand* op) {
			return Is(op) ? static_cast<Constant*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents a constant integer value.
class IntConstant : public Constant {
private:
	__int64 value_;

protected:
	IntConstant(const Type* type, __int64 value);

	virtual bool IsEqualTo(const Constant& other) const override;
	virtual string ToStringImpl(int level) const override;

public:
	friend class ConstantTable; // Give it access to the constructor.

	virtual ~IntConstant() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated value.
	__int64 Value() const {
		return value_;
	}

	// Returns 'true' if the value is 0.
	bool IsZero() const {
		return value_ == 0;
	}

	// Returns 'true' if the value is 1.
	bool IsOne() const {
		return value_ == 1;
	}

	// Returns 'true' if the value is -1.
	bool IsMinusOne() const {
		return value_ == -1;
	}

    // Returns 'true' if the value is positive (zero included).
    bool IsPositive() const {
        return value_ >= 0;
    }

    // Returns 'true' if the value is negative.
    bool IsNegative() const {
        return value_ < 0;
    }

	// Returns the type of the operand.
	const IntegerType* GetType() const {
		return static_cast<const IntegerType*>(type_);
	}

	// Methods for implementing hashing.
	unsigned GetHashCode() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<IntConstant> {
		static bool Is(const Operand* op) {
			if((Operand::Kind)op->kind_ == Operand::Kind::Constant) {
				return static_cast<const Constant*>(op)->IsIntConstant();
			}

			return false;
		}

		static IntConstant* As(Operand* op) {
			return Is(op) ? static_cast<IntConstant*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents a constant floating value. Contains methods to test
// certain properties of floating values, like NaN.
class FloatConstant : public Constant {
private:
	double value_;

protected:
	FloatConstant(const Type* type, double value);

	virtual bool IsEqualTo(const Constant& other) const override;
	virtual string ToStringImpl(int level) const override;

public:
	friend class ConstantTable; // Give it access to the constructor.

	virtual ~FloatConstant() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated value.
	double Value() const {
		return value_;
	}

	// Returns the type of the operand.
	const FloatingType* GetType() const {
		return static_cast<const FloatingType*>(type_);
	}

	// Returns 'true' if the type of the value is 'float'.
	bool IsFloat() const {
		return type_->IsFloat();
	}

	// Returns 'true' if the type of the value is 'double'.
	bool IsDouble() const {
		return type_->IsDouble();
	}

	// Returns 'true' if the value is positive.
	bool IsPositive() const {
		return value_ >= 0;
	}

	// Returns 'true' if the value is negative.
	bool IsNegative() const {
		return value_ < 0;
	}

	// Methods for implementing hashing.
	unsigned GetHashCode() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<FloatConstant> {
		static bool Is(const Operand* op) {
			if((Operand::Kind)op->kind_ == Operand::Kind::Constant) {
				return static_cast<const Constant*>(op)->IsFloatConstant();
			}

			return false;
		}

		static FloatConstant* As(Operand* op) {
			return Is(op) ? static_cast<FloatConstant*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents a character string string constant.
class StringConstant : public Constant {
private:
	StringBuffer value_;

protected:
	StringConstant(const Type* type, const StringBuffer& value);

	virtual bool IsEqualTo(const Constant& other) const override;
	virtual string ToStringImpl(int level) const override;

public:
	friend class ConstantTable; // Give it access to the constructor.

	virtual ~StringConstant() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated value.
	const StringBuffer& Value() const {
		return value_;
	}

	// Returns the length of the string.
	__int64 Length() {
		return value_.Length();
	}

	// Returns 'true' if the string is empty.
	bool IsEmpty() {
		return value_.Length() == 0;
	}

	// Methods for implementing hashing.
	unsigned GetHashCode() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<StringConstant> {
		static bool Is(const Operand* op) {
			if((Operand::Kind)op->kind_ == Operand::Kind::Constant) {
				return static_cast<const Constant*>(op)->IsStringConstant();
			}

			return false;
		}

		static StringConstant* As(Operand* op) {
			return Is(op) ? static_cast<StringConstant*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents the null-pointer constant.
class NullConstant : public Constant {
protected:
	NullConstant(const Type* type) : Constant(ConstantKind::Null, type) {}

	virtual string ToStringImpl(int level) const override {
		return "NullConstant";
	}

public:
	friend class ConstantTable; // Give it access to the constructor.

	virtual ~NullConstant() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for implementing hashing.
	unsigned GetHashCode() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<NullConstant> {
		static bool Is(const Operand* op) {
			if((Operand::Kind)op->kind_ == Operand::Kind::Constant) {
				return static_cast<const Constant*>(op)->IsNullConstant();
			}

			return false;
		}

		static NullConstant* As(Operand* op) {
			return Is(op) ? static_cast<NullConstant*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents the undefined constant. Used when the value has not been defined yet.
// Allows more optimizations to be done with variable that are not initialized.
class UndefinedConstant : public Constant {
protected:
	UndefinedConstant(const Type* type) : Constant(ConstantKind::Undefined, type) {}

	virtual string ToStringImpl(int level) const override {
		return "UndefConstant";
	}

public:
	friend class ConstantTable; // Give it access to the constructor.

	virtual ~UndefinedConstant() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for implementing hashing.
	unsigned GetHashCode() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<UndefinedConstant> {
		static bool Is(const Operand* op) {
			if((Operand::Kind)op->kind_ == Operand::Kind::Constant) {
				return static_cast<const Constant*>(op)->IsUndefinedConstant();
			}

			return false;
		}

		static UndefinedConstant* As(Operand* op) {
			return Is(op) ? static_cast<UndefinedConstant*>(op) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif