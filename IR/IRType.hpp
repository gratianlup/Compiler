// Type.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for all IR types.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_TYPE_HPP
#define PC_IR_TYPE_HPP

#include  "Visitor.hpp"
#include "../Base/String.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class Type;


namespace Detail {
	// Used for implementing 'As<T>'/'Is<T>'.
	template <class T>
	struct TypePromoter {
		static bool Is(const Type* op) {
			static_assert(false, "Type is not a Type");
			return false;
		}

		static const T* As(const Type* op) {
			static_assert(false, "Type is not a Type");
			return nullptr;
		}
	};
} // namespace Detail


class Type : public Visitable {
private:
	Type(const Type&);             // Should not be copied.
	Type& operator= (const Type&); // Should not be assigned.

protected:
	enum class Kind {
		Integer,
		Floating,
		Void,
		Pointer,
		Array,
		Record,
		Function
	};

	char kind_  : 4; // The kind should be set in the constructor of derived classes.
	char other_ : 4; // Can be used by derived classes to store other information.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Constructor to be used by derived classes only.
	Type() {}

	Type(int kind, int other = 0) : kind_(kind), other_(other) {}

	// Should generate a string that describes the type object.
	virtual string ToStringImpl(int level) const {
		return "";
	}

public:
	template <class T>
	friend struct Detail::TypePromoter; // Give it access to 'kind_'.

	virtual ~Type() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this is a integer type.
	bool IsInteger() const {
		return (Kind)kind_ == Kind::Integer;
	}

	// Returns 'true' if this is a floating type.
	bool IsFloating() const {
		return (Kind)kind_ == Kind::Floating;
	}

	// Returns 'true' if this is 'void'.
	bool IsVoid() const {
		return (Kind)kind_ == Kind::Void;
	}

	// Returns 'true' if this is a simple type (i8...i64, float double, void).
	bool IsSimple() const {
		return IsInteger() || IsFloating() || IsVoid();
	}

	// Returns 'true' if this is a pointer type.
	bool IsPointer() const {
		return (Kind)kind_ == Kind::Pointer;
	}

	// Returns 'true' if this is an array type.
	bool IsArray() const {
		return (Kind)kind_ == Kind::Array;
	}

	// Returns 'true' if this is a record type.
	bool IsRecord() const {
		return (Kind)kind_ == Kind::Record;
	}

	// Returns 'true' if this is a function type.
	bool IsFunction() const {
		return (Kind)kind_ == Kind::Function;
	}

	// Methods for testing the subtype of integer and floating types.
	bool IsInt8() const;
	bool IsInt16() const;
	bool IsInt32() const;
	bool IsInt64() const;
	bool IsFloat() const;
	bool IsDouble() const;

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	const T* As() const {
		return Detail::TypePromoter<T>::As(this);
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return Detail::TypePromoter<T>::Is(this);
	}

	// Returns 'true' if the type are of the same kind.
	bool IsSameKind(const Type* other) const {
		return kind_ == other->kind_;
	}

	// Returns a string representation of the type information.
	// Used for debugging.
	string ToString(int level) const {
		return ToStringImpl(level);
	}

	// Print the string representation to the console.
	void Dump() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};

} // namespace IR
#endif