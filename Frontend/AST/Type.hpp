// Type.hpp
// Copyright (c) Lup Gratian
//
// Defines the Type, QType and Qualifier classes.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_TYPE_HPP
#define PC_AST_TYPE_HPP

#include "../Base/String.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/ObjectHash.hpp"
#include "Visitor.hpp"
#include "Qualifier.hpp"
using namespace Base;

namespace AST {

// Forward declarations.
class QType;
class BasicType;

// This is the base class for the type system. Derived classes from this
// represent more specialized types (like ArrayType, EnumType, etc.).
class Type : public Visitable {
private:
	Type();                        // Should not be created.
	Type(const Type&);             // Should not be copied.
	Type& operator= (const Type&); // Should not be assigned.

protected:
	static const char TYPE_QUALIFIED = 1;
	static const char TYPE_BASIC     = 2;
	static const char TYPE_ARRAY     = 3;
	static const char TYPE_POINTER   = 4;
	static const char TYPE_ENUM      = 5;
	static const char TYPE_STRUCT    = 6;
	static const char TYPE_UNION     = 7;
	static const char TYPE_FUNCTION  = 8;
	static const char TYPE_TYPEDEF   = 9;

	char kind_; // The kind should be set in the constructor of derived classes.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Constructor to be used by derived classes only.
	Type(int kind) : kind_(kind) {}

	// Should return 'true' if the type is incomplete (not defined).
	virtual bool IsIncompleteImpl() const {
		return false;
	}

	// Should return 'true' if the types are identical.
	// This is stricter than "compatible".
	virtual bool EqualsImpl(const Type* other) const {
		return false;
	}

	// Should return 'true' if the type (or one of it's children)
	// has a variable-length array.
	virtual bool IsVariableImpl() const {
		return false;
	}

	// Should generate a string that describes the information contained
	// in the type object. Children should be included.
	virtual string ToStringImpl(int level) const {
		return "";
	};

public:
	virtual ~Type() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this is a basic type (int, float, etc.).
	bool IsQualified() const {
		return kind_ == TYPE_QUALIFIED;
	}

	// Returns 'true' if this is a basic type (int, float, etc.).
	bool IsBasic() const {
		return kind_ == TYPE_BASIC;
	}

	// Returns 'true' if this is a array type (int, float, etc.).
	bool IsArray() const {
		return kind_ == TYPE_ARRAY;
	}

	// Returns 'true' if this is a pointer type (int, float, etc.).
	bool IsPointer() const {
		return kind_ == TYPE_POINTER;
	}

	// Returns 'true' if this is a enumeration type (int, float, etc.).
	bool IsEnum() const {
		return kind_ == TYPE_ENUM;
	}

	// Returns 'true' if this is a struct type (int, float, etc.).
	bool IsStruct() const {
		return kind_ == TYPE_STRUCT;
	}

	// Returns 'true' if this is a union type (int, float, etc.).
	bool IsUnion() const {
		return kind_ == TYPE_UNION;
	}

	// Returns 'true' if this is a function type (int, float, etc.).
	bool IsFunction() const {
		return kind_ == TYPE_FUNCTION;
	}

	// Returns 'true' if this is a 'typedef'.
	bool IsTypedef() const {
		return kind_ == TYPE_TYPEDEF;
	}

	// Returns 'true' if this is an integral type (all ints, char, enum).
	// If 'ignoreEnum' is set 'true' is not returned for 'enum' types.
	bool IsInteger(bool ignoreEnum = false) const;

	// Returns 'true' if this is a floating type (float, double). 
	bool IsFloating() const;
	
	// Returns 'true' if this is a character type (char, wchar_t).
	bool IsChar() const;

	// Returns 'true' if this is a string type (includes the wide variant).
	bool IsString() const;

	// Returns 'true' if this is a signed integer type (char, short, int, long, long long).
	bool IsSigned() const;

	// Returns 'true' if this is a unsigned integer type (char, short, int, long, long long).
	bool IsUnsigned() const;

	// Returns 'true' if this is a integer or floating type.
	bool IsArithmetic() const;

	// Returns 'true' if this is an arithmetic or pointer type.
	bool IsScalar() const;

	// Returns 'true' if this is a 'struct' or 'union'.
	bool IsRecord() const;

	// Returns 'true' if this is a 'struct', 'union' or array.
	bool IsAggregate() const;

	// Returns 'true' if this is a object type.
	// Not included: functions, incomplete arrays and 'void'.
	bool IsObject() const;

	// Returns 'true' if this is the basic type 'void'.
	bool IsVoid() const;

	// Returns 'true' if this is the basic type 'bool'.
	bool IsBool() const;

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return dynamic_cast<T*>(this);
	}

	template <class T>
	const T* As() const {
		return dynamic_cast<T*>(const_cast<Type*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return As<T>();
	}

	// Returns the type casted to an integer type. Note that this considers
	// 'enum' to be an integer type, returning it's underlying type.
	// If the type can not be considered an integer 'nullptr' is returned.
	const BasicType* AsIntegerType() const;

	// Returns 'true' if this type can be promoted to a 'int' type.
	bool CanPromoteToInt() const;

	// Returns the type with all qualifiers removed. Note that the qualifiers
	// are not actually removed, only the 'naked' type is returned.
	const Type* WithoutQualifiers() const;

	// Returns the inner type of the chain of typedefs.
	const Type* InnerType() const;

	// Returns 'true' if the size of the type is not known.
	// Applies to arrays, struct, union, enum.
	bool IsIncomplete() const {
		return IsIncompleteImpl();
	}

	// Returns 'true' if the types are identical (children included).
	bool Equals(const Type* other) const {
		return EqualsImpl(other);
	}

	// Returns 'true' if the type is variable (children included).
	bool IsVariable() const {
		return IsVariableImpl();
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

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const = 0;
};


// Represents a type that has a qualifier attached to it.
// Separating the qualifier from the 'Type' allows creating fever 'Type' objects.
// For example, a single 'BasicType' is required to represent int, const int, etc.
class QType : public Type {
private:
	const Type* base_; // The type that is qualified by this object.
	Qualifier   qual_; // The qualifiers associated with the object.

protected:
	virtual bool IsIncompleteImpl() const override;
	virtual string ToStringImpl(int level) const override;

public:
	static const int KIND = TYPE_QUALIFIED;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	QType(const Type* base) : 
			base_(base), Type(TYPE_QUALIFIED) {}

	QType(const Type* base, const Qualifier& qual) : 
			base_(base), qual_(qual), Type(TYPE_QUALIFIED) {}

	virtual ~QType() {
		// Nothing to do here, the shared pointer takes care of 'base_'.
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the qualified type.
	const Type* Base() const {
		return base_;
	}

	// Returns the qualifiers applied to the type.
	const Qualifier& GetQualifiers() const {
		return qual_;
	}

	// Methods for quick access to info about the applied qualifiers.
	bool HasConst() const {
		return qual_.HasConst();
	}

	bool HasVolatile() const {
		return qual_.HasVolatile();
	}

	bool HasRestrict() const {
		return qual_.HasRestrict();
	}

	bool HasQualifier() const {
		return qual_.HasNone() != false;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}

	// Computes the hash code for a QType having the specified properties.
	static ObjectHash GetHashCode(const Type* type, Qualifier qual);
};

} // namespace AST
#endif