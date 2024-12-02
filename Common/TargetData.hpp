// TargetData.hpp
// Copyright (c) Lup Gratian
//
// Contains information about the target machine (data types, type size, etc.).
// This information is used only by the frontends.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_TARGET_INFO_HPP
#define PC_COMMON_TARGET_INFO_HPP

#include "../Base/DebugValidator.hpp"
#include "../Base/SharedPointer.hpp"
using namespace Base;

namespace Common {

// Defines the types of supported numbers.
enum class NumberKind {
	Integer,
	Floating
};


// Defines the types that are supported by the implementation.
enum class TypeKind {
	Bool,
	Char,
	UChar,
	WChar,
	String,
	WString,
	Short,
	UShort,
	Int,
	UInt,
	Long,
	ULong,
	LongLong,
	ULongLong,
	Float,
	Double,
	Void, // It's considered a basic type too.
	END   // Used as the number of constants in the enum.
};

// Provides information about the compilation target machine.
class TargetData {
protected:
	// Defines the types of supported integers.
	enum class IntegerKind {
		Int8,
		Int16,
		Int32,
		Int64
	};

	// Defines the types of supported floating values.
	enum class FloatingKind {
		Float,
		Double
	};

	// Methods that should map from a C type to a size-based type.
	virtual IntegerKind MapIntToSizeType(TypeKind kind) const = 0;
	virtual FloatingKind MapFloatToSizeType(TypeKind kind) const = 0;

public:
	virtual ~TargetData() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool IsInteger(NumberKind kind) const {
		return kind == NumberKind::Integer;
	}

	virtual bool IsFloating(NumberKind kind) const {
		return kind == NumberKind::Floating;
	}

	// Returns the size (in bytes) of the specified type.
	virtual int Size(TypeKind kind) const = 0;

	// Returns the size (in bytes) of the pointer type.
	virtual int GetPointerSize() const = 0;

	// Returns the size (in bits) of the pointer type.
	virtual int GetPointerSizeInBits() const = 0;

	// Returns the size (in bits) of a type.
	virtual int SizeInBits(TypeKind kind) const = 0;

	// Returns the default alignment for the specified integer type.
	virtual int GetAlignment(TypeKind kind) const = 0;

	// Returns the default alignment for the pointer type.
	virtual int GetPointerAlignment() const = 0;

	// Gets the maximum size (in bytes) of the specified number type.
	virtual __int64 GetMaxSize(NumberKind kind) const = 0;

	// Returns a mask that covers the maximum value for the given integer type.
	virtual __int64 GetMask(TypeKind kind) const = 0;

	// Truncates the value so that it fits in the specified integer type.
	virtual __int64 Truncate(__int64 value, TypeKind kind) const = 0;

	// Verifies if overflow occurred after 'newVal' was incremented.
	virtual bool IsOveflow(__int64 newVal, __int64 oldVal, TypeKind kind) const = 0;

	// Returns a mask with the sign bit set for the specified integer type.
	virtual __int64 GetSignMask(TypeKind kind) const = 0;

	// Returns the maximum value for the specified integer type.
	virtual __int64 GetMaxValue(TypeKind kind) const = 0;

	// Returns 'true' if all bits are set to 1.
	virtual bool AllBitsSet(__int64 value, TypeKind kind) const = 0;

	// Returns 'true' if the first (leftmost) 'n' bits are set to 1.
	virtual bool FirstNBitsSet(int n, __int64 value, TypeKind kind) const = 0;

	// Returns 'true' if the last (rightmost) 'n' bits are set to 1.
	virtual bool LastNBitsSet(int n, __int64 value, TypeKind kind) const = 0;

	// USE: int_fpclass on Win
	// Returns 'true' if the value is not a valid number (NaN).
	virtual bool IsNaN(double value) const = 0;

	// Returns 'true' if the value is 'positive infinity'.
	virtual bool IsPositiveInfinity(double value) const  = 0;

	// Returns 'true' if the value is 'negative infinity'.
	virtual bool IsNegativeInfinity(double value) const = 0;

	// Returns 'true' if the value is represents infinity (positive or negative).
	virtual bool IsInfinity(double value) const {
		return IsPositiveInfinity(value) || IsNegativeInfinity(value);
	}

	// Returns 'true' if the value is 'positive zero'.
	virtual bool IsPositiveZero(double value) const = 0;

	// Returns 'true' if the value is 'negative zero'.
	virtual bool IsNegativeZero(double value) const = 0;

	// Returns 'true' if the value is 0 (positive or negative).
	virtual bool IsZero(double value) const {
		return IsPositiveZero(value) || IsNegativeZero(value);
	}

	// // Returns 'true' if the value is 'positive denormalized'.
	virtual bool IsPositiveDenormalized(double value) const = 0;

	// // Returns 'true' if the value is 'negative denormalized'.
	virtual bool IsNegativeDenormalized(double value) const = 0;

	// // Returns 'true' if the value is 'positive denormalized' (positive or negative).
	virtual bool IsDenormalized(double value) const {
		return IsPositiveDenormalized(value) || IsNegativeDenormalized(value);
	}
};

} // namespace Common
#endif