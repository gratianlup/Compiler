// X64Target.hpp
// Copyright (c) Lup Gratian
//
// Provides information about the Intel 64-bit target.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_X64_X64_TARGET_HPP
#define PC_X64_X64_TARGET_HPP

#include "../../Common/TargetData.hpp"
#include "../../Abstraction/Platform.hpp"
using namespace Common;
using namespace Abstraction;

namespace X64 {

class X64Target : public TargetData {
private:
	virtual IntegerKind MapIntToSizeType(TypeKind kind) const override {
		switch(kind) {
			case TypeKind::Bool      : return IntegerKind::Int8;
			case TypeKind::Char      : return IntegerKind::Int8;
			case TypeKind::UChar     : return IntegerKind::Int8;
			case TypeKind::WChar     : return IntegerKind::Int16;
			case TypeKind::Short     : return IntegerKind::Int16;
			case TypeKind::UShort    : return IntegerKind::Int16;
			case TypeKind::Int       : return IntegerKind::Int32;   
			case TypeKind::UInt      : return IntegerKind::Int32;   
			case TypeKind::Long      : return IntegerKind::Int32;
			case TypeKind::ULong     : return IntegerKind::Int32;
			case TypeKind::LongLong  : return IntegerKind::Int64;
			case TypeKind::ULongLong : return IntegerKind::Int64;
		}

		DebugValidator::Unreachable();
		return (IntegerKind)0;
	}

	virtual FloatingKind MapFloatToSizeType(TypeKind kind) const override {
		if(kind == TypeKind::Float) return FloatingKind::Float;
		else return FloatingKind::Double;
	}

public:
	// Creates and returns a new instance of this target.
	static shared<TargetData> Create() {
		return new X64Target();
	}
	
	virtual int Size(TypeKind kind) const override {
		if((kind == TypeKind::Float) || (kind == TypeKind::Double)) {
			switch(MapFloatToSizeType(kind)) {
				case FloatingKind::Float:  return 4;
				case FloatingKind::Double: return 8;
			}
		}
		else {
			switch(MapIntToSizeType(kind)) {
				case IntegerKind::Int8:  return 1;
				case IntegerKind::Int16: return 2;
				case IntegerKind::Int32: return 4;
				case IntegerKind::Int64: return 8;
			}
		}

		DebugValidator::Unreachable();
		return 0;
	}

	virtual int GetPointerSize() const override {
		return 8;
	}

	virtual int GetPointerSizeInBits() const override {
		return 64;
	}

	virtual int SizeInBits(TypeKind kind) const override {
		return 8 * Size(kind);
	}

	virtual __int64 GetMaxSize(NumberKind kind) const override {
		switch(kind) {
			case NumberKind::Integer:  8;
			case NumberKind::Floating: 8;
		}

		DebugValidator::Unreachable();
		return 0;
	}

	virtual __int64 GetMask(TypeKind kind) const override {
		switch(MapIntToSizeType(kind)) {
			case IntegerKind::Int8:  return (__int64)0xFF;
			case IntegerKind::Int16: return (__int64)0xFFFF;
			case IntegerKind::Int32: return (__int64)0xFFFFFFFF;
			case IntegerKind::Int64: return (__int64)0xFFFFFFFFFFFFFFFF;
		}

		DebugValidator::Unreachable();
		return 0;
	}

	virtual __int64 GetSignMask(TypeKind kind) const override {
		switch(MapIntToSizeType(kind)) {
			case IntegerKind::Int8:  return (__int64)0x80;
			case IntegerKind::Int16: return (__int64)0x8000;
			case IntegerKind::Int32: return (__int64)0x80000000;
			case IntegerKind::Int64: return (__int64)0x8000000000000000;
		}

		DebugValidator::Unreachable();
		return 0;
	}

	virtual __int64 Truncate(__int64 value, TypeKind kind) const override {
		switch(MapIntToSizeType(kind)) {
			case IntegerKind::Int8:  return ((unsigned __int64)value << (64 - 8))  >> (64 - 8);
			case IntegerKind::Int16: return ((unsigned __int64)value << (64 - 16)) >> (64 - 16);
			case IntegerKind::Int32: return ((unsigned __int64)value << (64 - 32)) >> (64 - 32);
			case IntegerKind::Int64: return value;
		}

		DebugValidator::Unreachable();
		return 0;
	}

	virtual bool IsOveflow(__int64 newValue, __int64 oldValue, TypeKind kind) const override {
		newValue = Truncate(newValue, kind);
		return newValue < oldValue;
	}

	virtual __int64 GetMaxValue(TypeKind kind) const override {
		return GetMask(kind);
	}

	virtual int GetAlignment(TypeKind kind) const override {
		if((kind == TypeKind::Float) || (kind == TypeKind::Double)) {
			switch(MapFloatToSizeType(kind)) {
				case FloatingKind::Float:  return 4;
				case FloatingKind::Double: return 8;
			}
		}
		else {
			switch(MapIntToSizeType(kind)) {
				case IntegerKind::Int8:  return 1;
				case IntegerKind::Int16: return 2;
				case IntegerKind::Int32: return 4;
				case IntegerKind::Int64: return 8;
			}
		}

		DebugValidator::Unreachable();
		return 0;
	}

	virtual int GetPointerAlignment() const override {
		return 8;
	}

	virtual bool AllBitsSet(__int64 value, TypeKind kind) const override {
		return false;
	}

	virtual bool FirstNBitsSet(int n, __int64 value, TypeKind kind) const override {
		return false;
	}

	virtual bool LastNBitsSet(int n, __int64 value, TypeKind kind) const override {
		return false;
	}

	virtual bool IsNaN(double value) const override {
		return FloatInfo::IsNaN(value);
	}

	virtual bool IsPositiveInfinity(double value) const override {
		return FloatInfo::IsPositiveInfinity(value);
	}

	virtual bool IsNegativeInfinity(double value) const override {
		return FloatInfo::IsNegativeInfinity(value);
	}

	virtual bool IsPositiveZero(double value) const override {
		return FloatInfo::IsPositiveZero(value);
	}

	virtual bool IsNegativeZero(double value) const override {
		return FloatInfo::IsNegativeZero(value);
	}

	virtual bool IsPositiveDenormalized(double value) const override {
		return FloatInfo::IsPositiveDenormalized(value);
	}

	virtual bool IsNegativeDenormalized(double value) const override {
		return FloatInfo::IsNegativeDenormalized(value);
	}
};

} // namespace X64
#endif