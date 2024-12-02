// ConstantTable.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantTable.hpp"
#include "../Abstraction/Platform.hpp"

namespace IR {

ConstantTable::~ConstantTable() {
	// Free the memory allocated for all constants.
	intConsts_.ForEachValue([](IntConstant* intConst) -> bool {
		delete intConst;
		return true;
	});

	floatConsts_.ForEachValue([](FloatConstant* floatConst) -> bool {
		delete floatConst;
		return true;
	});

	stringConsts_.ForEachValue([](StringConstant* stringConst) -> bool {
		delete stringConst;
		return true;
	});

	nullConsts_.ForEachValue([](NullConstant* nullConst) -> bool {
		delete nullConst;
		return true;
	});

	undefConsts_.ForEachValue([](UndefinedConstant* undefConst) -> bool {
		delete undefConst;
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void ConstantTable::InitializeCache(TypeTable* typeTable) {
	cacheDisabled_ = true;

	// Create the first constant numbers.
	for(int i = 0; i < Cache::FirstSize; i++) {
		cache_.FirstInt8[i] = GetInt8(i);
		cache_.FirstInt16[i] = GetInt16(i);
		cache_.FirstInt32[i] = GetInt32(i);
		cache_.FirstInt64[i] = GetInt64(i);
	}

	cache_.Pow2Int8[0] = GetInt8(0);
	cache_.Pow2Int16[0] = GetInt16(0);
	cache_.Pow2Int32[0] = GetInt32(0);
	cache_.Pow2Int64[0] = GetInt64(0);

	// Create the power of two numbers.
	for(int  i = 1; i < Cache::Pow2Size; i++) {
		cache_.Pow2Int8[i] = GetInt8(1ULL << (i - 1));
		cache_.Pow2Int16[i] = GetInt16(1ULL << (i - 1));
		cache_.Pow2Int32[i] = GetInt32(1ULL << (i - 1));
		cache_.Pow2Int64[i] = GetInt64(1ULL << (i - 1));
	}

	// Create the -1 numbers, for each integer type.
	cache_.MinusOneInt[0] = GetInt8(-1);
	cache_.MinusOneInt[1] = GetInt16(-1);
	cache_.MinusOneInt[2] = GetInt32(-1);
	cache_.MinusOneInt[3] = GetInt64(-1);

	// Create frequently used floating-point numbers.
	cache_.ZeroFloat[0] = GetFloat(0.0);
	cache_.ZeroFloat[1] = GetDouble(0.0);
	cache_.OneFloat[0] = GetFloat(1.0);
	cache_.OneFloat[1] = GetDouble(1.0);
	cache_.TwoFloat[0] = GetFloat(2.0);
	cache_.TwoFloat[1] = GetDouble(2.0);
	cache_.MinusOneFloat[0] = GetFloat(-1.0);
	cache_.MinusOneFloat[1] = GetDouble(-1.0);

	// Create frequently used null pointer constants.
	cache_.Int8Nullptr = GetNull(typeTable->GetPointer(IntegerType::GetInt8()));
	cache_.Int16Nullptr = GetNull(typeTable->GetPointer(IntegerType::GetInt16()));
	cache_.Int32Nullptr = GetNull(typeTable->GetPointer(IntegerType::GetInt32()));
	cache_.Int64Nullptr = GetNull(typeTable->GetPointer(IntegerType::GetInt64()));
	cache_.FloatNullptr = GetNull(typeTable->GetPointer(FloatingType::GetFloat()));
	cache_.DoubleNullptr = GetNull(typeTable->GetPointer(FloatingType::GetDouble()));

	// The cache can now be enabled.
	cacheDisabled_ = false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* ConstantTable::GetIntFromCache(const IntegerType* intType, __int64 value) {
	DebugValidator::IsNotNull(intType);

	if((value >= 0) && (value < Cache::FirstSize)) {
		switch(intType->GetSubtype()) {
			case IRIntegerKind::Int8:  return cache_.FirstInt8[value];
			case IRIntegerKind::Int16: return cache_.FirstInt16[value];
			case IRIntegerKind::Int32: return cache_.FirstInt32[value];
			case IRIntegerKind::Int64: return cache_.FirstInt64[value];
			default: DebugValidator::Unreachable();
		}
	}
	else if(value == -1) {
		switch(intType->GetSubtype()) {
			case IRIntegerKind::Int8:  return cache_.MinusOneInt[0];
			case IRIntegerKind::Int16: return cache_.MinusOneInt[1];
			case IRIntegerKind::Int32: return cache_.MinusOneInt[2];
			case IRIntegerKind::Int64: return cache_.MinusOneInt[3];
			default: DebugValidator::Unreachable();
		}
	}
	else if((value & (value - 1)) == 0) {
		// This is a power of two, these numbers are all cached.
		int bit = Abstraction::Integer::RightmostSetBit(value) + 1;

		switch(intType->GetSubtype()) {
			case IRIntegerKind::Int8:  return cache_.Pow2Int8[bit];
			case IRIntegerKind::Int16: return cache_.Pow2Int16[bit];
			case IRIntegerKind::Int32: return cache_.Pow2Int32[bit];
			case IRIntegerKind::Int64: return cache_.Pow2Int64[bit];
			default: DebugValidator::Unreachable();
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* ConstantTable::GetInt(const Type* type, __int64 value) {
	DebugValidator::IsNotNull(type);
	DebugValidator::IsTrue(type->IsInteger());

	// First try to get the number from the cache, 
	// because no synchronization is needed in this case.
	auto intType = type->As<IntegerType>();

	if(cacheDisabled_ == false) {
		if(auto result = GetIntFromCache(intType, value)) {
			return result;
		}
	}

	// The numbers is not cached, use the standard method.
	IntConstant intConst(type, value);
	IntConstant* availableConst;

	// Try to get the constant from the table. 
    // If not available we need to create it now.
	if(intConsts_.TryGetValue(&intConst, &availableConst)) {
		return availableConst;
	}

	IntConstant* newConst = new IntConstant(type, value);
	intConsts_.Add(newConst, newConst);
	return newConst;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IntConstant* ConstantTable::GetInt8(__int64 value) {
	return GetInt(IntegerType::GetInt8(), value);
}

IntConstant* ConstantTable::GetInt16(__int64 value) {
	return GetInt(IntegerType::GetInt16(), value);
}

IntConstant* ConstantTable::GetInt32(__int64 value) {
	return GetInt(IntegerType::GetInt32(), value);
}
IntConstant* ConstantTable::GetInt64(__int64 value) {
	return GetInt(IntegerType::GetInt64(), value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FloatConstant* ConstantTable::GetFloatFromCache(const FloatingType* floatType, double value) {
	if(value == 0.0) {
		return cache_.ZeroFloat[floatType->IsFloat() ? 0 : 1];
	}
	else if(value == 1.0) {
		return cache_.OneFloat[floatType->IsFloat() ? 0 : 1];
	}
	else if(value == 2.0) {
		return cache_.TwoFloat[floatType->IsFloat() ? 0 : 1];
	}
	else if(value == -1.0) {
		return cache_.MinusOneFloat[floatType->IsFloat() ? 0 : 1];
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FloatConstant* ConstantTable::GetFloating(const Type* type, double value) {
	DebugValidator::IsNotNull(type);
	DebugValidator::IsTrue(type->IsFloating());
	auto floatType = type->As<FloatingType>();

	// First try to get the number from the cache, 
	// because no synchronization is needed in this case.
	if(cacheDisabled_ == false) {
		if(auto result = GetFloatFromCache(floatType, value)) {
			return result;
		}
	}

	// The numbers is not cached, use the standard method.
	FloatConstant floatConst(type, value);
	FloatConstant* availableConst;

	// Try to get the constant from the table. If not available we need to create it now.
	if(floatConsts_.TryGetValue(&floatConst, &availableConst)) {
		return availableConst;
	}

	FloatConstant* newConst = new FloatConstant(type, value);
	floatConsts_.Add(newConst, newConst);
	return newConst;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FloatConstant* ConstantTable::GetFloat(double value) {
	return GetFloating(FloatingType::GetFloat(), value);
}

FloatConstant* ConstantTable::GetDouble(double value) {
	return GetFloating(FloatingType::GetDouble(), value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StringConstant* ConstantTable::GetString(const Type* type, const StringBuffer& value) {
	DebugValidator::IsNotNull(type);

	StringConstant stringConst(type, value);
	StringConstant* availableConst;

	// Try to get the constant from the table.
	if(stringConsts_.TryGetValue(&stringConst, &availableConst)) {
		return availableConst;
	}

	StringConstant* newConst = new StringConstant(type, value);
	stringConsts_.Add(newConst, newConst);
	return newConst;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NullConstant* ConstantTable::GetNull(const Type* type) {
	DebugValidator::IsNotNull(type);
	DebugValidator::IsTrue(type->IsPointer());

	// Fast check for some common cases first.
	if(cacheDisabled_ == false) {
		auto pointeeType = type->As<PointerType>()->PointeeType();

		if(auto intType = pointeeType->As<IntegerType>()) {
			switch (intType->GetSubtype()) {
				case IRIntegerKind::Int8:  return cache_.Int8Nullptr;
				case IRIntegerKind::Int16: return cache_.Int16Nullptr;
				case IRIntegerKind::Int32: return cache_.Int32Nullptr;
				case IRIntegerKind::Int64: return cache_.Int64Nullptr;
				default: DebugValidator::Unreachable();
			}
		}
		else if(auto floatingType = pointeeType->As<FloatingType>()) {
			if(floatingType->IsFloat()) {
				return cache_.FloatNullptr;
			}
			else return cache_.DoubleNullptr;
		}
	}

	NullConstant nullConst(type);
	NullConstant* availableConst;

	// Try to get the constant from the table.
	if(nullConsts_.TryGetValue(&nullConst, &availableConst)) {
		return availableConst;
	}

	NullConstant* newConst = new NullConstant(type);
	nullConsts_.Add(newConst, newConst);
	return newConst;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UndefinedConstant* ConstantTable::GetUndefined(const Type* type) {
	UndefinedConstant undefConst(type);
	UndefinedConstant* availableConst;

	// Try to get the constant from the table.
	if(undefConsts_.TryGetValue(&undefConst, &availableConst)) {
		return availableConst;
	}

	UndefinedConstant* newConst = new UndefinedConstant(type);
	undefConsts_.Add(newConst, newConst);
	return newConst;
}

} // namespace IR