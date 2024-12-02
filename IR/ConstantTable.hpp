// ConstantTable.hpp
// Copyright (c) Lup Gratian
//
// Defines a table used to store constant operands.
// Provides factory methods for creating unique constants.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_CONSTANT_TABLE_HPP
#define PC_IR_CONSTANT_TABLE_HPP

#include "IRTypes.hpp"
#include "Constants.hpp"
#include "TypeTable.hpp"
#include "../Base/StringBuffer.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

// A cache for some of the most used constants.
template <int FirstCacheSize, int Pow2CacheSize>
struct ConstantCache {
	static const int FirstSize = FirstCacheSize;
	static const int Pow2Size = Pow2CacheSize;

	// A cache with the first integer numbers, for each integer type.
	IntConstant* FirstInt8[FirstCacheSize];
	IntConstant* FirstInt16[FirstCacheSize];
	IntConstant* FirstInt32[FirstCacheSize];
	IntConstant* FirstInt64[FirstCacheSize];

	// A cache with all power of two integer number, for each integer type.
	IntConstant* Pow2Int8[Pow2CacheSize];
	IntConstant* Pow2Int16[Pow2CacheSize];
	IntConstant* Pow2Int32[Pow2CacheSize];
	IntConstant* Pow2Int64[Pow2CacheSize];

	// A cache with all -1 numbers, for each integer type.
	// 0 - int8... 3 - int64
	IntConstant* MinusOneInt[4];

	// A cache for frequently used floating point numbers.
	// [0] - float version, [1] - double version
	FloatConstant* ZeroFloat[2];
	FloatConstant* OneFloat[2];
	FloatConstant* TwoFloat[2];
	FloatConstant* MinusOneFloat[2];

	// A cache for frequently used null pointer constants.
	NullConstant* Int8Nullptr;
	NullConstant* Int16Nullptr;
	NullConstant* Int32Nullptr;
	NullConstant* Int64Nullptr;
	NullConstant* FloatNullptr;
	NullConstant* DoubleNullptr;
};


class ConstantTable {
private:
	typedef ConstantCache<65536, 65> Cache;

	Cache cache_;
	Dictionary<IntConstant*, IntConstant*, true> intConsts_;
	Dictionary<FloatConstant*, FloatConstant*, true> floatConsts_;
	Dictionary<StringConstant*, StringConstant*, true> stringConsts_;
	Dictionary<NullConstant*, NullConstant*, true> nullConsts_;
	Dictionary<UndefinedConstant*, UndefinedConstant*, true> undefConsts_;
	bool cacheDisabled_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Initializes the cache with the most used integer and floating-point numbers.
	void InitializeCache(TypeTable* typeTable);

	// Tries to retrieve the specified integer constant from the cache.
	IntConstant* GetIntFromCache(const IntegerType* intType, __int64 value);

	// Tries to retrieve the specified floating-point constant from the cache.
	FloatConstant* GetFloatFromCache(const FloatingType* floatType, double value);

public:
	ConstantTable(TypeTable* typeTable) {
		DebugValidator::IsNotNull(typeTable);
		InitializeCache(typeTable);
	}

	~ConstantTable();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for creating integer constants.
	IntConstant* GetInt(const Type* type, __int64 value);
	IntConstant* GetInt8(__int64 value);
	IntConstant* GetInt16(__int64 value);
	IntConstant* GetInt32(__int64 value);
	IntConstant* GetInt64(__int64 value);

	// Methods for creating floating constants.
	FloatConstant* GetFloating(const Type* type, double value);
	FloatConstant* GetFloat(double value);
	FloatConstant* GetDouble(double value);

	// Returns a string constant having the specified data.
	StringConstant* GetString(const Type* type, const StringBuffer& value);

	// Returns a nullptr constant having the specified type.
	NullConstant* GetNull(const Type* type);

	// Returns an undef constant having the specified type.
	UndefinedConstant* GetUndefined(const Type* type);
};

} // namespace IR
#endif