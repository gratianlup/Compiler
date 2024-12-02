// Interlocked.hpp
// Copyright (c) Lup Gratian
//
// Provides atomic operations for variables that are shared by multiple threads.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_INTERLOCKED_HPP
#define PC_ABSTRACTION_INTERLOCKED_HPP

#define NOMINMAX
#include <Windows.h>

namespace Abstraction {

class Interlocked {
public:
	static int Increment(volatile int* location) {
		return (int)InterlockedIncrement((long*)location);
	}
	
	static int Decrement(volatile int* location) {
		return (int)InterlockedDecrement((long*)location);
	}

	static __int64 Increment64(volatile __int64* location) {
		return InterlockedIncrement64(location);
	}
	
	static __int64 Decrement64(volatile __int64* location) {
		return InterlockedDecrement64(location);
	}

	/*static int Add(volatile int* location, int value) {
		return (int)InterlockedAdd((long*)location, (long)value);
	}

	static __int64 Add(volatile __int64* location, __int64 value) {
		return InterlockedAdd64(location, value);
	}*/

	static int Exchange(volatile int* location, int value) {
		return (int)InterlockedExchange((long*)location, value);
	}

	static __int64 Exchange64(volatile __int64* location, __int64 value) {
		return InterlockedExchange64(location, value);
	}

	static int CompareExchange(volatile int* location, int value, int comparand) {
		return (int)InterlockedCompareExchange((long*)location, (long)value,
												(long)comparand);
	}

	static __int64  CompareExchange64(volatile __int64* location, __int64 value, 
									  __int64 comparand) {
		return InterlockedCompareExchange64(location, value, comparand);
	}
};

} // namespace Abstraction
#endif