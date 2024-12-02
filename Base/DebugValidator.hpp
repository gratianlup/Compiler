// DebugValidator.hpp
// Copyright (c) Lup Gratian
//
// Policy used for validating certain conditions/parameters under Debug mode.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_DEBUG_VALIDATOR_HPP
#define PC_BASE_DEBUG_VALIDATOR_HPP

#include <cassert>
#include <cstdlib>

namespace Base {

class DebugValidator {
public:
	template <class T>
	static void IsNotNull(T* value) {
		assert(value);
	}

	template <class T>
	static void IsNull(T* value) {
		assert(value == nullptr);
	}

	template <class T, class U>
	static void AreEqual(const T& expected, const U& actual) {
		assert(expected == (T)actual);
	}

	template <class T, class U>
	static void AreNotEqual(const T& expected, const U& actual) {
		assert(expected != (T)actual);
	}

	static void IsFalse(bool condition) {
		assert(condition == false); 
	}

	static void IsTrue(bool condition) {
		assert(condition == true);
	}

	template <class T, class U>
	static void IsSmaller(const T& actual, const U& expected) {
		assert(actual < (T)expected);
	}

	template <class T, class U>
	static void IsSmallerOrEqual(const T& actual, const U& expected) {
		assert(actual <= (T)expected);
	}

	template <class T, class U>
	static void IsLarger(const T& actual, const U& expected) {
		assert(actual > (T)expected);
	}

	template <class T, class U>
	static void IsLargerOrEqual(const T& actual, const U& expected) {
		assert(actual >= (T)expected);
	}

	static void Unreachable() {
		assert(false);
	}
};

} // namespace Base
#endif