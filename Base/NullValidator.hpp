// NullValidator.hpp
// Copyright (c) Lup Gratian
//
// Validation policy that does nothing.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_NULL_VALIDATOR_HPP
#define PC_BASE_NULL_VALIDATOR_HPP

namespace Base {

class NullValidator {
public:
	template <class T>
	static void IsNotNull(T* value) {}

	template <class T>
	static void IsNull(T* value) {}

	template <class T, class U>
	static void AreEqual(const T& expected, const U& actual) {}

	template <class T, class U>
	static void AreNotEqual(const T& expected, const U& actual) {}

	static void IsFalse(bool condition) {}

	template <class T>
	static void IsTrue(bool condition) {}

	template <class T, class U>
	static void IsSmaller(const T& actual, const U& expected) {}

	template <class T, class U>
	static void IsSmallerOrEqual(const T& actual, const U& expected) {}

	template <class T, class U>
	static void IsLarger(const T& actual, const U& expected) {}

	template <class T, class U>
	static void IsLargerOrEqual(const T& actual, const U& expected) {}

	static void Unreachable() {}
};

} // namespace Base
#endif