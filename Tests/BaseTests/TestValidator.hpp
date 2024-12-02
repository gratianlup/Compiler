// TestValidator.hpp
// Copyright (c) Lup Gratian
//
// Validator used by tests.
//---------------------------------------------------------------------------------------
#ifndef PC_TEST_VALIDATOR_HPP
#define PC_TEST_VALIDATOR_HPP

class TestValidator {
public:
	static int NotNullCt;
	static int NullCt;
	static int EqualCt;
	static int NotEqualCt;
	static int FalseCt;
	static int TrueCt;
	static int SmallerCt;
	static int SmallerEqCt;
	static int LargerCt;
	static int LargerEqCt;

	template <class T>
	static void IsNotNull(T* value) {
		if(value == nullptr) {
			NotNullCt++; 
		}
	}

	template <class T>
	static void IsNull(T* value) {
		if(value) {
			NullCt++; 
		}
	}

	template <class T, class U>
	static void AreEqual(const T& expected, const T& actual) {
		if(actual != expected) {
			EqualCt++; 
		}
	}

	template <class T, class U>
	static void AreNotEqual(const T& expected, const T& actual) {
		if(actual == expected) {
			NotEqualCt++; 
		}
	}

	static void IsFalse(bool condition) {
		if(condition) {
			FalseCt++; 
		}
	}

	template <class T>
	static void IsTrue(bool condition) {
		if(condition == false) {
			TrueCt++; 
		}
	}

	template <class T, class U>
	static void IsSmaller(const T& actual, const U& expected) {
		if(actual >= expected) {
			SmallerCt++; 
		}
	}

	template <class T, class U>
	static void IsSmallerOrEqual(const T& actual, const U& expected) {
		if(actual > expected) {
			SmallerEqCt++; 
		}
	}

	template <class T, class U>
	static void IsLarger(const T& actual, const U& expected) {
		if(actual <= expected) {
			LargerCt++; 
		}
	}

	template <class T, class U>
	static void IsLargerOrEqual(const T& actual, const U& expected) {
		if(actual < expected) {
			LargerEqCt++; 
		}
	}

	static void Reset() {
		NotNullCt= 0;
		NullCt= 0;
		EqualCt= 0;
		NotEqualCt= 0;
		FalseCt= 0;
		TrueCt= 0;
		SmallerCt= 0;
		SmallerEqCt= 0;
		LargerCt= 0;
		LargerEqCt= 0;
	}
};


// Static variables.
int TestValidator::NotNullCt = 0;
int TestValidator::NullCt= 0;
int TestValidator::EqualCt= 0;
int TestValidator::NotEqualCt= 0;
int TestValidator::FalseCt= 0;
int TestValidator::TrueCt= 0;
int TestValidator::SmallerCt= 0;
int TestValidator::SmallerEqCt= 0;
int TestValidator::LargerCt= 0;
int TestValidator::LargerEqCt= 0;

#endif