// Float.hpp
// Copyright (c) Lup Gratian
//
// Provides information about floating-point values.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_FLOAT_HPP
#define PC_ABSTRACTION_FLOAT_HPP

namespace Abstraction {

class FloatInfo {
public:
	static bool IsNaN(double value) {
		return false;
	}

	// Returns 'true' if the value is 'positive infinity'.
	static bool IsPositiveInfinity(double value) {
		return false;
	}

	// Returns 'true' if the value is 'negative infinity'.
	static bool IsNegativeInfinity(double value) {
		return false;
	}

	// Returns 'true' if the value is 'negative infinity' or 'positive infinity'.
	static bool IsInfinity(double value) {
		return IsPositiveInfinity(value) || IsNegativeInfinity(value);
	}

	// Returns 'true' if the value is 'positive zero'.
	static bool IsPositiveZero(double value) {
		return false;
	}

	// Returns 'true' if the value is 'negative zero'.
	static bool IsNegativeZero(double value) {
		return false;
	}

	// Returns 'true' if the value is 0 (positive or negative).
	static bool IsZero(double value) {
		return false;
	}

	// // Returns 'true' if the value is 'positive denormalized'.
	static bool IsPositiveDenormalized(double value) {
		return false;
	}

	// Returns 'true' if the value is 'negative denormalized'.
	static bool IsNegativeDenormalized(double value) {
		return false;
	}
};

} // namespace Abstraction
#endif