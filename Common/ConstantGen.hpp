// ConstantGen.hpp
// Copyright (c) Lup Gratian
//
// Defines templates to generate constants for the type of diagnostic messages.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_CONSTANT_GEN_HPP
#define PC_COMMON_CONSTANT_GEN_HPP

namespace Common {
	
// The type of the diagnostic code.
typedef int DiagnosticCode;


// Structure of a diagnostic code constant.
// A combination of the diagnostic type and an unique ID.
// |  TYPE  |           ID           |
// | 4 bits |         28 bits        |

// Constant beginning with TYPE are for internal use. 
// They are mapped, based on the client options, to USER codes.
static const int TYPE_ERROR   = 1;
static const int TYPE_WARNING = 2;
static const int TYPE_INFO    = 3;
static const int FLAG_FATAL   = 8; // Used in combination with TYPE_ERROR.

static const int USER_FATAL   = 4;
static const int USER_ERROR   = 5;
static const int USER_WARNING = 6;
static const int USER_INFO    = 7;


// Generators for diagnostic codes.
template <int Id, bool Fatal = false>
struct GenError {
	static const int Value = (TYPE_ERROR << 28) | Id | (Fatal ? 1 << 31 : 0);
};

template <int Id>
struct GenWarning {
	static const int Value = (TYPE_WARNING << 28) | Id;
};

template <int Id>
struct GenInfo {
	static const int Value = (TYPE_INFO << 28) | Id;
};

template <int Type, int Id>
struct GenAny {
	static const int Value = (Type << 28) | Id;
};


// Provides methods for manipulating diagnostic codes.
struct CodeInfo {
	// Returns the type of the specified code.
	static int GetType(DiagnosticCode code) {
		// Extract the type (last 4 bits).
		return (code & (~((1 << 28) - 1) - (1 << 31))) >> 28;
	}

	// Returns the code without the type part.
	static int GetCode(DiagnosticCode code) {
		return code & ((1 << 28) - 1);
	}

	// Returns 'true' if 'FLAG_FATAL' is set.
	static bool IsFatal(DiagnosticCode code) {
		return (code & (1 << 31)) != 0;
	}

	// Sets the 'FLAG_FATAL' flag.
	static int SetIsFatal(DiagnosticCode code) {
		return code | (1 << 31);
	}

	// Resets the 'FLAG_FATAL' flag.
	static int ResetIsFatal(DiagnosticCode code) {
		return code & ~(1 << 31);
	}

	// Sets the specified type to the code and returns a new code.
	// The 'FLAG_FATAL' flag is lost.
	static int SetType(DiagnosticCode code, DiagnosticCode type) {
		// Extract the ID and combine it with the new type.
		return (type << 28) | (code & ((1 << 28) - 1));
	}
};

}
#endif