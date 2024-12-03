// LexemeParsers.hpp
// Copyright (c) Lup Gratian
//
// Defines classes to parse lexemes for numbers, strings and character constants.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_LEXEME_PARSERS_HPP
#define PC_LEXING_LEXEME_PARSERS_HPP

#include "Token.hpp"
#include "../Common/TargetData.hpp"
#include "../Common/Diagnostic.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuffer.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/Stack.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/Log.hpp"
#include <cstdlib>
using namespace Base;
using namespace Common;

namespace Lexing {

// Represents the type of the lexed integer number.
enum LexemeIntegerType {
	IntType_Char,
	IntType_UChar,
	IntType_WChar,
	IntType_Short,
	IntType_UShort,
	IntType_Int,
	IntType_UInt,
	IntType_Long,
	IntType_ULong,
	IntType_LongLong,
	IntType_ULongLong
};


// Represents the type of the lexed floating number.
enum LexemeFloatingType {
	FloatType_Float,
	FloatType_Double
};

// Contains the result of the number parsing.
class NumberInfo {
private:
	LexemeIntegerType  intType_;
	LexemeFloatingType floatType_;

public:
	union {
		__int64 IntValue;
		double FloatValue;
	};

	bool IsInteger : 1;
	bool IsValid     : 1;
	bool Oveflow   : 1; // 'true' if the number overflows a 64 bit number.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool IsShort()     const { return IsInteger && (intType_ == IntType_Short);     }
	bool IsUShort()    const { return IsInteger && (intType_ == IntType_UShort);    }
	bool IsInt()       const { return IsInteger && (intType_ == IntType_Int);       }
	bool IsUInt()      const { return IsInteger && (intType_ == IntType_UInt);      }
	bool IsLong()      const { return IsInteger && (intType_ == IntType_Long);      }
	bool IsULong()     const { return IsInteger && (intType_ == IntType_ULong);     }
	bool IsLongLong()  const { return IsInteger && (intType_ == IntType_LongLong);  }
	bool IsULongLong() const { return IsInteger && (intType_ == IntType_ULongLong); }
	
	bool IsFloat()  const { return !IsInteger && (floatType_ == FloatType_Float);  }
	bool IsDouble() const { return !IsInteger && (floatType_ == FloatType_Double); }

	bool IsFloating() const {
		return IsFloat() || IsDouble();
	}

	bool IsSigned() const {
		return IsShort() || IsInt() || IsLong() || IsLongLong();
	}

	bool IsUnsigned() const {
		return IsUShort() || IsUInt() || IsULong() || IsULongLong();
	}

	void SetIntType(LexemeIntegerType value) {
		intType_ = value;
	}

	void SetFloatType(LexemeFloatingType value) {
		floatType_ = value;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const NumberInfo& other) const {
		if(IsValid != other.IsValid) return false;
		if(IsInteger != other.IsInteger) return false;

		if(IsInteger) {
			if(intType_ != other.intType_) return false;
			return IntValue == other.IntValue;
		}
		else {
			if(floatType_ != other.floatType_) return false;
			return FloatValue == other.FloatValue;
		}
		return true;
	}

	bool operator!= (const NumberInfo& other) const {
		return !operator== (other);
	}
};


// Contains the result of the character constant parsing.
class CharInfo {
public:
	bool IsValid;
	wchar_t Value;
	bool IsWide;
	bool MultipleChars; // 'true' if something like 'ab'.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const CharInfo& other) const {
		if(IsValid != other.IsValid) return false;
		if(IsWide != other.IsWide) return false;

		return Value == other.Value;
	}

	bool operator!= (const CharInfo& other) const {
		return !operator== (other);
	}
};


// Contains the result of the character constant parsing.
class StringInfo {
public:
	bool IsValid;
	StringBuffer Value;
	bool IsWide;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StringInfo() : IsValid(true), IsWide(false) {}

	StringInfo(int capacity) : Value(capacity), IsValid(true), IsWide(false) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const StringInfo& other) const {
		if(IsValid != other.IsValid) return false;
		if(IsWide != other.IsWide) return false;

		return Value == other.Value;
	}

	bool operator!= (const StringInfo& other) const {
		return !operator== (other);
	}
};


// Tries to parse a number token (both integer and floating).
//
// integer-constant:
//		decimal-constant integer-suffix-opt
//		octal-constant integer-suffix-opt
//		hexadecimal-constant integer-suffix-opt
//
// decimal-constant:
//		nonzero-digit
//		decimal-constant digit
//
// octal-constant: 
//		0
//		octal-constant octal-digit
//
// hexadecimal-constant:
//		hexadecimal-prefix hexadecimal-digit
//		hexadecimal-constant hexadecimal-digit
//
// hexadecimal-prefix: one of 0x 0X
//
// unsigned-suffix: one of u U
// long-suffix: one of l L
// long-long-suffix: one of ll LL
class NumberParser {
private:
	enum Radix {
		Radix_10    = 10,
		Radix_Octal = 8,
		Radix_Hex   = 16
	};

	typedef string::TChar T;
	friend class CharParser;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static _locale_t floatLocale_; // Used when converting string -> double.
	Diagnostic* diag_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static bool IsDigit(T value) {
		return (value >= '0') && (value <= '9');
	}

	static bool IsHex(T value) {
		return ((value >= '0') && (value <= '9')) ||
			   ((value >= 'a') && (value <= 'f')) ||
			   ((value >= 'A') && (value <= 'F'));
	}

	static bool IsOctal(T value) {
		return (value >= '0') && (value <= '7');
	}

	static bool IsExponent(T value) {
		return (value == 'e') || (value == 'E');
	}

	static bool IsBinExponent(T value) {
		return (value == 'p') || (value == 'P');
	}

	static bool IsSign(T value) {
		return (value == '+') || (value == '-');
	}

	static int DigitValue(T ch) {
		if((ch >= '0') && (ch <= '9')) return ch - '0';
		else if((ch >= 'a') && (ch <= 'f')) return (ch - 'a') + 10;
		else if((ch >= 'A') && (ch <= 'F')) return (ch - 'A') + 10;

		return 0; // Should not be reached.
	}

	// Returns the bits/digit for the largest value in the specified radix.
	int BitsPerDigit(Radix radix) {
		switch(radix) {
			case Radix_10:    return 4;
			case Radix_Octal: return 3;
			case Radix_Hex:   return 4;
		}

		DebugValidator::Unreachable(); // Invalid.
		return 0;
	}

	// Skips over the exponent part of the number.
	void SkipExponent(T*& data, LocationInfo location);

	// Verifies if the given number of digits overflows the largest integer type.
	bool IntegerFitts(int digits, Radix radix) {
		return (digits * BitsPerDigit(radix)) <= 64;
	}

	// Skips all digits that can form a base 10 number. 
	// Returns the number of skipped digits.
	int SkipDigits(T*& value) {
		int ct = 0;

		while(IsDigit(*value)) {
			ct++;
			value++;
		}

		return ct;
	}

	// Skips all digits that can form a hexadecimal number.
	// Returns the number of skipped digits.
	int SkipHexDigits(T*& value) {
		int ct = 0;

		while(IsHex(*value)) {
			ct++;
			value++;
		}

		return ct;
	}

	// Skips all digits that can form an octal number.
	// Returns the number of skipped digits.
	int SkipOctalDigits(T*& value) {
		int ct = 0;

		while(IsHex(*value)) {
			ct++;
			value++;
		}

		return ct;
	}

	// Gets info about a number that starts with '0'. It may be a hexadecimal
	// (0x, 0X), octal or floating point number.
	int GetNumberInfo(T*& data, string& text, NumberInfo& info, bool& hasExponent, 
					  bool& hasPeriod, Radix& radix, LocationInfo location);

	// Converts the specified range of characters to a integer value.
	NumberInfo ToInteger(NumberInfo& info, Radix radix, T* data, int digits,
						 LocationInfo location);

	// Converts the specified range of characters to a floating value.
	NumberInfo ToFloating(NumberInfo& info, Radix radix, T* data, int digits,
						  LocationInfo location);

public:
	NumberParser(Diagnostic* diag) : diag_(diag) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Parses the specified number token. Handles both integer and floating values.
	// Values are considered by default to be integers.
	NumberInfo Parse(Token& token);
};


// Tries to parse a character token.
//
// character-constant:
//		'  c-char-sequence '
//		L' c-char-sequence '
//
// c-char-sequence:
//		c-char
//		c-char-sequence c-char
//
// c-char:
//		any member of the source character set except
//			the single-quote ', backslash \, or new-line character
//		escape-sequence
// 
// escape-sequence:
//		simple-escape-sequence
//		octal-escape-sequence
//		hexadecimal-escape-sequence
//		universal-character-name
// 
// simple-escape-sequence: one of
//		\' \" \? \\
//		\a \b \f \n \r \t \v
// 
// octal-escape-sequence:
//		\ octal-digit
//		\ octal-digit octal-digit
//		\ octal-digit octal-digit octal-digit
//
// hexadecimal-escape-sequence:
//		\x hexadecimal-digit
//		hexadecimal-escape-sequence hexadecimal-digit
class CharParser {
private:
	friend class StringParser;

	const TargetData* target_;
	Diagnostic* diag_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static int HandleEscape(wchar_t*& data, wchar_t* end, bool isWide,
							Diagnostic* diag, LocationInfo location);

public:
	CharParser(Diagnostic* diag) : diag_(diag) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	CharInfo Parse(Token& token);
};


// Tries to parse a string token.
//
// string-literal:
// " s-char-sequence-opt "
// L" s-char-sequence-opt "
//
// s-char-sequence:
//		s-char
//		s-char-sequence s-char
//
// s-char:
//		any member of the source character set except
//			the double-quote ", backslash \, or new-line character
//		escape-sequence
class StringParser {
private:
	Diagnostic* diag_;
	bool appendTerm_;

public:
    typedef StaticList<Token, 3> TStringList;

	StringParser(Diagnostic* diag, bool appendTerminator = true) : 
			diag_(diag), appendTerm_(appendTerminator) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StringInfo Parse(TStringList& list);
};

} // namespace Lexing
#endif