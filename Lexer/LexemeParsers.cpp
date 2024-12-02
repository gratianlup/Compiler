// LexemeParsers.cpp
// Copyright (c) Lup Gratian
//
//  Implementation of LexemeParsers classes.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "LexemeParsers.hpp"
#include <stdlib.h>
#include <locale.h>

namespace Lexing {

// Static members.
_locale_t NumberParser::floatLocale_ = _create_locale(LC_ALL, "C");

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NumberInfo NumberParser::ToInteger(NumberInfo& info, Radix radix, T* data, int digits,
								   LocationInfo location) {
	__int64 rad = (__int64)radix;
	__int64 value = 0;

	// Check if the number certainly fits in the target maximum integer size.
	// If it does don't do any overflow checking (it's faster).
	if(IntegerFitts(digits, radix)) {
		for(int i = 0; i < digits; i++) {
			value = (value * rad) + DigitValue(data[i]);
		}
	}
	else {
		// Check for overflow at each step. If the number has overflowed
		// the new value is smaller than the old one.
		for(int i = 0; i < digits; i++) {
			__int64 oldValue = value;
			value *= rad;

			if(value < oldValue) {
				info.Oveflow = true;
			}
			else {
				value += DigitValue(data[i]);

				if(value < oldValue) {
					info.Oveflow = true;
				}
			}

			if(info.IsValid == false) {
				return info; // The number has overflowed.
			}
		}
	}

	info.IntValue = value;
	return info;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int NumberParser::GetNumberInfo(T*& data, string& text, NumberInfo& info, bool& hasExponent, 
								bool& hasPeriod, Radix& radix, LocationInfo location) {
	// Skip over the '0'.
	int skip = 0;
	radix = Radix_Octal; // Presume this is an octal number.
	T* end = text.Chars() + text.Length();
	data++;

	// Check if this is a Hex value.
	if((data < (end - 1)) &&
		((*data == 'x') || (*data == 'X')) &&
		(IsHex(data[1]) || (data[1] == '.'))) { // '.' for hex floating values.
		radix = Radix_Hex;
		data++;
		skip = 2; // Skip over '0x'.
		SkipHexDigits(data);

		// This could be a hex floating value.
		if((data != end) && (*data == '.')) {
			hasPeriod = true;
			data++;
			SkipHexDigits(data);
		}

		// Check if a binary exponent (p/P) is present.
		if(IsBinExponent(*data)) {
			hasExponent = true;

			if((SkipHexDigits(data) == 0) && diag_) {
				// Report there are no digits after the exponent character.
				diag_->Report(Error::EXPONENT_MISSING_DIGITS)<<location;
			}
		}

		return skip; // Done.
	}

	// Else the number is octal. Make sure no character represents
	// a digit above 7. If it does this could be a floating number (01.23).
	if(SkipOctalDigits(data) != 0) {
		skip = 1;
	}

	if(data == end) {
		return skip; // Done, the number is octal.
	}

	// If we stopped at a base 10 digit or at '.' this is a floating number.
	if(IsDigit(*data) || (*data == '.')) {
		SkipDigits(data);

		if((*data == '.') || IsExponent(*data)) {
			radix = Radix_10;
			hasExponent = true;
		}
	}

	return skip;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NumberInfo NumberParser::ToFloating(NumberInfo& info, Radix radix, T* data, int digits,
									LocationInfo location) {
	info.FloatValue = _wcstod_l(data, nullptr, floatLocale_);
	info.IsValid = (info.FloatValue != HUGE_VAL) && (info.FloatValue != -HUGE_VAL);

	return info;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void NumberParser::SkipExponent(T*& data, LocationInfo location) {
	
	data++; // Skip to next character.

	// Check if there is a sign, and then skip over digits.
	if(IsSign(*data)) {
		data++;
	}

	if((SkipDigits(data) == 0) && diag_) {
		// Report there are no digits after the exponent character.
		diag_->Report(Error::EXPONENT_MISSING_DIGITS)<<location;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
NumberInfo NumberParser::Parse(Token& token) {
	DebugValidator::IsTrue(token.IsNumber());
	NumberInfo info;
	info.IsValid = true;    // Presume the number is valid.
	info.Oveflow = false; // Presume no overflow will be found.

	Radix radix;
	bool hasExponent = false;
	bool hasPeriod = false;
	bool scanBody = true;
	int skip = 0; // The number of characters to skip from the beginning (used for Hex/Octal).
	string& text = token.NumberValue()->Number;
	T* data = text.Chars();

	if(*data == '0') {
		// The number is not in base 10 or of the form 0.xyz.
		skip = GetNumberInfo(data, text, info, hasExponent, hasPeriod, 
							 radix, token.Location());
		scanBody = radix == Radix_10; // If it's actually a floating number, scan the rest.
	}

	if(scanBody) {
		// This is a standard number.
		// Skip the digits to the first character that is not a digit.
		// Using this character we can find more about the number.
		radix = Radix_10;
		int skipped = SkipDigits(data);

		if(skipped == text.Length()) {
			// Reached the end -> this is an integer.
			info.IsInteger = true;
			info.SetIntType(IntType_Int);
			return ToInteger(info, radix, text.Chars(),
							 (int)(data - text.Chars()), token.Location());
		}
		else {
			// See if we stopped at an exponent ('e' or 'E').
			// This handles numbers like 123e456.
			if(IsExponent(*data)) {
				hasExponent = true;
				SkipExponent(data, token.Location());
			}

            // Check if we have a Hex character and report an error.
            // An exception is when we have 'f' or 'F', because they represent
            // the suffix of a floating-point number.
			if(IsHex(*data) && 
               ((hasExponent || hasPeriod) && ((*data == 'f') || (*data == 'F'))) == false) {
				// Hex characters are not valid if the number doesn't start with 0.
				if(diag_) diag_->Report(Error::HEX_DIGIT_IN_BASE_10)<<token.Location();
				info.IsValid = false;
				return info;
			}
			else if(*data == '.') {
				hasPeriod = true;
				data++; // Skip to next character.
				SkipDigits(data);

				// The exponent could be found now. Handles numbers like '123.4e56'
				if((hasExponent == false) && IsExponent(*data)) {
					hasExponent = true;
					SkipExponent(data, token.Location());
				}
			}
		}
	}

	// Check if the number has a suffix (U, LL, etc.) (C99:6.4.4.1).
	// At the end all data must be consumed, else there is something wrong.
	bool isFloating = hasPeriod || hasExponent;
	bool isUnsigned = false;
	bool isLong = false;
	bool isLongLong = false;
	bool isFloat = false; // Consider the number 'float' instead of the default 'double'.
	T* beforeSuffix = data;
	T* end = text.Chars() + text.Length();

	while(data < end) { // Loop because of ul, ll, ull, etc.
		switch(*data) { // C99:6.4.1.1.5
			case 'u': case 'U': {
				// Unsigned number. Not compatible with 'isFloating'.
				if(isFloating) break;
				if(isUnsigned) break; // Suffix repeated, not valid.

				isUnsigned = true;
				data++;
				continue;
			}
			case 'l': case 'L': {
				// Unsigned number. Not compatible with 'isFloating'.
				if(isFloating) break;
					
				if(isLongLong) break; // Suffix repeated, not valid.
				else if(isLong) {
					// Convert from 'long' to 'long long'.
					isLong = false;
					isLongLong = true;
				}
				else isLong = true; // Set 'long'.

				data++;
				continue;
			}
			case 'f': case 'F': {
				if(isFloating == false) break; // Not valid for integers.
				if(isFloat) break; // Suffix repeated, not valid.

				isFloat = true;
				data++;
				continue;
			}
		}

		break; // Any other character is not valid.
	}

	if(data != end) {
		if(diag_) diag_->Report(Error::NUMBER_SUFFIX_INVALID)<<token.Location();
		info.IsValid = false;
		return info;
	}
	else if(isFloating == false) {
		// Convert the flags to a 'IntegerKind' enumeration.
		info.IsInteger = true;

		if(isLongLong) {
			if(isUnsigned) info.SetIntType(IntType_ULongLong);
			else info.SetIntType(IntType_LongLong);
		}
		else if(isLong) {
			if(isUnsigned) info.SetIntType(IntType_ULong);
			else info.SetIntType(IntType_Long);
		}
		else {
			if(isUnsigned) info.SetIntType(IntType_UInt);
			else info.SetIntType(IntType_Int);
		}

		return ToInteger(info, radix, text.Chars() + skip,
						 (int)(beforeSuffix - text.Chars() - skip), token.Location());
	}
	else {
		info.IsInteger = false;

		if(isFloat) info.SetFloatType(FloatType_Float);
		else info.SetFloatType(FloatType_Double);

		return ToFloating(info, radix, text.Chars() + skip, 
						  (int)(beforeSuffix - text.Chars() - skip), token.Location());
	}	
}

// ######################################################################################
// CharParser
// ######################################################################################
int CharParser::HandleEscape(wchar_t*& data, wchar_t* end, bool isWide,
							 Diagnostic* diag, LocationInfo location) {
	// An escape sequence can be either a simple one (\n, \t, etc.), 
	// or made from octal or hexadecimal digits.
	data++; // Skip over /
	wchar_t ch = *data++;
	int result = 0;

	switch(ch) {
		case '\\': { result = ch;   break; } // The same.
		case '\'': { result = ch;	break; } // The same.
		case '"':  { result = ch;	break; } // The same.
		case '?':  { result = ch;	break; } // The same.
		case 'a':  { result = '\a';	break; }
		case 'b':  { result = '\b';	break; }
		case 'f':  { result = '\f';	break; }
		case 'n':  { result = '\n';	break; }
		case 'r':  { result = '\r';	break; }
		case 't':  { result = '\t';	break; }
		case 'v':  { result = '\v';	break; }
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7':{
			// An octal escape sequence. A maximum of 3 octal digits are allowed.
			int digits = 0;
			result =  NumberParser::DigitValue(ch);

			while((data < end) && (digits < 3) && NumberParser::IsOctal(*data)) {
				result = (result * 8) + NumberParser::DigitValue(*data);
				digits++;
				data++;
			}

			break;
		}
		case 'x': { // Only 'x' is allowed, 'X' is not (C99:6.4.4.4).
			// It '/x' shall be followed be at least a hex digit character.
			if(NumberParser::IsHex(*data) == false) {
				if(diag) diag->Report(Error::HEX_CHAR_NO_DIGITS)<<location;
				break;
			}

			while((data < end) && NumberParser::IsHex(*data)) {
				result = (result * 16) + NumberParser::DigitValue(*data);
				data++;
			}
			
			break;
		}
		default: {
			// Other escapes are not allowed (just warn).
			if(diag) diag->Report(Warning::ESCAPE_SEQUENCE_UNKNOWN)<<location;
			break;
		}
	}

	// Warn if the constant overflows.
	int charSize = isWide ? 16 : 8;
	int maxValue = (1 << charSize) - 1;

	if((result > maxValue) || (result < 0)) {
		diag->Report(Warning::CHARACTER_OVERFLOW)<<location;
	}

	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CharInfo CharParser::Parse(Token& token) {
	DebugValidator::IsTrue(token.IsChar());
	CharInfo info;
	StringData& data = *token.StringValue();
	wchar_t* dataPtr = data.Value.Chars();
	wchar_t* dataEnd = dataPtr + data.Value.Length();

	info.Value = 0;
	info.IsValid = dataPtr != dataEnd;
	if(info.IsValid == false) {
		return info; // No characters in constant.
	}

	// Used to truncate the value.
	int charSize = data.IsWide ? 16 : 8;
	int maxValue = (1 << charSize) - 1;
	int charCt = 0;
	info.MultipleChars = false;

	// Process each character. If it's an escape sequence handle the situation.
	while(dataPtr < dataEnd) {
		wchar_t ch = *dataPtr;
		if(ch == '\\') {
			// dataPtr++ not needed because 'HandleEscape' does it.
			charCt++;

			if(dataPtr == dataEnd) {
				// Invalid escape sequence.
				info.IsValid = false;
				return info;
			}

			int value = HandleEscape(dataPtr, dataEnd, data.IsWide, 
									 diag_, token.Location());

			// Use only the last part of a multi-character sequence.
			if(data.IsWide) {
				info.Value = (info.Value << 8) + (value & maxValue);
			}
			else info.Value = value & maxValue;
		}
		else {
			if(data.IsWide) {
				info.Value = (info.Value << 8) + ch;
			}
			else info.Value = ch;

			dataPtr++;
			charCt++;
		}
	}
	
	info.IsWide = data.IsWide;
	info.MultipleChars = charCt > 1;
	return info;
}

// ######################################################################################
// StringParser
// ######################################################################################
StringInfo StringParser::Parse(TStringList& list) {
	// Consecutive strings are concatenated (C99:6.4.5.4).
	// "abc" "xyz" " "123" -> "abcxyz123"
	StringInfo info(list[0].Length());

	for(int i = 0; i < list.Count(); i++) {
		Token& token = list[i];
		StringData* data = token.StringValue();
		DebugValidator::IsTrue(token.IsString());

		// If any of the string is wide the result will be wide too.
		info.IsWide |= data->IsWide;

		// If we know that the string has no escaped sequences we append
		// it without doing any more checks.
		if(data->HasEscaped == false) {
			info.Value.Append(data->Value.Chars(), data->Value.Length());
		}
		else {
			wchar_t* dataPtr = data->Value.Chars();
			wchar_t* dataEnd = dataPtr + data->Value.Length();

			while(dataPtr < dataEnd) {
				wchar_t ch = *dataPtr;
				if(ch == '\\') {
					// An escape sequence begins here.
					int value = CharParser::HandleEscape(dataPtr, dataEnd, data->IsWide,
														 diag_, token.Location());
					info.Value.Append((wchar_t)value);
				}
				else {
					info.Value.Append(ch);
					dataPtr++;
				}
			}
		}
	}

	// Append the null-terminator, if requested.
	if(appendTerm_) info.Value.Append('\0');
	return info;
}

} // namespace Lexing