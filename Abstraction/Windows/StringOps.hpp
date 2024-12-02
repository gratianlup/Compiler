// StringOps.hpp
// Copyright (c) Lup Gratian
//
// Defines operations that can be performed on a character array (string).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_STRING_OPS_HPP
#define PC_ABSTRACTION_STRING_OPS_HPP

#include <cstdlib>
#include <wchar.h>
#include <TCHAR.h>
#include "StringOpsConst.hpp"

namespace Abstraction {

template <class T>
class StringOps : public StringOpsConst {
public:
	static int RequiredSpace(int length) {
		return length + 1; // nullptr-terminated string.
	}

	static void ResetString(T* s) {
		s[0] = 0;
	}

	static int ToInt(const T* value) {
		if(value == nullptr) return 0;
		return (int)(*value);
	}

	static T* Next(T* current) {
		return (current + 1);
	}

	static int Compare(const T* a, int lengthA, 
					   const T* b, int lengthB, int cmpLength) {
		return _tcsncmp(a, b, cmpLength);
	}

	static int CompareIgnoreCase(const T* a, int lengthA,
								 const T* b, int lengthB, int cmpLength) {
		return _tcsnicmp(a, b, cmpLength);
	}

	static bool Equal(const T& a, const T& b) {
		return (a == b);
	}

	static int Length(const T* buffer) {
		return (int)_tcsclen(buffer);
	}

	static int Length(const char* buffer) {
		return (int)strlen(buffer);
	}

	static T* Copy(T* dest, int dstLength, const T* source, int srcLength) {
		_tcsncpy(dest, source, srcLength);
		dest[srcLength] = 0;
		return dest;
	}

	static T* Copy(T* dest, int dstLength, const char* source, int srcLength) {
		for(int i = 0; i < srcLength; i++) {
			dest[i] = T(source[i]);
		}

		dest[srcLength] = 0;
		return dest;
	}

	static T* Concat(T* dest, int dstLength, const T* source, int srcLength) {
		return _tcsncat(dest, source, srcLength);
	}

	static T* Concat(T* dest, int dstLength, T value) {
		dest[dstLength    ] = value;
		dest[dstLength + 1] = 0;
		return dest;
	}

	static T* Find(T* buffer, int length, const T& value) {
		return _tcschr(buffer, value);
	}

	static T* FindString(T* buffer, int strLength, const T* value, int valLength) {
		return _tcsstr(buffer, value);
	}

	static T* ToLower(T* buffer, int length) {
		return _tcslwr(buffer);
	}

	static T* ToUpper(T* buffer, int length) {
		return _tcsupr(buffer);
	}

	static T* Fill(T* buffer, int strLength, const T& value, int fillLength) {
		while(fillLength) {
			*buffer++ = value;
			fillLength--;
		}

		*buffer = 0;
		return buffer;
	}

	static int Format(T* buffer, int strLength, const T* format, va_list arguments) {
		int result = -1;
		if(strLength > 0) buffer[strLength - 1] = 0;

#ifdef _UNICODE
		result = _vsnwprintf(buffer, strLength - 1, format, arguments);
#else
		result = _vsnprintf(buffer, strLength - 1, format, arguments);
#endif
		if(result != -1) buffer[result] = 0;
		return result;
	}

	static int NewLineLength() {
		return 1; // Windows-style new line terminators.
	}

	static void ConcatNewLine(T* dest, int dstLength) {
		dest[dstLength + 0] = '\n';
		dest[dstLength + 1] = 0;
	}
};

} // namespace Abstraction
#endif