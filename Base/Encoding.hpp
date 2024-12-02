// Encoding.hpp
// Copyright (c) Lup Gratian
//
// Represents the base class for a character encoding.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_ENCODING_HPP
#define PC_BASE_ENCODING_HPP

#include "String.hpp"

namespace Base {

class Encoding {
public:
	virtual __int64 GetByteCount(string::TChar* chars, __int64 count) = 0;

	virtual __int64 GetByteCount(const string& s) {
		return GetByteCount(s.Chars(), s.Length());
	}

	virtual void* GetBytes(string::TChar *chars, __int64 charCount, void* bytes, 
						   __int64 byteCount, __int64& written) = 0;

	virtual void* GetBytes(const string& s, void* bytes, 
						   __int64 byteCount, __int64& written) {
		return GetBytes(s.Chars(), s.Length(), bytes, byteCount, written);
	}

	virtual __int64 GetCharCount(void* bytes, __int64 count) = 0;
	virtual string::TChar* GetChars(void* bytes, __int64 byteCount, string::TChar* chars, 
								    __int64 charCount, __int64& written) = 0;
	virtual string GetString(void* bytes, __int64 index, __int64 count) = 0;
	virtual __int64 GetPreamble(void* destination) = 0;
};

} // namespace Base
#endif