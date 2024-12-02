// UnicodeEncoding.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_UNICODE_ENCODING_HPP
#define PC_BASE_UNICODE_ENCODING_HPP

#include "Encoding.hpp"
#include "String.hpp"
#include "DebugValidator.hpp"

namespace Base {

class UnicodeEncoding : public Encoding {
private:
	typedef DebugValidator Validator;

	bool bigEndian_;

public:
	UnicodeEncoding() {}
	UnicodeEncoding(bool bigEndian) : bigEndian_(bigEndian) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual __int64 GetByteCount(string::TChar* chars, __int64 count) {
		Validator::IsNotNull(chars);
		return count * sizeof(string::TChar);
	}

	virtual void* GetBytes(string::TChar *chars, __int64 charCount, void* bytes, 
						   __int64 byteCount, __int64& written) {
		Validator::IsNotNull(chars);
		Validator::IsNotNull(bytes);
		Validator::IsLargerOrEqual(byteCount, charCount);
	
		written = charCount * sizeof(string::TChar);
		return (void*)chars;
	}

	virtual __int64 GetCharCount(void* bytes, __int64 count) {
		Validator::IsNotNull(bytes);
		return count / sizeof(string::TChar);
	}

	virtual string::TChar* GetChars(void* bytes, __int64 byteCount, string::TChar* chars, 
									__int64 charCount, __int64& written) {
		Validator::IsNotNull(chars);
		Validator::IsNotNull(bytes);
		Validator::IsLargerOrEqual(charCount, byteCount);
		
		written = byteCount / sizeof(string::TChar);
		return static_cast<string::TChar*>(bytes);
	}

	virtual string GetString(void* bytes, __int64 index, __int64 count) {
		__int64 charCount = count / sizeof(string::TChar);
		__int64 written;
		string temp((int)charCount);
		GetChars((char*)bytes + index, count, temp.Chars(), charCount, written);
		return temp;
	}

	virtual __int64 GetPreamble(void* destination) {
		return 0;
	}
};

} // namespace Base;
#endif