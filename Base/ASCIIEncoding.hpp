// ASCIIEncoding.hpp
// Copyright (c) Lup Gratian
//
// Represents an ASCII character encoding of Unicode characters.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_ASCII_ENCODING_HPP
#define PC_BASE_ASCII_ENCODING_HPP

#include "Encoding.hpp"
#include "String.hpp"
#include "DebugValidator.hpp"
#include <cmath>

namespace Base {

class ASCIIEncoding : public Encoding {
private:
	typedef DebugValidator Validator;

public:
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
		
		charCount = std::min(charCount, byteCount);
		written = charCount;
		char* bytePtr = (char*)bytes;
		string::TChar* charPtr = (string::TChar*)chars;

		// Align the pointer to 8 bytes.
		while((charCount > 0) && (((size_t)charPtr & 0x07) != 0)) {
			*bytePtr++ = (char)*charPtr++;
			charCount--;
		}

		// Process 4 characters at a time.
		while(charCount > 8) {
			__int64 data = *(__int64*)charPtr;
			*(bytePtr + 0) = (char)data;
			*(bytePtr + 1) = (char)(data >> 16);
			*(bytePtr + 2) = (char)(data >> 32);
			*(bytePtr + 3) = (char)(data >> 48);

			charPtr += 4;
			bytePtr += 4;
			charCount -= 4;
		}

		// Process the remaining characters.
		while(charCount > 0) {
			*bytePtr++ = (char)*charPtr++;
			charCount--;
		}

		return (void*)chars;
	}

	virtual __int64 GetCharCount(void* bytes, __int64 count) {
		Validator::IsNotNull(bytes);
		return count / sizeof(string::TChar);
	}

	virtual string::TChar* GetChars(void* bytes, __int64 byteCount, string::TChar* chars, 
									__int64 charCount, __int64& written) {
		Validator::IsNotNull(bytes);
		Validator::IsLargerOrEqual(charCount, byteCount);
		byteCount = std::min(byteCount, charCount);
		written = byteCount;

		char* bytePtr = (char*)bytes;
		string::TChar* charPtr = (string::TChar*)chars;

		// Align the pointer to 8 bytes.
		while((byteCount > 0) && (((size_t)bytePtr & 0x07) != 0)) {
			*charPtr++ = *bytePtr++;
			byteCount--;
		}

		// Process 8 bytes at a time.
		while(byteCount > 8) {
			__int64 data = *(__int64*)bytePtr;
			*(charPtr + 0) = (data) & 0xFF;
			*(charPtr + 1) = (data >> 8) & 0xFF;
			*(charPtr + 2) = (data >> 16) & 0xFF;
			*(charPtr + 3) = (data >> 24) & 0xFF;
			*(charPtr + 4) = (data >> 32) & 0xFF;
			*(charPtr + 5) = (data >> 40) & 0xFF;
			*(charPtr + 6) = (data >> 48) & 0xFF;
			*(charPtr + 7) = data >> 56;

			charPtr += 8;
			bytePtr += 8;
			byteCount -= 8;
		}

		// Process the remaining data.
		while(byteCount > 0) {
			*charPtr++ = *bytePtr++;
			byteCount--;
		}

		return chars;
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