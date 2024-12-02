// String.hpp
// Copyright (c) Lup Gratian
//
// Provides atomic operations for variables that are shared by multiple threads.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STRING_HPP
#define PC_BASE_STRING_HPP

#include "../Abstraction/PlatformString.hpp"
#include "DebugValidator.hpp"
#include "LocalPointer.hpp"
#include "List.hpp"
#include "DefaultComparer.hpp"
#include <cstdlib>
#include <cstdarg>
#include <memory>
#include <new>
#include <algorithm>

namespace Base {

#ifdef _UNICODE
	#ifndef _T
		#define _T(a) L##a
	#endif
#else
	#ifndef _T
		#define _T(a) a
	#endif
#endif


// Specifies whether applicable Split method overloads include or omit
// empty substrings from the return value.
enum class StringSplitOptions {
	None,
	RemoveEmptyEntries
};


template <class T, class Ops = Abstraction::StringOps<T>, 
		  class Validator = DebugValidator>
class String {
private:
	typedef typename String<T, Ops, Validator> TString;

	T* data_;
	int length_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Invalidates the string and deallocate the used memory.
	void ReleaseString() { // Can't throw.
		if(data_) {
			delete[] data_;
			data_ = nullptr;
			length_ = 0;
		}
	}

	// Allocates memory for 'length' characters. Releases the previously allocated memory.
	void AllocateString(int length) {
		ReleaseString(); // Release the previous data.
		data_ = new T[Ops::RequiredSpace(length)];
		Ops::ResetString(data_); // Set the terminator (if any).
	}

	// Copies 'length' characters from the given string	starting with 'startIndex' position.
	void CopyString(const T* string, int startIndex, int length) {
		Ops::Copy(data_, length, &string[startIndex], length);
		length_ = length;
	}

	// Copies 'length' chars from the given string	starting with 'startIndex' position.
	void CopyString(const char* string, int startIndex, int length) {
		Ops::Copy(data_, length, &string[startIndex], length);
		length_ = length;
	}

	// Copies an entire string.
	void CopyString(const T* string) {
		CopyString(string, 0, Ops::Length(string));
	}

	// Reallocates the existing string so that 'length' characters can be used.
	void Resize(int newLength, int oldLength) {
		Validator::IsLargerOrEqual(newLength, Ops::RequiredSpace(length_));
		T* temp = new T[Ops::RequiredSpace(newLength)];
		Ops::Copy(temp, newLength, data_, oldLength);

		delete[] data_;
		data_ = temp;
		length_ = newLength;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Makes a list with the positions where char separators can be found.
	int GetSeparatorPositions(const T* separators, int sepNumber, 
							  int* sepPositions) const {
		int positionCount = 0;
		T* currenT = data_;
		int currentPosition = 0;

		// Find the separator positions.
		while(currenT && (currentPosition < length_)) {
			for(int sepIndex = 0; sepIndex < sepNumber; sepIndex++) {
				if(Ops::Equal(*currenT, separators[sepIndex])) {
					sepPositions[positionCount++] = currentPosition;
					break;
				}
			}

			currenT = Ops::Next(currenT);
			currentPosition++;
		}

		return positionCount;  
	}
	
	// Makes a list with the positions where string separators can be found.
	int GetSeparatorPositions(const TString* separators, int sepNumber, 
							  int* sepPositions, int* sepLengths) const {
		int positionCount = 0;
		int currentPosition = 0;
		T* currenT = data_;

		// Find the separator positions.
		while((currenT) && (currentPosition < length_)) {
			for(int sepIndex = 0; sepIndex < sepNumber; sepIndex++) {
				if(separators[sepIndex].length_ == 0) {
					continue; // Skip empty separators.
				}

				// Compare string with separator
				// We don't compare the whole string if the first char doesn't match.
				if(Ops::Equal(*currenT, separators[sepIndex].data_[0]) &&
					Ops::Compare(const_cast<const T*>(currenT), length_, 
					separators[sepIndex].data_, 
					separators[sepIndex].length_,
					separators[sepIndex].length_) == 0) {
						// Separator found!
						sepPositions[positionCount] = currentPosition;
						sepLengths[positionCount++] = separators[sepIndex].length_;
						currentPosition += separators[sepIndex].length_ - 1;
						currenT += separators[sepIndex].length_ - 1;
						break;
				}
			}

			currenT = Ops::Next(currenT);
			currentPosition++;
		}

		return positionCount;
	}

	// Actual implementation of the split algorithm.
	void SplitImpl(List<TString>& substrList, int* sepPositions, 
				   int* sepLength, int sepCount, int positionCount, 
				   StringSplitOptions options, int maxSubstrings) const {
		int substringCount = 0;

		for(int sepIndex = 0; sepIndex < positionCount;sepIndex++) {
			if((sepIndex >= 1) && (sepPositions[sepIndex] - sepPositions[sepIndex - 1] == 1)) {
				if(options == StringSplitOptions::None) {
					// Allow empty substrings.
					substrList.Add(TString());
					substringCount++;
				}
			}
			else if(sepIndex == 0) {
				if(sepPositions[0] != 0) {
					// First substring.
					substrList.Add(Substring(0, sepPositions[sepIndex]));
					substringCount++;
				}
			}
			else {
				if(sepCount > 0) {
					// String-based split.
					substrList.Add(Substring(sepPositions[sepIndex - 1] + 
											 sepLength[sepIndex - 1],   // Start index.
											 sepPositions[sepIndex] - 
											 sepPositions[sepIndex - 1] -
											 sepLength[sepIndex - 1])); // Length.
					substringCount++;
				}
				else {
					// Char-based split.
					substrList.Add(Substring(sepPositions[sepIndex - 1] + 1,
											 sepPositions[sepIndex] - 
											 sepPositions[sepIndex - 1] - 1));
					substringCount++;
				}
			}

			// Stop if the maximum number of substrings is reached.
			if(substringCount == maxSubstrings) {
				break;
			}
		}

		// Last substring.
		if(positionCount > 0 && (substringCount != maxSubstrings)) {
			if(sepLength) {
				substrList.Add(Substring(sepPositions[positionCount - 1] + // Start index
										 sepLength[positionCount - 1],
										 length_ - sepPositions[positionCount - 1] -
										 sepLength[positionCount - 1]));   // Length
			}
			else {
				substrList.Add(Substring(sepPositions[positionCount - 1] + 1,
										 length_ - sepPositions[positionCount - 1] - 1));
			}
		}

		substrList.TrimExcess(); // Trim the list.
	}

public:
	typedef T TChar;
	static const int INVALID_INDEX = -1;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes an empty string.
	String() : data_(nullptr), length_(0) {
		AllocateString(0);
	}
	
	// Initializes an empty string with 'length' characters allocated.
	explicit String(int length) : data_(nullptr), length_(0) {
		AllocateString(length);
	}

	// Initializes the string as a copy of the given string.
	String(const TString& string) : data_(nullptr), length_(0) {
		// Make a deep copy.
		AllocateString(string.length_);
		CopyString(string.data_);
	}

	// Initializes the string with the given C-style array.	
	String(const T* string) : data_(nullptr), length_(0) {
		int length = Ops::Length(string);
		AllocateString(length);
		CopyString(string, 0, length);
	}

	String(const char* string) : data_(nullptr), length_(0) {
		int length = Ops::Length(string);
		AllocateString(length);
		CopyString(string, 0, length);
	}
	
	// Initializes the string with the given C-style array, using only the first 'length' characters.	
	String(const T* string, int length) : data_(nullptr), length_(length) {
		Validator::IsNotNull(string);
		AllocateString(length);
		CopyString(string, 0, length);
	}

	// Initializes the string with the given C-style array, using only the first 'length' characters
	// starting with the 'startIndex' position.
	String(const T* string, int startIndex, int length) : data_(nullptr), length_(length) {
		Validator::IsNotNull(string);
		AllocateString(length);
		CopyString(string, startIndex, length);
	}

	// Fills the string with 'length' given character values.	
	String(T value, int length = 1) : data_(nullptr) {
		AllocateString(Ops::RequiredSpace(length));
		Ops::Fill(data_, length, value, length);
		length_ = length;
	}

	// Copy with move semantics.
	String(TString&& other) : data_(other.data_), length_(other.length_) {
		other.data_ = nullptr;
		other.length_ = 0;
	}

	~String() {
		ReleaseString();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Compares 'length' characters from the two given strings,	starting at positions
	// 'indexA' and 'indexB'. Case-insensitive comparison if performed if 'ignoreCase' is true.
	static int Compare(const TString& strA, int indexA, 
					   const TString& strB, int indexB, 
					   int length, bool ignoreCase) {
		// Use proper method when 'ignoreCase' is enabled.
		if(ignoreCase) {
			return Ops::CompareIgnoreCase(&strA.data_[indexA], strA.length_,
										  &strB.data_[indexB], strB.length_, length);
		}
		else {
			return Ops::Compare(&strA.data_[indexA], strA.length_,
								&strB.data_[indexB],	strB.length_, length);
		}
	}

	// Compares 'length' characters from the two given strings,	starting at positions
	// 'indexA' and 'indexB'. Case-sensitive comparison is performed.	
	static int Compare(const TString& strA, int indexA, 
					   const TString& strB, int indexB, int length) {
		return Compare(strA, indexA, strB, indexB, length);
	}

	
	// Compares the two given strings. Case-insensitive comparison 
	// is performed if 'ignoreCase' is true.
	static int Compare(const TString& strA, const TString& strB, bool ignoreCase) {
		// Use proper method when 'ignoreCase' is enabled.
		if(ignoreCase) {
			return Ops::CompareIgnoreCase(strA.data_, strA.length_,
										  strB.data_, strB.length_,
										  std::max(strA.length_, strB.length_));
		}
		else {
			return Ops::Compare(strA.data_, strA.length_,
								strB.data_, strB.length_,
								std::max(strA.length_, strB.length_));
		}
	}

	// Compares the two given strings. Case-sensitive comparison is performed.
	static int Compare(const TString& strA, const TString& strB) {
		return Compare(strA, strB, false);
	}
	
	// Compares the integer representation of 'length' characters  from the two given strings,
	// starting at positions 'indexA' and 'indexB'.
	static int CompareOrdinal(const TString& strA, int indexA, 
							  const TString& strB, int indexB, int length) {
		// Compare every character.
		T* a = &strA.data_[indexA];
		T* b = &strB.data_[indexB];
		int count = 0;

		while(a && (count < length) &&
			   (Ops::ToInt(a) == Ops::ToInt(b))) {
			a = Ops::Next(a);
			b = Ops::Next(b);
			count++;
		}

		return Ops::ToInt(a) - Ops::ToInt(b);
	}

	// Compares the integer representation of the two given strings.
	static int CompareOrdinal(const TString& strA, const TString& strB) {
		return CompareOrdinal(strA, 0, strB, 0, std::max(strA.length_, strB.Length()));
	}

	// Returns a value indicating whether the two given strings are the same.
	static bool Equals(const TString& strA, const TString& strB) {
		return Compare(strA, strB) == 0;
	}
	
	// Returns a value indicating whether this string is the same with the given one.	
	bool Equals(const TString& string) const {
		return Equals(*this, string);
	}

	// Returns a value indicating whether this string starts with the specified string.
	// Case-insensitive comparison if performed if 'ignoreCase' is true.
	bool StartsWith(const TString& string, bool ignoreCase) const {
		if(IsEmpty(string)) {
            return true; // If the given string is empty, it matches.
        }
		if(IsEmpty(*this)) {
            return false; // If this string is empty, it cannot match.
        }

		return Compare(*this, 0, string, 0, string.Length(), ignoreCase) == 0;
	}
	
	// Returns a value indicating whether this string starts with the specified string.
	// Case-sensitive comparison if performed.
	bool StartsWith(const TString& string) const {
		return StartsWith(string, false);
	}
	
	// Returns a value indicating whether this string ends with the specified string.
	// Case-insensitive comparison if performed if 'ignoreCase' is true.
	bool EndsWith(const TString& string, bool ignoreCase) const {
		if(IsEmpty(string)) {
            return true; // If the given string is empty, it matches.
        }
		
        // If the length of this string is smaller of that of the given one, it cannot match.
		if(Length() < string.Length()) {
            return false;
        }

		return Compare(*this, Length() - string.Length(), string, 0, 
			           string.Length(), ignoreCase) == 0;
	}

	// Returns a value indicating whether this string ends with the specified string.
	// Case-sensitive comparison if performed.	
	bool EndsWith(const TString& string) const {
		return EndsWith(string, false);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Concatenates a series of strings.
	static TString Concat(const TString* values, int length) {
		Validator::IsNotNull(values);
		
		// Compute the required size.
		int size = 0;
		for(int valIndex = 0; valIndex < length; valIndex++) {
			size += values[valIndex].length_;
		}

		// Concatenate in a new string (immutable).
		TString newString(size);

		for(int valIndex = 0; valIndex < length; valIndex++) {
			Ops::Concat(newString.data_, size, values[valIndex].data_,
						values[valIndex].length_);
		}

		// Set the length and return.
		newString.length_ = size;
		return newString;
	}
	
	// Concatenates the two given strings.
	static TString Concat(const TString& source0, const TString& source1) {
		// Allocate a new string large enough to hold the concatenation result.
		int length = source0.length_ + source1.length_;
		TString newString(length);

		Ops::Concat(newString.data_, length, source0.data_, source0.length_);
		Ops::Concat(newString.data_, length, source1.data_, source1.length_);

		newString.length_ = length;
		return newString;
	}

	// Concatenates the three given strings.	
	static TString Concat(const TString& source0, const TString& source1, 
						  const TString& source2) {
		// Allocate a new string large enough to hold the concatenation result.
		int length = source0.length_ + source1.length_ + source2.length_;
		TString newString(length);

		Ops::Concat(newString.data_, length, source0.data_, source0.length_);
		Ops::Concat(newString.data_, length, source1.data_, source1.length_);
		Ops::Concat(newString.data_, length, source2.data_, source2.length_);

		newString.length_ = length;
		return newString;
	}

	// Concatenates the four given strings.	
	static TString Concat(const TString& source0, const TString& source1,
						  const TString& source2, const TString& source3) {
		// Allocate a new string large enough to hold the concatenation result.
		int length = source0.length_ + source1.length_ + source2.length_ + source3.length_;
		TString newString(length);

		Ops::Concat(newString.data_, length, source0.data_, source0.length_);
		Ops::Concat(newString.data_, length, source1.data_, source1.length_);
		Ops::Concat(newString.data_, length, source2.data_, source2.length_);
		Ops::Concat(newString.data_, length, source3.data_, source3.length_);

		newString.length_ = length;
		return newString;
	}
	
	// Returns a value indicating whether the specified string occurs within this string
	bool Contains(const TString& string) {
		return Ops::FindString(data_, length_, string.data_, string.length_);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the index of the first occurrence of the given character, 
	// found in the string starting  with 'startIndex' position, on a length
	// of maximum 'count' characters.
	int IndexOf(const T value, int startIndex, int count) const {
		Validator::IsSmallerOrEqual(startIndex + count, length_);
		int lastIndex = startIndex + count;
		
        if(lastIndex > length_) {
            return INVALID_INDEX;
        }

		for(int charIndex = startIndex; charIndex < lastIndex; charIndex++) {
			if(Ops::Equal(data_[charIndex], value)) {
				return charIndex; // Found!
			}
		}

		return INVALID_INDEX; // Not found.
	}

	
	// Returns the index of the first occurrence of the given character, 
	// found	in the string starting with 'startIndex' position.
	int IndexOf(const T value, int startIndex) const {
		return IndexOf(value, startIndex, length_ - startIndex);
	}

	// Returns the index of the first occurrence of the given character.	
	int IndexOf(const T value) const {
		return IndexOf(value, 0);
	}

	// Returns the index of the first occurrence of the given string, found in the string starting 
	// with 'startIndex' position, on a length of maximum 'count' characters.
	int IndexOf(const TString& string, int startIndex, int count) const {
		Validator::IsSmallerOrEqual(startIndex + count, length_);
		
		if(startIndex >= length_) {
			return INVALID_INDEX;
		}

		T* position = Ops::FindString(&data_[startIndex], length_, 
									  string.data_, string.Length());
		if(position == nullptr) {
			return INVALID_INDEX; // The string was Not found.!
		}

		// Compute the index.
		int index = (int)(position - data_);
		if((index + string.Length()) > (startIndex + count)) {
			return INVALID_INDEX; // Not in the specified range.
		}

		return index;
	}

	// Returns the index of the first occurrence of the given string, 
	// found in the string starting with 'startIndex' position.
	int IndexOf(const TString& string, int startIndex) const {
		return IndexOf(string, startIndex, length_ - startIndex);
	}

	// Returns the index of the first occurrence of the given string.	
	int IndexOf(const TString& string) const {
		return IndexOf(string, 0);
	}

	// Returns the index of the first occurrence of any of the characters 
	// from the given 'values' array, found in the string starting with 
	// 'startIndex' position, on a length of maximum 'count' characters.
	int IndexOfAny(const T* values, int valueNumber, 
				      int startIndex, int count) const {
		Validator::IsNotNull(values);
		int minIdx = length_;

		// Find the first matching character.
		for(int valIndex = 0; valIndex < valueNumber; valIndex++) {
			int index = IndexOf(values[valIndex], startIndex, count);
			if(index != INVALID_INDEX && index < minIdx) {
				minIdx = index;
			}
		}

		if(minIdx == length_) {
            return INVALID_INDEX;
        }
		else return minIdx;
	}

	// Returns the index of the first occurrence of any of the characters 
	// from the given 'values' array, found in the string starting with 'startIndex' position.
	int IndexOfAny(const T* values, int valueNumber, int startIndex) const {
		return IndexOfAny(values, valueNumber, startIndex, length_ - startIndex);
	}

	// Returns the index of the first occurrence of any of the characters from the given 'values' array.	
	int IndexOfAny(const T* values, int valueNumber) const {
		return IndexOfAny(values, valueNumber, 0);
	}

	// Returns the index of the last occurrence of the given character,
	// found in the string starting  with 'startIndex' position, 
	// going maximum 'count' characters back.	
	int LastIndexOf(const T value, int startIndex, int count) const {
		Validator::IsSmaller(startIndex, length_);
		Validator::IsLargerOrEqual(startIndex + 1, count);
		int lastIndex = startIndex - count + 1;
		if(lastIndex > length_) {
            return INVALID_INDEX;
        }

		for(int charIndex = startIndex; charIndex >= lastIndex; charIndex--) {
			if(Ops::Equal(data_[charIndex], value)){
				return charIndex; // Found!
			}
		}

		return INVALID_INDEX; // Not found.
	}

	// Returns the index of the last occurrence of the given character, 
	// found in the string starting with 'startIndex' position.
	int LastIndexOf(const T value, int startIndex) const {
		return LastIndexOf(value, startIndex, startIndex + 1);
	}

	// Returns the index of the last occurrence of the given character.	
	int LastIndexOf(const T value) const {
		return LastIndexOf(value, length_ - 1);
	}

	// Returns the index of the last occurrence of the given string, 
	// found in the string starting with 'startIndex' position,
	// going maximum 'count' characters back.
	int LastIndexOf(const TString& string, int startIndex, int count) const {
		Validator::IsSmaller(startIndex, length_);
		Validator::IsLargerOrEqual(startIndex + 1, string.Length());
		Validator::IsSmallerOrEqual(count, length_);
		
		int position = startIndex - string.Length();
		if(position > length_) {
            return INVALID_INDEX;
        }

		// Search backwards.
		for(int countIndex = 0; countIndex < count; countIndex++) {
			int index = IndexOf(string, position);
			if((index != INVALID_INDEX) &&
			   ((index + string.Length()) <= (startIndex + 1))) {
				return index; // Found!
			}

			position--;
		}

		return INVALID_INDEX; // Not found.
	}
	
	// Returns the index of the last occurrence of the given string,
	// found in the string starting  with 'startIndex' position.
	int LastIndexOf(const TString& string, int startIndex) const {
		return LastIndexOf(string, startIndex, startIndex + 1);
	}

	// Returns the index of the last occurrence of the given string.
	int LastIndexOf(const TString& string) const {
		return LastIndexOf(string, length_ - 1);
	}

	// Returns the index of the last occurrence of any of the characters 
	// from the given 'values' array, found in the string starting with 
	// 'startIndex' position, on a length of maximum 'count' characters.
	int LastIndexOfAny(const T* values, int valueNumber, 
					   int startIndex, int count) const {
		Validator::IsSmaller(startIndex, length_);
		Validator::IsLargerOrEqual(startIndex + 1, count);

		if((startIndex > length_) || ((startIndex + 1) < count)) {
			return INVALID_INDEX;
		}

		int previousIndex = INVALID_INDEX;
		int lastIndex = -1;

		do {
			previousIndex = lastIndex;
			lastIndex = IndexOfAny(values, valueNumber, lastIndex + 1,
								   length_ - lastIndex - 1);
		} while(lastIndex != INVALID_INDEX);

		return previousIndex;
	}

	// Returns the index of the last occurrence of any of the characters from
	// the given 'values' array, found in the string starting with 'startIndex' position.
	int LastIndexOfAny(const T* values, int valueNumber, int startIndex) const {
		return LastIndexOfAny(values, valueNumber, startIndex, length_);
	}

	// Returns the index of the last occurrence of any of the characters from the given 'values' array.
	int LastIndexOfAny(const T* values, int valueNumber) const {
		return LastIndexOfAny(values, valueNumber, 0);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Creates a new string instance with the same value as the specified one.
	static TString Copy(const TString& source) {
		return TString(source);
	}

	// Copies 'count' characters starting with 'sourceIndex' position to 
	// the destination array, starting at 'destinationIndex' position.
	void CopyTo(int srcIndex, T* dest, int destIndex, int count) const {
		Validator::IsNotNull(dest);
		Validator::IsSmallerOrEqual(srcIndex + count, length_);
		int destPos = 0;

		while(destPos < count) {
			dest[destIndex + destPos] = data_[srcIndex];
			srcIndex++;
			destPos++;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns a copy of this string converted to lowercase.
	TString ToLower() const {
		TString newString(*this);
		Ops::ToLower(newString.data_, newString.length_);
		return newString;
	}
	
	// Returns a copy of this string converted to uppercase.
	TString ToUpper() const {
		TString newString(*this);
		Ops::ToUpper(newString.data_, newString.length_);
		return newString;
	}
	
	// Removes all leading occurrences of a set of characters specified in an array.
	TString TrimStart(const T* trimChars, int trimCharsNumber) const {
		if(trimChars) {
			Validator::IsLarger(trimCharsNumber, 0);
		}
		
		if(trimChars == nullptr) {
			trimChars = Ops::WHITESPACE_CHARS; // use the "white space" characters
			trimCharsNumber = Ops::WHITESPACE_CHARS_NUMBER;
		}

		// Find the first valid character.
		int copyIndex = 0;
		while(copyIndex < length_) {
			bool found = false;

			for(int charIndex = 0; charIndex < trimCharsNumber; charIndex++) {
				if(Ops::Equal(data_[copyIndex], trimChars[charIndex])) {
					copyIndex++;
					found = true;
					break;
				}
			}

			if(found == false) {
				// No special character found, so return the trimmed string.
				return TString(&data_[copyIndex]);
			}
		}

		return TString(); // Return an empty string.
	}

	// Removes all leading occurrences of "white space" characters.
	TString TrimStart() const {
		return TrimStart(nullptr, 0);
	}
	
	// Removes all trailing occurrences of a set of characters specified in an array.	
	TString TrimEnd(const T* trimChars, int trimCharsNumber) const {
		if(trimChars) {
			Validator::IsLarger(trimCharsNumber, 0);
		}
		
		if(trimChars == nullptr) {
			trimChars = Ops::WHITESPACE_CHARS; // use the "white space" characters
			trimCharsNumber = Ops::WHITESPACE_CHARS_NUMBER;
		}

		// Find the first valid character.
		int copyLength = length_ - 1;
		while(copyLength >= 0) {
			bool found = false;

			for(int charIndex = 0; charIndex < trimCharsNumber; charIndex++) {
				if(Ops::Equal(data_[copyLength], trimChars[charIndex])) {
					copyLength--;
					found = true;
					break;
				}
			}

			if(found == false) {
				// No special character found, so return the trimmed string.
				return TString(data_, copyLength + 1);
			}
		}

		return TString(); // Return an empty string.
	}
	
	// Removes all trailing occurrences of "white space" characters.
	TString TrimEnd() const {
		return TrimEnd(nullptr, 0);
	}
	
	// Removes all leading trailing occurrences of a set of characters specified in an array.
	TString Trim(const T* trimChars, int trimCharsNumber) const {
		return TrimStart(trimChars, trimCharsNumber).TrimEnd(trimChars, trimCharsNumber);
	}

	// Removes all leading and trailing occurrences of "white space" characters.	
	TString Trim() const {
		return Trim(nullptr, 0);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Retrieves a substring of specified 'length' starting at 'startIndex' position.
	TString Substring(int startIndex, int length) const {
		Validator::IsSmallerOrEqual(startIndex + length, length_);
		return TString(data_, startIndex, length);
	}
	
	// Retrieves a substring starting at 'startIndex' position.
	TString Substring(int startIndex) const {
		return Substring(startIndex, length_ - startIndex);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Inserts the given string at 'startIndex' position.
	TString Insert(int startIndex, const TString& value) const {
		Validator::IsSmallerOrEqual(startIndex, length_);
		int newLength = length_ + value.length_;
		TString newString(newLength);

		// Insert the value.
		Ops::Concat(newString.data_, newLength, data_, startIndex);
		Ops::Concat(newString.data_, newLength, value.data_, value.length_);
		Ops::Concat(newString.data_, newLength, &data_[startIndex], length_ - startIndex);

		// Set length and return.
		newString.length_ = newLength;
		return newString;
	}

	// Deletes 'count' characters beginning with 'startIndex' position.
	TString Remove(int startIndex, int count) const {
		Validator::IsSmallerOrEqual(startIndex + count, length_);
		if(count == 0) {
            return *this; // Special case, return this instance.
        }

		if(startIndex == 0) {
            return Substring(startIndex + count); // Faster.
        }
		else {
			int newLength = length_ - count;
			TString newString(newLength);

			// Concatenate the remaining parts.
			Ops::Concat(newString.data_, newLength, data_, startIndex);
			Ops::Concat(newString.data_, newLength, 
						&data_[startIndex + count], length_ - startIndex - count);

			// Set length and return.
			newString.length_ = newLength;
			return newString;
		}
	}
	
	// Deletes all characters beginning with 'startIndex' position.
	TString Remove(int startIndex) const {
		return Remove(startIndex, length_ - startIndex);
	}

	// Replace all occurrences of the character 'oldChar' with 'newChar'.
	TString Replace(const T oldChar, const T newChar) const {
		// Make a copy of this instance.
		TString newString(*this);
		T* currenT = newString.data_;
		int count = 0;

		while(currenT && count < length_) {
			if(Ops::Equal(*currenT, oldChar)) {
				// Replace with the new character.
				*currenT = newChar;
			}

			currenT = Ops::Next(currenT);
			count++;
		}

		return newString;
	}
	
	// Replaces all occurrences of the string 'oldValue'  with 'newValue'.
	TString Replace(const TString& oldValue, const TString& newValue) {
		int currentIndex = 0;
		T* oldValuePosition = nullptr;

		// Compute the length of the new string
		int newLength = length_;

		if(newValue.length_ != oldValue.length_) {
			// the length differs, so compute the exact length of the resulting string
			// It will be faster than reallocating the string.
			T* temp = data_;
			int count = 0;

			while(temp) {
				temp = Ops::FindString(temp, length_, oldValue.data_, oldValue.length_);
				if(temp) {
					count++;
					temp = Ops::Next(temp);
				}
			}

			// Adjust the new length.
			newLength += (newValue.length_ - oldValue.length_) * count;
		}

		// Allocate the string.
		TString newString(newLength);

		// Now do the replacement.
		do {
			// Find the Next old value.
			oldValuePosition = Ops::FindString(&data_[currentIndex], length_, 
											   oldValue.data_, oldValue.length_);
			if(oldValuePosition) {
				// Compute the index of the old value.
				int oldValIndex = (int)(oldValuePosition - data_);

				// concatenate from the current index to the old value, than the new value.
				Ops::Concat(newString.data_, newLength, 
							&data_[currentIndex], oldValIndex - currentIndex);
				Ops::Concat(newString.data_, newLength, newValue.data_, newValue.length_);

				// Update the current index.
				currentIndex = oldValIndex + oldValue.length_; // Skip over old value.
			}
		} while(oldValuePosition);

		if(currentIndex < length_) {
			// Concatenate the remaining part.
			Ops::Concat(newString.data_, newLength, 
						&data_[currentIndex], length_ - currentIndex);
		}

		// Set the length and return.
		newString.length_ = newLength;
		return newString;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Right-aligns the characters, padding on the left with 'paddingChar'.
	TString PadLeft(int totalWidth, const T paddingChar) const {
		if(totalWidth < length_) {
			return *this;
		}
		else {
			TString newString(totalWidth);

			// Pad with the given character, than concatenate this instance.
			Ops::Fill(newString.data_, totalWidth, paddingChar, totalWidth - length_);
			Ops::Concat(newString.data_, totalWidth, data_, length_);

			// Set length and return.
			newString.length_ = totalWidth;
			return newString;
		}
	}

	// Right-aligns the characters, padding on the left with white spaces.
	TString PadLeft(int totalWidth) const {
		return PadLeft(totalWidth, (T)0x20); // Pad with white space character (0x20).
	}

	// Left-aligns the characters, padding on the right with 'paddingChar'.
	TString PadRight(int totalWidth, const T paddingChar) const {
		if(totalWidth < length_) {
			return *this;
		}
		else {
			TString newString(totalWidth);

			// Concatenate this instance, than pad with the given character.
			Ops::Concat(newString.data_, totalWidth, data_, length_);
			Ops::Fill(&newString.data_[length_], totalWidth, 
					  paddingChar, totalWidth - length_);

			// Set length and return.
			newString.length_ = totalWidth;
			return newString;
		}
	}
	
	// Left-aligns the characters, padding on the right with white spaces.
	TString PadRight(int totalWidth) const {
		return PadRight(totalWidth, (T)0x20); // Pad with white space character.
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Splits the string in substrings delimited by the given char separators.
	// A maximum of 'count' substrings are generated. Empty substrings are omitted 
	// if StringSplitOptions::RemoveEmptyEntries is set.
	List<TString>& Split(List<TString>& substrList, const T* separators,
						 int sepNumber, int count, StringSplitOptions options) const {
		if(separators == nullptr) {
			// Use "white space" separators if array is nullptr.
			separators = Ops::WHITESPACE_CHARS;
			sepNumber = Ops::WHITESPACE_CHARS_NUMBER;
		}

		// Find the separator positions and split.
		local<int> sepPositions(new int[length_]);
		
		int positionCount = GetSeparatorPositions(separators, sepNumber, sepPositions);
		if((sepNumber == 0) || (positionCount == 0)) {
			substrList.Add(TString(*this));
			return substrList;
		}

		SplitImpl(substrList, sepPositions, nullptr, 0, positionCount, options, count);
		return substrList;
	}
	
	// Splits the string in substrings delimited by the given char separators.
	// Empty substrings are omitted if StringSplitOptions::RemoveEmptyEntries is set.
	List<TString>& Split(List<TString>& substrList, const T* separators, 
						 int separatorNumber, StringSplitOptions options) const {
		return Split(substrList, separators, separatorNumber, (int) - 1, options);
	}

	// Splits the string in substrings delimited by the given char separators.
	// A maximum of 'count' substrings are generated.
	List<TString>& Split(List<TString>& substrList, T* separators, 
						 int separatorNumber, int count) const {
		return Split(substrList, separators, separatorNumber, count, 
					 StringSplitOptions::None);
	}

	// Splits the string in substrings delimited by the given char separators.
	List<TString>& Split(List<TString>& substrList, const T* separators, 
						 int separatorNumber) const {
		return Split(substrList, separators, separatorNumber, 
					 (int) - 1, StringSplitOptions::None);
	}

	// Split the string in substrings delimited by the given string separators.
	// A maximum of 'count' substrings are generated. Empty substrings are omitted 
	// if StringSplitOptions::RemoveEmptyEntries is set.
	List<TString>& Split(List<TString>& substrList, const TString *separators, 
						 int sepNumber, int count, StringSplitOptions options) const {
		Validator::IsNotNull(separators);
		int substringCount = 0;
		int positionCount = 0;
		local<int> sepPositions(new int[length_]);
		local<int> sepLengths(new int[length_]);

		// Find the separator positions and split.
		positionCount = GetSeparatorPositions(separators, sepNumber, 
                                              sepPositions, sepLengths);
		if((sepNumber == 0) || (positionCount == 0)) {
			substrList.Add(TString(*this));
			return substrList;
		}

		SplitImpl(substrList, sepPositions, sepLengths, sepNumber, 
                  positionCount, options, count);
		return substrList;
	}
	
	// Splits the string in substrings delimited by the given string separators.
	// Empty substrings are omitted if StringSplitOptions::RemoveEmptyEntries is set.
	List<TString>& Split(List<TString>& substrList, TString *separators, 
						 int sepNumber, StringSplitOptions options) const {
		return Split(substrList, separators, sepNumber, (int) - 1, options);
	}
	
	// Concatenates a separator between each string from the given array.
	static TString Join(const TString& separator, TString *values, int valNumber) {
		Validator::IsNotNull(values);
		if(valNumber == 0) {
            return ""; // Return an empty string.
        }

		// Compute the final length.
		int newLength = (valNumber - 1) * separator.length_;
		for(int valIndex = 0; valIndex < valNumber; valIndex++) {
			newLength += values[valIndex].length_;
		}

		TString newString(newLength);

		// Now join the strings.
		for(int valIndex = 0; valIndex < valNumber; valIndex++) {
			if(valIndex >= 1) {
				Ops::Concat(newString.data_, newLength, 
                            separator.data_, separator.length_);
			}

			Ops::Concat(newString.data_, newLength, 
				values[valIndex].data_, values[valIndex].length_);
		}

		// Set length and return.
		newString.length_ = newLength;
		return newString;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the length of the string.
	int Length() const {
		return length_;
	}

	// Returns a value indicating whether the string is empty.
	static bool IsEmpty(const TString& string) {
		return string.Length() == 0;
	}
	
	// Returns a pointer to the internal character array.
	T* Chars() const {
		return data_;
	}

	// Return the character found at the specified position.
	T Chars(int index) {
		Validator::IsSmaller(index, length);
		return data_[index];
	}
	
	// Returns a formatted string using the given format and arguments.
	static TString Format(T* format,...) {
		Validator::IsNotNull(format);
		int length = 16;
		int actualLen = 0;
		TString newString(length);

		// Method arguments.
		va_list arguments;
		va_start(arguments, format);

		while((actualLen = Ops::Format(newString.data_, length, 
                                       format, arguments)) == -1) {
			// Reallocate the string.
			length *= 2;
			newString.Resize(length, length);
		}

		// Set the length and restore the initial stack and return the string.
		newString.length_ = actualLen;
		va_end(arguments);
		return newString;
	}

    // Converts the specified integer to its string representation.
    static TString ToString(int value) {
        return Format(_T("%d"), value);
    }

    // Converts the specified unsigned integer to its string representation.
    static TString ToString(unsigned int value) {
        return Format(_T("%u"), value);
    }

    // Converts the specified short integer to its string representation.
    static TString ToString(short value) {
        return Format(_T("%d"), value);
    }

    // Converts the specified unsigned short integer to its string representation.
    static TString ToString(unsigned short value) {
        return Format(_T("%u"), value);
    }

    // Converts the specified long integer to its string representation.
    static TString ToString(__int64 value) {
        return Format(_T("%ll"), value);
    }

    // Converts the specified unsigned long integer to its string representation.
    static TString ToString(unsigned __int64 value) {
        return Format(_T("%ull"), value);
    }

    // Converts the specified float to its string representation.
    static TString ToString(float value) {
        return Format(_T("%f"), value);
    }

    // Converts the specified double to its string representation.
    static TString ToString(double value) {
        return Format(_T("%f"), value);
    }

    // Converts the specified float to its string representation.
    // 'precision' must be between 1 and 9 (inclusive).
    static TString ToString(float value, int precision) {
        Validator::IsLarger(precision, 0);
        Validator::IsSmaller(precision, 10);
        
        TChar formatString[] = {'%', '.', '0' + precision, 'f'};
        return Format(formatString, value);
    }

    // Converts the specified double to its string representation.
    // 'precision' must be between 1 and 9 (inclusive).
    static TString ToString(double value, int precision) {
        Validator::IsLarger(precision, 0);
        Validator::IsSmaller(precision, 10);

        TChar formatString[] = {'%', '.', '0' + precision, 'f'};
        return Format(formatString, value);
    }

    // Converts the specified pointer to its string representation.
    // The address is printed in the form 0xA232BC3.
    static TString ToString(void* value) {
        return Format(_T("0x%X"), value);
    }

    // Converts the specified boolean to its string representation.
    // The "true" or "false" string is returned.
    static TString ToString(bool value) {
        return value ? _T("true") : _T("false");
    }

    // Converts the specified character to a string.
    static TString ToString(char value) {
        return TString(value);
    }
    
#ifdef _UNICODE
    // Converts the specified character to a string.
    static TString ToString(TChar value) {
        return TString(value);
    }
#endif

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	TString& operator= (const TString& source) {
		if(&source == this) {
            return *this;
        }

		ReleaseString();
		new(this)TString(source); // Call the constructor.
		return *this;
	}

	// Assignment with move semantics.
	TString& operator= (TString&& source) {
		if(&source == this) {
            return *this;
        }

		ReleaseString();
		data_ = source.data_;
		length_ = source.length_;
		source.data_ = nullptr;
		source.length_ = 0;
		return *this;
	}

	bool operator== (const TString& b) const {
		return Equals(*this, b);
	}

	bool operator!= (const TString& b) const {
		return !Equals(*this, b);
	}

	bool operator< (const TString& other) const {
		return Compare(*this, other) < 0;
	}

	bool operator<= (const TString& other) const {
		return Compare(*this, other) <= 0;
	}

	bool operator> (const TString& other) const {
		return Compare(*this, other) > 0;
	}

	bool operator>= (const TString& other) const {
		return Compare(*this, other) >= 0;
	}

	TString operator+ (const TString& other) {
		return Concat(*this, other);
	}

	TString operator+ (const T other) {
		return Concat(*this, TString(other, 1));
	}
	
	friend TString operator+ (const TString& a, const TString& b) {
		return TString::Concat(a, b);
	}

	friend TString operator+ (const TString& a, const T b) {
		return TString::Concat(a, TString(b, 1));
	}

	friend TString operator+ (const T a, const TString& b) {
		return TString::Concat(TString(a, 1), b);
	}

	TString& operator+= (const TString& other) {
		if(other.length_ == 0) {
            return *this;
        }

		int newLength = length_ + other.length_;
		Resize(newLength, length_);
		Ops::Concat(data_, newLength, other.data_, other.length_);
		return *this;
	}

	const T operator[] (int index) const {
		Validator::IsSmaller(index, length_);
		return data_[index];
	}

	T operator[] (int index) {
		Validator::IsSmaller(index, length_);
		return data_[index];
	}

	int CompareTo(const TString& other) const{
		return Compare(*this, other);
	}

	unsigned GetHashCode() const {
		return HashCalculator::GetHashCode(data_, length_ * sizeof(T));
	}
};


// Definition for the base string type.
#ifdef _UNICODE
	typedef String<wchar_t> string;
#else
	typedef String<char> string;
#endif

} // namespace Base
#endif