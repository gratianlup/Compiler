// StringBuffer.hpp
// Copyright (c) Lup Gratian
//
// Implements a memory buffer that can be used to store strings of characters.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STRING_BUFFER
#define PC_BASE_STRING_BUFFER

#include "String.hpp"
#include "DebugValidator.hpp"
#include "LocalPointer.hpp"
#include "DefaultComparer.hpp"
#include <cstdlib>

namespace Base {

class StringBuffer {
private:
	static const int DEFAULT_CAPACITY = 4;

	wchar_t* data_;
	int length_;
	int capacity_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void Resize(int newCapacity) {
		if(newCapacity <= capacity_) return;

		local<wchar_t, true> newData = new wchar_t[newCapacity];
		memcpy(newData, data_, length_ * sizeof(wchar_t));
		delete[] data_;
		data_ = newData.Get();
		capacity_ = newCapacity;
	}

	void Release() {
		delete[] data_;
		data_ = nullptr;
		length_ = capacity_ = 0;
	}

public:
	// Initializes a buffer with the default capacity.
	StringBuffer() : data_(new wchar_t[DEFAULT_CAPACITY]), length_(0),
					 capacity_(DEFAULT_CAPACITY) {}

	// Initializes an empty buffer having the specified capacity.
	StringBuffer(int capacity) : data_(new wchar_t[capacity]), length_(0),
								 capacity_(capacity) {}

	// Initializes a buffer having the specified character 'count' times.
	StringBuffer(wchar_t ch, int count) :
			data_(new wchar_t[count]), length_(count), capacity_(count) {
		for(int i = 0; i < count; i++) {
			data_[i] = ch;
		}
	}

	// Initializes a buffer with the first 'count' characters of the given string.
	StringBuffer(const wchar_t* str, int count) :
			data_(new wchar_t[count]), length_(count), capacity_(count) {
		memcpy(data_, str, count * sizeof(wchar_t));
	}			
	
	// Initializes a buffer with the data of the other buffer.
	StringBuffer(const StringBuffer& other) :
			data_(new wchar_t[other.length_]), length_(other.length_),
			capacity_(other.capacity_) {
		memcpy(data_, other.data_, length_ * sizeof(wchar_t));
	}

	// Initializes a buffer with the first 'count' characters of the other buffer.
	StringBuffer(const StringBuffer& other, int count) :
			data_(new wchar_t[count]), length_(count), capacity_(count) {
		DebugValidator::IsSmallerOrEqual(count, other.Length());
		memcpy(data_, other.data_, count* sizeof(wchar_t));
	}
				
	// Initializes a buffer with the data of a 'string' object.
	StringBuffer(const string& other) :
			data_(new wchar_t[other.Length() + 1]), length_(other.Length() + 1),
			capacity_(other.Length() + 1) {
		memcpy(data_, other.Chars(), (other.Length() + 1) * sizeof(wchar_t));
	}

	~StringBuffer() {
		Release();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the length of the data.
	int Length() const {
		return length_;
	}

	// Returns a pointer to the internal data.
	wchar_t* Data() {
		return data_;
	}

	const wchar_t* Data() const {
		return data_;
	}

	// Resets the length of the data.
	void Clear() {
		length_ = 0;
	}

	// Appends the specified character.
	void Append(wchar_t ch) {
		Resize(length_ + 1);
		data_[length_++] = ch;
	}

	// Appends the specified character 'count' times.
	void Append(wchar_t ch, int count) {
		DebugValidator::IsLargerOrEqual(count, 0);
		Resize(length_ + count);
		
		for(int i = 0; i < count; i++) {
			data_[length_ + i] = ch;
		}

		length_ += count;
	}

	// Appends 'count' characters from the specified string.
	void Append(wchar_t* str, int count) {
		DebugValidator::IsNotNull(str);
		DebugValidator::IsLargerOrEqual(count, 0);
		Resize(length_ + count);
		
		for(int i = 0; i < count; i++) {
			data_[length_ + i] = str[i];
		}

		length_ += count;
	}

	// Append the data from the specified buffer.
	void Append(const StringBuffer& other) {
		Resize(length_ + other.length_);
		memcpy(&data_[length_], other.data_, other.length_ * sizeof(wchar_t));	
		length_ += other.length_;
	}

	// Appends the specified string, including the null-terminator.
	void Append(const string& str) {
		Resize(length_ + str.Length() + 1);
		memcpy(&data_[length_], str.Chars(), str.Length() * sizeof(wchar_t));
		data_[length_ + str.Length()] = 0; // Null-terminator.
		length_ += str.Length() + 1;
	}

	// Returns a 'string' representation of the data.
	string ToString() const {
		return string(data_);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StringBuffer& operator= (const StringBuffer& source) {
		if(&source == this) return *this;

		Release();
		new(this)StringBuffer(source); // Call the constructor.
		return *this;
	}

	// Assignment with move semantics.
	StringBuffer& operator= (StringBuffer&& source) {
		if(&source == this) return *this;

		Release();
		data_ = source.data_;
		length_ = source.length_;
		capacity_ = source.capacity_;
		source.data_ = nullptr;
		source.length_ = source.capacity_ = 0;
		return *this;
	}

	const wchar_t operator[] (int index) const {
		DebugValidator::IsSmaller(index, length_);
		return data_[index];
	}

	wchar_t operator[] (int index) {
		DebugValidator::IsSmaller(index, length_);
		return data_[index];
	}

	bool operator== (const StringBuffer& other) const {
		if(length_ != other.length_) return false;
		return memcmp(data_, other.data_, length_ * sizeof(wchar_t)) == 0;
	}

	bool operator!= (const StringBuffer& other) const {
		return !operator== (other);
	}

	bool operator< (const StringBuffer& other) const {
		return false; // Required by 'DefaultComparer'.
	}

	unsigned int GetHashCode() const {
		return HashCalculator::GetHashCode(data_, length_ * sizeof(wchar_t));
	}
};

} // namespace Base
#endif