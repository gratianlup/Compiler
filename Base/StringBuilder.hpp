// StringBuilder.hpp
// Copyright (c) Lup Gratian
//
// Represents a mutable string of characters.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STRING_BUILDER_HPP
#define PC_BASE_STRING_BUILDER_HPP

#include "DebugValidator.hpp"
#include "String.hpp"
#include "../Abstraction/PlatformString.hpp"
#include <cmath>
using namespace Abstraction;

namespace Base {

class StringBuilder {
private:
	typedef string::TChar T;
	typedef StringOps<T> Ops;
	typedef DebugValidator Validator;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const int DEFAULT_CAPACITY = 64;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	T* buffer_;
	int capacity_;
	int length_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Allocates enough space to hold 'size' characters and the terminator.
	void AllocateBuffer(int size) {
		buffer_ = new T[Ops::RequiredSpace(size)];
		Ops::ResetString(buffer_);
	}

	// Deallocates the character buffer.
	void DeallocateBuffer() {
		delete[] buffer_;
		buffer_ = nullptr;
		capacity_ = length_ = 0;
	}

	// Checks if the current capacity is enough to hold 'value' characters and the
	// terminator. If not, a larger buffer is allocated and the current content copied to it.
	void EnsureCapacityImpl(int value) {
		if(capacity_ < Ops::RequiredSpace(value)) {
			value = std::max(value, capacity_ * 2);
			T* temp = new T[Ops::RequiredSpace(value)];
			Ops::Copy(temp, value, buffer_, length_);

			delete[] buffer_;
			buffer_ = temp;
			capacity_ = value;
		}
	}

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StringBuilder() : capacity_(DEFAULT_CAPACITY), length_(0) {
		AllocateBuffer(capacity_);
	}

	StringBuilder(int capacity) : capacity_(capacity), length_(0) {
		AllocateBuffer(capacity_);
	}

	StringBuilder(const string& source) : capacity_(DEFAULT_CAPACITY), length_(0) {
		AllocateBuffer(capacity_);
		Append(source);
	}

	StringBuilder(const string& source, int capacity) : capacity_(capacity), length_(0) {
		AllocateBuffer(capacity_);
		Append(source);
	}

	~StringBuilder() {
		DeallocateBuffer();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Appends the specified string.
	StringBuilder& Append(const string& value) {
		if(value.Length() == 0) return *this;

		EnsureCapacityImpl(length_ + value.Length() + 1);
		Ops::Concat(buffer_, capacity_, value.Chars(), value.Length());
		length_ += value.Length();
		return *this;
	}

	// Appends the specified character.
	StringBuilder& Append(T value) {
		EnsureCapacityImpl(length_ + 1);
		Ops::Concat(buffer_, length_, value);
		length_++;
		return *this;
	}

	// Appends the specified character 'count' times.
	StringBuilder& Append(T value, int count) {
		EnsureCapacityImpl(length_ + count + 1);
		Ops::Fill(&buffer_[length_], length_, value, count);
		length_ += count;
		return *this;
	}

	// Appends the content of the specified character array.
	StringBuilder& Append(T* source, int index, int size) {
		Validator::IsNotNull(source);
		if(size == 0) return *this;

		EnsureCapacityImpl(length_ + size + 1);
		Ops::Concat(buffer_, capacity_, &source[index], size);
		length_ += size;
		return *this;
	}

	// Appends the content of the specified character array.
	StringBuilder& Append(T* source) {
		Validator::IsNotNull(source);
		int size = Ops::Length(source);
		EnsureCapacityImpl(length_ + size);
		Ops::Concat(buffer_, capacity_, source, size);
		length_ += size;
		return *this;
	}

	// Appends the specified substring.
	StringBuilder& Append(const string& value, int index, int count) {
		Validator::IsSmallerOrEqual(index + count, value.Length());
		if(count == 0) return *this;

		EnsureCapacityImpl(length_ + value.Length());
		Ops::Concat(buffer_, capacity_, value.Chars() + index, count);
		length_ += count;
		return *this;
	}

	// Appends the string returned by processing a composite format string.
	StringBuilder& AppendFormat(T* format,...) {
		Validator::IsNotNull(format);
		int actualLen = 0;
		va_list arguments;
		va_start(arguments, format);

		while((actualLen = Ops::Format(&buffer_[length_], capacity_ - length_, 
									   format, arguments)) == -1) {
			// The buffer is too small.
			EnsureCapacity(capacity_ * 2);
		}

		va_end(arguments);
		length_ += actualLen;
		return *this;
	}

	// Appends the given string, followed by a new line.
	StringBuilder& AppendLine(const string& value) {
		EnsureCapacityImpl(length_ + Ops::NewLineLength() + value.Length());
		Ops::Concat(buffer_, capacity_, value.Chars(), value.Length());
		Ops::ConcatNewLine(buffer_, length_ + value.Length());
		length_ += Ops::NewLineLength() + value.Length();
		return *this;
	}

	StringBuilder& AppendLine(T* source) {
		Validator::IsNotNull(source);
		int size = Ops::Length(source);
		EnsureCapacityImpl(length_ + Ops::NewLineLength() + size);
		Ops::Concat(buffer_, capacity_, source, size);
		Ops::ConcatNewLine(buffer_, length_ + size);
		length_ += Ops::NewLineLength() + size;
		return *this;
	}

	// Appends a new line.
	StringBuilder& AppendLine() {
		EnsureCapacityImpl(length_ + Ops::NewLineLength());
		Ops::ConcatNewLine(buffer_, length_);
		length_ += Ops::NewLineLength();
		return *this;
	}

	int EnsureCapacity(int value) {
		Validator::IsLarger(value, length_);
		EnsureCapacityImpl(value);
		return capacity_;
	}

	// Inserts the content of the specified character array at the given position.
	StringBuilder& Insert(int index, T* source, int size) {
		Validator::IsNotNull(source);
		if(size == 0) return *this;
		if(length_ == 0) return Append(source);

		// Split the string at the indicated place.
		int margin = length_ + size;
		EnsureCapacityImpl(margin);

		for(int i = margin; i >= (index + size); i--) {
			buffer_[i] = buffer_[i - size];
		}

		// Insert the given string.
		for(int i = 0; i < size; i++) {
			buffer_[index + i] = source[i];
		}

		length_ += size;
		return *this;
	}

	// Inserts the content of the specified character array at the given position.
	StringBuilder& Insert(int index, T* source) {
		return Insert(index, source, Ops::Length(source));
	}

	// Inserts the specified string at the given position.
	StringBuilder& Insert(int index, const string& value) {
		Validator::IsSmallerOrEqual(index, length_);
		if(value.Length() == 0) return *this;
		return Insert(index, value.Chars(), value.Length());
	}

	// Inserts the specified string at the given position 'count' times.
	StringBuilder& Insert(int index, const string& value, int count) {
		Validator::IsSmallerOrEqual(index, length_);
		if(value.Length() == 0) return *this;

		if(length_ == 0) {
			for(int i = 0; i < count; i++) {
				Append(value);
			}

			return *this;
		}

		// Split the string at the indicated place.
		int size = value.Length() * count;
		int margin = length_ + size;
		EnsureCapacityImpl(margin);

		for(int i = margin; i >= (index + size); i--) {
			buffer_[i] = buffer_[i - size];
		}

		// Insert the given string 'count' times.
		for(int i = 0; i < count; i++) {
			for(int j = 0; j < value.Length(); j++) {
				buffer_[index++] = value[j];
			}
		}

		length_ += size;
		return *this;
	}

	// Inserts the specified character at the given position.
	StringBuilder& Insert(int index, T value) {
		if(length_ == 0) {
			return Append(value);
		}

		// Split the string at the indicated place.
		int margin = length_ + 1;
		EnsureCapacityImpl(margin);

		for(int i = margin; i >= (index + 1); i--) {
			buffer_[i] = buffer_[i - 1];
		}

		buffer_[index++] = value;

		length_++;
		return *this;
	}

	// Removes the specified range of characters from this instance.
	StringBuilder& Remove(int index, int count) {
		Validator::IsSmallerOrEqual(index + count, length_);
		if(count == 0) return *this;

		for(int i = index; i < length_; i++) {
			buffer_[i] = buffer_[i + count];
		}

		return *this;
	}

	// Gets the underlying character buffer.
	T* Chars() const {
		return buffer_;
	}

	// Gets a String instance of the builder.
	string ToString() const {
		return string(buffer_, length_);
	}

	// Gets the capacity of the builder.
	int Capacity() const {
		return capacity_;
	}

	// Sets the capacity of the builder.
	void SetCapacity(int value) {
		EnsureCapacity(value);
	}

	// Gets the length of the string.
	int Length() const {
		return length_;
	}

	// Sets the length of the string.
	void SetLength(int value) {
		if(value < length_) {
			length_ = value;
			buffer_[length_ - 1] = 0;
		}
		else {
			for(int i = length_; i < value; i++) {
				buffer_[i] = 0;
			}

			length_ = value;
		}
	}

	unsigned GetHashCode() const {
		return HashCalculator::GetHashCode(buffer_, length_ * sizeof(T));
	}
};

} // namespace Base
#endif