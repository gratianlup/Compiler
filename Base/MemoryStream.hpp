// MemoryStream.hpp
// Copyright (c) Lup Gratian
//
// Creates a stream whose backing store is memory.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_MEMORY_STREAM_HPP
#define PC_BASE_MEMORY_STREAM_HPP

#include "Stream.hpp"
#include "DebugValidator.hpp"
#include "LocalPointer.hpp"
#include "SharedPointer.hpp"
#include <cstdlib>
#include <cmath>

namespace Base {

class MemoryStream : public Stream {
private:
	typedef DebugValidator Validator;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const __int64 DEFAULT_CAPACITY = 4096; // 4KB.
	static const __int64 MIN_CAPACITY = 256;

	sharedVect<char> buffer_;
	__int64 length_;
	__int64 capacity_;
	__int64 position_;
	bool canWrite_;
	bool canExpand_; // If buffer provided by user set to false.

	void AllocateArray(__int64 size) {
		buffer_ = new char[size];
	}

	void EnsureCapacity(__int64 size) {
		if(size > capacity_) {
			localVect<char> temp = new char[capacity_ * 2];
			memcpy(temp, buffer_, length_);
			buffer_ = temp;
			capacity_ *= 2;
		}
	}

public:
	MemoryStream() : 
			capacity_(DEFAULT_CAPACITY), length_(0), canWrite_(true), 
			position_(0), canExpand_(true) {
		AllocateArray(capacity_);
	}

	MemoryStream(__int64 capacity) : length_(0), canWrite_(true), position_(0), canExpand_(true) {
		capacity_ = std::max(capacity, MIN_CAPACITY);
		AllocateArray(capacity_);
	}

	MemoryStream(sharedVect<char> data, __int64 length, bool canWrite = false) : 
			buffer_(data), capacity_(length), length_(length), 
			canWrite_(canWrite), canExpand_(false), position_(0) {
		Validator::IsLarger(length, 0);
	}

	virtual ~MemoryStream() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool CanRead() const override {
		return true;
	}

	virtual bool CanWrite() const override {
		return canWrite_;
	}

	virtual bool CanSeek() const override {
		return true;
	}

	virtual void Close() override {}

	virtual void Flush() override {}

	virtual __int64 Capacity() const {
		return capacity_;
	}

	virtual void SetCapacity(__int64 value) {
		Validator::IsTrue(canExpand_);
		Validator::IsLarger(value, length_);
		
		if(value != capacity_) {
			value = std::max(value, MIN_CAPACITY);

			localVect<char> temp = new char[value];
			memcpy(temp, buffer_, length_);
			buffer_ = temp;
			capacity_ = value;
		}
	}

	virtual __int64 Length() const override {
		return length_;
	}

	virtual bool SetLength(__int64 value) override {
		return true;
	}

	virtual __int64 Position() const override {
		return position_;
	}

	virtual bool SetPosition(__int64 value) override {
		Validator::IsLargerOrEqual(value, 0);
		Validator::IsSmaller(value, length_);
		position_ = value;
		return true;
	}

	virtual int Read(void* buffer, __int64 offset, int count) override {
		Validator::IsNotNull(buffer);
		Validator::IsLargerOrEqual(offset, 0);
		Validator::IsLarger(count, 0);
		if(position_ >= length_) return 0; // EOF.

		int length = std::min((int)(length_ - position_), count);
		int processed = length;
		char* source = (char*)buffer_ + position_;
		char* dest = (char*)buffer + offset;

		if(length <= 32) {
			while(length > sizeof(__int64)) {
				*((__int64*)dest) = *((__int64*)source);
				length -= sizeof(__int64);
				dest += sizeof(__int64);
				source += sizeof(__int64);
				position_ += sizeof(__int64);
			}

			while(length > 0) {
				*dest++ = *source++;
				position_++;
				length--;
			}
		}
		else {
			std::memcpy(dest, source, length);
			position_ += length;
		}

		return processed;
	}

	virtual char ReadByte() override {
		if(position_ >= length_) return -1;
		return buffer_[position_++];
	}

	virtual int Write(const void* buffer, __int64 offset, int count) override {
		Validator::IsNotNull(buffer);
		Validator::IsLargerOrEqual(offset, 0);
		Validator::IsTrue(canWrite_);

		if(count == 0) return 0;
		EnsureCapacity(position_ + count);

		char* source = (char*)buffer + offset;
		char* dest = (char*)buffer_ + position_;
		int processed = count;

		if(count <= 32) {
			while(count > sizeof(__int64)) {
				*((__int64*)dest) = *((__int64*)source);
				count -= sizeof(__int64);
				dest += sizeof(__int64);
				source += sizeof(__int64);
				position_ += sizeof(__int64);
			}

			while(count > 0) {
				*dest++ = *source++;
				position_++;
				count--;
			}
		}
		else {
			std::memcpy(dest, source, count);
			position_ += count;
		}

		length_ = std::max(length_, position_);
		return processed;
	}

	virtual bool WriteByte(char value) override {
		Validator::IsTrue(canWrite_);
		EnsureCapacity(position_ + 1);
		buffer_[position_++] = (char)value;
		length_ = std::max(length_, position_);
		return true;
	}

	virtual __int64 Seek(__int64 offset, SeekOrigin origin) override {
		switch(origin) {
			case SeekOrigin::Begin: {
				Validator::IsSmaller(offset, length_);
				position_ = offset;
				break;
			}
			case SeekOrigin::Current: {
				Validator::IsSmaller(position_ + offset, length_);
				Validator::IsLargerOrEqual(position_ + offset, 0);
				position_ += offset;
				break;
			}
			case SeekOrigin::End: {
				Validator::IsSmaller(length_ + offset, length_);
				Validator::IsLargerOrEqual(length_ + offset, 0);
				position_ = length_ + offset;
				break;
			}
		}

		return position_;
	}

	virtual void* GetBuffer() const {
		return buffer_;
	}
};

} // namespace Base
#endif