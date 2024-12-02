// Stream.hpp
// Copyright (c) Lup Gratian
//
// Provides a generic view of a sequence of bytes.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STREAM_HPP
#define PC_BASE_STREAM_HPP

#include "../Abstraction/Platform.hpp"
using namespace Abstraction;

namespace Base {

class Stream {
public:
	virtual ~Stream() {}

	virtual void Close() = 0;
	virtual void Flush() = 0;
	virtual int Read(void* buffer, __int64 offset, int count) = 0;
	virtual char ReadByte() = 0;
	virtual __int64 Seek(__int64 offset, SeekOrigin origin) = 0;
	virtual int Write(const void *buffer, __int64 offset, int count) = 0;
	virtual bool WriteByte(char value) = 0;
	virtual bool CanRead() const = 0;
	virtual bool CanWrite() const = 0;
	virtual bool CanSeek() const = 0;
	virtual __int64 Length() const = 0;
	virtual bool SetLength(__int64 value) = 0;
	virtual __int64 Position() const = 0;
	virtual bool SetPosition(__int64 value) = 0;
};

} // namespace Base
#endif