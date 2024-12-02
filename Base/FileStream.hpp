// FileStream.hpp
// Copyright (c) Lup Gratian
//
// Exposes a stream around a file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_FILE_STREAM_HPP
#define PC_FILE_STREAM_HPP

#include "Stream.hpp"
#include "DebugValidator.hpp"
#include "../Abstraction/Platform.hpp"
#include <cmath>
#include <cstdlib>
using namespace Abstraction;

namespace Base {

/*	HOW THE BUFFER WORKS
 
  |############______________|
	  ^       ^              ^
	  |       |              |
	  |    readLen       bufferSize
   readPos

 ------------------------------------- OR

 to be flushed
   /      \
  |########__________________|
		  ^                  ^
		  |                  |
	   writePos          bufferSize
*/

class FileStream : public Stream {
private:
	typedef DebugValidator Validator;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const static __int64  DEFAULT_BUFFER_SIZE = 16348; // 16 KB

	char* buffer_;
	__int64 bufferSize_;
	__int64 readLength_; // The amount of data available in the buffer.
	__int64 readPos_;    // How much data was used from the buffer.
	__int64 writePos_;   // The next write position in the buffer
					     // (and the amount of data that needs to be flushed to the disk).
	string fileName_;
	IO::Handle handle_;
	__int64 filePos_;
	bool canRead_;
	bool canWrite_;
	bool canSeek_;
	bool isClosed_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Allocates a file buffer if one isn't allocated yet.
	void FileStream::EnsureBuffer() {
		// Allocate a new buffer if it wasn't allocated yet.
		if(buffer_ == nullptr) {
			buffer_ = new char[bufferSize_];
		}
	}

	// Deallocates the file buffer.
	void FileStream::ReleaseBuffer() {
		if(buffer_) 		{
			delete[] buffer_;
			buffer_ = nullptr;
		}
	}

	// Initializes the FileStream in the case of a file.
	bool FileStream::Initialize(const string &path, FileMode mode, FileAccess access,
								FileShare share, FileOptions options, __int64 bufferSize) {
		Validator::IsFalse(string::IsEmpty(path));
		Validator::IsLarger(bufferSize, 0);

		bool moveToEnd = false; // Used with the 'Append' flag.
		isClosed_ = true;

		// Open/create the file
		if(IO::CreateFile(path.Chars(), handle_, mode, 
						  access, share, options) == false) {
			return false;
		}

		// Set the operations that are allowed on this file.
		canRead_ = ((int)access & (int)FileAccess::Read)   != 0;
		canWrite_ = ((int)access & (int)FileAccess::Write) != 0;
		canSeek_ = true;

		// Initialize other members.
		fileName_ = path;
		readLength_  = 0;
		readPos_  = 0;
		writePos_ = 0;
		filePos_  = 0;
		bufferSize_ = bufferSize;
		buffer_ = nullptr;
		isClosed_ = false;

		// Allocate the buffer.
		EnsureBuffer();
		return true;
	}

	// Reads the specified number of bytes and moves the file pointer.
	int ReadImpl(void* buffer, __int64 offset, int count) {
		__int64 length = Length();

		if((filePos_ + count) > length) {
			// Don't read after the file end
			count = (int)std::max(0LL, length - filePos_);
		}

		int read = IO::ReadFile(handle_, (char*)buffer + offset, count);
		filePos_ += read;
		return read;
	}

	// Writes the specified number of bytes and moves the file pointer.
	int WriteImpl(const void* buffer, __int64 offset, int count) {
		int written = IO::WriteFile(handle_, (char*)buffer + offset, count);
		filePos_ += written;
		return written;
	}

	// Copies the specified number of bytes 
	// from the file buffer to the specified destination.
	void FillFromBuffer(void *dest, __int64 offset, __int64 buffOffset, int count)	{
		std::memcpy((char *)dest + offset, buffer_ + buffOffset, count);
	}
	
	// Copies the specified number of bytes
	// from the source to the file buffer.
	void FillBufferWith(const void *source, __int64 offset, __int64 buffOffset, int count) {
		std::memcpy(buffer_ + buffOffset, (char *)source + offset, count);
	}

	// Invalidates the read buffer, and if needed, 
	// sets the file position to the real value.
	bool InvalidateReadBuffer() {
		// The file pointer was moved ahead by the read buffer, so we need
		// to move it back to the real read position.
		if((readPos_ - readLength_) != 0) {
			if(IO::SeekFile(handle_, readPos_ - readLength_, 
							SeekOrigin::Current) == false) {
				return false;
			}
		}

		// Now invalidate the read buffer.
		readLength_ = 0;
		readPos_ = 0;
		return true;
	}

	// Invalidates the read write, and if needed, 
	// sets the file position to the real value.
	bool InvalidateWriteBuffer() {
		if(writePos_ > 0) {
			// We have some data that hasn't been written to the file.
			if(WriteImpl(buffer_, 0, (int)writePos_) != writePos_) {
				return false;
			}

			writePos_ = 0;
		}

		return true;
	}

public:
	FileStream::FileStream(const string &path, FileMode mode) {
		Initialize(path, mode, (mode == FileMode::Append ? FileAccess::Write : 
														  FileAccess::ReadWrite),
				   FileShare::Read, FileOptions::None, DEFAULT_BUFFER_SIZE);
	}

	FileStream::FileStream(const string &path, FileMode mode, FileAccess access) {
		Initialize(path, mode, access, FileShare::Read, 
				   FileOptions::None, DEFAULT_BUFFER_SIZE);
	}

	FileStream::FileStream(const string &path, FileMode mode, FileAccess access, 
						   FileShare share) {
		Initialize(path, mode, access, share, FileOptions::None, DEFAULT_BUFFER_SIZE);
	}

	FileStream::FileStream(const string &path, FileMode mode, FileAccess access, 
						   FileShare share, __int64 bufferSize_) {
		Initialize(path, mode, access, share, FileOptions::None, bufferSize_);
	}

	FileStream::FileStream(const string &path, FileMode mode, FileAccess access, 
						   FileShare share, __int64 bufferSize_, FileOptions options) {
		Initialize(path, mode, access, share, options, DEFAULT_BUFFER_SIZE);
	}

	virtual ~FileStream() {
		Flush();
		Close();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Reads a block of bytes from the stream and writes the data in a given buffer.
	virtual int Read(void* buffer, __int64 offset, int count) override {
		Validator::IsNotNull(buffer);
		Validator::IsLargerOrEqual(offset, 0);
		Validator::IsLarger(count, 0);
		Validator::IsTrue(canRead_);
		Validator::IsFalse(isClosed_);
		
		// See how much data is available in the buffer.
		__int64 available = readLength_ - readPos_;
		__int64 readBytes;

		// We try first to use the data that was read ahead in the buffer.
		// If there is no data, or using the buffer would incur a performance penalty
		// (the amount of data that we need to read is larger than the buffer), 
		// we read directly, skipping the buffer.
		if(available == 0) {
			// No data is available, we need to read from the file.
			if(count >= bufferSize_) {
				// There is no reason in this case to read into the buffer
				// (it would actually slow things down).
				// Make sure cached writes are flushed.
				if(InvalidateWriteBuffer() == false) {
					return -1;
				}

				// The read buffer is no longer valid.
				readPos_ = 0;
				readLength_ = 0;

				return ReadImpl(buffer, offset, count) == count;
			}
			else {
				// Read into the buffer as much as we can.
				available = ReadImpl(buffer_, 0, (int)bufferSize_);

				if(available == 0) {
					return 0;
				}

				readLength_ = available;
				readPos_ = 0;
			}
		}

		// Use as much data from the buffer as we can.
		// If there is still not enough data, read from file.
		readBytes = std::min(available, (__int64)count);
		FillFromBuffer(buffer, offset, readPos_, (int)readBytes);
		readPos_ += readBytes;

		if(available < count) {
			// We still need to read some bytes from the file.
			int read = ReadImpl(buffer, offset + available, 
							   (int)(count - available));
			if(read == 0) {
				return (int)readBytes; // Return what could be read.
			}

			readBytes += read;

			// Reset the read buffer.
			readPos_ = 0;
			readLength_ = 0;
		}

		return (int)readBytes;
	}

	// Reads a byte from the file and advances the read position one byte.
	virtual char ReadByte() override {
		Validator::IsTrue(canRead_);
		Validator::IsFalse(isClosed_);
		
		// If there is data available in the read buffer, we return it from there.
		// If not, we fill the buffer with another set of data 
		// and return the first byte from it.
		__int64 available = readLength_ - readPos_; // See if there is cached data.
		
		if(available != 0) {
			return (unsigned char)buffer_[readPos_++];
		}
		else {
			// We need to read from the file. 
			// We make sure cached writes are flushed to the file.
			EnsureBuffer();
			if(InvalidateWriteBuffer() == false) {
				return -1;
			}

			readPos_ = 0;
			readLength_ = ReadImpl(buffer_, 0, (int)bufferSize_);

			if(readLength_ == 0) {
				return -1;
			}
			else if(readLength_ > 0) {
				return (__int64)buffer_[readPos_++]; // Return the first read byte.
			}
		}

		return -1; // Could not be read (maybe EOF).
	}

	// Writes a block of bytes to this stream using data from a buffer.
	virtual int Write(const void* buffer, __int64 offset, int count) override {
		Validator::IsNotNull(buffer_);
		Validator::IsLargerOrEqual(offset, 0);
		Validator::IsLarger(count, 0);
		Validator::IsTrue(canWrite_);
		Validator::IsFalse(isClosed_);
		
		// We write as much as we can into the write buffer (first we make sure 
		// that the read buffer is invalidated). If the amount of data to write 
		// is larger than the buffer, we write it directly to the file.
		// Invalidate the read buffer (else the write could be performed
		// at a position after the real position).
		if(InvalidateReadBuffer() == false) {
			return -1;
		}

		// See if there is space in the buffer. If it is, copy 
		// as much as we can into the buffer. Otherwise, flush 
		// the buffer to disk and copy the remaining data.
		if(writePos_ > 0) {
			__int64 left = bufferSize_ - writePos_;
			__int64 toBuffer = std::min(left, (__int64)count);

			// Copy the data to the buffer.
			FillBufferWith(buffer, offset, writePos_, (int)toBuffer);
			writePos_ += toBuffer;

			if(toBuffer == count) {
				return count; // All data could be buffered.
			}

			// The buffer is full, flush it.
			count -= (int)toBuffer;

			if(InvalidateWriteBuffer() == false) {
				return -1;
			}
		}

		// Some that has not been written in the previous step.
		if(count >= bufferSize_) {
			// If the buffer size is too small, don't bother copying the data to it.
			return WriteImpl(buffer, offset, count);
		}
		else {
			// What remained is small enough, so buffer it.
			FillBufferWith(buffer, offset, 0, count);
			writePos_ = count;
			return count;
		}
	}

	// Writes a byte to the current position in the file stream.
	virtual bool WriteByte(char value) override {
		Validator::IsTrue(canWrite_);
		Validator::IsFalse(isClosed_);
		
		// If there is free space in the write buffer, we write the byte there.
		// If not, we flush the buffer and write the byte on 
		if(writePos_ == 0) {
			// It's possible that the previous operation was Read,
			// so flush the read buffer.
			if(InvalidateReadBuffer() == false) {
				return false;
			}
		}
		else if(writePos_ == bufferSize_) {
			// The buffer is full, we need to flush data to the file.
			if(InvalidateWriteBuffer() == false) {
				return false;
			}
		}

		buffer_[writePos_++] = (char)value; // Copy the byte to the buffer.
		return true;
	}

	// Clears all buffers for this stream and causes 
	// any buffered data to be written to the file system.
	virtual void Flush() override {
		InvalidateReadBuffer();
		InvalidateWriteBuffer();
		FlushFileBuffers(handle_);
	}
		
	virtual __int64 Seek(__int64 offset, SeekOrigin origin) override {
		Validator::IsTrue(canSeek_);
		Validator::IsFalse(isClosed_);
		
		// We first need to invalidate the write buffer.
		if(InvalidateWriteBuffer() == 0) {
			return filePos_;
		}

		// Adjust the offset if the origin is Current and the read buffer was used.
		//
		//  ahead (buffered)
		//      /      \
		// |#######################| <- file
		//      ^      ^
		//      |      |
		//      | filePos_
		// real position
		if(origin == SeekOrigin::Current) {
			// Subtract the amount of data we read ahead from the file.
			offset -= readLength_ - readPos_;
		}

		// Seek.
		__int64 oldPos = filePos_;
		filePos_ = IO::SeekFile(handle_, offset, origin);

		// Try to save as much as possible from the read buffer.
		if(readLength_ > 0) {
			__int64 distance = filePos_ - oldPos + (readLength_ - readPos_);

			if (distance == 0) {
				if(readPos_ > 0) {
					// Shift the remained valid cached data 
					// to the beginning of the buffer.
					FillBufferWith(buffer_, readPos_, 0, 
								   (int)(readLength_ - readPos_));
					readLength_ -= readPos_;
					readPos_ = 0;
				}

				if(readLength_ > 0) {
					// We need to adjust the file pointer.
					IO::SeekFile(handle_, readLength_, SeekOrigin::Current);
				}
			}
			else if(((oldPos - readLength_) < filePos_) && (filePos_ < oldPos)) {
				// We moved forward with at most 'bufferLength_' bytes.
				FillBufferWith(buffer_, readPos_ + distance, 0, 
							   (int)(readLength_ - (readPos_ + distance)));
				readLength_ -= readPos_ + distance;
				readPos_ = 0;

				if(readLength_ > 0) {
					// We need to adjust the file pointer.
					IO::SeekFile(handle_,readLength_, SeekOrigin::Current);
				}
			}
			else {
				// We moved backwards or forward > 'bufferLength_', 
				// so the read buffer needs to be reset.
				readLength_   = 0;
				readPos_ = 0;
			}
		}

		return filePos_;
	}

	// Sets the length of this stream to the given value.
	virtual bool SetLength(__int64 value) override {
		Validator::IsTrue(canSeek_);
		Validator::IsFalse(isClosed_);
		
		// We need to invalidate the buffer first!
		if(InvalidateReadBuffer() == false) {
			return false;
		}

		if(InvalidateWriteBuffer() == false) {
			return false;
		}

		IO::SeekFile(handle_, value, SeekOrigin::Begin);
		return IO::SetFileSize(handle_, value);
	}

	// Flushes the buffers and closes the file.
	virtual void Close() override {
		if(isClosed_ == false) {
			Flush();
			IO::CloseFile(handle_);
			ReleaseBuffer();

			isClosed_ = true;
			readLength_ = 0;
			readPos_    = 0;
			writePos_   = 0;
			filePos_    = 0;
		}
	}

	// Gets the position of the stream.
	virtual __int64 Position() const override {
		Validator::IsTrue(canSeek_);
		Validator::IsFalse(isClosed_);
		
		return (filePos_    - // Subtract the read buffer position,
				readLength_ + // but add the data that the user read so far
				readPos_    + // and the data that hasn't been flushed to disk yet.
				writePos_);   
	}

	// Sets the position of the stream to the specified value.
	virtual bool SetPosition(__int64 value) override {
		Validator::IsTrue(canSeek_);
		Validator::IsFalse(isClosed_);
		
		// We first need to invalidate the buffer.
		InvalidateWriteBuffer();
		readLength_ = 0; // Don't invalidate read buffer the normal way, 
		readPos_ = 0;    // it would call SeekImpl with no reason.
		return IO::SeekFile(handle_, value, SeekOrigin::Begin) != 0;	
	}

	// Gets the length in bytes of the stream.
	virtual __int64 Length() const override {
		Validator::IsTrue(canSeek_);
		Validator::IsFalse(isClosed_);
		__int64 size;

		if(IO::GetFileSize(fileName_, size) == false) {
			return -1;
		}
		
		return size;
	}

	virtual bool CanRead() const override {
		return canRead_;
	}

	virtual bool CanWrite() const override {
		return canWrite_;
	}

	virtual bool CanSeek() const override {
		return canSeek_;
	}

	virtual const string& Name() const {
		return fileName_;
	}

	virtual bool IsClosed() const {
		return isClosed_;
	}
};

} // namespace Base
#endif