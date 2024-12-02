// StreamReader.hpp
// Copyright (c) Lup Gratian
//
// Implements a reader that can read a sequence of characters from a byte stream.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STREAM_READER_HPP
#define PC_BASE_STREAM_READER_HPP

#include "String.hpp"
#include "Stream.hpp"
#include "Encoding.hpp"
#include "FileStream.hpp"
#include "UnicodeEncoding.hpp"
#include "ASCIIEncoding.hpp"
#include "UTF8Encoding.hpp"
#include "UTF32Encoding.hpp"
#include "StringBuilder.hpp"
#include "LocalPointer.hpp"
#include "SharedPointer.hpp"
#include "DebugValidator.hpp"
#include <memory.h>

namespace Base {

class StreamReader {
private:
	typedef string::TChar T;
	typedef DebugValidator Validator;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const static __int64 DEFAULT_BUFFER_SIZE = 4096; // 4 KB

	string path_;
	shared<Encoding> encoding_;
	shared<Stream> stream_;
	localVect<T> charBuffer_;
	localVect<char> byteBuffer_;
	__int64 charBuffPos_;
	__int64 charBuffData_;
	__int64 byteBuffPos_;
	__int64 byteBuffData_;
	bool userStream_;
	bool userEncoding_;
	bool isClosed_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns an encoding that matches the BOM found in the file.
	// If no BOM is found ASCIIEncoding is used.
	shared<Encoding> EncodingFromBOM() {
		// UTF-8: EF BB BF 
		// UTF-16 big endian byte order: FE FF 
		// UTF-16 little endian byte order: FF FE 
		// UTF-32 big endian byte order: 00 00 FE FF 
		// UTF-32 little endian byte order: FF FE 00 00 
		__int64 length = ReadPreamble(byteBuffer_);
		byteBuffPos_;
		byteBuffData_ = length;

		if(length < 2) {
			return new ASCIIEncoding();
		}

		if((byteBuffer_[0] == 0xFE) && (byteBuffer_[1] == 0xFF)) {
			// Big-endian Unicode.
			byteBuffPos_ += 2;
			return new UnicodeEncoding(true);
		}
		else if((byteBuffer_[0] == 0xFF) && (byteBuffer_[1] == 0xFE)) {
			if((length >= 4) && (byteBuffer_[2] == 0) && (byteBuffer_[3] == 0)) {
				// Little-endian UTF-32.
				byteBuffPos_ += 4;
				return new UTF32Encoding();
			}
			else {
				// Little-endian Unicode.
				byteBuffPos_ += 2;
				return new UnicodeEncoding(false);
			}
		}
		else if((length >= 3) && (byteBuffer_[0] == 0xEF) && (byteBuffer_[1] == 0xBB) &&
				(byteBuffer_[2] == 0xBF)) {
			// UTF-8.
			byteBuffPos_ += 3;
			return new UTF8Encoding();
		}
		else if((length >= 4) && (byteBuffer_[0] == 0) && (byteBuffer_[1] == 0) &&
				(byteBuffer_[2] == 0xFE) && (byteBuffer_[3] == 0xFF)) {
			// Big-endian UTF32.
			byteBuffPos_ += 4;
			return new UTF32Encoding(true);
		}
		else {
			// Use ASCII by default.
			return new ASCIIEncoding();
		}
	}

	// Reads the BOM from the stream.
	__int64 ReadPreamble(char* buff) {
		return stream_->Read(buff, 0, 4);
	}

	// Verifies if the given buffer starts with a valid BOM.
	bool IsPreamble(char* buff, __int64 length, char* preamble, __int64 preambleLen) {
		if((preambleLen == 0) || (length < preambleLen)) {
			return false;
		}

		for(__int64 i = 0; i < preambleLen; i++) {
			if(buff[i] != preamble[i]) {
				return false;
			}
		}

		return true;
	}

	// Advances the pointer after the BOM, if a valid one is found.
	void SkipPreamble() {
		char preamble[4];
		__int64 preambleLen = encoding_->GetPreamble(preamble);
		Validator::IsSmallerOrEqual(preambleLen, 4);

		byteBuffData_ = ReadPreamble(byteBuffer_);
		byteBuffPos_ = 0;

		if(IsPreamble(byteBuffer_, byteBuffData_, preamble, preambleLen)) {
			byteBuffPos_ += preambleLen;
		}
	}

	// Reads data from the stream and converts it to characters.
	__int64 ReadBuffer() {
		charBuffPos_ = 0;
		charBuffData_ = 0;

		// The buffer is empty.
		if(byteBuffPos_ == byteBuffData_) {
			byteBuffPos_ = 0;
			byteBuffData_ = 0;
		}

		// Read data from the stream until at least one character can be formed.
		do {
			__int64 read = stream_->Read(byteBuffer_, byteBuffData_, 
										 (int)(DEFAULT_BUFFER_SIZE - byteBuffData_));
			if((read == 0) && (byteBuffData_ == 0)) {
				return charBuffData_;
			}

			byteBuffData_ += read;
			__int64 written;
			encoding_->GetChars(byteBuffer_, byteBuffData_,
			    				&charBuffer_[charBuffData_],
								DEFAULT_BUFFER_SIZE - charBuffData_, written);
			byteBuffPos_ += written;
			charBuffData_ += written;
		} while(charBuffData_ == 0);

		return charBuffData_;
	}

	// Initializes the StreamReader. Called from constructor.
	void Initialize() {
		byteBuffer_ = new char[DEFAULT_BUFFER_SIZE];
		charBuffer_ = new T[DEFAULT_BUFFER_SIZE];
		charBuffPos_ = 0;
		charBuffData_ = 0;
	}

public:
	StreamReader(const string& path) : 
			path_(path), userStream_(false), userEncoding_(false) {
		Initialize();
		stream_ = new FileStream(path, FileMode::Open);
		encoding_ = new ASCIIEncoding();
		SkipPreamble();
	}

	StreamReader(shared<Stream> stream) : 
			userStream_(true), userEncoding_(false), stream_(stream) {
		Initialize();
		encoding_ = new ASCIIEncoding();
		SkipPreamble();
	}

	StreamReader(shared<Stream> stream, shared<Encoding> encoding) : 
			userStream_(true), userEncoding_(true),
			stream_(stream), encoding_(encoding) {
		Initialize();
		SkipPreamble();
	}

	StreamReader(shared<Stream> stream, bool useBOM) : 
			userStream_(true), userEncoding_(false), stream_(stream) {
		Initialize();
		encoding_ = EncodingFromBOM();
		SkipPreamble();
	}

	StreamReader(const string& path, bool useBOM) : 
			path_(path), userStream_(false), userEncoding_(false) {
		Initialize();
		stream_ = new FileStream(path, FileMode::Open);
		encoding_ = EncodingFromBOM();
		SkipPreamble();
	}

	~StreamReader() {
		// Handled by smart pointers.
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Closes the StreamReader object and the underlying stream.
	virtual void Close() {
		stream_->Close();
	}

	// Returns the underlying stream.
	virtual Stream* BaseStream() {
		return stream_;
	}

	// Verifies whether the current stream position is at the end of the stream.
	virtual bool EndOfStream() {
		return (charBuffPos_ == charBuffData_) &&
			   (byteBuffPos_ == byteBuffData_) &&
			   (ReadBuffer() == 0);
	}

	// Gets the current character encoding that the current StreamReader object is using.
	virtual Encoding* CurrentEncoding() {
		return encoding_;
	}

	// Returns the next available character but does not consume it.
	virtual T Peek() {
		if(charBuffPos_ < charBuffData_) {
			return charBuffer_[charBuffPos_];
		}
		else {
			if(ReadBuffer() == 0) {
				return T(-1); // EOF.
			}
			else return charBuffer_[charBuffPos_];
		}
	}

	// Reads the next character from the stream and advances the character position by one.
	virtual T Read() {
		if(charBuffPos_ < charBuffData_) {
			return charBuffer_[charBuffPos_++];
		}
		else {
			if(ReadBuffer() == 0) {
				return -1; // EOF.
			}
			else return charBuffer_[charBuffPos_++];
		}
	}

	// Reads maximum 'count' characters from the stream into 'buffer', beginning at 'index'.
	virtual __int64 Read(T* buffer, __int64 index, __int64 count) {
		Validator::IsNotNull(buffer);
		__int64 read = 0;

		while(count > 0) {
			__int64 available = charBuffData_ - charBuffPos_;
			if(available == 0) {
				available = ReadBuffer();
				if(available == 0) {
					return read; // EOF reached.
				}
			}

			// Copy the data to the user buffer.
			if(available > count) {
				available = count;
			}

			memcpy(&buffer[index + read], &charBuffer_[charBuffPos_],
				   available * sizeof(T));
			read += available;
			count -= available;
			charBuffPos_ += available;
		}
		
		return read;
	}

	// Reads a line of characters from the current stream and returns the data as a string.
	virtual string ReadLine() {
		// Read until we encounter the line terminators:
		// \r\n - Windows
		// \n - Unix, \r - Mac
		StringBuilder builder;

		do {
			__int64 startPos = charBuffPos_;
			__int64 startData = charBuffData_;

			for(__int64 i = charBuffPos_; i < charBuffData_; i++) {
				T c = charBuffer_[i];
				if((c == _T('\r')) || (c == _T('\n'))) {
					// Line terminator found; check type.
					builder.Append(charBuffer_, (int)charBuffPos_, (int)(i - charBuffPos_));
					charBuffPos_ = i + 1;

					// Check if after \r an \n follows, and if true, skip over it.
					if(c == _T('\r')) {
						if((i < charBuffData_) || (ReadBuffer() > 0)) {
							if(charBuffer_[charBuffPos_] == _T('\n')) {
								charBuffPos_++;
							}
						}
					}
					
					return builder.ToString();
				}
			}

			// Append the data processed at this step.
			builder.Append(charBuffer_, (int)startPos, (int)(startData - startPos));

			// New line terminator not found, read from the stream.
			if(ReadBuffer() == 0) {
				// The end of the stream has been reached, return what we could read.
				return builder.ToString();
			}
		} while(charBuffData_ > 0);

		return builder.ToString();
	}

	// Reads the stream from the current position to the end of the stream.
	virtual string ReadToEnd() {
		StringBuilder builder;

		do {
			builder.Append(charBuffer_, (int)charBuffPos_, 
						   (int)(charBuffData_ - charBuffPos_));
			charBuffPos_ = charBuffData_;
			ReadBuffer(); // Fill the buffer again.
		} while(charBuffData_ > 0);

		return builder.ToString();
	}
};

} // namespace Base
#endif