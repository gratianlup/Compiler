// CharSource.hpp
// Copyright (c) Lup Gratian
//
// Represents a source for the Lexer that provides characters.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_CHAR_SOURCE_HPP
#define PC_LEXING_CHAR_SOURCE_HPP

#include "../Common/FileManager.hpp"
#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Common;

namespace Lexing {

// Base class for the source of characters for the Lexer.
class CharSource {
public:
	// Returned to the Lexer when the buffer is empty. \0
	static const string::TChar Zero;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual ~CharSource() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the character at the current position and advances to the next character.
	// Returns 0 if no more characters are available.
	virtual string::TChar* NextChar() = 0;

	// Returns the character at the current position.
	virtual string::TChar* PeekChar(int count = 0) const = 0;

	// Moves the pointer back 'count' times.
	virtual string::TChar* GoBack(int count = 1) = 0;

	// Skip over 'count' characters.
	virtual string::TChar* Skip(int count = 1) = 0;

	// Returns the current position.
	virtual int Position() const = 0;

	// Sets the current position.
	virtual void SetPosition(int value) = 0;

	// Returns 'true' if the information about the current location needs to be restored.
	virtual bool NeedsStateRestore() {
		// By default no restoration is needed.
		return false;
	}

	// Returns 'true' if the current file should be popped from the include stack
	// after the last character in the source has been processed.
	virtual bool ShouldPopInclude() {
		return false;
	}
};


// Source that reads characters from a file buffer.
class FileCharSource : public CharSource {
private:
	shared<FileBuffer> buffer_;
	string::TChar* begin_;
	string::TChar* end_;
	int position_;

public:
	FileCharSource(shared<FileBuffer> buffer) : buffer_(buffer), position_(0) {
		begin_ = buffer->Buffer();
		end_ = buffer->BufferEnd();
		buffer->Reset();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool NeedsStateRestore() override {
		return true;
	}

	virtual bool ShouldPopInclude() override {
		return true;
	}

	virtual string::TChar* NextChar() override {
		if(begin_ + position_ >= end_) {
			return (string::TChar*)&CharSource::Zero;
		}
		else return begin_ + position_++;
	}

	virtual string::TChar* PeekChar(int count = 0) const override {
		if(begin_ + position_ + count >= end_) {
			return (string::TChar*)&CharSource::Zero;
		}
		else return begin_ + position_ + count;
	}

	virtual string::TChar* GoBack(int count = 1) override {
		DebugValidator::IsLargerOrEqual(position_ - count, 0);
		position_ -= count;
		return PeekChar();
	}

	virtual string::TChar* Skip(int count = 1) override {
		position_ += count;
		return PeekChar();
	}

	virtual int Position() const override {
		return position_;
	}

	virtual void SetPosition(int value) override {
		position_ = value;
	}
};


// Source of characters provided by the preprocessor.
class MacroCharSource : public CharSource {
private:
	string data_;
	string::TChar* begin_;
	string::TChar* end_;
	int position_;

public:
	MacroCharSource(const string& data) : data_(data), position_(0) {
		begin_ = data_.Chars();
		end_ = data_.Chars() + data_.Length();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual string::TChar* NextChar() override {
		if(begin_ + position_ >= end_) {
			return (string::TChar*)&CharSource::Zero;
		}
		else return begin_ + position_++;
	}

	virtual string::TChar* PeekChar(int count = 0) const override {
		if(begin_ + position_ + count >= end_) {
			return (string::TChar*)&CharSource::Zero;
		}
		else return begin_ + position_ + count;
	}

	virtual string::TChar* GoBack(int count = 1) override {
		DebugValidator::IsLargerOrEqual(position_ - count, 0);
		position_ -= count;
		return PeekChar();
	}

	virtual string::TChar* Skip(int count = 1) override {
		position_ += count;
		return PeekChar();
	}

	virtual int Position() const override {
		return position_;
	}

	virtual void SetPosition(int value) override {
		position_ = value;
	}
};

} // namespace Lexing
#endif