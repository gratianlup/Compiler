// Lexer.hpp
// Copyright (c) Lup Gratian
//
// Defines the Lexer. This is the main class of the lexing module.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_LEXER_HPP
#define PC_LEXING_LEXER_HPP

#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/List.hpp"
#include "../Base/Stack.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Common/FileManager.hpp"
#include "../Common/LocationInfo.hpp"
#include "../Common/Diagnostic.hpp"
#include "../Common/TargetData.hpp"
#include "CharSource.hpp"
#include "Token.hpp"
#include "Preprocessor.hpp"
#include "TokenSource.hpp"
#include "IdentifierTable.hpp"
using namespace Base;
using namespace Common;

namespace Lexing {

class Lexer {
private:
	typedef string::TChar T;

	// Used to save information about the current file before #including another file.
	// When the #included file is finished this information is restored.
	struct SourceInfo {
		FileId File;
		int Line;
		string Name;
		T* LineStart;
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Character type table; used for the methods that test the type of a character.
	// charType_[i] = FLAG1 | FLAG2 | ...
	static const char CHAR_WHITESPACE = 1;
	static const char CHAR_LETTER     = 2;
	static const char CHAR_DIGIT      = 4;
	static const char CHAR_NUMBER     = 8;
	static const char CHAR_IDENTIFIER = 16;
	static const char charType_[];

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const TargetData* target_;
	Diagnostic* diag_;
	FileId file_;           // The current file.
	int line_;              // The current line.
	string name_;           // The name of the source file.
	bool inPP_;             // 'true' if in preprocessor mode.
	bool isLineBegin_;      // 'true' if no tokens have been found on the line yet.
	T* lineStart_;          // The position of the current line start.
	T* current_;            // The current character.
	bool noExpand_;         // 'true' if identifiers should not be expanded.
	bool tokSourceEnabled_; // 'true' if the token source should be first considered.
	bool returnEOF_;        // 'true' if EOF should be returned as a token.
	bool returnNewLine_;    // 'true' if 'new line' should be returned as a token.
	bool inInclude_;        // 'true' if scanning an #include directive.
	bool hasCustomChar_;    // 'true' if a custom character should be considered valid.
	T customChar_;          // The custom character.
	bool usePP_;            // 'true' if the preprocessor should be invoked.
    Token peekedToken_;
    bool hasPeekedToken_;

	Stack<shared<CharSource>> sourceStack_;     // The stack of source objects.
	Stack<shared<TokenSource>> tokSourceStack_; // The stack of source objects.
	Stack<SourceInfo> sourceState_;             // The stack with information about the #included files.
	shared<TokenSource> tokSource_;             // The current token source.
	shared<CharSource> source_;                 // The current source.
	shared<Preprocessor> pp_;                   // The associated preprocessor.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for testing the type of a character.
	bool IsDigit(T value) const {
		return ((int)charType_[value] & (int)CHAR_DIGIT) != 0;
	}

	bool IsWhitespace(T value) const {
		return ((int)charType_[value] & (int)CHAR_WHITESPACE) != 0;
	}

	bool IsLetter(T value) const {
		return ((int)charType_[value] & (int)CHAR_LETTER) != 0;
	}

	bool IsNumberChar(T value) const {
		return ((int)charType_[value] & (int)CHAR_NUMBER) != 0;
	}

	bool IsIdentifierChar(T value) const {
		return ((int)charType_[value] & (int)CHAR_IDENTIFIER) != 0;
	}

	// Skips over all characters until the end of the line/document is reached.
	void SkipLineComment();

	// Skip all characters until the end of the comment */ is found.
	void SkipMultilineComment();

	// If the current character is an escaped newline skips to the next line.
	T* SkipEscapedNewline(T* ch);

	// Scans for an integer or floating number and creates the corresponding token.
	void ScanNumber(Token& token);

	// Scans for a string until 'stopChar' is found.
	// // If the end of string marker is not found until the end of line/document
	// is reached an error is reported. 
	// Used for scanning strings, characters and include strings.
	bool ScanStringLike(T stopChar, string& result, int& length, bool& hasEscaped);

	// Scans for a string and creates the corresponding token.
	void ScanString(Token& token, bool wide);

	// Scans for a character constant and creates the corresponding token.
	void ScanChar(Token& token, bool wide);

	// Scans for an include directive string <> and creates a string token.
	void ScanInclude(Token& token, bool wide);

	// Scans for an identifier and creates the appropriate token.
	void ScanIdentifier(Token& token);

	// Makes a new token having the specified properties.
	void MakeToken(TokenKind kind, T* begin, int length, Token& token);

	// Returns the next token from the current character source.
	// Handles special cases like returning end-of-line, EOF (used by the Preprocessor).
	void NextTokenImpl(Token& token);

	// Obtains a token from the active token source, if one is available.
	bool GetFromTokenSource(Token& token);

	// Enables the macro definition associated with the specified token source.
	void EnableTokenSourceMacro(TokenSource* source);

public:
	Lexer(IdentifierTable* table, const TargetData* target, 
		  FileManager* manager, Diagnostic* diagnostic, bool usePP = true);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the next token. EOF is returned when no more tokens are available.
	void NextToken(Token& token);

	// Reads until the end of line is found. Used by the #error directive.
	string ReadLine();

	// Sets the current line. Used by the #line directive.
	void SetLine(int value) {
		line_ = value;
	}

	// Returns the ID of the current file.
	FileId File() const {
		return file_;
	}

	void SetFile(FileId value) {
		file_ = value;
	}

	// Returns the current file name.
	string Name() const {
		return name_;
	}

	// Sets the current file name. Used by the #line directive.
	void SetName(const string& value) {
		name_ = value;
	}

	// Returns the current location in the scanned source file.
	LocationInfo Location() const {
		return LocationInfo(file_, line_, (int)(current_ - lineStart_));
	}

	// Returns the location of the first character on the current line.
	LocationInfo LineStart() const {
		return LocationInfo(file_, line_, 0);
	}

	// Pushes the specified character source on the stack.
	// The current character is pushed back on it's source, and the new
	// source is made active. The old one is pushed on the stack.
	void PushSource(shared<CharSource> source);

	// Pushes the specified token source on the stack.
	// If no token source is active this one is made the active one.
	void PushTokenSource(shared<TokenSource> source);

	// Pops a character source from the stack. The current character is
	// restored to the one that was before the source was pushed on the stack.
	bool PopSource();

	// Pops a token source from the stack.
	bool PopTokenSource();

	// Saves the state of the current source so that it can be restored
	// when the #included file is processed.
	void SaveSourceState();

	// Puts the specified token back in the source.
	// Handles both CharSource and TokenSource.
	void PutBack(const Token& token);

	bool LoadStart(const string& path);

	// Return the flag that indicates if macro expansion is enabled.
	bool NoExpand() const {
		return noExpand_;
	}

	void SetNotExpand(bool value) {
		noExpand_ = value;
	}

	// Returns true if the Lexer is allowed to read from the active TokenSource.
	bool TokenSourceEnabled() const {
		return tokSourceEnabled_;
	}

	void SetTokenSourceEnabled(bool value) {
		tokSourceEnabled_ = value;
	}

	// Returns true if the Lexer should return EOF as a token.
	// Used when expanding macros.
	bool ReturnEOF() const {
		return returnEOF_;
	}

	void SetReturnEOF(bool value) {
		returnEOF_ = value;
	}

	// Returns true if a preprocessor directive is being scanned.
	bool InPP() {
		return inPP_;
	}

	void SetInPP(bool value) {
		inPP_ = value;
	}

	// Returns true if an #include statement is being scanned.
	bool InInclude() {
		return inInclude_;
	}

	void SetInInclude(bool value) {
		inInclude_ = value;
	}

	// Returns true if the Lexer is allowed to produce new-line tokens.
	// Used scanning preprocessor directives.
	bool ReturnNewLine() {
		return returnNewLine_;
	}

	void SetReturnNewLine(bool value) {
		returnNewLine_ = value;
	}

	// The target platform.
	const TargetData* Target() {
		return target_;
	}

	// Returns 'true' if a custom character is used. 
	// Can be used to recognize a// character that is not part of the C character set.
	bool HasCustomChar() const {
		return hasCustomChar_;
	}

	void SetHasCustomChar(bool value) {
		hasCustomChar_ = value;
	}

	// Returns the custom character.
	T CustomChar() const {
		return customChar_;
	}

	void SetCustomChar(T value) {
		customChar_ = value;
	}
};

} // namespace Lexing
#endif