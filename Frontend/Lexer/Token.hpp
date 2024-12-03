// Token.hpp
// Copyright (c) Lup Gratian
//
// Defines the tokens used by the Lexer.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_TOKENS_HPP
#define PC_LEXING_TOKENS_HPP

#include "../Base/String.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Common/LocationInfo.hpp"
using namespace Base;
using namespace Common;

namespace Lexing {

// Definitions for token types.
// Some tokens (identifier, string, number, etc.) have additional associated data.
enum class TokenKind {
	Identifier,       // -> NameData
	Number,           // -> NumberData
	String,           // -> StringData
	Char,             // -> StringData
	Add,              // +
	Inc,              // ++
	AddEq,            // +=
	Sub,              // -
	Dec,              // --
	SubEq,            // -=
	Arrow,            // ->
	Mul,              // *
	MulEq,            // *=
	DivEq,            // /=
	Tilde,            // ~
	Eq,               // =
	EqEq,             // ==
	Not,              // !
	NotEq,            // !=
	Mod,              // %
	ModEq,            // %=
	And,              // &
	AndEq,            // &=
	AndAnd,           // &&
	Or,               // |
	OrEq,             // |=
	OrOr,             // ||
	Xor,              // ^
	XorEq,            // ^=
	Less,             // <
	LessEq,           // <=
	ShiftL,           // <<
	ShiftLEq,         // <<=
	Greater,          // >
	GreaterEq,        // >=
	ShiftR,           // >>
	ShiftREq,         // >>=
	Hash,             // #  (used by the preprocessor)
	HashHash,         // ## (used by the preprocessor)
	HashHashDisabled,
	Div,              // / 
	Colon,            // :
	SemiColon,        // ;
	Comma,            // ,
	Dot,              // .
	Question,         // ?
	OpenSquare,       // [
	CloseSquare,      // ]
	OpenParen,        // (
	CloseParen,       // )
	OpenCurly,        // {
	CloseCurly,       // }
	Ellipsis,         // ...
	LineEnd,          // For the preprocessor.
	FileEnd,          // The end of the file has been reached.
	Invalid,          // The token could not be determined.
	Skip,             // Used to signal the Lexer that it should take tokens
		              // from the new 'TokenSource'.
	Custom,           // Used to mark a custom (optional) token.
	Pragma,           // Used to implement pragma.
	KEYWORD_START     // All values after this one denote tokens.
};


// Various flags that can be attached to a token.
enum class TokenFlags {
	None             = 0,
	AfterWhitespace  = 1 << 0, // Set if the token is preceded by white-space characters.
	AtLineStart      = 1 << 1, // Set if it's the token begins exactly where the line starts.
	SystemInclude    = 1 << 2, // Set if the token originates from a system library file.
	DisableExpansion = 1 << 3  // Set if the identifier should not be expanded.
};


// Used by keyword and identifier tokens.
struct NameData {
	string Name;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	NameData(const string& name) : Name(name) {}
	
	// For debugging only.
	string ToString() const {
		return Name;
	}
};


// Used by number tokens.
struct NumberData {
	string Number;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	NumberData(const string& number) : Number(number) {}
	
	// For debugging only.
	string ToString() const {
		return Number;
	}
};


// Used by string tokens.
struct StringData {
	string Value;
	bool IsWide;
	bool HasEscaped;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StringData(const string& value, bool wide = false, bool escaped = false) :
			Value(value), IsWide(wide), HasEscaped(escaped) {}

	// For debugging only.
	string ToString() const {
		return Value;
	}
};


// Tokens are identified by their type ('TokenKind') and the associated data
// (if needed). The location of the token is recorded in a 'LocationInfo' object.
class Token {
private:
	shared<char> data_;     // Optional data associated with the token.
	LocationInfo location_; // Where the token starts.
	int length_;            // The length of the token (in characters).
	TokenKind kind_;        // The type of the token.
	TokenFlags flags_;      // Various flags.

public:
	Token() : kind_(TokenKind::Invalid), flags_(TokenFlags::None), length_(0) {}
	
	Token(TokenKind kind) : kind_(kind), flags_(TokenFlags::None), length_(0) {}

	Token(TokenKind kind, const LocationInfo& location) : 
			kind_(kind), location_(location), flags_(TokenFlags::None), length_(0) {}

	Token(const Token& other) :
			kind_(other.kind_), data_(other.data_), location_(other.location_),
			length_(other.length_), flags_(other.flags_) {}

	~Token() {
		ClearData();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the type of the token.
	TokenKind Kind() const {
		return kind_;
	}

	// Sets the type of the token.
	void SetKind(TokenKind value) {
		kind_ = value;
	}

	// Returns the data associated represents the token.
	NumberData* NumberValue() { 
		return reinterpret_cast<NumberData*>(data_.Raw()); 
	}

	NameData* NameValue() { 
		return reinterpret_cast<NameData*>(data_.Raw());   
	}

	StringData* StringValue() { 
		return reinterpret_cast<StringData*>(data_.Raw()); 
	}

	const NumberData* NumberValue() const {
		return reinterpret_cast<const NumberData*>(data_.Raw()); 
	}

	const NameData* NameValue() const { 
		return reinterpret_cast<const NameData*>(data_.Raw());   
	}

	const StringData* StringValue() const { 
		return reinterpret_cast<const StringData*>(data_.Raw()); 
	}

	// Sets the data associated represents the token.
	void SetData(void* value) {
		ClearData();
		data_ = value;
	}

	// Removes the data associated with this token.
	void ClearData() {
		if(data_ == nullptr || (*data_.Counter() != 1)) return;

		if(kind_ == TokenKind::Number) {
			delete NumberValue();
		}
		else if((kind_ == TokenKind::String) || (kind_ == TokenKind::Char)) {
			delete NameValue();
		}
		else if(kind_ == TokenKind::Identifier) {
			delete StringValue();
		}

		data_.Get();
	}

	// Returns the location where the token was found.
	LocationInfo Location() const {
		return location_;
	}

	// Sets the location where the token was found.
	void SetLocation(const LocationInfo& value) {
		location_ = value;
	}

	// Returns the length of the token (not necessarily equal to lexeme length).
	int Length() const {
		return length_;
	}

	// Sets the length of the token.
	void SetLength(int value) {
		length_ = value;
	}

	// Returns true if the token is valid.
	bool IsValid() const {
		return (kind_ != TokenKind::FileEnd) && 
			   (kind_ != TokenKind::Invalid);
	}

	// Returns true if the token describes and end-of-file situation.
	bool IsEOF() const {
		return kind_ == TokenKind::FileEnd;
	}
	
	// Returns true if the token is intended to be used by the preprocessor.
	bool IsMacro() const {
		return kind_ == TokenKind::Hash;
	}

	// Returns true if the token represents the '_Pragma' keyword.
	bool IsPragma() const {
		return kind_ == TokenKind::Pragma;
	}

	// Returns true if the token marks the end of a line. Used by the preprocessor.
	bool IsLineEnd() const {
		return kind_ == TokenKind::LineEnd;
	}

	// Returns true if the token represents an operator.
	bool IsOperator() const {
		return ((int)kind_ >= (int)TokenKind::Add) &&
			   ((int)kind_ <= (int)TokenKind::Div);
	}

	// Returns true if the token represents a keyword.
	bool IsKeyword() const {
		return (int)kind_ >= (int)TokenKind::KEYWORD_START;
	}

	template <class T>
	T AsKeyword() const {
		// The first keyword comes after 'KEYWORD_START'.
		return (T)((int)kind_ - (int)TokenKind::KEYWORD_START);
	}

	void SetKeyword(int keyword, int length) {
		kind_ = (TokenKind)((int)TokenKind::KEYWORD_START + keyword);
		length_ = length;
	}

	// Returns true if the token represents an identifier.
	bool IsIdentifier() const {
		return kind_ == TokenKind::Identifier;
	}

	// Returns true if the token represents a number.
	bool IsNumber() const {
		return kind_ == TokenKind::Number;
	}

	// Returns true if the token represents a string.
	bool IsString() const {
		return kind_ == TokenKind::String;
	}

	// Returns true if the token represents character constant.
	bool IsChar() const {
		return kind_ == TokenKind::Char;
	}

	// Returns true if the Lexer should consider another TokenSource.
	bool Skip() const {
		return kind_ == TokenKind::Skip;
	}

	// Verifies if the specified flag is set.
	bool HasFlag(TokenFlags flag) const {
		return ((int)flags_ & (int)flag) != 0;
	}

	// Sets the specified flag.
	void SetFlag(TokenFlags flag) {
		flags_ = (TokenFlags)((int)flags_ | (int)flag);
	}

	// Resets the specified flag.
	void ResetFlag(TokenFlags flag) {
		flags_ = (TokenFlags)((int)flags_ & ~((int)flag));
	}

	// Returns 'true' if the token is allowed to be expanded by the preprocessor.
	bool CanExpand() const {
		return HasFlag(TokenFlags::DisableExpansion) == false;
	}

	// For debugging only.
	string ToString() const {
		if(IsIdentifier()) {
			return NameValue()->ToString();
		}
		else if(IsString()) {
			return StringValue()->ToString();
		}
		else if(IsNumber()) {
			return NumberValue()->ToString();
		}
		else if(IsKeyword()) {
			return "KEYWORD";
		}
		else if(IsOperator()) {
			return "OPERATOR";
		}
		else return "PUNCTUATOR";
	}
};

} // namespace Lexing
#endif