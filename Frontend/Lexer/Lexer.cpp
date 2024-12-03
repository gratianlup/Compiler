// Lexer.hpp
// Copyright (c) Lup Gratian
//
// Implements the Lexer.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Lexer.hpp"

namespace Lexing {

// Constants with flags that specify the type of a character.
const char Lexer::charType_[] = {
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    1,    1,    1,    1,    1,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   1,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    8,    0, 
	  28,   28,   28,   28,   28,   28,   28,   28, 
	  28,   28,    0,    0,    0,    0,    0,    0, 
	   0,   26,   26,   26,   26,   26,   26,   26, 
	  26,   26,   26,   26,   26,   26,   26,   26, 
	  26,   26,   26,   26,   26,   26,   26,   26, 
	  26,   26,   26,    0,    0,    0,    0,   26, 
	   0,   26,   26,   26,   26,   26,   26,   26, 
	  26,   26,   26,   26,   26,   26,   26,   26, 
	  26,   26,   26,   26,   26,   26,   26,   26, 
	  26,   26,   26,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0, 
	   0,    0,    0,    0,    0,    0,    0,    0
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::SkipLineComment() {
	bool done = false;
	bool escaped = false;

	while(done == false) {
		if(escaped) {
			// We had an escaped newline inside a comment, like in '// abc \'.
			// The next line must also be a line comment; if not emit an error.
			// Skip whitespace and check that this line begins with //.
			while(IsWhitespace(*current_)) {
				current_ = source_->NextChar();
			}

			if((*current_ != '/') && (*source_->PeekChar() != '/')) {
				diag_->Report(Error::COMMENT_ESCAPED_NOT_COMMENT)<<Location();
				return;
			}

			escaped = false;
		}

		// We must take care about escaped new lines \.
		while((*current_ != 0) &&
			  (*current_ != '\\') &&
			  (*current_ != '\n') &&
			  (*current_ != '\r')) {
			current_ = source_->NextChar();
		}

		// If we stopped at a new line we are done.
		if((*current_ == '\n') || (*current_ == '\r')) {
			done = true;
		}
		else if(*current_ == '\\') {
			current_ = source_->NextChar();

			// If new-line follows after \ then it's an escape sequence.
			if((*current_ == '\n') || (*current_ == '\r')) {
				// Set the flag to check for // at the beginning of the next line.
				escaped = true;
				line_++;
			}
		}
		else {
			// Reached end of document.
			done = true;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::SkipMultilineComment() {
	// Search for / and verify that the character before is *.
	if(*current_ == '/') {
		// A comment that begins with /*/ does not end on the second /.
		current_ = source_->NextChar();
	}
	
	while(true) {
		if(*current_ == '/') {
			// Check if the previous character is *.
			current_ = source_->GoBack(2);

			if(*current_ == '*') {
				// Found!
				current_ = source_->Skip(2);
				current_ = source_->NextChar();
				return;
			}
			else current_ = source_->Skip(2);
		}
		else if(*current_ == 0) {
			// End of file reached, emit an error.
			diag_->Report(Error::COMMENT_MULTILINE_NOT_ENDED)<<Location();
			return;
		}
		else if(*current_ == '\r') {
			// Count the new line.
			line_++;
			lineStart_ = current_ + 1;
		}

		current_ = source_->NextChar();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lexer::T* Lexer::SkipEscapedNewline(T* ch) {
	// If after \ there is no whitespace return \.
	if(IsWhitespace(*current_) == false) {
		return ch;
	}

	// Skip to the next line. We need to make sure that \n\n is considered
    // as two separate lines.
	while(IsWhitespace(*current_)) {
        bool hasN = *current_ == '\n';
        bool hasR = *current_ == '\r';

		if(hasN || hasR) {
			current_ = source_->NextChar();

			// Check for \n\r or \r\n.
			if(((*current_ == '\n') && (hasN == false)) || (*current_ == '\r')) {
				current_ = source_->NextChar();
			}

			line_++;
			lineStart_ = current_;
			return current_;
		}
	}

	return ch;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::ScanNumber(Token& token) {
	StringBuilder sb;
	T* previous = source_->NextChar();
	current_ = previous;
	T* start = current_;

	while(IsNumberChar(*current_)) {
		sb.Append(*current_);
		previous = current_;
		current_ = source_->NextChar();
	}

	// See if this is a floating point number with scientific notation (123E+456).
	if(((*current_ == '+') || (*current_ == '-')) &&
		((*previous == 'E') || (*previous == 'e'))) {
		// Read the rest of the number.
		sb.Append(*current_); // Append + or -.
		current_ = source_->NextChar();
				
		while(IsNumberChar(*current_)) {
			sb.Append(*current_);
			current_ = source_->NextChar();
		}
	}
	else if(((*current_ == '+') || (*current_ == '-')) &&
			((*previous == 'P') || (*previous == 'p'))) {
		// Read the rest of the hex floating point number.
		sb.Append(*current_); // Append + or -.
		current_ = source_->NextChar();
				
		while(IsNumberChar(*current_)) {
			sb.Append(*current_);
			current_ = source_->NextChar();
		}
	}
	
	// Make the token.
	MakeToken(TokenKind::Number, start, sb.Length(), token);
	token.SetData(new NumberData(sb.ToString()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Lexer::ScanStringLike(T stopChar, string& result, int& length, bool& hasEscaped) {
	StringBuilder sb;
	hasEscaped = false;
	current_ = source_->NextChar();
	T* start = current_;
	length = 0;

	while(*current_ != stopChar) {
		if(*current_ == '\\') {
			// Skip over escape sequences (they will be handled later).
			current_ = source_->NextChar();
			
			// This could be an escaped newline. In this case we need to continue
			// after the current line.
			T* last = current_;

			if(SkipEscapedNewline(current_) != last) {
				continue;
			}
			
			// Append the \.
			sb.Append('\\');
			hasEscaped = true;
		}
		else if((*current_ == '\r') || (*current_ == '\n') || (*current_ == 0 /* EOF */)) {
			// Reached end of line or document before the string was ended.
			diag_->Report(Error::STRING_END_MARKER_EXPECTED)<<Location();
			return false;
		}

		sb.Append(*current_);
		current_ = source_->NextChar(); // Advance to next character.
		length++;
	}

	// Make the token.
	result = sb.ToString();
	current_ = source_->NextChar(); // Skip over ".
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::ScanString(Token& token, bool wide) {
	T* start = current_;
	string value;
	int length;
	bool hasEscaped;

	if(ScanStringLike('"', value, length, hasEscaped) == false) {
		MakeToken(TokenKind::Invalid, start, 0, token);
	}
	else {
		MakeToken(TokenKind::String, start, length, token);
		token.SetData(new StringData(value, wide, hasEscaped));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::ScanChar(Token& token, bool wide) {
	T* start = current_;
	string value;
	int length;
	bool hasEscaped;

	if(ScanStringLike('\'', value, length, hasEscaped) == false) {
		MakeToken(TokenKind::Invalid, start, 0, token);
	}
	else {
		MakeToken(TokenKind::Char, start, length, token);
		token.SetData(new StringData(value, wide));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::ScanInclude(Token& token, bool wide) {
	T* start = current_;
	string value;
	int length;
	bool hasEscaped;

	if(ScanStringLike('>', value, length, hasEscaped) == false) {
		MakeToken(TokenKind::Invalid, start, 0, token);
	}
	else {
		MakeToken(TokenKind::String, start, length, token);
		token.SetData(new StringData(value, wide, hasEscaped));
		token.SetFlag(TokenFlags::SystemInclude);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::ScanIdentifier(Token& token) {
	T* current = source_->NextChar();
	T* start = current;
	int length = 0;

	while(IsIdentifierChar(*current)) {
		current = source_->NextChar(); // Advance to next character.
		length++;
	}

	// If the identifier is actually a keyword or a preprocessor definition
	// it will be discovered later and handled appropriately.
	MakeToken(TokenKind::Identifier, start, length, token);
	token.SetData(new NameData(string(start, length)));
	current_ = current; // Set start position for next token.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::MakeToken(TokenKind kind, T* begin, int length, Token& token) {
	token = Token(kind, LocationInfo(file_, line_, (int)(begin - lineStart_)));
	token.SetLength(length);

	// Set flags.
	if(line_ > 0 || (begin != lineStart_)) {
		string::TChar wsChar = *(begin - 1);
		
		if((wsChar == ' ')  || (wsChar == '\t') || (wsChar == '\v') ||
		   (wsChar == '\f') || (wsChar == '\n') || (wsChar == '\f')) {
			token.SetFlag(TokenFlags::AfterWhitespace);
		}
	}

	if(isLineBegin_) {
		token.SetFlag(TokenFlags::AtLineStart);
		isLineBegin_ = false;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::NextTokenImpl(Token& token) {
	string::TChar* ch = current_;

	// If there is no character source return EOF.
	if(source_ == nullptr) {
		MakeToken(TokenKind::FileEnd, ch, 1, token);
		return;
	}

	while(true) { // Scan until a valid token is found.
		current_ = source_->NextChar(); // Advance to the next character.

		switch(*ch) {
			case 0: { // EOF
				// The end of the current source has been reached.
				if(inPP_ || returnNewLine_) {
					// We are in preprocessor mode. Notify that the end of
					// the line was reached end exit the mode.
					MakeToken(TokenKind::LineEnd, ch, 1, token);
					inPP_ = false;
					return;
				}
				else {
					MakeToken(TokenKind::FileEnd, ch, 1, token);
					return;
				}
			}
			case ' ': break;
			case 13: break;
			case '\n': {
				line_++;
				isLineBegin_ = true;
				lineStart_ = current_;

				if(inPP_ || returnNewLine_) {
					// We are in preprocessor mode. Notify that the end of
					// the line was reached end exit the mode.
					MakeToken(TokenKind::LineEnd, ch, 1, token);
					isLineBegin_ = true; // A new line begins.
					return;
				}

				break;
			}
			case '\\': {
				// This could be an escaped newline.
				while(true) {
					// Skip over all escaped newlines.
					T* temp = SkipEscapedNewline(ch);
					if(temp == ch) {
						// This is not an escaped newline.
						MakeToken(TokenKind::Invalid, ch, 1, token);
						return;
					}
					else if(*temp == 0) {
						// Reached end of document; will be handled above.
						break;
					}
					else break;
				}
			}
			case '\t': break; // Tab.
			case '\v': break; // Vertical tab.
			case '\f': break; // Form feed.
			case '0': case '1': case '2': case '3': case '4': case '5':
			case '6': case '7': case '8': case '9': {
				// Found the start of a number.
				source_->GoBack(2);
				ScanNumber(token);
				return;
			}
			case 'L': {
				// This could be the start of a wide character string or constant.
				if(*current_ == '"') {
					// A wide string is starting.
					ScanString(token, true);
					return;
				}
				else if(*current_ == '\'') {
					// A wide character constant is starting.
					ScanChar(token, true);
					return;
				}

				// Fall through to identifier/keyword.
			}
			case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': 
			case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
			case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
			case 's': case 't': case 'u': case 'v': case 'w': case 'x':
			case 'y': case 'z':
			case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': 
			case 'G': case 'H': case 'I': case 'J': case 'K': /* L handled above */
			case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
			case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
			case 'Y': case 'Z': case '_': {
				// Found the start of an identifier or a keyword.
				source_->GoBack(2);
				ScanIdentifier(token);
				return;
			}
			case '"': {
				// Found the start of a string.
				source_->GoBack();
				ScanString(token, false);
				return;
			}
			case '\'': {
				// Found the start of a character constant.
				source_->GoBack();
				ScanChar(token, false);
				return;
			}
			case '(': {
				MakeToken(TokenKind::OpenParen, ch, 1, token);
				return;
			}
			case ')': {
				MakeToken(TokenKind::CloseParen, ch, 1, token);
				return;
			}
			case '[': {
				MakeToken(TokenKind::OpenSquare, ch, 1, token);
				return;
			}
			case ']': {
				MakeToken(TokenKind::CloseSquare, ch, 1, token);
				return;
			}
			case '{': {
				MakeToken(TokenKind::OpenCurly, ch, 1, token);
				return;
			}
			case '}': {
				MakeToken(TokenKind::CloseCurly, ch, 1, token);
				return;
			}
			case '?': {
				MakeToken(TokenKind::Question, ch, 1, token);
				return;
			}
			case ':': {
				MakeToken(TokenKind::Colon, ch, 1, token);
				return;
			}
			case ';': {
				MakeToken(TokenKind::SemiColon, ch, 1, token);
				return;
			}
			case ',': {
				MakeToken(TokenKind::Comma, ch, 1, token);
				return;
			}
			case '.': {
				// Check if it's the start of a floating value or it's an ellipsis.
				if(IsDigit(*current_)) {
					source_->GoBack(); // Go back one character.
					ScanNumber(token);
					return;
				}
				else if((*current_ == '.') && (*source_->PeekChar() == '.')) {
					// Ellipsis (...).
					MakeToken(TokenKind::Ellipsis, ch, 3, token);
					current_ = source_->Skip();
					source_->Skip();
					return;
				}

				// Dot.
				MakeToken(TokenKind::Dot, ch, 1, token);
				return;
			}
			case '&': {
				if(*current_ == '&') { // &&
					MakeToken(TokenKind::AndAnd, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else if(*current_ == '=') { // &=
					MakeToken(TokenKind::AndEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // &
					MakeToken(TokenKind::And, ch, 1, token);
					return;
				}
			}
			case '|': {
				if(*current_ == '|') { // ||
					MakeToken(TokenKind::OrOr, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else if(*current_ == '=') { // |=
					MakeToken(TokenKind::OrEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // |
					MakeToken(TokenKind::Or, ch, 1, token);
					return;
				}
			}
			case '^': {
				if(*current_ == '=') { // ^=
					MakeToken(TokenKind::XorEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else {
					MakeToken(TokenKind::Xor, ch, 1, token);
					return;
				}
			}
			case '<': {
				if(inInclude_) {
					// In #include mode '<' starts system header names.
					source_->GoBack(2);
					current_ = source_->NextChar();
					ScanInclude(token, false);
					return;
				}
				else if(*current_ == '=') { // <=
					MakeToken(TokenKind::LessEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else if(*current_ == '<') {
					if(*source_->PeekChar() == '=') { // <<=
						MakeToken(TokenKind::ShiftLEq, ch, 3, token);
						current_ = source_->Skip();
						source_->Skip();
						return;
					}
					else { // <<
						MakeToken(TokenKind::ShiftL, ch, 2, token);
						current_ = source_->NextChar();
						return;
					}
				}
				else { // <
					MakeToken(TokenKind::Less, ch, 1, token);
					return;
				}
			}
			case '>': {
				if(*current_ == '=') { // >=
					MakeToken(TokenKind::GreaterEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else if(*current_ == '>') {
					if(*source_->PeekChar() == '=') { // >>=
						MakeToken(TokenKind::ShiftREq, ch, 3, token);
						current_ = source_->Skip();
						source_->Skip();
						return;
					}
					else { // >>
						MakeToken(TokenKind::ShiftR, ch, 2, token);
						current_ = source_->NextChar();
						return;
					}
				}
				else { // >
					MakeToken(TokenKind::Greater, ch, 1, token);
					return;
				}
			}
			case '+': {
				if(*current_ == '+') { // ++
					MakeToken(TokenKind::Inc, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else if(*current_ == '=') { // +=
					MakeToken(TokenKind::AddEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // +
					MakeToken(TokenKind::Add, ch, 1, token);
					return;
				}
			}
			case '-': {
				if(*current_ == '-') { // --
					MakeToken(TokenKind::Dec, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else if(*current_ == '=') { // -=
					MakeToken(TokenKind::SubEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else if(*current_ == '>') { // ->
					MakeToken(TokenKind::Arrow, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // +
					MakeToken(TokenKind::Sub, ch, 1, token);
					return;
				}
			}
			case '*': {
				if(*current_ == '=') { // *=
					MakeToken(TokenKind::MulEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // *
					MakeToken(TokenKind::Mul, ch, 1, token);
					return;
				}
			}
			case '/': {
				if(*current_ == '/') {
					current_ = source_->Skip(1); // Skip over second /.
					SkipLineComment();
					break;
				}
				else if(*current_ == '*') {
					current_ = source_->Skip(1); // Skip over second /.
					SkipMultilineComment();
					break;
				}
				else if(*current_ == '=') { // /=
					MakeToken(TokenKind::DivEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // /
					MakeToken(TokenKind::Div, ch, 1, token);
					return;
				}
			}
			case '~': {
				MakeToken(TokenKind::Tilde, ch, 1, token);
				return;
			}
			case '!': {
				if(*current_ == '=') { // !=
					MakeToken(TokenKind::NotEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // !
					MakeToken(TokenKind::Not, ch, 1, token);
					return;
				}
			}
			case '%': {
				if(*current_ == '=') { // %=
					MakeToken(TokenKind::ModEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // %
					MakeToken(TokenKind::Mod, ch, 1, token);
					return;
				}
			}
			case '=': {
				if(*current_ == '=') { // ==
					MakeToken(TokenKind::EqEq, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else { // =
					MakeToken(TokenKind::Eq, ch, 1, token);
					return;
				}
			}
			case '#': {
				if(*current_ == '#') { // ##
					MakeToken(TokenKind::HashHash, ch, 2, token);
					current_ = source_->NextChar();
					return;
				}
				else {
					// A directive is starting. This will be handled by the preprocessor.
					MakeToken(TokenKind::Hash, ch, 1, token);
					return;
				}
			}
			default: {
				// This is an invalid character; emit a warning and an invalid token.
				if(hasCustomChar_ && (*ch == customChar_)) {
					// If the client registered a special character that
					// should be considered part of an identifier we handle it here.
					MakeToken(TokenKind::Custom, ch, 1, token);
					break;
				}
				else {
					diag_->Report(Warning::CHARACTER_INVALID)<<Location()<<*ch;
					MakeToken(TokenKind::Invalid, ch, 1, token);
					return;
				}
			}
		}

		ch = current_;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lexer::Lexer(IdentifierTable* table, const TargetData* target, 
			 FileManager* manager, Diagnostic* diagnostic, bool usePP) : 
		usePP_(usePP), target_(target), diag_(diagnostic), 
        hasCustomChar_(false), hasPeekedToken_(false) {
	// Create the preprocessor now.
	pp_ = new Preprocessor(this, table, new HeaderLookup(this, manager), diagnostic);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Lexer::GetFromTokenSource(Token& token) {
	// Test token sources (starting with the active one) until we get a token,
	// or we run out of token sources.
	while(true) {
		if(tokSource_ && tokSourceEnabled_) {
			Token* temp = tokSource_->NextToken();

			if(temp == nullptr) {
				// Check if there are other token sources on the stack.
				if(PopTokenSource()) {
					continue; // Try again.
				}
			}
			else if(temp->Skip()) {
				// There was a paste (##) operator that generated a macro.
				// A new TokenSource has been pushed on the stack containing the expansion.
				delete temp;
				continue;
			}
			else {
				// Obtained a token, return.
				token = *temp;
				return true;
			}
		}
		else return false;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::NextToken(Token& token) {
	bool done = false;

	while(done == false) {
		bool tokenObtained = false;

        // Get the next token from a character/token source. 
        // If it's EOF we see if there are other source objects
        // on the stack and use one of these if true.
        if(GetFromTokenSource(token) == false) {
			// Try first to use the token that was put back into the source.
            if(hasPeekedToken_) {
                token = peekedToken_;
                hasPeekedToken_ = false;
            }
			else NextTokenImpl(token);
		}

		if(token.IsEOF()) {
			if(tokSource_) {
				// This is the end marker for a token source.
				done = true;
			}
			else if(PopSource()) {
				// A source was on the stack, continue with it.
				continue;
			}
			else {
				// No more data. Check the file for unmatched directives.
				pp_->CheckFileEnd();
				done = true;
			}
		}
		else if(token.IsIdentifier() || token.IsKeyword()) {
			// Check if it's a keyword or a macro definition.
			// We check first for a macro definition because keywords can be
			// names for macros too!
			if(token.CanExpand() && pp_->IsDefinition(token)) {
				// The preprocessor sometimes wants unexpanded tokens.
				if(noExpand_ == false) {
					// This is a definition (possible function-like macro).
					pp_->HandleDefinition(token);
					continue; // See if the Preprocessor pushed another source.
				}
			}
			
			// Mark the token as a 'keyword' if it's the case.
			if(inPP_ == false) {
				pp_->TryMarkAsKeyword(token);
			}

			done = true;
		}
		else if(token.IsMacro()) { // #
			if(usePP_) {
				// We are using the preprocessor.
				// A valid macro definition should be the first on the line.
				if(token.HasFlag(TokenFlags::AtLineStart) && (inPP_ == false)) {
					// Let the preprocessor handle the directive.
					inPP_ = true;
					isLineBegin_ = false;
					pp_->HandleDirective();
					inPP_ = false;
					continue; // See if the Preprocessor pushed another source.
				}
				else done = true; // Should be handled by the parser.
			}
			else if(hasCustomChar_ && (customChar_ == '#')) {
				// This is the custom token.
				token.SetKind(TokenKind::Custom);
				done = true;
			}
			else done = true; // Should be handled by the parser.
		}
		else done = true;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Lexer::ReadLine() {
	// Read until the end of line markers of EOF is found.
	T* start = current_;

	while((*current_ != '\r') && (*current_ != '\n') && (*current_ != 0)) {
		current_ = source_->NextChar();
	}

	return string(start, (int)(current_ - start));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::PushSource(shared<CharSource> newSource) {
	if(source_) {
		source_->GoBack(); // Put the character from 'current_' back.
		sourceStack_.Push(source_);

		// If the new source modifies the file name, line number, etc.
		// save the current values on a stack, so they can be restored later.
		if(newSource->NeedsStateRestore()) {
			SaveSourceState();
			isLineBegin_ = true;
		}
	}
	
	source_ = newSource;
	current_ = source_->NextChar(); // Read the first character.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Lexer::PopSource() {
	bool needStateRestore = false;

	if(source_) {
		needStateRestore = source_->NeedsStateRestore();

		// If the source is associated with a file, remove the file from the include stack.
		if(source_->ShouldPopInclude()) {
			pp_->HandleEOF();
		}
	}

	source_ = nullptr;

	// Make sure the starting source is not removed.
	if(sourceStack_.Count() > 0) {
		source_ = sourceStack_.Pop();

		if(needStateRestore) {
			// This source modified the state, restore it to the previous values.
			SourceInfo info = sourceState_.Pop();
			file_ = info.File;
			line_ = info.Line;
			name_ = info.Name;
			lineStart_ = info.LineStart;
		}

		isLineBegin_ = true;
		current_ = source_->NextChar(); // Read the first character.
		return true;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::EnableTokenSourceMacro(TokenSource* source) {
	if(source->Macro() && source->Macro()->CanBeEnabled()) {
		// If the macro was not disabled in the meantime, enable it now.
		source->Macro()->SetEnabled(true);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Lexer::PopTokenSource() {
	if(tokSource_) {
		// There is an active token source.
		EnableTokenSourceMacro(tokSource_);
		tokSource_ = nullptr;
	}

	if(tokSourceStack_.Count() > 0) {
		tokSource_ = tokSourceStack_.Pop();
		return true;
	}
		
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::PushTokenSource(shared<TokenSource> source) {
	// If there is currently an active token source, push it on the stack.
	if(tokSource_) {
		tokSourceStack_.Push(tokSource_);
	}

	// Make it the current source.
	tokSource_ = source;

	if(source->Macro()) {
		// Disable expansion for this definition.
		source->Macro()->SetEnabled(false);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::PutBack(const Token& token) {
	if(tokSource_) {
		// The token originated from here, it can be put back into the list.
		tokSource_->GoBack();
	}
	else {
        // This will be the first token used on the next request.
        peekedToken_ = token;
        hasPeekedToken_ = true;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Lexer::SaveSourceState() {
	SourceInfo info;

	info.File = file_;
	info.Line = line_;
	info.Name = name_;
	info.LineStart = lineStart_;
	sourceState_.Push(info);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Lexer::LoadStart(const string& path) {
	if(pp_->LoadStart(path)) {
		lineStart_ = current_;
		isLineBegin_ = true;
		tokSourceEnabled_ = true;
		returnEOF_ = false;
		noExpand_ = false;
		inInclude_ = false;
		returnNewLine_ = false;
		inPP_ = false;

		return true;
	}
	else return false;
}

} // namespace Lexing