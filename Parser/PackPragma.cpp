// PackPragma.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'PackPragma' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "PackPragma.hpp"
#include "Parser.hpp"

namespace Parsing {

PackPragma::PackPragma(Context* context, Parser* parser) :
	  PragmaExtension(context, parser) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PackPragma::Handle(ExtensionContext& context) {
	// #pragma pack( [ show ] | [ push | pop ] [, identifier ] , n  )
	Lexer* lexer = parser_->GetLexer();

	if(parser_->CurrentToken().Kind() != TokenKind::OpenParen) {
		diag_->Report(Warning::PRAGMA_MESSAGE_NO_OPEN_PAREN)<<
					  parser_->CurrentToken().Location();
		SkipToLineEnd();
		return false;
	}
	else parser_->EatToken(); // Skip over (

	// If the command list is empty the pack value is set to it's default.
	if(parser_->CurrentToken().Kind() == TokenKind::CloseParen) {
		context_->Options().RestorePackValue();
		parser_->EatToken(); // Skip over )
		return true;
	}

	// Scan until ) or an invalid token combination is found.
	bool isPush        = false;
	bool isPop         = false;
	bool hasShow       = false;
	bool hasValue      = false;
	bool hasIdentifier = false;
	string identifier;
	NumberInfo value;

	// See if a command is set (like 'pop', 'push' or 'show').
	while((parser_->CurrentToken().Kind() != TokenKind::CloseParen) ||
		   parser_->CurrentToken().IsEOF()) {
		Token token = parser_->CurrentToken();
		parser_->EatToken();

		if(IsPush(token)) {
			if(isPush || isPop) {
				diag_->Report(Warning::PRAGMA_PACK_DUPLICATE_COMMAND)<<
							  token.Location()<<string("push");
				SkipToLineEnd();
				return false;
			}
			else isPush = true;
		}
		else if(IsPop(token)) {
			if(isPop || isPush) {
				diag_->Report(Warning::PRAGMA_PACK_DUPLICATE_COMMAND)<<
							  token.Location()<<string("pop");
				SkipToLineEnd();
				return false;
			}
			else isPop = true;
		}
		else if(IsShow(token)) {
			if(hasShow) {
				diag_->Report(Warning::PRAGMA_PACK_DUPLICATE_COMMAND)<<
							  token.Location()<<string("show");
				SkipToLineEnd();
				return false;
			}
			else hasShow = true;
		}
		else if(token.IsNumber()) {
			// The number should be specified a single type and be an integer.
			if(hasValue) {
				diag_->Report(Warning::PRAGMA_PACK_NUMBER_INVALID)<<token.Location();
				SkipToLineEnd();
				return false;
			}

			value = NumberParser(diag_).Parse(token);

			if((value.IsValid == false) || value.IsFloating()) {
				diag_->Report(Warning::PRAGMA_PACK_NUMBER_INVALID)<<token.Location();
				SkipToLineEnd();
				return false;
			}
			else hasValue = true;
		}
		else if(token.IsIdentifier()) {
			if(hasIdentifier) {
				diag_->Report(Warning::PRAGMA_PACK_DUPLICATE_IDENTIFIER)<<token.Location();
				SkipToLineEnd();
				return false;
			}

			identifier = token.NameValue()->Name;
			hasIdentifier = true;
		}

		// If a comma is found more commands should follow.
		if(parser_->CurrentToken().Kind() == TokenKind::Comma) {
			parser_->EatToken();
		}
		else break;
	}

	// pack(pop, identifier, n) is not valid.
	if(isPop && hasIdentifier && hasValue) {
		diag_->Report(Warning::PRAGMA_PACK_INVALID)<<lexer->LineStart();
		SkipToLineEnd();
		return false;
	}

	// If 'show' is set we need to show the current pack value.
	if(hasShow) {
		diag_->Report(Warning::PRAGMA_PACK_INVALID)<<lexer->LineStart()<<
					  context_->Options().PackValue();
	}

	// Apply the command.
	if(isPush) {
		// Push the specified value on the stack. If the value is not specified, push
		// the current pack value. If an identifier was specified it is associated with the value. 
		PackInfo info;

		if(hasValue) {
			// The specified value is made the current pack value.
			info.Value = (int)value.IntValue;
			context_->Options().SetPackValue((int)value.IntValue);
		}
		else info.Value = context_->Options().PackValue();

		if(hasIdentifier) {
			info.Name = identifier;
		}

		packStack_.Add(info);
	}
	else if(isPop) {
		// If no value is specified, the top of the stack becomes the current pack value.
		// If the value is specified, the value becomes the current pack value.
		// If the identifier is specified, values are popped from the stack until
		// a value with the specified identifier is found; it's value becomes the current
		// pack value; if such a value is not found the pop is ignored.
		if(hasValue) {
			if(packStack_.Count() > 0) {
				packStack_.RemoveAt(packStack_.Count() - 1);
			}

			context_->Options().SetPackValue((int)value.IntValue);
		}
		else if(hasIdentifier) {
			int index = packStack_.FindLastIndex([&identifier](PackInfo& info) {
				return info.Name == identifier;
			});

			if(index != -1) {
				context_->Options().SetPackValue(packStack_[index].Value);
				packStack_.RemoveRange(index, packStack_.Count() - index);
			}
		}
		else {
			if(packStack_.Count() > 0) {
				context_->Options().SetPackValue(packStack_[packStack_.Count() - 1].Value);
				packStack_.RemoveAt(packStack_.Count() - 1);
			}
		}
	}
	else if(hasValue) {
		context_->Options().SetPackValue((int)value.IntValue);
	}

	// ) should be found now.
	if(parser_->CurrentToken().Kind() != TokenKind::CloseParen) {
		diag_->Report(Warning::PRAGMA_MESSAGE_NO_OPEN_PAREN)<<
					  parser_->CurrentToken().Location();
		SkipToLineEnd();
		return false;
	}
	else parser_->EatToken(); // Skip over )

	return true;
}

}