// MessagePragma.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'MessagePragma' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "MessagePragma.hpp"
#include "../Lexer/LexemeParsers.hpp"
#include "Parser.hpp"
using namespace Lexing;

namespace Parsing {

MessagePragma::MessagePragma(Context* context, Parser* parser) :
	  PragmaExtension(context, parser) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool MessagePragma::Handle(ExtensionContext& context) {
	// #pragma message ( string-list )
	// All strings between the parens are concatenated and emitted as an
	// information message. If we find a token that is not a string we emit a warning.
	StringParser::TStringList list;
	Lexer* lexer = parser_->GetLexer();
	
	if(parser_->CurrentToken().Kind() != TokenKind::OpenParen) {
		diag_->Report(Warning::PRAGMA_MESSAGE_NO_OPEN_PAREN)<<
					  parser_->CurrentToken().Location();
		SkipToLineEnd();
		return false;
	}
	else parser_->EatToken(); // Skip over (

	// Read all strings until ) (or until something that is not a string is found).
	while(parser_->CurrentToken().IsString()) {
		list.Add(parser_->CurrentToken());
		parser_->EatToken();
	}

	// Combine the tokens and emit an information message.
	StringInfo info = StringParser(diag_).Parse(list);
	diag_->Report(Info::PRAGMA_MESSAGE)<<lexer->LineStart()<<info.Value.ToString();

	if(parser_->CurrentToken().Kind() != TokenKind::CloseParen) {
		diag_->Report(Warning::PRAGMA_MESSAGE_NO_CLOSE_PAREN)<<
					  parser_->CurrentToken().Location();
		SkipToLineEnd();
		return false;
	}
	else parser_->EatToken(); // Skip over )

	return true;
}

}