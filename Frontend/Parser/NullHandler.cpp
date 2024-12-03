// NullHandler.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'NullHandler' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "NullHandler.hpp"
#include "Parser.hpp"

namespace Parsing {

NullHandler::NullHandler(Context* context, Parser* parser) :
	  Extension(context, parser) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool NullHandler::Handle(ExtensionContext& context) {
	// Skip all the tokens until the end of the line.
	// Instruct the lexer to return the 'new line' token so that we known
	// when we reached the end of the line.
	Lexer* lexer = parser_->GetLexer();
	bool oldState = lexer->ReturnNewLine();	
	lexer->SetReturnNewLine(true);
	
	while(parser_->CurrentToken().IsLineEnd() == false) {
		parser_->EatToken();
	}
		
	lexer->SetReturnNewLine(oldState);
	parser_->EatToken(); // Skip over the new-line token.
	return true;
}

}