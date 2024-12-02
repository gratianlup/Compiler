// PragmaExtension.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'PragmaExtension' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "PragmaExtension.hpp"
#include "Parser.hpp"

namespace Parsing {

PragmaExtension::PragmaExtension(Context* context, Parser* parser) :
		Extension(context, parser) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PragmaExtension::SkipToLineEnd() {
	// Instruct the lexer to return the 'new-line' token so that we know when to stop.
	Lexer* lexer = parser_->GetLexer();
	bool oldState = lexer->ReturnNewLine();	
	lexer->SetReturnNewLine(true);
	
	while(parser_->CurrentToken().IsLineEnd() == false) {
		parser_->EatToken();
	}
		
	lexer->SetReturnNewLine(oldState);
	parser_->EatToken(); // Skip over the new-line token.
}

} // namespace Parsing
