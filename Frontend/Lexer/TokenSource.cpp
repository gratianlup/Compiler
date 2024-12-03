// TokenSource.cpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TokenSource.hpp"
#include "MacroExpander.hpp"
#include "Preprocessor.hpp"

namespace Lexing {

Token* TokenSource::NextToken() {
	// If the source is not valid we give up.
	if(valid_ == false) return nullptr;

	// This should catch an empty token list.
	if(position_ >= tokens_->Count()) {
		valid_ = false;
		return nullptr;
	}

	if((position_ < (tokens_->Count() - 1)) &&
		((*tokens_)[position_ + 1].Kind() == TokenKind::HashHash)) {
		// The next token is a ## (paste) operator. 
		// Concatenate the current token with the next one (or more 
		// if multiple ## are linked).
		expander_->PasteTokens(position_, tokens_);

		// The pasted token could form a macro definition (C99:6.10.3.3.3).
		// (but only if it's an identifier).
		Token& token = (*tokens_)[position_];

		if(token.IsIdentifier()) {
			if(expander_->PP()->IsDefinition(token)) {
				// Handle the definition and skip over the token.
				expander_->PP()->HandleDefinition(token);
				position_++;
				return new Token(TokenKind::Skip);
			}
		}
	}

	// Get the token at the current position. 
	Token* token = &(*tokens_)[position_];
	position_++;
	
	if(position_ == tokens_->Count()) {
		// If this is the last token mark the source as 'invalid'.
		valid_ = false;
	}

	return token;
}

}