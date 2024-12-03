// TokenSource.hpp
// Copyright (c) Lup Gratian
//
// Defines the TokenSource class. Provides Tokens to the Lexer from a list.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_TOKEN_SOURCE_HPP
#define PC_LEXING_TOKEN_SOURCE_HPP

#include "Token.hpp"
#include "Identifiers.hpp"
#include "../Base/List.hpp"
#include "../Base/DebugValidator.hpp"

namespace Lexing {

class MacroExpander;

class TokenSource {
private:
	List<Token>* tokens_;
	int position_;
	DefinitionInfo* macro_; // The attached macro definition.
	MacroExpander* expander_;
	bool freeList_; // True if the list should be deallocated.
	bool valid_;

public:
	TokenSource(List<Token>* tokens, bool freeList = false, 
				DefinitionInfo* macro = nullptr, MacroExpander* expander = nullptr) :
			tokens_(tokens), position_(0), macro_(macro),
			freeList_(freeList), expander_(expander), valid_(true) {}

	~TokenSource() {
		if(freeList_) {
			delete tokens_;
			tokens_ = nullptr;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the next token without the current position.
	virtual Token* NextToken();

	// Moves the pointer back 'count' times.
	virtual void GoBack(int count = 1) {
		DebugValidator::IsLargerOrEqual(position_ - count, 0);
		position_ -= count;
		valid_ = true;
	}

	// Skip over 'count' tokens.
	virtual void Skip(int count = 1) {
		position_ += count;
	}

	// Returns the current position.
	virtual int Position() const {
		return position_;
	}

	// Sets the current position.
	virtual void SetPosition(int value) {
		position_ = value;
	}

	virtual DefinitionInfo* Macro() {
		return macro_;
	}

	bool IsValid() const {
		return valid_;
	}
};

} // namespace Lexing
#endif