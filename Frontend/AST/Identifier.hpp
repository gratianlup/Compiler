// Identifier.hpp
// Copyright (c) Lup Gratian
//
// Represents an identifier used by a declaration.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_IDENTIFIER_HPP
#define PC_AST_IDENTIFIER_HPP

#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Lexer/Token.hpp"
#include "../Common/LocationInfo.hpp"
using namespace Base;
using namespace Lexing;
using namespace Common;

namespace AST {

class Identifier {
private:
	string name_;
	LocationInfo location_;
	bool isBuiltin_;

public:
	Identifier() : isBuiltin_(false) {}

	Identifier(const string& name, LocationInfo location, bool builtin = false) :
			name_(name), isBuiltin_(builtin), location_(location) {}

	Identifier(const Token& token) :
			name_(token.NameValue()->Name), location_(token.Location()),
			isBuiltin_(false) {
		DebugValidator::IsTrue(token.IsIdentifier());
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Creates a new Identifier using the text of the specified token.
	static shared<Identifier> FromToken(Token& token) {
		return new Identifier(token);
	}

	// Creates a new Identifier using the text of the specified NameData.
	static shared<Identifier> FromData(const NameData& data, LocationInfo location) {
		return new Identifier(data.Name, location);
	}

	// Returns the name of the identifier.
	string& Name() {
		return name_;
	}

	const string& Name() const {
		return name_;
	}

	// Returns 'true' if the identifier could be a built-in one.
	bool IsBuiltin() const {
		return isBuiltin_;
	}

	// Returns the location where the identifier was found.
	LocationInfo Location() const {
		return location_;
	}

	unsigned GetHashCode() const {
		return name_.GetHashCode();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const Identifier& other) const {
		return name_ == other.name_;
	}

	bool operator== (const string& other) const {
		return name_ == other;
	}

	bool operator!= (const Identifier& other) const {
		return name_ != other.name_;
	}

	bool operator!= (const string& other) const {
		return name_ != other;
	}

	bool operator< (const Identifier& other) const {
		return name_ < other.name_;
	}
};

} // namespace AST
#endif