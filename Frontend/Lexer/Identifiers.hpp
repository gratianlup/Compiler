// Identifiers.hpp
// Copyright (c) Lup Gratian
//
// Implements the type of identifiers supported by the language.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_IDENTIFIERS_HPP
#define PC_LEXING_IDENTIFIERS_HPP

#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "Token.hpp"
using namespace Base;

namespace Lexing {

// The type an identifier can be.
enum class IdentifierType {
	Keyword,    // if, else, while, etc.
	Definition, // #define PI 3.14
	Macro       // #define max(a,b) ...
};


// Base class for identifiers. Used by keyword identifiers.
class IdentifierInfo {
private:
	IdentifierType type_;
	string name_;
	bool isBuiltin_;

public:
	IdentifierInfo(IdentifierType type) : 
			type_(type), isBuiltin_(false) {}

	IdentifierInfo(IdentifierType type, const string& name, bool builtin = false) : 
			type_(type), name_(name), isBuiltin_(builtin) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	IdentifierType Type() const {
		return type_;
	}

	void SetType(IdentifierType value) {
		type_ = value;
	}

	string& Name() {
		return name_;
	}

	void SetName(const string& value) {
		name_ = value;
	}

	bool IsBuiltin() const {
		return isBuiltin_;
	}

	unsigned GetHashCode() const {
		return name_.GetHashCode();
	}
};


// Identifier class used by macro definitions.
class DefinitionInfo : public IdentifierInfo {
private:
	List<Token> body_;
	bool enabled_;
	bool canEnable_;

public:
	DefinitionInfo() : 
			IdentifierInfo(IdentifierType::Definition), enabled_(true), canEnable_(true) {
		SetType(IdentifierType::Definition);
	}

	DefinitionInfo(const string& name, bool builtin = false) : 
			IdentifierInfo(IdentifierType::Definition, name, builtin), 
			enabled_(true), canEnable_(true) {
		SetType(IdentifierType::Definition);
	}

	DefinitionInfo(const string& name, bool builtin, List<Token> body) : 
			IdentifierInfo(IdentifierType::Definition, name, builtin), 
			enabled_(true), canEnable_(true), body_(body) {
		SetType(IdentifierType::Definition);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	List<Token>& Body() {
		return body_;
	}

	bool Enabled()  const {
		return enabled_;
	}

	void SetEnabled(bool value) {
		enabled_ = value;
	}

	bool CanBeEnabled() const {
		return canEnable_;
	}

	void SetCanBeEnabled(bool value) {
		canEnable_ = value;
	}
};


// Identifier class used by function-like macro definitions.
class MacroInfo : public DefinitionInfo {
private:
	List<string> args_;
	bool isVarargs_;

public:
	MacroInfo() : 
			DefinitionInfo(), isVarargs_(false) {
		SetType(IdentifierType::Macro);
	}

	MacroInfo(const string& name, bool builtin = false) : 
			DefinitionInfo(name, builtin), isVarargs_(false) {
		SetType(IdentifierType::Macro);
	}

	MacroInfo(const string& name, bool builtin, List<Token> body, List<string> arguments) : 
			DefinitionInfo(name, builtin, body), isVarargs_(false), args_(arguments) {
		SetType(IdentifierType::Macro);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	List<string>& Arguments() {
		return args_;
	}

	bool IsVarargs() const {
		return isVarargs_;
	}

	void SetIsVarargs(bool value) {
		isVarargs_ = value;
	}
};

} // namespace Lexing
#endif