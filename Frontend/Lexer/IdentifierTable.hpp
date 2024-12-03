// IdentifierTable.hpp
// Copyright (c) Lup Gratian
//
// Implements the table of identifiers used by the Lexer and Preprocessor.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_IDENTIFIER_TABLE_HPP
#define PC_LEXING_IDENTIFIER_TABLE_HPP

#include "../Base/String.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/DebugValidator.hpp"
#include "Identifiers.hpp"
#include "Token.hpp"
using namespace Base;

namespace Lexing {

// The built-in definitions.
enum BuiltinDefinition {
	BuiltinDefinition_File,   // __FILE__
	BuiltinDefinition_Line,   // __LINE__
	BuiltinDefinition_Date,   // __DATE__
	BuiltinDefinition_Time,   // __TIME__
	BuiltinDefinition_Counter // __COUNTER__
};


class IdentifierTable {
private:
	Dictionary<const string*, shared<IdentifierInfo>, true> table_;
	Dictionary<const string, int> keywords_;
	Dictionary<const string*, BuiltinDefinition, true> builtins_;

public:
	// Adds the specified keyword to the table.
	void AddKeyword(const string& keyword, int value) {
		keywords_.Add(keyword, value);
	}

	// Adds the specified definition to the table.
	// Should be used for built-in definitions and definitions specified
	// on the driver command-line.
	void AddDefinition(DefinitionInfo* keyword, BuiltinDefinition defType) {
		table_.Add(&keyword->Name(), keyword);
		builtins_.Add(&keyword->Name(), defType);
	}

	// Adds the specified identifier to the table. 
	// Should be used for #definitions found in the source files. 
	void Add(shared<IdentifierInfo> info) {
        table_.Remove(&info->Name());
		table_.Add(&info->Name(), info);
	}

	// Verifies if an identifier with the specified name exists.
	bool Contains(const string& name) const {
		return table_.ContainsKey(&name);
	}

	// Returns the identifier with the specified name.
	IdentifierInfo* Get(const string& name) {
		DebugValidator::IsTrue(Contains(name));
		return table_[&name];
	}

	const IdentifierInfo* Get(const string& name) const {
		DebugValidator::IsTrue(Contains(name));
		return table_[&name];
	}

	// Removes the identifier with the specified name.
	void Remove(const string& name) {
		DebugValidator::IsTrue(Contains(name));
		table_.Remove(&name);
	}

	// Verifies if an identifier represents a keyword.
	bool IsKeyword(const string& name) const {
		return keywords_.ContainsKey(name);
	}

	// Verifies if the token is a keyword, and if true, sets it's type.
	bool TryMarkAsKeyword(Token& token) {
		DebugValidator::IsTrue(token.IsIdentifier() || token.IsKeyword());
		
		// We're done if the token is already marked as a keyword.
		if(token.IsKeyword()) return true;

		int value;
		int tokenLength = token.NameValue()->Name.Length();

		if(keywords_.TryGetValue(token.NameValue()->Name, &value)) {
			token.SetKeyword(value, tokenLength);
			return true;
		}
		
		return false;
	}

	// Verifies if an identifier represents a macro definition.
	// Note that it returns 'false' if the found definition is disabled.
	bool IsDefinition(const string& name, bool onlyEnabled = true) {
		if(Contains(name)) {
			// The identifier should be a macro definition 
			// and it shouldn't be disabled, if requested.
			auto ident = Get(name);
			return ((ident->Type() == IdentifierType::Definition) ||
					(ident->Type() == IdentifierType::Macro)) &&
					((onlyEnabled == false) || 
                    static_cast<DefinitionInfo*>(ident)->Enabled());
		}

		return false;
	}

	// Verifies if an identifier represents a built-in definition.
	bool IsBuiltinDefinition(const string& name) const {
		if(Contains(name)) {
			auto ident = Get(name);
			return (ident->Type() == IdentifierType::Definition) &&
					static_cast<const DefinitionInfo*>(ident)->IsBuiltin();
		}

		return false;
	}

	// Returns the type of the specified built-in definition.
	BuiltinDefinition GetBuiltinType(const string& name) const {
		DebugValidator::IsTrue(IsBuiltinDefinition(name));
		return builtins_[&name];
	}
};

} // namespace Lexing
#endif