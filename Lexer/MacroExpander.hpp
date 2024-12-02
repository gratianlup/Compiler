// MacroExpander.hpp
// Copyright (c) Lup Gratian
//
// Defines the module used to expand macro definitions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_MACRO_EXPANDER_HPP
#define PC_MACRO_EXPANDER_HPP

#include "Token.hpp"
#include "Identifiers.hpp"
#include "IdentifierTable.hpp"
#include "TokenSource.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/Stack.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/Log.hpp"
#include "../Common/Diagnostic.hpp"
using namespace Base;
using namespace Common;

namespace Lexing {

class Preprocessor;
class Lexer;

class MacroExpander {
private:
	static Dictionary<TokenKind, string> tokenStrings_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Preprocessor* pp_;
	Lexer* lexer_;
	Diagnostic* diag_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Copies the flags from the source to the destination token.
	void CopyFlags(Token& source, Token& dest, TokenFlags optional = TokenFlags::None);

	// Converts the specified value to a string.
	string IntToString(int value);

	// Returns the string representing the token.
	string TokenString(Token& token);

	// Expands the specified definition.
	void ExpandDefinition(DefinitionInfo* def, Token& nameToken) ;

	// Expands the specified macro definition. Scans the argument list
	// and expands the arguments.
	void ExpandMacro(MacroInfo* macro, Token& nameToken);

	// Verifies if the macro invocation is function-like - is followed by an (.
	// If it's not the Lexer is restored to the previous state.
	bool IsFunctionLike();

	// Scans the list of macro arguments. If the list is not valid nullptr is returned.
	shared<List<List<Token>>> ScanMacroArguments(MacroInfo* macro);

	// Expands the specified argument, replacing all macro invocations
	// with tokens that don't represent other macros.
	shared<List<Token>> ExpandArgument(List<Token>& arguments, DefinitionInfo* macro);

	// Returns a list of tokens where the definition arguments have been
	// replaced by those provided in the invocation.
	shared<List<Token>> ExpandMacroArguments(shared<List<List<Token>>> arguments, 
                                             MacroInfo* macro);

	// Adds the identifier to the result list, or if it's the name of an enabled
	// macro definition adds the expanded macro.
	void ExpandMacroIdentifier(int index, Token& token, List<Token>& tokens, 
							   List<List<Token>>* arguments, MacroInfo* macro,
							   List<string>& macroArguments, List<Token>* resultList, 
							   TokenFlags& addSpace, 
                               Dictionary<int, shared<List<Token>>>& expanded);

	// Adds the concatenated (pasted) tokens of the longest paste sequence.
	// For 'a##b##c 12 d##e', the first sequence is considered.
	// Returns the new position in the list of body tokens.
	int ExpandMacroPaste(int index, Token& token, List<Token>& tokens, 
						 List<List<Token>>* arguments, MacroInfo* macro,
						 List<string>& macroArguments, List<Token>* resultList, 
						 TokenFlags& addSpace,
                         Dictionary<int, shared<List<Token>>>& expanded);

	// Returns a TokenKind::String token that contains the result of the concatenation
	// of all the tokens in the specified list.
	Token MakeString(List<Token>& list);

	// Verifies if the specified argument needs to be expanded before
	// it can be used in a macro expansion.
	bool ArgNeedsExpansion(int argument, List<List<Token>>* arguments);

public:
	MacroExpander(Preprocessor* preproc, Lexer* lexer, Diagnostic* diagnostic) :
			pp_(preproc), lexer_(lexer), diag_(diagnostic) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes the mappings between tokens and their string representations.
	// Used by MakeString.
	static void InitTokenStrings();

	// Returns the string that represents the specified operator.
	static string OperatorString(TokenKind kind);

	// Expands the specified built-in definition (__LINE__, etc.).
	void ExpandBuiltinDefinition(BuiltinDefinition def);

	// Expands the specified identifier. Can be either a macro definition
	// or a function-like macro.
	void Expand(IdentifierInfo* info, Token& nameToken);

	// Concatenates the specified tokens into a new token.
	// If the resulting token is invalid, a TokenKind::Invalid token is returned.
	void PasteTokens(int start, List<Token>* list);

	// Returns the associated Lexer.
	Lexer* Lexer();

	// Returns the associated Preprocessor.
	Preprocessor* PP();
};

} // namespace Lexing
#endif