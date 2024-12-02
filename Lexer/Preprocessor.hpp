// Preprocessor.hpp
// Copyright (c) Lup Gratian
//
// Defines the Preprocessor, used to parse C preprocessing directives.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_PREPROCESSOR_HPP
#define PC_LEXING_PREPROCESSOR_HPP

#include "Token.hpp"
#include "Identifiers.hpp"
#include "IdentifierTable.hpp"
#include "TokenSource.hpp"
#include "MacroExpander.hpp"
#include "IfEvaluator.hpp"
#include "HeaderLookup.hpp"
#include "LexemeParsers.hpp"
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

// Forward declarations.
class Lexer;

class Preprocessor {
private:
	// The supported directives.
	enum DirectiveKind {
		PP_Include,
		PP_Define,
		PP_Undef,
		PP_Line,
		PP_Error,
		PP_If,
		PP_Elif,
		PP_Else,
		PP_Endif,
		PP_Ifdef,
		PP_Ifndef,
		PP_Pragma,
		PP_Warning // Extension.
	};

	// Stores flags about an #if-#else-#endif conditional block.
	// SkipIf    - true: the #if block should be skipped, until an #elif/#else block
	//				     is found, or until the matching #endif.
	// SkipElse  - true: any #elif/#else block that is found should be skipped.
	// ElseFound - true: the #else block has been found and another #else or #elif
	//					 after it should not be allowed.
	struct IfElseInfo {
		bool SkipIf;
		bool SkipElse;
		bool ElseFound;
		LocationInfo Start;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		IfElseInfo(bool skipIf, LocationInfo start) : 
				SkipIf(skipIf), SkipElse(!skipIf), ElseFound(false), Start(start) {}

		IfElseInfo(const IfElseInfo& other, LocationInfo start) :
				SkipIf(other.SkipIf), SkipElse(other.SkipElse), 
				ElseFound(other.ElseFound), Start(start) {}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static Dictionary<string, DirectiveKind> directives_;

	Lexer* lexer_;
	Diagnostic* diag_;
	IdentifierTable* identTable_;
	Token lastToken_;
	MacroExpander expander_;
	IfEvaluator ifEvaluator_;
	Stack<IfElseInfo> ifStack_;
	shared<HeaderLookup> lookup_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Checks if the content of the given tokens is identical.
	// Used to check for duplicate macro definitions.
	bool SameTokenContent(Token& a, Token& b) const;

	// Verifies if the given macro definitions are identical (C99:6.10.3).
	bool AreMacrosIdentical(DefinitionInfo& a, DefinitionInfo& b) const;

	// Skips all tokens until the end of the line is found.
	void ScanToEnd();

	// Skips until the #endif directive associated with the current block is found.
	// Is specified it pops the current IfElseInfo item from the stack.
	void SkipToBlockEnd(bool popInfo);

	// Skips tokens until an #else, #elif or #endif directive is found.
	void SkipToElse();

	// Checks that ## is not at the beginning or the end of the body (C99:10.6.3.3.1).
	bool CheckPasteOperator(List<Token>& list);

	// Verifies if the directive ends in newline. If not an error is emitted.
	void CheckDirectiveEnd();

	// Tries to convert the text of the specified token to a integer.
	// If it's not possible a number < 0 is returned.
	unsigned ConvertToInt(Token& token);

	// Scans and checks the name of a macro in 'define' and 'undef' directives.
	bool ScanMacroName(Token& token, bool undef);

	// Handles function-like macro definition arguments.
	MacroInfo* ScanMacroFunctionArguments();

	// Handles both object-like and function-like macro definitions.
	void ScanDefine();

	// Handles the #line directive (with or without a file name).
	void ScanLine();

	// Handles an #error. The whole line is considered to be part of the message.
	void ScanError();

	// Handles an #warning. The whole line is considered to be part of the message.
	// This is an extension of the standard.
	void ScanWarning();

	// Evaluates the expression of an #if directive.
	// Returns 'true' if the expression evaluated to 'true'.
	bool ScanIf(bool pushInfo);

	// Evaluates the expression of an #elif directive. 
	void ScanElif();

	// Handles an #else directive. If the top of the 'if' stack indicates that
	// the content of the #else block should be skipped it is skipped, else not.
	// Duplicate #else directives are detected and an error is emitted.
	// If the 'if' stack is empty there is no matching #if and an error is emitted.
	void ScanElse();

	// Handles the #endif directive. If it doesn't have a matching #if an error is emitted.
	void ScanEndif();

	// Disables the definition of a macro. 
	// If the macro doesn't exist the directive is ignored (C99:6.10.3.5.2).
	void ScanUndef();

	// Scans the #ifdef conditional include directive.
	// If 'isIfdef' is false it behaves like #ifndef.
	void ScanIfdef(bool isIfdef);

	// Scans an #include directive. Handles both user and system <> headers.
	void ScanInclude();

	// Handles #pragma directives. No pragma is handled as yet.
	void ScanPragma();

public:
	Preprocessor(Lexer* lexer, IdentifierTable* table, 
				 HeaderLookup* lookup, Diagnostic* diagnostic);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes the static dictionary of preprocessor directives.
	static void InitDirectives();

	// Scans the next token expanding identifiers.
	void Scan(Token& token);

	// Scans the next token without expanding identifiers.
	void ScanUnexpanded(Token& token);

	// Verifies if the specified identifier represents a macro definition.
	// Only enabled macros are considered if 'onlyEnabled' is set.
	bool IsDefinition(Token& token, bool onlyEnabled = true);

	// Verifies if the specified identifier is a keyword.
	bool IsKeyword(Token& token);

	// Verifies if the token is a keyword, and if true, sets it's type.
	bool TryMarkAsKeyword(Token& token);

	// Start place for all directive handling.
	void HandleDirective();

	// Expands the definition of the macro with the specified name.
	void HandleDefinition(Token& token);

	// Gets the definition that has the specified name.
	// If no definition is found nullptr is returned.
	DefinitionInfo* GetDefinition(string& name);

	// Verifies if there are unmatched #if-like directive.
	// An error is emitted for each unmatched #if.
	void CheckFileEnd();

	// Loads the first file to be used by the Lexer.
	bool LoadStart(const string& path);

	// Pops the file from the include stack when EOF is reached.
	void HandleEOF();
};

} // namespace Lexing
#endif