// Preprocessor.hpp
// Copyright (c) Lup Gratian
//
// Implementation of the C preprocessor.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Preprocessor.hpp"
#include "Lexer.hpp"

namespace Lexing {

// Static data.
Dictionary<string, Preprocessor::DirectiveKind> Preprocessor::directives_ =
	Dictionary<string, Preprocessor::DirectiveKind>();

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::Scan(Token& token) {
	lexer_->NextToken(lastToken_);
	token = lastToken_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanUnexpanded(Token& token) {
	lexer_->SetNotExpand(true);
	lexer_->NextToken(lastToken_);
	lexer_->SetNotExpand(false);
	token = lastToken_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::SameTokenContent(Token& a, Token& b) const {
	if(a.IsKeyword()) {
		if(b.IsKeyword()) return a.AsKeyword<int>() == b.AsKeyword<int>();
		else return false;
	}

	switch(a.Kind()) {
		case TokenKind::Identifier: {
			return a.NameValue()->Name ==
				   b.NameValue()->Name;
		}
		case TokenKind::Number: {
			return a.NumberValue()->Number ==
				   b.NumberValue()->Number;
		}
		case TokenKind::Char: // Fall through.
		case TokenKind::String: {
			auto sdA = a.StringValue();
			auto sdB = a.StringValue();

			return (sdA->IsWide == sdB->IsWide) &&
				   (sdA->HasEscaped == sdB->HasEscaped) &&
				   (sdA->Value == sdB->Value);
		}
		default: {
			// Other tokens don't have content.
			return true;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::AreMacrosIdentical(DefinitionInfo& a, DefinitionInfo& b) const {
	// First check if the type is the same.
	if(a.Type() != b.Type()) {
		return false;
	}

	if(a.Type() == IdentifierType::Macro) {
		// Both are function macros, check the arguments.
		auto& argsA = static_cast<MacroInfo*>(&a)->Arguments();
		auto& argsB = static_cast<MacroInfo*>(&b)->Arguments();

		if(argsA.Count() != argsB.Count()) {
			return false;
		}
		else {
			for(int i = 0; i < argsA.Count(); i++) {
				if(argsA[i] != argsB[i]) {
					return false;
				}
			}
		}
	}
	
	// Check the body.
	auto& bodyA = a.Body();
	auto& bodyB = b.Body();

	if(bodyA.Count() != bodyB.Count()) {
		return false;
	}
	
	for(int i = 0; i < bodyA.Count(); i++) {
		if((bodyA[i].Kind() != bodyB[i].Kind()) &&
			(bodyA[i].HasFlag(TokenFlags::AfterWhitespace) !=
			bodyB[i].HasFlag(TokenFlags::AfterWhitespace)) &&
			(bodyA[i].HasFlag(TokenFlags::AtLineStart) !=
			bodyB[i].HasFlag(TokenFlags::AtLineStart)) &&
			(SameTokenContent(bodyA[i], bodyB[i]) == false)) {
			return false;	
		}
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unsigned Preprocessor::ConvertToInt(Token& token) {
	DebugValidator::IsTrue(token.IsNumber());
	unsigned value = 0;
	string& data = token.NumberValue()->Number;

	for(int i = 0; i < data.Length(); i++) {
		unsigned oldValue = value;
		string::TChar ch = data[i];

		if((ch >= '0') && (ch <= '9')) {
			value = value * 10 + (ch - '0');

			// If the value overflows, 'value' < 'oldVal'.
			if(value < oldValue) return -1;
		}
		else return -1; // Invalid value.
	}

	return value;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanToEnd() {
	Token token = lastToken_;

	while((token.IsLineEnd() == false) && (token.IsEOF() == false)) {
		ScanUnexpanded(token);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::CheckDirectiveEnd() {
	Token token;
	ScanUnexpanded(token);

	if((token.IsLineEnd() == false) && (token.IsEOF() == false)) {
		diag_->Report(Error::PP_TOKENS_AFTER_DIRECTIVE)<<token.Location();
	}

	ScanToEnd();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::SkipToBlockEnd(bool popInfo) {
	// Skip until #endif is found, or the end of file is reached
	// (in this case an error is emitted).
	while(true) {
		Token token;
		ScanUnexpanded(token);

		if(token.IsEOF()) {
			// The end of file has been reached, something is wrong.
			diag_->Report(Error::PP_SKIP_TO_ENDIF_EOF_REACHED)<<token.Location();
			return;
		}
		else if(token.IsMacro() && token.HasFlag(TokenFlags::AtLineStart)) {
			// Found #.
			ScanUnexpanded(token);

			if(token.IsIdentifier() == false) {
				// Can't be what we're looking for.
				continue;
			}

			string& name = token.NameValue()->Name;
			if(name == "endif") {
				// The end of the inclusion block has been found!
				CheckDirectiveEnd(); // Nothing should be after.

				// The context info is no longer needed.
				if(popInfo) {
					DebugValidator::IsLargerOrEqual(ifStack_.Count(), 0);
					ifStack_.Pop();
				}

				return;
			}
			else if((name == "if") || (name == "ifdef") || (name == "ifndef")) {
				// This is a nested conditional directive that must be skipped.
				// We skip it here so that the #endif directive of the nested
				// conditional is not mistaken with the one of the current conditional.
				SkipToBlockEnd(false /* don't pop */);
			}
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::SkipToElse() {
	// Skip until we find an #. Then check the next token to see if it's
	// an #else, #elif or #endif. If it's #endif we are done.
	while(true) {
		Token token;
		ScanUnexpanded(token);

		if(token.IsEOF()) {
			// The end of file has been reached, something is wrong.
			diag_->Report(Error::PP_SKIP_TO_ENDIF_EOF_REACHED)<<token.Location();
			return;
		}
		else if(token.IsMacro() && token.HasFlag(TokenFlags::AtLineStart)) {
			// Found #.
			ScanUnexpanded(token);

			if(token.IsIdentifier() == false) {
				// Non-identifiers are skipped.
				continue;
			}

			string& name = token.NameValue()->Name;
			if(name == "endif") {
				// The end of the inclusion block has been found!
				CheckDirectiveEnd(); // Nothing should be after.

				// The context info is no longer needed.
				DebugValidator::IsLargerOrEqual(ifStack_.Count(), 0);
				ifStack_.Pop();
				return;
			}
			else if((name == "if") || (name == "ifdef") || (name == "ifndef")) {
				// This is a nested conditional directive that must be skipped.
				// We skip it here so that the #endif directive of the nested
				// conditional is not mistaken with the one of the current conditional.
				SkipToBlockEnd(false /* don't pop */);
			}
			else if((name == "else") || (name == "elif")) {
				// Put the token back and handle the directive the usual way.
				lexer_->PutBack(token);
				HandleDirective();
				return;
			}
				
			// Skip over all other directives.
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::ScanMacroName(Token& token, bool undef) {
	// Macro names are not expanded even if they are names for already defined macros.
	ScanUnexpanded(token);

	if(token.IsLineEnd() || token.IsEOF()) {
		// The end of the line (or document) was reached before finding the name.
		diag_->Report(Error::PP_EXPECTED_MACRO_NAME)<<token.Location();
		return false;
	}
	else if(token.IsIdentifier() == false) {
		// Must be a valid identifier.
		diag_->Report(Error::PP_MACRO_NAME_INVALID)<<token.Location();
		return false;
	}

	// Check that the name doesn't overwrite one of the built-ins.
	string& name = token.NameValue()->Name;

	if(identTable_->IsBuiltinDefinition(name)) {
		if(undef) {
			diag_->Report(Error::PP_MACRO_UNDEF_BUILTIN)<<token.Location();
			return false;
		}
		else {
			diag_->Report(Error::PP_MACRO_DEFINE_BUILTIN)<<token.Location();
			return false;
		}
	}
	else if(name == "defined") {
		diag_->Report(Error::PP_MACRO_NAME_IS_DEFINED)<<token.Location();
		return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MacroInfo* Preprocessor::ScanMacroFunctionArguments() {
	// #define identifier lparen identifier-listopt ) replacement-list new-line
	// #define identifier lparen ... ) replacement-list new-line
	// #define identifier lparen identifier-list , ... ) replacement-list new-line
	local<MacroInfo> info = new MacroInfo();
	List<string>& arguments = info->Arguments();

	while(true) { // Until one of the conditions is met.
		Token token;
		ScanUnexpanded(token);

		// An argument can appear only once in the list.
		// Ex. #define ABC(x, y, x) is not valid.
		if(token.IsIdentifier()) {
			// Add the argument to the list (it it's not already added).
			string& name = token.NameValue()->Name;

			if(arguments.Contains(name)) {
				diag_->Report(Error::PP_DEFINE_DUPLICATE_ARGUMENT)<<token.Location()<<name;
				return nullptr;
			}

			arguments.Add(name);

			// Check the next token. It should be either , or ).
			ScanUnexpanded(token);

			if(token.Kind() == TokenKind::CloseParen) {
				// End of definition.
				return info.Get();
			}
			else if(token.Kind() == TokenKind::Comma) {
				continue; // More arguments.
			}
			else {
				// Error, something like FUNCT(A+B) (+ is any invalid token).
				diag_->Report(Error::PP_DEFINE_INVALID_TOKEN_IN_ARGUMENT_LIST)<<
							  token.Location()<<name;
				return nullptr;
			}
		}
		else if(token.Kind() == TokenKind::CloseParen) {
			// The end of the token has been found.
			// If there are arguments this is an error: FUNCT(A,)
			if(arguments.Count() == 0) {
				return info.Get(); // Empty macro function FUNCT().
			}
			else {
				// Error, something like FUN(A,)
				diag_->Report(Error::PP_DEFINE_EXPECTED_ARGUMENT)<<token.Location();
				return nullptr;
			}
		}
		else if(token.IsLineEnd() || token.IsEOF()) {
			// Error, argument list not closed FUNCT(A
			diag_->Report(Error::PP_DEFINE_LIST_NOT_CLOSED)<<token.Location();
			return nullptr;
		}
		else if(token.Kind() == TokenKind::Ellipsis) {
			// Variable arguments. Make sure the next token is ).
			ScanUnexpanded(token);

			if(token.Kind() != TokenKind::CloseParen) {
				diag_->Report(Error::PP_DEFINE_ARGUMENT_AFTER_VARARGS)<<token.Location();
				return nullptr;
			}

			// __VA_ARGS__ is explicitly added to the argument list
			// so that 'MacroExpander'doesn't need to care about it.
			arguments.Add("__VA_ARGS__");
			info->SetIsVarargs(true);
			return info.Get();
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::CheckPasteOperator(List<Token>& list) {
	// The list should not begin and end with ##.
	if(list.Count() > 0) {
		if(list[0].Kind() == TokenKind::HashHash) {
			diag_->Report(Error::PP_PASTE_BODY_BEGIN)<<list[0].Location();
			return false;
		}
		else if(list[list.Count() - 1].Kind() == TokenKind::HashHash) {
			diag_->Report(Error::PP_PASTE_BODY_END)<<list[list.Count() - 1].Location();
			return false;
		}
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanDefine() {
	// #define identifier replacement-list new-line
	// Scan the name of the definition. Then see if it's a function-like macro,
	// and if it is scan the argument list. When scanning the body for
	// function macros the conditions from C99:10.6.3.1 are checked.
	Token id;
	local<DefinitionInfo> definitionInfo;

	if(ScanMacroName(id, false) == false) {
		// The identifier is not valid.
		ScanToEnd();
		return;
	}

	// See if this is a function-like definition.
	// Note that there should be no space between the name and '('.
	Token paren;
	ScanUnexpanded(paren);

	if((paren.Kind() == TokenKind::OpenParen) &&
	   (paren.HasFlag(TokenFlags::AfterWhitespace) == false)) {
		// A macro function should begin now.
		definitionInfo = ScanMacroFunctionArguments();

		if(definitionInfo == nullptr) {
			// Error in argument list. Errors already emitted.
			ScanToEnd();
			return;
		}
		else definitionInfo->SetName(id.NameValue()->Name);
	}
	else if(paren.IsLineEnd() || paren.IsEOF()) {
		// The definition is empty (this is a valid situation).
        // '#define DEBUG' for example
		identTable_->Add(new DefinitionInfo(id.NameValue()->Name));
		return;
	}
	else {
		// Put the token back so it's considered a part of the body.
		lexer_->PutBack(paren);
	}

	// Scan the body of the definition.
	if(definitionInfo == nullptr) {
        // This is a define without parameters.
		// Create the definition storage now if not a function.
		Token data;
		definitionInfo = new DefinitionInfo(id.NameValue()->Name);
		auto& body = definitionInfo->Body();

		do {
			ScanUnexpanded(data);

			if(data.IsLineEnd() == false) {
				body.Add(data);
			}
		} while((data.IsLineEnd() || data.IsEOF()) == false);

		// Paste operators apply for object-like macros too.
		if(CheckPasteOperator(definitionInfo->Body()) == false) {
			return;
		}
	}
	else {
		// While scanning the body check for the conditions from C99:10.6.3.1.
		auto& arguments = static_cast<MacroInfo*>(definitionInfo.Raw())->Arguments();
		auto& body = definitionInfo->Body();
		Token data;
		Token lastData;

		while(true) {
			ScanUnexpanded(data);

			if(data.IsLineEnd() || data.IsEOF()) {
				// We're done if we reached the end of the line.
				break;
			}

			// If this is # check that the next token represents a valid argument.
			if(data.Kind() == TokenKind::Hash) {
				lastData = data;
				ScanUnexpanded(data);

				if(!(data.IsIdentifier() && arguments.Contains(data.NameValue()->Name))) {
					// Emit an error if the associated token is not valid.
					diag_->Report(Error::PP_STRINGIFY_EXPECTED_ARGUMENT)<<data.Location();
					return;
				}

				body.Add(lastData); // Add the #
			}

			body.Add(data);
		};

		// Make sure no paste operators are found at the beginning and at the end.
        // This reports the error if it's the case.
		if(CheckPasteOperator(definitionInfo->Body()) == false) {
			return;
		}
	}

	// All seems well, add the definition to the identifier table.
	// Check first that there isn't already a macro with the same name.
	// If there is we replace the previous body. If the bodies aren't the same
	// we warn, but only if the previous definition is not disabled.
	//
	// #define ABC(a) a + a
	// #undef ABC           
	// #define ABC(a) a - a      -> the warning is not emitted in this case
	if(identTable_->IsDefinition(definitionInfo->Name())) {
		DefinitionInfo* other = 
            static_cast<DefinitionInfo*>(identTable_->Get(definitionInfo->Name()));

		if((AreMacrosIdentical(*definitionInfo, *other) == false) && 
            other->Enabled()) {
			diag_->Report(Warning::PP_REDEFINITION_NOT_IDENTICAL)<<id.Location();
		}

		identTable_->Remove(definitionInfo->Name());
	}

	identTable_->Add(definitionInfo);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanLine() {
	// #line digit-sequence new-line
	// #line digit-sequence "s-char-sequence-opt" new-line
	// #line pp-tokens new-line
	// Scan the next token. The next token should be a number (possibly originated in
	// a macro expansion, but it's valid). Try to convert it to a number and then assign it.
	// It can be followed by a string that specifies the new file name.
	Token token;
	Scan(token);

	if(token.IsNumber() == false) {
		diag_->Report(Error::PP_LINE_EXPECTED_NUMBER)<<token.Location();
		ScanToEnd();
		return;
	}

	unsigned value = ConvertToInt(token);
	if((value>= 0) && (value < 2147483647)) { // C99:6.10.4.3
		lexer_->SetLine(value);
	}

	// Check the next token. If it's 'TokenKind::LineEnd' no file name is specified.
	// Else, the token must be a 'TokenKind::String', else an error is emitted.
	Scan(token);
	if(token.IsLineEnd()) {
		return; // Done.
	}
	else if(token.IsString()) {
		// Convert string to internal format and set it.
		if(token.StringValue()->IsWide) {
			// Wide character strings not allowed (C99:6.10.4.4).
			diag_->Report(Error::PP_LINE_WIDE_STRING)<<token.Location();
			ScanToEnd();
			return;
		}

		// The string could contain escaped characters, so pass it to 'StringParser'.
		StringParser::TStringList text(&token, 1);
		StringParser parser(diag_);
		StringInfo info = parser.Parse(text);

		if(info.IsValid == false) {
			diag_->Report(Error::PP_LINE_INVALID_STRING)<<token.Location();
			ScanToEnd();
		}

		lexer_->SetName(info.Value.ToString());
	}
	else {
		diag_->Report(Error::PP_LINE_INVALID_TOKEN)<<token.Location();
		ScanToEnd();
		return;
	}
	
	CheckDirectiveEnd();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanError() {
	// All content of the line (after #error) is considered to be part
	// of the error message.
	LocationInfo location = lexer_->Location();
	string message = lexer_->ReadLine();

	diag_->Report(Error::PP_ERROR_MESSAGE)<<message<<location;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanWarning() {
	// All content of the line (after #warning) is considered to be part
	// of the warning message.
	LocationInfo location = lexer_->Location();
	string message = lexer_->ReadLine();

	diag_->Report(Warning::PP_WARNING_MESSAGE)<<message<<location;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanPragma() {
	// We emit a keyword with the name '_Pragma' and let the lexer parse the rest 
	// of the line the usual way. The 'PragmaHandler' is responsible for handling all cases.
	local<List<Token>> list = new List<Token>(1);
	Token pragma(TokenKind::Pragma, lastToken_.Location());
	list->Add(pragma);
	lexer_->PushTokenSource(new TokenSource(list.Get(), true /* free */));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanUndef() {
	// #undef identifier new-line
	// The next token should be the name of the macro to disable.
	// Once a macro has been disabled it can never be enabled again, 
	// so we set a special flag for this.
	Token id;
	if(ScanMacroName(id, true) == false) {
		return; // Errors already emitted.
	}

	DefinitionInfo* macro = GetDefinition(id.NameValue()->Name);

	if(macro) {
		// Disable the macro.
		macro->SetEnabled(false);
		macro->SetCanBeEnabled(false); // C99:6.10.3.5
	}

	CheckDirectiveEnd(); // Nothing should be after.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanIfdef(bool isIfdef) {
	// #ifdef  identifier new-line group-opt
	// #ifndef identifier new-line group-opt
	Token id;

	// The next token should be the name of the macro definition.
	if(ScanMacroName(id, true) == false) {
		return; // Errors already emitted.
	}

	bool value = IsDefinition(id);

	// If this is #ifndef negate the result.
	if(isIfdef == false) {
		value = !value;
	}

	if(value) {
		// The content of the 'if' block is valid. Push information on the stack
		// so we can skip an #else or #elif block if it appears before #endif.
		ifStack_.Push(IfElseInfo(false, id.Location()));
	}
	else {
		// Skip until an #elif, #else or #endif directive is found.
		ifStack_.Push(IfElseInfo(true, id.Location())); // Context for a possible #else.
		SkipToElse();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::ScanIf(bool pushInfo) {
	lexer_->SetInPP(false);         // Macros should be expanded (C99:6.1.10.4).
	lexer_->SetReturnNewLine(true); // We want to be notified about this.

	bool value = ifEvaluator_.Evaluate();
	
	// Restore the state used by all other directives.
	lexer_->SetInPP(true);
	lexer_->SetReturnNewLine(false);

	// No other tokens should be after the end of the expression.
	Token token;
	ScanUnexpanded(token);

	if(token.IsLineEnd() == false) {
		diag_->Report(Error::PP_IF_TOKENS_AFTER)<<token.Location();
		ScanToEnd();
	}

	if(value) {
		// The content of the 'if' block is valid. Push information on the stack
		// so we can skip an #else or #elif block if it appears before #endif.
		if(pushInfo) {
			ifStack_.Push(IfElseInfo(false, token.Location()));
		}
	}
	else {
		// Skip until an #elif, #else or #endif directive is found.
		if(pushInfo) {
			ifStack_.Push(IfElseInfo(true, token.Location())); // Context for a possible #else.
		}

		SkipToElse();
	}

	return value;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanElif() {
	if(ifStack_.Count() == 0) {
		// An #elif directive without an #if before is not valid.
		diag_->Report(Error::PP_ELIF_NO_MATHING_IF)<<lexer_->Location();
		return;
	}

	IfElseInfo& info = ifStack_.Peek();

	if(info.ElseFound) {
		// An #else has been already found, so an #elif is now invalid.
		diag_->Report(Error::PP_ELIF_AFTER_ELSE)<<lexer_->Location();
		SkipToBlockEnd(true /* pop */);
		return;
	}

	// Let 'ScanIf' handle the expression.
	if(ScanIf(false /* don't push another IfElseInfo */)) {
		// The expression evaluated to 'true', so any #elif/#else
		// after this one must be skipped.
		info.SkipElse = true;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanElse() {
	if(ifStack_.Count() == 0) {
		diag_->Report(Error::PP_ELSE_NO_MATCHING_IF)<<lexer_->Location();
		return;
	}
	
	IfElseInfo& info = ifStack_.Peek();

	if(info.ElseFound) {
		// An #else has been already found, so another one is invalid.
		diag_->Report(Error::PP_ELSE_AFTER_ELSE)<<lexer_->Location();
		SkipToBlockEnd(true /* pop */);
		return;
	}

	// Mark that we found the #else block.
	info.ElseFound = true;

	// See if the content of the block should be skipped.
	if(info.SkipElse) {
		SkipToElse(); // Use this instead of 'SkipToBlockEnd' so that
		              // #else/#elif after #else situations can be detected.
	}
	else {
		// Consider this block.
		CheckDirectiveEnd(); // Nothing should be found after #else.
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanEndif() {
	// Make sure there is a matching #if-like directive before.
	if(ifStack_.Count() == 0) {
		diag_->Report(Error::PP_ENDIF_NO_MATCHING_IF)<<lexer_->Location();
		return;
	}

	ifStack_.Pop();		 // Not needed anymore.
	CheckDirectiveEnd(); // Nothing should be found after.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::ScanInclude() {
	// Scan the next token. If it's a string it represents an user header file.
	// If it's an < a string representing a system header should follow.
	// Any other token types are invalid.
	lexer_->SetInInclude(true); // The Lexer should consider '<' as string start.
	Token token;
	Scan(token); // Expand macros (C99:6.10.2.4).
	lexer_->SetInInclude(false);

	if(token.IsString()) {
		// Nothing should be after the include string.
		CheckDirectiveEnd();

		// This is a user header if the corresponding flag is set.
		// The string can have escape characters, so use StringParser
		// to get the actual value.
		bool isSystem = token.HasFlag(TokenFlags::SystemInclude);
		StringParser::TStringList text(&token, 1);
		StringParser parser(diag_);

		StringInfo info = parser.Parse(text);

		if(info.IsValid == false) {
			diag_->Report(Error::PP_INCLUDE_INVALID_STRING)<<token.Location();
			ScanToEnd();
			return;
		}

		// Try to include the file and make it the active one.
		if(lookup_->PushHeader(info.Value.ToString(), isSystem) == false) {
			diag_->Report(Error::PP_INCLUDE_FAILED)<<token.Location()<<info.Value.ToString();
			return;
		}
	}
	else if(token.IsLineEnd() == false) {
		// Any other token is not allowed. If the line end has been reached
		// an error has already been reported by the Lexer, so we don't
		// report it here again.
		diag_->Report(Error::PP_INCLUDE_INVALID_TOKEN)<<token.Location();
		ScanToEnd();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Preprocessor::Preprocessor(Lexer* lexer, IdentifierTable* table, 
						   HeaderLookup* lookup, Diagnostic* diagnostic) : 
		lexer_(lexer), identTable_(table), lookup_(lookup), diag_(diagnostic), 
		expander_(MacroExpander(this, lexer, diagnostic)),
		ifEvaluator_(IfEvaluator(this, lexer, diagnostic)) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::InitDirectives() {
	// Define the mappings between the directive name and kind.
	directives_.Add("include", PP_Include);
	directives_.Add("define",  PP_Define);
	directives_.Add("undef",   PP_Undef);
	directives_.Add("line",    PP_Line);
	directives_.Add("error",   PP_Error);
	directives_.Add("if",      PP_If);
	directives_.Add("elif",    PP_Elif);
	directives_.Add("else",    PP_Else);
	directives_.Add("endif",   PP_Endif);
	directives_.Add("ifdef",   PP_Ifdef);
	directives_.Add("ifndef",  PP_Ifndef);
	directives_.Add("pragma",  PP_Pragma);
	directives_.Add("warning", PP_Warning); // Extension.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::HandleDirective() {
	// Get the next token. It should be an identifier. If not emit an error.
	Token token;
	ScanUnexpanded(token);

	if(token.IsEOF() || token.IsLineEnd()) {
		// Null directive, valid (C99:6.10.7).
		return;
	}
	else if(token.IsIdentifier() == false) {
		// This is not an identifier.
		diag_->Report(Error::PP_DIRECTIVE_INVALID)<<token.Location();
	}

	string& name = token.NameValue()->Name;
	DirectiveKind kind;

	if(directives_.TryGetValue(name, &kind) == false) {
		// Invalid directive name.
		diag_->Report(Error::PP_DIRECTIVE_INVALID)<<token.Location();
		return;
	}

	// Call the appropriate handler.
	switch(kind) {
		case PP_Include: { ScanInclude();    break; }
		case PP_Define:  { ScanDefine();     break; }
		case PP_Undef:   { ScanUndef();      break; }
		case PP_Line:    { ScanLine();       break; }
		case PP_Error:   { ScanError();      break; }
		case PP_If:      { ScanIf(true);     break; }
		case PP_Elif:    { ScanElif();       break; }
		case PP_Else:    { ScanElse();       break; }
		case PP_Endif:   { ScanEndif();      break; }
		case PP_Ifdef:   { ScanIfdef(true);  break; }
		case PP_Ifndef:  { ScanIfdef(false); break; }
		case PP_Pragma:  { ScanPragma();     break; }
		case PP_Warning: { ScanWarning();    break; }
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::IsDefinition(Token& token, bool onlyEnabled) {
	// Only identifiers and keywords can name a macro definition.
	if(token.IsIdentifier() == false) {
		return false;
	}

	string& name = token.NameValue()->Name;
	return identTable_->IsDefinition(name, onlyEnabled) ||
		   identTable_->IsBuiltinDefinition(name);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::HandleDefinition(Token& token) {
	DebugValidator::IsTrue(token.IsIdentifier());
	
	// Check if this is one of the built-in definitions (__LINE__, etc.).
	string& name = token.NameValue()->Name;

	if(identTable_->IsBuiltinDefinition(name)) {
		BuiltinDefinition def = identTable_->GetBuiltinType(name);
		expander_.ExpandBuiltinDefinition(def);
	}
	else {
		// This could be either a simple definition or a function-like macro.
		IdentifierInfo* info = identTable_->Get(name);
		expander_.Expand(info, token);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::IsKeyword(Token& token) {
	DebugValidator::IsTrue(token.IsKeyword() || token.IsIdentifier());
	return identTable_->IsKeyword(token.NameValue()->Name);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::TryMarkAsKeyword(Token& token) {
	return identTable_->TryMarkAsKeyword(token);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DefinitionInfo* Preprocessor::GetDefinition(string& name) {
	if(identTable_->IsDefinition(name)) {
		return static_cast<DefinitionInfo*>(identTable_->Get(name));
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::CheckFileEnd() {
	while(ifStack_.Count()) {
		IfElseInfo info = ifStack_.Peek();

        // Report the error only if the 'if' descriptor was pushed
        // while parsing the current file (it's possible to have an '#ifdef' in a file,
        // then to include in another file, and to have the '#endif' only after
        // the '#include').
        if(info.Start.File() == lexer_->Location().File()) {
		    diag_->Report(Error::PP_UNMATCHED_IF)<<info.Start;
            ifStack_.Pop();
        }
        else break;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Preprocessor::HandleEOF() {
	// Pop the file from the include stack.
	lookup_->PopHeader();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Preprocessor::LoadStart(const string& path) {
	return lookup_->LoadStart(path);
}

} // namespace Lexing