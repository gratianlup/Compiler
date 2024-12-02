// MacroExpander.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "MacroExpander.hpp"
#include "Lexer.hpp"
#include "Preprocessor.hpp"
#include "CharSource.hpp"
#include "../Common/FileManager.hpp"
#include "../Base/DateTime.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/StaticList.hpp"

namespace Lexing {

// Static data.
Dictionary<TokenKind, string> MacroExpander::tokenStrings_ = Dictionary<TokenKind, string>();

void MacroExpander::InitTokenStrings() {
	tokenStrings_.Add(TokenKind::Add,         "+");
	tokenStrings_.Add(TokenKind::Inc,         "++");
	tokenStrings_.Add(TokenKind::AddEq,       "+=");
	tokenStrings_.Add(TokenKind::Sub,         "-");
	tokenStrings_.Add(TokenKind::Dec,         "--");
	tokenStrings_.Add(TokenKind::SubEq,       "-=");
	tokenStrings_.Add(TokenKind::Arrow,       "->");
	tokenStrings_.Add(TokenKind::Mul,         "*");
	tokenStrings_.Add(TokenKind::MulEq,       "*=");
	tokenStrings_.Add(TokenKind::DivEq,       "/=");
	tokenStrings_.Add(TokenKind::Tilde,       "~");
	tokenStrings_.Add(TokenKind::Eq,          "=");
	tokenStrings_.Add(TokenKind::EqEq,        "==");
	tokenStrings_.Add(TokenKind::Not,         "!");
	tokenStrings_.Add(TokenKind::NotEq,       "!=");
	tokenStrings_.Add(TokenKind::Mod,         "%");
	tokenStrings_.Add(TokenKind::ModEq,       "%=");
	tokenStrings_.Add(TokenKind::And,         "&");
	tokenStrings_.Add(TokenKind::AndEq,       "&=");
	tokenStrings_.Add(TokenKind::AndAnd,      "&&");
	tokenStrings_.Add(TokenKind::Or,          "|");
	tokenStrings_.Add(TokenKind::OrEq,        "|=");
	tokenStrings_.Add(TokenKind::OrOr,        "||");
	tokenStrings_.Add(TokenKind::Xor,         "^");
	tokenStrings_.Add(TokenKind::XorEq,       "^=");
	tokenStrings_.Add(TokenKind::Less,        "<");
	tokenStrings_.Add(TokenKind::LessEq,      "<=");
	tokenStrings_.Add(TokenKind::ShiftL,      "<<");
	tokenStrings_.Add(TokenKind::ShiftLEq,    "<<=");
	tokenStrings_.Add(TokenKind::Greater,     ">");
	tokenStrings_.Add(TokenKind::GreaterEq,   ">=");
	tokenStrings_.Add(TokenKind::ShiftR,      ">>");
	tokenStrings_.Add(TokenKind::ShiftREq,    ">>=");
	tokenStrings_.Add(TokenKind::Div,         "/");
	tokenStrings_.Add(TokenKind::Colon,       ":");
	tokenStrings_.Add(TokenKind::SemiColon,   ";");
	tokenStrings_.Add(TokenKind::Comma,       ",");
	tokenStrings_.Add(TokenKind::Dot,         ".");
	tokenStrings_.Add(TokenKind::Question,    "?");
	tokenStrings_.Add(TokenKind::OpenSquare,  "[");
	tokenStrings_.Add(TokenKind::CloseSquare, "]");
	tokenStrings_.Add(TokenKind::OpenParen,   "(");
	tokenStrings_.Add(TokenKind::CloseParen,  ")");
	tokenStrings_.Add(TokenKind::OpenCurly,   "{");
	tokenStrings_.Add(TokenKind::CloseCurly,  "}");
	tokenStrings_.Add(TokenKind::Ellipsis,    "...");
	tokenStrings_.Add(TokenKind::Hash,        "#");
	tokenStrings_.Add(TokenKind::HashHash,    "##");
	tokenStrings_.Add(TokenKind::HashHashDisabled, "##");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void MacroExpander::CopyFlags(Token& source, Token& dest, TokenFlags optional) {
	if(source.HasFlag(TokenFlags::AfterWhitespace)) {
		dest.SetFlag(TokenFlags::AfterWhitespace);
	}

	if(source.HasFlag(TokenFlags::AtLineStart)) {
		dest.SetFlag(TokenFlags::AtLineStart);
	}

	dest.SetFlag(optional);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string MacroExpander::IntToString(int value) {
	StringBuilder sb;
	StaticList<string::TChar, 32> list;

	do {
		list.Add((string::TChar)(value % 10 + '0'));
		value /= 10;
	} while(value > 0);

	list.Reverse();
	list.ForEach([&sb](string::TChar ch) -> bool {
		sb.Append(ch);
		return true;
	});

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string MacroExpander::TokenString(Token& token) {
	if(token.IsIdentifier() ||  token.IsKeyword()) {
		return token.NameValue()->Name;
	}
	else if(token.IsString() || token.IsChar()) {
		return token.StringValue()->Value;
	}
	else if(token.IsNumber()) {
		return token.NumberValue()->Number;
	}
	else {
		// It's an operator;
		return tokenStrings_[token.Kind()];
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void MacroExpander::ExpandBuiltinDefinition(BuiltinDefinition def) {
	switch(def) {
		case BuiltinDefinition_File: {
			// Replace with the current file name.
			local<List<Token>> list = new List<Token>(1);
			Token token(TokenKind::String, lexer_->Location());
			token.SetData(new StringData(lexer_->Name()));
			list->Add(token);
			lexer_->PushTokenSource(new TokenSource(list.Get(), true /* free */));
			return;
		}
		case BuiltinDefinition_Line: {
			// Replace with the index of the current line.
			local<List<Token>> list = new List<Token>(1);
			Token token(TokenKind::Number, lexer_->Location());
			token.SetData(new NumberData(IntToString(lexer_->Location().Line())));
			list->Add(token);
			lexer_->PushTokenSource(new TokenSource(list.Get(), true /* free */));
			return;
		}
		case BuiltinDefinition_Date: {
			// Replace with the current date.
			local<List<Token>> list = new List<Token>(1);
			Token token(TokenKind::String, lexer_->Location());
			token.SetData(new StringData(DateTime::Now().ToShortDateString()));
			list->Add(token);
			lexer_->PushTokenSource(new TokenSource(list.Get(), true /* free */));
			return;
		}
		case BuiltinDefinition_Time: {
			// Replace with the current time in 'hh:mm:ss' format.
			local<List<Token>> list = new List<Token>(1);
			Token token(TokenKind::String, lexer_->Location());
			token.SetData(new StringData(DateTime::Now().ToShortTimeString()));
			list->Add(token);
			lexer_->PushTokenSource(new TokenSource(list.Get(), true /* free */));
			return;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool MacroExpander::IsFunctionLike() {
	// A definition is handled like a function if it's immediately followed by a (.
	Token token;
	pp_->Scan(token);

	if(token.Kind() == TokenKind::OpenParen) {
		// Found (. The token remains consumed.
		return true;
	}
	
	// Restore the previous state.
	lexer_->PutBack(token);
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<List<List<Token>>> MacroExpander::ScanMacroArguments(MacroInfo* macro) {
	// Scan until ) is found (or the end of the file is reached).
	local<List<List<Token>>> arguments = new List<List<Token>>();
	int maxArguments = macro->Arguments().Count();
	int argIndex = 0;
	int parenCount = 1; // The start parenthesis is counted too.
	Token token;
	
	while(token.Kind() != TokenKind::CloseParen) {
		// Interior parenthesis are allowed, so we make a second loop
		// here so we don't stop immediately if we find an ).
		// In this case we stop only if the number of open and close parenthesis match.
		arguments->Add(List<Token>()); // Add a new argument.
		auto& currArguments = (*arguments)[argIndex];

		while(true) {
			pp_->ScanUnexpanded(token);

			if(token.IsEOF() || token.IsLineEnd()) {
				// The invocation has no end.
				diag_->Report(Error::PP_MACRO_CALL_EXPECTED_CLOSE_PAREN)<<token.Location();
				return nullptr;
			}
			else if(token.Kind() == TokenKind::OpenParen) {
				parenCount++; // Found (.
			}
			else if(token.Kind() == TokenKind::CloseParen) {
				parenCount--;

				if(parenCount == 0) {
					// The end of the invocation has been found.
					break;
				}
			}
			else if(token.Kind() == TokenKind::Comma) {
				// If the macro is Varargs the comma is part of the arguments.
				if((macro->IsVarargs() == false) && (parenCount == 1)) {
					break;
				}

				// If there are more arguments expected, comma ends the current argument.
                // An exception is when the number of open parens is larger than one
                // ('FOO(a, (b,c), d)' - 'b' and 'c' are in the same argument.
				if((argIndex < (macro->Arguments().Count() - 1)) && 
				   (parenCount == 1)) {
					break;
				}
			}

			// Add the argument to the current list.
			currArguments.Add(token);
		}

		// Check the number of arguments.
		if(currArguments.Count() == 0) {
			// Remove the empty argument.
			arguments->RemoveAt(argIndex);
		}
		else {
			argIndex++;

			if((argIndex > maxArguments) && (macro->IsVarargs() == false)) {
				// There are more arguments than defined; this is OK only if
				// the macro is defined as Varargs.
				diag_->Report(Error::PP_MACRO_CALL_TOO_MANY_ARGUMENTS)<<token.Location()<<
							  macro->Name();
				return nullptr;
			}
		}
	}

	// Make sure we there is the right number of arguments.
	if(argIndex < maxArguments) {
		// There are some cases when this is allowed.
		if((argIndex == 0) && (maxArguments == 1)) {
			// The argument is empty (#define X(a), X()).
			arguments->Add(List<Token>()); // Add an empty argument.
		}
		else if(macro->IsVarargs() == false) {
			diag_->Report(Error::PP_MACRO_CALL_TOO_FEW_ARGUMENTS)<<token.Location()<<
						  macro->Name();
			return nullptr;
		}
	}

	return arguments; // All seems OK.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool MacroExpander::ArgNeedsExpansion(int argument, List<List<Token>>* arguments) {
	DebugValidator::IsSmaller(argument, arguments->Count());
	
	// If one of the tokens is a macro name (and it's enabled) expansion is required.
	auto& tokens = (*arguments)[argument];

	for(int i = 0; i < tokens.Count(); i++) {
		Token& token = tokens[i];

		if(token.IsIdentifier()) {
			string& name = token.NameValue()->Name;
			auto info = pp_->GetDefinition(name);

			if(info && info->Enabled()) {
				return true; // Argument token is a macro and it's enabled.
			}
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Token MacroExpander::MakeString(List<Token>& list) {
	StringBuilder sb;
	Token result;

	// Make a string containing the appended tokens.
	for(int i = 0; i < list.Count(); i++) {
		Token& token = list[i];

		// Append a space if the original token had one.
		// It's not appended for the first token though.
		if((i > 0) && token.HasFlag(TokenFlags::AfterWhitespace)) {
			sb.Append(" ");
		}

		if(token.IsString() || token.IsChar()) {
			// "abc" is stored in in StringData as abc, so we need to append
			// \" or \' at the beginning and end, resulting \"abc\" in the example.
			if(token.IsString()) sb.Append("\\\"");
			else sb.Append("\\\'");

			// Insert \ for every occurrence of " or \ in the string (C99:6.10.3.2.2).
			string& value = token.StringValue()->Value;
												
			for(int i = 0; i < value.Length(); i++) {
				if((value[i] == '"') || (value[i] == '\\')) {
					sb.Append('\\');
				}

				sb.Append(value[i]);
			}

			if(token.IsString()) sb.Append("\\\"");
			else sb.Append("\\\'");
		}
		else if(token.IsNumber()) {
			sb.Append(token.NumberValue()->Number);
		}
		else if(token.IsIdentifier() || token.IsKeyword()) {
			sb.Append(token.NameValue()->Name);
		}
		else {
			// Insert the text found in the source file.
			sb.Append(tokenStrings_[token.Kind()]);
		}

	}

	// Check that the resulting string doesn't end in an \ that is not
	// part of an escaped sequence. If \ is found, count the \ characters and
	// if only backslashes are formed we don't emit the error (C99:6.10.3.2.2).
	if(sb.Chars()[sb.Length() - 1] == '\\') {
		int slashCount = 0;
		for(int i = sb.Length() - 1; i >= 0; i--) {
			if(sb.Chars()[i] == '\\') {
				slashCount++;
			}
			else break;
		}

		if((slashCount % 2) != 0) {
			diag_->Report(Error::PP_STRINGIED_ENDS_WITH_INCOMPLETE_ESCAPE)<<
						 list[0].Location();
			return Token(TokenKind::Invalid, LocationInfo());
		}
	}
	
	result = Token(TokenKind::String, list[0].Location());
	result.SetData(new StringData(sb.ToString()));
	result.SetLength(sb.Length());
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void MacroExpander::PasteTokens(int start, List<Token>* list) {
	Token result = (*list)[start];

	while((list->Count() - start) >= 3) {
		if((*list)[start + 1].Kind() != TokenKind::HashHash) {
			break; // The sequence is over.
		}

		Token a = result;
		Token b = (*list)[start + 2];
		list->RemoveRange(start, 3);

		// Handle common cases first, that can be evaluated without using the Lexer.
		if(a.IsIdentifier()) {
			if(b.IsIdentifier()) {
				// ident + ident = ident.
				result.SetKind(TokenKind::Identifier);
				result.SetData(new NameData(a.NameValue()->Name +
											b.NameValue()->Name));
			}
			else if(b.IsString() || b.IsChar()) {
				// ident + string/char = INVALID.
				break;
			}
			else if(b.IsNumber()) {
				// ident + number = ident.
				result.SetKind(TokenKind::Identifier);
				result.SetData(new NameData(a.NameValue()->Name +
											b.NumberValue()->Number));
			}
			else if(b.IsOperator()) {
				// ident + op = INVALID.
				break;
			}
		}
		else if(a.IsString()) {
			if(b.IsIdentifier() || b.IsNumber() || b.IsOperator() || b.IsChar()) {
				break; // INVALID
			}
			else if(b.IsString()) {
				// string + string = string.
				result.SetKind(TokenKind::String);
				result.SetData(new StringData(a.StringValue()->Value +
											  b.StringValue()->Value));
			}
		}
		else if(a.IsNumber() && b.IsNumber()) {
			// number + number = number.
			result.SetKind(TokenKind::Number);
			result.SetData(new NumberData(a.NumberValue()->Number +
										  b.NumberValue()->Number));
		}
		else {
			// For other possible combination use the Lexer to identify
			// the type of the resulting token. Append the string representation of
			// the two tokens and feed it to the Lexer.
			string temp = TokenString(a) + TokenString(b);
			lexer_->PushSource(new MacroCharSource(temp));
			lexer_->SetTokenSourceEnabled(false);
			lexer_->SetNotExpand(true);
			lexer_->SetReturnEOF(true);

			Token token;
			lexer_->NextToken(token);

			// If no token could be formed the concatenation is invalid.
			if(token.IsEOF()) {
				break;
			}

			// C99:6.10.3.3.4 - ## should not be turned in an operator.
			// Replace the token with another representation.
			if(token.Kind() == TokenKind::HashHash) {
				token.SetKind(TokenKind::HashHashDisabled);
			}

			// Assign the result.
			result = token;

			// The next token should be EOF, else more than one token
			// was formed and this is invalid.
			lexer_->NextToken(token);
			if(token.IsEOF() == false) {
				lexer_->SetNotExpand(false);
				lexer_->SetTokenSourceEnabled(true);
				lexer_->SetReturnEOF(false);
				break; // Invalid.
			}

			lexer_->SetNotExpand(false);
			lexer_->SetTokenSourceEnabled(true);
			lexer_->SetReturnEOF(false);
		}

		// Remove the left token and the ##, and insert the new result.
		list->Insert(start, result);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<List<Token>> MacroExpander::ExpandArgument(List<Token>& arguments,
                                                  DefinitionInfo* macro) {
	// This is called only when the argument needs to be expanded.
	// Make a fake TokenSource and lex from it the argument tokens, properly
	// expanded if needed. An EOF token is inserted at the end so we know when we
	// finished lexing the arguments.
	List<Token> fakeList(arguments);
	fakeList.Add(Token(TokenKind::FileEnd));
	lexer_->PushTokenSource(new TokenSource(&fakeList, false, nullptr, this));
	lexer_->SetReturnEOF(true);

	Token token;
	local<List<Token>> resultList = new List<Token>();
	lexer_->NextToken(token);

	while(token.IsEOF() == false) {
		// If there is an argument having the name of a macro definition we need
		// to disable it, so it doesn't get expanded, like in the following example.
		// #define x x[2]
		// #define y(argument) argument
		// y(x) -> should be transformed into 'x[0]', not 'x[0][0]'.
		if(pp_->IsDefinition(token, false /* onlyEnabled */)) {
			token.SetFlag(TokenFlags::DisableExpansion);
		}

		resultList->Add(token);
		lexer_->NextToken(token); // Advance to next token.
	} 

	lexer_->SetReturnEOF(false);
	return resultList;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void MacroExpander::ExpandMacroIdentifier(int index, Token& token, List<Token>& tokens, 
										  List<List<Token>>* arguments, MacroInfo* macro,
										  List<string>& macroArguments, 
                                          List<Token>* resultList, TokenFlags& addSpace, 
										  Dictionary<int, shared<List<Token>>>& expanded) {
	// If the next token is the ## operator we skip over this one
	// because it will be handled when handling ##.
	if((index < (tokens.Count() - 1)) && 
	   (tokens[index + 1].Kind() == TokenKind::HashHash)) {
		return;
	}

	// This could be a macro argument.
	string& name = token.NameValue()->Name;
	int argIndex = macroArguments.IndexOf(name);

	if(argIndex < 0) {
		// Add the token without expanding it.
		resultList->Add(token);
		CopyFlags(token, (*resultList)[resultList->Count() - 1], addSpace);
		addSpace = TokenFlags::None;
		return;
	}

	int prevCount = resultList->Count();

	// Arguments that are not macros don't need to be expanded (like in 'A(1,2)').
	if(ArgNeedsExpansion(argIndex, arguments)) {
		// Expand the argument list (if it's not already expanded).
		if(expanded.ContainsKey(argIndex)) {
			resultList->AddRange(*expanded[argIndex]);
		}
		else {
			shared<List<Token>> newArguments = ExpandArgument((*arguments)[argIndex], macro);
			expanded.Add(argIndex, newArguments);
			resultList->AddRange(*newArguments);
		}

		// If something was added, the first added token inherits
		// the flags of the current token.
		if(resultList->Count() != prevCount) {
			CopyFlags(token, (*resultList)[prevCount], addSpace);
		}
		else {
			// If the current token had whitespace before it make sure
			// that at least the next one gets it.
			if(token.HasFlag(TokenFlags::AfterWhitespace)) {
				addSpace = TokenFlags::AfterWhitespace;
			}
		}
	}
	else {
		// Just add the arguments, because no expansion is needed.
		// This also speeds up evaluation, because memory allocation is reduced.
		resultList->AddRange((*arguments)[argIndex]);

		if(resultList->Count() != prevCount) {
			CopyFlags(token, (*resultList)[prevCount], addSpace);
		}

		addSpace = TokenFlags::None;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int MacroExpander::ExpandMacroPaste(int index, Token& token, List<Token>& tokens, 
									List<List<Token>>* arguments, MacroInfo* macro,
									List<string>& macroArguments, List<Token>* resultList, 
									TokenFlags& addSpace, 
									Dictionary<int, shared<List<Token>>>& expanded) {
	// The ## (paste) operator has been found.
	// left1 ## left2 ## right
	//       ^ current position
	//
	// Scan until no other ## is found.
	// Replace 'left' with the unexpanded arguments. If it's empty
	// ## is not added. Else add ## and replace 'right'. If right is empty
	// set a flag so that a future ## operator knows this.
	bool wasEmpty = false;
	int limit = tokens.Count() - 1;

	while(index < limit) {
		Token& hashToken = tokens[index];
		if(hashToken.Kind() != TokenKind::HashHash) {
			break; // Reached the end of the sequence.
		}

		Token& left  = tokens[index - 1];

		if(left.IsIdentifier()) {
			string& leftName = left.NameValue()->Name;
			int argIndex = macroArguments.IndexOf(leftName);

			if(argIndex != -1) {
				// This is an argument.
				List<Token>& leftList = (*arguments)[argIndex];

				if(leftList.Count() != 0) {
					CopyFlags(hashToken, leftList[0], addSpace);
					resultList->AddRange(leftList);
					
					// If 'left' expanded to something and we don't have
					// an unused ## -> append ## (C99:6.10.3.3.3).
					resultList->Add(hashToken);
				}

				index += 2;
				continue;
			}
		}
				
		// The token is not an argument.
		resultList->Add(left);			
		resultList->Add(token);				
		index += 2;
	}

	// Append the 'right' part.
	Token& right = tokens[index - 1];

	if(right.IsIdentifier()) {
		string& rightName = right.NameValue()->Name;
		int argIndex = macroArguments.IndexOf(rightName);

		if(argIndex != -1) {
			// This is an argument.
			List<Token>& rightList = (*arguments)[argIndex];
			CopyFlags(token, rightList[0], addSpace);
			resultList->AddRange(rightList);

			if((wasEmpty == false) && (rightList.Count() == 0)) {
				// 'left' was not empty, but 'right' is; remove ##.
				resultList->RemoveAt(resultList->Count() - 1);
			}

            // tokens[index] is not part of the paste, consider it again.
			return index - 1;
		}
	}

	// The token is not an argument.
    // tokens[index] is not actually part of the paste, consider it again.
	resultList->Add(right);
	return index - 1;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<List<Token>> MacroExpander::ExpandMacroArguments(shared<List<List<Token>>> arguments, 
												        MacroInfo* macro) {
	Dictionary<int, shared<List<Token>>> expanded;
	local<List<Token>> resultList = new List<Token>(macro->Body().Count());
	auto& tokens = macro->Body();
	auto& macroArguments = macro->Arguments();
	TokenFlags addSpace = TokenFlags::None;

	for(int i = 0; i < tokens.Count(); i++) {
		Token& token = tokens[i];

		// See if there is a # (stringify) operator. If yes, the next
		// argument will be converted to a string.
		if(token.Kind() == TokenKind::Hash) {
			// The next token must be an identifier.
			DebugValidator::IsTrue(tokens[i + 1].IsIdentifier());

			// Get the argument list and convert it to a string token.
			Token& nameToken = tokens[i + 1];
			string& name = nameToken.NameValue()->Name;
			List<Token>& list = (*arguments)[macroArguments.IndexOf(name)];
			
			Token result = MakeString(list);
			CopyFlags(token, result, addSpace);
			resultList->Add(result);
			addSpace = TokenFlags::None;

			// We handled the identifier, don't consider it on the next step anymore.
			i++;
		}
		else if(token.IsIdentifier()) {
			ExpandMacroIdentifier(i, token, tokens, arguments, macro, 
                                  macroArguments, resultList, addSpace, expanded);
		}
		else if(token.Kind() == TokenKind::HashHash) {
			// Note that the position can be changed by this method.
			i = ExpandMacroPaste(i, token, tokens, arguments, macro, 
                                 macroArguments, resultList, addSpace, expanded);
		}
		else {
			// Other kind of tokens.
			// Add the token without expanding it.
			resultList->Add(token);
			CopyFlags(token, (*resultList)[resultList->Count() - 1], addSpace);
			addSpace = TokenFlags::None;
		}

		// Note that __VA__ARGS__ is treated above like it were an identifier.
	}

	return resultList;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void MacroExpander::ExpandDefinition(DefinitionInfo* def, Token& nameToken) {
	// Check if the definition is empty (like #define UNIX). If it is, do nothing.
	if(def->Body().Count() == 0) return;

	// Push a 'TokenSource' containing the body of the definition.
	// If the definition contains other macros these will be discovered and handled later.
	// Note that the definition will be disabled, so that any identifier with the name
	// of the definition is not expanded anymore, until the source is popped.
	lexer_->PushTokenSource(new TokenSource(&def->Body(), false, def, this));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void MacroExpander::ExpandMacro(MacroInfo* macro, Token& nameToken) {
	if(IsFunctionLike() == false) {
		// If this is not a function call we need to reconsider the name as a
		// simple identifier, like in '#define ABC(x) smth    int ABC = 5;' 
		// 'ABC' shall NOT be expanded in this case.
		// Push a 'TokenSource' containing the identifier marked with 
		// 'TokenFlags::DisableExpansion' so that it isn't considered
		// again as a macro definition.
		local<List<Token>> list = new List<Token>(1);
		nameToken.SetFlag(TokenFlags::DisableExpansion);
		list->Add(nameToken);
		lexer_->PushTokenSource(new TokenSource(list.Get(), true /* free */));
		return;
	}

	// Scan the macro arguments and give up if something went wrong.
	shared<List<List<Token>>> arguments = ScanMacroArguments(macro);

	if(arguments == nullptr) {
		// Something is invalid, errors already emitted.
		return;
	}

	// Now expand all the arguments, if needed. 
	shared<List<Token>> expanded = ExpandMacroArguments(arguments, macro);

	// A token source is created only if tokens resulted after the expansion.
	if(expanded->Count() > 0) {
		lexer_->PushTokenSource(new TokenSource(expanded.Get(), true, macro, this));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void MacroExpander::Expand(IdentifierInfo* info, Token& nameToken) {
	DebugValidator::IsNotNull(info);
	DebugValidator::IsTrue(info->Type() != IdentifierType::Keyword);
	
	if(info->Type() == IdentifierType::Definition) {
		ExpandDefinition(static_cast<DefinitionInfo*>(info), nameToken);
	}
	else {
		ExpandMacro(static_cast<MacroInfo*>(info), nameToken);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Lexer* MacroExpander::Lexer() {
	return lexer_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Preprocessor* MacroExpander::PP() {
	return pp_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string MacroExpander::OperatorString(TokenKind kind) {
	DebugValidator::IsTrue(tokenStrings_.ContainsKey(kind));
	return tokenStrings_[kind];
}

} // namespace Lexing