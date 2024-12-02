// ExpressionParser.cpp
// Copyright (c) Lup Gratian
//
// Implements the parser for all types of expression.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_EXPRESSION_PARSER_HPP
#define PC_PARSING_EXPRESSION_PARSER_HPP

#include "Parser.hpp"
#include "../Base/StaticList.hpp"
#include "../Lexer/LexemeParsers.hpp"
using namespace Base;
using namespace Lexing;

namespace Parsing {

int Parser::GetPrecedence(TokenKind kind) {
	switch(kind) {
		default:               return 0;
		case TokenKind::CloseParen: return 1;  // )
		case TokenKind::Comma:      return 2;  // ,
		case TokenKind::Eq:                    // =
		case TokenKind::AddEq:                 // +=
		case TokenKind::SubEq:                 // -=
		case TokenKind::MulEq:                 // *=
		case TokenKind::DivEq:                 // /=
		case TokenKind::ModEq:                 // %=
		case TokenKind::AndEq:                 // &=
		case TokenKind::OrEq:                  // |=
		case TokenKind::XorEq:                 // =
		case TokenKind::ShiftLEq:              // <<=
		case TokenKind::ShiftREq:   return 3;  // >>=
		case TokenKind::Question:   return 4;  // ?
		case TokenKind::OrOr:       return 5;  // ||
		case TokenKind::AndAnd:     return 6;  // &&
		case TokenKind::Or:         return 7;  // |
		case TokenKind::Xor:        return 8;  // 
		case TokenKind::And:        return 9;  // &
		case TokenKind::EqEq:                  // ==
		case TokenKind::NotEq:      return 10; // !=
		case TokenKind::Less:                  // <
		case TokenKind::LessEq:                // <=
		case TokenKind::Greater:               // >
		case TokenKind::GreaterEq:  return 11; // >=
		case TokenKind::ShiftR:                // >>
		case TokenKind::ShiftL:     return 12; // <<
		case TokenKind::Add:                   // +
		case TokenKind::Sub:        return 13; // -
		case TokenKind::Mul:                   // *
		case TokenKind::Div:                   // /
		case TokenKind::Mod:        return 14; // %
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseString() {
	// C99:6.4.5: Consecutive string are treated like a single one.
	// Ex: "abc" "def" L"123" -> L"abcdef123"
	// Parse until the found token is not a string literal.
	StringParser::TStringList list;

	while(current_.IsString()) {
		list.Add(current_);
		EatToken();
	}

	// Combine the tokens and handle escape sequences.
	StringInfo info = StringParser(diag_).Parse(list);
	return exprSema_->HandleString(info, list[0].Location());	
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseNumber() {
	// First convert the token (character representation) to a number.
	// If it's valid choose the type according to the rules from C99:6.4.1.5.
	LocationInfo location = current_.Location();
	NumberInfo info = NumberParser(diag_).Parse(current_);
	EatToken(); // Skip over the number.

	return exprSema_->HandleNumber(info, location);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseCharacter() {
	// Convert the character (handles escape sequences and multiple letters
	// in the constant) and select the type.
	LocationInfo location = current_.Location();
	CharInfo info = CharParser(diag_).Parse(current_);
	EatToken(); // Skip the character.

	return exprSema_->HandleCharacter(info, location);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseExpression() {
	// expression:
	//		assignment-expression
	//		expression , assignment-expression
	shared<Expression> left = ParseOtherExpression(false /* isUnary */);

	// Parse the rest of the expression until ; or EOF.
	return ParseBinaryOp(left, GetPrecedence(TokenKind::Comma));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseAssignmentExpression() {
	// assignment-expression:
	//		conditional-expression
	//		unary-expression assignment-operator assignment-expression
	shared<Expression> left = ParseOtherExpression(false /* isUnary */);

	// Parse the rest of the expression to the first comma
	// (or ; EOF if no comma is found until then).
	return ParseBinaryOp(left, GetPrecedence(TokenKind::Eq));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseConstantExpression() {
	shared<Expression> left = ParseOtherExpression(false /* isUnary */);

	// Parse only operators with precedence >= the one of the conditional operator.
	return ParseBinaryOp(left, GetPrecedence(TokenKind::Question));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::IsRightAssociative(TokenKind kind) {
	// All assignment operators and the conditional op. are right-associative.
	switch(kind) {
		case TokenKind::Eq:      
		case TokenKind::AddEq:   
		case TokenKind::SubEq:   
		case TokenKind::MulEq:   
		case TokenKind::DivEq:   
		case TokenKind::ModEq:   
		case TokenKind::AndEq:   
		case TokenKind::OrEq:    
		case TokenKind::XorEq:   
		case TokenKind::ShiftLEq:
		case TokenKind::ShiftREq:
		case TokenKind::Question: return true;
	};

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseBinaryOp(shared<Expression> left, int minPrecedence) {
	// This is the same algorithm that is used by the Preprocessor, with some changes.
	// Parse the operator. If it's precedence is smaller than the  minimum one
	// exit and let a previous invocation of this method handle it.
	TokenKind kind = current_.Kind();
	LocationInfo operandLocation = current_.Location();
	int precedence = GetPrecedence(kind);

	while(precedence >= minPrecedence) {
		shared<Expression> condLeft;
		EatToken(); // Skip over op.

		// The conditional operator (E ? E1:E2) needs special treatment.
		if(kind == TokenKind::Question) {
			// We parsed the condition expression. Now parse the left expression.
			// logical-OR-expression ? expression : conditional-expression
			condLeft = ParseExpression();

			// The next token should be :
			if(current_.Kind() == TokenKind::Colon) {
				EatToken();
			}
			else {
				diag_->Report(Error::CONDITIONAL_MISSING_COLON)<<operandLocation;
				return new InvalidExpression();
			}
		}

		// Parse the right part of the expression. If it fails don't parse
		// the rest anymore; don't emit errors, they were already emitted.
		shared<Expression> right = ParseOtherExpression(false /* onlyUnary */);

		// Check the type of the operator to determine the associativity.
		// Assignment (all forms) and the conditional op. are right-associative
		// (for example, 'a=b=c' is evaluated as 'a=(b=c)'). In this case we need
		// to evaluate the right side of the expression first.
		bool isRightAssoc = IsRightAssociative(kind);

		// Get the next precedence. If the current op. is right-associative
		// or the precedence of the current op. is smaller than that of the next
		// one, parse the right part first.
		int nextPrecedence = GetPrecedence(current_.Kind());

		if((precedence < nextPrecedence) ||
		   ((precedence == nextPrecedence) && isRightAssoc)) {
			right = ParseBinaryOp(right, precedence + (isRightAssoc ? 0 : 1));

			// A new operator is considered now.
			nextPrecedence = GetPrecedence(kind);
		}

		// Apply the operator and do semantic analysis on it.
		if(condLeft) {
			// This is a conditional operator.
			left = exprSema_->HandleConditionalOp(left, condLeft, right, 
                                                  operandLocation);
		}
		else left = exprSema_->HandleBinaryOp(kind, left, right, operandLocation, 
											  contextStack_.Peek());

		kind = current_.Kind();
		precedence = GetPrecedence(current_.Kind());
		operandLocation = current_.Location();
	}

	return left;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseOtherExpression(bool onlyUnary) {
	// Handles the 'cast-expression' nonterminal and all the nonterminals
	// with higher precedence, like 'unary-expression' and 'postfix-expression'.

	// primary-expression:
	//		identifier
	//		constant
	//		string-literal
	//		( expression )
	//
	// First are handled constants and identifiers. Note that constants
	// can be followed by postfix expression, like in "abc"[1].
	if(current_.IsNumber()) {
		shared<Expression> result = ParseNumber();
		return ParsePostfixRight(result);
	}
	else if(current_.IsChar()) {
		shared<Expression> result = ParseCharacter();
		return ParsePostfixRight(result);
	}
	else if(current_.IsString()) {
		shared<Expression> result = ParseString();
		return ParsePostfixRight(result);
	}
	else if(current_.IsIdentifier()) {
		// Let semantic analysis verify that the identifier exists
		// and if it does, create a 'DeclarationExpression' object.
		Identifier name(current_.NameValue()->Name, current_.Location());
		EatToken();

		shared<Expression> result = 
                exprSema_->HandleIdentifier(name, contextStack_.Peek());

		// An identifier could be followed by a postfix-expression
		// (like 'abc.x', 'abc(1,2)', 'abc[x]', etc.).
		return ParsePostfixRight(result);
	}
	else if(IsOpenParen()) {
		// This can be either a grouping paren, a cast, or a compound literal.
		// It can't be a cast if we're allowed to parse only unary expressions.
		EatToken(); // Skip over (

		ParenExprType type;
		shared<Expression> result = ParseParenExpression(onlyUnary == false, type);

		// If the expression is a compound literal or a grouping paren it could be
		// followed by a postfix-expression (like in '(int[]){1,2}[0]' or '(E)[0]').
		if(type != Expr_Cast) {
            return ParsePostfixRight(result);
        }
		else return result;
	}
	else if(Kwd(current_) == Keyword_Sizeof) {
		// 'sizeof' can be applied to a expression or a type name.
		return ParseSizeof();
	}
	
	// unary-expression:
	//		postfix-expression
	//		++ unary-expression
	//		-- unary-expression
	//		unary-operator cast-expression
	//		sizeof unary-expression
	//		sizeof ( type-name )
	//
	// unary-operator: one of
	//		& * + - ~ !
	switch(current_.Kind()) {
		case TokenKind::Inc:
		case TokenKind::Dec: {
			// Unary ++ or --. Parse the unary expression that follows
			// and let semantic analysis do the check and create the expression.
			TokenKind kind = current_.Kind();
			EatToken(); // Skip over ++/--.

			shared<Expression> right = ParseOtherExpression(true /* onlyUnary */);
			return exprSema_->HandleUnaryOp(kind, right, contextStack_.Peek());
		}
		case TokenKind::And: // address-of
		case TokenKind::Mul: // indirection
		case TokenKind::Add:
		case TokenKind::Sub:
		case TokenKind::Tilde:
		case TokenKind::Not: {
			// Unary operators can be followed by a cast-expression.
			TokenKind kind = current_.Kind();
			EatToken(); // Skip over the operator.

			shared<Expression> right = ParseOtherExpression(false /* onlyUnary */);
			return exprSema_->HandleUnaryOp(kind, right, contextStack_.Peek());
		}
	}

	// Call the extension handlers. 
	// If there was a valid extension return the created expression.
	ExtensionContext context(Context_Prefix, nullptr /* left */);
	
	if(HandleUnknown(context)) {
		if(Expression::IsInvalid(context.LeftExpression)){
			return new InvalidExpression();
		}

		// Parse the postfix expression if the extension says so.
		if(context.CanHavePostfix) {
			return ParsePostfixRight(context.LeftExpression);
		}
		else return context.LeftExpression;
	}
	
	// If we're here the expression is invalid.
	diag_->Report(Error::EXPECTED_EXPRESSION)<<current_.Location();
	return new InvalidExpression();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
Parser::ParseParenExpression(bool castAllowed, ParenExprType& type) {
	// If we are allowed to parse only unary expressions what's between
	// parens cannot be a cast. We try to see if these are grouping
	// parens by looking at the first token after (. If it's a specifier
	// or type qualifier the expression is a compound literal or cast.
	// It's a compound literal if after the type-name { is found.
	if(IsTypeStartToken()) {
		LocationInfo startLocation = current_.Location();

		const Type* exprType = ParseTypename();

		if(exprType == nullptr) {
			type = Expr_Invalid; // We couldn't figure out the type yet.
			return new InvalidExpression();
		}

		// A ) should be found now.
		if(IsCloseParen() == false) {
			type = Expr_Invalid; // We couldn't figure out the type yet.
			diag_->Report(Error::EXPECTED_CLOSE_PAREN)<<current_.Location();
			return new InvalidExpression();
		}
		else EatToken(); // Skip the )

		// If the current token is { this is a compound literal.
		if(IsOpenCurly()) {
			// Parse the initializer.
			type = Expr_Compound;
			shared<InitInfo> initializer = new InitInfo();

			if(ParseInitializer(initializer, true /* allowDesign */) == false) {
				// Failed to parse the initializer, give up.
				return new InvalidExpression();
			}

			// Let semantic analysis create a 'CompoundExpression' object
			// and apply the initializer to it.
			return exprSema_->HandleCompoundLiteral(exprType, initializer, startLocation, 
                                                    contextStack_.Peek());
		}
		else if(castAllowed) { // Don't parse the next expression if not allowed.
			// cast-expression:
			//		unary-expression
			//		( type-name ) cast-expression
			type = Expr_Cast;

			// Parse the cast-expression and perform semantic analysis on the cast.
			shared<Expression> castExpr = ParseOtherExpression(false /* onlyUnary */);
			
			return exprSema_->HandleCast(exprType, castExpr, startLocation, 
                                         contextStack_.Peek());
		}
		else {
			// Return a NullExpression that contains the parsed type. Used for 'sizeof'
            // (like in 'sizeof(int)' - the parens are actually optional).
			return new NullExpression(exprType);
		}
	}
	else {
		// This is a grouping paren. Parse the contained expression and
		// return it without further checks.
		type = Expr_Grouping;
		shared<Expression> expr = ParseExpression();

		// The closing paren ) should now be found.
		if(IsCloseParen() == false) {
			diag_->Report(Error::EXPECTED_CLOSE_PAREN)<<current_.Location();
			return new InvalidExpression();
		}
		else EatToken(); // Skip over )

		return expr;
	}

	// If we reached this point we are not allowed to parse casts.
	type = Expr_Cast;
	return new InvalidExpression();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParsePostfixRight(shared<Expression> left) {
	// postfix-expression:
	//		primary-expression
	//		postfix-expression [ expression ]
	//		postfix-expression ( argument-expression-list-opt )
	//		postfix-expression . identifier
	//		postfix-expression -> identifier
	//		postfix-expression ++
	//		postfix-expression --
	//
	// This methods parses the suffix of a postfix expression ([], (), ., ->, ++, --).
	// If none of these tokens are found it is not an error, it just means 
	// the suffix part doesn't exist. We return the input expression in this case.
	LocationInfo startLocation = current_.Location();

	if(IsOpenParen()) { // (
		// Function call is handled separately.
		left = ParseFunctionCall(left);
	}
	else if(IsOpenSquare()) { // [
		// A subscript is starting. Parse the inner expression and let
		// semantic analysis validate and create the resulting expression.
		EatToken(); // Skip over [
		shared<Expression> expr = ParseExpression();

		// ] should be found now.
		if(IsCloseSquare() == false) {
			diag_->Report(Error::EXPECTED_CLOSE_SQUARE)<<current_.Location();
			return new InvalidExpression();
		}
		else EatToken(); // Skip over ]

		left = exprSema_->HandleSubscript(left, expr, startLocation, 
										  current_.Location(), 
                                          contextStack_.Peek());
	}
	else if((current_.Kind() == TokenKind::Dot) ||   // .
			(current_.Kind() == TokenKind::Arrow)) { // ->
		bool isPointer = current_.Kind() == TokenKind::Arrow;
		EatToken();

		// The name of the member should follow.
		if(current_.IsIdentifier() == false) {
			diag_->Report(Error::MEMBER_IDENTIFIER_EXPECTED)<<current_.Location();
			return new InvalidExpression();
		}

		// Let semantic analysis validate and create the expression.
		shared<Identifier> name = Identifier::FromToken(current_);
		EatToken();

		left = exprSema_->HandleMember(left, name, isPointer, 
                                       contextStack_.Peek());
	}
	else if((current_.Kind() == TokenKind::Inc) || // ++
			(current_.Kind() == TokenKind::Dec)) { // --
		// Postfix increment/decrement operator (handled together).
		bool isIncrement = current_.Kind() == TokenKind::Inc;
		EatToken();

		left = exprSema_->HandlePostfixIncDecOp(left, isIncrement, 
                                                contextStack_.Peek());
	}
	else  {
		// Call the extension handlers. 
		// If there was a valid extension return the created expression.
		ExtensionContext context(Context_Prefix, left);
	
		if(HandleUnknown(context)) {
			if(context.LeftExpression == nullptr) {
				return new InvalidExpression();
			}

			// Parse the postfix expression if the extension says so.
			if(context.CanHavePostfix) {
				return ParsePostfixRight(context.LeftExpression);
			}
			else return context.LeftExpression;
		}

		// This is not a postfix suffix if we arrived here.
		return left;
	}

	// Other suffixes may follow after this one, like in '[0][1]' or 'a->p[0]'.
	if(left) {
		return ParsePostfixRight(left);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseSizeof() {
	// If ( is not found an unary expression must follow.
	// Else it can be a compound statement or a parenthesized expression.
	LocationInfo startLocation = current_.Location();
    shared<Expression> target;

    // Skip over 'sizeof'.
	EatToken(); 

	if(IsOpenParen() == false) {
		target = ParseOtherExpression(true /* onlyUnary */);
	}
	else {
		// We instruct 'ParseParenExpression' to stop parsing if it finds something
		// that appears to be a cast (like in 'sizeof(int)').
		// Else if would continue to parse what is after the cast as 'cast-expression'.
		ParenExprType type;
		EatToken(); // Skip over (

		target = ParseParenExpression(false /* castAllowed */, type);

		// If it was a grouping paren case there could be a postfix-expression
		// after that is part of the argument for 'sizeof' (like 'sizeof (p)[0]'). 
		// This is possible because of the following expression chain:
		// unary => postfix => primary- => ( expression ) + postfix
		if(type == Expr_Grouping) {
			target = ParsePostfixRight(target);
		}
	}

	// Validate and create the expression.
	return exprSema_->HandleSizeof(target, startLocation, current_.Location(), 
                                   contextStack_.Peek());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> Parser::ParseFunctionCall(shared<Expression> left) {
	// First let semantic analysis verify that 'left' can be used as a function.
	shared<Expression> expr = exprSema_->HandleCallBegin(left, contextStack_.Peek());

	if(Expression::IsInvalid(expr)) {
		// A call is invalid; skip to the end of the argument list )
		SkipToCloseParen();
		return expr;
	}

	// argument-expression-list:
	//		assignment-expression
	//		argument-expression-list , assignment-expression
	//
	CallExpression* callExpr = expr->As<CallExpression>();
	LocationInfo startLocation = left->Location();
    auto& arguments = callExpr->Arguments();

    // Skip over (
	EatToken();

	// Parse until ) is found (or ;, EOF, but it's an error already).
	while(IsCloseParen() == false) {
		shared<Expression> argument = ParseAssignmentExpression();

		if(argument == nullptr) {
			// The argument is invalid; we stop here and skip to ).
			SkipToCloseParen();
			return nullptr;
		}

		// Add the argument to the list. It will be checked
		// by semantic analysis when the whole call is checked.
		arguments.Add(argument);

		// If we stopped at , more arguments need to follow.
		// Else it should be ), else it is considered an error.
		if(IsComma()) EatToken(); // Skip over  ,
		else break; // No other token is allowed.
	}

	// ) should be found at the end of the list.
	if(IsCloseParen() == false) {
		// ) not found, skip to the location of ) (or any accepted token, like ; EOF).
		diag_->Report(Error::EXPECTED_CLOSE_PAREN)<<current_.Location();
		SkipToCloseParen();
		return nullptr;
	}
	else EatToken(); // Skip over )

	// Perform semantic analysis on the call and create the expression.
	return exprSema_->HandleCallEnd(expr, startLocation, current_.Location(), 
                                    contextStack_.Peek());
}

} // namespace Parsing
#endif