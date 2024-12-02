// Copyright (c) Lup Gratian
//
// Implements the statement parsing methods.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Parser.hpp"
#include "../AST/ASTDotPrinter.hpp"

namespace Parsing {

shared<Statement> Parser::ParseStatement() {
	// This parses statements, expressions and declarations and acts like
	// a dispatch mechanism to the appropriate parsing method.
	//
	// statement:
	//		labeled-statement
	//		compound-statement
	//		expression-statement
	//		selection-statement
	//		iteration-statement
	//		jump-statement
	if(current_.IsIdentifier()) {
		// This is a labeled statement if the identifier is followed by :
		// Else fall through, because it must be an expression.
		if(PeekNext().Kind() == TokenKind::Colon) {
			return ParseLabeledStatement();
		}
	}
	else if(current_.IsKeyword()) {
		switch(Kwd(current_)) {
			// labeled-statement:
			//		identifier : statement
			//		case constant-expression : statement
			//		default : statement
			case Keyword_Case: {
				return ParseCaseStatement();
				break;
			}
			case Keyword_Default: {
				return ParseDefaultStatement();
				break;
			}

			// selection-statement:
			//		if ( expression ) statement
			//		if ( expression ) statement else statement
			//		switch ( expression ) statement
			case Keyword_If: {
				return ParseIfStatement();
				break;
			}
			case Keyword_Switch: {
				return ParseSwitchStatement();
				break;
			}
			case Keyword_Else: {
				// This is an error. If we find 'else' here the user probably
				// forgot ; or }. We let the caller handle this situation.
				return nullptr;
			}

			// iteration-statement:
			//		while ( expression ) statement
			//		do statement while ( expression ) ;
			//		for ( expression-opt ; expression-opt ; expression-opt ) statement
			//		for ( declaration expression-opt ; expression-opt ) statement
			case Keyword_While: {
				return ParseWhileStatement();
				break;
			}
			case Keyword_Do: {
				return ParseDoStatement();
				break;
			}
			case Keyword_For: {
				return ParseForStatement();
				break;
			}

			// jump-statement:
			//		goto identifier ;
			//		continue ;
			//		break ;
			//		return expression-opt ;
			case Keyword_Goto: {
				return ParseGotoStatement();
				break;
			}
			case Keyword_Continue: {
				return ParseContinueStatement();
				break;
			}
			case Keyword_Break: {
				return ParseBreakStatement();
				break;
			}
			case Keyword_Return: {
				return ParseReturnStatement();
				break;
			}
		}
	}

	// If the token is { it's a compound statement.
	if(IsOpenCurly()) {
		return ParseCompoundStatement();
	}
	else if(IsSemiColon()) {
		// This is an empty statement.
		LocationInfo location = current_.Location();
		EatToken();
		return new NullStatement(location);
	}
	else if(IsTypeStartToken()) {
		// This is a declaration.
		return ParseDeclarationStatement();
	}
	else {
		// It can be either an expression or an extension.
		ExtensionContext context(Context_Statement);

		if(HandleUnknown(context)) {
			return context.ResultStatement;
		}
		else {
			// It should be an expression otherwise.
			return ParseExpressionStatement();
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseLabeledStatement() {
	// Get the label name and skip over it and over the colon.
	shared<Identifier> label = Identifier::FromToken(current_);
	LocationInfo statementStart = current_.Location();

	EatToken(); // Skip over label name.
	EatToken(); // Skip over :

	// Parse the referenced statement. If it fails we still create the labeled
	// statement, because otherwise we would get an error for each 'goto' that
	// refers to this label (we use a 'NullStatement' instead).
	shared<Statement> statement = ParseStatement();

	if(statement == nullptr) {
		statement = new NullStatement(statementStart, true /* isError */);
	}

	return statementSema_->HandleLabel(label, statement, contextStack_.Peek());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseIfStatement() {
	LocationInfo startLocation = current_.Location();

    // Skip over 'if'.
	EatToken(); 

	// The next token should be (, then an expression followed by ).
	if(IsOpenParen() == false) {
		// Report and skip after ; or }
		diag_->Report(Error::IF_EXPECTED_OPEN_PAREN)<<current_.Location();

		SkipToBlockEnd(true /* eat */, true /* stopAtSemi */);
		return nullptr;
	}
	else EatToken(); // Skip over (

	// C99:6.8.4.1.3: the body and the controlling expression are placed 
	// in a block  that is the child of the current one.
	shared<DeclarationContext> context = contextStack_.Peek();
	shared<DeclarationContext> ifContext = 
            new DeclarationContext(ScopeType::Block, BlockFlags::If, contextStack_.Peek());

	contextStack_.Push(ifContext);
	bool inParens = IsOpenParen();

	shared<Expression> condExpr = ParseExpression();

	if(Expression::IsInvalid(condExpr)) {
		// The expression is invalid. Skip to ) and try to parse the body.
		SkipToCloseParen(false /* eat */);
	}

	if(IsCloseParen() == false) {
		// We couldn't find the ), give up.
		diag_->Report(Error::EXPECTED_CLOSE_PAREN)<<current_.Location();

		contextStack_.Pop();
		return nullptr;
	}
	else EatToken(); // Skip over )

	// Parse the body. If we fail skip to } se we can parse
    // a possible 'else' part too.
	LocationInfo bodyLoc = current_.Location();

	shared<Statement> bodyStatement = ParseStatement();

	if(bodyStatement == nullptr) {
		SkipToCloseCurly();
	}

	// The 'if' context is no longer needed.
	contextStack_.Pop();

	// There may be an 'else' part. The scope is not the same 
    // as for the 'if' body, so make a new context and use it instead.
	shared<Statement> elseStatement;
	LocationInfo elseLocation;
	bool parsedElse = false;

	if(Kwd(current_) == Keyword_Else) {
		parsedElse = true;
		elseLocation = current_.Location();
		shared<DeclarationContext> elseContext = 
                new DeclarationContext(ScopeType::Block, BlockFlags::If, context);

		// Skip over 'else'.
		EatToken();

        contextStack_.Push(elseContext);
		elseStatement = ParseStatement();
		contextStack_.Pop();
	}

	// If the controlling expression or both statements are invalid give up.
	// Else replace the invalid statement with a 'NullStatement' so we can continue.
	if(condExpr == nullptr) {
        return nullptr;
    }
	else if((bodyStatement == nullptr) && (elseStatement == nullptr)) {
		return nullptr;
	}
	else if(bodyStatement == nullptr) {
		bodyStatement = new NullStatement(bodyLoc, true /* isError */);
	}
	else if(parsedElse && (elseStatement == nullptr)) {
		elseStatement = new NullStatement(elseLocation, true /* isError */);
	}

	// Perform semantic analysis and create the 'if' statement.
	return statementSema_->HandleIf(condExpr, bodyStatement, elseStatement, inParens, 
							        contextStack_.Peek(), startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseSwitchStatement() {
	// Parse the controlling expression, then the body. The statement if pushed
	// on a 'switch' stack and some checks are made at the beginning.
	// When the 'switch' ends it is popped from the stack and the labels are checked.
	LocationInfo startLocation = current_.Location();

    // Skip over 'switch'.
	EatToken(); 

	// C99:6.8.4.3: 'switch' is a block, so create a new context.
	shared<DeclarationContext> switchContext =
            new DeclarationContext(ScopeType::Block, BlockFlags::Switch, contextStack_.Peek());
	contextStack_.Push(switchContext);

	// ( should be found now.
	if(IsOpenParen() == false) {
		// Report and skip after ; or }
		diag_->Report(Error::SWITCH_MISSING_EXPR)<<current_.Location();

		SkipToBlockEnd(true /* eat */, true /* stopAtSemi */);
		contextStack_.Pop();
		return nullptr;
	}
	else EatToken(); // Skip over (

	// Parse the controlling expression. 
	// If we fail skip to the end of the body and give up.
	shared<Expression> expr = ParseExpression();

	if(Expression::IsInvalid(expr)) {
        return nullptr;
    }

	// ) should be found now.
	if(IsCloseParen() == false) {
		// We couldn't find the ), give up.
		diag_->Report(Error::EXPECTED_CLOSE_PAREN)<<current_.Location();

		contextStack_.Pop();
		return nullptr;
	}
	else EatToken(); // Skip over )

	// Create the 'SwitchStatement' object. This pushes the switch on the stack and
	// validates the expression. If it fails skip the whole body.
	shared<SwitchStatement> switchStatement = 
            statementSema_->HandleSwitchBegin(expr, contextStack_.Peek(), 
                                              startLocation);

	if(switchStatement == nullptr) {
		contextStack_.Pop(); // Don't leave the 'switch' on the stack.
		return nullptr;
	}

	// The body is starting here. Notify and parse the body.
	// If there was an error try to skip to } and give up.
	shared<Statement> bodyStatement = ParseStatement();
	contextStack_.Pop(); // 'switchCtx' not needed anymore.

	if(bodyStatement == nullptr) {
		statementSema_->PopSwitch(); // Don't leave the 'switch' on the stack.
		return nullptr;
	}

	// Make checks on the whole switch and create the statement.
	return statementSema_->HandleSwitchEnd(switchStatement, bodyStatement, 
                                           contextStack_.Peek(), startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseCaseStatement() {
	// The expression must be an ICE an be followed by :
	// Because 'case' can be nested (like in 'case 1: case 2: {}') we parse all
	// 'case' statements here so we don't overflow the stack.
	List<shared<Expression>> caseValues(16);
	List<LocationInfo> locationList(16);
	shared<Statement> bodyStatement;
	int caseCount = 0;

	while(Kwd(current_) == Keyword_Case) {
		locationList.Add(current_.Location());

        // Skip over 'case'.
		EatToken();

		// Parse the expression. If we fail skip to : so we parse the body.
		shared<Expression> value = ParseExpression();

		if(Expression::IsInvalid(value)) {
			SkipToColon(false);
		}
		else {
			caseValues.Add(value);
			caseCount++;
		}

		// : should now be found. If not skip to the end of the block and give up.
		if(IsColon() == false) {
			diag_->Report(Error::SWITCH_EXPECTED_COLON)<<current_.Location();

			SkipToBlockEnd(true /* eat */, IsOpenCurly() == false /* stopAtSemi */);
			return nullptr;
		}
		else EatToken(); // Skip over :

		// Parse the statement associated with the case. If it begins with
		// 'case' we parse it here; else parse it the standard way and exit the loop.
		if(Kwd(current_) == Keyword_Case) {
            continue;
        }
		
		bodyStatement = ParseStatement();

		if(bodyStatement == nullptr) {
			// Failed to parse the statement, give up.
			return nullptr;
		}
		else break; // We're done with the case list.
	}

	// Build the 'case' statements form right to left.
	// If we fail at any step give up.
	if(caseCount == 0) {
        return nullptr;
    }
	
    shared<Statement> caseStatement = 
            statementSema_->HandleCase(caseValues[caseCount- 1], bodyStatement, 
									   contextStack_.Peek(), 
                                       locationList[caseCount- 1]);
	if(caseStatement == nullptr) {
        return nullptr;
    }

    for(int i = caseCount - 2; i >= 0; i--) {
        caseStatement = statementSema_->HandleCase(caseValues[i], caseStatement, 
                                                   contextStack_.Peek(), 
                                                   locationList[i]);
    }

	return caseStatement;
}
	
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseDefaultStatement() {
	LocationInfo startLocation = current_.Location();

    // Skip over 'default'.
	EatToken(); 

	// : should be found now. If not skip to the end of the statement and give up.
	if(IsColon() == false) {
		diag_->Report(Error::SWITCH_EXPECTED_COLON)<<current_.Location();

		SkipToBlockEnd(true /* eat */, IsOpenCurly() == false /* stopAtSemi */);
		return nullptr;
	}
	else EatToken(); // Skip over :

	// Parse the associated statement.
	shared<Statement> bodyStatement = ParseStatement();

	if(bodyStatement == nullptr) {
		// Failed to parse the statement, give up.
		return nullptr;
	}

	return statementSema_->HandleDefault(bodyStatement, contextStack_.Peek(),
                                         startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseWhileStatement() {
	// Parse the controlling expression, then the body.
	LocationInfo startLocation = current_.Location();

    // Skip over 'while'.
	EatToken();

	// ( should be found now.
	if(IsOpenParen() == false) {
		// Report and skip after ; or }
		diag_->Report(Error::WHILE_EXPECTED_OPEN_PAREN)<<current_.Location();

		SkipToBlockEnd(true /* eat */, IsOpenCurly() == false /* stopAtSemi */);
		return nullptr;
	}
	else EatToken(); // Skip over (

	// C99:6.8.5.5: an iteration statement is a block whose scope 
	// is a strict subset of the scope of its enclosing block.
	shared<DeclarationContext> whileContext = 
            new DeclarationContext(ScopeType::Block, BlockFlags::Loop, contextStack_.Peek());

	contextStack_.Push(whileContext);
	shared<Expression> condExpr = ParseExpression();

	if(Expression::IsInvalid(condExpr)) {
		// The expression is invalid. Skip to ) and try to parse the body.
		SkipToCloseParen(false /* eat */);
	}

	if(IsCloseParen() == false) {
		// We couldn't find the ), give up.
		diag_->Report(Error::EXPECTED_CLOSE_PAREN)<<current_.Location();
		SkipToBlockEnd();
		contextStack_.Pop();
		return nullptr;
	}
	else EatToken(); // Skip over )

	// The body has its own scope. Create it only if the body is not a
	// compound statement (because it creates his own scope).
	shared<Statement> bodyStatement;

	if(IsOpenCurly()) {
		bodyStatement = ParseStatement();
	}
	else {
		shared<DeclarationContext> bodyContext = 
                new DeclarationContext(ScopeType::Block, BlockFlags::Default, whileContext);

		contextStack_.Push(bodyContext);
		bodyStatement = ParseStatement();
		contextStack_.Pop();
	}

	// 'whileCtx' not needed anymore.
	contextStack_.Pop();

	// If there was an error there is no reason to continue.
	if((bodyStatement == nullptr) || (condExpr == nullptr)) {
        return nullptr;
    }

	return statementSema_->HandleWhile(condExpr, bodyStatement, 
                                       contextStack_.Peek(), startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseDoStatement() {
	// Parse the body, then the controlling expression.
	LocationInfo startLocation = current_.Location();

    // Skip over 'do'.
	EatToken();

	// C99:6.8.5.5: an iteration statement is a block whose scope 
	// is a strict subset of the scope of its enclosing block.
	shared<DeclarationContext> doContext =
            new DeclarationContext(ScopeType::Block, BlockFlags::Loop, contextStack_.Peek());
	contextStack_.Push(doContext);

	// The body needs another scope, but create it only if it's not a 
	// compound statement (because it creates his own scope).
	shared<Statement> bodyStatement;

	if(IsOpenCurly()) {
		bodyStatement = ParseStatement();
	}
	else {
		shared<DeclarationContext> bodyContext = 
                new DeclarationContext(ScopeType::Block, BlockFlags::Default, doContext);

		contextStack_.Push(bodyContext);
		bodyStatement = ParseStatement();
		contextStack_.Pop();
	}

	// If the body is invalid skip to 'while' so that we read the expression.
	if(bodyStatement == nullptr) {
		SkipToKwd(Keyword_While, false /* eat */);
	}

	// 'while' should be found now.
	LocationInfo whileLocation = current_.Location();

	if(Kwd(current_) != Keyword_While) {
		diag_->Report(Error::DO_MISSING_WHILE)<<current_.Location();

		SkipToSemiColon();
		contextStack_.Pop();
		return nullptr; // No reason to continue.
	}
	else EatToken(); // Skip over 'while'

	// ( should be found now.
	if(IsOpenParen() == false) {
		// Report and skip after ; or }
		diag_->Report(Error::WHILE_EXPECTED_OPEN_PAREN)<<current_.Location();

		SkipToBlockEnd();
		contextStack_.Pop();
		return nullptr;
	}
	else EatToken(); // Skip over (

	shared<Expression> condExpr = ParseExpression();
	if(Expression::IsInvalid(condExpr)) {
		// The expression is invalid, skip to )
		SkipToCloseParen(false /* eat */);
		contextStack_.Pop();
		return nullptr;
	}

	if(IsCloseParen() == false) {
		// We couldn't find the ), give up.
		diag_->Report(Error::EXPECTED_CLOSE_PAREN)<<current_.Location();

		SkipToBlockEnd();
		contextStack_.Pop();
		return nullptr;
	}
	else EatToken(); // Skip over )

	// 'doCtx' not needed anymore.
	contextStack_.Pop();

	// ; should be found now.
	if(IsSemiColon() == false) {
		diag_->Report(Error::EXPECTED_DECLARATION_SEMICOLON)<<current_.Location();

		SkipToBlockEnd(true /* eat */, true /* stopAtSemi */);
		return nullptr;
	}
	else EatToken(); // Skip over ;

	// If the body is invalid give up.
	if(bodyStatement == nullptr) {
        return nullptr;
    }
	else return statementSema_->HandleDo(condExpr, bodyStatement, contextStack_.Peek(), 
                                         startLocation, whileLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseForStatement() {
	// The first part allows declarations to be made, so we parse it
	// as a statement. The declarations are not visible outside the 'for'.
	// All three parts are optional; part 2 is replaced by a constant so that
	// it forms a infinite loop.
	LocationInfo startLocation = current_.Location();
	bool invalid = false;

    // Skip over 'for'.
	EatToken();

	// ( should be found now.
	if(IsOpenParen() == false) {
		// Report and skip after ; or }
		diag_->Report(Error::FOR_EXPECTED_OPEN_PAREN)<<current_.Location();

		SkipToBlockEnd(true /* eat */, true /* stopAtSemi */);
		return nullptr;
	}
	else EatToken(); // Skip over (

	// Parse the 'clause-1' statement. 
	// If it's a 'NullStatement' it's not an error; 
	// it means that the statement is empty. A new scope is entered.
	shared<DeclarationContext> forContext = 
            new DeclarationContext(ScopeType::Block, BlockFlags::Loop, contextStack_.Peek());
	contextStack_.Push(forContext);

	// Note that this guarantees that ; is skipped.
	shared<Statement> initStatement = ParseStatement();

	if(initStatement == nullptr) {
		// The statement is invalid, skip to ; and continue.
		SkipToSemiColon(false);
	}

	// Parse the second part. If the current token is ; the expression is empty.
	shared<Expression> condExpr;

	if(IsSemiColon() == false) {
		condExpr = ParseExpression();

		if(Expression::IsInvalid(condExpr)) {
			// The expression is invalid. The ; will be skipped below.
			invalid = true;
		}
	}

	// ; should be found now.
	if(IsSemiColon() == false) {
		diag_->Report(Error::EXPECTED_FOR_SEMICOLON)<<current_.Location();
		SkipToSemiColon();
	}
	else EatToken(); // Skip over ;

	// Parse the third part. If ) is found the expression is empty.
	shared<Expression> incExpr;

	if(IsCloseParen() == false) {
		incExpr = ParseExpression();

		if(incExpr == nullptr) {
			// The expression is invalid, skip to ; and continue.
			invalid = true;
			SkipToSemiColon();
		}
	}

	// ) should be found now.
	if(IsCloseParen() == false) {
		diag_->Report(Error::EXPECTED_FOR_CLOSE_PAREN)<<current_.Location();
		SkipToBlockEnd(true /* eat */, true /* stopAtSemi */);
	}
	else EatToken(); // Skip over )

	// Now read the body. It needs another scope, but create it only if it's not a 
	// compound statement (because it creates his own scope).
	shared<Statement> bodyStatement;

	if(IsOpenCurly()) {
		bodyStatement = ParseStatement();
	}
	else {
		shared<DeclarationContext> bodyContext = 
                new DeclarationContext(ScopeType::Block, BlockFlags::Default, forContext);

		contextStack_.Push(bodyContext);
		bodyStatement = ParseStatement();
		contextStack_.Pop();
	}

	// 'forCtx' not needed anymore.
	contextStack_.Pop();

	// If there was an error try to skip to } or ; and give up.
	if(bodyStatement == nullptr) {
		invalid = true;
		SkipToBlockEnd(true /* eat */, true /* stopAtSemi */);
	}

	// If one of the expressions or the body was invalid give up.
	if(invalid) {
        return nullptr;
    }
	else return statementSema_->HandleFor(initStatement, condExpr, incExpr, 
									      bodyStatement, contextStack_.Peek(),
                                          startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseGotoStatement() {
	LocationInfo startLocation = current_.Location();

    // Skip over 'goto'.
	EatToken();

	// An identifier should be found.
	if(current_.IsIdentifier() == false) {
		diag_->Report(Error::GOTO_MISSING_TARGET)<<startLocation;
		SkipToSemiColon();
		return nullptr;
	}

	shared<Identifier> name = Identifier::FromToken(current_);
	EatToken();

	// The statement must end with ;
	if(IsSemiColon() == false) {
		diag_->Report(Error::EXPECTED_DECLARATION_SEMICOLON)<<current_.Location();
		return nullptr;
	}
	else EatToken(); // Skip over ;

	return statementSema_->HandleGoto(name, contextStack_.Peek(), startLocation);
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseContinueStatement() {
	LocationInfo startLocation = current_.Location();

    // Skip over 'continue'.
	EatToken();

	// The statement must end with ;
	if(IsSemiColon() == false) {
		diag_->Report(Error::EXPECTED_DECLARATION_SEMICOLON)<<current_.Location();
		return nullptr;
	}
	else EatToken(); // Skip over ;

	return statementSema_->HandleContinue(contextStack_.Peek(), startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseBreakStatement() {
	LocationInfo startLocation = current_.Location();

    // Skip over 'break'.
	EatToken();

	// The statement must end with ;
	if(IsSemiColon() == false) {
		diag_->Report(Error::EXPECTED_DECLARATION_SEMICOLON)<<current_.Location();
		return nullptr;
	}
	else EatToken(); // Skip over ;

	return statementSema_->HandleBreak(contextStack_.Peek(), startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseReturnStatement() {
	LocationInfo startLocation = current_.Location();

    // Skip over 'return'.
	EatToken();

	// Parse the optional expression. It must be followed by ;
	shared<Expression> returnExpr;

	if(IsSemiColon() == false) {
		returnExpr = ParseExpression();
		if(returnExpr == nullptr) {
			// The expression is invalid, skip to ; and give up.
			SkipToBlockEnd(true /* eat */, true /* stopAtSemi */);
			return nullptr;
		}
	}

	if(IsSemiColon() == false) {
		SkipToSemiColon();
		return nullptr;
	}
	else EatToken(); // Skip over ;

	// Semantic analysis will verify if the expression is allowed and valid.
	return statementSema_->HandleReturn(returnExpr, contextStack_.Peek(), 
                                   startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseCompoundStatement() {
	// A new scope is active between {}.
	shared<DeclarationContext> blockContext =
            new DeclarationContext(ScopeType::Block, BlockFlags::Default, contextStack_.Peek());

	contextStack_.Push(blockContext);
	shared<Statement> statement = ParseCompoundBody();
	contextStack_.Pop();
	return statement;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseCompoundBody() {
	shared<CompoundStatement> compound = new CompoundStatement(current_.Location());
	auto& children = compound->Children();
	bool invalid = false;

    // Skip over {
	EatToken();

	// Parse statements until } is found (or EOF is reached). 
	while((IsCloseCurly() == false) && (current_.IsEOF() == false)) {
		// Parse the child statement. If it's an error we don't add it
		// to the list, but we continue parsing.
		shared<Statement> child = ParseStatement();

		if(child) {
			children.Add(child);
		}
		else if(Kwd(current_) == Keyword_Else) {
			// Something like 'if(E1) { E2 else E3' (the user forgot }).
			// Skip over the rest of the compound in this case.
			break;
		}
	}

	// Check that we stopped at }.
	if(IsCloseCurly() == false) {
		// Continue even without the }
		invalid = true;
		SkipToBlockEnd(true /* eat */);
		diag_->Report(Error::EXPECTED_CLOSE_CURLY)<<current_.Location();
	}
	else EatToken(); // Skip over }

	// No semantic analysis needs to be done (each child was already analyzed).
	return invalid ? nullptr : compound;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseDeclarationStatement() {
	// Parse the declaration and wrap it into a 'DeclarationStatement' object.
	// If the declaration is invalid return a 'NullStatement'.
	bool functionDef;
    LocationInfo startLocation = current_.Location();
	shared<Declaration> declaration = ParseDeclaration(functionDef);

	if(declaration == nullptr) {
		SkipToSemiColon();
		return new NullStatement(startLocation, true /* isError */);
	}

	// The declaration should end in ;
	if(IsSemiColon() == false) {
		// Report, skip to ; (or } EOF if ; not found) and continue.
		diag_->Report(Error::EXPECTED_DECLARATION_SEMICOLON)<<current_.Location();
	}
	else EatToken(); // Skip over ;

	return new DeclarationStatement(declaration, startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> Parser::ParseExpressionStatement() {
	// Parse the expression and wrap it into a 'ExpressionStatement' object.
	// If the expression is invalid return a 'NullStatement'.
	LocationInfo startLocation = current_.Location();
	shared<Expression> expr = ParseExpression();

	if(Expression::IsInvalid(expr)) {
		SkipToSemiColon();
		return new NullStatement(startLocation, true /* isError */);
	}

	// The expression should end in ;
	if(IsSemiColon() == false) {
		// Report, skip to ; (or } EOF if ; not found) and continue.
		diag_->Report(Error::EXPECTED_EXPRESSION_SEMICOLON)<<current_.Location();
	}
	else EatToken(); // Skip over ;

	return new ExpressionStatement(expr, startLocation);
}

}; // namespace Parsing