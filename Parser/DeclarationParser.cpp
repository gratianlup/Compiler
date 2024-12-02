// DeclarationParser.hpp
// Copyright (c) Lup Gratian
//
// Implements the declaration parsing methods.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Parser.hpp"

namespace Parsing {

shared<DeclarationList> Parser::ParseDeclaration(bool& functionDef) {
	// declaration:
	//		declaration-specifiers initializer-declarator-list-opt ;
	//
	// initializer-declarator-list:
	//		initializer-declarator
	//		initializer-declarator-list , initializer-declarator
	//
	// initializer-declarator:
	//		declarator
	//		declarator = initializer
	//
	// First parse the specifiers. If no specifiers are found the declaration is invalid.
	// All declarations are placed in a 'DeclarationList' object.
	LocationInfo startLocation = current_.Location();
	SpecifierInfo info;
	functionDef = false; // Presume it's not.
	bool skipToSemi;

	if(ParseSpecifiers(info, skipToSemi) == false) {
		// Errors have already been emitted. Skip to ; if needed.
		if(skipToSemi) {
			SkipToSemiColon(false);
		}

		return nullptr;
	}

	// Parse the (optional) list of declarators.
	shared<DeclarationList> declList = new DeclarationList(startLocation);

	if(ParseDeclaratorList(info, declList, functionDef) == false) {
		return nullptr;
	}
	else if(functionDef) return declList;

	// C99:6.7.2: at least a declarator or a tag should be declared.
	if(declList->Count() == 0) {
		if((info.Struct || info.Union || info.Enum) == false) {
			diag_->Report(Error::DECLARATION_NOTHING_DECLARED)<<startLocation;
			return nullptr;
		}

		// Warn if we're declaring a tag and storage-class specifiers are used.
		if(info.HasStorage() && info.IsUserType()) {
			diag_->Report(Warning::USELESS_DECLARATION_SPECIFIERS)<<startLocation;
		}
	}

	declList->SetEndLocation(current_.Location());
	return declList;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseDeclaratorList(SpecifierInfo& info, shared<DeclarationList> declList, 
                                 bool& functionDef) {
	// initializer-declarator-list:
	//		initializer-declarator
	//		initializer-declarator-list , initializer-declarator
	// Parse declarators until ; or EOF is found.
	LocationInfo startLocation = current_.Location();
	functionDef = false;

	// If the first token is ; this is an empty declarator list.
	if(IsSemiColon()) return true;

	// Set the context.
	shared<DeclarationContext> context = contextStack_.Peek();

	while(true) {
		// Parse the declarator. Pointers, arrays and functions are handled
        // by calling 'ParseDeclarator' recursively.
        shared<DI> declInfo = new DI(nullptr);
		bool isRedeclaration;
		declInfo->Context = context;
		shared<Declaration> declaration;

		LocationInfo declStartLoc = current_.Location();
		declInfo->Location = declStartLoc;

		if(ParseDeclarator(declInfo) == false) {
			// Failed to parse the declarator. Skip to , and continue.
			// Don't emit any errors because they were already emitted.
			SkipToComma(false);
			goto declEnd;
		}

		// Check if this is the start of a function definition.
		// If the parsed declarator was not a function an error is emitted.
		// After the function no other declarator should follow.
		if(IsOpenCurly()) {
			functionDef = true;
			return ParseFunctionDefinition(declInfo, info, declStartLoc);
		}

		// Perform semantic analysis on the declarator
		// and insert it into the context if it has a name.
		declaration = declSema_->HandleDeclaratorBegin(declInfo, info, context, 
                                                       isRedeclaration);

		if(declaration == nullptr) {
			SkipToComma(false);

			// Announce the extension handlers that the declaration failed.
			NotifyDeclarationFailed();
			goto declEnd;
		}

		if(declaration->Name()) {
			// Add it to the context if it's not already added.
			// It will be found using the linked list formed between declarations.
			declList->Declarations().Add(declaration);

			if(isRedeclaration == false) {
				context->Add(declaration->Name(), declaration);
			}
		}

		// Check for initializer.
		if(current_.Kind() == TokenKind::Eq) {
			// The declarator has an initializer. Skip over the = and parse it.
			shared<InitInfo> initializer = new InitInfo();
			EatToken();
			
			if(ParseInitializer(initializer, true) == false) {
				// Failed to parse the initializer. Skip to , and continue.
				// If we really stopped at a comma eat it and continue parsing.
				// Else fall through so that the presence of ; is checked.
				SkipToComma(false);
				goto declEnd;
			}

			// Try to associate the initializer with the declarator.
			declaration = declSema_->HandleDeclaratorInitializer(declaration, info,
                                                                 initializer, context);

			if(declaration == nullptr) {
				SkipToComma(false);

				// Announce the extension handlers that the declaration failed.
				NotifyDeclarationFailed();
				goto declEnd;
			}
		}

		// Perform final checks on the declarator.
		declaration = declSema_->HandleDeclaratorEnd(declaration, info, context);

		if(declaration) {
			// Add it to the corresponding list of the translation unit.
			if(declaration->Linkage() == LinkageType::Internal) {
				unit_.Internal().Add(declaration);
				unit_.UnitDeclarations().Add(declaration);
			}
			else if(declaration->Linkage() == LinkageType::External) {
				unit_.External().Add(declaration);
				unit_.UnitDeclarations().Add(declaration);
			}

			// Notify the extension handlers that a new declaration was made.
			NotifyDeclarationSucceeded(declaration);
		}
		else {
			// Announce the extension handlers that the declaration failed.
			NotifyDeclarationFailed();
		}

declEnd:
		// Next there should be either , or ;
		if(IsComma() == false) {
			if(IsSemiColon() == false) {
				// An invalid token was found. Stop, report and skip to ;
				// Make an exception when the token is a type-start token and the
				// user forgot the ; (like in 'int a int b;').
				// Another exception is for 'int a b;' - user probably forgot ,
				if(current_.IsIdentifier()) {
					// Emit the error about the missing , and continue.
					diag_->Report(Error::EXPECTED_DECLARATION_COMMA)<<
								  RangeInfo(startLocation, current_.Location());
					continue;
				}
				else if(IsTypeStartToken() == false) {
					// We can't recover for any other token. The error will be
					// emitted by 'ParseFileDeclaration' when it doesn't find ;
					SkipToSemiColon(false);
					return false;
				}
			}

			// Declaration finished as expected.
			return true;
		}
		
		EatToken(); // Skip over ,
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::IsDeclaratorEnd() {
	// If the current token is not , = ) ; : { or EOF the declarator hasn't ended.
	// If = is found there is an initializer that must be parsed by 'ParseDeclaratorList'.
	// { means that this is a function definition.
	return IsSemiColon() || IsComma() || IsCloseParen() ||
		   (current_.Kind() == TokenKind::Eq) || IsOpenCurly() ||
		   IsColon() || current_.IsEOF();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseDeclarator(shared<DI>& declaration,
                             bool identAllowed, bool ptrAllowed) {
	// declarator:
	//		pointer-opt direct-declarator
	//
	// direct-declarator:
	//		identifier
	//		( declarator )
	//		direct-declarator [ type-qualifier-list-opt assignment-expression-opt ]
	//		direct-declarator [ static type-qualifier-list-opt assignment-expression ]
	//		direct-declarator [ type-qualifier-list static assignment-expression ]
	//		direct-declarator [ type-qualifier-list-opt * ]
	//		direct-declarator ( parameter-type-list )
	//		direct-declarator ( identifier-list-opt ) // NOT IMPLEMENTED!
	//
	// pointer:
	//		* type-qualifier-list-opt
	//		* type-qualifier-list-opt pointer
	//
	// type-qualifier-list:
	//		type-qualifier
	//		type-qualifier-list type-qualifier
	//
	// parameter-type-list:
	//		parameter-list
	//		parameter-list , ...
	//
	// parameter-list:
	//		parameter-declaration
	//		parameter-list , parameter-declaration
	//
	// parameter-declaration:
	//		declaration-specifiers declarator
	//		declaration-specifiers abstract-declarator-opt
	// First check for pointers.
	bool grouping = false;
	shared<DI> parenDecl;

	if(current_.Kind() == TokenKind::Mul) {
		// This is a pointer. Set the type, parse the (optional) qualifiers
		// and handle the rest by calling 'ParseDeclarator' again.
		// Note that a pointer can appear only in the first declarator position
		// (something like 'int a*;' is not valid);
		if(ptrAllowed == false) {
			return false;
		}

		declaration->Type = DI::Pointer;
        declaration->Location = current_.Location();
		EatToken();

		if(ParseQualifiers(declaration->Info) == false) {
			// The specifiers where not valid.
			return false;
		}
	}
	else if (current_.IsIdentifier()) {
		// This is the name of the declarator. It should appear only once.
		if(identAllowed == false) {
			// The user probably forgot the , or the =, like in 'int a b;'.
			// Continue and don't report here anything, it will be reported
			// by the caller when it doesn't find the expected token.
			return true;
		}

		declaration->Type = DI::Normal;
		declaration->Name = Identifier::FromToken(current_);
        declaration->Location = current_.Location();
		identAllowed = false;
		ptrAllowed = false;
		EatToken();
	}
	else if(IsOpenSquare()) { // [
		// This is an array.
		declaration->Type = DI::Array;
		EatToken();

		if(ParseArrayDeclarator(declaration) == false) {
			return false;
		}
	}
	else if(IsOpenParen()) { // (
		// This can be either a grouping paren (E) or a function declaration.
		EatToken();

		// It's a grouping paren if we haven't reached the identifier yet,
		// the next token is not ) (like in 'int()', which is a function)
		// and it's not a token that starts a type declaration (again, it's a function).
		if(identAllowed && (IsCloseParen() == false) &&
		   (IsTypeStartToken() == false)) {
			grouping = true;
		}
		
		if(grouping) {
			// The declarator in the paren must be placed after the ones that follow
			// the paren. We parse the paren content in a separate declarator, and at
			// the end place it in the tail of the declarator list.
			// Note that pointers are allowed to appear in the paren declarator.
			parenDecl = new DI(nullptr);

			if(ParseDeclarator(parenDecl, identAllowed, true) == false) {
				return false;
			}
			
			// An ) should be found at the end.
			if(IsCloseParen() == false) {
				return false;
			}
			else EatToken(); // Skip over )
		}
		else {
			// This is a function. Parse the parameter list.
			declaration->Type = DI::Function;

			if(ParseFunctionDeclarator(declaration) == false) {
				return false;
			}
		}
	}
	else {
		// Call the extensions handlers now.
		// If it was an extension we continue parsing the declarator.
		ExtensionContext context(Context_Declarator, nullptr /* specifiers */, declaration);

		if(HandleUnknown(context)) {
			declaration = context.Declarator;
		}
		else if(IsTypeStartToken()) {
			// No other tokens are allowed, so this is an error.
			// Make an exception when the token is a type-start keyword, like in
			// 'int a int b;' - the user probably forgot ;, so emit an error and
			// declare 'a' so that we recover well.
			return true;
		}
		else return false;
	}

	// If the current token is not , = ) ; { or EOF the declarator hasn't ended.
	// If = is found there is an initializer that must be parsed by 'ParseDeclaratorList'.
	// { means that this is a function definition.
	if(IsDeclaratorEnd() == false) {
        // If the only parsed declarator is a function declarator
        // we really should have had { or ; otherwise we should report
        // an error and give up.
        if((declaration->Type == DI::Function) && declaration->Previous && 
           (declaration->Previous->Previous == nullptr) &&
           (declaration->Previous->Type == DI::Normal)) {
            diag_->Report(Error::FUNCTION_WRONG_START_TOKEN)<<declaration->Location;
            return false;
        }

		// If the current declarator was a grouping paren the slot wasn't used,
		// so parse what follows in it. Else create a new child declarator.
        shared<DI> childDecl = grouping ? declaration : new DI(declaration);

		if(ParseDeclarator(childDecl, identAllowed, ptrAllowed) == false) {
			return false; // Give up.
		}
		
        // Link the current declarator with the child.
        if(grouping == false) {
            declaration->Next = childDecl;
        }

		// Arrays have higher precedence than functions, change positions.
		if(declaration->IsArray() && childDecl->IsFunction()) {
			if(declaration->Previous) {
				declaration->Previous->Next = childDecl;
			}

			childDecl->Next = declaration;
			declaration->Previous = childDecl;
			declaration->Next = nullptr;
		}
	}

	// The end of the declarator has been found. If this was a grouping paren
	// insert the paren content at the end of the list.
	if(grouping) {
		// Consider the case 'int (*a)[1][2][3]': 'declaration' points to '[1]', while
		// '(*a)' needs to be moved after '[3]'; skip to the last declarator.
        // Don't change the order if in the parens we have a normal (name) declarator.
        // 'int (abc)(char, int)'
        if(parenDecl->Type != DI::Normal) {
		    DeclaratorInfo* last = nullptr;
		    DeclaratorInfo* current = declaration;

		    while(current) {
			    last = current;
			    current = current->Next;
		    }

		    parenDecl->Previous = last;
		    last->Next = parenDecl;
        }
        else {
            parenDecl->Next = declaration;
            declaration->Previous = parenDecl;
            declaration = parenDecl;
        }
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseArrayDeclarator(shared<DI> declaration) {
	LocationInfo startLocation = current_.Location();
	DI::ArrayInfo& info = declaration->ArrayDim;
    declaration->Location = startLocation;
    info.FunctProto = contextStack_.Peek()->IsFunctProtoScope();

	// Check for []
	if(IsCloseSquare()) {
		info.Incomplete = true;
		EatToken();
		return true;
	}
	
	// Check for 'static'.
	if(Kwd(current_) == Keyword_Static) {
		info.HasStatic = true;
		EatToken();
	}
	
	// Parse the qualifiers. If qualifiers where not found
	// and 'static' follows, it's not valid.
	if(ParseQualifiers(info.Info) == false) {
		// There are qualifiers and something is wrong.
		diag_->Report(Error::INVALID_ARRAY_QUALIFIER)<<startLocation;
		return false;
	}

	if(Kwd(current_) == Keyword_Static) {
		if(info.Info.Qual.HasNone()) {
			// No qualifiers before 'static'.
			// Report and skip to the end of the array.
			diag_->Report(Error::ARRAY_STATIC_NO_QUALIFIER)<<startLocation;
			SkipToToken(TokenKind::CloseSquare);
			return false;
		}

		info.HasStatic = true;
	}

	// Check for variable array with *.
	// The star can be part of the value expression, so in order to mark
	// a variable length array, the * must be immediately followed by ].
	if(current_.Kind() == TokenKind::Mul) {
		if(PeekNext().Kind() == TokenKind::CloseSquare) {
			// This is [*].
			info.HasStar = true;
			EatToken(); // *
			EatToken(); // ]
			return true;
		}
	}

	// Parse the index expression.
	info.Value = ParseAssignmentExpression();

	// ] should now be found.
	if(IsCloseSquare() == false) {
		diag_->Report(Error::EXPECTED_CLOSE_SQUARE)<<startLocation;
	}
	else EatToken();

	// Semantic analysis is done later for the whole array.		
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseFunctionDeclarator(shared<DI> declaration) {
	// Note that old-style identifier lists are not supported!
	LocationInfo startLocation = current_.Location();
    declaration->Location = startLocation;

	// Parse the parameter list until ), ; or EOF is found.
	// If the function has no parameters we're done.
	if(IsCloseParen()) {
		EatToken();
		return true;
	}

	int parameters = 0;
	bool invalid = false;
	bool skipToSemi;

	// Enter a new context (function-prototype kind).
	shared<DeclarationContext> functCtx = 
            new DeclarationContext(ScopeType::FunctPrototype, 
                                   contextStack_.Peek(), declaration);
	PushContext(functCtx);

	while(true) {
		// Check for variable parameter function.
		if(current_.Kind() == TokenKind::Ellipsis) {
			// This should not be the first parameter.
			// Other parameters should not be after ..., but this check is
			// made by the semantic analysis step.
			if(parameters == 0) {
				diag_->Report(Error::MISSING_COMMA_ELLIPSIS)<<current_.Location();
			}

			declaration->IsVarargs = true;
			EatToken(); // Skip over ...
		}
		else {
			// Create the new parameter.
			shared<DI::ParameterInfo> parameter = new DI::ParameterInfo();
			parameter->Location = current_.Location();
			declaration->Parameters.Add(parameter);
			parameters++;

			// Parse the declaration specifiers, then the (optional) declarator.
			// Declarators are optional in function prototypes.
			if(ParseSpecifiers(parameter->Info, skipToSemi) == false) {
				// Failed to parse the specifiers; skip to the next ,
				diag_->Report(Error::MISSING_PARAMETER)<<parameter->Location;
				invalid = true;
				SkipToComma(false);
			}
			else {
				// If we're not at , or ) a declarator should follow.
				if(((current_.Kind() == TokenKind::Comma) || 
					(current_.Kind() == TokenKind::CloseParen)) == false) {
					shared<DI> paramDecl = new DI(nullptr /* no parent */);
					paramDecl->Context = functCtx; // Set the context.

					if(ParseDeclarator(paramDecl) == false) {
						// There is a declarator and it's not valid.
						invalid = true;
						SkipToComma(false);
					}
					else parameter->Declarator = paramDecl;
				}	
			}

			// Let semantic analysis do some checks and add it to the context.
			// Don't set 'invalid' if it's the case so that we parse the next declarators.
			if(declSema_->HandleFunctionParam(parameter, functCtx)) {
				if(parameter->HasName()) {
					// The name is used to prevent something like 'int f(a, b, a)'.
					functCtx->Add(parameter->GetName(), nullptr /* not needed */);
				}
			}
		}

		// If we are at a comma more parameters are expected.
		// Else the list should be properly ended by a )
		if(IsComma()) {
			EatToken();
		}
		else if(IsCloseParen()) {
			// The parameter list is over. Do semantic analysis.
			EatToken();
			break;
		}
		else {
			// Other tokens are not allowed, so this is an error.
			diag_->Report(Error::INVALID_FUNCTION_DECLARATION)<<startLocation;
			invalid = true;
			break;
		}
	}

	PopContext(); // The prototype context is no longer needed.
	return invalid == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseInitializer(shared<InitInfo> initializer, bool allowDesign) {
	// initializer:
	//		assignment-expression
	//		{ initializer-list }
	//		{ initializer-list , }
	//
	// initializer-list:
	//		designation-opt initializer
	//		initializer-list , designation-opt initializer
	//
	// designation:
	//		designator-list =
	//
	// designator-list:
	//		designator
	//		designator-list designator
	//
	// designator:
	//		[ constant-expression ]
	//		. identifier
	//
	// If the first token is { we have an initializer list.
	// Else it's an expression that should be an ICE.
	// Even if 'allowDesign' is set, designators are ignored only if
	// not surrounded by braces.
	// Ex: 'struct A { int a, b[2] } a = {.b = {[1] = 3}};' is valid.
	//     'int a[] = {[0] = [1]};' is not.
	bool invalid = false;

	if(IsOpenCurly()) {
		// Parse the list of initializers between { }.
		// If another { is found we recourse and expect the } afterwards.
		initializer->Type = InitInfo::InitList;
		EatToken();
		
		while(IsCloseCurly() == false) {
			shared<InitInfo> child = new InitInfo();

			// If a designator follows it is added directly
			// to the current initializer. Else it's a standard initializer.
			if(IsOpenSquare() || (current_.Kind() == TokenKind::Dot)) {
				shared<Designator> designator = new Designator();

				if(ParseDesignator(designator)) {
					initializer->Children.Add(designator);
				}
				else {
					invalid = true;
					SkipToComma();
				}
			}
			else if(ParseInitializer(child, true) == false) {
				// Something bad happened, skip to the next constant.
				invalid = true;
				SkipToCloseCurly(false);
			}
			else {
				initializer->Children.Add(child);
			}

			// Either , or } should be found now.
			if(IsCloseCurly()) {
				break; // The list is finished (the token is eaten below).
			}
			else if(IsComma() == false) {
				// The token is invalid, report.
				diag_->Report(Error::INVALID_TOKEN_IN_INITIALIZER_LIST)<<
							  current_.Location();
				return false;
			} else EatToken(); // Skip over ,
		}

		// } should be found now.
		if(IsCloseCurly() == false) {
			diag_->Report(Error::EXPECTED_CURLY_DESIGN)<<current_.Location();
			invalid = true;
			SkipToComma(false); // The parent should continue.
		} else EatToken();      // Skip over }
	}
	else if(allowDesign && (IsOpenSquare() || (current_.Kind() == TokenKind::Dot))) {
		// A designator is starting (possible multiple linked designators, 
		// like in '[0][1] = 3' or '[0].a[2].b = 2').
		shared<Designator> designator = new Designator();

		if(ParseDesignator(designator)) {
			initializer->Children.Add(designator);
		}
		else {
			invalid = true;
			SkipToComma();
		}
	}
	else {
		// An assignment expression should be found.
		initializer->Type = InitInfo::ConstExpression;
		initializer->Value = ParseAssignmentExpression();
		if(Expression::IsInvalid(initializer->Value)) {
			// Failed to parse the expression, continue.
			return true;
		}
	}

	return invalid == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseDesignator(shared<Designator> designator) {
	// A [E] designator has been found. Recursive designators like
	// '[0][1] = 3' are handled by calling this method again.
	while(true) {
		designator->IsField = IsOpenSquare() == false;
		EatToken();

		if(designator->IsField) {
			// The identifier should follow.
			if(current_.IsIdentifier() == false) {
				return false;
			}
			
			designator->Name = Identifier::FromToken(current_);
			EatToken();
		}
		else {
			// Parse the expression, but check first for [].
			if(IsCloseSquare()) {
				diag_->Report(Error::ARRAY_DESIGNATOR_MISSING_INDEX)<<current_.Location();
				SkipToComma(false);
				return false;
			}

			// Now parse the index expression.
			designator->ConstExpr = ParseConstantExpression();

			// ] should be found now.
			if(IsCloseSquare() == false) {
				diag_->Report(Error::ARRAY_DESIGNATOR_EXPECTED_CLOSE_SQARE)<<
							  current_.Location();
				SkipToComma(false);
				return false;
			} else EatToken();
		}

		// Check if other designators come, like '[0][1]' or '[0].a[3].b'.
		if(IsOpenSquare() || (current_.Kind() == TokenKind::Dot)) {
			// The new designator should be a child of the current one.
			// Create the child and recourse to parse the rest.
			shared<Designator> child = new Designator();
			designator->Child = child;
			designator = child;
		}
		else if(current_.Kind() == TokenKind::Eq) {
			// The value of the designator should follow.
			// It can have {} in it, so use 'ParseInitializer' to parse it,
			// but with designator recognition disabled (forbids something
			// like '[0].a = [2].b').
			shared<InitInfo> value = new InitInfo;
			EatToken(); // Skip over =

			if(ParseInitializer(value, false) == false) {
				return false;
			}
			else {
				designator->Value = value;
				return true;
			}
		}
		else {
			// A designator or the value should have followed.
			diag_->Report(Error::EXPECTED_DESIGNATOR_OR_VALUE)<<current_.Location();
			return false; // The token is invalid.
		}
	}		

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::IsTypedefName() {
	// If there is a 'typedef' in the current scope the statement should be
	// considered a declaration (else it would be considered an expression and
	// an error will most likely occur).  An exception is when a variable 
	// has been declared with the same name as the typedef, like in 
	// 'typedef char A; int A; A b;'. 'A b' is invalid because 'A' is a declaration
	// of type 'int', not the name of the typedef.
	if(current_.IsIdentifier() == false) {
		return false;
	}

	Identifier name(current_);
	Declaration* declaration = contextStack_.Peek()->Find(&name);
	return declaration && 
           declaration->IsTypedefDecl();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::IsTypeStartToken() {
	// Tokens that can appear in the beginning of a declaration.
	switch(Kwd(current_)) {
		case Keyword_Auto     :	return true;
		case Keyword_Extern	  :	return true;
		case Keyword_Static	  :	return true;
		case Keyword_Register :	return true;
		case Keyword_Typedef  :	return true;
		case Keyword_Void	  :	return true;
		case Keyword_Char	  :	return true;
		case Keyword_WChar	  :	return true;
		case Keyword_Short	  :	return true;
		case Keyword_Int	  :	return true;
		case Keyword_Long	  :	return true;
		case Keyword_Float	  :	return true;
		case Keyword_Double	  :	return true;
		case Keyword_Bool	  :	return true;
		case Keyword_Enum	  :	return true;
		case Keyword_Struct	  :	return true;
		case Keyword_Union	  :	return true;
		case Keyword_Const	  :	return true;
		case Keyword_Restrict :	return true;
		case Keyword_Volatile :	return true;
		case Keyword_Inline	  : return true;
		case Keyword_Signed   : return true;
		case Keyword_Unsigned : return true;
	}

	// If it's not one of the standard tokens, it could be a 'typedef' or an extension.
	return IsTypedefName() || QueryType(current_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseSpecifiers(SpecifierInfo& info, bool& skipToSemi) {
	// declaration-specifiers:
	//		storage-class-specifier declaration-specifiers-opt
	//		type-specifier declaration-specifiers-opt
	//		type-qualifier declaration-specifiers-opt
	//		function-specifier declaration-specifiers-opt
	//
	// storage-class-specifier:
	//		typedef
	//		extern
	//		static
	//		auto
	//		register
	//
	// type-specifier:
	//		void
	//		char
	//		short
	//		int
	//		long
	//		float
	//		double
	//		signed
	//		unsigned
	//		_Bool
	//		struct-or-union-specifier
	//		enum-specifier
	//		typedef-name
	//
	// type-qualifier:
	//		const
	//		restrict
	//		volatile
	// 
	// function-specifier: inline
	bool invalid = false;
	bool done    = false;
	skipToSemi   = false; // Presume the declaration will be valid.
	DiagnosticCode errorCode = Error::DECLARATION_NO_SPECIFIER;
	LocationInfo startLocation = current_.Location();

	// Parse until a token that is not a keyword in the set above is found.
	// Invalid combinations (like 'extern static' or 'char char') are handled later.
	// This tries to recover well from some simple errors that appear frequently.
	while(done == false) {
		switch(Kwd(current_)) {
			// Storage-class specifiers.
			case Keyword_Auto: {
				invalid = info.HasStorage(); // Only one storage specifier is allowed.
				errorCode = Error::DUPLICATE_DECLARATION_SPECIFIERS;
				info.Auto = true;
				break;
			}
			case Keyword_Extern: {
				invalid = info.HasStorage(); // Only one storage specifier is allowed. 
				errorCode = Error::DUPLICATE_DECLARATION_SPECIFIERS;
				info.Extern = true;
				break;
			}
			case Keyword_Static: {
				invalid = info.HasStorage(); // Only one storage specifier is allowed. 
				errorCode = Error::DUPLICATE_DECLARATION_SPECIFIERS;
				info.Static = true;
				break;
			}
			case Keyword_Register: {
				invalid = info.HasStorage(); // Only one storage specifier is allowed. 
				errorCode = Error::DUPLICATE_DECLARATION_SPECIFIERS;
				info.Register = true;
				break;
			}
			case Keyword_Typedef: {
				invalid = info.HasStorage(); // Only one storage specifier is allowed.
				errorCode = Error::DUPLICATE_DECLARATION_SPECIFIERS;
				info.Typedef = true;
				break;
			}

			// Type specifiers.
			case Keyword_Void: {
				invalid = info.VoidAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Void = true;
				break;
			}
			case Keyword_Char: {
				invalid = info.CharAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Char = true;
				break;
			}
			case Keyword_WChar: {
				invalid = info.WCharAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.WChar = true;
				break;
			}
			case Keyword_Short: {
				invalid = info.ShortAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Short = true;
				break;
			}
			case Keyword_Int: {
				invalid = info.IntAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Int = true;
				break;
			}
			case Keyword_Long: {
				// The number of 'long' keywords is tested later.
				invalid = info.LongAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.LongCount++;
				break;
			}
			case Keyword_Float: {
				invalid = info.FloatAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Float = true;
				break;
			}
			case Keyword_Double: {
				invalid = info.DoubleAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Double = true;
				break;
			}
			case Keyword_Bool: {
				invalid = info.BoolAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Bool = true;
				break;
			}
			case Keyword_Enum: {
				invalid = info.EnumAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				invalid = ParseEnumDeclaration(info) == false;
				continue; // Don't eat the token yet (because of ;).
			}
			case Keyword_Struct: 
			case Keyword_Union: {
				// 'struct' and 'union' are treated together.
				invalid = info.StructAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				invalid = ParseStructDeclaration(info) == false;
				continue; // Don't eat the token yet (because of ;).
			}
			case Keyword_Signed: {
				invalid = info.SignedAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Signed = true;
				break;
			}
			case Keyword_Unsigned: {
				invalid = info.UnsignedAllowed() == false;
				errorCode = Error::INVALID_SPECIFIER_COMBINATION;
				info.Unsigned = true;
				break;
			}

			// Type qualifiers. All can be present, even duplicates (C99:6.7.3.4).
			case Keyword_Const: {
				info.Qual.SetHasConst(true);
				break;
			}
			case Keyword_Restrict:
			case Keyword_Restrict2: {
				// 'restrict' can't appear before the type is known
				// ('restrict int* a' or 'const restrict int* a' are invalid).
				invalid = info.NoType();
				errorCode = Error::RESTRICT_BEFORE_TYPE;
				info.Qual.SetHasRestrict(true);
				break;
			}
			case Keyword_Volatile: {
				info.Qual.SetHasVolatile(true);
				break;
			}

			// Function specifiers.
			case Keyword_Inline:
			case Keyword_Inline2: {
				info.Inline = true;
				break;
			}

			// Any other keyword (or no keyword at all) ends the specifier sequence.
			default: {
				// Call the extensions handlers now.
				ExtensionContext context(Context_Specifiers, &info, 
                                         nullptr /* declarator */);

				if(HandleUnknown(context)) {
					// If it was an extension don't stop parsing yet.
					invalid = context.Invalid;
					done = false;
					continue;
				}
				
				done = true;
				break;
			}
		}

		// If 'done' is set it doesn't necessarily mean so.
		// If we stopped at an identifier we must check if it's a type name.
		// It can't be a type name if we already seen a type.
		if(current_.IsIdentifier() && info.NoType()) {
			Identifier ident(current_);
			Declaration* declaration = contextStack_.Peek()->Find(&ident);

			if(auto tdef = declaration->As<TypedefDeclaration>()) {
				// Use the inner type of the typedef.
				invalid |= info.TypedefAllowed() == false;
				info.TypedefT = tdef->DeclarationType();
				done = false; // Continue parsing, qualifiers may follow.
			}
		}

		if(((done == false) && ((invalid == false)))) {
			// Don't eat the token if we're done, or this token made the declaration
			// invalid. Doing this handled cases like 'int a int main() {...}';
			// eating the second 'int' would make the function invalid too.
			EatToken();
		}

		// Stop parsing the specifiers if we are in an invalid situation.
		if(invalid) break;
	}

	// At least a specifier should have been declared (C99:6.7.2.2).
	// If the error involves the storage-class specifier we don't emit this error.
	if(((info.IsBasicType() == false) && (info.IsUserType() == false)) &&
		(errorCode != Error::DUPLICATE_DECLARATION_SPECIFIERS)){
		errorCode = Error::DECLARATION_MISSING_TYPE;
		invalid = true;
	}

	if(invalid) {
		// The specifier has an invalid combinations of terms (for ex. 'char char').
		// If the errors involves a type name (like in 'typedef int A; unsigned A t')
		// emit a more specific error message.
		skipToSemi = (current_.IsKeyword() == false) || 
                     (info.IsUserType() == false);

		if(info.TypedefT) {
			diag_->Report(Error::DECLARATION_SPECIFIER_FOR_TYPENAME)<<
						  startLocation<<current_.ToString();
		}
		else if(errorCode == Error::DECLARATION_NO_SPECIFIER) {
			diag_->Report(errorCode)<<startLocation;
		}
        else if((IsTypeStartToken() && info.IsUserType()) == false) {
            diag_->Report(errorCode)<<current_.Location()<<current_.ToString();
        }

		if(errorCode == Error::DUPLICATE_DECLARATION_SPECIFIERS) {
			// If we have something like 'int char' we skip over 'char', else we would
			// get a 'semicolon not found' error message.
			EatToken();
		}
	}

	// Set the range of the specifiers.
	info.Start = startLocation;
	info.End = current_.Location();
	return invalid == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseQualifiers(SpecifierInfo& info) {
	bool invalid = false;
	bool done = false;
	LocationInfo startLocation = current_.Location();

	while(done == false) {
		switch(Kwd(current_)) {
			case Keyword_Const: {
				invalid = info.Qual.HasConst();    // 'const const' invalid.
				info.Qual.SetHasConst(true);
				break;
			}
			case Keyword_Restrict: {
				invalid = info.Qual.HasRestrict(); // 'restrict restrict' invalid.
				info.Qual.SetHasRestrict(true);
				break;
			}
			case Keyword_Volatile: {
				invalid = info.Qual.HasVolatile(); // 'volatile volatile' invalid.
				info.Qual.SetHasVolatile(true);
				break;
			}
			default: {
				// Call the extensions handlers now.
				ExtensionContext context(Context_Qualifiers, &info, 
                                         nullptr /* declarator */);

				if(HandleUnknown(context)) {
					// If it was an extension don't stop parsing yet.
					done = false;
					break;
				}
				
				done = true;
				break;
			}
		}

		if(done == false) {
			EatToken();
		}
	}

	if(invalid) {
		diag_->Report(Error::DUPLICATE_DECLARATION_SPECIFIERS)<<
					  RangeInfo(startLocation, current_.Location());
	}

	// Set the range of the specifiers.
	info.Start = startLocation;
	info.End = current_.Location();
	return invalid == false;
}



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseTypeQualifiers(SpecifierInfo& info) {
	// Only type and qualifier specifiers are allowed in the list.
	// Use 'ParserSpecifiers' and report if storage-class specifiers are found.
	bool skipToSemi;

	if(ParseSpecifiers(info, skipToSemi) == false) {
		return false; // Failed, don't check anymore.
	}

	if(info.HasStorage()) { // auto, extern, static, register, typedef
		diag_->Report(Error::STRUCT_STORAGE_SPECIFIER_ON_FIELD)<<info.Start;
		return false;
	}

	// Check for 'inline'.
	if(info.Inline) {
		diag_->Report(Error::STRUCT_INLINE_ON_FIELD)<<info.Start;
		return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseTypenameSpecifiers(SpecifierInfo& info) {
	// Same as above, but with corresponding error codes.
	bool skipToSemi;

	if(ParseSpecifiers(info, skipToSemi) == false) {
		return false; // Failed, don't check anymore.
	}

	if(info.HasStorage()) { // auto, extern, static, register, typedef
		diag_->Report(Error::TYPENAME_STORAGE)<<info.Start;
		return false;
	}

	// Check for 'inline'.
	if(info.Inline) {
		diag_->Report(Error::TYPENAME_INLINE)<<info.Start;
		return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* Parser::ParseTypename() {
	// Parse the specifiers.
	SpecifierInfo info;

	if(ParseTypenameSpecifiers(info) == false) {
		// Failed to parse the specifiers, give up.
		return nullptr;
	}

	// Parse the declarator. No name should be provided.
	// Check if ) follows. If yes we have something like '(int)' and don't need
	// to parse the declarator anymore.
	shared<DI> declInfo;

	if(IsCloseParen() == false) {
		declInfo = new DI(nullptr);
		if(ParseDeclarator(declInfo) == false) {
			return nullptr;
		}

		if(declInfo->GetName()) {
			diag_->Report(Error::TYPENAME_HAS_NAME)<<declInfo->Location;
			return nullptr;
		}
	}

	// Perform semantic analysis on what we parsed and create the type.
	return declSema_->HandleTypename(declInfo, info, contextStack_.Peek());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseFunctionDefinition(shared<DI> declInfo, SpecifierInfo& info,
									 LocationInfo startLocation) {
	// Verify that this is a valid function definition start, then parse the body.
	bool isRedeclaration;
	shared<Declaration> declaration = declSema_->HandleDeclaratorBegin(declInfo, info, 
                                                                       contextStack_.Peek(), 
											                           isRedeclaration);
	if(declaration == nullptr) {
		// The declaration/type is not valid.
		diag_->Report(Error::FUNCTION_DEFINITION_NOT_FUNCTION)<<startLocation;

		// Notify the extension handlers that the declaration failed.
		NotifyDeclarationFailed();
		return false;
	}

	// Notify the extension handlers that a new declaration was made.
	NotifyDeclarationSucceeded(declaration);

	// Create a new context for the function. The parameters will be placed in it.
	shared<DeclarationContext> functCtx = 
            new DeclarationContext(ScopeType::Function, contextStack_.Peek());

	contextStack_.Push(functCtx);

	if(declSema_->HandleFunctionDefinitionBegin(declaration, info, functCtx) == false) {
		// If the definition start is invalid skip to the end
		// of the block and give up.
		SkipToBlockEnd();
		return false;
	}

	// Notify the extension handlers that a new function definitions begins.
	// Useful for extensions that apply some attribute on the first defined function.
	NotifyFunctionBegin(declaration);
	
	// Now parse the compound statement.
	shared<Statement> bodyStatement = ParseCompoundBody();

	if(bodyStatement == nullptr) {
		// Recovery has already be done, just return.
		return false;
	}

	// Check the function again and add it to the context if all is OK.
	if(declSema_->HandleFunctionDefinitionEnd(declaration, bodyStatement, functCtx)) {
		if(isRedeclaration == false) {
			// Add it to the context only if it's not already declared.
			contextStack_.Peek()->Add(declaration->Name(), declaration);
		}

		// Add it to the list of defined function in the translation unit.
		unit_.Functions().Add(declaration.As<FunctionDeclaration>());
		unit_.UnitDeclarations().Add(declaration);
		contextStack_.Pop();

		// Notify the extension handlers that the function definition has ended.
		NotifyFunctionEnd(declaration);
		return true;
	}
	else {
        // Error.
		contextStack_.Pop();
		return false;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseFileDeclaration() {
	// Parse a declaration (or a declaration list).
	// If we found a function definition we're done, else check for ;
	bool functionDef;

	shared<DeclarationList> list = ParseDeclaration(functionDef);

	if(functionDef == false) {
		if(IsSemiColon() == false) {
			// Don't report this error when we've stopped at a close curly }
			// This happens in situations like these: 'INVALID_TYPE f() {...}'
            // An exception is when a struct/union/enum was declared.
            if(list && (list->Count() == 1)) {
                auto declaration = list->Declarations()[0];

                if(declaration->IsStructDecl() || 
                   declaration->IsUnionDecl()  ||
                   declaration->IsEnumDecl()) {
                    diag_->Report(Error::EXPECTED_DECLARATION_SEMICOLON)<<
                                  current_.Location();
                }
            }
			else diag_->Report(Error::EXPECTED_DECLARATION_SEMICOLON)<<
                               current_.Location();

			// If it's not a keyword that can start a declaration skip it.
			if(IsTypeStartToken() == false) {
				EatToken();
			}
			
			return false;
		}

		EatToken(); // Skip over ;
	}

	return true;
}

} // namespace Parsing