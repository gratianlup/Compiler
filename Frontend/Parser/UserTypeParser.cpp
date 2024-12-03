// DeclarationParser.hpp
// Copyright (c) Lup Gratian
//
// Implements the user type (enum/struct/union) declaration parsing methods.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Parser.hpp"

namespace Parsing {

bool Parser::ParseEnumDeclaration(SpecifierInfo& info) {
	// enum-specifier:
	//		enum identifier-opt { enumerator-list }
	//		enum identifier-opt { enumerator-list , }
	//		enum identifier
	LocationInfo startLocation = current_.Location();
	EatToken(); // Parsing stopped at 'enum', eat it.

	// If the next token is an identifier we have a declaration.
	// If no identifier is present the declaration is anonymous.
	// A { marks the start of the enumerator list.
	TagDeclType declType = Tag_Reference;
	shared<Identifier> name;
	bool hasName = false;

	// The current token should either be an identifier or a {.
	// enum; enum 123 and the like are not valid.
	if((current_.IsIdentifier() == false) && 
       (IsOpenCurly() == false)) {
		diag_->Report(Error::EXPECTED_OPEN_CURLY)<<current_.Location();
		return false;
	}

	if(current_.IsIdentifier()) {
		hasName = true;
		name = Identifier::FromToken(current_);
		EatToken();
	}

	// Check the token to see if this is a declaration, definition or reference.
	// Declaration: enum name;
	// Definition:  enum name {...}
	// Reference:   enum name var_name;
	if(IsOpenCurly()) {
		declType = Tag_Definition;
	}
	else if(IsSemiColon()) {
		declType = Tag_Declaration;
	}

	// Let semantic analysis make sure this 'enum' is valid until here.
	// If it is, a 'EnumDeclaration' object is returned.
	bool isReference;
	info.Enum = declSema_->HandleTagDecl(contextStack_.Peek(), Tag_Enum, declType, 
										 hasName, name, startLocation, isReference);
	if(info.Enum == nullptr) {
		// The 'enum' is not valid. Skip the entire declaration.
		SkipToSemiColon();
		return false;
	}
	else if(isReference == false) {
		// This is the first time we see a declaration.
		if(hasName) {
			contextStack_.Peek()->ParentBlock()->AddTag(name, info.Enum);
		}

		unit_.Tags().Add(info.Enum);
	}

	// See if the body of the 'enum' follows.
	if(IsOpenCurly()) {
		return ParseEnumList(info);
	}
	else return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseEnumList(SpecifierInfo& info) {
	// enumerator-list:
	//		enumerator
	//		enumerator-list , enumerator
	shared<EnumDeclaration> declaration = info.Enum;

	int ct = 0; // The number of enum constants found.
	LocationInfo startLocation = current_.Location();
	EatToken(); // Parsing stopped at {, eat the token.
	
	// Enter a new context.
	shared<DeclarationContext> enumCtx = 
            new DeclarationContext(ScopeType::Block, contextStack_.Peek());

	PushContext(enumCtx);

	// Parse until } is found.
	while(IsCloseCurly() == false) {
		if(ParseEnumConstant(declaration) == false) {
			// The constant is not valid. Skip to the next constant.
			while((IsComma() == false) && (current_.IsEOF() == false)) {
				if(IsCloseCurly() || IsSemiColon()) {
					// The declaration has ended.
					// Stop, but still do some semantic analysis on the list.
					bool result = declSema_->HandleEnumList(enumCtx, declaration, 
															startLocation);
					PopContext(); // Pop the 'enum' context.
					return result;
				}

				EatToken();
			}
		}

		// Increment the number of parsed constants.
		ct++;

		// If a comma follows more constants are expected.
		// If not the list should properly end.
		if(IsComma() == false) {
			break;
		}
		else EatToken(); // Skip the comma.
	}

	// The current token should be }.
	if(IsCloseCurly() == false) {
		// } not found, report error.
		diag_->Report(Error::EXPECTED_CLOSE_CURLY)<<startLocation;
		PopContext(); // Pop the 'enum' context.
		return false;
	}
	else EatToken(); // Skip over }

	// Empty constant lists are not allowed.
	if(ct == 0) {
		diag_->Report(Error::EMPTY_ENUM_DEFINITION)<<startLocation;
		PopContext(); // Pop the 'enum' context.
		return false;
	}

	// Perform semantic analysis on the list. 
	// Also determines the integer type that stores the 'enum'.
	bool result = declSema_->HandleEnumList(contextStack_.Peek(), 
                                            declaration, startLocation);
	PopContext(); // Pop the 'enum' context.
    return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseEnumConstant(shared<EnumDeclaration> enumDecl) {
	// enumerator:
	//		enumeration-constant
	//		enumeration-constant = constant-expression
	// If a =  if found a constant is present.
	// The first token should be an identifier.
	if(current_.IsIdentifier() == false) {
		diag_->Report(Error::NO_ENUM_CONSTANT_IDENTIFIER)<<current_.Location();
		return false; // The caller should recover.
	}

	shared<Identifier> name = Identifier::FromToken(current_);
	LocationInfo nameLoc = current_.Location();

	// See if there is a value for the constant.
	bool hasValue = false;
	shared<Expression> value;

	EatToken();
	if(current_.Kind() == TokenKind::Eq) {
		hasValue = true;
		EatToken(); // Skip over =

		value = ParseConstantExpression();

		if(value == nullptr) {
			return false;
		}
	}

	// No other identifier with the same name should be in the parent context.
	// The 'parent' is the outermost context that is either a function or the file.
	// Perform semantic analysis and add the constant to the type and context.
	LocationInfo endLoc = current_.Location();
	shared<EnumConstDeclaration> constDecl = 
			declSema_->HandleEnumConst(contextStack_.Peek(), enumDecl, name,
									   hasValue, value, nameLoc, endLoc);

	if(constDecl) {
		DeclarationContext* parent = contextStack_.Peek()->ParentFunction();
		parent->Add(name, constDecl);
		enumDecl->DeclarationType()->Constants().Add(constDecl);
		return true;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseStructDeclaration(SpecifierInfo& info) {
	// struct-or-union-specifier:
	//		struct-or-union identifier-opt { struct-declaration-list }
	//		struct-or-union identifier
	LocationInfo startLocation = current_.Location();

	// Determine if this is 'struct' or 'union'.
	TagType aggregateType = Kwd(current_) == Keyword_Struct ? 
                            Tag_Struct : Tag_Union;
	EatToken();

	// If the next token is an identifier we have a declaration.
	// If no identifier is present the declaration is anonymous.
	// A { marks the start of the field list (the body).
	TagDeclType declType = Tag_Reference;
	bool hasName = false;
	shared<Identifier> name;

	// The current token should either be an identifier or a {.
	// struct; struct 123 and the like are not valid.
	if((current_.IsIdentifier() == false) && 
       (IsOpenCurly() == false)) {
		diag_->Report(Error::EXPECTED_OPEN_CURLY_OR_IDENTIFIER)<<current_.Location();
		return false;
	}

	if(current_.IsIdentifier()) {
		hasName = true;
		name = Identifier::FromToken(current_);
		EatToken();
	}

	// Check the token to see if this is a declaration, definition or reference.
	// Declaration: struct name;
	// Definition:  struct name {...}
	// Reference:   struct name var_name;
	if(IsOpenCurly()) {
		declType = Tag_Definition;
	}
	else if(IsSemiColon()) {
		declType = Tag_Declaration;
	}

	// Let semantic analysis make sure this 'struct'/'union' is valid until here.
	// Insert the returned declaration into the context so that
	// the aggregate can be referred in the body (pointers to it, actually).
	// We insert it only if it's not a reference.
	bool isReference;
	shared<Declaration> structDecl = 
			declSema_->HandleTagDecl(contextStack_.Peek(), aggregateType,
                                     declType, hasName, name,
									 startLocation, isReference);

	if(aggregateType == Tag_Struct) info.Struct = structDecl;
	info.Union = structDecl;

	if(structDecl == nullptr) {
		// The struct/union is not valid. Skip the entire declaration.
		SkipToSemiColon();
		return false;
	}
	else if(isReference == false) {
		// This is the first time we see a declaration for this type.
		if(hasName) {
			contextStack_.Peek()->ParentBlock()->AddTag(name, structDecl);
		}

		unit_.Tags().Add(structDecl);
	}

	// See if the body of the aggregate follows.
	if(IsOpenCurly()) {
		if(aggregateType == Tag_Struct) {
            return ParseFieldList(info.Struct);
        }
		else return ParseFieldList(info.Union);
	}
	
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseFieldList(shared<StructUnionDeclaration> declaration) {
	// struct-or-union:
	//		struct
	//		union
	//
	// struct-declaration-list:
	//		struct-declaration
	//		struct-declaration-list struct-declaration
	LocationInfo startLocation = current_.Location();
	StructUnionType* type = declaration->DeclarationType();
	
	EatToken(); // Parsing stopped at {, skip over it.
	
	// Enter a new context. Associate it with the declaration.
	shared<DeclarationContext> structCtx = 
            new DeclarationContext(ScopeType::Record, contextStack_.Peek());
	type->SetBodyContext(structCtx);
	PushContext(structCtx);

	// Parse until } is found.
	while(IsCloseCurly() == false) {
		if(ParseFieldDeclaration(declaration) == false) {
			// The declarator list is not valid, skip to ;
			SkipToSemiColon(false);

			if(IsCloseCurly()) {
				// The declaration has ended.
				EatToken();
				PopContext(); // Pop the struct/union context.
				return false;
			}
			
			// If } is not found the error will be reported below.
		}

		// If we stopped at a a semicolon the body hasn't ended.
		if(IsSemiColon() == false) {
			break;
		}
		else EatToken(); // Skip over ;
	}

	// This should be }.
	if(IsCloseCurly() == false) {
		// } not found, report error.
		diag_->Report(Error::EXPECTED_CLOSE_CURLY)<<startLocation;
		PopContext(); // Pop the struct/union context.
		return false;
	}
	else EatToken(); // Skip over }

	// Perform semantic analysis on the list. 
	PopContext(); // Pop the struct/union context.
	return declSema_->HandleFieldList(declaration, contextStack_.Peek(), startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseFieldDeclaration(shared<StructUnionDeclaration> declaration) {
	// struct-declaration:
	//		specifier-qualifier-list struct-declarator-list ;
	//
	// specifier-qualifier-list:
	//		 type-specifier specifier-qualifier-listopt
	//		 type-qualifier specifier-qualifier-listopt
	//
	// struct-declarator-list:
	//		struct-declarator
	//		struct-declarator-list , struct-declarator
	LocationInfo startLocation = current_.Location();
	StructUnionType* type = declaration->DeclarationType();
	bool invalid = false;

	// Parse the specifiers, than the list of declarators.
	// If one of the declarators couldn't be parsed skip to the end of the list ;
	SpecifierInfo info;

	if(ParseTypeQualifiers(info) == false) {
		// Failed to parse the specifiers, give up.
		return false;
	}

	// Parse until ; is found. For each found declarator 
    // a new 'FieldDeclaration' is created.
	while(true) {
		LocationInfo startLocation = current_.Location();
		shared<DI> declInfo = new DI(nullptr);
		declInfo->Context = contextStack_.Peek(); // Set the context.
		declInfo->Location = startLocation;

		// We parse the declarator only if we haven't stopped at :
		// (: without a declarator represents an unnamed bitfield).
		if(current_.Kind() != TokenKind::Colon) {
			if(ParseDeclarator(declInfo) == false) {
				// Failed to parse the declarator. Skip to , and continue.
				// Don't emit any errors because they were already emitted.
				invalid = true;
				SkipToComma(false);

				if(IsCloseCurly()) return invalid; // Declaration end found.
			}
		}

		// Check for bitfield. The value should be an ICE.
		shared<Expression> bitfield;

		if(current_.Kind() == TokenKind::Colon) {
			EatToken(); // Skip over :
			
			// Parse the expression (which should be constant, checked later).
			bitfield = ParseConstantExpression();

			if(bitfield == nullptr) {
				// Failed to parse the bitfield, but continue to parse the rest.
				invalid = true;
				SkipToComma(false);

				if(IsCloseCurly()) {
					return invalid; // Declaration end found.
				}
			}
		}

		// Perform semantic analysis and if all is well, a 'FieldDeclaration' 
        // is returned and it will be added to the type and to the context.
		LocationInfo endLoc = current_.Location();
		shared<FieldDeclaration> field = 
				declSema_->HandleFieldDeclaration(declInfo, info, declaration, 
                                                  contextStack_.Peek(),
												  bitfield, startLocation, endLoc);
        if(field) {
			type->Fields().Add(field);

			// The field is added to the context only if it has a name.
			// Unnamed fields are allowed if a bitfield is present.
			if(field->Name()) {
				contextStack_.Peek()->Add(field->Name(), field);
			}
		}
		else invalid = true; // Continue to parse the rest.

		// See if more declarators follow.
		if(IsComma() == false) {
			if(IsSemiColon() == false) { // ; will be eaten by the caller.
				// An invalid token was found.
				// Emit a more specific error if this was the last declaration; in this
				// case we don't report the failure to the caller so that we recover.
				if(IsCloseCurly()) {
					diag_->Report(Error::LAST_STRUCT_DECL_NO_SEMICOLON)<<
								  RangeInfo(startLocation, current_.Location());
					return invalid == false;
				}
				else {
					// The more general case.
					diag_->Report(Error::EXPECTED_DECLARATION_SEMICOLON)<<
								  RangeInfo(startLocation, current_.Location());
					return false;
				}
			}
			else return invalid == false; // Declaration finished.
		}
		
		EatToken(); // Skip over ,
	}

	return invalid == false;
}

} // namespace Parsing