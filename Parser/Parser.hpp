// Parser.hpp
// Copyright (c) Lup Gratian
//
// Defines the Parser.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_PARSER_HPP
#define PC_PARSING_PARSER_HPP

#include "../Base/String.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/Stack.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Lexer/Token.hpp"
#include "../Lexer/Lexer.hpp"
#include "../Common/Context.hpp"
#include "../Common/Errors.hpp"
#include "../Common/Warnings.hpp"
#include "../AST/DeclarationContext.hpp"
#include "../AST/Declarations.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/Unit.hpp"
#include "../AST/Types.hpp"
#include "../AST/Attributes.hpp"
#include "../AST/Identifier.hpp"
#include "../AST/TypeCombiner.hpp"
#include "../AST/TypeManager.hpp"
#include "ParserHelpers.hpp"
#include "DeclarationSemantic.hpp"
#include "ExpressionSemantic.hpp"
#include "StatementSemantic.hpp"
#include "SemanticHolder.hpp"
#include "Extension.hpp"
using namespace Lexing;
using namespace Common;
using namespace AST;

namespace Parsing {

// Represents the supported keywords.
enum KeywordType {
	Keyword_None, // If the token is not actually a keyword.

	#define keyword(NAME, TYPE) TYPE,
	#include "../Common/Keywords.def"
	#undef keyword

	Keyword_END
};


class Parser {
private:
	// Represents the expression type designated by a group of parens.
	enum ParenExprType {
		Expr_Grouping, // (E)               - grouping paren
		Expr_Compound, // (type-name) {...} - compound expression
		Expr_Cast,     // (int) E           - cast
		Expr_Invalid   // The expression is invalid.
	};
	
	typedef DeclaratorInfo DI;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Context* context_;                     // The context of the compilation process.
	SemanticHolder* semaHolder_;           // Contains the modules that perform semantic analysis.
	Diagnostic* diag_;                     // The diagnostics module.
	Lexer* lexer_;                         // The associated lexer.
	TypeManager* types_;                   // Creates and manages built-in and user types.
	TypeCombiner typeComb_;                // Combines the types from multiple declarations.
	shared<DeclarationContext> activeCtx_; // The context that is currently active.
	DeclarationSemantic* declSema_;        // Performs semantic analysis for declarations.
	ExpressionSemantic* exprSema_;         // Performs semantic analysis for expressions.
	StatementSemantic* statementSema_;     // Performs semantic analysis for statements.
	Token current_;                        // The current token.
	Token peekedToken_;                    // The token read in advance.
	Unit unit_;                            // The parsed translation unit.
	Stack<shared<DeclarationContext>> contextStack_; // All declaration contexts use it.
	bool hasPeekedToken_;                  // A token has been read in advance.

	// Support for extensions.
	List<shared<Extension>> declSucceededExts_;
	List<shared<Extension>> declFailedExts_;
	List<shared<Extension>> functBeginExts_;
	List<shared<Extension>> functEndExts_;
	List<shared<Extension>> typeExts_;
	List<shared<Extension>> unknownExts_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Skips a token of the specified type or EOF is found.
	// If 'semiHasPriority' is set it stops at the first valid semicolon instead.
	void SkipToToken(TokenKind kind, bool eat = true, bool semiHasPriority = false);

	// Skips a keyword of the specified type or EOF is found.
	void SkipToKwd(KeywordType type, bool eat = true);

	// Skips until a colon ; or EOF is found.
	void SkipToSemiColon(bool eat = true);

	// Skips until a comma or ; EOF is found.
	void SkipToComma(bool eat = true);

	// Skips until a close curly } or , ; EOF is found.
	void SkipToCloseCurly(bool eat = true);

	// Skips until a close paren ) or ; EOF is found.
	void SkipToCloseParen(bool eat = true);

	// Skips until the end of a block } or EOF is found.
	void SkipToBlockEnd(bool eat = true, bool stopAtSemi = false);

	// Skips until a colon : is or } EOF is found.
	void SkipToColon(bool eat = true);

	// Methods to test for the beginning/end of a section.
	bool IsSemiColon() const { // ;
		return current_.Kind() == TokenKind::SemiColon;
	}

	bool IsOpenCurly() const { // {
		return current_.Kind() == TokenKind::OpenCurly;
	}

	bool IsOpenParen() const { // (
		return current_.Kind() == TokenKind::OpenParen;
	}

	bool IsOpenSquare() const { // [
		return current_.Kind() == TokenKind::OpenSquare;
	}

	bool IsCloseCurly() const { // }
		return current_.Kind() == TokenKind::CloseCurly;
	}

	bool IsCloseParen() const { // )
		return current_.Kind() == TokenKind::CloseParen;
	}

	bool IsCloseSquare() const { // ]
		return current_.Kind() == TokenKind::CloseSquare;
	}

	bool IsComma() const { // ,
		return current_.Kind() == TokenKind::Comma;
	}

	bool IsColon() const { // :
		return current_.Kind() == TokenKind::Colon;
	}

	bool IsTypeStartToken();

	// Returns 'true' if the specified token is a keyword that marks the beginning
	// of a statement (if, while, for, etc.).
	bool IsStatementKeyword(const Token& token);

	// Pushes the specified context on the stack, and makes it the active one.
	void PushContext(shared<DeclarationContext> context);

	// Popes the current context from the stack, and makes the first one active.
	void PopContext();

	// Calls all extension handlers and returns 'true' if at least one of them
	// could handle the situation.
	bool HandleUnknown(ExtensionContext& context);
	void NotifyDeclarationSucceeded(Declaration* declaration);
	void NotifyDeclarationFailed();
	void NotifyFunctionBegin(Declaration* declaration);
	void NotifyFunctionEnd(Declaration* declaration);
	bool QueryType(Token& token);

protected:
	// Parses a declaration (specifiers and the declaration list).
	shared<DeclarationList> ParseDeclaration(bool& functionDef);

	// Parses a list of declarators. If a declarator cannot be parsed it
	// skips to the next one to recover, but will return 'false'.
	// After a declarator is parsed, semantic analysis is invoked to validate it.
	bool ParseDeclaratorList(SpecifierInfo& info, shared<DeclarationList> declList, 
                             bool& functionDef);
	
	// Returns 'true' if the current token marks the end of the declarator.
	bool IsDeclaratorEnd();

	// The main method for declarator parsing. Array and function declarators
	// are handled separately. The method is called recursively until , or ; is found.
	// All information is placed in temporary objects.
	bool ParseDeclarator(shared<DI>& declaration, bool identAllowed = true, 
                         bool ptrAllowed = true);

	// Parses an array declarator. Handles both standard arrays and VLA's.
	bool ParseArrayDeclarator(shared<DI> declaration);
	
	// Parses a function declarator, including all the parameters.
	// Each parameter is validated by semantic analysis.
	bool ParseFunctionDeclarator(shared<DI> declaration);

	// Parses an initializer construct. Handles {} and constants.
	// Designators are handled by 'ParseDesignator'. The information is placed
	// in temporary objects that will be converted to the final form when
	// the whole declaration is validated by semantic analysis.
	bool ParseInitializer(shared<InitInfo> initializer, bool allowDesign);
	
	// Parses a C99-only designator. Handles both array and field-designators.
	// Uses 'ParseInitializer' to parse the right side of the =.
	bool ParseDesignator(shared<Designator> designator);

	// Parses the specifiers of a declarator. Includes storage-class, qualifiers,
	// type specifiers and tags. Tags are redirected to the corresponding methods.
	bool ParseSpecifiers(SpecifierInfo& info, bool& skipToSemi);

	// Parses the qualifiers of a declarator. Any combination is allowed.
	bool ParseQualifiers(SpecifierInfo& info);

	// Used by typenames. Parses type specifiers and qualifiers. 
	// Errors are emitted if storage-class specifiers are found.
	bool ParseTypenameSpecifiers(SpecifierInfo& info);

	// Parses an 'enum' declaration found in a specifier list. Handles declaration,
	// definition and referencing. Calls 'ParseEnumList' if it has a body.
	bool ParseEnumDeclaration(SpecifierInfo& info);

	// Parses the list of constants that form the body of the 'enum'.
	// The final value of the constants is chosen during the semantic analysis step.
	bool ParseEnumList(SpecifierInfo& info);

	// Parses an 'enum' constant (identifier and the optional ICE).
	// Each constant is validated before added to the 'enum' type.
	bool ParseEnumConstant(shared<EnumDeclaration> enumDecl);

	// Parses the declaration of a 'struct' or 'union'. Handles declaration,
	// definition and referencing. Calls 'ParseFieldList' if a body is found.
	bool ParseStructDeclaration(SpecifierInfo& info);

	// Used for struct/union parsing. Parses all specifiers, except storage-class.
	bool ParseTypeQualifiers(SpecifierInfo& info);

	// Parses the body of a struct/union declaration. When the whole list
	// is parsed the declaration is turned into a definition.
	bool ParseFieldList(shared<StructUnionDeclaration> declaration);

	// Parses the declaration of a struct/union member. Parses the declarator
	// and the optional bitfield. Semantic analysis validates each field.
	bool ParseFieldDeclaration(shared<StructUnionDeclaration> declaration);

	// Parses type-names, which are declarators without a name.
	const Type* ParseTypename();

	// Parses and combines a list of string literals.
	shared<Expression> ParseString();

	// Converts the current number token to the corresponding expression.
	// Emits errors if the number is invalid, and warnings if overflow occurs.
	shared<Expression> ParseNumber();

	// Converts the current character token to the corresponding expression.
	shared<Expression> ParseCharacter();

	// Returns the precedence of the given operator. The precedence can be deduced
	// from the grammar (Windows SDK has a table with the precedence of all operators).
	int GetPrecedence(TokenKind kind);

	// Parses and validates an expression. The expression stops at ;
	shared<Expression> 
	ParseExpression();

	// Parses and validates an expression. The expression stops at ,
	shared<Expression> 
	ParseAssignmentExpression();

	// Parses and validates an expression. The expression stops at any
	// of the assignment operators. Used for enum constants, bitfields, etc.
	shared<Expression> 
	ParseConstantExpression();

	// Returns 'true' if the operator is right-associative (any of the
	// assignment operators and the conditional operator).
	bool IsRightAssociative(TokenKind kind);

	// Parses a sequence of binary operator expressions. Assumes that the left
	// part has been already parsed and the first token is the next operator.
	// Uses the "precedence climbing" algorithm (with some optimizations).
	shared<Expression>
	ParseBinaryOp(shared<Expression> left, int minPrecedence);

	// Parses constant (number, char, string), unary and postfix expressions.
	// Other methods are called to handle more complicated cases.
	shared<Expression> 
	ParseOtherExpression(bool onlyUnary);

	// Parses the content of the 'sizeof' operator. Uses 'ParseParenExpression'
	// to figure out what the content of the parens is.
	shared<Expression> 
	ParseSizeof();

	// Parses an expression that stars with a paren. Can be either a cast,
	// a compound literal or grouping parens.
	// If 'castAllowed' is not set it returns without parsing the found cast.
	shared<Expression> 
	ParseParenExpression(bool castAllowed, ParenExprType& type);

	// Parses the suffix of a postfix expression. Handles function call, subscript,
	// object member and the postfix ++, -- operators.
	shared<Expression> 
	ParsePostfixRight(shared<Expression> left);

	// Parses the arguments of a function call (each can be an expression).
	shared<Expression> 
	ParseFunctionCall(shared<Expression> left);
	
	// Verifies if the current token should be treated like a type.
	bool IsTypedefName();

	// Parses any valid statement. Acts like a dispatch method based on the
	// type of the current token (and the next token, for labels).
	// This includes declarations and expressions.
	shared<Statement> ParseStatement();
	
	// Parses a statement referenced by a label (name:statement).
	shared<Statement> ParseLabeledStatement();

	// Parses a 'for' statement, including it's body.
	// Skips to the end of the body if an error occurs.
	shared<Statement> ParseForStatement();
	
	// Parses the controlling expression and the body of a 'while' statement.
	shared<Statement> ParseWhileStatement();
	
	// Parses a 'do' statement. Skips the body if an error occurs, so that
	// the controlling expression can be parsed and validated.
	shared<Statement> ParseDoStatement();
	
	// Parses a 'switch' statement, including all the associated 'case' labels.
	// At the beginning the statement is pushed on a stack so that 'case'
	// statements know which the active 'switch' is.
	shared<Statement> ParseSwitchStatement();
	
	// Parses a 'case' statement. Because these can be nested we parse all
	// of them here instead of calling this again (it could overflow the stack).
	shared<Statement> ParseCaseStatement();
	
	// Parses a 'default' statement. Duplicates are checked only when the whole
	// 'switch' body is available by the semantic analysis module.
	shared<Statement> ParseDefaultStatement();
	
	// Parses a compound statement (a series of statements in curly braces).
	// If an error occurs it skips to the end (after }).
	// This only creates a new context and calls 'ParseCompoundBody' to do the work.
	shared<Statement> ParseCompoundStatement();

	shared<Statement> ParseCompoundBody();
	
	// Parses an 'if' statement, and the associated 'else' if available.
	// If an error occurs in the 'then' part it skips to it's end so that
	// a potential 'else' part to be parsed and checked.
	shared<Statement> ParseIfStatement();
	
	// Parses a 'goto' statement. Note that the referenced label must not be
	// necessarily defined yet (a definition will be created now and will be
	// completed when the label is found).
	shared<Statement> ParseGotoStatement();

	// Parses a 'continue' statement. It can only inside a loop.
	shared<Statement> ParseContinueStatement();

	// Parses a 'break' statement. It can only inside a loop or 'switch'.
	shared<Statement> ParseBreakStatement();

	//  Parses a 'return' statement and it's associated expression (if any). 
	shared<Statement> ParseReturnStatement();

	// Parses a declaration and wraps it into a 'DeclarationStatement' object.
	// Note that when we call this we're certain that a declaration should begin.
	shared<Statement> ParseDeclarationStatement();

	// Parses an expression and wraps it into a 'ExpressionStatement' object.
	shared<Statement> ParseExpressionStatement();

	// Parses a a function definition.
	bool ParseFunctionDefinition(shared<DI> declInfo, SpecifierInfo& info,
								 LocationInfo startLocation);

	// Parses a declaration at the top-level (file) scope.
	// Can be a declaration of a variable or a function definition.
	bool ParseFileDeclaration();

public:
	Parser(Lexer* lexer, Context* context, 
           TypeManager* types, SemanticHolder* semaHolder);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns data about the current translation unit.
	Unit& UnitInfo() {
		return unit_;
	}

	const Unit& UnitInfo() const {
		return unit_;
	}

	// Returns the associated lexer.
	Lexer* GetLexer() {
		return lexer_;
	}

	// Peeks the next token from the Lexer, without consuming it.
	Token& PeekNext();

	// Obtains the next token from the Lexer.
	void EatToken();

	// Returns the current token.
	Token& CurrentToken() {
		return current_;
	}

	// Returns the type of the keyword. 
	// 'Keyword_None' is returned if the token is not a keyword.
	KeywordType Kwd(const Token& token) const;

	// Adds the specified extension to the appropriate lists.
	void AddExtension(shared<Extension> extension);

	// Returns the list of extensions that handle tokens that are not standard.
	List<shared<Extension>>& UnknownExtensions() {
		return unknownExts_;
	}

	// Returns the list of extensions that can introduce new types.
	List<shared<Extension>>& TypeExtensions() {
		return typeExts_;
	}

	// Returns the list of extensions that want to be notified when
	// a function definition begins.
	List<shared<Extension>>& FunctionBeginExtensions() {
		return functBeginExts_;
	}

	// Returns the list of extensions that want to be notified when
	// a function definition ends.
	List<shared<Extension>>& FunctionEndExtensions() {
		return functEndExts_;
	}

	// Returns the list of extensions that want to be notified when
	// a new declaration has been parsed and validated.
	List<shared<Extension>>& DeclarationSucceededExtensions() {
		return declSucceededExts_;
	}

	// Returns the list of extensions that want to be notified when
	// a new declaration could not be parsed or validated.
	List<shared<Extension>>& DeclarationFailedExtensions() {
		return declFailedExts_;
	}

	// Parses the whole translation unit.
	bool ParseTranslationUnit();
};

} // namespace Parsing
#endif