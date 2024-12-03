// Extension.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for all modules that handle extensions of the language.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_EXTENSION_HANDLER_HPP
#define PC_PARSING_EXTENSION_HANDLER_HPP

#include "../Common/Context.hpp"
#include "../Common/LocationInfo.hpp"
#include "../AST/Declaration.hpp"
#include "../AST/Expression.hpp"
#include "../AST/Statement.hpp"
#include "../Lexer/Lexer.hpp"
#include "../Lexer/Token.hpp"
#include "ParserHelpers.hpp"
using namespace AST;
using namespace Common;
using namespace Lexing;

namespace Parsing {

// Forward declarations.
class Parser;

// Represents the situations in which an extension can appear.
enum ContextType {
	Context_Specifiers, // Called while parsing specifiers.
	Context_Qualifiers, // Called while parsing qualifiers.
	Context_Prefix,     // Called while parsing a prefix expression.
	Context_Postfix,    // Called while parsing a postfix expression.
	Context_Declarator, // Called while parsing a declarator.
	Context_Statement,  // Called while parsing statements.
	Context_Unit,       // Called while parsing at the unit level.
};


// Represents the available information when the extension handler has been called.
struct ExtensionContext {
	SpecifierInfo*   Specifiers;
	DeclaratorInfo*  Declarator;
	Declaration*     DeclarationInfo;
	Expression*      LeftExpression;
	ContextType      Type;
	bool             CanHavePostfix; // Should be set by extensions that handle expressions.
	Statement*       ResultStatement;
	bool             Invalid;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ExtensionContext(ContextType type) :
			Type(type), DeclarationInfo(nullptr), Specifiers(nullptr), 
			Declarator(nullptr), LeftExpression(nullptr), CanHavePostfix(false),
			ResultStatement(nullptr), Invalid(false) {}

	// To be used for declarations.
	ExtensionContext(ContextType type, SpecifierInfo* specifiers, 
					 DeclaratorInfo* declarator, Declaration* declaration = nullptr) :
			Type(type), DeclarationInfo(declaration), Specifiers(specifiers),
			Declarator(declarator), LeftExpression(nullptr), CanHavePostfix(false),
			ResultStatement(nullptr), Invalid(false) {}

	// To be used for expressions.
	ExtensionContext(ContextType type, Expression* left) :
			Type(type), LeftExpression(left), DeclarationInfo(nullptr), 
			Specifiers(nullptr), Declarator(nullptr), CanHavePostfix(false),
			ResultStatement(nullptr), Invalid(false) {}
};


// Represents the base class for all extension handlers.
class Extension {
protected:
	Context*    context_;
	Parser*     parser_;
	Diagnostic* diag_;

public:
	Extension() : context_(nullptr), parser_(nullptr), diag_(nullptr) {}

	Extension(Context* context, Parser* parser) :
			context_(context), parser_(parser), diag_(&context->Diagnostic()) {}

	virtual ~Extension() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Should return 'true' if the extension is interested in being notified
	// when a declaration has been parsed successfully and is valid.
	virtual bool ReceiveDeclarationSucceeded() const {
		return false;
	}

	// Should return 'true' if the extension is interested in being notified
	// when a declaration could not be parsed or is invalid.
	virtual bool ReceiveDeclarationFailed() const {
		return false;
	}

	// Should return 'true' if the extension is interested in being notified
	// when the definition of a function begins.
	virtual bool ReceiveFunctionBegin() const {
		return false;
	}

	// Should return 'true' if the extension is interested in being notified
	// when the definition of a function ended.
	virtual bool ReceiveFunctionEnd() const {
		return false;
	}

	// Should return 'true' if the extension should check 
	// if a token represents a type specifier.
	virtual bool ReceiveTypeQuery() const {
		return false;
	}

	// Should return 'true' if the extension handles tokens that are not part
	// of the standard specifications.
	virtual bool ReceiveUnknown() const {
		return false;
	}

	// Handles the extension that begins with the specified token.
	// Should return 'true' if the token could be successfully handled, 'false' otherwise.
	virtual bool Handle(ExtensionContext& context) {
		return false;
	}

	// Called when a declaration could be parsed and validated.
	virtual void DeclarationSucceeded(Declaration* declaration) {}

	// Called when a declaration could not be parsed or validated.
	virtual void DeclarationFailed() {}

	// Called when a function definition is beginning.
	virtual void FunctionBegin(Declaration* declaration) {}

	// Called when a function definition has ended.
	virtual void FunctionEnd(Declaration* declaration) {}

	// Should return 'true' if the specified token represents a type specifier.
	virtual bool IsTypeToken(Token* token) const { 
		return false; 
	}
};

} // namespace Parsing
#endif
