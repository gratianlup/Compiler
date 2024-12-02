// StatementSemantic.hpp
// Copyright (c) Lup Gratian
//
// Defines the methods that perform semantic analysis on statements.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_STATEMENT_SEMANTIC_HPP
#define PC_PARSING_STATEMENT_SEMANTIC_HPP

#include "../Base/DebugValidator.hpp"
#include "../Base/Stack.hpp"
#include "../Common/Context.hpp"
#include "../AST/DeclarationContext.hpp"
#include "../AST/Declaration.hpp"
#include "../AST/Declarations.hpp"
#include "../AST/Expression.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/Type.hpp"
#include "../AST/Types.hpp"
#include "../AST/Statements.hpp"
#include "../Lexer/Token.hpp"
#include "../AST/Unit.hpp"
#include "../AST/TypeCombiner.hpp"
#include "../AST/TypeManager.hpp"
#include "ParserHelpers.hpp"
#include "Semantic.hpp"
using namespace Base;
using namespace AST;
using namespace Common;
using namespace Lexing;

namespace Parsing {

class StatementSemantic : public Semantic {
protected:
	Context* context_;
	Diagnostic* diag_;
	TypeManager* types_;
	TypeCombiner typeComb_;
	int returnCount_;                         // Used to check for missing 'return'.
	Stack<shared<SwitchStatement>> switchStack_;
	shared<FunctionDeclaration> activeFunct_; // Used by 'return' statements.

public:
	StatementSemantic(Context* context, TypeManager* typeMan) :
			context_(context), diag_(&context->Diagnostic()),
			types_(typeMan), typeComb_(typeMan),
			returnCount_(0) {}

	virtual ~StatementSemantic() {
		// The shared pointers take care of everything.
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// 
	virtual shared<Statement> 
	HandleLabel(shared<Identifier> label, shared<Statement> statement,
				shared<DeclarationContext> context);
	
	// 
	virtual shared<Statement>
	HandleIf(shared<Expression> condition, shared<Statement> bodyStatement, 
			 shared<Statement> elseStatement, bool inParens, 
             shared<DeclarationContext> context,
			 LocationInfo startLocation);
	
	// 
	virtual shared<Statement> 
	HandleWhile(shared<Expression> condition, shared<Statement> bodyStatement, 
			    shared<DeclarationContext> context, LocationInfo startLocation);
	
	// 
	virtual shared<Statement> 
	HandleDo(shared<Expression> condition, shared<Statement> bodyStatement, 
			 shared<DeclarationContext> context, LocationInfo startLocation,
             LocationInfo whileLocation);
	
	// 
	virtual shared<Statement> 
	HandleFor(shared<Statement> initStatement, shared<Expression> condExpr, 
			  shared<Expression> incExpr, shared<Statement> bodyStatement,
			  shared<DeclarationContext> context, LocationInfo startLocation);
	
	// 
	virtual shared<Statement> 
	HandleGoto(shared<Identifier> name, shared<DeclarationContext> context, 
               LocationInfo startLocation);
	
	// 
	virtual shared<Statement> 
	HandleContinue(shared<DeclarationContext> context, LocationInfo startLocation);
	
	// 
	virtual shared<Statement> 
	HandleBreak(shared<DeclarationContext> context, LocationInfo startLocation);
	
	// 
	virtual shared<Statement> 
	HandleReturn(shared<Expression> returnExpr, shared<DeclarationContext> context, 
				 LocationInfo startLocation);
	
	// 
	virtual shared<SwitchStatement> 
	HandleSwitchBegin(shared<Expression> expr, shared<DeclarationContext> context, 
					  LocationInfo startLocation);
	
	// 
	virtual shared<SwitchStatement> 
	HandleSwitchEnd(shared<SwitchStatement> switchStatement, shared<Statement> bodyStatement,
					shared<DeclarationContext> context, LocationInfo startLocation);
	
	// 
	virtual void PopSwitch();
	
	// 
	virtual shared<Statement> 
	HandleCase(shared<Expression> expr, shared<Statement> statement,
			   shared<DeclarationContext> context, LocationInfo startLocation);

	// 
	virtual shared<Statement> 
	HandleDefault(shared<Statement> statement, shared<DeclarationContext> context, 
                  LocationInfo startLocation);

	// 
	virtual void SetActiveFunction(shared<FunctionDeclaration> value);

	virtual shared<FunctionDeclaration> ActiveFunction() {
		return activeFunct_;
	}

	// 
	virtual int ReturnCount() const;
};

} // namespace Parsing
#endif