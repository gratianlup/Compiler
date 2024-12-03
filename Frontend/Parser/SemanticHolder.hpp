// SemanticHolder.hpp
// Copyright (c) Lup Gratian
//
// Contains the modules that should be used for semantic analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_SEMANTIC_HOLDER_HPP
#define PC_PARSING_SEMANTIC_HOLDER_HPP

#include "../Base/SharedPointer.hpp"
#include "Semantic.hpp"
using namespace Base;

namespace Parsing {

// Forward declarations
class DeclarationSemantic;
class ExpressionSemantic;
class StatementSemantic;

class SemanticHolder {
private:
	shared<DeclarationSemantic> declSema_;
	shared<ExpressionSemantic> exprSema_;
	shared<StatementSemantic> statementSema_;

public:
	SemanticHolder() {}

	SemanticHolder(shared<DeclarationSemantic> declaration, 
                   shared<ExpressionSemantic> expr,
				   shared<StatementSemantic> statement);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the module that performs semantic analysis on declarations.
	shared<DeclarationSemantic> GetDeclarationSemantic();

	void SetDeclarationSemantic(shared<DeclarationSemantic> value);

	// Returns the module that performs semantic analysis on expressions.
	shared<ExpressionSemantic> GetExpressionSemantic();

	void SetExpressionSemantic(shared<ExpressionSemantic> value);

	// Returns the module that performs semantic analysis on statements.
	shared<StatementSemantic> GetStatementSemantic();

	void SetStatementSemantic(shared<StatementSemantic> value);
};

} // namespace Parsing
#endif