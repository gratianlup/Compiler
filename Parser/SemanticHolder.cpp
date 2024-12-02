#include "SemanticHolder.hpp"
#include "DeclarationSemantic.hpp"
#include "ExpressionSemantic.hpp"
#include "StatementSemantic.hpp"

namespace Parsing {

SemanticHolder::SemanticHolder(shared<DeclarationSemantic> declaration, shared<ExpressionSemantic> expr,
							   shared<StatementSemantic> statement) :
			declSema_(declaration), exprSema_(expr), statementSema_(statement) {
	declaration->SetHolder(this);
	expr->SetHolder(this);
	statement->SetHolder(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<DeclarationSemantic> SemanticHolder::GetDeclarationSemantic() { 
	return declSema_; 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SemanticHolder::SetDeclarationSemantic(shared<DeclarationSemantic> value) {
	declSema_ = value;
	declSema_->SetHolder(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SemanticHolder::SetExpressionSemantic(shared<ExpressionSemantic> value) {
	exprSema_ = value;
	exprSema_->SetHolder(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<ExpressionSemantic> SemanticHolder::GetExpressionSemantic() { 
	return exprSema_; 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<StatementSemantic> SemanticHolder::GetStatementSemantic() { 
	return statementSema_; 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SemanticHolder::SetStatementSemantic(shared<StatementSemantic> value) {
	statementSema_ = value;
	statementSema_->SetHolder(this);
}

} // namespace Parsing