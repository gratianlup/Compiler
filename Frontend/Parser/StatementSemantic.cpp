// StatementSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements the methods that perform semantic analysis on statements.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StatementSemantic.hpp"
#include "ExpressionSemantic.hpp"
#include "SemanticHolder.hpp"

namespace Parsing {

shared<Statement> 
StatementSemantic::HandleLabel(shared<Identifier> label, shared<Statement> statement,
						       shared<DeclarationContext> context) {
	// The identifier must be unique in the function. If it's already declared
	// it isn't necessarily an error because it can be a forward reference
	// from a 'goto' that needs to be completed now.
	shared<LabelStatement> previous = context->FindLabel(label, nullptr);

	if(previous) {
		if(previous->IsReference()) {
			// Complete the reference.
			previous->SetTarget(statement);
			previous->SetLocation(label->Location());
			return previous;
		}

		// This is a redeclaration.
		diag_->Report(Error::LABEL_REDECLARATION)<<*label;
		return nullptr;
	}

	// Create a new statement and add it to the context.
	shared<LabelStatement> labelStatement = 
            new LabelStatement(label, statement, label->Location());

	context->ParentFunction()->AddLabel(label, labelStatement);
	return labelStatement;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleIf(shared<Expression> condition, shared<Statement> bodyStatement, 
					        shared<Statement> elseStatement, bool inParens,
					        shared<DeclarationContext> context, LocationInfo startLocation) {
	if(Expression::IsInvalid(condition)) {
        return nullptr;
    }

	// The controlling expression must have scalar type (C99:.6.8.4.1.1).
	if(condition->ResultType()->WithoutQualifiers()->IsScalar() == false) {
		diag_->Report(Error::IF_CONDITION_NOT_SCALAR)<<startLocation;
		return nullptr;
	}

	// Warn if the body is a 'NullStatement' and no 'else' part exists
	// (can be a typo like 'if(E) ;').
	if(bodyStatement->IsNullStatement() && (elseStatement == nullptr)) {
		diag_->Report(Warning::IF_BODY_NULL)<<startLocation;
	}

	// Check for assignment in the controlling expressions.
	// Warn about a (possible) mistake like 'if(a = 3)' (instead of 'if(a == 3)').
	// If the expression is in parens don't emit the warning.
	if((inParens == false) && condition->IsBinaryOp()) {
		auto temp = condition->As<BinaryOperator>();

		if(temp->Operator() == BinaryOpType::Eq) {
			diag_->Report(Warning::IF_BODY_NULL)<<startLocation;
		}
	}

	// Create the statement.
	return new IfStatement(condition, bodyStatement, elseStatement, startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleWhile(shared<Expression> condition, shared<Statement> bodyStatement, 
						       shared<DeclarationContext> context, LocationInfo startLocation) {
	if(Expression::IsInvalid(condition)) {
        return nullptr;
    }

	// The controlling expression must have scalar type (C99:.6.8.5.2).
	if(condition->ResultType()->WithoutQualifiers()->IsScalar() == false) {
		diag_->Report(Error::WHILE_CONDITION_NOT_SCALAR)<<startLocation;
		return nullptr;
	}

	return new WhileStatement(condition, bodyStatement, startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleDo(shared<Expression> condition, shared<Statement> bodyStatement, 
					        shared<DeclarationContext> context, LocationInfo startLocation,
					        LocationInfo whileLocation) {
	if(Expression::IsInvalid(condition)) {
        return nullptr;
    }

	// The controlling expression must have scalar type (C99:.6.8.5.2).
	if(condition->ResultType()->WithoutQualifiers()->IsScalar() == false) {
		diag_->Report(Error::DO_CONDITION_NOT_SCALAR)<<startLocation;
		return nullptr;
	}

	return new DoStatement(condition, bodyStatement, startLocation, whileLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleFor(shared<Statement> initStatement, shared<Expression> condExpr, 
						     shared<Expression> incExpr, shared<Statement> bodyStatement,
						     shared<DeclarationContext> context, LocationInfo startLocation) {
	// The declaration part should not have declarations with other 
	// storage-class than 'auto' or 'register'.
	if(auto temp = initStatement->As<DeclarationStatement>()) {
		DeclarationList* list = temp->Base()->As<DeclarationList>();
		auto& declarations = list->Declarations();

		for(int i = 0; i < declarations.Count(); i++) {
			if((declarations[i]->Storage() == StorageType::Static) ||
			   (declarations[i]->Storage() == StorageType::Extern)) {
				diag_->Report(Error::FOR_DECLARATION_INVALID_STORAGE)
                              <<*declarations[i]->Name();
				return nullptr;   
			}
		}
	}

	// The controlling expression must have scalar type (C99:.6.8.5.2).
	// If it doesn't exist the loop becomes an infinite loop (handled during codegen).
    auto resultType = condExpr->ResultType()->WithoutQualifiers();

	if(condExpr && (resultType->IsScalar() == false)) {
		diag_->Report(Error::IF_CONDITION_NOT_SCALAR)<<startLocation;
		return nullptr;
	}

	return new ForStatement(initStatement, condExpr, incExpr,
                            bodyStatement, startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleGoto(shared<Identifier> name, 
                              shared<DeclarationContext> context, 
						      LocationInfo startLocation) {
	// Check if the target is a label in the enclosing function.
	// If none is found this must be forward reference, so we add it to the context.
	// When the label is found this reference is converted in a real label.
	shared<LabelStatement> label = context->FindLabel(name);

	if(label == nullptr) {
		label = new LabelStatement(name, nullptr, startLocation);
		context->ParentFunction()->AddLabel(name, label);
	}

	return new GotoStatement(label, startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleContinue(shared<DeclarationContext> context, 
                                  LocationInfo startLocation) {
	// C99:6.8.6.2.1: 'continue' can appear only in a scope
	// that is the child of a loop statement.
	if(context->ParentLoop()->IsFileScope()) {
		diag_->Report(Error::CONTINUE_NOT_IN_LOOP)<<startLocation;
	}

	return new ContinueStatement(startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleBreak(shared<DeclarationContext> context, 
                               LocationInfo startLocation) {
	// C99:6.8.6.3.1: 'break' can appear only in a scope	
	// that is the child of a loop or a switch statement.
	if(context->ParentLoop()->IsFileScope() && 
       context->ParentSwitch()->IsFileScope()) {
		diag_->Report(Error::BREAK_NOT_IN_LOOP_OR_SWITCH)<<startLocation;
	}

	return new BreakStatement(startLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleReturn(shared<Expression> returnExpr, 
                                shared<DeclarationContext> context, 
						        LocationInfo startLocation) {
	// 'return' can appear only in the context of a function definition.
	// If the function returns 'void' there should be no expression.
	// Else the type of the expression should be compatible (as by assignment)
	// with the type returned by the function.
	// Note that we emit warnings instead of errors of this, because this is
	// the behavior of both gcc and VC.
	if(activeFunct_ == nullptr) {
		diag_->Report(Error::RETURN_NOT_IN_FUNCTION)<<startLocation;
		return nullptr;
	}

	const FunctionType* functionType = activeFunct_->DeclarationType();
	bool invalid = false;
	
	if(functionType->IsVoid()) {
		if(returnExpr) {
			// Something like 'void f() { return E; }'.
			diag_->Report(Warning::RETURN_EXPRESSION_FOR_VOID)<<startLocation;
			invalid = true;
		}
	}
	else if(returnExpr == nullptr) {
		// Something like 'T f() { return; }'.
		diag_->Report(Warning::RETURN_MISSING_EXPRRESSION)<<startLocation;
	}

	// Handled the special cases first.
	if(returnExpr == nullptr) {
		// We can create the statement if it doesn't have an expression.
		return new ReturnStatement(nullptr, startLocation);
	}
	else if(invalid) {
		// No conversions are needed if the return is invalid.
		return new ReturnStatement(returnExpr, startLocation);
	}

	// Typecheck the return type and make the conversion.
	ExpressionSemantic* exprSema = Holder()->GetExpressionSemantic();

	if(exprSema->IsSimpleAssignmentValid(functionType->ReturnType(), 
                                         returnExpr, nullptr)) {
		// Assignment is valid, now convert the right expression
		// to the left (if the types are the same no conversion will be done).
		// C99:6.5.2.3.7: the unqualified version of 'left' is considered.
		const Type* destType = functionType->ReturnType()->WithoutQualifiers();

		// Keep track of the number of 'return' statements
		// (used to check that a function with return type has at least a 'return').
		returnCount_++;
		returnExpr = exprSema->CreateImplicitCast(returnExpr, destType, CastType::Unknown);
		return new ReturnStatement(returnExpr, startLocation);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<SwitchStatement> 
StatementSemantic::HandleSwitchBegin(shared<Expression> expr, 
                                     shared<DeclarationContext> context, 
								     LocationInfo startLocation) {
	if(Expression::IsInvalid(expr)) return nullptr;

	// The controlling expression must have integer type. 
	// The integer promotions are performed on it (C99:6.8.4.2.5).
	if(expr->ResultType()->WithoutQualifiers()->IsInteger() == false) {
		diag_->Report(Error::SWITCH_EXPRESSION_NOT_INTEGER_TYPE)<<startLocation;
		return nullptr;
	}

	// Warn if the expression value acts like a boolean 
	// (like in 'switch(a == b)' or 'switch(a < b)').
	if(auto temp = expr->As<BinaryOperator>()) {
		if(temp->IsEquality() || temp->IsRelational()) {
			diag_->Report(Warning::SWITCH_EXPRESSION_BOOLEAN)<<startLocation;
		}
	}

	// The integer promotions need to be applied on the expression.
	expr = Holder()->GetExpressionSemantic()->IntegerPromotion(expr);
	shared<SwitchStatement> switchStatement = new SwitchStatement(expr, startLocation);

	// Push it on the stack so that the next statements know we're inside a 'switch'.
	switchStack_.Push(switchStatement);
	return switchStatement;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<SwitchStatement> 
StatementSemantic::HandleSwitchEnd(shared<SwitchStatement> switchStatement, 
                                   shared<Statement> bodyStatement,
							       shared<DeclarationContext> context, 
                                   LocationInfo startLocation) {
	DebugValidator::IsLarger(switchStack_.Count(), 0);

	// Each 'case' statement should have an unique value, and 'default'
    // should appear at most once. A dictionary is used to keep track of the values.
	ExpressionSemantic* exprSema = Holder()->GetExpressionSemantic();
	Dictionary<__int64, Statement*> caseStatements;
	shared<CaseStatement> defaultStatement;
	int caseCount = 0;
	bool invalid = false;
	auto& statementList = switchStatement->CaseList();

	for(int i = 0; i < statementList.Count(); i++) {
		auto& statement = statementList[i];

		// Check for duplicate 'default'.
		if(statement->IsDefault()) {
			if(defaultStatement) {
				// Report, but continue so we verify more.
				diag_->Report(Error::DEFAULT_DUPLICATE)<<startLocation;
				invalid = true;
			}
			else defaultStatement = statement;
		}
		else {
			// Obtain the value of the 'case' and see if it's a duplicate.
			EvaluationInfo info = statement->Value()->EvaluateAsICE(context_, true /* warn */);
			caseCount++;

			if(caseStatements.ContainsKey(info.IntValue())) {
				// Report, but continue so we verify more.
				diag_->Report(Error::CASE_DUPLICATE)<<statement->Location();
				invalid = true;
				continue;
			}

			caseStatements.Add(info.IntValue(), statement);

			// The type of the expression must be converted to the one
			// of the 'switch' controlling expression. Warn if overflow
			// is going to occur (C99:6.8.4.2.5).
			const BasicType* prevType = statement->Value()->ResultType()->As<BasicType>();
			const Type* newType = switchStatement->Condition()->ResultType();

			statement->SetValue(exprSema->CreateImplicitCast(statement->Value(), newType, 
															 CastType::IntToInt));

			// Note that the promoted type can be  only 'int' or 'unsigned'.
			if(context_->Target()->IsOveflow(info.IntValue(), info.IntValue(), 
											 prevType->GetKind())) {
				// Warn about the overflow.
				diag_->Report(Warning::CASE_EXPRESSION_OVERFLOW)<<statement->Location();
			}
		}
	}

	// If the list is not valid give up.
	if(invalid) {
		switchStack_.Pop();
		return nullptr;
	}

	// Warn if there are statements before the first 'case' or 'default'.
	// (like in 'switch(E) { E1; case 0: {E2} }' - E1 will never be executed).
	// Don't remove theme, because there could be a 'goto' jump to these statements.
	if(auto temp = bodyStatement->As<CompoundStatement>()) {
		auto& list = temp->Children();

		for(int i = 0; i < list.Count(); i++) {
			if(list[i]->IsCaseStatement() == false) {
				diag_->Report(Warning::SWITCH_UNREACHEABLE_STMT)<<list[i]->Location();
			}
			else break;
		}
	}
	else if((bodyStatement->IsCaseStatement() == false) && 
            (bodyStatement->IsDefaultStatement() == false)) {
		diag_->Report(Warning::SWITCH_UNREACHEABLE_STMT)<<bodyStatement->Location();
	}

	// Warn if the body of the switch has no case/default statement.
	if((caseCount == 0) && (defaultStatement == nullptr)) {
		diag_->Report(Warning::SWITCH_EMPTY)<<switchStatement->Location();
	}

	switchStatement->SetBody(bodyStatement);
	switchStack_.Pop();
	return switchStatement;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementSemantic::PopSwitch() {
	DebugValidator::IsLarger(switchStack_.Count(), 0);
	switchStack_.Pop();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleCase(shared<Expression> expr, shared<Statement> statement,
						      shared<DeclarationContext> context, 
                              LocationInfo startLocation) {
	if(Expression::IsInvalid(expr)) return nullptr;

	// Verify that we're inside a switch.
	if(switchStack_.Count() == 0) {
		diag_->Report(Error::CASE_NOT_IN_SWITCH)<<startLocation;
		return nullptr;
	}

	// The expression should be an ICE (C99:6.8.4.3.2).
	EvaluationInfo eval = expr->EvaluateAsICE(context_, true /* warn */);

	if(eval.IsIntConstant() == false) {
		diag_->Report(Error::CASE_EXPRESSION_NOT_ICE)<<startLocation;
		return nullptr;
	}

	// Create the statement and add it to the active 'switch'.
	shared<CaseStatement> caseStatement = 
            new CaseStatement(expr, statement, switchStack_.Peek(), 
				              false /* isDefault */, startLocation);

	switchStack_.Peek()->CaseList().Add(caseStatement);
	return caseStatement;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Statement> 
StatementSemantic::HandleDefault(shared<Statement> statement, 
                                 shared<DeclarationContext> context, 
							     LocationInfo startLocation) {
	// Verify that we're inside a switch.
	if(switchStack_.Count() == 0) {
		diag_->Report(Error::CASE_NOT_IN_SWITCH)<<startLocation;
		return nullptr;
	}

	// Add it to the active 'switch'; duplicates will be checked later.
	shared<CaseStatement> defaultStatement = 
            new CaseStatement(nullptr, statement, switchStack_.Peek(), 
							  true /* isDefault */, startLocation);

	switchStack_.Peek()->CaseList().Add(defaultStatement);
	return defaultStatement;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementSemantic::SetActiveFunction(shared<FunctionDeclaration> value) {
	activeFunct_ = value;
	returnCount_ = 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int StatementSemantic::ReturnCount() const {
	return returnCount_;
}

} // namespace Parsing