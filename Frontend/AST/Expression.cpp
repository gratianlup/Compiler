#include "Expression.hpp"
#include "Expressions.hpp"
#include "Declarations.hpp"
#include "ConstantEvaluator.hpp"

namespace AST {

bool Expression::EqualsImpl(const Expression* other) const {
	return resultType_->Equals(other->resultType_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Expression* Expression::WithoutCasts() {
	// Removes all casts that surround the expression.
	if(auto castExpr = As<CastExpression>()) {
		while(true) {
			Expression* expr = castExpr->Target();

			if(expr->IsCastExpr()) {
				castExpr = expr->As<CastExpression>();
			}
			else return expr;
		}
	}
	else return this;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Expression* Expression::WithoutCasts() const {
	// Removes all casts that surround the expression.
	if(auto castExpr = As<CastExpression>()) {
		while(true) {
			const Expression* expr = castExpr->Target();

			if(expr->IsCastExpr()) {
				castExpr = expr->As<CastExpression>();
			}
			else return expr;
		}
	}
	else return this;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
EvaluationInfo Expression::Evaluate(const Context* context, bool warn,
							  bool expectICE, bool allowVars) const {
	EvaluationResult result;
	EvaluationInfo info;
	bool hasSideffects;
	bool hasVariable;
	auto evaluator = ConstantEvaluator(this, expectICE, context, allowVars, warn);

	if(evaluator.GetResult(result, hasSideffects, hasVariable)) {
		if(result.ResultType->IsInteger()) {
			// Can't be a constant if it's a reference to a variable.
			if(hasVariable) return info;
			info = EvaluationInfo(ResultType::IntConst, result.Value);
		}
		else if(expectICE) {
			// This is not an ICE.
			return info;
		}
		else if(result.ResultType->IsFloating()) {
			// Can't be a constant if it's a reference to a variable.
			if(hasVariable) return info;
			info = EvaluationInfo(ResultType::FloatConst, result.Value);
		}
		else {
			// For pointers to function, null-pointer constants and global variables.
			info = EvaluationInfo(ResultType::Other, result.Value, hasVariable);
		}
	}

	return info;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
EvaluationInfo Expression::EvaluateAsICE(const Context* context, bool warn) const {
	return Evaluate(context, warn, true /* expectICE */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Expression::IsConstant(const Context* context, bool allowVars) const {
	if(isConst_ == 1) return true;
	else if(isConst_ == 2) return false;
	else return Evaluate(context, false /* warn */, 
						 false /* expectICE */, allowVars).IsConstant();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Expression::IsIntegerConstant(const Context* context) const {
	return EvaluateAsICE(context, false /* warn */).IsConstant();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Expression::TryEvaluateConstant(const Context* context, EvaluationInfo* info,
									 bool allowVars) const {
	DebugValidator::IsNotNull(info);
	
	// If the expression is marked as not being a constant 
	// we don' try to evaluate it anymore.
	if(isConst_ == 2) return false;

	*info = Evaluate(context, false /* warn */, false /* expectICE */, allowVars);
	return info->IsConstant();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<FieldDeclaration> Expression::GetBitfield() const {
	// First strip all implicit casts that don't modify the value.
	const Expression* expr = this;

	while(expr->IsCastExpr()) {
		auto cvtExpr = expr->As<CastExpression>();
		if(cvtExpr->IsExplicit()) {
			break;
		}

		if((cvtExpr->Type() != CastType::ArrayToPtr) &&
		   (cvtExpr->Type() != CastType::FunctionToPtr),
		   (cvtExpr->Type() != CastType::RemoveQual)) {
			break;
		}

		expr = cvtExpr->Target();
	}

	// If this is a member expression check the member.
	if(auto temp = expr->As<MemberExpression>()) {
		if(temp->Member()->IsBitfield()) {
			return temp->Member();
		}
	}

	// An expression like (a.b = xyz), where b is a bitfield has
	// the bitfield type as it's result; we need to check for this case.
	if(auto temp = expr->As<BinaryOperator>()) {
		if(temp->IsAssignment() && temp->LeftValue()) { // Check any assignment op.
			return temp->LeftValue()->GetBitfield();
		}
	}

	return nullptr; // No bitfield found.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Expression::IsNullPointer(const Context* context, bool relaxed) const {
	// C99:6.3.2.3.3: a null pointer is considered an ICE with value 0
	// or such an expression cast to 'void *'.
	if(auto temp = As<CastExpression>()) {
		if((temp->IsExplicit() == false) || relaxed) {
			return temp->Target()->IsNullPointer(context);
		}

		if(auto pointerType = temp->ResultType()->As<PointerType>()) {
			if(pointerType->PointeeType()->IsVoid() && 
				temp->Target()->ResultType()->IsInteger()) {
				return temp->Target()->IsNullPointer(context);
			}
		}
		else return false;
	}
	else if(ResultType()->IsInteger() == false) return false;

	// Try to evaluate the expression, it should be 0.
	EvaluationInfo eval = EvaluateAsICE(context, false /* warn */);
	return eval.IsIntConstant() && (eval.IntValue() == 0);
}

} // namespace AST