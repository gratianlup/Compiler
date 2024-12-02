// ConstantEvaluator.hpp
// Copyright (c) Lup Gratian
//
// A simple constant evaluator that folds constants according to the C99 rules.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_CONSTANT_EVALUATOR_HPP
#define PC_AST_CONSTANT_EVALUATOR_HPP

#include "../Base/Stack.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/Types.hpp"
#include "../AST/Visitor.hpp"
#include "../AST/StructLayout.hpp"
#include "../AST/TypeSize.hpp"
using namespace Base;
using namespace AST;

namespace AST {

// Represents the result of a subexpression.
struct EvaluationResult {
	ValueUnion Value;
	const Type* ResultType;
    const DeclarationExpression* BaseVariable;
	bool HasVariable;
    bool IsInt;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	EvaluationResult() : IsInt(false), HasVariable(false), ResultType(nullptr) {}

	EvaluationResult(ValueUnion value, const Type* type, bool isInt = true, 
			         bool hasVariable = false, const DeclarationExpression* baseVar = nullptr) :
			Value(value), ResultType(type), IsInt(isInt), 
            HasVariable(hasVariable), BaseVariable(baseVar) {}
};


class ConstantEvaluator : public Visitor {
private:
	bool expectICE_;     // 'true' if the expression must be an ICE.
	bool invalid_;       // 'true' when the expression is found not to be a constant.
	bool hasSideffects_; // 'true' if there are side effects, like calling a function.
	bool warn_;          // If set we emit warnings about overflow.
	bool markConst_;     // If set the expressions are marked as 'constant' if it's the case.
	bool allowVars_;     // True if variables can appear in the expression.
	EvaluationResult result_;  // The stack used to keep the intermediate results.
	const Context* context_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual void Visit(const UnaryOperator	       *expr) override;
	virtual void Visit(const BinaryOperator	       *expr) override;
	virtual void Visit(const NumberConstant	       *expr) override;
	virtual void Visit(const CharConstant	       *expr) override;
	virtual void Visit(const StringConstant	       *expr) override;
	virtual void Visit(const SizeofOperator	       *expr) override;
	virtual void Visit(const SubscriptExpression   *expr) override;
	virtual void Visit(const MemberExpression	   *expr) override;
	virtual void Visit(const CallExpression		   *expr) override;
	virtual void Visit(const ConditionalOperator   *expr) override;
	virtual void Visit(const CastExpression		   *expr) override;
	virtual void Visit(const DeclarationExpression *expr) override;
	virtual void Visit(const CompoundExpression	   *expr) override;
	virtual void Visit(const InvalidExpression	   *expr) override;

	// Marks the expression as a constant, if requested.
	// Can be used as a cache mechanism.
	void MarkConstantExpression(const Expression* expr);

	// Verifies whether the specified expression is a reference to a
	// variable declaration (note that this looks though implicit casts).
	bool IsVariableDeclaration(const Expression* expr);

	// Verifies whether the specified expression is a integer number constant.
	// Used to handle cases like '&((char*)0)[3]'.
	bool IsIntegerBase(const Expression* expr);

	// Verifies whether the specified expression is a string constant.
	// Used to handle cases like '&"abc"[3]'.
	bool IsStringBase(const Expression* expr);

	// Verifies if the specified expression contains a reference to a declaration.
	// Does a recursive search in the whole subtree.
	bool ContainsReference(const Expression* expr);

	// Methods for evaluating binary operators.
	void EvaluateAdditiveOp(const BinaryOperator* expr);
	void EvaluatePointerAdditiveOp(const BinaryOperator* expr, 
                                   EvaluationResult left, EvaluationResult right);
	void EvaluateMultiplicativeOp(const BinaryOperator* expr);
	void EvaluateComparisonOp(const BinaryOperator* expr);
	void EvaluateLogicalOp(const BinaryOperator* expr);
	void EvaluateBitwiseOp(const BinaryOperator* expr);

	// Methods for performing casting between different types.
	void CastIntToFloat(const CastExpression* expr);
	void CastFloatToInt(const CastExpression* expr);
	void CastIntToInt(const CastExpression* expr);
	void CastFloatToFloat(const CastExpression* expr);
	void CastIntToPointer(const CastExpression* expr);

    // Methods that verify if overflow occurs.
    bool AdditionOveflowed(const BinaryOperator* expr, __int64 result, 
                           EvaluationResult& left, EvaluationResult& right);
    
    bool SubtractionOveflowed(const BinaryOperator* expr, __int64 result, 
                              EvaluationResult& left, EvaluationResult& right);
    
    bool ShiftLeftOveflowed(const BinaryOperator* expr, __int64 result, 
                            EvaluationResult& left, EvaluationResult& right);

    bool MultiplicationOveflowed(const BinaryOperator* expr, __int64 result, 
                                 EvaluationResult& left, EvaluationResult& right);

public:
	// 'warn': 'true' if warnings should be emitted.
	// 'expectICE': 'true' if the expression should be evaluated as an ICE.
	// 'markConst': 'true' if the involved expressions should be marked as constant.
	ConstantEvaluator(const Expression* expr, bool expectICE, const Context* context, 
					  bool allowVars = true, bool warn = true, bool ice = false, 
					  bool markConst = false);
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the results of the evaluation. 
	// Returns 'false' if the expression is not constant.
	// 'hasVariable' is set to 'true' if the expression references at least one variable.
	// Note that 'hasSideEffects' can be set even if 'hasVariable' is 'false'
	// (if the expression contains a function call, for example).
	bool GetResult(EvaluationResult& result, bool& hasSideffects, bool& hasVariable);
};

} // namespace AST
#endif