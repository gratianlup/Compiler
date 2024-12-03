// StatementGenerator.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_STATEMENT_GEN_HPP
#define PC_IR_GENERATOR_STATEMENT_GEN_HPP

#include "NameGenerator.hpp"
#include "TypeGenerator.hpp"
#include "../AST/Unit.hpp"
#include "../AST/Attributes.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/TypeSize.hpp"
#include "../AST/StructLayout.hpp"
#include "../IR/IRTypes.hpp"
#include "../IR/Symbols.hpp"
#include "../IR/Unit.hpp"
#include "../IR/TypeTable.hpp"
#include "../IR/Intrinsic.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Common/Context.hpp"
#include "../Common/TargetData.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
using namespace Base;
using namespace AST;
using namespace Common;

namespace IRGenerator {

// Forward declarations.
class FunctionGenerator;


class StatementGenerator : public Visitor {
private:
	FunctionGenerator* functGen_;
	TypeGenerator* typeGen_; // Helper that creates IR types.
	IR::IRGenerator* irGen_; // Helper that creates IR objects.
	bool sawTerminator_;     // return, break, continue, goto, switch.
	bool namedTemp_;         // 'true' if temporaries should be named.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the corresponding IR type of the specified type.
	const IR::Type* GetIRType(const Type* type);

	// Returns 'true' if the specified expression is && or ||.
	bool IsLogicalExpression(const Expression* expr);

	// Returns 'true' if the specified statement can be considered empty 
	// (it's a 'NullStatement' or a compound statement with empty children, like '{{}{}}').
	bool IsEmptyStatement(const Statement* statement);

	// Returns 'true' if the specified statement, or at least any of it's children,
	// has an attached label.
	bool HasLabel(const Statement* statement);

	// Returns 'true' if at least one of the children of the specified compound
	// statement, starting with 'startIndex', has a label.
	bool ChildWithLabel(const List<shared<Statement>>& childList, int startIndex);

	// Verifies if the specified 'if' statement can be optimized, in case the
	// condition expression is a compile-time constant. Sets the flags accordingly.
	void CheckIfOptimization(const IfStatement* statement, bool& skipCondition, 
							 bool& skipTrue, bool& skipFalse, bool& isFalse);

	// Creates a block for a statement, using the specified block name.
	// If the statement is labeled the name of the label is used instead.
	IR::Block* CreateBlock(const Statement* statement, const string& name);

    IR::Block* CreateBlock(const Statement* statement, BlockName name);

    IR::Block* CreateBlock(BlockName name);

	// Generates code for the statement in the specified block.
	// If 'insert' is 'true' the block is added to the function.
	void GenerateBlock(IR::Block* Block, const Statement* statement, bool insert = true);

	// Generates code for the statement in the specified block.
	// If 'prevBlock' is provided, 'prevBlock' is linked with 'block' using a 'goto'.
	// If 'nextBlock' is provided, 'block' is linked with 'nextBlock' using a 'goto'.
	void GenerateUnconditionalBlock(IR::Block* block, const Statement* statement, 
									IR::Block* prevBlock = nullptr, 
									IR::Block* nextBlock = nullptr);

	// Generates code for an 'if' statement whose condition expression
	// is a compile-time constant.
	void GenerateUnconditionalIf(IR::Block* trueBlock, const Statement* trueStatement,
								 IR::Block* falseBlock, const Statement* falseStatement,
								 IR::Block* prevBlock, IR::Block* contBlock, bool isFalse);

	// Generates code for an 'if' statement whose condition expression is not constant.
	void GenerateConditionalIf(IR::Block* trueBlock, const Statement* trueStatement,
							   IR::Block* falseBlock, const Statement* falseStatement,
							   const Expression* condition,
							   IR::Block* prevBlock, IR::Block* contBlock, bool isFalse);

	// Generates code to test a && or || expression. 
	// Also used for !, in cases like '!(E1 && E2)'.
	void GenerateIfOnLogical(const Expression* expr, IR::Block* trueBlock, 
							 IR::Block* falseBlock);

	// Generates an 'if' instruction that uses the result of the specified expression.
	void GenerateIfOnExpression(const Expression* expr, IR::Block* trueBlock, 
								IR::Block* falseBlock);

	// Generates code that compares the result of the specified expression with 0.
	// Handles a result with integer, floating or pointer type.
	IR::Operand* GenerateIfComparison(const Expression* expr);

	// Links the specified blocks using a (unconditional) 'goto' instruction.
	void LinkWithContinuation(IR::Block* block, IR::Block* contBlock,
							  IR::GotoOrigin origin = IR::GotoOrigin::Unknown);

	// Generates code that copies the returned record to the address
	// given by the last parameter of the function.
	void GenerateRecordReturn(const ReturnStatement* statement);

	// Generates a 'while' statement whose condition expression is a constant.
	// If the condition is equivalent to 'true' a infinite loop is generated.
	// Returns 'true' if the simplification could be applied.
	bool GenerateWhileOptimized(const WhileStatement* statement);

	// Generates code for an infinite loop.
	void GenerateInfiniteLoop(const Statement* body);

	// Generates a 'do' statement whose condition expression is a constant.
	// If the condition is equivalent to 'true' a infinite loop is generated.
	// Returns 'true' if the simplification could be applied.
	bool GenerateDoOptimized(const DoStatement* statement);

	// Generates a 'for' statement that has missing parts, or whose condition 
	// expression is a constant. Returns 'true' if the simplification could be applied.
	bool GenerateForOptimized(const ForStatement* statement);

	// Generates code for a 'for' statement that has no stop condition
	// expression to be tested after each iteration.
	void GenerateForWithoutTest(const ForStatement* statement);

	// Generates code for a 'for' statement that has no increment
	// expression to be tested after each iteration.
	void GenerateForWithoutIncrement(const ForStatement* statement);

	// Generates code for the body of a 'for' statement, and inserts jumps
	// to the header block and the continuation block.
	void GenerateForBody(const Statement* body, IR::Block* bodyBlock,
						 IR::Block* headerBlock, IR::Block* contBlock);

	// Generates code for the specified declaration.
	// Acts as a dispatch mechanism to the appropriate method.
	void GenerateDeclaration(const Declaration* declaration);

	// Returns 'true' if the parent of the current statement is a 'switch'.
	bool InSwitch();

	// Methods that implement the Visitor pattern.
	virtual void Visit(const IfStatement*          statement) override;
	virtual void Visit(const ForStatement*         statement) override;
	virtual void Visit(const WhileStatement*       statement) override;
	virtual void Visit(const DoStatement*          statement) override;
	virtual void Visit(const ContinueStatement*    statement) override;
	virtual void Visit(const BreakStatement*       statement) override;
	virtual void Visit(const ReturnStatement*      statement) override;
	virtual void Visit(const LabelStatement*       statement) override;
	virtual void Visit(const CaseStatement*        statement) override;
	virtual void Visit(const SwitchStatement*      statement) override;
	virtual void Visit(const GotoStatement*        statement) override;
	virtual void Visit(const CompoundStatement*    statement) override;
	virtual void Visit(const DeclarationStatement* statement) override;
	virtual void Visit(const ExpressionStatement*  statement) override;
	virtual void Visit(const NullStatement*        statement) override;

public:
	StatementGenerator(FunctionGenerator* functGen);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Generates code for the specified statement and it's children.
	void Generate(const Statement* statement);

	// Generates code that evaluates the specified expression and jumps to
	// 'trueBlocl' if the expression is equivalent to 'true', else it jumps to 'falseBlock'.
	void GenerateIf(const Expression* expr, IR::Block* trueBlock, IR::Block* falseBlock);

	// Returns 'true' if the expression evaluates to a constant not equal to 0.
	bool IsAlwaysTrue(const Expression* expr);

	// Returns 'true' if the expression evaluates to a constant equal to 0.
	bool IsAlwaysFalse(const Expression* expr);
};

} // namespace IRGenerator
#endif