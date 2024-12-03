// StatementGenerator.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StatementGenerator.hpp"
#include "ExpressionGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

StatementGenerator::StatementGenerator(FunctionGenerator* functGen) :
		functGen_(functGen), 
        irGen_(functGen_->GetIRGen()), 
		typeGen_(functGen_->GetTypeGen()), 
        sawTerminator_(false),
		namedTemp_(functGen->GetContext()->Options().ShouldNameTemporaries()) {
	DebugValidator::IsNotNull(functGen);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Generate(const Statement* statement) {
	statement->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IR::Type* StatementGenerator::GetIRType(const Type* type) {
	return typeGen_->GetType(type, functGen_->GetIRFunction());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::IsAlwaysTrue(const Expression* expr) {
	// An expression is always true if it's result is a constant != 0,
	// or in case of an OR (||), if one of the subexpressions is true.
	EvaluationInfo eval;
	auto context = functGen_->GetContext();

	if(expr->TryEvaluateConstant(context, &eval)) {
		// The whole expression is constant, use the result.
		// (but not if it depends on a variable).
		if(eval.HasVariable() == false) {
			return eval.IntValue() != 0;
		}
	}

	if(auto binOp = expr->As<BinaryOperator>()) {
		if(binOp->Operator() == BinaryOpType::OrOr) {
			// TRUE || E = TRUE
			if(IsAlwaysTrue(binOp->LeftValue())) {
                return true;
            }
		}
		else if(binOp->Operator() == BinaryOpType::AndAnd) {
			// FALSE && E = FALSE
			if(IsAlwaysFalse(binOp->LeftValue())) {
                return false;
            }
		}
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::IsAlwaysFalse(const Expression* expr) {
	// An expression is always false if it's result is a constant == 0,
	// or in case of an AND (&&), if one of the subexpressions if false.
	EvaluationInfo eval;
	auto context = functGen_->GetContext();

	if(expr->TryEvaluateConstant(context, &eval)) {
		// The whole expression is constant, use the result
		// (but not if it depends on a variable).
		if(eval.HasVariable() == false) {
			return eval.IntValue() == 0;
		}
	}

	if(auto binOp = expr->As<BinaryOperator>()) {
		if(binOp->Operator() == BinaryOpType::OrOr) {
			// TRUE || E = TRUE
			if(IsAlwaysTrue(binOp->LeftValue())) {
                return false;
            }
		}
		else if(binOp->Operator() == BinaryOpType::AndAnd) {
			// FALSE && E = FALSE
			if(IsAlwaysFalse(binOp->LeftValue())) {
                return true;
            }
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::IsLogicalExpression(const Expression* expr) {
	// && and || are the only logical expressions.
	// ! is also considered logical in cases like '!(a && b)'.
	if(auto binaryOp = expr->As<BinaryOperator>()) {
		return binaryOp->IsLogical();
	}
	else if(auto unaryOp = expr->As<UnaryOperator>()) {
		if(unaryOp->Operator() == UnaryOpType::Not) {
			if(auto otherBinaryOp = unaryOp->Value()->As<BinaryOperator>()) {
				return otherBinaryOp->IsLogical();
			}
		}
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::IsEmptyStatement(const Statement* statement) {
	// A 'NullStatement' and a 'CompoundStatement' without children have no effect.
	// Note that a statement that has a label is not considered 'empty' 
    // even if it qualifies, because there may be a 'goto' to it.
	if(statement->IsNullStatement()) {
        return true;
    }

	if(auto compoundStatement = statement->As<CompoundStatement>()) {
		if(compoundStatement->Children().Count() == 0) {
			// {}, the statement is definitely empty.
			return true;
		}

		// It's possible that even with children the statement
        // to be empty ('{{} {}}').
		auto& children = compoundStatement->Children();

		for(int i = 0; i < children.Count(); i++) {
			if(IsEmptyStatement(children[i]) == false) {
				// At least one child is not empty, so the parent isn't too.
				return false;
			}
		}

		// All children are empty, so the parent is empty too.
		return true;
	}
	
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::HasLabel(const Statement* statement) {
	// This method is called if an opportunity to not generate
    // a statement appears. The statement still needs to be 
    // generated if it contains a 'label'. It would be better 
    // to check if there is a 'goto' to the found label, 
    // but the code optimizer should catch this case easily. 
	if(statement->IsLabelStatement()) {
        return true;
    }
	else if(auto compoundStatement = statement->As<CompoundStatement>()) {
		auto& children = compoundStatement->Children();

		for(int i = 0; i < children.Count(); i++) {
			if(HasLabel(children[i])) {
                return true;
            }
		}
	}
	else if(auto ifStatement = statement->As<IfStatement>()) {
		if(HasLabel(ifStatement->True())) {
            return true;
        }
		else if(ifStatement->False()) { // false branch can be missing.
			if(HasLabel(ifStatement->False())) {
                return true;
            }
		}
	}
	else if(auto forStatement = statement->As<ForStatement>()) {
		return HasLabel(forStatement->Body());
	}
	else if(auto whileStatement = statement->As<WhileStatement>()) {
		return HasLabel(whileStatement->Body());
	}
	else if(auto doStatement = statement->As<DoStatement>()) {
		return HasLabel(doStatement->Body());
	}
	else if(auto switchStatement = statement->As<SwitchStatement>()) {
		return HasLabel(switchStatement->Body());
	}
	else if(auto caseStatement = statement->As<CaseStatement>()) {
		return HasLabel(caseStatement->Target());
	}
	
	// All other statement's can't have children (and labels).
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::ChildWithLabel(const List<shared<Statement>>& childList, 
										int startIndex) {
	for(int i = startIndex; i < childList.Count(); i++) {
		if(HasLabel(childList[i])) {
            return true;
        }
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::InSwitch() {
	// We're in a switch if the 'LoopInfo' object has no header block set.
	// Note that this returns 'false' if we're in a loop inside a 'switch'.
	return (functGen_->LoopDepth() > 0) &&
		   (functGen_->CurrentLoop().HeaderBlock == nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Block* StatementGenerator::CreateBlock(const Statement* statement, 
                                           const string& name) {
	if(auto labelStatement = statement->As<LabelStatement>()) {
		// It's possible that the block for the labeled statement 
        // is already created (because of a forward referencing 'goto').
		IR::Block* block = functGen_->GetLabeledBlock(labelStatement);

		if(block) {
            return block;
        }

		// The block was not created yet; after we create 
        // it it must be added to the map. Use the name of the label 
        // instead of the specified one.
		block = functGen_->CreateBlock(labelStatement->Name()->Name());
		functGen_->AddLabeledBlock(labelStatement, block);
		return block;
	}

	return functGen_->CreateBlock(name);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Block* StatementGenerator::CreateBlock(const Statement* statement, BlockName name) {
    return CreateBlock(statement, functGen_->GetNameGen()->GetBlockName(name));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Block* StatementGenerator::CreateBlock(BlockName name) {
    return functGen_->CreateBlock(functGen_->GetNameGen()->GetBlockName(name));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateBlock(IR::Block* Block, 
                                       const Statement* statement, bool insert) {
	if(insert) {
		functGen_->InsertAndMakeActive(Block);
	}

	functGen_->PushParentStatement(statement);
	Generate(statement);
	functGen_->PopParentStatement();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::LinkWithContinuation(IR::Block* block, 
                                              IR::Block* contBlock,
											  IR::GotoOrigin origin) {
	// If the block ends with a branching instruction we don't need
	// (and actually are not allowed) to emit the 'goto' 
    // to the continuation block.
	IR::Instruction* lastInstr = block->LastInstruction();

	if(lastInstr && lastInstr->IsBranching()) {
		return; // No 'goto' needed.
	}
	else {
		auto instr = irGen_->GetGoto(contBlock, block);
		instr->SetGotoOrigin(origin);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::CheckIfOptimization(const IfStatement* statement, 
											 bool& skipCondition, bool& skipTrue, 
											 bool& skipFalse, bool& isFalse) {
	if(IsAlwaysTrue(statement->Condition())) {
		// If 'E1' is always 'true':
		// 'if(E1) E2 else E3' -> 'E2'.
		// 'if(E1) {} else E3' -> nothing.
		isFalse = false;
		skipCondition = true;
		skipFalse = true;

		if(IsEmptyStatement(statement->True())) {
			// Second situation.
			skipTrue = true;
		}
	}
	else if(IsAlwaysFalse(statement->Condition())) {
		// If 'E1' is always 'false':
		// 'if(E1) E2 else E3' -> 'E3', if 'E2' has no labels.
		// 'if(E1) E2' -> nothing, if 'E2' has no labels.
		isFalse = true;
		skipCondition = true;
		skipTrue = true;

		if(statement->False() && IsEmptyStatement(statement->False())) {
			// Second situation.
			skipFalse = true;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateUnconditionalBlock(IR::Block* block, 
                                                    const Statement* statement, 
													IR::Block* prevBlock, 
                                                    IR::Block* nextBlock) {
	DebugValidator::IsNotNull(block);
	DebugValidator::IsNotNull(statement);
	
	// Connect the block to the previous one using a 'goto'.
	if(prevBlock) {
		LinkWithContinuation(prevBlock, block);
	}

	GenerateBlock(block, statement);

	// Connect the block to the next one using a 'goto'.
	if(nextBlock) {
		// Use 'ActiveBlock()' because more blocks
        // could have been added after 'block'.
		LinkWithContinuation(functGen_->ActiveBlock(), nextBlock);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const IfStatement* statement) {
	auto condition = statement->Condition();
	auto trueStatement = statement->True(); // Always available.
	auto falseStatement = statement->False();
	bool skipTrue = false;
	bool skipFalse = false;
	bool skipCondition = false;
	bool genTrue = true;
	bool genFalse = true;
	bool isFalse;

	// OPTIMIZATION:
	// If we can prove that the condition is always 'true'
    // or 'false' we can get away without generating one 
    // of the branches (note that if we have 'if(E1) E2, E3'
	// and 'E1' is always 'false' we don't need to generate the 'if' at all).
	CheckIfOptimization(statement, skipCondition, skipTrue, skipFalse, isFalse);

	// The condition will be generated in the current block. 
	// A block for for the 'true' case is created 
    // (note that if the 'true' statement is a 'NullStatement
    // ' we don't create a block, we jump directly to the continuation).
	// A block for the 'false' case is created, if needed.
	// A continuation block is created only if there 
    // is a 'true' or a 'false' block.
	if(skipTrue) {
		// The 'true' part is unnecessary, but we still must
        // generate it if there is a label inside it (at any level).
		genTrue = HasLabel(trueStatement);
	}

	if(falseStatement == nullptr) {
        genFalse = false;
    }
	else if(skipFalse) {
		// The 'false' part is unnecessary, but we still must
        // generate it if there is a label inside it (at any level).
		genFalse = HasLabel(falseStatement);
	}

	// Create the blocks that are needed.
	IR::Block* trueBlock = nullptr;
	IR::Block* falseBlock = nullptr;
	IR::Block* contBlock = nullptr;

	if(genTrue) {
        trueBlock = CreateBlock(trueStatement, BlockName::IfTrue);
    }

	if(genFalse) {
        falseBlock = CreateBlock(falseStatement, BlockName::IfFalse);
    }

	if(genTrue || genFalse) {
		// A continuation block is needed.
		contBlock = CreateBlock(BlockName::IfCont);
	}
	else return; // The 'if' body is not generated.

	// Generate the code for the condition.
	auto prevBlock = functGen_->ActiveBlock();

	if(skipCondition) {
		GenerateUnconditionalIf(trueBlock, trueStatement, 
                                falseBlock, falseStatement,
								prevBlock, contBlock, isFalse);
	}
	else GenerateConditionalIf(trueBlock, trueStatement, 
                               falseBlock, falseStatement, 
                               condition, prevBlock, 
                               contBlock, isFalse);

	// Make the continuation block the active one.
	functGen_->InsertAndMakeActive(contBlock);

    // A terminator in an 'if' doesn't affect the rest.
	sawTerminator_ = false; 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateIf(const Expression* expr, 
                                    IR::Block* trueBlock, 
                                    IR::Block* falseBlock) {
	if(IsLogicalExpression(expr)) {
		GenerateIfOnLogical(expr, trueBlock, falseBlock);
	}
	else GenerateIfOnExpression(expr, trueBlock, falseBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateIfOnLogical(const Expression* expr, 
                                             IR::Block* trueBlock, 
											 IR::Block* falseBlock) {
	// Check for '!(E1 && E2)' or '!(E1 || E2)'.
	if(auto unaryOp = expr->As<UnaryOperator>()) {
		DebugValidator::IsTrue(unaryOp->Operator() == UnaryOpType::Not);

		// !E     'E' TRUE  -> 'falseBlock'
		//        'E' FALSE -> 'trueBlock'
		GenerateIfOnLogical(unaryOp->Value(), falseBlock, 
                            trueBlock);
		return;
	}

	// It's a logical expression (&&, ||).
	auto logicalExpr = expr->As<BinaryOperator>();
	DebugValidator::IsTrue(logicalExpr && logicalExpr->IsLogical());

	auto leftValue = logicalExpr->LeftValue();
	auto rightValue = logicalExpr->RightValue();

	// OPTIMIZATION:
	// Try to remove unnecessarily cast expressions.
	ExpressionGenerator(functGen_).SimplifyBinaryOpCasts(leftValue, rightValue);

	if(logicalExpr->Operator() == BinaryOpType::AndAnd) {
		// 'E1 && E2' - 'E1' evaluated in the current block, 'E2' in 'andRightBlock'
		// 'E1' TRUE  -> 'andRightBlock'
		// 'E1' FALSE -> 'falseBlock'
		// 'E2' TRUE  -> 'trueBlock'
		// 'E2' FALSE -> 'falseBlock'
		auto andRightBlock = CreateBlock(BlockName::AndRight);
		GenerateIf(leftValue, andRightBlock, falseBlock);

		functGen_->InsertAndMakeActive(andRightBlock);
		GenerateIf(rightValue, trueBlock, falseBlock);
	}
	else {
		// 'E1 || E2' - 'E1' evaluated in the current block, 'E2' in 'orRightBlock'.
		// 'E1' TRUE  -> 'trueBlock'
		// 'E1' FALSE -> 'orRightBlock'
		// 'E2' TRUE  -> 'trueBlock'
		// 'E3' FALSE -> 'falseBlock'
		auto orRightBlock = CreateBlock(BlockName::OrRight);
		GenerateIf(leftValue, trueBlock, orRightBlock);

		functGen_->InsertAndMakeActive(orRightBlock);
		GenerateIf(rightValue, trueBlock, falseBlock);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateIfOnExpression(const Expression* expr, 
                                                IR::Block* trueBlock, 
												IR::Block* falseBlock) {
	// Suppose the result is found in 't1'; then we generate:
	// 't2 = cmp t1, 0     if t2, condFalseBlock, condTrueBlock'

	// OPTIMIZATION:
	// If the expression is a constant we jump directly
    // to the corresponding block.
	if(IsAlwaysTrue(expr)) {
		irGen_->GetGoto(trueBlock, functGen_->ActiveBlock());
		return;
	}
	else if(IsAlwaysFalse(expr)) {
		irGen_->GetGoto(falseBlock, functGen_->ActiveBlock());
		return;
	}

	// Jump based on the comparison result.
	auto compResultOp = GenerateIfComparison(expr);
	auto condTrueRef = irGen_->GetBlockRef(trueBlock);
	auto condFalseRef = irGen_->GetBlockRef(falseBlock);
	auto ifInstr = irGen_->GetIf(compResultOp, condTrueRef, 
                                 condFalseRef, 
                                 functGen_->ActiveBlock());
    functGen_->AfterIf(ifInstr, trueBlock, falseBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* StatementGenerator::GenerateIfComparison(const Expression* expr) {
	// The expression can have integer, floating or pointer type.
	auto exprType = expr->ResultType()->WithoutQualifiers();
	auto irExprType = GetIRType(exprType);

	// Mark the result as having boolean type.
    // This can improve some optimizations.
	auto compResultOp = irGen_->GetTemporary(irGen_->GetInt32());
	compResultOp->SetIsBoolean(true);

	// Generate the code for the expression and take it's result.
	auto conditionOp = ExpressionGenerator(functGen_).Generate(expr);

    // OPTIMIZATION:
    // Note that a comparison is not needed if the generated 
    // expression ends with a comparison.
    if(auto definingInstr = conditionOp->DefiningInstruction()) {
        if(definingInstr->IsComparison()) {
            return conditionOp;
        }
    }

    if(exprType->IsInteger()) {
		auto zeroConst = irGen_->GetIntConst(irExprType, 0);
		irGen_->GetCmp(IR::OrderType::NotEqual, conditionOp, zeroConst, 
                       compResultOp, functGen_->ActiveBlock());
	}
	else if(exprType->IsFloating()) {
		auto zeroConst = irGen_->GetFloatingConst(irExprType, 0.0);
		irGen_->GetFcmp(IR::OrderType::NotEqual, conditionOp, zeroConst, 
                        compResultOp, functGen_->ActiveBlock());
	}
	else {
		DebugValidator::IsTrue(exprType->IsPointer());
		auto nullConst = irGen_->GetNullConst(irExprType);
		irGen_->GetUcmp(IR::OrderType::NotEqual, conditionOp, nullConst, 
                        compResultOp, functGen_->ActiveBlock());
	}
    
    functGen_->BeforeIf(conditionOp);
	return compResultOp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateConditionalIf(IR::Block* trueBlock, 
						 const Statement* trueStatement, IR::Block* falseBlock, 
						 const Statement* falseStatement, const Expression* condition, 
						 IR::Block* prevBlock, IR::Block* contBlock, bool isFalse) {
	IR::IRGenerator irGen(functGen_->GetIRUnit());

	// 'if(E1) E2 else E3, E4'
	// 'E1' TRUE  -> 'E2' if available, else 'E4'
	// 'E1' FALSE -> 'E3' if available, else 'E4'
	IR::Block* condTrueBlock = trueBlock ? trueBlock : contBlock;
	IR::Block* condFalseBlock = falseBlock ? falseBlock : contBlock;
	GenerateIf(condition, condTrueBlock, condFalseBlock);

	// Generate code for the branches and connect them 
    // to the continuation block. We use 'ActiveBlock()' 
    // because more than one blocks could have been added.
	if(trueBlock) {
		GenerateBlock(trueBlock, trueStatement);
		LinkWithContinuation(functGen_->ActiveBlock(), 
                             contBlock);
	}

	if(falseBlock) {
		GenerateBlock(falseBlock, falseStatement);
		LinkWithContinuation(functGen_->ActiveBlock(), 
                             contBlock);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateUnconditionalIf(IR::Block* trueBlock, 
						 const Statement* trueStatement, IR::Block* falseBlock, 
						 const Statement* falseStatement, IR::Block* prevBlock, 
                         IR::Block* contBlock, bool isFalse) {
	// No condition is needed, use an unconditional branch 
    // ('goto') to connect the current block to the target. 
    // Note that only one block is the target, the other one 
    // is generated because it contains labels (if it's the case).
	if(trueBlock) {
		IR::Block* parentBlock = isFalse == false ? prevBlock : nullptr;
		GenerateUnconditionalBlock(trueBlock, trueStatement, 
                                   parentBlock, contBlock);
	}

	if(falseBlock) {
		IR::Block* parentBlock = isFalse ? prevBlock : nullptr;
		GenerateUnconditionalBlock(falseBlock, falseStatement, 
                                   parentBlock, contBlock);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const DeclarationStatement* statement) {
	if(auto declList = statement->Base()->As<DeclarationList>()) {
		// A declaration statement (like 'int a = 5, b, c;').
		// Add all declared variable to the current block.
		auto declarations = declList->Declarations();

		for(int i = 0; i < declarations.Count(); i++) {
			GenerateDeclaration(declarations[i]);
		}
	}
	else {
		// A single declaration.
		GenerateDeclaration(statement->Base());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateDeclaration(const Declaration* declaration) {
	auto block = functGen_->ActiveBlock();

	if(auto variableDecl = declaration->As<VariableDeclaration>()) {
		if(variableDecl->DeclarationType()->WithoutQualifiers()->IsVariable()) {
			// Variables that have VLA type need special treatment.
			functGen_->AddVLAVariable(variableDecl);
		}
		else functGen_->AddVariable(variableDecl, block);
	}
	else if(auto typedefDecl = declaration->As<TypedefDeclaration>()) {
		// 'typedef's that have VLA type the only one handled.
		if(typedefDecl->DeclarationType()->WithoutQualifiers()->IsVariable()) {
			functGen_->GenerateVLATypedef(typedefDecl);
		}
	}
	else {
		// Function declarations inside a function are ignored 
        // (a global declaration for them has already been created 
        // by the semantic analysis module).
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const ExpressionStatement* statement) {
	// Generate the expression in the current block.
	ExpressionGenerator exprGen(functGen_);
	exprGen.Generate(statement->Base());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const CompoundStatement* statement) {
	functGen_->PushParentStatement(statement);
	auto& children = statement->Children();

	for(int i = 0; i < children.Count(); i++) {
		Generate(children[i]);

		// If the child was a 'break', 'continue' or 'return' 
        // there is no point in generating the rest of the children, 
        // except when they contain a label.
		if(sawTerminator_) {
			sawTerminator_ = false;
			
			if(ChildWithLabel(children, i + 1) == false) {
				functGen_->PopParentStatement();
				return;
			}
			else {
				// At least one of the children contains a label.
				// A new block needs to be created, in which we generate the rest.
				IR::Block* contBlock = CreateBlock(BlockName::TermCont);
				functGen_->InsertAndMakeActive(contBlock);
			}
		}
	}

	functGen_->PopParentStatement();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const NullStatement* statement) {
	// We do nothing for empty (null) statements.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const ReturnStatement* statement) {
	// If we are inside inside a 'switch' statement
    // we don't set the flag (else the rest of the 'case' 
    // statements would not be generated). Note that this 
    // doesn't apply if we're in a loop that is inside a 'switch'
    // (we want to stop generating the rest of the loop).
	sawTerminator_ = InSwitch() == false;

    // There may be VLAs allocated; restore the stack now, else the stack
    // restoration code will be added after the 'ret' instruction.
    functGen_->RestoreVLAStack(true /* popAllArrays */);

	// If the function returns a simple type then we 
    // just generate the expression, then a 'ret' 
    // instruction with the result of the expression.
	// For 'record' return types the process is more involved.
    auto functionType = functGen_->GetFunction()->DeclarationType();
	auto returnType = functionType->ReturnType()->WithoutQualifiers();
	auto irReturnType = functGen_->GetIRFunction()->ReturnType();

	if(returnType->IsRecord()) {
		GenerateRecordReturn(statement);
		return;
	}
	
	if(irReturnType->IsVoid()) {
		// 'void' is another special case.
		irGen_->GetVoidReturn(functGen_->ActiveBlock());
	}
	else {
		auto resultOp = ExpressionGenerator(functGen_).
                            Generate(statement->Value());
		irGen_->GetReturn(resultOp, functGen_->ActiveBlock());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateRecordReturn(const ReturnStatement* statement) {
	// A returned 'record' is copied to the last parameter 
    // of the function, which is a 'pointer to record' 
    // (found in the caller).
	auto& functParams = functGen_->GetIRFunction()->Parameters();
	auto returnVar = functParams[functParams.Count() - 1];
	auto returnVariableRef = functGen_->GetVariableReference(returnVar);

	// Load the pointer, generate the expression to be returned, 
    // then make the copy of the record.
	ExpressionGenerator exprGen(functGen_);
	auto returnOp = irGen_->GetTemporary(returnVar->GetType());
	irGen_->GetLoad(returnVariableRef, returnOp, 
                    functGen_->ActiveBlock());

	auto valueOp = exprGen.Generate(statement->Value());
	exprGen.GenerateRecordAssignment(returnOp, valueOp, 
                                     statement->Value()->ResultType());
	irGen_->GetVoidReturn(functGen_->ActiveBlock());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const LabelStatement* statement) {
	// If the block for the labeled statement was not generated yet
	// (by a forward referencing 'goto'), do it now.
	IR::Block* labeledBlock = functGen_->GetLabeledBlock(statement);

	if(labeledBlock == nullptr) {
		labeledBlock = functGen_->CreateBlock(statement->Name()->Name());
	}

	// Terminate the current block by inserting a 'goto' 
    // to the labeled block. Then make the labeled block 
    // the active one and generate the target in it.
	LinkWithContinuation(functGen_->ActiveBlock(), labeledBlock);
	functGen_->AddLabeledBlock(statement, labeledBlock);
	GenerateBlock(labeledBlock, statement->Target());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const SwitchStatement* statement) {
	// Generate code in the current block, then generate
    // the body statement. The block for each 'case' target 
    // will be added to 'caseBlocks'.
	IR::Block* parentBlock = functGen_->ActiveBlock();
	auto conditionOp = ExpressionGenerator(functGen_).Generate(statement->Condition());
	auto switchInstr = irGen_->GetSwitch(conditionOp, statement->CaseList().Count(), 
										 nullptr, parentBlock);

	// If the first statement in the 'switch' is not a 'case'
    // then we need to generate another block, in which it can 
    // be generated (we can't skip it because there may be a 'goto' to it).
	if(auto compoundStatement = statement->Body()->As<CompoundStatement>()) {
		if((compoundStatement->Children().Count() > 0) &&
		   (compoundStatement->Children()[0]->IsCaseStatement() == false)) {
			IR::Block* block = CreateBlock(BlockName::Block);
			functGen_->InsertAndMakeActive(block);
		}
	}

	// Create a continuation block. We push a 'LoopInfo' object, 
    // even if this is not a loop, because a 'break' found in a 
    // 'case' target jumps outside the 'switch' (no 'break' means 
    // that control falls through to the next 'case').
	IR::Block* contBlock = CreateBlock(BlockName::SwitchCont);

	functGen_->PushLoopInfo(LoopInfo(nullptr, contBlock, statement->Body()));
	GenerateBlock(parentBlock, statement->Body(), 
                  false /* insert block */);
	functGen_->PopLoopInfo(); // Not needed anymore.

	// Create the list with the case statements.
	auto context = functGen_->GetContext();
	auto& cases = statement->CaseList();
	IR::BlockReference* defaultBlockRef = nullptr;
	
	for(int i = 0; i < cases.Count(); i++) {
		CaseStatement* caseStatement = cases[i];
		IR::Block* caseBlock = functGen_->GetCaseBlock(caseStatement);
		auto caseBlockRef = irGen_->GetBlockRef(caseBlock);

		// Check if this is a 'default' statement.
		if(caseStatement->IsDefault()) {
			defaultBlockRef = caseBlockRef;
		}
		else {
			// Evaluate the value expression (it is an ICE).
			EvaluationInfo eval = caseStatement->Value()->EvaluateAsICE(context, 
                                                                   false /* warn */);
			switchInstr->AddCase(eval.IntValue(), caseBlockRef);
		}
	}

	// If there was a 'default' statement it's target block
    // is the one used if no 'case' value matches. 
    // Else use the continuation block.
	if(defaultBlockRef == nullptr) {
		defaultBlockRef = irGen_->GetBlockRef(contBlock);
	}

	switchInstr->SetDefaultTargetOp(defaultBlockRef);

	// Connect the last 'case' target with the continuation block.
	LinkWithContinuation(functGen_->ActiveBlock(), contBlock);
	functGen_->InsertAndMakeActive(contBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const CaseStatement* statement) {
	// End the current block by inserting a jump to the one
    // in which we will generate the target of the 'case' statement.
	IR::Block* caseBlock = CreateBlock(BlockName::SwitchCase);
	functGen_->AddCaseBlock(statement, caseBlock);
	LinkWithContinuation(functGen_->ActiveBlock(), caseBlock);
	GenerateBlock(caseBlock, statement->Target());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const GotoStatement* statement) {
    // Generate stack restoration code for all created VLAs.
    functGen_->RestoreVLAStack();

	// Check if the target block has been already generated.
	// If not we generate it now.
	auto labelStatement = statement->Target()->As<LabelStatement>();
	IR::Block* targetBlock = functGen_->GetLabeledBlock(labelStatement);

	if(targetBlock == nullptr) {
		targetBlock = CreateBlock(BlockName::GotoDest);
		functGen_->AddLabeledBlock(labelStatement, targetBlock);
	}

	// End the current block by generating a 'goto' to the target block.
	LinkWithContinuation(functGen_->ActiveBlock(), targetBlock,
                         IR::GotoOrigin::Goto);

	// If we are inside inside a 'switch' statement we don't set the flag
	// (else the rest of the 'case' statements would not be generated).
	sawTerminator_ = InSwitch() == false;
}

} // namespace IRGenerator