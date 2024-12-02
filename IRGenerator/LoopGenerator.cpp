// LoopGen.cpp
// Copyright (c) Lup Gratian
//
// Implements the code for generating loop-related statements.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StatementGenerator.hpp"
#include "ExpressionGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const WhileStatement* statement) {
    functGen_->BeforeLoop(LoopKind::While);

	// OPTIMIZATION:
	// First test for cases in which optimizations can be applied.
	if(GenerateWhileOptimized(statement)) {
        return;
    }

	// Create a block (header) where the code for the condition is generated, 
	// and terminate the current block by jumping to it.
	// The continuation block is also created now.
	IR::Block* headerBlock = functGen_->CreateBlock(BlockName::WhileHeader, true);
	IR::Block* bodyBlock = functGen_->CreateBlock(BlockName::WhileBody, true);
	IR::Block* contBlock = functGen_->CreateBlock(BlockName::WhileCont);
    functGen_->PushLoopInfo(LoopInfo(headerBlock, contBlock, statement->Body()));

	// Generate the expression, which jumps to the body if 'true' 
	// and to the continuation otherwise.
	LinkWithContinuation(functGen_->ActiveBlock(), headerBlock);
	functGen_->InsertAndMakeActive(headerBlock);
	GenerateIf(statement->Condition(), bodyBlock, contBlock);

	// Make the body block active and generate the body.
	GenerateBlock(bodyBlock, statement->Body());

	// End the loop by generating a 'goto' to the header.
	LinkWithContinuation(functGen_->ActiveBlock(), headerBlock, 
                         IR::GotoOrigin::Loop);
	functGen_->PopLoopInfo(); // The loop is no longer valid.
	functGen_->InsertAndMakeActive(contBlock);
    functGen_->AfterLoop(LoopKind::While, headerBlock, bodyBlock, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::GenerateWhileOptimized(const WhileStatement* statement) {
	// While loops that have a constant condition 
    // expression can be sometimes simplified:
	// 'while(TRUE)', 'while(FALSE)'
	EvaluationInfo eval;

	if(IsAlwaysTrue(statement->Condition())) {
		// This is an infinite loop. 
        // Don't generate it if the body is empty.
		if(IsEmptyStatement(statement->Body()) == false) {
			GenerateInfiniteLoop(statement->Body());
		}

		return true;
	}
	else if(IsAlwaysFalse(statement->Condition())) {
		// The loop is dead. We still need to generate it 
        // if the body contains labels.
		return HasLabel(statement->Body()) == false;
	}
	
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateInfiniteLoop(const Statement* body) {
	// End the current block and generate a block that branches to itself.
	// Note that we still need to generate a continuation block because 
	// the loop may contain a 'break' (it's a pretty common pattern).
	IR::Block* bodyBlock = functGen_->CreateBlock(BlockName::InfLoop, true);
	IR::Block* contBlock = functGen_->CreateBlock(BlockName::InfCont);
	LinkWithContinuation(functGen_->ActiveBlock(), bodyBlock);
	
	functGen_->PushLoopInfo(LoopInfo(bodyBlock, contBlock, body));
	GenerateBlock(bodyBlock, body);
	functGen_->PopLoopInfo(); // Loop no longer valid.

	LinkWithContinuation(functGen_->ActiveBlock(), bodyBlock, 
                         IR::GotoOrigin::Loop);
	functGen_->InsertAndMakeActive(contBlock);

    functGen_->AfterLoop(LoopKind::Infinite, nullptr, bodyBlock, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const DoStatement* statement) {
	// OPTIMIZATION:
	// First test for cases in which optimizations can be applied.
	if(GenerateDoOptimized(statement)) return;

	// Create a block for the body, a footer and a continuation block.
	// The condition will be generated in the footer.
	IR::Block* bodyBlock = functGen_->CreateBlock(BlockName::DoBody, true);
	IR::Block* footerBlock = functGen_->CreateBlock(BlockName::DoFoo, true);
	IR::Block* contBlock = functGen_->CreateBlock(BlockName::DoCont);
	functGen_->PushLoopInfo(LoopInfo(footerBlock, contBlock, statement->Body()));

	// Make the body block active and generate the body.
	LinkWithContinuation(functGen_->ActiveBlock(), bodyBlock);
	GenerateBlock(bodyBlock, statement->Body());

	// Generate the expression, which jumps to the body if 'true' 
	// and to the continuation otherwise.
	LinkWithContinuation(functGen_->ActiveBlock(), footerBlock);
	functGen_->InsertAndMakeActive(footerBlock);
	GenerateIf(statement->Condition(), bodyBlock, contBlock);

	// End the loop by generating a 'goto' to the header.
	functGen_->PopLoopInfo(); // The loop is no longer valid.
	functGen_->InsertAndMakeActive(contBlock);

    functGen_->AfterLoop(LoopKind::Do, footerBlock, bodyBlock, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::GenerateDoOptimized(const DoStatement* statement) {
	// We can optimize the 'do' if the condition expression is a constant.
	// 'do { E1 } while(TRUE);'  -> infinite loop
	// 'do { E1 } while(FALSE);' -> no loop at all, only 'E1' remains.
	EvaluationInfo eval;

	if(IsAlwaysTrue(statement->Condition())) {
		// If the body is empty nothing needs to be generated.
		if(IsEmptyStatement(statement->Body()) == false) {
			GenerateInfiniteLoop(statement->Body());
		}

		return true;
	}
	else if(IsAlwaysFalse(statement->Condition())) {
		if(IsEmptyStatement(statement->Body())) {
			// 'do {} while(FALSE);' is a dead loop.
			return true;
		}

		// Create a body for the block and for the continuation.
		// We still need to create them because the body is executed
        // one time even if the condition is always false.
		IR::Block* bodyBlock = functGen_->CreateBlock(BlockName::DoBody, true);
		IR::Block* contBlock = functGen_->CreateBlock(BlockName::DoCont);
		functGen_->PushLoopInfo(LoopInfo(bodyBlock, contBlock, statement->Body()));

		LinkWithContinuation(functGen_->ActiveBlock(), bodyBlock,
                             IR::GotoOrigin::Loop);
		GenerateBlock(bodyBlock, statement->Body());

		LinkWithContinuation(functGen_->ActiveBlock(), contBlock);
		functGen_->InsertAndMakeActive(contBlock);
		functGen_->PopLoopInfo();
        functGen_->AfterLoop(LoopKind::Do, nullptr, bodyBlock, nullptr);
        return true;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const BreakStatement* statement) {
    // Generate stack restoration code for all VLAs created
    // inside the loop, but without popping them from the stack.
    functGen_->RestoreVLAStack(false /* popAllArrays */, 
                               InSwitch() == false /* popLoopArrays */);

	// Jump to the continuation of the last loop.
	LoopInfo loop = functGen_->CurrentLoop();
	auto instr = irGen_->GetGoto(loop.ContinuationBlock, 
                                 functGen_->ActiveBlock());
	instr->SetGotoOrigin(IR::GotoOrigin::Break);

	// If we are inside inside a 'switch' statement 
    // we don't set the flag (else the rest of the 'case'
    // statements would not be generated). Note that this 
    // doesn't apply if we're in a loop, which is inside a 'switch'.
	sawTerminator_ = InSwitch() == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const ContinueStatement* statement) {
    // Generate stack restoration code for all VLAs created
    // inside the loop, but without popping them from the stack.
    functGen_->RestoreVLAStack(false /* popAllArrays */, true /* popLoopArrays */);

	// Jump to the header of the last loop.
	LoopInfo loop = functGen_->CurrentLoop();
	auto instr = irGen_->GetGoto(loop.HeaderBlock, functGen_->ActiveBlock());
	instr->SetGotoOrigin(IR::GotoOrigin::Continue);
	sawTerminator_ = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::Visit(const ForStatement* statement) {
	// OPTIMIZATION:
	// First try to optimize some cases that are pretty common.
	if(GenerateForOptimized(statement)) {
        return;
    }

	// 'for(E1; E2; E3) E4, E5'
	// 'E1' - current block          'E2' - test block
	// 'E3' - increment block        'E4' - body block
	// 'E5' - continuation block
	IR::Block* testBlock = functGen_->CreateBlock(BlockName::ForTest, true);
	IR::Block* incBlock = functGen_->CreateBlock(BlockName::ForInc, true);
	IR::Block* contBlock = functGen_->CreateBlock(BlockName::ForCont);
	IR::Block* testContBlock; // Where to jump if the test result is 'true'.

	// It's pretty common that the body doesn't exist. 
	// 'for(p = first; p->next; p = p->next);' 
    // - go to the last node of a linked list.
	IR::Block* bodyBlock = nullptr;
	bool hasBody = IsEmptyStatement(statement->Body()) == false;

	if(hasBody) {
		bodyBlock = functGen_->CreateBlock(BlockName::ForBody, true);
		testContBlock = bodyBlock;
	}
	else testContBlock = incBlock;

	// Generate the initialization statement.
	if(statement->HasInit()) Generate(statement->Init());
	LinkWithContinuation(functGen_->ActiveBlock(), testBlock);

	// Generate the test expression.
	functGen_->InsertAndMakeActive(testBlock);
	GenerateIf(statement->Condition(), testContBlock, contBlock);

	// Generate the body, if needed.
	if(hasBody) GenerateForBody(statement->Body(), bodyBlock, 
                                incBlock, contBlock);

	// Generate the increment expression and insert a 'goto' to the test block.
	functGen_->InsertAndMakeActive(incBlock);
	ExpressionGenerator(functGen_).Generate(statement->Increment());
	LinkWithContinuation(functGen_->ActiveBlock(), testBlock, 
                         IR::GotoOrigin::Loop);

	// Insert the continuation block.
	functGen_->InsertAndMakeActive(contBlock);

    functGen_->AfterLoop(LoopKind::For, testBlock, bodyBlock, incBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StatementGenerator::GenerateForOptimized(const ForStatement* statement) {
	// The following cases can be simplified:
	// 'for(E1;      ;   ) E1' -> infinite loop.
	// 'for(E1; TRUE ;   ) E3' -> infinite loop.
	// 'for(E1;      ; E2) E3' -> infinite loop, increment block needed.
	// 'for(E1; TRUE ; E2) E3' -> infinite loop, increment block needed.
	// 'for(E1; E2   ;   ) E3' -> increment block not needed.
	// 'for(E1; FALSE; E2) E3' -> generate only E1.
	if((statement->HasCondition() == false) || 
        IsAlwaysTrue(statement->Condition())) {
		if(statement->HasIncrement() == false) {
			// 'for(E1;;) E1' -> infinite loop.
			if(statement->HasInit()) {
				// 'E1' is not empty, generate it.
				Generate(statement->Init());
			}

			GenerateInfiniteLoop(statement->Body());
		}
		else GenerateForWithoutTest(statement);

		return true;
	}
	else if(IsAlwaysFalse(statement->Condition())) {
		// 'for(E1; FALSE; E2) E3' -> generate only E1.
		// We need to generate 'E1' because it may have
        // side-effects (function call, for ex.).
		if(statement->HasInit()) Generate(statement->Init());
		return true;
	}
	else if(statement->HasIncrement() == false) {
		// 'for(E1; E2;) E3' -> increment block not needed.
		GenerateForWithoutIncrement(statement);
		return true;
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateForWithoutTest(const ForStatement* statement) {
	// for(E1; ; E2) E3, E4
	// This is an infinite loop, but one that has an increment expression.
	// 'E1' is generated in the current block. Create a block for the body,
	// one for the increment expression and one for the continuation.
	IR::Block* incBlock = functGen_->CreateBlock(BlockName::ForInc, true);
	IR::Block* contBlock = functGen_->CreateBlock(BlockName::ForCont);
	IR::Block* initContBlock;
	
	// It's pretty common that the body doesn't exist.
	IR::Block* bodyBlock = nullptr;
	bool hasBody = IsEmptyStatement(statement->Body()) == false;

	if(hasBody) {
		bodyBlock = functGen_->CreateBlock(BlockName::ForBody, true);
		initContBlock = bodyBlock;
	}
	else initContBlock = incBlock;

	if(statement->HasInit()) {
		Generate(statement->Init());
	}
	
	LinkWithContinuation(functGen_->ActiveBlock(), initContBlock);

	// Generate the body, if needed.
	if(hasBody) GenerateForBody(statement->Body(), bodyBlock, 
                                incBlock, contBlock);

	// Generate the increment expression.
	functGen_->InsertAndMakeActive(incBlock);
	ExpressionGenerator(functGen_).Generate(statement->Increment());
	LinkWithContinuation(functGen_->ActiveBlock(), initContBlock, 
                         IR::GotoOrigin::Loop);

	// Insert the continuation block (it will be dead 
    // until there is a 'break' in the body).
	functGen_->InsertAndMakeActive(contBlock);
    functGen_->AfterLoop(LoopKind::For, nullptr, bodyBlock, incBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateForWithoutIncrement(const ForStatement* statement) {
	// for(E1; E2; ) E3, E4
	// This is equivalent to a while loop.
	// 'E1' is generated in the current block, 'E2' in the test block,
	// and 'E3' (if not empty) in the body block.
	IR::Block* testBlock = functGen_->CreateBlock(BlockName::ForTest, true);
	IR::Block* contBlock = functGen_->CreateBlock(BlockName::ForCont);
	IR::Block* testContBlock;
	
	// It's pretty common that the body doesn't exist.
	IR::Block* bodyBlock = nullptr;
	bool hasBody = IsEmptyStatement(statement->Body()) == false;

	if(hasBody) {
		bodyBlock = functGen_->CreateBlock(BlockName::ForBody, true);
		testContBlock = bodyBlock;
	}
	else testContBlock = testBlock;

	// Generate the initialization statement.
	if(statement->HasInit()) Generate(statement->Init());
	LinkWithContinuation(functGen_->ActiveBlock(), testBlock);

	// Generate the test expression.
	functGen_->InsertAndMakeActive(testBlock);
	GenerateIf(statement->Condition(), testContBlock, contBlock);

	// Generate the body, if needed, then insert the continuation block.
	if(hasBody) GenerateForBody(statement->Body(), bodyBlock, 
                                testBlock, contBlock);
	functGen_->InsertAndMakeActive(contBlock);
    functGen_->AfterLoop(LoopKind::For, testBlock, bodyBlock, nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StatementGenerator::GenerateForBody(const Statement* body, IR::Block* bodyBlock,
										 IR::Block* headerBlock, IR::Block* contBlock) {
	// Insert the block and generate the body in it.
	functGen_->PushLoopInfo(LoopInfo(headerBlock, contBlock, body));
	GenerateBlock(bodyBlock, body);
	functGen_->PopLoopInfo();
	LinkWithContinuation(functGen_->ActiveBlock(), headerBlock);
}

} // IRGenerator