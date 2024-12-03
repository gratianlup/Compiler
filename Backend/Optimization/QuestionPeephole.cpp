// ComparisonPeephole.hpp
// Copyright (c) Lup Gratian
//
// Implements the methods that handle peephole optimization for 'quest' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Peephole.hpp"

namespace Optimization {

Operand* Peephole::HandleQuestion(QuestionInstr* instr) {
    // Try to constant-fold it.
    if(auto result = folder_.Fold(instr)) {
        return result;
    }

    // quest c, a, a -> a
    if(instr->TrueOp() == instr->FalseOp()) {
        return LOG(instr->TrueOp());
    }

    // quest (quest c, 4, 5), a, b -> a
    if(auto questInstr = instr->ConditionOp()->DefiningInstrAs<QuestionInstr>()) {
        if(questInstr->TrueOp()->IsIntConstant()  &&
           questInstr->FalseOp()->IsIntConstant() &&
           ((questInstr->TrueOp()->IsZeroInt() || 
             questInstr->FalseOp()->IsZeroInt()) == false)) {
            return LOG(instr->TrueOp());
        }
    }

    if(auto condInstr = instr->ConditionOp()->DefiningInstruction()) {
        auto cmpInstr = condInstr->As<CmpInstrBase>();

        if(cmpInstr && cmpInstr->IsEquality()) {
            // quest (cmp eq a, b), a, b -> b
            // quest (cmp neq a, b), a, b -> a
            if(cmpInstr->LeftOp() == instr->TrueOp() &&
               cmpInstr->RightOp() == instr->FalseOp()) {
                return LOG(cmpInstr->IsEqual() ? instr->FalseOp() : 
                                             instr->TrueOp());
            }

            // quest (cmp eq a, b), b, a -> a
            // quest (cmp neq a, b), b, a -> b
            if(cmpInstr->LeftOp() == instr->FalseOp() &&
               cmpInstr->RightOp() == instr->TrueOp()) {
                return LOG(cmpInstr->IsEqual() ? instr->FalseOp() : 
                                                 instr->TrueOp());
            }
        }
    }

    // Check if the condition operand is a boolean.
    if(instr->ConditionOp()->IsBoolean() ||
       instr->ConditionOp()->DefiningInstrIs<CmpInstrBase>()) {
        if(auto result = HandleQuestionOnBool(instr)) {
            return LOG(result);
        }
    }

    // quest c, (ext a), (ext b) -> ext (quest c, a, b)
    if(auto convInstrA = instr->TrueOp()->DefiningInstrAs<ConversionInstr>()) {
        if(auto convInstrB = instr->FalseOp()->DefiningInstrAs<ConversionInstr>()) {
            // The opcodes should be the same.
            if(convInstrA->IsSameKind(convInstrB)) {
                auto questOp = GetTemporary(convInstrA->TargetOp());
                irGen_.GetQuestion(instr->ConditionOp(), convInstrA->TargetOp(),
                                   convInstrB->TargetOp(), questOp);

                auto conversionOp = GetTemporary(instr->TrueOp());
                irGen_.GetConversion(convInstrA->GetOpcode(), questOp,
                                     convInstrA->CastType(), conversionOp);
                return LOG(conversionOp);
            }
        }
    }

    // quest c, (load a), (load b) -> load (quest c, a, b)
    if(auto loadInstrA = instr->TrueOp()->DefiningInstrAs<LoadInstr>()) {
        if(auto loadInstrB = instr->FalseOp()->DefiningInstrAs<LoadInstr>()) {
            auto questOp = GetTemporary(loadInstrA->SourceOp());
            irGen_.GetQuestion(instr->ConditionOp(), loadInstrA->SourceOp(),
                               loadInstrB->SourceOp(), questOp);

            auto loadOp = GetTemporary(instr->TrueOp());
            irGen_.GetLoad(questOp, loadOp);
            return loadOp;
        }
    }

    // (a > -1) ? C1 : C2 -> ((a >> 31) & (C2 - C1)) + C1
    // (a <  0) ? C2 : C1 -> ((a >> 31) & (C2 - C1)) + C1
    if(auto cmpInstr = instr->ConditionOp()->DefiningInstrAs<CmpInstr>()) {
        if(auto result = ExpandQuestToLogical(instr, cmpInstr)) {
            return LOG(result);
        }
    }

    // quest (a == C) ? a : b -> (a == C) ? C : b
    // quest (a != C) ? b : a -> (a != C) ? b : C
    if(auto cmpInstr = instr->ConditionOp()->DefiningInstrAs<CmpInstrBase>()) {
        // We do this simplification only if we don't increase the register pressure.
        if(cmpInstr->IsEqual() && cmpInstr->RightOp()->IsConstant() &&
           (instr->TrueOp() == cmpInstr->LeftOp())) {
            instr->SetTrueOp(cmpInstr->RightOp());
            return nullptr;
        }
        else if(cmpInstr->IsNotEqual() && cmpInstr->RightOp()->IsConstant() &&
                (instr->FalseOp() == cmpInstr->LeftOp())) {
            instr->SetFalseOp(cmpInstr->RightOp());
            return nullptr;
        }
    }

    // quest c, (add a, b), (add a, d) -> add a, (quest c, a, d)
    if(auto result = HandleQuestionOnBinary(instr)) {
        return result;
    }

    // if(E) a++ -> a += E, if E is a comparison instruction
    // if(E) a-- -> a -= E
    if(instr->ConditionOp()->DefiningInstrIs<CmpInstrBase>()) {
        if(auto addInstr = instr->TrueOp()->DefiningInstrAs<AddInstr>()) {
            if((addInstr->RightOp()->IsOneInt() ||
                addInstr->RightOp()->IsMinusOneInt()) &&
                addInstr->LeftOp() == instr->FalseOp()) {
                auto resultOp = GetTemporary(instr->TrueOp());
                auto cmpResultOp = instr->ConditionOp();

                // Make sure that the types match.
                if(cmpResultOp->GetType() != instr->TrueOp()->GetType()) {
                    if(cmpResultOp->HasSingleUser()) {
                        // Change the type directly.
                        cmpResultOp->SetType(instr->TrueOp()->GetType());
                    }
                    else cmpResultOp = CreateIntCastIfRequired(cmpResultOp, 
                                                               instr->TrueOp()->GetType(), 
                                                               true /* isSigned */);
                }

                if(addInstr->RightOp()->IsOneInt()) {
                    irGen_.GetAdd(addInstr->LeftOp(), cmpResultOp, resultOp);
                }
                else irGen_.GetSub(addInstr->LeftOp(), cmpResultOp, resultOp);
                
                return LOG(resultOp);
            }
        }
    }

    // Try to convert to branchless code.
    if(auto result = HandleQuestionBranchless(instr)) {
        return result;
    }

    if(auto result = HandleQuestionOneZero(instr)) {
        return result;
    }

	if(auto result = HandleQuestionSetTrue(instr)) {
		return result;
	}

    if(auto result = HandleQuestionAndOr(instr)) {
        return result;
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Peephole::HasOnlyOneOrZeroResult(Operand* op, Block* testBlock, int level) {
    if(op->IsUndefinedConstant() || // Presume it's favorable.
       op->IsOneInt() || op->IsZeroInt()) {
       return true;
    }

    // Stop if the level is too high (the chances of finding the operand
    // is only 0/1 are very low, and we might enter a 'phi' loop).
    if(level > 4) {
        return false;
    }

    if(auto definingInstr = op->DefiningInstruction()) {
        if(definingInstr->IsComparison()) {
            return true;
        }
        else if(auto phiInstr = definingInstr->As<PhiInstr>()) {
            // Make sure that all incoming operands are 0 or 1.
            for(int i = 0; i < phiInstr->OperandCount(); i++) {
				auto incomingOp = phiInstr->GetOperand(i);

				if(incomingOp == op) {
					// Ignore self-referencing incoming operands.
					continue;
				}
				else if(HasOnlyOneOrZeroResult(phiInstr->GetOperand(i), 
											   testBlock, level + 1) == false) {
                    return false;
                }
            }

            return true;
        }
        else if(auto questInstr = definingInstr->As<QuestionInstr>()) {
            return HasOnlyOneOrZeroResult(questInstr->TrueOp(), 
                                          testBlock, level + 1) &&
                   HasOnlyOneOrZeroResult(questInstr->FalseOp(), 
                                          testBlock, level + 1);
        }
        else return false;
    }

    // Check if the range of the operand is [0, 1].
    if(op->IsInteger()) {
        OperandInfo opInfo(irGen_.GetUnit(), GetTarget());
		return (opInfo.IsGreaterOrEqual(op, GetZeroInt(op), true, testBlock) == RangeResult::Yes) &&
               (opInfo.IsSmallerOrEqual(op, GetOneInt(op), true, testBlock) == RangeResult::Yes);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionOneZero(QuestionInstr* instr) {
    // t1 = phi (0, 1, 0, 0, 1,...)
    // t2 = quest t1, a, 0     ->    t2 = mul t1, a
    // t3 = quest t1, 0, a     ->    t3 = sub 1, t1      t4 = mul t3, a
    // Make sure that the condition operand can be only 1 or 0.
    if(instr->FalseOp()->IsInteger() &&
       HasOnlyOneOrZeroResult(instr->ConditionOp(), instr->ParentBlock())) {
        bool invert = false;

        // The false or true operand should be 0.
        if(IsZeroInt(instr->FalseOp(), instr->ParentBlock()) == false) {
            if(IsZeroInt(instr->TrueOp(), instr->ParentBlock())) {
                invert = true;
            }
            else return nullptr;
        }
        
        // Make sure that the types match.
        auto conditionOp = instr->ConditionOp();
        
        if(conditionOp->GetType() != instr->TrueOp()->GetType()) {
            if(conditionOp->HasSingleUser()) {
                // Change the type directly.
                conditionOp->SetType(instr->TrueOp()->GetType());
            }
            else conditionOp = CreateIntCastIfRequired(conditionOp, 
                                                       instr->TrueOp()->GetType(), 
                                                       true /* isSigned */);
        }

        if(invert) {
            // sub 1, conditionOp
            auto subOp = GetTemporary(conditionOp);
            irGen_.GetSub(GetOneInt(conditionOp), conditionOp, subOp);
            conditionOp = subOp;
        }

        auto resultOp = GetTemporary(instr->TrueOp());
        auto valueOp = invert ? instr->FalseOp() : instr->TrueOp();

        irGen_.GetMul(conditionOp, valueOp, resultOp);
        return LOG(resultOp);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionSetTrue(QuestionInstr* instr) {
	// In the following example once 'flag' is set on 1 it never
	// changes back. The 'quest' obtained after If Combining can be
	// replaced by a much cheaper 'or' instruction like illustrated below:
	// flag = 0;                         flag = 0
	// for(i = 0; i < n; i++) {		     for(i = 0; i < n; i++) {
	//    if(condition) flag = 1;   ->      flag |= condition;
	// }							     }
	//
	// t1 = phi(0, t2)              ->   t1 = phi(0, t2)
	// t2 = quest condOp, 1, t1          t2 = quest condOp, 1, t1
	if((IsBoolean(instr->ConditionOp()) == false)                 ||
	   (IsOneInt(instr->TrueOp(), instr->ParentBlock()) == false) ||
	   (instr->FalseOp()->DefiningInstrIs<PhiInstr>() == false)) {
		return nullptr;
	}

	// The 'phi' should be of the form phi(0, t2, t2, ..., t2).
	// This allows having loops with 'continue' statements.
	auto phiInstr = instr->FalseOp()->DefiningInstrAs<PhiInstr>();
	bool valid = true;
	bool foundZero = false;
	
	phiInstr->ForEachSourceOp([&](Operand* incomingOp, int index) -> bool {
		if(incomingOp->IsZeroInt()) {
			foundZero = true;
		}
		else if(incomingOp != instr->ResultOp()) {
			// The incoming operand is not the 'quest' result.
			valid = false;
			return false;
		}
		
        return true;
	});

	if(valid && foundZero) {
		auto orOp = GetTemporary(instr->ResultOp());
		auto requiredType = instr->ResultOp()->GetType();
		auto conditionOp = CreateIntCastIfRequired(instr->ConditionOp(), requiredType);
		irGen_.GetOr(instr->FalseOp(), instr->ConditionOp(), orOp);
		return LOG(orOp);
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionOnBinary(QuestionInstr* instr) {
    // quest c, (add a, b), (add a, d) -> add a, (quest c, b, d)
    auto trueInstr = instr->TrueOp()->DefiningInstruction();
    auto falseInstr = instr->FalseOp()->DefiningInstruction();

    // We should have the same kind of instructions, which have
    // one operand the same (for commutative instructions it doesn't
    // matter where the operator is placed).
    if(((trueInstr && falseInstr) == false) ||
        (trueInstr->IsSameKind(falseInstr) == false)) {
        return nullptr;
    }

    // Only arithmetic and logical instructions are handled.
    if(((trueInstr->IsArithmetic() || trueInstr->IsLogical()) == false) ||
       ((falseInstr->IsArithmetic() || falseInstr->IsLogical()) == false)) {
        return nullptr;
    }

    Operand* commonOp = nullptr;
    Operand* otherOp1 = nullptr;
    Operand* otherOp2 = nullptr;

    if(trueInstr->GetSourceOp(0) == falseInstr->GetSourceOp(0)) {
        commonOp = trueInstr->GetSourceOp(0);
        otherOp1 = trueInstr->GetSourceOp(1);
        otherOp2 = falseInstr->GetSourceOp(1);
    }
    else if(trueInstr->GetSourceOp(1) == falseInstr->GetSourceOp(1)) {
        commonOp = trueInstr->GetSourceOp(1);
        otherOp1 = trueInstr->GetSourceOp(0);
        otherOp2 = falseInstr->GetSourceOp(0);
    }
    else if(trueInstr->IsCommutative()) {
        if(trueInstr->GetSourceOp(0) == falseInstr->GetSourceOp(1)) {
            commonOp = trueInstr->GetSourceOp(0);
            otherOp1 = trueInstr->GetSourceOp(1);
            otherOp2 = falseInstr->GetSourceOp(0);
        }
        else if(trueInstr->GetSourceOp(1) == falseInstr->GetSourceOp(0)) {
            commonOp = trueInstr->GetSourceOp(1);
            otherOp1 = trueInstr->GetSourceOp(0);
            otherOp2 = falseInstr->GetSourceOp(1);
        }
    }

    // Give up if we don't have a common operand.
    if(commonOp == nullptr) {
        return nullptr;
    }

    // Create the 'quest' that selects between the other operands.
    auto questOp = GetTemporary(commonOp);
    irGen_.GetQuestion(instr->ConditionOp(), otherOp1, otherOp2, questOp);

    if(trueInstr->IsArithmetic()) {
        auto arithOp = GetTemporary(commonOp);
        irGen_.GetArithmetic(trueInstr->GetOpcode(), commonOp, questOp, arithOp);
        return LOG(arithOp);
    }
    else {
        auto logicalOp = GetTemporary(commonOp);
        irGen_.GetLogical(trueInstr->GetOpcode(), commonOp, questOp, logicalOp);
        return LOG(logicalOp);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::ExpandQuestToLogical(QuestionInstr* instr, CmpInstr* cmpInstr) {
    // (a > -1) ? C1 : C2 -> ((a >> 31) & (C2 - C1)) + C1
    // (a <  0) ? C2 : C1 -> ((a >> 31) & (C2 - C1)) + C1
    // Note that the comparison must be signed, so that we get sign extension
    // when we shift by the size of the type.
    if((cmpInstr->IsGreater() && cmpInstr->RightOp()->IsMinusOneInt()) ||
        (cmpInstr->IsLess() && cmpInstr->RightOp()->IsZeroInt())) {
        IntConstant* C1;
        IntConstant* C2;

        if(cmpInstr->IsGreater()) {
            C1 = instr->TrueOp()->As<IntConstant>();
            C2 = instr->FalseOp()->As<IntConstant>();
        }
        else {
            C2 = instr->TrueOp()->As<IntConstant>();
            C1 = instr->FalseOp()->As<IntConstant>();
        }

        if(C1 && C2) {
            // ((a >> 31) & (C2 - C1)) + C1
            auto a = cmpInstr->LeftOp();
            int bits = C1->GetType()->SizeInBits();
            auto shiftOp = irGen_.GetIntConst(a->GetType(), bits - 1);

            auto shrOp = GetTemporary(a);
            irGen_.GetShr(a, shiftOp, shrOp);

            auto constDiffOp = folder_.FoldBinary(Opcode::Sub, C2, C1,
                                                  instr->ParentBlock());
            auto andOp = GetTemporary(a);
            irGen_.GetAnd(shrOp, constDiffOp, andOp);
            
            auto addOp = GetTemporary(a);
            irGen_.GetAdd(andOp, C1, addOp);
            return LOG(addOp);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionOnBool(QuestionInstr* instr) {
    // Check if the operands are 1 and 0. This simplification happens often.
    // quest c, 1, 0 -> zext c, intX
    if(instr->TrueOp()->IsOneInt() && instr->FalseOp()->IsZeroInt()) {
        return CreateIntCastIfRequired(instr->ConditionOp(), 
                                       instr->TrueOp()->GetType(), false);
    }

    // quest c, -1, 0 -> sext c, intX
    if(instr->TrueOp()->IsMinusOneInt() && instr->FalseOp()->IsZeroInt()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetSext(instr->ConditionOp(), resultOp->GetType(), resultOp);
        return LOG(resultOp);
    }

    // quest c, 0, 1 -> zext ~c, intX
    if(instr->TrueOp()->IsZeroInt() && instr->FalseOp()->IsOneInt()) {
        auto conditionOp = GetNegated(instr->ConditionOp());
        return LOG(CreateIntCastIfRequired(conditionOp, instr->TrueOp()->GetType(), false));
    }

    // quest c, 0, -1 -> sext ~c, intX
    if(instr->TrueOp()->IsZeroInt() && instr->FalseOp()->IsMinusOneInt()) {
        auto conditionOp = GetNegated(instr->ConditionOp());
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetSext(conditionOp, resultOp->GetType(), resultOp);
        return LOG(resultOp);
    }

    // If the source operands are also boolean then we can do the following:
    // quest c, 1, a -> or c, a
    if(instr->TrueOp()->IsOneInt() && instr->FalseOp()->IsBoolean()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetOr(instr->ConditionOp(), instr->FalseOp(), resultOp);
        return LOG(resultOp);
    }

    // quest c, a, 1 -> or ~c, a
    if(instr->TrueOp()->IsBoolean() && instr->FalseOp()->IsZeroInt()) {
        auto conditionOp = GetNegated(instr->ConditionOp());
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetOr(conditionOp, instr->TrueOp(), resultOp);
        return LOG(resultOp);
    }

    // quest c, a, 0 -> and c, a
    if(instr->TrueOp()->IsBoolean() && instr->FalseOp()->IsZeroInt()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetAnd(instr->ConditionOp(), instr->TrueOp(), resultOp);
        return LOG(resultOp);
    }

    // quest c, 0, a -> and ~c, a
    if(instr->TrueOp()->IsZeroInt() && instr->FalseOp()->IsBoolean()) {
        auto conditionOp = GetNegated(instr->ConditionOp());
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetAnd(conditionOp, instr->FalseOp(), resultOp);
        return LOG(resultOp);
    }

    // quest a, b, a -> and a, b
    if((instr->ConditionOp() == instr->FalseOp()) && instr->TrueOp()->IsBoolean()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetAnd(instr->ConditionOp(), instr->TrueOp(), resultOp);
        return LOG(resultOp);
    }

    // quest a, a, b -> or a, b
    if((instr->ConditionOp() == instr->TrueOp()) && instr->FalseOp()->IsBoolean()) {
        auto resultOp = GetTemporary(instr->TrueOp());
        irGen_.GetOr(instr->ConditionOp(), instr->FalseOp(), resultOp);
        return LOG(resultOp);
    }

    if(instr->ConditionOp()->IsBoolean() || 
       instr->ConditionOp()->DefiningInstrIs<CmpInstrBase>()) {
        // quest c, C + POW2, C -> add C, c * POW2
        // quest c, 7, 6 -> add 6, c
        auto trueConst = AsIntConstant(instr->TrueOp(), instr->ParentBlock());
        auto falseConst = AsIntConstant(instr->FalseOp(), instr->ParentBlock());

        if((trueConst && falseConst) &&
           (trueConst->Value() > falseConst->Value())) {
            __int64 difference = trueConst->Value() - falseConst->Value();

            if(IA::IsPowerOfTwo(difference)) {
                auto diffOp = irGen_.GetIntConst(trueConst->GetType(), difference);
                auto conditionOp = CreateIntCastIfRequired(instr->ConditionOp(), 
                                                           instr->TrueOp()->GetType());

                auto mulOp = GetTemporary(instr->TrueOp());
                irGen_.GetMul(conditionOp, diffOp, mulOp);

                auto resultOp = GetTemporary(instr->TrueOp());
                irGen_.GetAdd(falseConst, mulOp, resultOp);
                return LOG(resultOp);
            }
        }

        // quest c, C, C + POW2 -> sub C + POW2, c * POW2
        // quest c, 6, 7 -> sub 7, c
        if((trueConst && falseConst) &&
           (trueConst->Value() < falseConst->Value())) {
            __int64 difference = falseConst->Value() - trueConst->Value();

            if(IA::IsPowerOfTwo(difference)) {
                auto diffOp = irGen_.GetIntConst(falseConst->GetType(), difference);
                auto conditionOp = CreateIntCastIfRequired(instr->ConditionOp(), 
                                                           instr->FalseOp()->GetType());

                auto mulOp = GetTemporary(instr->FalseOp());
                irGen_.GetMul(conditionOp, diffOp, mulOp);

                auto resultOp = GetTemporary(instr->FalseOp());
                irGen_.GetSub(falseConst, mulOp, resultOp);
                return LOG(resultOp);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOperationOnQuest(Opcode opcode, Operand* opA, Operand* opB) {
    // add (quest c, 4, 5), 2 -> quest c, 6, 7
    // add (quest c, 4, 5), (quest c, 1, 2) -> quest c, 5, 7
    // We do this only if the 'true' and 'false' operands are constants,
    // and only if the register pressure is not going to increase.
    if(opB->DefiningInstrIs<QuestionInstr>() &&
       Instruction::IsCommutative(opcode)) {
        // Move the 'quest' to the left side (fewer cases to test for).
        std::swap(opA, opB);
    }

    auto questInstrA = opA->DefiningInstrAs<QuestionInstr>();

    if(questInstrA == nullptr) {
        return nullptr;
    }

    // Don't increase register pressure by creating a new 'quest' instruction.
    if(questInstrA->ResultOp()->HasSingleUser() == false) {
        return nullptr;
    }

    if(auto questInstrB = opB->DefiningInstrAs<QuestionInstr>()) {
        // 'OP quest, quest' case.
        // Don't increase register pressure by creating a new 'quest' instruction.
        if(questInstrB->ResultOp()->HasSingleUser() == false) {
            return nullptr;
        }
        else return LOG(HandleOperationOnQuestAndQuest(opcode, questInstrA, 
                                                       questInstrB));
    }

    // Check if there is at least one operand that is not a constant;
    // in this case we use another method to handle the situation.
    if((questInstrA->TrueOp()->IsConstant() == false ||
        questInstrA->FalseOp()->IsConstant() == false)) {
        return LOG(HandleOperationOnQuestAndOther(opcode, questInstrA, opB));
    }

    // Both operands are constants.
    auto trueConst = questInstrA->TrueOp()->As<Constant>();
    auto falseConst = questInstrA->FalseOp()->As<Constant>();

    if((trueConst  && falseConst) == false) {
        return nullptr;
    }

    if(trueConst->IsIntConstant()) {
        auto intConst = AsIntConstant(opB, nullptr);

        if(intConst == nullptr) {
            return nullptr;
        }

        auto intTrueConst = trueConst->As<IntConstant>();
        auto intFalseConst = falseConst->As<IntConstant>();

        switch(opcode) {
            case Opcode::Add: 
            case Opcode::Sub:
            case Opcode::Mul:
            case Opcode::Div:
            case Opcode::Udiv:
            case Opcode::Mod:
            case Opcode::Umod:
            case Opcode::And:
            case Opcode::Or:
            case Opcode::Xor:
            case Opcode::Shl:
            case Opcode::Shr:
            case Opcode::Ushr: {
                auto newTrueConst = folder_.FoldBinary(opcode, intTrueConst, 
                                                       intConst, nullptr);
                auto newFalseConst = folder_.FoldBinary(opcode, intFalseConst, 
                                                        intConst, nullptr);

                // Create the result 'quest' instruction.
                auto resultOp = GetTemporary(newTrueConst);
                irGen_.GetQuestion(questInstrA->ConditionOp(), newTrueConst, 
                                   newFalseConst, resultOp);
                return LOG(resultOp);
            }
        }
    }
    else if(trueConst->IsFloatingConstant()) {
        auto floatConst = opB->As<FloatConstant>();

        if(floatConst == nullptr) {
            return nullptr;
        }

        auto floatTrueConst = trueConst->As<FloatConstant>();
        auto floatFalseConst = falseConst->As<FloatConstant>();

        switch(opcode) {
            case Opcode::Fadd:
            case Opcode::Fsub:
            case Opcode::Fmul:
            case Opcode::Fdiv: {
                auto newTrueConst = folder_.FoldBinary(opcode, floatTrueConst, 
                                                       floatConst, nullptr);
                auto newFalseConst = folder_.FoldBinary(opcode, floatFalseConst, 
                                                        floatConst, nullptr);
                auto resultOp = GetTemporary(newTrueConst);
                irGen_.GetQuestion(questInstrA->ConditionOp(), newTrueConst, 
                                   newFalseConst, resultOp);
                return LOG(resultOp);
            }
        }
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOperationOnQuestAndOther(Opcode opcode, QuestionInstr* instr,
                                                  Operand* op) {
    // Try to simplify cases like the following one:
    // t1 = add a, C1                t1 = add a, C1 + C3
    // t2 = add b, C2          =>    t2 = add b, C2 + C3
    // t3 = quest c, t1, t2          t3 = quest c, t1, t2
    // t4 = add t3, C2               
    if(op->IsConstant() == false) {
        return nullptr;
    }

    // We handle only arithmetic and logical instructions (the common cases).
    if((Instruction::IsArithmetic(opcode) || 
        Instruction::IsLogical(opcode)) == false) {
        return nullptr;
    }

    // We do it only if the operands used in 'quest' have a single user,
    // so that we don't need to clone the instructions and 
    // increase the register pressure.
    if(auto temp = instr->TrueOp()->As<Temporary>()) {
        if(temp->HasSingleUser() == false) {
            return nullptr;
        }
    }
    
    if(auto temp = instr->FalseOp()->As<Temporary>()) {
        if(temp->HasSingleUser() == false) {
            return nullptr;
        }
    }

    // Try to simplify both operands.
    auto newTrueOp = SimplifyBinary(opcode, instr->TrueOp(), op, 
                                    instr->ParentBlock());
    auto newFalseOp = SimplifyBinary(opcode, instr->FalseOp(), op,
                                     instr->ParentBlock());
    if((newTrueOp && newFalseOp) == false) {
        return nullptr;
    }

    // Both operands simplified, create a new 'quest' instruction.
    auto resultOp = GetTemporary(instr->TrueOp());
    irGen_.GetQuestion(instr->ConditionOp(), newTrueOp, 
                       newFalseOp, resultOp);
    return LOG(resultOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleOperationOnQuestAndQuest(Opcode opcode, 
                                                  QuestionInstr* instrA,
                                                  QuestionInstr* instrB) {
    // add (quest c, 4, 5), (quest c, 1, 2) -> quest c, 5, 7
    // We do the simplification only if the 'quest' result operands are used
    // only by the min/max intrinsics (prevents creating new instructions).
    if((instrA->ResultOp()->HasSingleUser() == false) ||
       (instrB->ResultOp()->HasSingleUser() == false)) {
        return nullptr;
    }

    // Check if only constants are used.
    auto trueConstA = instrA->TrueOp()->As<Constant>();
    auto falseConstA = instrA->FalseOp()->As<Constant>();

    if((trueConstA == nullptr) ||
       (falseConstA == nullptr)) {
        return nullptr;
    }

    auto trueConstB = instrB->TrueOp()->As<Constant>();
    auto falseConstB = instrB->FalseOp()->As<Constant>();

    if((trueConstB == nullptr) ||
       (falseConstB == nullptr)) {
        return nullptr;
    }

    if(trueConstA->IsIntConstant()) {
        auto intTrueConstA = trueConstA->As<IntConstant>();
        auto intFalseConstA = falseConstA->As<IntConstant>();
        auto intTrueConstB = trueConstB->As<IntConstant>();
        auto intFalseConstB = falseConstB->As<IntConstant>();

        switch(opcode) {
            case Opcode::Add: 
            case Opcode::Sub:
            case Opcode::Mul:
            case Opcode::Div:
            case Opcode::Udiv:
            case Opcode::Mod:
            case Opcode::Umod:
            case Opcode::And:
            case Opcode::Or:
            case Opcode::Xor:
            case Opcode::Shl:
            case Opcode::Shr:
            case Opcode::Ushr: {
                auto newTrueConst = folder_.FoldBinary(opcode, intTrueConstA, 
                                                       intTrueConstB, nullptr);
                auto newFalseConst = folder_.FoldBinary(opcode, intFalseConstA, 
                                                        intFalseConstB, nullptr);
                auto resultOp = GetTemporary(newTrueConst);
                irGen_.GetQuestion(instrA->ConditionOp(), newTrueConst, 
                                   newFalseConst, resultOp);
                return LOG(resultOp);
            }
        }
    }
    else if(trueConstA->IsFloatingConstant()) {
        DebugValidator::IsTrue(falseConstA->IsFloatingConstant());
        DebugValidator::IsTrue(trueConstB->IsFloatingConstant());
        DebugValidator::IsTrue(falseConstB->IsFloatingConstant());

        auto floatTrueConstA = trueConstA->As<FloatConstant>();
        auto floatFalseConstA = falseConstA->As<FloatConstant>();
        auto floatTrueConstB = trueConstB->As<FloatConstant>();
        auto floatFalseConstB = falseConstB->As<FloatConstant>();

        switch(opcode) {
            case Opcode::Fadd:
            case Opcode::Fsub:
            case Opcode::Fmul:
            case Opcode::Fdiv: {
                auto newTrueConst = folder_.FoldBinary(opcode, floatTrueConstA, 
                                                       floatTrueConstB, nullptr);
                auto newFalseConst = folder_.FoldBinary(opcode, floatFalseConstA, 
                                                        floatFalseConstB, nullptr);
                auto resultOp = GetTemporary(newTrueConst);
                irGen_.GetQuestion(instrA->ConditionOp(), newTrueConst, 
                                   newFalseConst, resultOp);
                return LOG(resultOp);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionBranchless(QuestionInstr* instr) {
    // Consider the following example:
    // for(i = 0; i < n; i++)
    //     if(v[i] >= 128) sum += v[i];
    // 
    // The branch inside the loop heavily penalizes modern CPUs 
    // and can replaced with the following equivalent branchless code:
    // sum += ~((v[i] - 128) >> 31) & v[i].
    // 
    // How the code works:
    // if v[i] >= 128 
    //     - the difference is positive and the sign bit is 0
    //     - after the sign extension the number is 0, -1 inverted
    //     - -1 & v[i] = v[i] -> sum += v[i]
    // if v[i] < 128
    //     - the difference is negative and the sign bit is 1
    //     - after the sign extension the number is -1, 0 inverted
    //     - 0 & v[i] = 0 -> sum += 0
    // 
    // Make sure the condition is a comparison on integers
    // and the right operand is a constant.
    auto cmpInstr = instr->ConditionOp()->DefiningInstrAs<CmpInstrBase>();

    if((cmpInstr == nullptr) || cmpInstr->IsFcmp() ||
       (cmpInstr->RightOp()->IsIntConstant() == false)) {
        return false;
    }

    // Check if one of the operands is the other one but adjusted.
    bool trueOperand;
    auto adjustedInstr = DetectBranchlessOperand(instr, trueOperand);

    if(adjustedInstr == nullptr) {
        return nullptr;
    }

    // The compared operand must be the one used in the adjustment.
    // The adjustment can be an 'add' or 'sub' instruction.
    if(adjustedInstr->RightOp() != cmpInstr->LeftOp()) {
        return nullptr;
    }

    if(adjustedInstr->IsAdd()) {
        return HandleQuestionBranchlessAdd(cmpInstr, adjustedInstr, 
                                           trueOperand, true /* isAdd */);
    }
    else if(adjustedInstr->IsSub()) {
        return HandleQuestionBranchlessAdd(cmpInstr, adjustedInstr, 
                                           trueOperand, false /* isAdd */);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ArithmeticInstr* Peephole::DetectBranchlessOperand(QuestionInstr* instr, 
                                                   bool& trueOperand) {
    // Check if the left operand is the right one with an adjustment. Example:
    // quest c, (add a, b), a
    if(auto trueInstr = instr->TrueOp()->DefiningInstrAs<ArithmeticInstr>()) {
        if(trueInstr->IsAdd() || trueInstr->IsSub()) {
            // Canonicalize by moving the right operand on the left.
            if(trueInstr->RightOp() == instr->FalseOp()) {
                trueInstr->SwapOperands();
            }

            if(trueInstr->LeftOp() == instr->FalseOp()) {
                trueOperand = true;
                return trueInstr;
            }
        }
    }

    if(auto falseInstr = instr->FalseOp()->DefiningInstrAs<ArithmeticInstr>()) {
        if(falseInstr->IsAdd() || falseInstr->IsSub()) {
            // Canonicalize by moving the right operand on the left.
            if(falseInstr->RightOp() == instr->TrueOp()) {
                falseInstr->SwapOperands();
            }

            if(falseInstr->LeftOp() == instr->TrueOp()) {
                trueOperand = false;
                return falseInstr;
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionBranchlessAdd(CmpInstrBase* cmpInstr, 
                                               ArithmeticInstr* adjustmentInstr,
                                               bool trueOperand, bool isAdd) {
    // Some examples of the generated code:
    // a > C  -> ~((a - C+1) >> 31)
    // a >= C -> ~((a - C) >> 31)
    // a < C  -> ((a - C+1) >> 31)
    // a <= C -> ((a - C) >> 31)
    bool needsNot = (trueOperand && (cmpInstr->IsGreater() || 
                                     cmpInstr->IsGreaterOrEqual())) ||
                    ((trueOperand == false) && (cmpInstr->IsLess() ||
                                                cmpInstr->IsLessOrEqual()));
    auto intConst = cmpInstr->RightOp()->As<IntConstant>();
    auto oneConst = irGen_.GetIntConst(intConst->GetType(), 1);

    // If the constant needs to be incremented make sure
    // there is no overflow when doing it.
    if((cmpInstr->IsGreaterOrEqual() || cmpInstr->IsLessOrEqual()) ||
        (IA::AddOverflows(intConst, oneConst) == false)) {
        IntConstant* newIntConst = cmpInstr->IsGreaterOrEqual() || 
                                   cmpInstr->IsLessOrEqual() ? intConst :
                                   irGen_.GetIntConst(intConst->GetType(), 
                                                      IA::Add(intConst, oneConst));

        auto subOp = irGen_.GetTemporary(newIntConst->GetType());
        irGen_.GetSub(cmpInstr->LeftOp(), newIntConst, subOp);

        // Sign-extend the subtraction result.
        int bits = newIntConst->GetType()->SizeInBits();
        auto amountConst = irGen_.GetIntConst(newIntConst->GetType(), bits - 1);
        auto shrOp = irGen_.GetTemporary(newIntConst->GetType());
        irGen_.GetShr(subOp, amountConst, shrOp);

        // Negate the result if required.
        if(needsNot) {
            auto xorOp = irGen_.GetTemporary(newIntConst->GetType());
            auto minusOneConst = irGen_.GetIntConst(newIntConst->GetType(), -1);
            irGen_.GetXor(shrOp, minusOneConst, xorOp);
            shrOp = xorOp;
        }

        // Combine the result with the value, then generate
        // the increment or decrement instruction.
        auto andOp = irGen_.GetTemporary(newIntConst->GetType());
        irGen_.GetAnd(shrOp, cmpInstr->LeftOp(), andOp);

        if(isAdd) {
            auto addOp = irGen_.GetTemporary(newIntConst->GetType());
            irGen_.GetAdd(adjustmentInstr->RightOp(), andOp, addOp);
            return LOG(addOp);
        }
        else {
            auto subOp2 = irGen_.GetTemporary(newIntConst->GetType());
            irGen_.GetSub(adjustmentInstr->RightOp(), andOp, subOp2);
            return LOG(subOp2);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionAndOr(QuestionInstr* instr) {
    // Sometimes users write code like the following, although
    // the form on the right is faster because it has no branches.
    // Jump-to-return elimination converts the code from the left
    // to a 'quest' instruction and here we try to eliminate it
    // by generating code equivalent to the right.
    // if(a == 0)      return 1;   ->   return a == 0 || b == 0
    // else if(b == 0) return 1;
    // else return 0;
    if((instr->TrueOp()->DefiningInstrIs<CmpInstrBase>() ||
        instr->FalseOp()->DefiningInstrIs<CmpInstrBase>()) == false) {
        return nullptr;
    }

    if(instr->TrueOp()->IsZeroInt()) {
        return HandleQuestionAndOrTrueZero(instr);
    }
    else if(instr->FalseOp()->IsZeroInt()) {
        return HandleQuestionAndOrFalseZero(instr);
    }
    else if(instr->TrueOp()->IsOneInt()) {
        return HandleQuestionAndOrTrueOne(instr);
    }
    else if(instr->FalseOp()->IsOneInt()) {
        return HandleQuestionAndOrFalseOne(instr);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionAndOrTrueZero(QuestionInstr* instr) {
    // if(a != 0) return 0;
    // else if(b != 0) return 0;   ->   return a == 0 && b == 0
    // else return 1;
    //
    // t1 = cmp eq a, 0      ->   t1 = cmp eq a, 0
    // t2 = quest b, 0, t1        t2 = cmp eq b, 0
    //                            t3 = and t1, t2
    auto cmpInstr = instr->FalseOp()->DefiningInstrAs<CmpInstrBase>();
    DebugValidator::IsNotNull(cmpInstr);

    if((cmpInstr->IsFcmp() == false) && 
        cmpInstr->IsEqual() && 
        cmpInstr->RightOp()->IsZeroInt()) {
        // The pattern matches, create the branch-less form.
        auto resultType = instr->ResultOp()->GetType();
        auto a = instr->ConditionOp();
        auto b = cmpInstr->LeftOp();

        auto cmpOpA = CreateCompare(a, GetZeroInt(a), OrderType::Equal,
                                    resultType, cmpInstr->IsUcmp());
        auto cmpOpB = CreateCompare(b, GetZeroInt(b), OrderType::Equal,
                                    resultType, cmpInstr->IsUcmp());

        auto andOp = irGen_.GetTemporary(resultType);
        irGen_.GetAnd(cmpOpA, cmpOpB, andOp);
        andOp->SetIsBoolean(true);
        return LOG(andOp);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionAndOrFalseZero(QuestionInstr* instr) {
    // if(a == 0) return 0;
    // else if(b == 0) return 0;   ->   return a != 0 && b != 0
    // else return 1;
    //
    // t1 = cmp neq a, 0      ->   t1 = cmp neq a, 0
    // t2 = quest b, t1, 0         t2 = cmp neq b, 0
    //                             t3 = and t1, t2
    auto cmpInstr = instr->TrueOp()->DefiningInstrAs<CmpInstrBase>();
    DebugValidator::IsNotNull(cmpInstr);

    if((cmpInstr->IsFcmp() == false) && 
        cmpInstr->IsNotEqual() && 
        cmpInstr->RightOp()->IsZeroInt()) {
        // The pattern matches, create the branch-less form.
        auto resultType = instr->ResultOp()->GetType();
        auto a = instr->ConditionOp();
        auto b = cmpInstr->LeftOp();

        auto cmpOpA = CreateCompare(a, GetZeroInt(a), OrderType::NotEqual,
                                    resultType, cmpInstr->IsUcmp());
        auto cmpOpB = CreateCompare(b, GetZeroInt(b), OrderType::NotEqual,
                                    resultType, cmpInstr->IsUcmp());
        
        auto andOp = irGen_.GetTemporary(resultType);
        irGen_.GetAnd(cmpOpA, cmpOpB, andOp);
        andOp->SetIsBoolean(true);
        return LOG(andOp);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionAndOrFalseOne(QuestionInstr* instr) {
    // if(a == 0) return 1;
    // else if(b == 0) return 1;   ->   return a == 0 || b == 0
    // else return 0;
    //
    // t1 = cmp eq a, 0      ->   t1 = cmp eq a, 0
    // t2 = quest b, t1, 1        t2 = cmp eq b, 0
    //                            t3 = or t1, t2
    auto cmpInstr = instr->TrueOp()->DefiningInstrAs<CmpInstrBase>();
    DebugValidator::IsNotNull(cmpInstr);

    if((cmpInstr->IsFcmp() == false) && 
        cmpInstr->IsEqual() && 
        cmpInstr->RightOp()->IsZeroInt()) {
        // The pattern matches, create the branch-less form.
        auto resultType = instr->ResultOp()->GetType();
        auto a = instr->ConditionOp();
        auto b = cmpInstr->LeftOp();

        auto cmpOpA = CreateCompare(a, GetZeroInt(a), OrderType::Equal,
                                    resultType, cmpInstr->IsUcmp());
        auto cmpOpB = CreateCompare(b, GetZeroInt(b), OrderType::Equal,
                                    resultType, cmpInstr->IsUcmp());

        auto orOp = irGen_.GetTemporary(resultType);
        irGen_.GetOr(cmpOpA, cmpOpB, orOp);
        orOp->SetIsBoolean(true);
        return LOG(orOp);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* Peephole::HandleQuestionAndOrTrueOne(QuestionInstr* instr) {
    // if(a != 0) return 1;
    // else if(b != 0) return !;   ->   return a != 0 || b != 0
    // else return 0;
    //
    // t1 = cmp neq a, 0      ->   t1 = cmp neq a, 0
    // t2 = quest b, 1, t1         t2 = cmp neq b, 0
    //                             t3 = or t1, t2
    auto cmpInstr = instr->FalseOp()->DefiningInstrAs<CmpInstrBase>();
    DebugValidator::IsNotNull(cmpInstr);

    if((cmpInstr->IsFcmp() == false) && 
        cmpInstr->IsNotEqual() && 
        cmpInstr->RightOp()->IsZeroInt()) {
        // The pattern matches, create the branch-less form.
        auto resultType = instr->ResultOp()->GetType();
        auto a = instr->ConditionOp();
        auto b = cmpInstr->LeftOp();

        auto cmpOpA = CreateCompare(a, GetZeroInt(a), OrderType::NotEqual,
                                    resultType, cmpInstr->IsUcmp());
        auto cmpOpB = CreateCompare(b, GetZeroInt(b), OrderType::NotEqual,
                                    resultType, cmpInstr->IsUcmp());

        auto orOp = irGen_.GetTemporary(resultType);
        irGen_.GetOr(cmpOpA, cmpOpB, orOp);
        orOp->SetIsBoolean(true);
        return LOG(orOp);
    }

    // if(a > 3) return 1;
    // else if(b < 6) return !;   ->   return a > 3 || b < 6
    // else return 0;
    //
    // t1 = cmp gt @a, 3      ->   t1 = cmp gt @a, 3
	// t2 = cmp lt @b, 6           t2 = cmp lt @b, 6
	// t3 = quest t1, 1, t2        t3 = and t1, t2
    if(instr->ConditionOp()->DefiningInstrIs<CmpInstrBase>()) {
        auto andOp = irGen_.GetTemporary(instr->ConditionOp()->GetType());
        irGen_.GetAnd(instr->FalseOp(), instr->ConditionOp(), andOp);
        andOp->SetIsBoolean(true);
        return LOG(andOp);
    }

    return nullptr;
}

} // namespace Optimization