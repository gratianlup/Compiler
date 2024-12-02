// ConstantFolderArithmeticLogical.cpp
// Copyright (c) Lup Gratian
//
// Implements the constant folder for arithmetic and logical instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantFolder.hpp"

namespace Analysis {

Operand* ConstantFolder::HandleCompare(Opcode opcode, Operand* opA, Operand* opB, 
									   OrderType order, Block* block) {
	// We handle both integer and floating comparison here, because they
	// have many properties in common. First check for constant operands.
    if(opA->IsInteger()) {
        if(auto result = HandleCompareInt(opcode, opA, opB, order, block)) {
            return result;
        }
    }
	else if(opA->IsFloatingConstant() && 
            opB->IsFloatingConstant()) {
		return HandleCompareFloat(opcode, opA, opB, order, block);
	}

	if(opA->IsNullConstant() && opB->IsNullConstant()) {
		// nullptr ==/<=/>= nullptr -> 1
		// nullptr !=/</> nullptr -> 0
		switch(order) {
			case OrderType::Equal:
			case OrderType::LessOrEqual:
			case OrderType::GreaterOrEqual: return GetBool(true);
			case OrderType::NotEqual:
			case OrderType::Less:
			case OrderType::Greater: return GetBool(false);
			default: DebugValidator::Unreachable();
		}
	}
	else if(opA->IsUndefinedConstant() || 
            opB->IsUndefinedConstant()) {
		return HandleCompareUndef(opcode, opA, opB, order, block);
	}

    // Test for comparisons involving null pointers.
    if(opA->IsPointer()) {
        if(auto result = HandleCompareNullPointer(opcode, opA, opB, order, block)) {
            return result;
        }
    }

    // Test for comparisons involving 'quest' instructions.
    if(auto result = HandleCompareQuestion(opcode, opA, opB, order)) {
        return result;
    }

    // Test for comparisons involving 'phi' instructions.
    if(auto result = HandleComparePhi(opcode, opA, opB, order)) {
        return result;
    }

    // Test for the relation of one or two parameters
    // for which the possible constant values are known.
    if(auto result = HandleCompareParameters(opcode, opA, opB, order)) {
        return result;
    }

	// Test for some relationships including global variables.
	if(auto result = HandleCompareGlobalNull(opcode, opA, opB, order)) {
		return result;
	}

    // Test for some relationships including loads
    // from the same/different locations.
	if(auto result = HandleCompareLoadLoad(opcode, opA, opB, order)) {
		return result;
	}

    // Test for the relation between values loaded from one or two
    // constant global variables.
    if(auto result = HandleCompareGlobalLoad(opcode, opA, opB, order)) {
        return result;
    }

	// Test for cases in which at least one of the operands is a variable
	// or an expression that involves an 'addr'/'index'/'elem' instruction.
	if(auto result = HandleCompareVars(opcode, opA, opB, order)) {
        return result;
    }

    return HandleCompareRange(opcode, opA, opB, order, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareInt(Opcode opcode, Operand* opA, Operand* opB, 
									      OrderType order, Block* block) {
    OperandInfo opInfo(irGen_->GetUnit(), target_);
    auto icA = opInfo.GetIntConstant(opA, block);
    auto icB = opInfo.GetIntConstant(opB, block);

	if(icA && icB) {
		// This is a 'cmp' instruction with both operands constant.
		// Evaluate the relation and return '0' for 'false' and '1' for true.
		// Note that we need to take care about unsigned comparison.
		bool isUnsigned = opcode == Opcode::Ucmp;

		switch(order) {
			case OrderType::Equal: {
                return GetBool(IA::AreEqual(icA, icB));
            }
			case OrderType::NotEqual: {
                return GetBool(IA::AreNotEqual(icA, icB));
            }
			case OrderType::Less: {
				if(isUnsigned) return GetBool(IA::IsSmallerUnsigned(icA, icB));
				else return GetBool(IA::IsSmaller(icA, icB));
			}
			case OrderType::LessOrEqual: {
				if(isUnsigned) return GetBool(IA::IsSmallerOrEqualUnsigned(icA, icB));
				else return GetBool(IA::IsSmallerOrEqual(icA, icB));
			}
			case OrderType::Greater: {
				if(isUnsigned) return GetBool(IA::IsLargerUnsigned(icA, icB));
				else return GetBool(IA::IsLarger(icA, icB));
			}
			case OrderType::GreaterOrEqual: {
				if(isUnsigned) return GetBool(IA::IsLargerOrEqualUnsigned(icA, icB));
				else return GetBool(IA::IsLargerOrEqual(icA, icB));
			}
			default: DebugValidator::Unreachable();
		}
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareFloat(Opcode opcode, Operand* opA, Operand* opB, 
									        OrderType order, Block* block) {
    IRFloatingKind kind = opA->GetType()->As<FloatingType>()->GetSubtype();
	double va = opA->As<FloatConstant>()->Value();
	double vb = opB->As<FloatConstant>()->Value();

	switch(order) {
		case OrderType::Equal:          return GetBool(FA::AreEqual(va, vb, kind));
		case OrderType::NotEqual:       return GetBool(FA::AreNotEqual(va, vb, kind));
		case OrderType::Less:           return GetBool(FA::IsSmaller(va, vb, kind));
		case OrderType::LessOrEqual:    return GetBool(FA::IsSmallerOrEqual(va, vb, kind));
		case OrderType::Greater:        return GetBool(FA::IsLarger(va, vb, kind));
		case OrderType::GreaterOrEqual: return GetBool(FA::IsLargerOrEqual(va, vb, kind));
		default: DebugValidator::Unreachable();
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareUndef(Opcode opcode, Operand* opA, Operand* opB, 
									        OrderType order, Block* block) {
    if(opA->IsUndefinedConstant() && opB->IsUndefinedConstant()) {
		// Both operands are undefined, so we can choose 'undef' as the result.
		return GetUndefined(opA);
	}

	// One of the operands may be a constant. 
    // If it is the result is based on it.
	Operand* constantOp = nullptr;

	if(opA->IsIntConstant() || opA->IsFloatingConstant()) {
		constantOp = opA;
	}
	else if(opB->IsIntConstant() || opB->IsFloatingConstant()) {
		constantOp = opB;
	}
	else {
		// Comparing an 'undef' with something that is
        // not a constant, return 'undef'.
		return GetUndefined(opA);
	}

	if(auto intConst = constantOp->As<IntConstant>()) {
		IRIntegerKind kind = intConst->GetType()->GetSubtype();
		return GetBool(IA::AreEqual(intConst->Value(), 0, kind));
	}
	else if(auto floatConst = constantOp->As<FloatConstant>()) {
		IRFloatingKind kind = floatConst->GetType()->GetSubtype();
		return GetBool(FA::AreEqual(floatConst->Value(), 0.0, kind));
	}
	else { 
        DebugValidator::Unreachable();
        return nullptr;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareNullPointer(Opcode opcode, Operand* opA, 
                                                  Operand* opB, OrderType order,
                                                  Block* block) {
    DebugValidator::IsTrue(opA->IsPointer());

    NullConstant* nullConst1 = nullptr;
    NullConstant* nullConst2 = nullptr;
    Operand* other;

    if(auto nullConst = opA->As<NullConstant>()) {
        nullConst1 = nullConst;
    }
    else other = opA;

    if(auto nullConst = opB->As<NullConstant>()) {
        if(nullConst1) nullConst2 = nullConst;
        else nullConst1 = nullConst;
    }
    else other = opB;

    // If we have two 'nullptr' then we definitely know the result.
    if(nullConst1 && nullConst2) {
        switch(order) {
            case OrderType::Equal:
            case OrderType::LessOrEqual:
            case OrderType::GreaterOrEqual: {
                return GetBool(true);
            }
            default: {
                return GetBool(false);
            }
        }
    }
    else if(nullConst1) {
        // Move the null constant on the right so that
        // we have fewer cases to test for.
        if(nullConst1 == opA) {
            order = CmpInstrBase::InvertedOrder(order, false /* invertEquality */);
        }

        // Check if we know anything about the other operand.     
        // p != NULL -> 1, if we know p != NULL
        // p != NULL -> 0, if we know p == NULL
        OperandInfo opInfo(irGen_->GetUnit(), target_);

        if(opInfo.IsPointerNotNull(other, block)) {
            switch(order) {
                case OrderType::Equal:
                case OrderType::LessOrEqual:
                case OrderType::GreaterOrEqual: {
                    return GetBool(false);
                }
                default: return GetBool(true);
            }
        }
        else if(opInfo.IsPointerNull(other, block)) {
            switch(order) {
                case OrderType::Equal:
                case OrderType::LessOrEqual:
                case OrderType::GreaterOrEqual: {
                    return GetBool(true);
                }
                default: return GetBool(false);
            }
        }
    }
    else if((order == OrderType::Equal) ||
            (order == OrderType::NotEqual)) {
        OperandInfo opInfo(irGen_->GetUnit(), target_);
        bool isEqual = order == OrderType::Equal;

        // First result is for 'equal', second for 'not equal'.
        // a == b -> 1/0, if a == NULL and b == NULL
        if(opInfo.IsPointerNull(opA, block) &&
           opInfo.IsPointerNull(opB, block)) {
            return GetBool(isEqual ? true : false);
        }

        // a == b -> 0/1, if a != NULL and b == NULL or a == NULL and b != NULL
        if((opInfo.IsPointerNotNull(opA, block) &&
            opInfo.IsPointerNull(opB, block))
            ||
            (opInfo.IsPointerNull(opA, block) &&
             opInfo.IsPointerNotNull(opB, block))) {
            return GetBool(isEqual ? false : true);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareRange(Opcode opcode, Operand* opA, 
                                            Operand* opB, OrderType order, 
                                            Block* block) {
    // Try so compare the operands using value-range info.
    // Floating-point comparisons can't be handled here.
    if(opcode == Opcode::Fcmp) {
        return nullptr;
    }

    OperandInfo opInfo(irGen_->GetUnit(), target_);

    // For pointers we can handle only equality.
    if(opA->IsPointer()) {
        switch(order) {
            case OrderType::Equal: {
                auto result = opInfo.AreEqual(opA, opB, block);

                if(result == RangeResult::Yes) return GetBool(true);
                else if(result == RangeResult::No) return GetBool(false);
                break;
            }
            case OrderType::NotEqual: {
                auto result = opInfo.AreNotEqual(opA, opB, block);

                if(result == RangeResult::Yes) return GetBool(true);
                else if(result == RangeResult::No) return GetBool(false);
                break;
            }
        }

        return nullptr;
    }

    switch(order) {
		case OrderType::Equal: {
            auto result = opInfo.AreEqual(opA, opB, block);

            if(result == RangeResult::Yes) return GetBool(true);
            else if(result == RangeResult::No) return GetBool(false);
            break;
        }
		case OrderType::NotEqual: {
            auto result = opInfo.AreNotEqual(opA, opB, block);

            if(result == RangeResult::Yes) return GetBool(true);
            else if(result == RangeResult::No) return GetBool(false);
            break;
        }
		case OrderType::Less: {
            bool isSigned = opcode == Opcode::Cmp;
            auto result = opInfo.IsSmaller(opA, opB, isSigned, block);

            if(result == RangeResult::Yes) return GetBool(true);
            else if(result == RangeResult::No) return GetBool(false);
            break;
        }
		case OrderType::LessOrEqual: {
            bool isSigned = opcode == Opcode::Cmp;
            auto result = opInfo.IsSmallerOrEqual(opA, opB, isSigned, block);

            if(result == RangeResult::Yes) return GetBool(true);
            else if(result == RangeResult::No) return GetBool(false);
            break;
        }
		case OrderType::Greater: {
            bool isSigned = opcode == Opcode::Cmp;
            auto result = opInfo.IsGreater(opA, opB, isSigned, block);

            if(result == RangeResult::Yes) return GetBool(true);
            else if(result == RangeResult::No) return GetBool(false);
            break;
        }
		case OrderType::GreaterOrEqual: {
            bool isSigned = opcode == Opcode::Cmp;
            auto result = opInfo.IsGreaterOrEqual(opA, opB, isSigned, block);

            if(result == RangeResult::Yes) return GetBool(true);
            else if(result == RangeResult::No) return GetBool(false);
            break;
        }
		default: DebugValidator::Unreachable();
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleComparePhi(Opcode opcode, Operand* opA,  Operand* opB, 
							              OrderType order) {
    // cmp lt phi {1,2}, 5 -> 1
    // cmp lt phi {1,2}, phi {3,4} -> 1
    // We do this only for integer constants.
    auto phiInstrA = opA->DefiningInstrAs<PhiInstr>();
    auto phiInstrB = opB->DefiningInstrAs<PhiInstr>();

    // All incoming operands should be integer constants.
    if(phiInstrA) {
        if(phiInstrA->ResultOp()->IsInteger() == false) {
            return nullptr;
        }

        if(phiInstrA->HasOnlyConstants() == false) {
            return nullptr;
        }

        // Don't do any check if the 'phi' has many incoming operands,
        // it would be time consuming and the chances to simplify something are small.
        if((phiInstrA->OperandCount() < 2) ||
           (phiInstrA->OperandCount() > 8)) {
            return nullptr;
        }
    }

    if(phiInstrB) {
        if(phiInstrA == nullptr) {
            return nullptr;
        }

        if((phiInstrB->ResultOp()->IsInteger() == false) ||
           (phiInstrB->HasOnlyConstants() == false)) {
            return nullptr;
        }

        if((phiInstrA->OperandCount() < 2) ||
           (phiInstrA->OperandCount() > 8)) {
            return nullptr;
        }
    }

    if(phiInstrA && phiInstrB) {
        bool isSigned = opcode == Opcode::Cmp;
        bool first = true;
        bool previous = false;

        // Test each pair.
        for(int i = 0; i < phiInstrA->OperandCount(); i++) {
            for(int j = 0; j < phiInstrB->OperandCount(); j++) {
                auto incomingOpA = phiInstrA->GetOperand(i);
                auto incomingOpB = phiInstrB->GetOperand(j);

                if((incomingOpA && incomingOpB) == false) {
                    return nullptr;
                }

                bool result = OrderHolds(order, incomingOpA, 
                                         incomingOpB, isSigned);
                if(first) {
                    previous = result;
                    first = false;
                }
                else if(result != previous) {
                    // The result for all incoming operands should be the same.
                    return nullptr;
                }
            }
        }

        // We can fold to a constant.
        return GetBool(previous);
    }
    else if(phiInstrA) {
        auto intConst = AsIntConstant(opB, phiInstrA->ParentBlock());

        if(intConst == nullptr) {
            return nullptr;
        }

        bool isSigned = opcode == Opcode::Cmp;
        bool first = true;
        bool previous = false;

        for(int i = 0; i < phiInstrA->OperandCount(); i++) {
            auto op = phiInstrA->GetOperand(i);

            if(op->IsIntConstant() == false) {
                return nullptr;
            }

            bool result = OrderHolds(order, op, intConst, isSigned);

            if(first) {
                previous = result;
                first = false;
            }
            else if(result != previous) {
                // The result for all incoming operands should be the same.
                return nullptr;
            }
        }

        // We can fold to a constant.
        return GetBool(previous);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::OrderHolds(OrderType order, Operand* opA, Operand* opB, 
                                bool isSigned) {
    auto intConstA = opA->As<IntConstant>();
    auto intConstB = opB->As<IntConstant>();
    DebugValidator::IsNotNull(intConstA);
    DebugValidator::IsNotNull(intConstB);

    switch(order) {
        case OrderType::Equal: {
            return IA::AreEqual(intConstA, intConstB);
        }
        case OrderType::NotEqual: {
            return IA::AreNotEqual(intConstA, intConstB);
        }
        case OrderType::Less: {
            if(isSigned) return IA::IsSmaller(intConstA, intConstB);
            else return IA::IsSmallerUnsigned(intConstA, intConstB);
        }
        case OrderType::LessOrEqual: {
            if(isSigned) return IA::IsSmallerOrEqual(intConstA, intConstB);
            else return IA::IsSmallerOrEqualUnsigned(intConstA, intConstB);
        }
        case OrderType::Greater: {
            if(isSigned) return IA::IsLarger(intConstA, intConstB);
            else return IA::IsLargerUnsigned(intConstA, intConstB);
        }
        case OrderType::GreaterOrEqual: {
            if(isSigned) return IA::IsLargerOrEqual(intConstA, intConstB);
            else return IA::IsLargerOrEqualUnsigned(intConstA, intConstB);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareQuestion(Opcode opcode, Operand* opA, 
                                               Operand* opB, OrderType order) {
    DebugValidator::IsFalse(opA->IsConstant() && opB->IsConstant());

    // Test for things like:
    // cmp gt (quest c, 5, 3), 0
    // We don't do this for floating numbers and for equality comparisons.
    if(opcode == Opcode::Fcmp) {
        return nullptr;
    }

    if((order == OrderType::Equal) || (order == OrderType::NotEqual)) {
       return nullptr;
    }

    // Check if at least one of the operands is a 'quest' instruction.
    QuestionInstr* questInstrA = nullptr;
    QuestionInstr* questInstrB = nullptr;
    IntConstant* intConst = nullptr;
    
    if(DetectQuestion(opA, opB, questInstrA, questInstrB, 
                      intConst, order) == false) {
        return nullptr;
    }

    // Now check for the possible cases.
    if(intConst) {
        // Both values need to met the comparison order
        // to be able to fold the comparison to a constant.
        auto trueConst = questInstrA->TrueOp()->As<IntConstant>();
        auto falseConst = questInstrA->FalseOp()->As<IntConstant>();
        bool isUnsigned = opcode == Opcode::Ucmp;

        switch(order) {
        case OrderType::Less: {
            if(isUnsigned) {
                if(IA::IsSmallerUnsigned(trueConst, intConst) &&
                   IA::IsSmallerUnsigned(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsLargerUnsigned(trueConst, intConst) &&
                        IA::IsLargerUnsigned(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            else {
                if(IA::IsSmaller(trueConst, intConst) &&
                   IA::IsSmaller(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsLarger(trueConst, intConst) &&
                        IA::IsLarger(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            break;
        }
        case OrderType::LessOrEqual: {
            if(isUnsigned) {
                if(IA::IsSmallerOrEqualUnsigned(trueConst, intConst) &&
                   IA::IsSmallerOrEqualUnsigned(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsLargerUnsigned(trueConst, intConst) &&
                        IA::IsLargerUnsigned(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            else {
                if(IA::IsSmallerOrEqual(trueConst, intConst) &&
                   IA::IsSmallerOrEqual(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsLarger(trueConst, intConst) &&
                        IA::IsLarger(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            break;
        }
        case OrderType::Greater: {
            if(isUnsigned) {
                if(IA::IsLargerUnsigned(trueConst, intConst) &&
                   IA::IsLargerUnsigned(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsSmallerUnsigned(trueConst, intConst) &&
                        IA::IsSmallerUnsigned(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            else {
                if(IA::IsLarger(trueConst, intConst) &&
                   IA::IsLarger(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsSmaller(trueConst, intConst) &&
                        IA::IsSmaller(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            break;
        }
        case OrderType::GreaterOrEqual: {
            if(isUnsigned) {
                if(IA::IsLargerOrEqualUnsigned(trueConst, intConst) &&
                   IA::IsLargerOrEqualUnsigned(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsSmallerUnsigned(trueConst, intConst) &&
                        IA::IsSmallerUnsigned(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            else {
                if(IA::IsLargerOrEqual(trueConst, intConst) &&
                   IA::IsLargerOrEqual(falseConst, intConst)) {
                    return GetBool(true);
                }
                else if(IA::IsSmaller(trueConst, intConst) &&
                        IA::IsSmaller(falseConst, intConst)) {
                    return GetBool(false);
                }
            }
            break;
        }
        default: DebugValidator::Unreachable();
        }
    }
    else return HandleCompareQuestionQuestion(opcode, questInstrA, 
                                              questInstrB, order);

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::DetectQuestion(Operand* opA, Operand* opB, 
                                    QuestionInstr*& questInstrA, 
                                    QuestionInstr*& questInstrB, 
                                    IntConstant*& intConst, OrderType& order) {
    // Check if we have a 'quest' instructions as one of the operands.
    if(auto questInstr = opA->DefiningInstrAs<QuestionInstr>()) {
        questInstrA = questInstr;
    }
    else intConst = opA->As<IntConstant>();

    if(auto questInstr = opB->DefiningInstrAs<QuestionInstr>()) {
        if(questInstrA) questInstrB = questInstr;
        else questInstrA = questInstr;

        // Invert the order in this case.
        if(intConst) {
            order = CmpInstrBase::InvertedOrder(order, false /* invertEquality */);
        }
    }
    else intConst = opB->As<IntConstant>();

    // If we don't have the right operands return.
    if(questInstrA == nullptr) {
        return false;
    }

    if((intConst == nullptr) && (questInstrB == nullptr)) {
        return false;
    }

    // The operands of the 'quest' instructions need to be constant.
    if((questInstrA->TrueOp()->IsConstant() && 
        questInstrA->FalseOp()->IsConstant()) == false) {
        return false;
    }

    if(questInstrB) {
        if((questInstrB->TrueOp()->IsConstant() && 
            questInstrB->FalseOp()->IsConstant()) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareQuestionQuestion(Opcode opcode, 
                                                       QuestionInstr* questInstrA,
                                                       QuestionInstr* questInstrB,
                                                       OrderType order) {
    // cmp lt (quest c1, 1, 2), (quest c2, 3, 4) -> always true
    // We don't test 'equal' and 'not equal' orders.
    if((order == OrderType::Equal) || (order == OrderType::NotEqual)) {
        return nullptr;
    }

    auto trueConstA = questInstrA->TrueOp()->As<IntConstant>();
    auto falseConstA = questInstrA->FalseOp()->As<IntConstant>();
    auto trueConstB = questInstrB->TrueOp()->As<IntConstant>();
    auto falseConstB = questInstrB->FalseOp()->As<IntConstant>();
    bool isUnsigned = opcode == Opcode::Ucmp;

    // Check if each constant from the left side satisfies the order
    // when compared with any constant from the right side.
    if(IsOrderSatisfied(trueConstA, trueConstB, falseConstB, order, isUnsigned) && 
       IsOrderSatisfied(falseConstA, trueConstB, falseConstB, order, isUnsigned)) {
        return GetBool(true);
    }

    // Try on the reverse order, so we may return 'false' as the result.
    order = CmpInstrBase::InvertedOrder(order);

    if(IsOrderSatisfied(trueConstA, trueConstB, falseConstB, order, isUnsigned) && 
       IsOrderSatisfied(falseConstA, trueConstB, falseConstB, order, isUnsigned)) {
        return GetBool(false);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::IsOrderSatisfied(IntConstant* intConstA, IntConstant* intConstB,
                                      IntConstant* intConstC, OrderType order,
                                      bool isUnsigned) {
    switch(order) {
        case OrderType::Less: {
            if(isUnsigned) return IA::IsSmallerUnsigned(intConstA, intConstB) &&
                                  IA::IsSmallerUnsigned(intConstA, intConstC);
            else return IA::IsSmaller(intConstA, intConstB) &&
                        IA::IsSmaller(intConstA, intConstC);
        }
        case OrderType::LessOrEqual: {
            if(isUnsigned) return IA::IsSmallerOrEqualUnsigned(intConstA, intConstB) &&
                                  IA::IsSmallerOrEqualUnsigned(intConstA, intConstC);
            else return IA::IsSmallerOrEqual(intConstA, intConstB) &&
                        IA::IsSmallerOrEqual(intConstA, intConstC);
        }
        case OrderType::Greater: {
            if(isUnsigned) return IA::IsLargerUnsigned(intConstA, intConstB) &&
                                  IA::IsLargerUnsigned(intConstA, intConstC);
            else return IA::IsLarger(intConstA, intConstB) &&
                        IA::IsLarger(intConstA, intConstC);
        }
        case OrderType::GreaterOrEqual: {
            if(isUnsigned) return IA::IsLargerOrEqualUnsigned(intConstA, intConstB) &&
                                  IA::IsLargerOrEqualUnsigned(intConstA, intConstC);
            else return IA::IsLargerOrEqual(intConstA, intConstB) &&
                        IA::IsLargerOrEqual(intConstA, intConstC);
        }
        default: DebugValidator::Unreachable();
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareGlobalNull(Opcode opcode, Operand* opA, 
												 Operand* opB, OrderType order) {
	// Test for the comparison of a global variable reference with 'nullptr'.
	// The address of global variables can never be 'nullptr', so we can evaluate
	// the comparison at compile time.
	VariableReference* globalVariableRef = nullptr;
	Operand* otherOp = nullptr;

	// Also look through 'ptop' conversions, because they don't change bits,
	// and through 'ptoi' conversions (when compared with 0).
	// Handles cases like '(int*)&p != NULL' or '(int)&p == 0'.
	if(auto variableRef = opA->As<VariableReference>()) {
		globalVariableRef = variableRef;
		otherOp = opB;
	}
	else if(auto variableRef = opB->As<VariableReference>()) {
		globalVariableRef = variableRef;
		otherOp = opA;
	}
	else if(auto ptopInstr = opA->DefiningInstrAs<PtopInstr>()) {
		// Strip all other 'ptop' instructions and then check the base.
		while(auto basePtopInstr = ptopInstr->TargetOp()->DefiningInstrAs<PtopInstr>()) {
			ptopInstr = basePtopInstr;
		}

		globalVariableRef = ptopInstr->TargetOp()->As<VariableReference>();
		otherOp = opB;
	}
	else if(auto ptopInstr = opB->DefiningInstrAs<PtopInstr>()) {
		// Strip all other 'ptop' instructions and then check the base.
		while(auto basePtopInstr = ptopInstr->TargetOp()->DefiningInstrAs<PtopInstr>()) {
			ptopInstr = basePtopInstr;
		}

		globalVariableRef = ptopInstr->TargetOp()->As<VariableReference>();
		otherOp = opA;
	}
	else if(auto ptoiInstrA = opA->DefiningInstrAs<PtoiInstr>()) {
		globalVariableRef = ptoiInstrA->TargetOp()->As<VariableReference>();
		otherOp = opB;
	}
	else if(auto ptoiInstrB = opB->DefiningInstrAs<PtoiInstr>()) {
		globalVariableRef = ptoiInstrB->TargetOp()->As<VariableReference>();
		otherOp = opA;
	}

	// If we don't have a variable reference, and if the variable 
	// is not global we have nothing to do.
	if((globalVariableRef == nullptr) || 
       (globalVariableRef->IsGlobalVariableRef() == false)) {
		return nullptr;
	}

	// Determine the type of the other operand.
	// Supported are only 'nullptr' and the integer constant '0'.
	if(otherOp->IsNullConstant() || MatchInt(0)(otherOp)) {
		// cmp eq G, nullptr -> false
		// cmp neq G, nullptr -> true
		// For other cases there is nothing can be done
		if(order == OrderType::Equal) {
			return GetZeroInt(otherOp);
		}
		else if(order == OrderType::NotEqual) {
			return GetOneInt(otherOp);
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareVars(Opcode opcode, Operand* opA, 
                                           Operand* opB, OrderType order) {
	// Test for comparisons involving references to global variables.
	VariableReference* variableRefA = opA->As<VariableReference>();
	VariableReference* variableRefB = opB->As<VariableReference>();

	if(variableRefA && variableRefB) {
		// Example in C: '&a == &b' or '&c == &c'.
		if(variableRefA->IsGlobalVariableRef() && variableRefB->IsGlobalVariableRef()) {
			if(variableRefA == variableRefB) {
				// The same variable is on both sides.
				return GetResultForOrder(order, OrderType::Equal);
			}
			else {
				// The address of the variables can never be the same.
				return GetResultForOrder(order, OrderType::NotEqual);
			}
		}
	}
	else if(variableRefA || variableRefB) {
		// One of the operands is a variable reference.
		// See which is the reference and which is the other operand.
		VariableReference* variableRef = nullptr;
		Operand* otherOp = nullptr;

		if(variableRefA && variableRefA->IsGlobalVariableRef()) {
			variableRef = variableRefA;
			otherOp = opB;
		}
		else if(variableRefB && variableRefB->IsGlobalVariableRef()) {
			variableRef = variableRefB;
			otherOp = opA;
		}

		if(variableRef) {
			// We have a reference to a global variable.
			if(auto result1 = HandleCompareVarAddr(opcode, variableRef, otherOp, 
							  					   order, variableRef == variableRefA)) {
				return result1;
			}
			else if(auto result2 = HandleCompareVarAggregate(opcode, variableRef, otherOp, 
															 order, variableRef == variableRefA)) {
				return result2;
			}
		}
	}

	// Test for a comparison between a 'nullptr' and 
    // an 'addr' involving a 'nullptr'.
	if(auto result = HandleCompareNullAddr(opcode, opA, opB, order)) {
		return result;
	}

	// Test for comparisons involving references to functions.
	if(auto functRefA = opA->As<FunctionReference>()) {
		if(auto functRefB = opB->As<FunctionReference>()) {
			if(functRefA->Target() == functRefB->Target()) {
				// The same function is on both sides.
				return GetResultForOrder(order, OrderType::Equal);
			}
			else {
				// Two functions reside in memory at different addresses.
				return GetResultForOrder(order, OrderType::NotEqual);
			}
		}
		else if(opB->IsNullConstant()) {
			// A function always has an address.
			return GetResultForOrder(order, OrderType::NotEqual);
		}
	}
	else if(opA->IsNullConstant() && opB->IsFunctionReference()) {
		// A function always has an address.
		return GetResultForOrder(order, OrderType::NotEqual);
	}

	// Test for cases involving the 'addr' and 'index' as both operands.
	// Example in C: '&a[4] < &a[6]' or '(p + 5) > 'p + 2'.
	if(IsAddressOrIndex(opA, opB)) {
		if(auto result = HandleCompareAddrAddr(opcode, opA, opB, order)) {
			return result;
		}	
	}
	else if(opA->DefiningInstrIs<FieldInstr>() &&
			opB->DefiningInstrIs<FieldInstr>()) {
		if(auto result = HandleCompareFieldField(opcode, opA, opB, order)) {
			return result;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareVarAddr(Opcode opcode, VariableReference* variableRef,
											  Operand* otherOp, OrderType order, 
											  bool varLeftOp) {
	// Test for cases in which 'otherOp' is an 'addr' instruction.
	// Example in C: '&a < (&a + 5)' or '&v < (NULL + 3)'.
	auto addrInstr = otherOp->DefiningInstrAs<AddressInstr>();

	if(addrInstr == nullptr) {
        return nullptr;
    }

	// cmp ORDER, variableRef, (addr BASE, INDEX)
	// We can handle here only an index that is a constant.
	auto index = addrInstr->IndexOp()->As<IntConstant>();

	if(index == nullptr) {
        return nullptr;
    }

	// The index is a constant, now check the type of the base.
	// If it's 'nullptr' and 'INDEX' is 0 then we can compute the result now.
	if(addrInstr->BaseOp()->IsNullConstant() && (index->Value() == 0)) {
		if(varLeftOp) {
            return GetResultForOrder(order, OrderType::Greater);
        }
		else return GetResultForOrder(order, OrderType::Less);
	}

	// If 'BASE' is 'variableRef', we can compute the result now,
	// based on the value of 'INDEX'.
	if(auto baseVariableRef = addrInstr->BaseOp()->As<VariableReference>()) {
		if(baseVariableRef->GetVariable() == variableRef->GetVariable()) {
			if(index->Value() == 0) {
				return GetResultForOrder(order, OrderType::Equal);
			}
			else if(index->Value() > 0) {
				// cmp ORDER &g, (addr &g, 2)
				if(varLeftOp) {
                    return GetResultForOrder(order, OrderType::Less);
                }
				else return GetResultForOrder(order, OrderType::Greater);
			}
			else {
				// cmp ORDER (addr &g, 2), &g
				if(varLeftOp) {
                    return GetResultForOrder(order, OrderType::Greater);
                }
				else return GetResultForOrder(order, OrderType::Less);
			}
		}
		
		// If they are different global variables, and 'INDEX' is 0
		// then they can't be located at the same address.
		if(index->Value() == 0) {
			return GetResultForOrder(order, OrderType::NotEqual);
		}
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareVarAggregate(Opcode opcode, 
                                                   VariableReference* variableRef,
												   Operand* otherOp, 
                                                   OrderType order, 
                                                   bool varLeftOp) {
	// Test for cases in which 'otherOp' is an 'index' or 'field' instruction.
	// Example in C: '&a < &a[5]' or '&p <= &s.x'.
	if((otherOp->DefiningInstrIs<IndexInstr>() || 
	    otherOp->DefiningInstrIs<FieldInstr>()) == false) {
		return nullptr;
	}

	// cmp ORDER G1, (index/field BASE, INDEX)
	// If 'BASE' is the same as the referenced variable, we can use
	// 'INDEX' to decide the outcome of the comparison.
	auto addrInstr = static_cast<AddressInstr*>(otherOp->DefiningInstruction());
	bool isElement = addrInstr->Is<FieldInstr>();
	auto baseVariableRef = addrInstr->BaseOp()->As<VariableReference>();

	if(baseVariableRef == nullptr) {
        return nullptr;
    }
	
	// We can handle here only an index that is a constant.
	auto index = addrInstr->IndexOp()->As<IntConstant>();

	if(index == nullptr) {
        return nullptr;
    }

	// If the variables are not the same, and the index is in the bounds
	// of the array (or record, but that is mandatory), we know that the 
	// address returned by the 'index'/'field' instruction 
    // can't be the one of the global.
	if(baseVariableRef->GetVariable() != variableRef->GetVariable()) {
        auto localVariable = baseVariableRef->GetLocalVariable(); 

        if(localVariable) {
		    if(auto arrayType = localVariable->GetType()->As<ArrayType>()) {
			    if((index->Value() >= 0) && index->Value() < arrayType->Size()) {
				    return GetResultForOrder(order, OrderType::NotEqual);
			    }
		    }
		    else if(localVariable->GetType()->Is<RecordType>()) {
			    // The index is always valid for records.
			    return GetResultForOrder(order, OrderType::NotEqual);
		    }
        }

		return nullptr;
	}
	
	// The variables are the same, try to make a decision based on the index.
	if(index->Value() == 0) {
		// The addresses are the same. If the base is a record then the offset
		// of the first element must be 0 for the addresses to be the same.
		const Type* baseType = baseVariableRef->GetVariable()->GetType();

		if(const RecordType* record = baseType->As<RecordType>()) {
			if(record->Fields()[0].FieldOffset == 0) {
				return GetResultForOrder(order, OrderType::Equal);
			}
									
			// Other cases should not appear here.
		}
		else return GetResultForOrder(order, OrderType::Equal);
	}
	else if(index->Value() > 0) {
		// The address of the variable is smaller than the one returned by the 
		// 'index'/field instruction. For records we must test the offset of the field,
		// because in case of unions, the offset can be 0 for more fields.
		const Type* baseType = baseVariableRef->GetVariable()->GetType();
								
		if(const RecordType* record = baseType->As<RecordType>()) {
			if(record->Fields()[(int)index->Value()].FieldOffset == 0) {
				return GetResultForOrder(order, OrderType::Equal);
			}
		}
								
		if(varLeftOp) {
            return GetResultForOrder(order, OrderType::Less);
        }
		else return GetResultForOrder(order, OrderType::Greater);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleCompareNullAddr(Opcode opcode, Operand* opA, 
                                               Operand* opB, OrderType order) {
	// Test for a comparison between a 'nullptr' and an 'addr' involving a 'nullptr'.
	// Example in C: '(&a + 4) != NULL' or 'NULL == NULL' or 'NULL != (NULL + 4')'.
	AddressInstr* addrInstr = nullptr;
	bool addrOnLeft = true;

	if(opB->IsNullConstant() && 
       (addrInstr = opA->DefiningInstrAs<AddressInstr>())) {
        addrOnLeft = true;
    }
	else if(opA->IsNullConstant() && 
           (addrInstr = opB->DefiningInstrAs<AddressInstr>())) {
		addrOnLeft = false;
	}

	// If there is no address instruction we have nothing to do.
	if(addrInstr == nullptr) {
        return nullptr;
    }

	// Test for 'nullptr' as the base of the 'addr'.
	if(addrInstr->BaseOp()->IsNullConstant()) {
		// If the index is 0 then the result is still 'nullptr'.
		// Else the address returned by 'addr' is greater than
        // 'nullptr' in all cases.
		if(auto index = addrInstr->IndexOp()->As<IntConstant>()) {
			if(index->Value() == 0) {
				return GetResultForOrder(order, OrderType::Equal);
			}
			else if(addrOnLeft) {
                return GetResultForOrder(order, OrderType::Greater);
            }
			else return GetResultForOrder(order, OrderType::Less);
		}
	}
	else if(addrInstr->BaseOp()->IsVariableReference()) {
		// No matter what the offset is, 'nullptr' will never be equal to the
		// address of the referenced variable.
		if(addrOnLeft) {
            return GetResultForOrder(order, OrderType::Greater);
        }
		else return GetResultForOrder(order, OrderType::Less);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleCompareAddrAddr(Opcode opcode, Operand* opA,
                                               Operand* opB, OrderType order) {
	// Compare the index of the first 'addr' with the second one.
	// If it's smaller, set the result 'Less", if it's larger, it's 'Greater'.
	// If it's equal to the other one, we need to test for a 'addr' or 'index' base.
	auto addrInstrA = static_cast<AddressInstr*>(opA->DefiningInstruction());
	auto addrInstrB = static_cast<AddressInstr*>(opB->DefiningInstruction());
		
	while(true) {
		// Note that we can compare the indices only if
        // they are integer constants.
		auto indexA = addrInstrA->IndexOp()->As<IntConstant>();
		auto indexB = addrInstrB->IndexOp()->As<IntConstant>();

		if(indexA == nullptr || indexB == nullptr) {
            break;
        }

		// Compare the values of the indices.
		if(SameAddressBase(addrInstrA, addrInstrB)) {
			if(IA::IsSmaller(indexA, indexB)) {
				// (addr p, 2) < (addr p, 5)
				return GetResultForOrder(order, OrderType::Less);
			}
			else if(IA::IsLarger(indexA, indexB)) {
				// (addr p, 4) > (addr p, 2)
				return GetResultForOrder(order, OrderType::Greater);
			}
			else {
				// (addr p, 2) == (addr p, 2)
				return GetResultForOrder(order, OrderType::Equal);
			}
		}
			
		// The base of the 'addr'/'index' instructions is not the same.
		// Test if we have another level of these instructions.
		Operand* baseA = addrInstrA->BaseOp();
		Operand* baseB = addrInstrB->BaseOp();
							
		if(IsAddressOrIndex(baseA, baseB)) {
			addrInstrA = static_cast<AddressInstr*>(baseA->DefiningInstruction());
			addrInstrB = static_cast<AddressInstr*>(baseB->DefiningInstruction());
		}
		else {
			// The bases differ, so no relationship can be obtained.
			break;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* ConstantFolder::HandleCompareFieldField(Opcode opcode, Operand* opA, 
                                                 Operand* opB, OrderType order) {
	// Test for comparisons involving two 'field' instructions.
	// Example in C: '&a.x < &a.y'.
	auto fieldA = opA->DefiningInstrAs<FieldInstr>();
	auto fieldB = opB->DefiningInstrAs<FieldInstr>();
	
	// The base for both must be a global variable reference.
	auto variableRefA = fieldA->BaseOp()->As<VariableReference>();
	auto variableRefB = fieldB->BaseOp()->As<VariableReference>();

	if(((variableRefA && variableRefA->IsGlobalVariableRef()) &&
		(variableRefB && variableRefB->IsGlobalVariableRef())) == false) {
		return nullptr;
	}

	if(variableRefA != variableRefB) {
		// We can't determine the relationship between different variables.
		return nullptr;
	}

	// Make a decision based on the index.
	auto indexA = fieldA->IndexOp()->As<IntConstant>();
	auto indexB = fieldB->IndexOp()->As<IntConstant>();
	auto recordType = variableRefA->GetGlobalVariable()->GetType()->As<RecordType>();

	if(IA::AreEqual(indexA, indexB)) {
		// Both 'field' instructions return the same address.
		return GetResultForOrder(order, OrderType::Equal);
	}
	else {
		// Because more fields of the record can have the same offset
		// the decision is based on the offset, and not on the member index.
		__int64 offsetA = recordType->GetFieldOffset(indexA->Value());
		__int64 offsetB = recordType->GetFieldOffset(indexB->Value());
		
		if(offsetA < offsetB) {
            return GetResultForOrder(order, OrderType::Less);
        }
		else if(offsetA > offsetB) {
            return GetResultForOrder(order, OrderType::Greater);
        }
		else return GetResultForOrder(order, OrderType::Equal);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareLoadLoad(Opcode opcode, Operand* opA,    
                                               Operand* opB, OrderType order) {
    // *p !=/</>   *p -> 0
    // *p ==/<=/>= *p -> 1
    // Note that this is 100% true only if the loads are
    // in the same block and no store/call is between them,
    // else there could be a path where 'p' is assigned a value.
    auto loadA = opA->DefiningInstrAs<LoadInstr>();
    auto loadB = opB->DefiningInstrAs<LoadInstr>();

    if((loadA && loadB) == false) {
        return nullptr;
    }

    if(loadA->IsVolatile() || loadB->IsVolatile()) {
        return nullptr;
    }

    if((loadA->SourceOp() == loadB->SourceOp()) &&
       (loadA->ParentBlock() == loadB->ParentBlock()) &&
       (IsStoreCallBetweenLoads(loadA, loadB) == false)) {
        switch(order) {
            case OrderType::Equal:
            case OrderType::LessOrEqual:
            case OrderType::GreaterOrEqual: {
                return GetBool(true);
            }
            case OrderType::NotEqual:
            case OrderType::Less:
            case OrderType::Greater: {
                return GetBool(false);
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::IsStoreCallBetweenLoads(LoadInstr* loadA, LoadInstr* loadB) {
    DebugValidator::IsNotNull(loadA);
    DebugValidator::IsNotNull(loadB);
    DebugValidator::AreEqual(loadA->ParentBlock(), loadB->ParentBlock());

    // Walk from one load to the first instruction in the block
    // and stop when we find the other one, reporting if a
    // 'store' or 'call' was found between.
    bool sawStoreOrCall = false;

    for(Instruction* instr = loadB; instr; instr = instr->PreviousInstruction()) {
        if(instr->PreviousInstruction() == loadA) {
            return sawStoreOrCall;
        }
        else if(instr->IsStore() || instr->IsCall()) {
            sawStoreOrCall = true;
        }
    }
    
    // Check the other way around.
    sawStoreOrCall = false;

    for(Instruction* instr = loadA; instr; instr = instr->PreviousInstruction()) {
        if(instr->PreviousInstruction() == loadB) {
            return sawStoreOrCall;
        }
        else if(instr->IsStore() || instr->IsCall()) {
            sawStoreOrCall = true;
        }
    }

    DebugValidator::Unreachable();
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareGlobalLoad(Opcode opcode, Operand* opA,    
                                                 Operand* opB, OrderType order) {
    // Handle the case of a constant compared with a value
    // loaded from a global variable for which the possible
    // constant values are known or the case of two such variables.
    // For example, if we know that 'a = {1 or 2}' and 'b = {4 or 5 or 6}'
    // 'a < b' is always true and 'a == b' is always false.
    auto loadA = opA->DefiningInstrAs<LoadInstr>();
    auto loadB = opB->DefiningInstrAs<LoadInstr>();

    if((loadA || loadB) == false) {
        return nullptr;
    }
    else if((loadA && (loadA->SourceOp()->IsGlobalVariableRef() == false)) || 
            (loadB && (loadB->SourceOp()->IsGlobalVariableRef() == false))) {
        return nullptr;
    }

    if(loadA && loadB) {
        return HandleCompareGlobalLoads(opcode, loadA->SourceOp(),
                                        loadB->SourceOp(), order);
    }
	else if((loadA == nullptr) || (opB->IsIntConstant() == false)) {
		// It is expected for the constant to be on the right side.
		return nullptr;
	}

    // Check if we have a loaded value compared with an integer constant.
    // Note that it is expected that the constant is on the right side.
    auto intConstant = opB->As<IntConstant>();
    auto globalConstants = GetGlobalConstants(loadA->SourceOp());
    
    if(globalConstants == nullptr) {
        return nullptr;
    }

    // Check if the order holds for each pair of constants.
    bool orderHolds = true;
    IntConstantList listA;
    IntConstantList listB;

    globalConstants->ForEachConstant([&](Constant* constant, float p) -> bool {
        if(auto intContant = constant->As<IntConstant>()) {
            listA.Add(intContant);
            return true;
        }
        else {
            orderHolds = false;
            return false;
        }
    });

    if(orderHolds) {
        listB.Add(intConstant);
        return ComputeResultForAllPairs(listA, listB, opcode, order);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareGlobalLoads(Opcode opcode, Operand* sourceA, 
                                                  Operand* sourceB, OrderType order) {
    DebugValidator::IsTrue(sourceA->IsGlobalVariableRef());
    DebugValidator::IsTrue(sourceB->IsGlobalVariableRef());

    // Check if the possible constant values for both
    // global variables are known. If yes check the order
    // of each possible pair of constants from both variables.
    auto globalConstantsA = GetGlobalConstants(sourceA);
    auto globalConstantsB = GetGlobalConstants(sourceB);

    if((globalConstantsA && globalConstantsB) == false) {
        return nullptr;
    }

    bool orderHolds = true;
    IntConstantList listA;
    IntConstantList listB;

    globalConstantsA->ForEachConstant([&](Constant* constant, float p) -> bool {
        if(auto intContant = constant->As<IntConstant>()) {
            listA.Add(intContant);
            return true;
        }
        else {
            orderHolds = false;
            return false;
        }
    });

    globalConstantsB->ForEachConstant([&](Constant* constant, float p) -> bool {
        if(auto intContant = constant->As<IntConstant>()) {
            listB.Add(intContant);
            return true;
        }
        else {
            orderHolds = false;
            return false;
        }
    });

    if(orderHolds) {
        return ComputeResultForAllPairs(listA, listB, opcode, order);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareParameters(Opcode opcode, Operand* opA,    
                                                 Operand* opB, OrderType order) {
    // Handle the case of a parameter compared with an integer constant
    // or two compared parameters for which the possible values are known.
    auto parameterA = opA->As<Parameter>();
    auto parameterB = opB->As<Parameter>();

    if(parameterA && parameterB) {
        return HandleCompareParameters(opcode, parameterA, parameterB, order);
    }

    // Check if the order holds for each pair of constants.
    auto intConstant = opB->As<IntConstant>();
    auto constantsTag = GetParameterConstants(opA);

    if((intConstant && constantsTag) == false) {
        return nullptr;
    }

    bool orderHolds = true;
    IntConstantList listA;
    IntConstantList listB;

    constantsTag->ForEachConstant([&](Constant* constant, FunctionReference* f) -> bool {
        if(auto intContant = constant->As<IntConstant>()) {
            listA.Add(intContant);
            return true;
        }
        else {
            orderHolds = false;
            return false;
        }
    });

    if(orderHolds) {
        listB.Add(intConstant);
        return ComputeResultForAllPairs(listA, listB, opcode, order);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleCompareParameters(Opcode opcode, Parameter* parameterA, 
                                                 Parameter* parameterB, OrderType order) {
    // Check if the possible constant values for both parameters are known.
    // If yes check the order of each possible pair of parameter values.
    auto constantsTagA = GetParameterConstants(parameterA);
    auto constantsTagB = GetParameterConstants(parameterB);

    if(constantsTagA && constantsTagB) {
        bool orderHolds = true;
        IntConstantList listA;
        IntConstantList listB;

        constantsTagA->ForEachConstant([&](Constant* constant, FunctionReference* f) -> bool {
            if(auto intContant = constant->As<IntConstant>()) {
                listA.Add(intContant);
                return true;
            }
            else {
                orderHolds = false;
                return false;
            }
        });

        constantsTagB->ForEachConstant([&](Constant* constant, FunctionReference* f) -> bool {
            if(auto intContant = constant->As<IntConstant>()) {
                listB.Add(intContant);
                return true;
            }
            else {
                orderHolds = false;
                return false;
            }
        });

        if(orderHolds) {
            return ComputeResultForAllPairs(listA, listB, opcode, order);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::ComputeResultForAllPairs(IntConstantList& listA, 
                                                  IntConstantList& listB, 
                                                  Opcode opcode, OrderType order) {
    // First check if all pairs formed from constants taken from both lists
    // hold the required order. If not and the order is not equality
    // we also try the inverted order.
    bool orderHolds = true;
    bool isSigned = opcode == Opcode::Cmp;

    // If too many constants need to be compared it's unlikely
    // that we can reach a result, so give up now.
    if((listA.Count() * listB.Count()) > 32) {
        return nullptr;
    }
    else if(OrderHoldsForAllPairs(listA, listB, order, isSigned)) {
        return GetBool(true);
    }
    else if((order == OrderType::Equal) || (order == OrderType::NotEqual)) {
        return nullptr;
    }

    // Test using the inverted order.
    auto invertedOrder = CmpInstrBase::InvertedOrder(order);
    
    if(OrderHoldsForAllPairs(listA, listB, invertedOrder, isSigned)) {
        return GetBool(false);
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::OrderHoldsForAllPairs(IntConstantList& listA, 
                                           IntConstantList& listB, 
                                           OrderType order, bool isSigned) {
    // Check if the order holds for all possible pairs.
    for(int i = 0; i < listA.Count(); i++) {
        for(int j = 0; j < listB.Count(); j++) {
            if(OrderHolds(order, listA[i], listB[j], isSigned) == false) {
                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GlobalConstantsTag* ConstantFolder::GetGlobalConstants(Operand* op) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsTrue(op->IsGlobalVariableRef());

    auto globalVariable = op->As<VariableReference>()->GetGlobalVariable();
    auto tag = globalVariable->GetTag<GlobalConstantsTag>();

    if(tag && tag->HasOnlyConstants()) {
        return tag;
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -    
ParameterConstantsTag* ConstantFolder::GetParameterConstants(Operand* op) {
    DebugValidator::IsNotNull(op);

    if(auto parameter = op->As<Parameter>()) {
        auto tag = parameter->GetVariable()->GetTag<ParameterConstantsTag>();

        if(tag && tag->IsSafeToUse()) {
            return tag;
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::SameAddressBase(AddressInstr* instrA, AddressInstr* instrB) {
	// Test for a base that is a variable.
	if(auto variableRefA = instrA->BaseOp()->As<VariableReference>()) {
		if(auto variableRefB = instrB->BaseOp()->As<VariableReference>()) {
			return variableRefA->GetVariable() == variableRefB->GetVariable();
		}
	}

	// Test for a base that is an integer converted to pointer.
	// (addr (itop 0, int32*), 5), (addr (itop 0, int32*), 6)
	if(auto itopInstrA = instrA->BaseOp()->DefiningInstrAs<ItopInstr>()) {
		if(auto itopInstrB = instrB->BaseOp()->DefiningInstrAs<ItopInstr>()) {
			// The pointer type to which we cast must be the same.
			if(itopInstrA->CastType() != itopInstrB->CastType()) {
                return false;
            }

			// The converted values must be the same constants.
			if(auto intConstA = itopInstrA->TargetOp()->As<IntConstant>()) {
				if(auto intConstB = itopInstrB->TargetOp()->As<IntConstant>()) {
					return IA::AreEqual(intConstA, intConstB);
				}
			}
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::GetResultForOrder(OrderType requiredOrder, 
                                           OrderType order) {
	// We know the order established between the instruction operands,
	// now we need to combine it with the required order.
	switch(order) {
		case OrderType::Equal: {
			if((requiredOrder == OrderType::Equal)       || 
               (requiredOrder == OrderType::LessOrEqual) ||
			   (requiredOrder == OrderType::GreaterOrEqual)) {
				// The result is 'true'.
				return GetBool(true);
			}
			else {
				// The result is 'false' for all other cases.
				return GetBool(false);
			}
			break;
		}
		case OrderType::NotEqual: {
			if(requiredOrder == OrderType::NotEqual) {
				return GetBool(true);
			}
			else if(requiredOrder == OrderType::Equal) {
				return GetBool(false);
			}
			break;
		}
		case OrderType::Less: {
			if((requiredOrder == OrderType::Less)        || 
               (requiredOrder == OrderType::LessOrEqual) ||
			   (requiredOrder == OrderType::NotEqual)) {
				return GetBool(true);
			}
			else if((requiredOrder == OrderType::Greater) || 
                    (requiredOrder == OrderType::Equal)   ||
				    (requiredOrder == OrderType::GreaterOrEqual)) {
				return GetBool(false);
			}
			break;
		}
		case OrderType::LessOrEqual: {
			if((requiredOrder == OrderType::Less) || 
               (requiredOrder == OrderType::LessOrEqual)) {
				return GetBool(true);
			}
			else if(requiredOrder == OrderType::Greater) {
				return GetBool(false);
			}
			break;
		}
		case OrderType::Greater: {
			if((requiredOrder == OrderType::Greater)        || 
               (requiredOrder == OrderType::GreaterOrEqual) ||
			   (requiredOrder == OrderType::NotEqual)) {
				return GetBool(true);
			}
			else if((requiredOrder == OrderType::Less)        || 
                    (requiredOrder == OrderType::LessOrEqual) ||
					(requiredOrder == OrderType::Equal)) {
				return GetBool(false);
			}
			break;
		}
		case OrderType::GreaterOrEqual: {
			if((requiredOrder == OrderType::Greater) || 
               (requiredOrder == OrderType::GreaterOrEqual)) {
				return GetBool(true);
			}
			else if(requiredOrder == OrderType::Less) {
				return GetBool(false);
			}
			break;
		}
	}

	// Nothing could be decided.
	return nullptr;
}

} // namespace Analysis