// ConstantFolder.cpp
// Copyright (c) Lup Gratian
//
// Implements the constant folder.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantFolder.hpp"

namespace Analysis {

Operand* ConstantFolder::Fold(Instruction* instr, Block* testBlock) {
	DebugValidator::IsNotNull(instr);
	
	// Select what to do based on the category, then based on the opcode.
	switch(instr->Category()) {
		case InstructionCategory::Arithmetic: {
			auto arithInstr = static_cast<ArithmeticInstr*>(instr);
			FloatMode mode = FloatMode::Exact;

			// If this is a floating-point instruction we used its associated mode,
			// because it may allow more constant-like expressions to be folded.
			if(arithInstr->IsFloatArithmetic()) {
				mode = arithInstr->GetFPMode();
			}

			return HandleArithmetic(arithInstr->GetOpcode(), 
                                    arithInstr->LeftOp(),
									arithInstr->RightOp(), mode,
                                    SelectBlock(testBlock, arithInstr));
		}
		case InstructionCategory::Logical: {
			auto logicalInstr = static_cast<LogicalInstr*>(instr);
			return HandleLogical(logicalInstr->GetOpcode(), 
                                 logicalInstr->LeftOp(),
								 logicalInstr->RightOp(),
                                 SelectBlock(testBlock, logicalInstr));
		}
		case InstructionCategory::Conversion: {
			auto convInstr = static_cast<ConversionInstr*>(instr);
			return HandleConversion(convInstr->GetOpcode(), 
                                    convInstr->TargetOp(),
									convInstr->CastType(),
                                    SelectBlock(testBlock, convInstr));
		}
		case InstructionCategory::Control: {
			switch(instr->GetOpcode()) {
				case Opcode::Address: {
					auto addrInstr = static_cast<AddressInstr*>(instr);
					return HandleAddress(addrInstr->GetOpcode(), 
                                         addrInstr->BaseOp(),
										 addrInstr->IndexOp(),
                                         SelectBlock(testBlock, addrInstr));
				}
			}
		}
		case InstructionCategory::Other: {
			switch(instr->GetOpcode()) {
				case Opcode::Cmp:
				case Opcode::Ucmp:
				case Opcode::Fcmp: {
					auto cmpInstr = static_cast<CmpInstrBase*>(instr);
					return HandleCompare(cmpInstr->GetOpcode(), 
                                         cmpInstr->LeftOp(),
										 cmpInstr->RightOp(),
                                         cmpInstr->Order(),
                                         SelectBlock(testBlock, cmpInstr));
				}
				case Opcode::Load: {
					// Try to load the operand if it targets a global variable.
					auto loadInstr = static_cast<LoadInstr*>(instr);
					return HandleLoad(loadInstr->SourceOp());
				}
				case Opcode::Call: {
					// Tries to evaluate math functions from the standard library.
					auto callInstr = static_cast<CallInstr*>(instr);
					return HandleCall(callInstr, nullptr /* replacementArgs */);
				}
                case Opcode::Question: {
                    return HandleQuestion(instr->As<QuestionInstr>());
                }
                case Opcode::Phi: {
                    return HandlePhi(instr->As<PhiInstr>());
                }
			}
		}
	}
	
	// This means that the instruction is not supported.
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldBinary(Opcode opcode, Operand* opA, 
									Operand* opB, Block* block, FloatMode mode) {
	DebugValidator::IsNotNull(opA);
	DebugValidator::IsNotNull(opB);

	switch(Instruction::Category(opcode)) {
		case InstructionCategory::Arithmetic: {
			return HandleArithmetic(opcode, opA, opB, mode, block);
		}
		case InstructionCategory::Logical: {
			return HandleLogical(opcode, opA, opB, block);
		}
	}

	// This is not a binary instruction!
	DebugValidator::Unreachable();
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldCompare(Opcode opcode, Operand* opA, Operand* opB,
									 OrderType order, Block* block) {
	DebugValidator::IsNotNull(opA);
	DebugValidator::IsNotNull(opB);
	return HandleCompare(opcode, opA, opB, order, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldConversion(Opcode opcode, Operand* op, 
                                        const Type* castType, Block* block) {
	DebugValidator::IsNotNull(op);
	DebugValidator::IsNotNull(castType);
	return HandleConversion(opcode, op, castType, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldAddress(AddressInstr* instr, Block* block) {
	DebugValidator::IsNotNull(instr);
	return HandleAddress(instr->GetOpcode(), instr->BaseOp(), 
                         instr->IndexOp(), block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldAddress(Operand* baseOp, Operand* indexOp,
                                     Block* block) {
    DebugValidator::IsNotNull(baseOp);
    DebugValidator::IsNotNull(indexOp);
    return HandleAddress(Opcode::Address, baseOp, indexOp, block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldCall(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	return HandleCall(instr, nullptr /* replacementArgs */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldCall(CallInstr* instr, ArgumentList* replacementArgs) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsNotNull(replacementArgs);

    return HandleCall(instr, replacementArgs);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldLoad(Operand* sourceOp) {
	DebugValidator::IsNotNull(sourceOp);
	return HandleLoad(sourceOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldLoad(LoadInstr* instr) {
	DebugValidator::IsNotNull(instr);
	return HandleLoad(instr->SourceOp());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::FoldLoad(Operand* op, const Type* loadType, __int64 offset) {
    DebugValidator::IsNotNull(op);
    DebugValidator::IsNotNull(loadType);
    return LoadFromAddress(op, loadType, offset);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* ConstantFolder::FoldBranching(Instruction* instr, Operand* conditionOp) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(instr->IsBranching());
    DebugValidator::IsNotNull(conditionOp);

    // If the instruction is not a branching instruction with
	// a constant condition operand ignore it.
	if(instr->IsGoto() || instr->IsReturn()) {
		return nullptr;
	}

	// We may have a 'phi' instruction that has constants as the incoming operands,
    // or a 'quest' that selects between constants.
	IntConstant* intConditionOp = nullptr;
	PhiInstr* conditionPhiInstr = nullptr;
    QuestionInstr* conditionQuestInstr = nullptr;

    if(instr->IsIf() && conditionOp->DefiningInstrIs<CmpInstrBase>()) {
        // The most frequent case is a simple comparison.
        auto ifInstr = instr->As<IfInstr>();
        auto cmpInstr = conditionOp->DefiningInstrAs<CmpInstrBase>();

        if(auto result = Fold(cmpInstr, instr->ParentBlock())) {
            if(result->IsZeroInt()) {
			    return ifInstr->FalseTargetOp();
            }
		    else return ifInstr->TrueTargetOp();
        }
    }
	if(auto phiInstr = conditionOp->DefiningInstrAs<PhiInstr>()) {
		if(phiInstr->IsConstant()) { // Same constant?
			intConditionOp = phiInstr->GetConstant()->As<IntConstant>();
		}
		else if(phiInstr->HasOnlyConstants()) {
			// We may obtain some information if all operands are constants.
			conditionPhiInstr = phiInstr; 
		}
	}
    else if(auto questInstr = conditionOp->DefiningInstrAs<QuestionInstr>()) {
        if(questInstr->HasConstantOperands()) {
            conditionQuestInstr = questInstr;
        }
    }
    else intConditionOp = AsIntConstant(conditionOp, instr->ParentBlock());

	if(conditionPhiInstr || conditionQuestInstr || intConditionOp) {
	    // Try to simplify the branching instruction.
	    if(auto ifInstr = instr->As<IfInstr>()) {
		    return HandleIf(ifInstr, intConditionOp, conditionPhiInstr, 
                            conditionQuestInstr);
	    }
	    else if(auto switchInstr = instr->As<SwitchInstr>()) {
		    return HandleSwitch(switchInstr, intConditionOp, 
                                conditionPhiInstr, conditionQuestInstr);
	    }
    }
    else {
        // If nothing matched give up we can try using Operand Info.
        // For example, if we know that at least one bit is 1 
        // then the true branch of an 'if' will always be taken.
        if(auto ifInstr = instr->As<IfInstr>()) {
            return HandleIfOperandInfo(ifInstr);
        }
    }

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* ConstantFolder::FoldBranching(Instruction* instr) {
	DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(instr->IsBranching());

    if(instr->IsIf() || instr->IsSwitch()) {
        return FoldBranching(instr, instr->GetSourceOp(0));
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 BlockReference* ConstantFolder::HandleIf(IfInstr* instr, IntConstant* intConditionOp, 
                                          PhiInstr* conditionPhiInstr, 
                                          QuestionInstr* conditionQuestInstr) {
	if(intConditionOp) {
        // If the condition operand is 0 the 'false' branch is taken,
	    // otherwise the 'true' branch.
		if(intConditionOp->Value() == 0) {
			return instr->FalseTargetOp();
		}
		else return instr->TrueTargetOp();
	}
	else if(conditionPhiInstr) {
		// All the operands of the 'phi' are constants. If all are different
		// from zero then we know for sure that the 'true' branch is taken.
		for(int i = 0; i < conditionPhiInstr->OperandCount(); i++) {
			if(conditionPhiInstr->GetOperand(i)->IsZeroInt()) {
				// We must presume that this path may be taken.
				return nullptr;
			}
		}

		return instr->TrueTargetOp();
	}
    else {
        // If both operands of a 'quest' are constants different from zero
        // we know that the 'true' branch will always be taken.
        if((conditionQuestInstr->TrueOp()->IsZeroInt() ||
            conditionQuestInstr->FalseOp()->IsZeroInt()) == false) {
            return instr->TrueTargetOp();
        }
    }

    return nullptr;
 }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* ConstantFolder::HandleSwitch(SwitchInstr* instr, 
                                             IntConstant* intConditionOp, 
                                             PhiInstr* conditionPhiInstr, 
                                             QuestionInstr* conditionQuestInstr) {
    if(intConditionOp) {
		// Try to find a 'case' label that has the value of the condition operand.
		// If no such label is found the 'default' target is chosen.
		auto& caseList = instr->CaseList();
		__int64 conditionValue = intConditionOp->Value();

		for(int i = 0; i < caseList.Count(); i++) {
			if(caseList[i].Value == conditionValue) {
				// Found a matching 'case' label.
				return caseList[i].Target;
			}
		}

		// No matching case label was found, use the default one.
		return instr->DefaultTargetOp();
	}
	else if(conditionPhiInstr || conditionQuestInstr) {
        // We check if any of the known operands match the 'case' values.
        // Make a list with the 'phi' incoming operands/'quest' operands.
        StaticList<Operand*, 16> ops;

        if(conditionPhiInstr) {
            for(int i = 0; i < conditionPhiInstr->OperandCount(); i++) {
                ops.Add(conditionPhiInstr->GetOperand(i));
            }
        }
        else {
            ops.Add(conditionQuestInstr->TrueOp());
            ops.Add(conditionQuestInstr->FalseOp());
        }

		// Make a dictionary with all the case values.
		auto& caseList = instr->CaseList();
		Dictionary<__int64, SwitchCase> caseValues(caseList.Count());

		for(int i = 0; i < caseList.Count(); i++) {
			__int64 value = caseList[i].Value;
			caseValues.Add(value, caseList[i]);
		}

		// 1. If none of the operands are among the 'case' values,
		// we know for sure that the default target is taken.
		bool anyCaseValueOp = false;

        for(int i = 0; i < ops.Count(); i++) {
			auto intConst = ops[i]->As<IntConstant>();

            if(intConst == nullptr) {
                return nullptr;
            }
				
			if(caseValues.ContainsKey(intConst->Value())) {
				anyCaseValueOp = true;
				break;
			}
		}

		if(anyCaseValueOp == false) {
			// None of the operands can select anything other
            // than the default target.
			return instr->DefaultTargetOp();
		}

		// 2. If all the operands map to 'case' values that have the same
		// target block, we know for sure the target is that block.
		auto intConst = ops[0]->As<IntConstant>();
        if(intConst == nullptr) return nullptr;
		
        if(caseValues.ContainsKey(intConst->Value()) == false) {
            return nullptr;
        }

		auto target = caseValues[intConst->Value()].Target;

		for(int i = 1; i < ops.Count(); i++) {
			auto intConst = ops[i]->As<IntConstant>();

			if(intConst == nullptr) {
                return nullptr;
            }

            if(caseValues.ContainsKey(intConst->Value()) == false) {
                return nullptr;
            }

			if(caseValues[intConst->Value()].Target != target) {
				// The target of the 'case' is not the same, give up.
				return nullptr;
			}
		}
			
		// We're sure the same 'case' is targeted.
		return target;
	}

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* ConstantFolder::HandleIfOperandInfo(IfInstr* instr) {
    OperandInfo opInfo(irGen_->GetUnit(), target_);

    if(opInfo.IsNotZero(instr->ConditionOp(), 
                        instr->ParentBlock())) {
        return instr->TrueTargetOp();
    }
    else if(opInfo.IsZero(instr->ConditionOp(),
                          instr->ParentBlock())) {
        return instr->FalseTargetOp();
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* ConstantFolder::FoldBranching(Block* block) {
	DebugValidator::IsNotNull(block);

	if(auto branchInstr = block->BranchInstruction()) {
		return FoldBranching(branchInstr);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleConversion(Opcode opcode, Operand* op, 
                                          const Type* castType, Block* block) {
    // Test for an undefined target operand.
	if(op->IsUndefinedConstant()) {
		// We can presume that the result is 0 for integer extensions.
		if((opcode == Opcode::Sext) || (opcode == Opcode::Zext)) {
			return GetZeroInt(op);
		}
		else return GetUndefined(op);
	}

	// If the target operand is a constant we can evaluate the cast now.
	if(op->IsConstant()) {
		if(auto intType = castType->As<IntegerType>()) {
			// Something is cast to an integer.
			if(auto intConst = AsIntConstant(op, block)) {
				__int64 result;

				switch(opcode) {
					case Opcode::Sext:  { result = IA::Sext(intConst, intType);  break; }
					case Opcode::Zext:  { result = IA::Zext(intConst, intType);  break; }
					case Opcode::Trunc: { result = IA::Trunc(intConst, intType); break; }
                    case Opcode::Ptoi:  { result = IA::LimitToType(intConst, intType); break; }
					default: DebugValidator::Unreachable();
				}

				return irGen_->GetIntConst(intType, result);
			}
			else if(auto floatConst = op->As<FloatConstant>()) {
				__int64 result;

				switch(opcode) {
					case Opcode::Ftoi:  { result = FA::Ftoi(floatConst, intType);  break; }
					case Opcode::Ftoui: { result = FA::Ftoui(floatConst, intType); break; }
					default: DebugValidator::Unreachable();
				}

				return irGen_->GetIntConst(intType, result);
			}
			else {
				// This must be a 'pointer-to-int' cast using 'nullptr'.
				DebugValidator::IsTrue(opcode == Opcode::Ptoi);
				DebugValidator::IsTrue(op->IsNullConstant());
				return GetZeroInt(intType);
			}
		}
		else if(auto floatType = castType->As<FloatingType>()) {
			// Something is cast to a floating value.
			double result;

			if(auto intConst = AsIntConstant(op, block)) {
				switch(opcode) {
					case Opcode::Itof:  { result = IA::Itof(intConst, floatType);  break; }
					case Opcode::Uitof: { result = IA::Uitof(intConst, floatType); break; }
					default: DebugValidator::Unreachable();
				}
			}
			else if(auto floatConst = op->As<FloatConstant>()) {
				switch(opcode) {
					case Opcode::Ftrunc: { result = FA::Ftrunc(floatConst, floatType); break; }
					case Opcode::Fext:   { result = FA::Fext(floatConst, floatType);   break; }
					default: DebugValidator::Unreachable();
				}
			}
			else DebugValidator::Unreachable(); // No other variant is possible.

			return irGen_->GetFloatingConst(floatType, result);
		}
		else if(auto pointerType = castType->As<PointerType>()) {
			// Something is cast to a pointer.
			switch(opcode) {
				case Opcode::Itop: {
					// The only value we can cast is 0, which becomes 'nullptr'.
                    auto intConst = AsIntConstant(op, block);

                    if(intConst && intConst->IsZero()) {
						return irGen_->GetNullConst(pointerType);
					}
					break;
				}
				case Opcode::Ptop: {
					// Try to fold a 'pointer-to-pointer' cast.
					// If we cast to the same pointer type we can eliminate the cast.
					if(pointerType == op->GetType()) {
						return op;
					}

					// If we cast a 'nullptr' to a 'nullptr' having a different type
					// we can return directly the 'nullptr' having the new type.
					if(op->IsNullConstant()) {
						return irGen_->GetNullConst(pointerType);
					}
					break;
				}
			}
		}
	}

	// Test for expressions that compute sizeof/offsetof/alignof.
	if(opcode == Opcode::Ptoi) {
		if(auto result = HandleConversionPtoi(opcode, op, castType)) {
			return result;
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleConversionPtoi(Opcode opcode, Operand* op, 
											  const Type* castType) {
	auto intType = castType->As<IntegerType>();
	NullConstant* ptrOp;
	IntConstant* indexOp;

	// Test for a sizeof-like expression.
	// Example in C: (int)((X*)0 + 1)
	// ptoi (addr (nullptr, INDEX), int32
	if(Match<AddressInstr>(MatchNullptr(&ptrOp), MatchIC(&indexOp))(op)) {
		// Compute the size of the object and return it.
		auto type = ptrOp->GetType()->As<PointerType>()->PointeeType();
		__int64 size = OperandInfo::Size(ptrOp, target_);
		__int64 index = indexOp->As<IntConstant>()->Value();
		__int64 result = IA::Mul(size, index, intType->GetSubtype());
		return irGen_->GetIntConst(intType, result);
	}

	// Test for a offsetof-like expression.
	// Example in C: '(int)(&((X*)0)->Field)' or '(int)((char*)&((X*)0)->Field)'
	// 'ptoi (field nullptr, FIELD), int32' or 
    // 'ptoi (ptop (field nullptr, FIELD), int8*), int32'.
	if(Match<FieldInstr>(MatchNullptr(&ptrOp), MatchIC(&indexOp))(op) ||
	   MatchConversion<PtopInstr>(Match<FieldInstr>(
	   MatchNullptr(&ptrOp), MatchIC(&indexOp)))(op)) {
		// Obtain the offset of the field from the record type.
		auto type = ptrOp->GetType()->As<PointerType>()->PointeeType();
		auto recordType = type->As<RecordType>();
		__int64 index = indexOp->As<IntConstant>()->Value();
		__int64 offset = recordType->Fields()[(int)index].FieldOffset;
		return irGen_->GetIntConst(intType, offset);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleAddress(Opcode opcode, Operand* baseOp, 
                                       Operand* indexOp, Block* block) {
	DebugValidator::IsTrue(opcode != Opcode::Index);
	DebugValidator::IsTrue(opcode != Opcode::Field);

	// An 'addr' with a 'nullptr' base and an index equal to 0 can be eliminated.
	// addr nullptr, 0 -> nullptr
    OperandInfo opInfo(irGen_->GetUnit(), target_);

    if(opInfo.IsPointerNull(baseOp, block) && 
       opInfo.IsZero(indexOp, block)) {
		return baseOp;
	}

	// Test for undefined values, both as a base and as an index.
	if(baseOp->IsUndefinedConstant()) {
		return baseOp;
	}
	else if(indexOp->IsUndefinedConstant()) {
		return indexOp;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleQuestion(QuestionInstr* instr) {
    // undef, a, b -> 'a' or 'b', try to choose a constant if possible.
    if(instr->ConditionOp()->IsUndefinedConstant()) {
        if(instr->TrueOp()->IsConstant()) {
            return instr->TrueOp();
        }
        else return instr->FalseOp();
    }
    // c, undef, b -> b
    else if(instr->TrueOp()->IsUndefinedConstant()) {
        return instr->FalseOp();
    }
    // c, a, undef -> a
    else if(instr->FalseOp()->IsUndefinedConstant()) {
        return instr->TrueOp();
    }

    // 1, a, b -> a
    // 0, a, b -> b
    if(instr->ConditionOp()->IsZeroInt()) {
        return instr->FalseOp();
    }
    else if(instr->ConditionOp()->IsIntConstant()) { // != 0
        return instr->TrueOp();
    }

    // (quest c, 3, 5), a, b -> a
    // If both operands in the condition are different from zero
    // we known that the 'true' operand will always be selected.
    if(auto questInstr = instr->ConditionOp()->DefiningInstrAs<QuestionInstr>()) {
        if(questInstr->HasConstantOperands() &&
           (questInstr->TrueOp()->IsZeroInt() ||
            questInstr->FalseOp()->IsZeroInt()) == false) {
            if(instr->TrueOp()->IsConstant()) {
                return instr->TrueOp();
            }
        }
    }

    // cond, 4, 4 -> 4
    if(instr->HasSameOperands() && instr->HasConstantOperands()) {
        return instr->TrueOp();
    }

    // Try to fold using Operand Info.
    OperandInfo opInfo(irGen_->GetUnit(), target_);

    if(opInfo.IsNotZero(instr->ConditionOp(), instr->ParentBlock())) {
        return instr->FalseOp();
    }
    else if(opInfo.IsZero(instr->ConditionOp(), instr->ParentBlock())) {
        return instr->TrueOp();
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleOperatorOnQuestion(Opcode opcode, IntConstant* intConst,
                                                  QuestionInstr* instr) {
    // This handles the case when we obtain the same constant for
    // the 'true' and 'false' branch of a 'quest' instruction. For example,
    // div (quest c, 4, 5), 2 -> quest c, 2, 2 -> 2
    auto trueConst = AsIntConstant(instr->TrueOp(), instr->ParentBlock());

    if(trueConst == nullptr) {
        return nullptr;
    }

    auto falseConst = AsIntConstant(instr->FalseOp(), instr->ParentBlock());

    if(falseConst == nullptr) {
        return nullptr;
    }

    __int64 trueResult;
    __int64 falseResult;

    switch(opcode) {
        case Opcode::Div: {
            trueResult = IA::Div(trueConst, intConst);
            falseResult = IA::Div(falseConst, intConst);
            break;
        }
        case Opcode::Udiv: {
            trueResult = IA::Udiv(trueConst, intConst);
            falseResult = IA::Udiv(falseConst, intConst);
            break;
        }
        case Opcode::Mod: {
            trueResult = IA::Mod(trueConst, intConst);
            falseResult = IA::Mod(falseConst, intConst);
            break;
        }
        case Opcode::Umod: {
            trueResult = IA::Umod(trueConst, intConst);
            falseResult = IA::Umod(falseConst, intConst);
            break;
        }
        case Opcode::And: {
            trueResult = IA::And(trueConst, intConst);
            falseResult = IA::And(falseConst, intConst);
            break;
        }
        case Opcode::Or: {
            trueResult = IA::Or(trueConst, intConst);
            falseResult = IA::Or(falseConst, intConst);
            break;
        }
        case Opcode::Xor: {
            trueResult = IA::Xor(trueConst, intConst);
            falseResult = IA::Xor(falseConst, intConst);
            break;
        }
        case Opcode::Shl: {
            trueResult = IA::Shl(trueConst, intConst);
            falseResult = IA::Shl(falseConst, intConst);
            break;
        }
        case Opcode::Shr: {
            trueResult = IA::Shr(trueConst, intConst);
            falseResult = IA::Shr(falseConst, intConst);
            break;
        }
        case Opcode::Ushr: {
            trueResult = IA::Ushr(trueConst, intConst);
            falseResult = IA::Ushr(falseConst, intConst);
            break;
        }
        default: {
            // Any other operation is not supported.
            return nullptr;
        }
    }

    // Check if we definitely have the same constant.
    if(trueResult == falseResult) {
        return irGen_->GetIntConst(intConst->GetType(), trueResult);
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleAbsOnQuestion(QuestionInstr* instr, bool is64Bit) {
    // abs(quest c, 5, -5) -> 5
    auto trueConst = AsIntConstant(instr->TrueOp(), instr->ParentBlock());

    if((trueConst == nullptr) || 
	   ((int)trueConst->Value() != trueConst->Value())) {
        return nullptr;
    }

    auto falseConst = AsIntConstant(instr->FalseOp(), instr->ParentBlock());

	if((falseConst == nullptr) || 
	   ((int)falseConst->Value() != falseConst->Value())) {
        return nullptr;
    }

    __int64 trueResult;
    __int64 falseResult;

    if(is64Bit) {
        trueResult = std::labs((long)trueConst->Value());
        falseResult = std::labs((long)falseConst->Value());
    }
    else {
        trueResult = std::abs((int)trueConst->Value());
        falseResult = std::abs((int)falseConst->Value());
    }

    if(trueResult == falseResult) {
        return irGen_->GetIntConst(trueConst->GetType(), trueResult);
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandlePhi(PhiInstr* instr) {
    // phi(C, C, ..., C) -> C
    if(instr->IsConstant()) {
        return instr->GetConstant();
    }

    // phi(undef, ..., undef) -> undef
    if(instr->SameOperands() && 
       instr->GetOperand(0)->IsUndefinedConstant()) {
        return instr->GetOperand(0);
    }

    // Check for a combination of constants and 'undef', because
    // we're allowed to choose the constant as the result.
    Operand* lastOp = nullptr;

    for(int i = 0; i < instr->OperandCount(); i++) {
        auto op = instr->GetOperand(i);

        if(op->IsConstant()) {
            if(lastOp && (op != lastOp)) {
                return nullptr;
            }

            lastOp = op;
        }
        else if(op->IsUndefinedConstant() == false) {
            // Not a constant or an 'undef', give up.
            return nullptr;
        }
    }

    return lastOp ? lastOp : GetUndefined(instr->ResultOp());
}

} // namespace Analysis