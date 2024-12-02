// ControlInstruction.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
#include "ControlInstructions.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

void ControlInstr::AddPredecessorTo(BlockReference* target) {
	if(target == nullptr) return;

	// If this instruction has a parent add it to the list 
	// of predecessor blocks in the block 'target'.
	if(parent_ && target) {
		target->Target()->AddPredecessor(parent_);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
void ControlInstr::RemovePredecessorFrom(BlockReference* target, bool removeAlways) {
	// The parent block is removed from the predecessor list of 'target'
    // only if 'target' appears a single time as a successor
    // (it can appear multiple times in case of 'switch', for example).
	if(parent_ && target) {
        if(removeAlways == false) {
            for(int i = 0, ct = 0; i < parent_->SuccessorCount(); i++) {
                if(parent_->SuccessorAt(i) == target->Target()) {
                    ct++;
                    if(ct == 2) return;
                }
            }
        }

		target->Target()->RemovePredecessor(parent_);
	}
}

// ######################################################################################
// IfInstr
// ######################################################################################
IfInstr::IfInstr(Operand* condition, BlockReference* trueTarget,
				 BlockReference* falseTarget, Block* parent, Instruction* previous) :
		ControlInstr(Opcode::If, 0, parent, previous), condition_(condition) {
	targets_[0] = trueTarget;
	targets_[1] = falseTarget;
	LinkUser(condition);
	LinkUser(targets_[0]);
	LinkUser(targets_[1]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
IfInstr* IfInstr::GetIf(Operand* condition, BlockReference* trueTarget,
						BlockReference* falseTarget, Block* parent,
						Instruction* previous) {
	IfInstr* instr = new IfInstr(condition, trueTarget, falseTarget, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IfInstr::Clone() {
	auto copy = GetIf(condition_, targets_[0], targets_[1]);
	copy->other_ = other_;
	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
string IfInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendLine("if");

	if(ConditionOp())   sb.Append('\t', level + 1)
					    .Append("Condition: ").AppendLine(ConditionOp()->ToString(level + 1));
	if(TrueTargetOp())  sb.Append('\t', level + 1)
					    .Append("True: ").AppendLine(TrueTargetOp()->ToString(level + 1));
	if(FalseTargetOp()) sb.Append('\t', level + 1)
					    .Append("False: ").AppendLine(FalseTargetOp()->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// GotoInstr
// ######################################################################################
GotoInstr::GotoInstr(BlockReference* target, Block* parent, Instruction* previous) :
		ControlInstr(Opcode::Goto, (int)GotoOrigin::Unknown, parent, previous), target_(target) {
	LinkUser(target_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
GotoInstr* GotoInstr::GetGoto(BlockReference* target, Block* parent, 
                              Instruction* previous) {
	GotoInstr* instr = new GotoInstr(target, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* GotoInstr::Clone() {
	auto copy = GetGoto(target_);
	copy->other_ = other_;
	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
string GotoInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendLine("goto");

	if(TargetOp()) sb.Append('\t', level + 1)
				   .Append("Target: ").AppendLine(TargetOp()->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// CallInstr
// ######################################################################################
int CallInstr::nextId_ = 0;

CallInstr::CallInstr(Operand* target, Operand* result, int argCapacity,
					 Block* parent, Instruction* previous) :
		ControlInstr(Opcode::Call, 0, parent, previous), target_(target), 
        result_(result ? result->As<Temporary>() : nullptr),
        args_(argCapacity > 0 ? new ArgumentList(argCapacity) : nullptr),
        id_(nextId_) {
	// Link the result operand to this instruction.
    nextId_++;
	LinkUser(target);
	if(result) result->SetDefiningInstr(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
CallInstr* CallInstr::GetCall(Operand* target, Operand* result, int argCapacity,
							  Block* parent, Instruction* previous) {
	CallInstr* instr = new CallInstr(target, result, argCapacity, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* CallInstr::Clone() {
	int reserved = args_ ? args_->Count() : 0;
	auto copy = GetCall(target_, nullptr, reserved);
	copy->other_ = other_;

    // Copy the arguments.
    if(args_) {
        for(int i = 0; i < args_->Count(); i++) {
            copy->AddArgument((*args_)[i]);
        }
    }

	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
CallInstr::~CallInstr() {
	FreeOperand(target_);
	target_ = nullptr;
	FreeOperand(result_);

	// Free all arguments.
	if(args_) {
		for(int i = 0; i < args_->Count(); i++) {
			FreeOperand((*args_)[i]);
			(*args_)[i] = nullptr;
		}

        delete args_;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
string CallInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendLine("call:");
	sb.Append('\t', level + 1).AppendFormat(L"Arguments: %d\n", args_ ? args_->Count() : 0);

    if(args_) {
	    for(int i = 0; i < args_->Count(); i++) {
		    sb.AppendLine((*Arguments())[i]->ToString(level + 1));
	    }
    }

	if(TargetOp()) sb.Append('\t', level + 1)
				   .Append("Target: ").AppendLine(TargetOp()->ToString(level + 1));
	if(ResultOp()) sb.Append('\t', level + 1)
				   .Append("Result: ").AppendLine(ResultOp()->ToString(level + 1));
	
	return sb.ToString();
}

// ######################################################################################
// ReturnInstr
// ######################################################################################
ReturnInstr::ReturnInstr(Operand* returnOp, Block* parent, Instruction* previous) :
		ControlInstr(Opcode::Return, 0, parent, previous), return_(returnOp) {
	LinkUser(returnOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
ReturnInstr* ReturnInstr::GetReturn(Operand* returnOp, Block* parent, 
                                    Instruction* previous) {
	ReturnInstr* instr = new ReturnInstr(returnOp, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* ReturnInstr::Clone() {
	auto copy = GetReturn(return_);
	copy->other_ = other_;
	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
string ReturnInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendLine("ret");

	if(return_) sb.Append('\t', level + 1)
				  .Append("Result: ").AppendLine(return_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// SwitchInstr
// ######################################################################################
SwitchInstr::SwitchInstr(Operand* condition, int caseListCapacity,
						 BlockReference* defaultTarget, Block* parent, 
						 Instruction* previous) :
		ControlInstr(Opcode::Switch, 0, parent, previous), condition_(condition),
		caseList_(caseListCapacity), defaultTarget_(defaultTarget) {
	LinkUser(condition);
	LinkUser(defaultTarget_);
	AddPredecessorTo(defaultTarget_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
SwitchInstr* SwitchInstr::GetSwitch(Operand* condition, int caseListCapacity,
									BlockReference* defaultTarget, Block* parent,
									Instruction* previous) {
	SwitchInstr* instr = new SwitchInstr(condition, caseListCapacity, defaultTarget,
										 parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* SwitchInstr::Clone() {
	auto copy = GetSwitch(condition_, caseList_.Count(), defaultTarget_);
	copy->other_ = other_;

    // Copy the cases.
    for(int i = 0; i < caseList_.Count(); i++) {
        copy->AddCase(caseList_[i].Value, caseList_[i].Target);
    }

	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* SwitchInstr::GetTargetForValue(__int64 value, bool defaultIfNotFound) {
    for(int i = 0; i < caseList_.Count(); i++) {
        // Check if the value of this case is the one we're looking for.
        if(caseList_[i].Value == value) {
            DebugValidator::IsNotNull(caseList_[i].Target);
            return caseList_[i].Target->Target();
        }
    }

    if(defaultIfNotFound) {
        // The user want the default target if the value was not found.
        DebugValidator::IsNotNull(defaultTarget_);
        return defaultTarget_->Target();
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SwitchInstr::GetValueForTarget(Block* target, __int64& value) {
    DebugValidator::IsNotNull(target);

    for(int i = 0; i < caseList_.Count(); i++) {
        DebugValidator::IsNotNull(caseList_[i].Target);

        if(caseList_[i].Target->Target() == target) {
            value = caseList_[i].Value;
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
string SwitchInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendLine("switch");

	if(defaultTarget_) {
		sb.Append('\t', level + 1).AppendLine(defaultTarget_->ToString(0));		
	}

	for(int i = 0; i < caseList_.Count(); i++) {
		sb.Append('\t', level + 1).AppendFormat(L"%lld : ", caseList_[i].Value);
		sb.Append(caseList_[i].Target->ToString(0));
	}

	return sb.ToString();
}

} // namespace IR
