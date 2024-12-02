// Block.cpp
// Copyright (c) Lup Gratian
//
// Implements the Block class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Block.hpp"
#include "Function.hpp"
#include "Unit.hpp"
#include "ControlInstructions.hpp"
#include "OtherInstructions.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

Block::PredecessorEnum::PredecessorEnum(const PredecessorList* list) : 
		list_(list), position_(0) {
	DebugValidator::IsNotNull(list);
}

Block::PredecessorEnum::PredecessorEnum(const PredecessorEnum& other) :
		list_(other.list_),  position_(other.position_) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Block* Block::PredecessorEnum::Next() const {
	if(position_ < list_->Count()) {
		return (*list_)[position_++];
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block::PredecessorEnum& Block::PredecessorEnum::operator= (const PredecessorEnum& other) {
	if(&other == this) return *this;

	list_ = other.list_;
	position_ = other.position_;
	return *this;
}

// ######################################################################################
// SuccessorHelper
// ######################################################################################
int SuccessorHelper::SuccessorCount(const Instruction* instr) {
	DebugValidator::IsNotNull(instr);
	switch(instr->GetOpcode()) {
		case Opcode::Goto: return 1;
		case Opcode::If:   return 2;
		case Opcode::Switch: {
			int caseCount = static_cast<const SwitchInstr*>(instr)->CaseCount();
			return caseCount + 1 /* default target */;
		}
		default: return 0;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* SuccessorHelper::SuccessorRefAt(Instruction* instr, int index) {
	DebugValidator::IsSmaller(index, SuccessorCount(instr));
	
	// For each case we need to check that the reference points to a block.
	switch(instr->GetOpcode()) {
		case Opcode::Goto: {
			auto gotoInstr = static_cast<GotoInstr*>(instr);
			return gotoInstr->TargetOp();
		}
		case Opcode::If: {
			auto ifInstr = static_cast<IfInstr*>(instr);

			// Two successors at most.
			if(index == 0) {
				return ifInstr->TrueTargetOp();
			}
			else return ifInstr->FalseTargetOp();
		}
		case Opcode::Switch: {
			auto switchInstr = static_cast<SwitchInstr*>(instr);

			// We consider first the list of 'case' blocks, then the default block.
			if(index < switchInstr->CaseCount()) {
				return switchInstr->GetCase(index).Target;
			}
			else return switchInstr->DefaultTargetOp();
		}
		default: return nullptr; // Any other instruction can't define successors.
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SuccessorHelper::SetSuccessorAt(Instruction* instr, BlockReference* blockRef, 
                                     int index) {
    DebugValidator::IsSmaller(index, SuccessorCount(instr));

    switch(instr->GetOpcode()) {
		case Opcode::Goto: {
			auto gotoInstr = static_cast<GotoInstr*>(instr);
            gotoInstr->SetTargetOp(blockRef);
            break;
		}
		case Opcode::If: {
			auto ifInstr = static_cast<IfInstr*>(instr);

			if(index == 0) {
				ifInstr->SetTrueTargetOp(blockRef);
			}
			else ifInstr->SetFalseTargetOp(blockRef);
			break;
		}
		case Opcode::Switch: {
			auto switchInstr = static_cast<SwitchInstr*>(instr);

			if(index < switchInstr->CaseCount()) {
                __int64 value = switchInstr->GetCase(index).Value;
                switchInstr->ReplaceCase(index, value, blockRef);
			}
			else switchInstr->SetDefaultTargetOp(blockRef);
			break;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* SuccessorHelper::SuccessorAt(Instruction* instr, int index) {
	BlockReference* succRef = SuccessorRefAt(instr, index);

	if(succRef) {
		return succRef->Target();
	}
	else return nullptr;
}

// ######################################################################################
// SuccessorEnum
// ######################################################################################
Block::SuccessorEnum::SuccessorEnum(const Instruction* lastInstr) :
		lastInstr_(lastInstr), position_(0) {
	if(lastInstr == nullptr) {
		// There is no instruction in the block, so there are no successors.
		successors_ = 0;
	}
	else successors_ = SuccessorHelper::SuccessorCount(lastInstr);
}

Block::SuccessorEnum::SuccessorEnum(const SuccessorEnum& other) :
		lastInstr_(other.lastInstr_), successors_(other.successors_),
		position_(other.position_) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Block* Block::SuccessorEnum::Next() const {
	// Some of the branches can be missing (the block reference is nullptr),
	// so we need to skip until we find the first valid block.
	while(position_ < successors_) {
		const Block* result = SuccessorHelper::SuccessorAt(lastInstr_, position_++);
		if(result) return result;
	}

	return nullptr; // No more successors.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block::SuccessorEnum& Block::SuccessorEnum::operator= (const SuccessorEnum& other) {
	if(&other == this) {
		return *this;
	}

	lastInstr_ = other.lastInstr_;
	successors_ = other.successors_;
	position_ = other.position_;
	return *this;
}

// ######################################################################################
// Block
// ######################################################################################
Block::Block(shared<string> name, Function* parent, Block* previous) :
		Symbol(Kind::Block, nullptr /* type */, name, parent ? &parent->Symbols() : nullptr), 
		IntrusiveHeader(previous), parent_(parent), IntrusiveList() {
	if(previous) {
		DebugValidator::IsTrue(!parent || (previous->ParentFunction() == parent));
		previous->ParentFunction()->InsertBlockAfter(this, previous);
	}
	else if(parent) {
		parent->InsertBlock(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block::~Block() {
	// Free all instructions found in the block.
	ClearInstructions(true /* free */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* Block::GetBlock(const string& name, Function* parent, Block* previous) {
	return new Block(new string(name), parent, previous);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::NotifyInstructionAdded(Instruction* instr) {
    if(auto callInstr = instr->As<CallInstr>()) {
        parent_->AddCallInstruction(callInstr);
    }

    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->InstructionAdded(instr, this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::NotifyInstructionRemoved(Instruction* instr) {
    if(auto callInstr = instr->As<CallInstr>()) {
        parent_->RemoveCallInstruction(callInstr);
    }

    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->InstructionRemoved(instr, this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::NotifyAddedToFunction(Function* function) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->AddedToFunction(function, this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::NotifyRemovedFromFunction(Function* function) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->RemovedFromFunction(function, this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::NotifyParameterAdded(Variable* parameter) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->ParameterAdded(parameter, ParentFunction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::NotifyParameterRemoved(Variable* parameter) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->ParameterRemoved(parameter, ParentFunction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::NotifyVariableAdded(Variable* variable) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->VariableAdded(variable, ParentFunction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::NotifyVariableRemoved(Variable* variable) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->VariableRemoved(variable, ParentFunction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::InsertInstructionFirst(Instruction* value) {
	DebugValidator::IsNotNull(value);
	value->SetParentBlock(this);
	value->HasNewParent();
	IntrusiveList::InsertFirst(value);
    NotifyInstructionAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::InsertInstructionLast(Instruction* value) {
	DebugValidator::IsNotNull(value);
	value->SetParentBlock(this);
	value->HasNewParent();
	IntrusiveList::InsertLast(value);
    NotifyInstructionAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::InsertInstructionBefore(Instruction* value, Instruction* other) {
	DebugValidator::IsNotNull(value);
	DebugValidator::IsNotNull(other);
	
	value->SetParentBlock(this);
	value->HasNewParent();
	IntrusiveList::InsertBefore(value, other);
    NotifyInstructionAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::InsertInstructionAfter(Instruction* value, Instruction* other) {
	DebugValidator::IsNotNull(value);
	DebugValidator::IsNotNull(other);
	
	value->SetParentBlock(this);
	value->HasNewParent();
	IntrusiveList::InsertAfter(value, other);
    NotifyInstructionAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::RemoveInstruction(Instruction* value, bool free, bool notify) {
	DebugValidator::IsNotNull(value);

    if(notify) {
	    value->RemovedFromParent();      // Notify instruction.
        NotifyInstructionRemoved(value); // Notify block tags.
    }

    // Remove from instruction list, then either destoy the instruction
	// or disconnect it from its parent block.
	IntrusiveList::Remove(value);
    
	if(free) {
        value->Free(true /* dontRemoveFromBlock */);
    }
	else value->SetParentBlock(nullptr, false /* prevents infinite recursion */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::ClearInstructions(bool free, bool fromDestructor) {
	Instruction* instr = FirstInstruction();

	while(instr) {
		// Unlink the instruction from the block, and free it, if requested.
		Instruction* nextInstr = instr->NextInstruction();
        RemoveInstruction(instr, free, fromDestructor == false /* notify */);
		instr = nextInstr;
	}

	IntrusiveList::Reset();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Block::InstructionCountWithoutPhis() const {
    int phiCount = 0;
    auto instr = FirstInstruction();

    while(instr) {
        if(instr->IsPhi()) {
            phiCount++;
            instr = instr->NextInstruction();
        }
        else break;
    }

    return Count() - phiCount;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Block::HasSuccessor(const Block* successor) const {
	// Don't use the enumerator, it's too much overhead.
	if(auto lastInstr = LastInstruction()) {
    	int succCount = SuccessorHelper::SuccessorCount(lastInstr);

	    for(int i = 0; i < succCount; i++) {
		    if(SuccessorHelper::SuccessorAt(lastInstr, i) == successor) {
			    return true; // Found the block.
		    }
	    }
    }

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Block::SuccessorsJumpingTo(const Block* block) const {
    int jumpingCount = 0;

    if(auto lastInstr = LastInstruction()) {
    	int succCount = SuccessorHelper::SuccessorCount(lastInstr);

        for(int i = 0; i < succCount; i++) {
		    if(SuccessorHelper::SuccessorAt(lastInstr, i) == block) {
                jumpingCount++;
            }
        }
    }

    return jumpingCount;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Block::SuccessorsJumpingTo(const BlockReference* blockRef) const {
    DebugValidator::IsNotNull(blockRef);
    return SuccessorsJumpingTo(blockRef->Target());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* Block::SuccessorAt(int index) {
	DebugValidator::IsSmaller(index, SuccessorCount());
	return SuccessorHelper::SuccessorAt(LastInstruction(), index);
}

const Block* Block::SuccessorAt(int index) const {
	DebugValidator::IsSmaller(index, SuccessorCount());
	return SuccessorHelper::SuccessorAt(LastInstruction(), index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::ReplaceSuccessor(int index, Block* newSuccessor) {
	DebugValidator::IsSmaller(index, SuccessorCount());
    DebugValidator::IsNotNull(newSuccessor);
	
    ReferenceTable* refTable = &ParentFunction()->ParentUnit()->References();
    ReplaceSuccessor(index, refTable->GetBlockRef(newSuccessor));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::ReplaceSuccessor(int index, BlockReference* newSuccessor) {
    DebugValidator::IsSmaller(index, SuccessorCount());
    DebugValidator::IsNotNull(newSuccessor);

    // The previous successor is unlinked from this block.
    auto previousRef = SuccessorHelper::SuccessorRefAt(LastInstruction(), index);

    if(SuccessorsJumpingTo(previousRef) == 1) {
        previousRef->Target()->RemovePredecessor(this);
    }

    SuccessorHelper::SetSuccessorAt(LastInstruction(), newSuccessor, index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::ReplaceSuccessor(Block* oldSuccessor, Block* newSuccessor) {
    DebugValidator::IsNotNull(oldSuccessor);
    DebugValidator::IsNotNull(newSuccessor);
    
    if(oldSuccessor == newSuccessor) return;

    ReferenceTable* refTable = &ParentFunction()->ParentUnit()->References();
    BlockReference* newSuccessorRef = refTable->GetBlockRef(newSuccessor);
    ReplaceSuccessor(oldSuccessor, newSuccessorRef);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::ReplaceSuccessor(Block* oldSuccessor, BlockReference* newSuccessor) {
    DebugValidator::IsNotNull(oldSuccessor);
    DebugValidator::IsNotNull(newSuccessor);

    for(int i = 0; i < SuccessorCount(); i++) {
        if(SuccessorHelper::SuccessorAt(LastInstruction(), i) == oldSuccessor) {
            auto previousRef = SuccessorHelper::SuccessorRefAt(LastInstruction(), i);
            previousRef->Target()->RemovePredecessor(this);
            SuccessorHelper::SetSuccessorAt(LastInstruction(), newSuccessor, i);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::LinkWith(Block* successor) {
    DebugValidator::IsNotNull(successor);

	// If there is a branching instruction remove it.
	if(LastInstruction() && LastInstruction()->IsBranching()) {
		DropLinks();
        LastInstruction()->RemoveFromBlock(true /* free */);
	}

	// Create a 'goto' to the block.
	ReferenceTable* refTable = &ParentFunction()->ParentUnit()->References();
	BlockReference* blockRef = refTable->GetBlockRef(successor);
	GotoInstr::GetGoto(blockRef, this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::LinkWith(BlockReference* successorRef) {
    DebugValidator::IsNotNull(successorRef);
    LinkWith(successorRef->Target());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::DropLinks() {
	// If there are no instructions nothing needs to be done.
	if(LastInstruction() == nullptr) {
        return;
    }

	// Unlink all successors from this block.
	int succCount = SuccessorHelper::SuccessorCount(LastInstruction());
	
	for(int i = 0; i < succCount; i++) {
		SuccessorAt(i)->RemovePredecessor(this);
	}

    // Remove all successors from the branching instruction.
    for(int i = 0; i < succCount; i++) {
        SuccessorHelper::SetSuccessorAt(LastInstruction(), nullptr, i);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::ReplacePredecessor(Block* oldPredecessor, Block* newPredecessor) {
    DebugValidator::IsNotNull(oldPredecessor);
    DebugValidator::IsNotNull(newPredecessor);

    for(int i = 0; i < preds_.Count(); i++) {
        if(preds_[i] == oldPredecessor) {
            preds_[i] = newPredecessor;
            return;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Block::HasInstruction(const Instruction* instr) const {
	const Instruction* temp = FirstInstruction();

	while(temp) {
		if(temp == instr) {
            return true;
        }

		temp = temp->NextInstruction();
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* Block::FirstNonPhiInstruction() {
    auto firstInstr = FirstInstruction();
    if(firstInstr == nullptr) return nullptr;

    while(firstInstr->IsPhi()) {
        firstInstr = firstInstr->NextInstruction();
    }

    return firstInstr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Block::HasPhi() const {
    // 'phi' can appear only at the beginning of the block.
    if(auto firstInstr = FirstInstruction()) {
        return firstInstr->IsPhi();
    }
    else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Block::PhiInstructionCount() const {
    // Count how many 'phi' instructions are in the block.
    int count = 0;
    auto instr = FirstInstruction();

    while(instr && instr->IsPhi()) {
        count++;
        instr = instr->NextInstruction();
    }

    return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* Block::SplitAt(Instruction* instr, const string& newBlockName) {
	DebugValidator::IsNotNull(instr);
	DebugValidator::IsTrue(HasInstruction(instr));
	
	// Create a new block in which all the instruction after 'instr' are placed.
	// Note that 'instr' is also placed in the new block.
	Block* newBlock = Block::GetBlock(newBlockName, ParentFunction());
	
	while(instr) {
        auto nextInstr = instr->NextInstruction();
		RemoveInstruction(instr);
		newBlock->InsertInstruction(instr);
		instr = nextInstr;
	}

	// Now link this block with the new one.
	ReferenceTable* refTable = &ParentFunction()->ParentUnit()->References();
	BlockReference* blockRef = refTable->GetBlockRef(newBlock);
	GotoInstr::GetGoto(blockRef, this);
	return newBlock;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Block::RemoveFromFunction(bool free) {
    DebugValidator::IsNotNull(parent_);
    NotifyRemovedFromFunction(parent_);
    parent_->RemoveBlock(this, free);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* Block::GetReference() {
    DebugValidator::IsNotNull(ParentFunction());
    auto unit = ParentFunction()->ParentUnit();
    return  unit->References().GetBlockRef(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unit* Block::ParentUnit() {
    DebugValidator::IsNotNull(parent_);
    return parent_->ParentUnit();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Unit* Block::ParentUnit() const {
    DebugValidator::IsNotNull(parent_);
    return parent_->ParentUnit();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Block::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append("Block ");
	if(HasName()) sb.Append(*Name());
	sb.AppendLine(":");
	
	for(auto p = FirstInstruction(); p; p = p->NextInstruction()) {
		sb.Append(p->ToString(level + 1));
	}

	return sb.ToString();
}

} // namespace IR 