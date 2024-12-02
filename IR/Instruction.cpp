// Instruction.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Instruction.hpp"
#include "Temporary.hpp"
#include "References.hpp"
#include "Block.hpp"
#include "Function.hpp"
#include "../Base/DebugValidator.hpp"
#include <iostream>
using namespace Base;

namespace IR {

Instruction::Instruction(Opcode opcode, int other, Block* parent,
						 Instruction* previous) :
		IntrusiveHeader(previous), opcode_((unsigned short)opcode), other_(other), 
		parent_(parent), overflowUndef_(0) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::LinkWithPrevious(Instruction* instr, Block* parent,
								   Instruction* previous) {
	DebugValidator::IsNotNull(instr);
	
	if(previous) {
		// If 'parent' is specified it must be the parent of the previous instruction.
		DebugValidator::IsTrue((parent == nullptr) || 
                               (previous->ParentBlock() == parent));
		previous->ParentBlock()->InsertInstructionAfter(instr, previous);		
	}
		
	if(parent && (previous == nullptr)) {
		// Insert the instruction at the end of the list.
		parent->InsertInstruction(instr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::Free(bool dontRemoveFromBlock) {
	if(parent_ && (dontRemoveFromBlock == false)) {
		RemovedFromParent();
		parent_->RemoveInstruction(this);
	}

	FreeImpl();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::FreeOperand(Operand* op) {
	if(op == nullptr) {
		return;
	}

	if(auto temp = op->As<Temporary>()) {
		// If the operand is a temporary defined by this instruction remove the link.
        // We need to be careful about 'phi' instructions, because they may
        // contain cycles ('t2 = phi(t1, t2)', for example). 
		// In this case we remove the 't2' from the incoming list first.
        int useCount = 0;

		if((temp->DefiningInstruction() == this) && (IsPhi() == false)) {
		    temp->SetDefiningInstr(nullptr);
		}
		else {
			// If the instruction doesn't use this temporary anymore,
			// remove it from the temporary's user list.
			for(int i = 0; (i < SourceOpCount()) && (useCount < 2); i++) {
				if(GetSourceOp(i) == temp) {
                    useCount++;
                }
			}

			if(useCount == 1) {
				// This user can be removed.
				temp->RemoveUser(this);
			}
		}

        // If this is a 'phi' instruction and the destination temporary
        // wasn't part of the list of incoming values we can remove it.
        if((temp->DefiningInstruction() == this) && 
           (useCount == 0) && IsPhi()) {
            temp->SetDefiningInstr(nullptr);
        }
	}
	
	op->Free(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::LinkUser(Operand* op) {
	if(op == nullptr) return;

	// Only temporaries are considered for def-use chains.
	if(auto temp = op->As<Temporary>()) {
		temp->AddUser(this);
	}
	else if(auto reference = op->As<Reference>()) {
		reference->AddUser(this);
	}
    else if(auto parameter = op->As<Parameter>()) {
        parameter->AddUser();
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::NotifyOperandAdded(Operand* op, int index) {
    if(op == nullptr) return;

    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->OperandAdded(op, index, this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::NotifyOperandRemoved(Operand* op, int index) {
    if(op == nullptr) return;

    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->OperandRemoved(op, index, this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::NotifyAddedToBlock() {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->AddedToBlock(ParentBlock(), this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::NotifyRemovedFromBlock() {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->RemovedFromBlock(ParentBlock(), this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* Instruction::ParentFunction() {
	if(parent_) {
		return parent_->ParentFunction();
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Function* Instruction::ParentFunction() const {
	if(parent_) {
		return parent_->ParentFunction();
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::SetParentBlock(Block* value, bool removeFromParent) {
	if(parent_ == value) return; // Nothing to do.
	if(parent_ && removeFromParent) parent_->RemoveInstruction(this);
	parent_ = value;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::RemoveFromBlock(bool free) {
    DebugValidator::IsNotNull(parent_);
    parent_->RemoveInstruction(this, free);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Instruction::IsFunctionEntry() const {
	DebugValidator::IsNotNull(parent_);
	DebugValidator::IsNotNull(parent_->ParentFunction());

	return (parent_->ParentFunction()->BlockCount() > 0) &&
		   (parent_->ParentFunction()->FirstBlock()->FirstInstruction() == this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::ReplaceSourceOp(Operand* oldOp, Operand* newOp) {
    DebugValidator::IsNotNull(oldOp);
    DebugValidator::IsNotNull(newOp);

    if(oldOp == newOp) {
        return;
    }
    
    for(int i = 0; i < SourceOpCount(); i++) {
        if(GetSourceOp(i) == oldOp) {
            ReplaceSourceOp(i, newOp);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Instruction::OpcodeString(Opcode opcode) {
	// The names are automatically generated from the instruction list.
	switch(opcode) {
		#define instruction(TYPE, CAT, NAME) \
			case Opcode::##TYPE: { return NAME; }
		#include "Instructions.def"
		#undef instruction
	}

	DebugValidator::Unreachable();
	return "";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Instruction::IsAssociative(Opcode opcode) {
	return (opcode == Opcode::Add)  || (opcode == Opcode::Mul)  ||
		   (opcode == Opcode::Fadd) || (opcode == Opcode::Fmul) ||
		   (opcode == Opcode::And)  || (opcode == Opcode::Or)   ||
		   (opcode == Opcode::Xor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Instruction::IsCommutative(Opcode opcode) {
	return (opcode == Opcode::Add)  || (opcode == Opcode::Mul)  ||
		   (opcode == Opcode::Fadd) || (opcode == Opcode::Fmul) ||
		   (opcode == Opcode::And)  || (opcode == Opcode::Or)   ||
		   (opcode == Opcode::Xor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Instruction::IsDistributive(Opcode opcode1, Opcode opcode2) {
	// Multiplication and division distributes over addition and subtraction.
	if((opcode1 == Opcode::Mul) || (opcode1 == Opcode::Fmul) ||
       (opcode1 == Opcode::Div) || (opcode2 == Opcode::Udiv) ||
       (opcode1 == Opcode::Fdiv)) {
		return (opcode2 == Opcode::Add)  ||
			   (opcode2 == Opcode::Sub)  ||
			   (opcode2 == Opcode::Fadd) ||
			   (opcode2 == Opcode::Fsub);
	}
	else if(opcode1 == Opcode::And) {
		// And distributes over Or and Xor.
		return (opcode2 == Opcode::Or)  || (opcode2 == Opcode::Xor);
	}
	else if(opcode1 == Opcode::Or) {
		// Or distributes over And.
		return opcode2 == Opcode::And;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Instruction::Dump() const {
	std::wcout<<ToString(0).Chars();
}

} // namespace IR