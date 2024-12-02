// OtherInstructions.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "OtherInstructions.hpp"
#include "Block.hpp"
#include "Constants.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

CmpInstrBase::CmpInstrBase(Opcode opcode, OrderType order, Operand* left,
						   Operand* right, Operand* result, Block* parent, 
						   Instruction* previous) :
        Instruction(opcode, (int)order, parent, previous), 
		result_(result ? result->As<Temporary>() : nullptr) {
	// Link the result operand to this instruction.
	sources_[0] = left;
	sources_[1] = right;
	LinkUser(left);
	LinkUser(right);

	if(result) {
        result_->SetDefiningInstr(this);
        result_->SetIsBoolean(true);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
OrderType CmpInstrBase::InvertedOrder(OrderType order, bool invertEquality) {
	// Before   After
	// ==       ==
	// !=       !=
	// <        >
	// <=       >=
	// >        <
	// >=       <=
	switch(order) {
        case OrderType::Equal:          return invertEquality ? OrderType::NotEqual : OrderType::Equal;      
        case OrderType::NotEqual:       return invertEquality ? OrderType::Equal : OrderType::NotEqual;         
		case OrderType::Less:           return OrderType::Greater;
		case OrderType::LessOrEqual:    return OrderType::GreaterOrEqual;       
		case OrderType::Greater:        return OrderType::Less;   
		case OrderType::GreaterOrEqual: return OrderType::LessOrEqual;          
	}

	DebugValidator::Unreachable();
	return OrderType::Equal;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CmpInstrBase::InvertOrder(bool invertOperands, bool invertEquality) {
	SetOrder(InvertedOrder(Order(), invertEquality));

	// Also change the order of the operands, if requested.
	if(invertOperands) {
		Operand* temp = sources_[0];
		sources_[0] = sources_[1];
		sources_[1] = temp;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
OrderType CmpInstrBase::NegatedOrder(OrderType order) {
	// Before   After
	// ==       !=
	// !=       ==
	// <        >=
	// <=       >
	// >        <=
	// >=       <
	switch(order) {
        case OrderType::Equal:          return OrderType::NotEqual;      
        case OrderType::NotEqual:       return OrderType::Equal;         
		case OrderType::Less:           return OrderType::GreaterOrEqual;
		case OrderType::LessOrEqual:    return OrderType::Greater;       
		case OrderType::Greater:        return OrderType::LessOrEqual;   
		case OrderType::GreaterOrEqual: return OrderType::Less;          
	}

	DebugValidator::Unreachable();
	return OrderType::Equal;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void CmpInstrBase::NegateOrder() {
	SetOrder(NegatedOrder(Order()));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string CmpInstrBase::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append(OpcodeString() + ": ");

	switch(other_) {
		case OrderType::Less:           { sb.AppendLine("<");  break; }
		case OrderType::LessOrEqual:    { sb.AppendLine("<="); break; }
		case OrderType::Greater:        { sb.AppendLine(">");  break; }
		case OrderType::GreaterOrEqual: { sb.AppendLine(">="); break; }
		case OrderType::Equal:          { sb.AppendLine("=="); break; }
		case OrderType::NotEqual:       { sb.AppendLine("!="); break; }
	}

	if(LeftOp())   sb.Append('\t', level + 1)
				   .Append("Left: ").AppendLine(LeftOp()->ToString(level + 1));
	if(RightOp())  sb.Append('\t', level + 1)
				   .Append("Right: ").AppendLine(RightOp()->ToString(level + 1));
	if(ResultOp()) sb.Append('\t', level + 1)
				   .Append("Result: ").AppendLine(ResultOp()->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// CmpInstr
// ######################################################################################
CmpInstr::CmpInstr(OrderType order, Operand* left, Operand* right, 
				   Operand* result, Block* parent, Instruction* previous) :
		CmpInstrBase(Opcode::Cmp, order, left, right, result, parent, previous) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CmpInstr* CmpInstr::GetCmp(OrderType order, Operand* left, Operand* right, 
						   Operand* result, Block* parent, Instruction* previous) {
	CmpInstr* instr = new CmpInstr(order, left, right, result, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* CmpInstr::Clone() {
	auto copy = GetCmp(Order(), sources_[0], sources_[1], nullptr);
	copy->other_ = other_;
	return copy;
}

// ######################################################################################
// UcmpInstr
// ######################################################################################
UcmpInstr::UcmpInstr(OrderType order, Operand* left, Operand* right, 
				   Operand* result, Block* parent, Instruction* previous) :
		CmpInstrBase(Opcode::Ucmp, order, left, right, result, parent, previous) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UcmpInstr* UcmpInstr::GetUcmp(OrderType order, Operand* left, Operand* right, 
							  Operand* result, Block* parent, Instruction* previous) {
	UcmpInstr* instr = new UcmpInstr(order, left, right, result, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* UcmpInstr::Clone() {
    auto copy = GetUcmp(Order(), sources_[0], sources_[1], nullptr);
    copy->other_ = other_;
    return copy;
}

// ######################################################################################
// FcmpInstr
// ######################################################################################
FcmpInstr::FcmpInstr(OrderType order, Operand* left, Operand* right, 
				   Operand* result, Block* parent, Instruction* previous) :
		CmpInstrBase(Opcode::Fcmp, order, left, right, result,  parent, previous) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FcmpInstr* FcmpInstr::GetFcmp(OrderType order, Operand* left, Operand* right, 
                              Operand* result, Block* parent, Instruction* previous) {
	FcmpInstr* instr = new FcmpInstr(order, left, right, result, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* FcmpInstr::Clone() {
	auto copy = GetFcmp(Order(), sources_[0], sources_[1], nullptr);
	copy->other_ = other_;
	return copy;
}

// ######################################################################################
// LoadInstr
// ######################################################################################
LoadInstr::LoadInstr(Operand* source, Operand* result, Block* parent, Instruction* previous) :
		Instruction(Opcode::Load, 0, parent, previous), source_(source), 
		result_(result ? result->As<Temporary>() : nullptr) {
	// Link the operands to this instruction.
	LinkUser(source);
	if(result) result->SetDefiningInstr(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LoadInstr* LoadInstr::GetLoad(Operand* source, Operand* result, Block* parent,
							  Instruction* previous) {
	LoadInstr* instr = new LoadInstr(source, result, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* LoadInstr::Clone() {
	auto copy = GetLoad(source_, nullptr);
	copy->other_ = other_;
	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string LoadInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append("load: ");
	sb.AppendLine();

	if(SourceOp()) sb.Append('\t', level + 1)
				  .Append("Source: ").AppendLine(SourceOp()->ToString(level + 1));
	if(ResultOp()) sb.Append('\t', level + 1)
				  .Append("Result: ").AppendLine(ResultOp()->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// StoreInstr
// ######################################################################################
StoreInstr::StoreInstr(Operand* dest, Operand* source, Block* parent, Instruction* previous) :
		Instruction(Opcode::Store, 0, parent, previous) {
	sources_[0] = dest;
	sources_[1] = source;
	LinkUser(dest);
	LinkUser(source);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StoreInstr* StoreInstr::GetStore(Operand* dest, Operand* source, Block* parent,
								 Instruction* previous) {
	StoreInstr* instr = new StoreInstr(dest, source, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* StoreInstr::Clone() {
	auto copy = GetStore(sources_[0], sources_[1]);
	copy->other_ = other_;
	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string StoreInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append("store: ");
	sb.AppendLine();

	if(DestinationOp()) sb.Append('\t', level + 1)
				        .Append("Dest: ").AppendLine(DestinationOp()->ToString(level + 1));
	if(SourceOp()) sb.Append('\t', level + 1)
				   .Append("Source: ").AppendLine(SourceOp()->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// AddressInstr
// ######################################################################################
AddressInstr::AddressInstr(Opcode opcode, Operand* base, Operand* index, Operand* result, 
						   Block* parent, Instruction* previous) :
		Instruction(opcode, 0, parent, previous), 
		result_(result ? result->As<Temporary>() : nullptr) {
	// Link the operands to this instruction.
	sources_[0] = base;
	sources_[1] = index;
	LinkUser(base);
	LinkUser(index);
	if(result_) result_->SetDefiningInstr(this);
}

AddressInstr::AddressInstr(Operand* base, Operand* index, Operand* result, 
						   Block* parent, Instruction* previous) :
		Instruction(Opcode::Address, 0, parent, previous), 
                    result_(result ? result->As<Temporary>() : nullptr) {
	// Link the operands to this instruction.
	sources_[0] = base;
	sources_[1] = index;
	LinkUser(base);
	LinkUser(index);
	if(result_) result_->SetDefiningInstr(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AddressInstr* AddressInstr::GetAddress(Operand* base, Operand* index, Operand* result, 
									   Block* parent, Instruction* previous) {
	AddressInstr* instr = new AddressInstr(base, index, result, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* AddressInstr::Clone() {
	auto copy = GetAddress(sources_[0], sources_[1], nullptr);
	copy->other_ = other_;
	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string AddressInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendLine(OpcodeString() + ": ");

	if(BaseOp())   sb.Append('\t', level + 1)
				   .Append("Base: ").AppendLine(BaseOp()->ToString(level + 1));
	if(IndexOp())  sb.Append('\t', level + 1)
				   .Append("Index: ").AppendLine(IndexOp()->ToString(level + 1));
	if(ResultOp()) sb.Append('\t', level + 1)
				   .Append("Result: ").AppendLine(ResultOp()->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// IndexInstr
// ######################################################################################
IndexInstr::IndexInstr(Operand* base, Operand* index, Operand* result, 
					   Block* parent, Instruction* previous) :
		AddressInstr(Opcode::Index, base, index, result, parent, previous) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IndexInstr* IndexInstr::GetIndex(Operand* base, Operand* index, Operand* result,
								 Block* parent, Instruction* previous) {
	IndexInstr* instr = new IndexInstr(base, index, result, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IndexInstr::Clone() {
	auto copy = GetIndex(sources_[0], sources_[1], nullptr);
	copy->other_ = other_;
	return copy;
}

// ######################################################################################
// FieldInstr
// ######################################################################################
FieldInstr::FieldInstr(Operand* base, Operand* index, Operand* result, 
						   Block* parent, Instruction* previous) :
		AddressInstr(Opcode::Field, base, index, result, parent, previous) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FieldInstr* FieldInstr::GetField(Operand* base, Operand* index, Operand* result,
									   Block* parent, Instruction* previous) {
	FieldInstr* instr = new FieldInstr(base, index, result, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* FieldInstr::Clone() {
	auto copy = GetField(sources_[0], sources_[1], nullptr);
	copy->other_ = other_;
	return copy;
}

// ######################################################################################
// PhiInstr
// ######################################################################################
PhiInstr::PhiInstr(Operand* result, int opCapacity, Block* parent, Instruction* previous) :
		Instruction(Opcode::Phi, 0,  parent, previous), 
		result_(result ? result->As<Temporary>() : nullptr),
		ops_(opCapacity), blocks_(opCapacity) {
	if(result) result->SetDefiningInstr(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PhiInstr::~PhiInstr() {
    for(int i = 0; i < ops_.Count(); i++) {
        FreeOperand(ops_[i]);
        ops_[i] = nullptr;
    }

    for(int i = 0; i < blocks_.Count(); i++) {
        FreeOperand(blocks_[i]);
        blocks_[i] = nullptr;
    }

    FreeOperand(result_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PhiInstr* PhiInstr::GetPhi(Operand* result, int opCapacity, Block* parent,
						   Instruction* previous) {
	PhiInstr* instr = new PhiInstr(result, opCapacity, parent, previous);
	LinkWithPrevious(instr, parent, previous);
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* PhiInstr::Clone() {
	auto copy = GetPhi(nullptr, ops_.Count());

    // Copy the incoming operands.
    for(int i = 0; i < ops_.Count(); i++) {
        copy->AddOperand(ops_[i], blocks_[i]);
    }

	return copy;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* PhiInstr::GetOperandFromBlock(Block* block) {
	DebugValidator::IsNotNull(block);

	for(int i = 0; i < blocks_.Count(); i++) {
		if(blocks_[i]->Target() == block) {
			return ops_[i];
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Operand* PhiInstr::GetOperandFromBlock(Block* block) const {
	DebugValidator::IsNotNull(block);

	for(int i = 0; i < blocks_.Count(); i++) {
		if(blocks_[i]->Target() == block) {
			return ops_[i];
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PhiInstr::AddOperand(Operand* op, BlockReference* block) {
	DebugValidator::IsNotNull(op);
	DebugValidator::IsNotNull(block);

	ops_.Add(op);
    NotifyOperandAdded(op, ops_.Count() - 1);
    LinkUser(op);

	blocks_.Add(block);
    LinkUser(block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PhiInstr::RemoveOperand(int index) {
    NotifyOperandRemoved(ops_[index], ops_.Count() - 1);
    FreeOperand(ops_[index]);
	ops_.RemoveAt(index);

    FreeOperand(blocks_[index]);
	blocks_.RemoveAt(index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PhiInstr::RemoveOperand(Block* block) {
	DebugValidator::IsNotNull(block);

	for(int i = 0; i < blocks_.Count(); i++) {
		if(blocks_[i]->Target() == block) {
            RemoveOperand(i);
			return;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
void PhiInstr::ReplaceOperandBlock(int index, BlockReference* newBlock) {
    DebugValidator::IsNotNull(newBlock);
    FreeOperand(blocks_[index]);
    blocks_[index] = newBlock;
    LinkUser(newBlock);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool PhiInstr::IsConstant() const {
	auto constantOp = ops_[0]->As<Constant>();

	if(constantOp == nullptr) {
        return nullptr;
    }

	for(int i = 1; i < ops_.Count(); i++) {
        // Ignore operands for recursive 'phi' instructions.
        if(result_ && (ops_[i] == result_)) {
            continue;
        }
		else if(ops_[i] != constantOp) {
            return false;
        }
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Operand* PhiInstr::GetConstant() {
    DebugValidator::IsTrue(IsConstant());

    for(int i = 0; i < ops_.Count(); i++) {
        auto op = ops_[i];
        if(op->IsConstant()) return op;
    }

    return nullptr; // Shouldn't happen.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool PhiInstr::HasOnlyConstants() const {
	for(int i = 0; i < ops_.Count(); i++) {
        // Ignore operands for recursive 'phi' instructions.
        if(result_ && (ops_[i] == result_)) {
            continue;
        }
        else if(ops_[i]->IsConstant() == false) {
			return false;
		}
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool PhiInstr::HasOnlyParameters() const {
    for(int i = 0; i < ops_.Count(); i++) {
        // Ignore operands for recursive 'phi' instructions.
        if(result_ && (ops_[i] == result_)) {
            continue;
        }
        else if(ops_[i]->IsParameter() == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool PhiInstr::HasOnlyParametersOrConstants() const {
    for(int i = 0; i < ops_.Count(); i++) {
        // Ignore operands for recursive 'phi' instructions.
        if(result_ && (ops_[i] == result_)) {
            continue;
        }
        else if((ops_[i]->IsParameter() || ops_[i]->IsConstant()) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
bool PhiInstr::SameOperands() const {
	auto firstOp = ops_[0];

	for(int i = 1; i < ops_.Count(); i++) {
		if(ops_[i] != firstOp) return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string PhiInstr::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendLine(OpcodeString() + ": ");
	sb.Append("\tOperands: ");

	for(int i = 0; i < ops_.Count(); i++) {
		sb.Append("{");
		sb.Append(ops_[i]->ToString(0));
		sb.Append(", ");
		sb.Append(blocks_[i]->ToString(0));
		sb.Append("}, ");
	}

	if(ResultOp()) sb.Append('\t', level + 1)
				   .Append("Result: ").AppendLine(ResultOp()->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// QuestionInstr
// ######################################################################################
QuestionInstr::QuestionInstr(Operand* condition, Operand* left, Operand* right, 
                             Operand* result, Block* parent, Instruction* previous) :
        Instruction(Opcode::Question, 0, parent, previous), 
        result_(result ? result->As<Temporary>() : nullptr){
    sources_[0] = condition;
    sources_[1] = left;
    sources_[2] = right;
    LinkUser(condition);
    LinkUser(left);
    LinkUser(right);
    if(result_) result_->SetDefiningInstr(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
QuestionInstr* QuestionInstr::GetQuestion(Operand* condition, Operand* left, 
                                          Operand* right, Operand* result,
                                          Block* parent, Instruction* previous) {
    QuestionInstr* instr = new QuestionInstr(condition, left, right, result, 
                                             parent, previous);
    LinkWithPrevious(instr, parent, previous);
    return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* QuestionInstr::Clone() {
    return GetQuestion(ConditionOp(), TrueOp(), FalseOp(), nullptr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string QuestionInstr::ToStringImpl(int level) const {
    StringBuilder sb(string('\t', level));
    sb.Append(OpcodeString() + ": ");

    if(ConditionOp())   sb.Append('\t', level + 1)
        .Append("Condition: ").AppendLine(ConditionOp()->ToString(level + 1));
    if(TrueOp())  sb.Append('\t', level + 1)
        .Append("True: ").AppendLine(TrueOp()->ToString(level + 1));
    if(TrueOp())  sb.Append('\t', level + 1)
        .Append("False: ").AppendLine(TrueOp()->ToString(level + 1));
    if(ResultOp()) sb.Append('\t', level + 1)
        .Append("Result: ").AppendLine(ResultOp()->ToString(level + 1));

    return sb.ToString();
}

} // namespace IR