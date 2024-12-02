// OtherInstructions.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_OTHER_INSTRUCTIONS_HPP
#define PC_IR_OTHER_INSTRUCTIONS_HPP

#include "Instruction.hpp"
#include "IRTypes.hpp"
#include "Operand.hpp"
#include "References.hpp"
#include "Temporary.hpp"
#include "Constants.hpp"
#include "../Base/StaticList.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class Block;
class BlockReference;

// Represents the comparison order.
enum class OrderType {
	Less,
	LessOrEqual,
	Greater,
	GreaterOrEqual,
	Equal,
	NotEqual
};


// The base class for all comparison instructions.
class CmpInstrBase : public Instruction {
protected:
	// All operands need to have the same type.
	Operand* sources_[2]; // The source operands.
	Temporary* result_;   // The operand where the result is put.

	// Comparison order is stored in 'other_'.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual string ToStringImpl(int level) const override;

public:
	CmpInstrBase(Opcode opcode, OrderType order, Operand* left,
				 Operand* right, Operand* result, Block* parent, 
				 Instruction* previous);

	virtual ~CmpInstrBase() {
		FreeOperand(sources_[0]);
		sources_[0] = nullptr;
		FreeOperand(sources_[1]);
		sources_[1] = nullptr;
		FreeOperand(result_);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the comparison order (less than, equal, etc.).
	OrderType Order() const {
		return (OrderType)other_;
	}

	void SetOrder(OrderType value) {
		other_ = (unsigned short)value;
	}
	
	// Methods for testing the type of the order relation.
	bool IsLess()           const { return Order() == OrderType::Less;           }
	bool IsLessOrEqual()    const { return Order() == OrderType::LessOrEqual;    }
	bool IsGreater()        const { return Order() == OrderType::Greater;        }
	bool IsGreaterOrEqual() const { return Order() == OrderType::GreaterOrEqual; }
	bool IsEqual()          const { return Order() == OrderType::Equal;          }
	bool IsNotEqual()       const { return Order() == OrderType::NotEqual;       }

	bool IsEquality()       const { return IsEqual() || IsNotEqual(); }
	bool IsRelational()     const { return IsEquality() == false;     }

	// Returns the inverted order (for example, < becomes >).
	static OrderType InvertedOrder(OrderType order, bool invertEquality = true);

	// Inverts the order of the comparison (for example, < becomes >).
	// If 'invertOperands' is set then the operands are also swapped.
    // If 'invertEquality' is set then  == -> !=  and  != -> ==. 
	void InvertOrder(bool invertOperands = true, bool invertEquality = true);

    // Returns the negated order (for example, < becomes >=, == becomes !=).
	static OrderType NegatedOrder(OrderType order);

    // Negates the order (for example, < becomes >=, == becomes !=).
    void NegateOrder();

    // Returns 'true' if the order is identical 
    // to the order of the other comparison instruction.
    bool HasSameOrder(CmpInstrBase* other) const {
        DebugValidator::IsNotNull(other);
        return Order() == other->Order();
    }

    // Returns 'true' if the order is the same as the specified one.
    bool HasSameOrder(OrderType order) const {
        return Order() == order;
    }

	// Returns the left operand of the instruction.
	Operand* LeftOp() {
		return sources_[0];
	}

	const Operand* LeftOp() const {
		return sources_[0];
	}

	void SetLeftOp(Operand* value) {
        NotifyOperandRemoved(sources_[0], 0);
		FreeOperand(sources_[0]);

		LinkUser(value);
		sources_[0] = value;

        NotifyOperandAdded(value, 0);
	}

	// Returns the right operand of the instruction.
	Operand* RightOp() {
		return sources_[1];
	}

	const Operand* RightOp() const {
		return sources_[1];
	}

	void SetRightOp(Operand* value) {
        NotifyOperandRemoved(sources_[1], 0);
		FreeOperand(sources_[1]);

		LinkUser(value);
		sources_[1] = value;
        
        NotifyOperandAdded(value, 0);
	}

	// Returns the result operand of the instruction.
	Temporary* ResultOp() {
		return result_;
	}

	const Temporary* ResultOp() const {
		return result_;
	}

	void SetResultOp(Operand* value) {
		FreeOperand(result_);

        if(value) {
            result_ = value->As<Temporary>();
            result_->SetIsBoolean(true);
            value->SetDefiningInstr(this);
        }
        else result_ = nullptr;
	}

	// Returns 'true' if the result of 
	bool IsResultUsed() const {
		return result_ != nullptr;
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return 2; 
    }

	virtual Operand* GetSourceOp(int index) override {
		DebugValidator::IsSmaller(index, 2);
		return sources_[index];
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::IsSmaller(index, 2);
		return sources_[index];
	}

	virtual bool HasDestinationOp() const override { 
        return result_; 
    }

	virtual Temporary* GetDestinationOp() override { 
        return result_; 
    }

	virtual const Temporary* GetDestinationOp() const override { 
        return result_; 
    }

	virtual void ReplaceSourceOp(int index, Operand* newOp) override {
		DebugValidator::IsSmaller(index, 2);
        NotifyOperandRemoved(sources_[index], index);
        FreeOperand(sources_[index]);
		
        LinkUser(newOp);
		sources_[index] = newOp; 
        
        NotifyOperandAdded(newOp, index);
	}

	virtual void ReplaceDestinationOp(Temporary* newOp) override {
		SetResultOp(newOp);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct InstructionPromoter<CmpInstrBase> {
		static bool Is(const Instruction* instr) {
			return instr->IsComparison();
		}

		static CmpInstrBase* As(Instruction* instr) {
			return Is(instr) ? static_cast<CmpInstrBase*>(instr) : nullptr;
		}
	};
} // namespace Detail


// Comparison instruction for signed integer types.
class CmpInstr : public CmpInstrBase {
protected:
	CmpInstr(OrderType order, Operand* left, Operand* right, 
			 Operand* result, Block* parent, Instruction* previous);

public:
	static CmpInstr* GetCmp(OrderType order, Operand* left = nullptr, 
							Operand* right = nullptr, Operand* result = nullptr,
							Block* parent = nullptr, Instruction* previous = nullptr);
	virtual ~CmpInstr() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Comparison instruction for unsigned integer types.
class UcmpInstr : public CmpInstrBase {
protected:
	UcmpInstr(OrderType order, Operand* left, Operand* right, 
			  Operand* result, Block* parent, Instruction* previous);

public:
	static UcmpInstr* GetUcmp(OrderType order, Operand* left = nullptr, 
							  Operand* right = nullptr, Operand* result = nullptr,
							  Block* parent = nullptr, Instruction* previous = nullptr);
	virtual ~UcmpInstr() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Comparison instruction for unsigned integer types.
class FcmpInstr : public CmpInstrBase {
protected:
	FcmpInstr(OrderType order, Operand* left, Operand* right,
			  Operand* result, Block* parent, Instruction* previous);

public:
	static FcmpInstr* GetFcmp(OrderType order, Operand* left = nullptr, 
							  Operand* right = nullptr, Operand* result = nullptr,
							  Block* parent = nullptr, Instruction* previous = nullptr);

	virtual ~FcmpInstr() {}
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the instruction used to load data from memory.
class LoadInstr : public Instruction {
protected:
	Operand* source_;   // The operand that points to the source location.
	Temporary* result_; // The operand where the loaded value is placed.

	// 'other_' stores a flag that indicates whether the load is 'volatile'.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	LoadInstr(Operand* source, Operand* result, Block* parent, 
              Instruction* previous);

	virtual string ToStringImpl(int level) const override;

public:
	static LoadInstr* GetLoad(Operand* source, Operand* result = nullptr, 
							  Block* parent = nullptr, Instruction* previous = nullptr);
	virtual ~LoadInstr() {
		FreeOperand(source_);
        source_ = nullptr;
		FreeOperand(result_);
        result_ = nullptr;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual Instruction* Clone() override;
 
	// Returns the operand that indicates the source location.
	Operand* SourceOp() {
		return source_;
	}

	const Operand* SourceOp() const {
		return source_;
	}

	void SetSourceOp(Operand* value) {
        NotifyOperandRemoved(source_, 0);
		FreeOperand(source_);
	
        LinkUser(value);
		source_ = value;
        
        NotifyOperandAdded(value, 0);
	}

	// Returns the operand used to store the loaded value.
	Temporary* ResultOp() {
		return result_;
	}

	const Temporary* ResultOp() const {
		return result_;
	}

	void SetResultOp(Operand* value) {
		FreeOperand(result_);

        if(value) {
            result_ = value->As<Temporary>();
            value->SetDefiningInstr(this);
        }
        else result_ = nullptr;
	}

	// Returns the type of the loaded value pointed by the source operand.
	const Type* SourceType() const {
		return source_->GetType();
	}

	// Returns 'true' if the variable is marked 'volatile'.
	bool IsVolatile() const {
		return other_;
	}

	void SetIsVolatile(bool value) {
		other_ = value;
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return 1; 
    }

	virtual Operand* GetSourceOp(int index) override {
		DebugValidator::IsSmaller(index, 1);
		return source_;
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::IsSmaller(index, 1);
		return source_;
	}

	virtual bool HasDestinationOp() const override { 
        return result_; 
    }

	virtual Temporary* GetDestinationOp() override { 
        return result_; 
    }

	virtual const Temporary* GetDestinationOp() const override { 
        return result_; 
    }

	virtual void ReplaceSourceOp(int index, Operand* newOp) override {
		DebugValidator::IsSmaller(index, 1);
        NotifyOperandRemoved(source_, 0);
		FreeOperand(source_);
		
        LinkUser(newOp);
		source_ = newOp;
        
        NotifyOperandAdded(newOp, 0);
	}

	virtual void ReplaceDestinationOp(Temporary* newOp) override {
		SetResultOp(newOp);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the instruction used to load data from memory.
class StoreInstr : public Instruction {
protected:
	Operand* sources_[2]; // The destination and source operands.

	// 'other_' stores a flag that indicates whether the store is 'volatile'.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StoreInstr(Operand* dest, Operand* source, Block* parent, Instruction* previous);

	virtual string ToStringImpl(int level) const override;

public:
	static StoreInstr* GetStore(Operand* dest = nullptr, Operand* source = nullptr, 
							    Block* parent = nullptr, Instruction* previous = nullptr);
	virtual ~StoreInstr() {
		FreeOperand(sources_[0]);
		sources_[0] = nullptr;
		FreeOperand(sources_[1]);
		sources_[1] = nullptr;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	// Returns the operand that indicates the source location.
	Operand* DestinationOp() {
		return sources_[0];
	}

	const Operand* DestinationOp() const {
		return sources_[0];
	}

	void SetDestinationOp(Operand* value) {
        NotifyOperandRemoved(sources_[0], 0);
        FreeOperand(sources_[0]);
		
        LinkUser(value);
		sources_[0] = value;
        
        NotifyOperandAdded(value, 0);
	}

	// Returns the operand that contains the value to be stored.
	Operand* SourceOp() {
		return sources_[1];
	}

	const Operand* SourceOp() const {
		return sources_[1];
	}

    void SetSourceOp(Operand* value) {
        NotifyOperandRemoved(sources_[1], 1);
        FreeOperand(sources_[1]);
		
        LinkUser(value);
		sources_[1] = value;
        
        NotifyOperandAdded(value, 1);
	}

	// Returns the type of the stored value pointed by the destination operand.
	const Type* DestinationType() const {
		return sources_[0]->GetType();
	}

	// Returns 'true' if the variable is marked 'volatile'.
	bool IsVolatile() const {
		return other_;
	}

	void SetIsVolatile(bool value) {
		other_ = value;
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return 2; 
    }

	virtual Operand* GetSourceOp(int index) override {
		DebugValidator::IsSmaller(index, 2);
		return sources_[index];
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::IsSmaller(index, 2);
		return sources_[index];
	}

	virtual void ReplaceSourceOp(int index, Operand* newOp) override {
		DebugValidator::IsSmaller(index, 2);
        NotifyOperandRemoved(sources_[index], index);
		FreeOperand(sources_[index]);
		
        LinkUser(newOp);
		sources_[index] = newOp; 
        
        NotifyOperandAdded(newOp, index);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the instruction use to increment/decrement a pointer.
class AddressInstr : public Instruction {
protected:
	Operand* sources_[2]; // The base and index operands.
	Temporary* result_;   // The operand where the computed address will be stored.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// To be used by derived classes.
	AddressInstr(Opcode opcode, Operand* base, Operand* index, Operand* result,
				 Block* parent, Instruction* previous);

	AddressInstr(Operand* base, Operand* index, Operand* result,
				 Block* parent, Instruction* previous);

	virtual string ToStringImpl(int level) const override;

public:
	static AddressInstr* GetAddress(Operand* base = nullptr, Operand* index = nullptr, 
								    Operand* result = nullptr, Block* parent = nullptr,
									Instruction* previous = nullptr);
	virtual ~AddressInstr() {
		FreeOperand(sources_[0]);
		sources_[0] = nullptr;
		FreeOperand(sources_[1]);
		sources_[1] = nullptr;
		FreeOperand(result_);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	// Returns the operand that acts as the base address in the computation.
	Operand* BaseOp() {
		return sources_[0];
	}

	const Operand* BaseOp() const {
		return sources_[0];
	}

	void SetBaseOp(Operand* value) {
        NotifyOperandRemoved(sources_[0], 0);
		FreeOperand(sources_[0]);
		LinkUser(value);
		sources_[0] = value;
        NotifyOperandAdded(value, 0);
	}

	// Returns the operand used to store the loaded value.
	Temporary* ResultOp() {
		return result_;
	}

	const Temporary* ResultOp() const {
		return result_;
	}

	void SetResultOp(Operand* value) {
		FreeOperand(result_);

        if(value) {
            result_ = value->As<Temporary>();
            value->SetDefiningInstr(this);
        }
        else result_ = nullptr;
	}

	// Returns the operand that acts as the index (offset from base ) in the computation.
	Operand* IndexOp() {
		return sources_[1];
	}

	const Operand* IndexOp() const {
		return sources_[1];
	}

	void SetIndexOp(Operand* value) {
        NotifyOperandRemoved(sources_[1], 1);
		FreeOperand(sources_[1]);
		
        LinkUser(value);
		sources_[1] = value;
        
        NotifyOperandAdded(value, 1);
	}

    // Returns the type of the object pointed by the base operand.
    const Type* GetPointeeType() const {
        return sources_[0]->GetType()->As<PointerType>()->PointeeType();
    }

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return 2; 
    }

	virtual Operand* GetSourceOp(int index) override {
		DebugValidator::IsSmaller(index, 2);
		return sources_[index];
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::IsSmaller(index, 2);
		return sources_[index];
	}

	virtual bool HasDestinationOp() const override { 
        return result_; 
    }

	virtual Temporary* GetDestinationOp() override { 
        return result_; 
    }

	virtual const Temporary* GetDestinationOp() const override { 
        return result_; 
    }

	virtual void ReplaceSourceOp(int index, Operand* newOp) override {
		DebugValidator::IsSmaller(index, 2);
        NotifyOperandRemoved(sources_[index], index);
		FreeOperand(sources_[index]);
		
        LinkUser(newOp);
		sources_[index] = newOp;
        
        NotifyOperandAdded(newOp, index);
	}

	virtual void ReplaceDestinationOp(Temporary* newOp) override {
		SetResultOp(newOp);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the instruction used to access the elements of an array.
// Computes the address of the specified array element.
class IndexInstr : public AddressInstr {
protected:
	IndexInstr(Operand* base, Operand* index, Operand* result,
			   Block* parent, Instruction* previous);

public:
	static IndexInstr* GetIndex(Operand* base = nullptr, Operand* index = nullptr, 
								Operand* result = nullptr, Block* parent = nullptr,
								Instruction* previous = nullptr);
	virtual ~IndexInstr() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

    // Returns the type of the array used by the base operand.
    const ArrayType* GetArrayType() const {
        auto pointerType = sources_[0]->GetType()->As<PointerType>();
        return pointerType->PointeeType()->As<ArrayType>();
    }

    // Returns the type of the array element used by the base operand.
    const Type* GetElementType() const {
        return GetArrayType()->ElementType();
    }

    // Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the instruction used to access the elements of a record.
class FieldInstr : public AddressInstr {
protected:
	FieldInstr(Operand* base, Operand* index, Operand* result,
			   Block* parent, Instruction* previous);

public:
	static FieldInstr* GetField(Operand* base = nullptr, Operand* index = nullptr, 
								Operand* result = nullptr, Block* parent = nullptr,
								Instruction* previous = nullptr);
	virtual ~FieldInstr() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual Instruction* Clone() override;

    // Returns the type of the record used by the base operand.
    const RecordType* GetRecordType() const {
        auto pointerType = sources_[0]->GetType()->As<PointerType>();
        return pointerType->PointeeType()->As<RecordType>();
    }

	// Returns the referenced field.
	RecordField GetSelectedField() const {
		auto recordType = GetRecordType();
		DebugValidator::IsNotNull(recordType);
		return recordType->Fields()[(int)GetFieldIndex()];
	}

	// Returns the index of the referenced field.
	int GetFieldIndex() const {
		DebugValidator::IsTrue(IndexOp()->IsInteger());
		return (int)IndexOp()->As<IntConstant>()->Value();
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the classic PHI operator, used when a program is in SSA form.
class PhiInstr : public Instruction {
private:
	StaticList<Operand*, 4> ops_;
	StaticList<BlockReference*, 4> blocks_;
	Temporary* result_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	PhiInstr(Operand* result, int opCapacity, Block* parent, 
             Instruction* previous);

	virtual string ToStringImpl(int level) const override;

public:
	static PhiInstr* GetPhi(Operand* result = nullptr, int opCapacity = 0,
							Block* parent = nullptr, Instruction* previous = nullptr);
    virtual ~PhiInstr();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	virtual Instruction* Clone() override;

	// Returns the number of incoming operands.
	int OperandCount() const {
		return ops_.Count();
	}

	// Returns the incoming operand at the specified index.
	Operand* GetOperand(int index) {
		return ops_[index];
	}

	const Operand* GetOperand(int index) const {
		return ops_[index];
	}

	// Returns the block where the incoming operand 
	// at the specified index is found.
	Block* GetOperandBlock(int index) {
		DebugValidator::IsNotNull(blocks_[index]);
		return blocks_[index]->Target();
	}

	const Block* GetOperandBlock(int index) const {
		DebugValidator::IsNotNull(blocks_[index]);
		return blocks_[index]->Target();
	}

	// Returns the reference of the block where the incoming operand 
	// at the specified index is found.
	BlockReference* GetOperandBlockReference(int index) {
		return blocks_[index];
	}

	const BlockReference* GetOperandBlockReference(int index) const {
		return blocks_[index];
	}

	// Returns the incoming operand from the specified block,
	// or 'nullptr' if no such operand exists.
	Operand* GetOperandFromBlock(Block* block);
	const Operand* GetOperandFromBlock(Block* block) const;

    // Returns 'true' if one of the operands originates from the specified block.
    bool HasOperandFromBlock(Block* block) {
		DebugValidator::IsNotNull(block);
        return GetOperandFromBlock(block) != nullptr;
    }

	// Adds the specified operand to the PHI node so that it is incoming 
	// from the block associated with the specified block reference.
	void AddOperand(Operand* op, BlockReference* block);

	// Adds the specified operand to the PHI node so that it is incoming 
	// from the specified block.
	void AddOperand(Operand* op, Block* block) {
		DebugValidator::IsNotNull(op);
		DebugValidator::IsNotNull(block);
		AddOperand(op, block->GetReference());
	}

	// Removes the incoming operand found at the specified index.
	void RemoveOperand(int index);

	// Removes the incoming operand associated with the specified block.
	void RemoveOperand(Block* block);

    // Replaces the block associated with the incoming operand
    // found at the specified index with the specified one.
    void ReplaceOperandBlock(int index, BlockReference* newBlock);

	// Replaces the block associated with the incoming operand
	// found at the specified index with the specified one.
	void ReplaceOperandBlock(int index, Block* newBlock) {
		DebugValidator::IsNotNull(newBlock);
		ReplaceOperandBlock(index, newBlock->GetReference());
	}

	// Replaces the incoming operand found at the specified position
	// with the specified one.
    void ReplaceOperand(int index, Operand* newOp) {
        ReplaceSourceOp(index, newOp);
    }

	// Returns 'true' if all incoming operands are the same constant.
	bool IsConstant() const;

    // Returns the operand that is the representative constant.
    // For 't1 = phi(3, t1)' it returns 3. Returns 'nullptr' if not
	// all incoming operands are the same constant.
    Operand* GetConstant();

	// Returns 'true' if all incoming operands are constant,
	// but not necessarily the same constant.
	bool HasOnlyConstants() const;

    // Returns 'true' if all incoming operands are parameters.
    bool HasOnlyParameters() const;

    // Returns 'true' if all incoming operands are parameters or constants.
    bool HasOnlyParametersOrConstants() const;

	// Returns 'true' if all incoming operands are the same.
	bool SameOperands() const;

	// Returns 'true' if it has a single incoming operand.
	// In this case the PHI can be removed.
	bool HasSingleOperand() const {
		return ops_.Count() == 1;
	}

	// Returns the operand where the result will be stored.
	Temporary* ResultOp() {
		return result_;
	}

	const Temporary* ResultOp() const {
		return result_;
	}

	void SetResultOp(Operand* value) {
		FreeOperand(result_);

        if(value) {
            result_ = value->As<Temporary>();
            value->SetDefiningInstr(this);
        }
        else result_ = nullptr;
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return OperandCount(); 
    }

	virtual Operand* GetSourceOp(int index) override {
		return ops_[index];
	}

	virtual const Operand* GetSourceOp(int index) const override {
		return ops_[index];
	}

    virtual void ReplaceSourceOp(int index, Operand* newOp) override {
        FreeOperand(ops_[index]);
        NotifyOperandRemoved(ops_[index], index);
        ops_[index] = nullptr;
        LinkUser(newOp);
        ops_[index] = newOp;
        NotifyOperandAdded(newOp, index);
    }

	virtual bool HasDestinationOp() const override { 
        return result_; 
    }

	virtual Temporary* GetDestinationOp() override { 
        return result_; 
    }

	virtual const Temporary* GetDestinationOp() const override { 
        return result_; 
    }

	virtual void ReplaceDestinationOp(Temporary* newOp) override {
		SetResultOp(newOp);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// An instruction that can be used to select one of two values based on a condition.
// t = quest conditionOp, trueOp, falseOp
class QuestionInstr : public Instruction {
protected:
    Operand* sources_[3]; // The condition, true and false operands.
    Temporary* result_;   // The operand where the result is put.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    QuestionInstr(Operand* condition, Operand* left, Operand* right, 
                  Operand* result, Block* parent, Instruction* previous);

    virtual string ToStringImpl(int level) const override;

public:
    static QuestionInstr* GetQuestion(Operand* condition = nullptr, 
                                      Operand* left = nullptr, 
                                      Operand* right = nullptr, 
                                      Operand* result = nullptr, 
                                      Block* parent = nullptr, 
                                      Instruction* previous = nullptr);

    virtual ~QuestionInstr() {
        FreeOperand(sources_[0]);
        sources_[0] = nullptr;
        FreeOperand(sources_[1]);
        sources_[1] = nullptr;
        FreeOperand(sources_[2]);
        sources_[2] = nullptr;
        FreeOperand(result_);
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual Instruction* Clone() override;

    // Returns the operand that acts as the condition.
    Operand* ConditionOp() {
        return sources_[0];
    }

    const Operand* ConditionOp() const {
        return sources_[0];
    }

    void SetConditionOp(Operand* value) {
        NotifyOperandRemoved(sources_[0], 0);
        FreeOperand(sources_[0]);
        
        LinkUser(value);
        sources_[0] = value;
        
        NotifyOperandAdded(value, 0);
    }

    // Returns the left operand of the instruction.
    Operand* TrueOp() {
        return sources_[1];
    }

    const Operand* TrueOp() const {
        return sources_[1];
    }

    void SetTrueOp(Operand* value) {
        NotifyOperandRemoved(sources_[1], 1);
        FreeOperand(sources_[1]);
        
        LinkUser(value);
        sources_[1] = value;
        
        NotifyOperandAdded(value, 1);
    }

    // Returns the right operand of the instruction.
    Operand* FalseOp() {
        return sources_[2];
    }

    const Operand* FalseOp() const {
        return sources_[2];
    }

    void SetFalseOp(Operand* value) {
        NotifyOperandRemoved(sources_[1], 2);
        FreeOperand(sources_[2]);

        LinkUser(value);
        sources_[2] = value;

        NotifyOperandAdded(value, 2);
    }

    // Returns the result operand of the instruction.
    Temporary* ResultOp() {
        return result_;
    }

    const Temporary* ResultOp() const {
        return result_;
    }

    void SetResultOp(Operand* value) {
        FreeOperand(result_);

        if(value) {
            result_ = value->As<Temporary>();
            value->SetDefiningInstr(this);
        }
        else result_ = nullptr;
    }

    // Returns 'true' if the result of 
    bool IsResultUsed() const {
        return result_ != nullptr;
    }

    // Returns 'true' if the true and false operands are the same.
    bool HasSameOperands() const {
        return TrueOp() == FalseOp();
    }

    // Returns 'true' if both the true and false operands are constants.
    bool HasConstantOperands() const {
        return TrueOp()->IsConstant() &&
               FalseOp()->IsConstant();
    }

    // Methods for operand access.
    virtual int SourceOpCount() const override { 
        return 3; 
    }

    virtual Operand* GetSourceOp(int index) override {
        DebugValidator::IsSmaller(index, 3);
        return sources_[index];
    }

    virtual const Operand* GetSourceOp(int index) const override {
        DebugValidator::IsSmaller(index, 3);
        return sources_[index];
    }

    virtual bool HasDestinationOp() const override { 
        return result_; 
    }

    virtual Temporary* GetDestinationOp() override { 
        return result_; 
    }

    virtual const Temporary* GetDestinationOp() const override { 
        return result_; 
    }

    virtual void ReplaceSourceOp(int index, Operand* newOp) override {
        DebugValidator::IsSmaller(index, 3);
        NotifyOperandRemoved(sources_[index], index);
        FreeOperand(sources_[index]);
        
        LinkUser(newOp);
        sources_[index] = newOp; 
        
        NotifyOperandAdded(newOp, index);
    }

    virtual void ReplaceDestinationOp(Temporary* newOp) override {
        SetResultOp(newOp);
    }

    // Implements the visitor pattern.
    virtual void Accept(Visitor* v) override {
        v->Visit(this);
    }
};


// Creates the helper class needed for 'As<T>'/'Is<T>'.
#define MAKE_PROMOTION_HELPER(NAME) \
	namespace Detail { \
		template <> \
		struct InstructionPromoter<NAME##Instr> { \
			static bool Is(const Instruction* instr) { \
				return instr->GetOpcode() == Opcode::##NAME; \
			} \
			static NAME##Instr* As(Instruction* instr) { \
				return Is(instr) ? static_cast<NAME##Instr*>(instr) : nullptr; \
			} \
		}; \
	}

#define instruction(NAME, CAT, TEXT) MAKE_PROMOTION_HELPER(NAME)
#define ONLY_SELECTED
#define SELECT_OTHER
#include "Instructions.def"
#undef instruction
#undef ONLY_SELECTED
#undef SELECT_OTHER
#undef MAKE_PROMOTION_HELPER

} // namespace IR
#endif