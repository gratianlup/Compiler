// Instruction.hpp
// Copyright (c) Lup Gratian
//
// Represents an instruction, defined by an opcode, a series of source
// operands (usually two) and a destination operand.
// The 'Instructions.def' file is used to generate a large part
// of the information automatically using the preprocessor.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_INSTRUCTION_HPP
#define PC_IR_INSTRUCTION_HPP

#include "IRType.hpp"
#include "Tagged.hpp"
#include "Visitor.hpp"
#include "Operand.hpp"
#include "IntrusiveList.hpp"
#include "../Base/String.hpp"
#include "../Base/StaticList.hpp"
#include <assert.h>
using namespace Base;

namespace IR {

// Forward declarations.
class Block;
class Function;
class Instruction;
class Temporary;

namespace Detail {
	// Used for implementing 'As<T>'/'Is<T>'.
	template <class T>
	struct InstructionPromoter {
		static bool Is(const Instruction* op) {
			//static_assert(false, "Type is not an Instruction in Is<T>");
			assert(0);
			return false;
		}

		static T* As(Instruction* op) {
			//static_assert(false, "Type is not an Instruction in As<T>");
			assert(0);
			return nullptr;
		}
	};
} // namespace Detail


// Represents the opcode for all supported instructions.
enum class Opcode {
	// Only the instructions in the selected category should be considered.
	#define ONLY_SELECTED
	#define instruction(TYPE, CAT, NAME) TYPE,

	// Arithmetic instructions.
	BEGIN_ARITHMETIC,
	#define SELECT_ARITHMETIC
	#include "Instructions.def"
	#undef SELECT_ARITHMETIC
	END_ARITHMETIC,

	// Conversion instructions.
	BEGIN_CONVERSION,
	#define SELECT_CONVERSION
	#include "Instructions.def"
	#undef SELECT_CONVERSION
	END_CONVERSION,

	// Logical instructions.
	BEGIN_LOGICAL,
	#define SELECT_LOGICAL
	#include "Instructions.def"
	#undef SELECT_LOGICAL
	END_LOGICAL,

	// Control instructions.
	BEGIN_CONTROL,
	#define SELECT_CONTROL
	#include "Instructions.def"
	#undef SELECT_CONTROL
	END_CONTROL,

	// Other instructions.
	#define SELECT_OTHER
	#include "Instructions.def"
	#undef SELECT_OTHER
	#undef instruction
	#undef ONLY_SELECTED
};


// Represents the general categories of instructions.
enum class InstructionCategory {
	Arithmetic, // add, sub, mul, div, udiv, mod, umod, 
                // fadd, fsub, fmul, fdiv, frem
	Conversion, // trunc, zext, sext, ftoi, ftoui, itof, uitof, ftrunc,
                // fext, ptoi, itop, ptop
	Logical,    // and, or, xor, shl, shr, ushr
	Control,    // cmp, ucmp, fcmp, if, goto, switch, call, ret
	Other       // load, store, addr, index, field, phi, quest
};


// Represents the base class for all instructions.
class Instruction : public IntrusiveHeader<Instruction>, public Visitable,
                    public Tagged<InstructionTag> {
private:
	Instruction();                               // Should not be created.
	Instruction(const Instruction&);             // Should not be copied.
	Instruction& operator= (const Instruction&); // Should not be assigned.

protected:
	typedef IntrusiveHeader<Instruction> THeader;

	Block* parent_;                     // The block to which the instruction belongs.
	unsigned short opcode_        : 6;  // The type of the instruction.
	unsigned short overflowUndef_ : 1;  // Signed overflow undefined.
	unsigned short other_         : 9;  // Can be used by derived classes to store info.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Instruction(Opcode opcode, int other = 0, Block* parent = nullptr,
				Instruction* previous = nullptr);

	// Frees the specified operand, and if this is the
    // defining instruction, it unlinks it.
	void FreeOperand(Operand* op);

	// Adds the instruction as a user to the specified operand.
	// Applies only if the operand is a temporary.
	void LinkUser(Operand* op);

	// Should generate a string that describes the operand object.
	virtual string ToStringImpl(int level) const {
		return "";
	}

	// Should deallocate the space used by the instruction.
	virtual void FreeImpl() {
		delete this; // Default implementation.
	}
	
    // Method that should notify the associated tags
    // that a new operand has been added.
    virtual void NotifyOperandAdded(Operand* op, int index);

    // Method that should notify the associated tags
    // that an operand has been removed.
    virtual void NotifyOperandRemoved(Operand* op, int index);

    // Method that should notify the associated tags
    // that an operand has been removed.
    virtual void NotifyAddedToBlock();

    // Method that should notify the associated tags
    // that an operand has been removed.
    virtual void NotifyRemovedFromBlock();

	static void LinkWithPrevious(Instruction* instr, Block* parent,
								 Instruction* previous);

public:
	virtual ~Instruction() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// The methods that shall be used to deallocate 
    // the memory used by an instruction.
	void Free(bool dontRemoveFromBlock = false);

	// This method shall be called before the instruction
    // is removed from its parent block.
	virtual void RemovedFromParent() {
        NotifyRemovedFromBlock();
    }

	// This method shall be called after the instruction
    // was added to a new block.
	virtual void HasNewParent() {
        NotifyAddedToBlock();
    }

	// This method shall create an exact duplicate of the instruction.
	// Note that the operands should only be copied (this is not a deep clone).
	virtual Instruction* Clone() {
		return nullptr;
	}

	// Returns the opcode (type) of the instruction.
	Opcode GetOpcode() const {
		return (Opcode)opcode_;
	}

	// Returns 'true' if this is an arithmetic instruction.
	static bool IsArithmetic(Opcode opcode) {
		return ((int)opcode > (int)Opcode::BEGIN_ARITHMETIC) && 
			   ((int)opcode < (int)Opcode::END_ARITHMETIC);
	}

	bool IsArithmetic() const {
		return IsArithmetic((Opcode)opcode_);
	}

	// Returns 'true' if this is a conversion instruction.
	static bool IsConversion(Opcode opcode) {
		return ((int)opcode > (int)Opcode::BEGIN_CONVERSION) && 
			   ((int)opcode < (int)Opcode::END_CONVERSION);
	}

	bool IsConversion() const {
		return IsConversion((Opcode)opcode_);
	}

	// Returns 'true' if this is a logical instruction.
	static bool IsLogical(Opcode opcode) {
		return ((int)opcode > (int)Opcode::BEGIN_LOGICAL) && 
			   ((int)opcode < (int)Opcode::END_LOGICAL);
	}

	bool IsLogical() const {
		return IsLogical((Opcode)opcode_);
	}

	// Returns 'true' if this is a shift instruction.
	static bool IsShift(Opcode opcode) {
		return (opcode == Opcode::Shl) || (opcode == Opcode::Shr) || 
               (opcode == Opcode::Ushr);
	}

	bool IsShift() const {
		return IsShift((Opcode)opcode_);
	}

	// Returns 'true' if this is an control instruction.
	static bool IsControl(Opcode opcode) {
		return ((int)opcode > (int)Opcode::BEGIN_CONTROL) && 
			   ((int)opcode < (int)Opcode::END_CONTROL);
	}

	bool IsControl() const {
		return IsControl((Opcode)opcode_);
	}

	// Returns 'true' if the instruction performs a branch to another block.
	// Note that 'call' is not considered a branch instruction.
	static bool IsBranching(Opcode opcode) {
		return ((int)opcode > (int)Opcode::BEGIN_CONTROL) && 
			   ((int)opcode < (int)Opcode::END_CONTROL)   &&
			   (opcode != Opcode::Call);
	}

	bool IsBranching() const {
		return IsBranching((Opcode)opcode_);
	}

	// Returns the category to which the instruction belongs
    // (arithmetic, logical, conversion, etc.).
	static InstructionCategory Category(Opcode opcode) {
		if(IsArithmetic(opcode))      return InstructionCategory::Arithmetic;
		else if(IsConversion(opcode)) return InstructionCategory::Conversion;
		else if(IsLogical(opcode))    return InstructionCategory::Logical;
		else if(IsControl(opcode))    return InstructionCategory::Control;
		else                          return InstructionCategory::Other;
	}

	InstructionCategory Category() const {
		return Category((Opcode)opcode_);
	}

	// Methods for testing the type of the instruction.
	// Generated automatically from the list of instructions.
	#define instruction(TYPE, CAT, NAME) \
		bool Is##TYPE() const { return (Opcode)opcode_ == Opcode::##TYPE; }
	#include "Instructions.def"
	#undef instruction

	// Returns 'true' if this is a comparison instruction (cmp/ucmp/fcmp).
    static bool IsComparison(Opcode opcode) {
        return (opcode == Opcode::Cmp) || (opcode == Opcode::Ucmp) ||
               (opcode == Opcode::Fcmp);
    }

	bool IsComparison() const {
		return IsComparison((Opcode)opcode_);
	}

    // Returns 'true' if this is an instruction that computes
    // a memory address (addr/index/field).
    static bool IsAddressing(Opcode opcode) {
        return (opcode == Opcode::Address) || (opcode == Opcode::Index) ||
               (opcode == Opcode::Field);
    }

    bool IsAddressing() const {
        return IsAddressing((Opcode)opcode_);
    }

	// Returns 'true' if this is an arithmetic instruction 
    // that operates on integer types.
    bool IsIntArithmetic(Opcode opcode) {
        return IsArithmetic(opcode) && (IsFloatArithmetic(opcode) == false);
    }

	bool IsIntArithmetic() const {
		return IsArithmetic() && (IsFloatArithmetic() == false);
	}

    // Returns 'true' if this is an arithmetic instruction 
    // that operates on floating types.
    static bool IsFloatArithmetic(Opcode opcode) {
		return (opcode == Opcode::Fadd) || (opcode == Opcode::Fsub) ||
			   (opcode == Opcode::Fmul) || (opcode == Opcode::Fdiv);
	}
    
    bool IsFloatArithmetic() const {
		return IsFloatArithmetic((Opcode)opcode_);
	}

	// Returns 'true' if this is an arithmetic instruction 
	// that operates on signed integer types.
    static bool IsSignedArithmetic(Opcode opcode) {
        return (opcode == Opcode::Add) || (opcode == Opcode::Sub) ||
               (opcode == Opcode::Mul) || (opcode == Opcode::Div) ||
               (opcode == Opcode::Mod);
	}

	bool IsSignedArithmetic() const {
		return IsSignedArithmetic((Opcode)opcode_);
	}

	// Returns 'true' if this is an arithmetic instruction
	// that operates on unsigned types.
    static bool IsUnsignedArithmetic(Opcode opcode) {
        return (opcode == Opcode::Add) || (opcode == Opcode::Sub) ||
               (opcode == Opcode::Div) || (opcode == Opcode::Umod);
    }
    
    bool IsUnsignedArithmetic() const {
		return IsUdiv() || IsUmod();
	}

	// Returns the number of operands the instruction uses.
	virtual int SourceOpCount() const {
		return 0;
	}

	// Returns the specified source operand.
	virtual Operand* GetSourceOp(int index) {
		DebugValidator::Unreachable(); // Should not be used!
		return nullptr;
	}

	virtual const Operand* GetSourceOp(int index) const {
		DebugValidator::Unreachable(); // Should not be used!
		return nullptr;
	}

    // Returns 'true' if the function uses the specified operand.
    virtual bool HasSourceOp(Operand* op) const {
        DebugValidator::IsNotNull(op);
        int count = SourceOpCount();
        
        for(int i = 0; i < count; i++) {
            if(GetSourceOp(i) == op) {
                return true;
            }
        }
        
        return false;
    }

	// Calls the specified operation for each of the source operands.
	// bool Predicate(Operand* op, int index)
	template <class Predicate>
	void ForEachSourceOp(Predicate action) {
		int count = SourceOpCount();
		for(int i = 0; i < count; i++) {
			if(action(GetSourceOp(i), i) == false) {
				return; // The user aborted.
			}
		}
	}

    template <class Predicate>
	void ForEachSourceOp(Predicate action) const {
		int count = SourceOpCount();
		for(int i = 0; i < count; i++) {
			if(action(GetSourceOp(i), i) == false) {
				return; // The user aborted.
			}
		}
	}

	// Returns 'true' if the instruction has an operand 
    // in which the resulting value is stored.
	virtual bool HasDestinationOp() const {
		return false;
	}

	// Returns the destination operand. By default it returns 'nullptr',
	// presuming no operand is defined.
	virtual Temporary* GetDestinationOp() {
		return nullptr;
	}

	virtual const Temporary* GetDestinationOp() const {
		DebugValidator::Unreachable(); // Should not be used!
		return nullptr;
	}

	// Replaces the operand found at the specified position with a new one.
	virtual void ReplaceSourceOp(int index, Operand* newOp) {
		DebugValidator::Unreachable(); // Should not be used!
	}

    // Replaces each instance of the specified operand with a new one.
	virtual void ReplaceSourceOp(Operand* oldOp, Operand* newOp);

	// Replaces the destination operand with a new one.
	virtual void ReplaceDestinationOp(Temporary* newOp) {
		DebugValidator::Unreachable(); // Should not be used!
	}

	// Returns 'true' if the operator is associative 
    // ('a OP (b OP c) == (a OP b) OP c').
	static bool IsAssociative(Opcode opcode);

	bool IsAssociative() const {
		return IsAssociative((Opcode)opcode_);
	}

	// Returns 'true' if the operator is commutative ('a OP b == b OP a').
	static bool IsCommutative(Opcode opcode);

	bool IsCommutative() const {
		return IsCommutative((Opcode)opcode_);
	}

	// Returns 'true' if the operator is distributive in relation
    // with the other one ('(a OP1 b) OP2 c == (a OP2 c OP1) (b OP2 c)').
	static bool IsDistributive(Opcode opcode1, Opcode opcode2);

	// Returns 'true' if the source language specified that
	// signed integer overflow is undefined for this instruction.
	bool HasUndefinedOverflow() const {
		return overflowUndef_;
	}

	void SetHasUndefinedOverflow(bool state) {
		overflowUndef_ = state;
	}

	// Methods for accessing the next/previous instruction.
	Instruction* NextInstruction() { 
        return static_cast<Instruction*>(THeader::Next()); 
    }

	const Instruction* NextInstruction() const { 
		return static_cast<const Instruction*>(THeader::Next());
	}

	Instruction* PreviousInstruction() { 
        return static_cast<Instruction*>(THeader::Previous()); 
    }

	const Instruction* PreviousInstruction() const { 
		return static_cast<const Instruction*>(THeader::Previous());
	}

	// Returns the block to which this instruction belongs.
	Block* ParentBlock() {
		return parent_;
	}

	const Block* ParentBlock() const {
		return parent_;
	}

	void SetParentBlock(Block* value, bool removeFromParent = true);

    // Removes the instruction from its parent block, optionally freeing it.
    void RemoveFromBlock(bool free = false);

	// Returns the function to which this instruction belongs.
	Function* ParentFunction();
	const Function* ParentFunction() const;
    
	// Returns 'true' if this is the first instruction in the block.
	bool IsFirst() const {
		return THeader::Previous() == nullptr;
	}

	// Returns 'true' if this is the first instruction in the block.
	bool IsLast() const {
		return THeader::Next() == nullptr;
	}

	// Returns 'true' if this is the first instruction executed in a function.
	bool IsFunctionEntry() const;

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return Detail::InstructionPromoter<T>::As(this);
	}

	template <class T>
	const T* As() const {
		return Detail::InstructionPromoter<T>::As(const_cast<Instruction*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return Detail::InstructionPromoter<T>::Is(this);
	}

	// Returns 'true' if the other instruction has the same opcode.
	bool IsSameKind(const Instruction* other) const {
		return opcode_ == other->opcode_;
	}

	// Returns a string representation of the instruction information.
	string ToString(int level) const {
		return ToStringImpl(level);
	}

	// Returns the string representation of the opcode.
	static string OpcodeString(Opcode opcode);

	string OpcodeString() const {
		return OpcodeString((Opcode)opcode_);
	}

	// Print the string representation to the console.
	void Dump() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	template <class T>
	struct DefiningInstrPromoter<T, Instruction> {
		static bool Is(const Instruction* instr) {
			return instr->Is<T>();
		}

		static T* As(Instruction* instr) {
			return instr->As<T>();
		}
	};
} // namespace Detail

} // namespace IR
#endif