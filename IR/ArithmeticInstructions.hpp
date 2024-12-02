// ArithmeticInstruction.hpp
// Copyright (c) Lup Gratian
//
// Defines the arithmetic instructions ('add', 'sub', 'mul', etc.).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_ARITHMETIC_INSTRUCTION_HPP
#define PC_IR_ARITHMETIC_INSTRUCTION_HPP

#include "Instruction.hpp"
#include "IRType.hpp"
#include "Operand.hpp"
#include "Temporary.hpp"

namespace IR {

// Represents the mode in which a floating-point operation is performed.
// Can be applied only on floating point instructions like 'fadd'.
enum class FloatMode {
	Exact, // No optimizations of expressions is allowed.
	Safe,  // Some optimizations that don't decrease the precision to much are allowed.
	Fast   // All optimizations are enabled, including reassociation of expressions.
};


// Represents the class of instructions that operate on arithmetic operands
// (add, sub, mul, div, udiv, mod, umod, fadd, fsub, fmul, fdiv, frem).
class ArithmeticInstr : public Instruction {
protected:
	// All operands need to have the same type.
	Operand* sources_[2]; // The source operands.
	Temporary* result_;   // The operand where the result is put.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual string ToStringImpl(int level) const override;

	ArithmeticInstr(Opcode opcode, Operand* left, Operand* right,
					Operand* result, Block* parent, Instruction* previous);

public:
	static ArithmeticInstr* GetArithmetic(Opcode opcode, Operand* left = nullptr,
										  Operand* right = nullptr, 
										  Operand* result = nullptr,
										  Block* parent = nullptr, 
										  Instruction* previous = nullptr);
	virtual ~ArithmeticInstr() {
		FreeOperand(sources_[0]);
		sources_[0] = nullptr;
		FreeOperand(sources_[1]);
		sources_[1] = nullptr;
		FreeOperand(result_);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

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

	// Returns 'true' if the division is guaranteed to not have a remainder.
	// Divisions involved in pointer arithmetic have this property, for example.
	bool HasNoRemainder() const {
		return other_ != 0;
	}

	void SetHasNoRemainder(bool value) {
		other_ = value;
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

	// Returns the mode in which the floating operation is performed.
	FloatMode GetFPMode() const {
		return (FloatMode)other_;
	}

	void SetFPMode(FloatMode mode) {
		other_ = (unsigned short)mode;
	}

	// Returns 'true' if the floating-point is 'exact'.
	bool IsExact() const {
		return (FloatMode)other_ == FloatMode::Exact;
	}

	// Returns 'true' if the floating-point is 'safe'.
	bool IsSafe() const {
		return (FloatMode)other_ == FloatMode::Safe;
	}

	// Returns 'true' if the floating-point is 'fast'.
	bool IsFast() const {
		return (FloatMode)other_ == FloatMode::Fast;
	}

	// Swaps the left and right operands.
	void SwapOperands() {
		auto temp = sources_[0];
		sources_[0] = sources_[1];
		sources_[1] = temp;
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
        return result_ != nullptr; 
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
	struct InstructionPromoter<ArithmeticInstr> {
		static bool Is(const Instruction* instr) {
			return instr->Category() == InstructionCategory::Arithmetic;
		}

		static ArithmeticInstr* As(Instruction* instr) {
			return Is(instr) ? static_cast<ArithmeticInstr*>(instr) : nullptr;
		}
	};
} // namespace Detail


// Macro for creating classes derived from 'ArithmeticInstr'.
#define MAKE_ARITHMETIC_INSTRUCTION(NAME) \
	class NAME##Instr : public ArithmeticInstr { \
	private: \
		NAME##Instr(Operand* left, Operand* right, Operand* result, \
					Block* parent, Instruction* previous) : \
				ArithmeticInstr(Opcode::##NAME, left, right, result, parent, previous) {} \
		\
	public: \
		static NAME##Instr* \
		Get##NAME(Operand* left = nullptr, Operand* right = nullptr, \
				  Operand* result = nullptr, Block* parent = nullptr, \
				  Instruction* previous = nullptr) { \
			NAME##Instr* instr = new NAME##Instr(left, right, result, parent, previous); \
			LinkWithPrevious(instr, parent, previous); \
			return instr; \
		} \
		\
		virtual void Accept(Visitor* v) override { \
			v->Visit(this); \
		} \
	}; \
	\
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

#define instruction(NAME, CAT, TEXT) MAKE_ARITHMETIC_INSTRUCTION(NAME)
#define ONLY_SELECTED
#define SELECT_ARITHMETIC
#include "Instructions.def"
#undef instruction
#undef ONLY_SELECTED
#undef SELECT_ARITHMETIC
#undef MAKE_ARITHMETIC_INSTRUCTION

} // namespace IR
#endif