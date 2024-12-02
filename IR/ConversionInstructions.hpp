// ConversionInstruction.hpp
// Copyright (c) Lup Gratian
//
// Defines the conversion instructions ('zext', 'ftoi', 'itop', etc.).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_CONVERSION_INSTRUCTION_HPP
#define PC_IR_CONVERSION_INSTRUCTION_HPP

#include "Instruction.hpp"
#include "IRType.hpp"
#include "Operand.hpp"
#include "Temporary.hpp"

namespace IR {

// Represents the class of instructions that operate on arithmetic operands.
// (trunc, zext, sext, ftoi, ftoui, itof, uitof, ftrunc, fext, ptoi, itop)
class ConversionInstr : public Instruction {
protected:
	Operand* target_;      // The operand to be converted.
	const Type* castType_; // The type of the result.
	Temporary* result_;    // The result operand.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual string ToStringImpl(int level) const override;

	ConversionInstr(Opcode opcode, Operand* target, const Type* castType,
					Operand* result, Block* parent, Instruction* previous);

public:
	static ConversionInstr* 
	GetConversion(Opcode opcode, Operand* target = nullptr, 
				  const Type* castType = nullptr, Operand* result = nullptr,
				  Block* parent = nullptr, Instruction* previous = nullptr);

	virtual ~ConversionInstr() {
		FreeOperand(target_);
		FreeOperand(result_);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

    bool IsUseless() const {
        return other_ != 0;
    }

    void SetIsUseless(bool value) {
        other_ = value;
    }

	// Returns the target operand of the instruction.
	Operand* TargetOp() {
		return target_;
	}

	// Returns the type to which the target operand is converted.
	const Operand* TargetOp() const {
		return target_;
	}

	void SetTargetOp(Operand* value) {
        NotifyOperandRemoved(target_, 0);
		FreeOperand(target_);
		
        LinkUser(value);
		target_ = value;
        
        NotifyOperandAdded(value, 0);
	}

	// Returns the type to which the target is cast.
	const Type* CastType() const {
		return castType_;
	}

	void SetCastType(const Type* value) {
		castType_ = value;
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

    // Methods for operand access.
    virtual int SourceOpCount() const override { 
        return 1; 
    }

	virtual Operand* GetSourceOp(int index) override {
		DebugValidator::IsSmaller(index, 1);
		return target_;
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::IsSmaller(index, 1);
		return target_;
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
        NotifyOperandRemoved(target_, 0);
		FreeOperand(target_);
		
        LinkUser(newOp);
		target_ = newOp;
        
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


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct InstructionPromoter<ConversionInstr> {
		static bool Is(const Instruction* instr) {
			return instr->Category() == InstructionCategory::Conversion;
		}

		static ConversionInstr* As(Instruction* instr) {
			return Is(instr) ? static_cast<ConversionInstr*>(instr) : nullptr;
		}
	};
} // namespace Detail


// Macro for creating classes derived from 'ConversionInstr'.
#define MAKE_CONVERSION_INSTRUCTION(NAME) \
	class NAME##Instr : public ConversionInstr { \
	private: \
		NAME##Instr(Operand* target, const Type* castType, Operand* result, \
					Block* parent, Instruction* previous) : \
				ConversionInstr(Opcode::##NAME, target, castType, result, parent, previous) {} \
		\
	public: \
		static NAME##Instr* \
		Get##NAME(Operand* target = nullptr, const Type* castType = nullptr, \
				  Operand* result = nullptr, Block* parent = nullptr, \
				  Instruction* previous = nullptr) { \
			NAME##Instr* instr = new NAME##Instr(target, castType, result, parent, previous); \
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

#define instruction(NAME, CAT, TEXT) MAKE_CONVERSION_INSTRUCTION(NAME)
#define ONLY_SELECTED
#define SELECT_CONVERSION
#include "Instructions.def"
#undef instruction
#undef ONLY_SELECTED
#undef SELECT_CONVERSION
#undef MAKE_ARITHMETIC_INSTRUCTION

} // namespace IR
#endif