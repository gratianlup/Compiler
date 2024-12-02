// ControlInstructions.hpp
// Copyright (c) Lup Gratian
//
// 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_CONTROL_INSTRUCTIONS_HPP
#define PC_IR_CONTROL_INSTRUCTIONS_HPP

#include "Instruction.hpp"
#include "IRType.hpp"
#include "Operand.hpp"
#include "References.hpp"
#include "Function.hpp"
#include "Intrinsic.hpp"
#include "Temporary.hpp"

namespace IR {

// The base class for all control instructions.
class ControlInstr : public Instruction {
protected:
	ControlInstr(Opcode opcode, int other = 0, Block* parent = nullptr,
				 Instruction* previous = nullptr) :
			Instruction(opcode, other, parent, previous) {}

	// Adds the parent block of this instruction 
    // as a predecessor in the target block.
	void AddPredecessorTo(BlockReference* target);

	// Removes the parent block of this instruction from the list 
    // of the predecessors in the target block.
	void RemovePredecessorFrom(BlockReference* target, bool removeAlways = false);

public:
	virtual ~ControlInstr() {}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct InstructionPromoter<ControlInstr> {
		static bool Is(const Instruction* instr) {
			return instr->Category() == InstructionCategory::Control;
		}

		static ControlInstr* As(Instruction* instr) {
			return Is(instr) ? static_cast<ControlInstr*>(instr) : nullptr;
		}
	};
} // namespace Detail


// Represents the conditional jump instruction.
class IfInstr : public ControlInstr {
protected:
	Operand* condition_;         // The condition to test.
	BlockReference* targets_[2]; // The target blocks for true (0) and false (1).

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	IfInstr(Operand* condition, BlockReference* trueTarget,
			BlockReference* falseTarget, Block* parent, Instruction* previous);

	virtual string ToStringImpl(int level) const override;

	virtual void RemovedFromParent() override {
		RemovePredecessorFrom(targets_[0], true /* removeAlways */);
		RemovePredecessorFrom(targets_[1], true /* removeAlways */);
        Instruction::RemovedFromParent();
	}

	virtual void HasNewParent() override {
		AddPredecessorTo(targets_[1]);
		AddPredecessorTo(targets_[0]);
        Instruction::HasNewParent();
	}

public:
	static IfInstr* GetIf(Operand* condition = nullptr, 
                          BlockReference* trueTarget = nullptr,
						  BlockReference* falseTarget = nullptr, 
                          Block* parent = nullptr,
						  Instruction* previous = nullptr);
	virtual ~IfInstr() {
		FreeOperand(condition_);
		FreeOperand(targets_[0]);
		targets_[0] = nullptr;
		FreeOperand(targets_[1]);
		targets_[1] = nullptr;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	Operand* ConditionOp() {
		return condition_;
	}

	const Operand* ConditionOp() const {
		return condition_;
	}

	void SetConditionOp(Operand* value) {
        NotifyOperandRemoved(condition_, 0);
		FreeOperand(condition_);
		
        LinkUser(value);
		condition_ = value;

        NotifyOperandAdded(value, 0);
	}

	// Returns the block reference used if the condition evaluates to 'true'.
	BlockReference* TrueTargetOp() {
		return targets_[0];
	}

	const BlockReference* TrueTargetOp() const {
		return targets_[0];
	}

	// Returns the block used if the condition evaluates to 'true'.
	Block* TrueTargetBlock() {
		DebugValidator::IsNotNull(targets_[0]);
		return targets_[0]->Target();
	}

	const Block* TrueTargetBlock() const {
		DebugValidator::IsNotNull(targets_[0]);
		return targets_[0]->Target();
	}

	void SetTrueTargetOp(BlockReference* value) {
        NotifyOperandRemoved(targets_[0], -1);
		RemovePredecessorFrom(targets_[0]);
		FreeOperand(targets_[0]);

		LinkUser(value);
		targets_[0] = value;
		AddPredecessorTo(value);

        NotifyOperandAdded(value, -1);
	}

	// Returns the block reference used if the condition evaluates to 'false'.
	BlockReference* FalseTargetOp() {
		return targets_[1];
	}

	const BlockReference* FalseTargetOp() const {
		return targets_[1];
	}

	// Returns the block used if the condition evaluates to 'false'.
	Block* FalseTargetBlock() {
		DebugValidator::IsNotNull(targets_[1]);
		return targets_[1]->Target();
	}

	const Block* FalseTargetBlock() const {
		DebugValidator::IsNotNull(targets_[1]);
		return targets_[1]->Target();
	}

	void SetFalseTargetOp(BlockReference* value) {
        NotifyOperandRemoved(targets_[1], -1);
		RemovePredecessorFrom(targets_[1]);
		FreeOperand(targets_[1]);
		
        LinkUser(value);
		targets_[1] = value;
		AddPredecessorTo(value);

        NotifyOperandAdded(value, -1);
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return 1; 
    }

	virtual Operand* GetSourceOp(int index) override {
		DebugValidator::IsSmaller(index, 1);
		return condition_;
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::IsSmaller(index, 1);
		return condition_;
	}

	virtual void ReplaceSourceOp(int index, Operand* newOp) override {
		DebugValidator::IsSmaller(index, 1);
	    SetConditionOp(newOp);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the high-level construct that generated the 'goto' instruction.
// Can be used as a hint for optimizations like block reordering.
enum class GotoOrigin {
	Unknown,  // Type not specified.
	Break,    // Originates from a 'break' statement.
	Continue, // Originates from a 'continue' statement.
	Goto,     // Originates from a 'goto' statement.
	Loop      // The back-edge of a loop.
};


// Represents the unconditional jump instruction.
class GotoInstr : public ControlInstr {
protected:
	BlockReference* target_; // The target block of the jump.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	GotoInstr(BlockReference* target, Block* parent, Instruction* previous);

	virtual string ToStringImpl(int level) const override;

	virtual void RemovedFromParent() override {
		RemovePredecessorFrom(target_);
        Instruction::RemovedFromParent();
	}

	virtual void HasNewParent() override {
		AddPredecessorTo(target_);
        Instruction::HasNewParent();
	}

public:
	static GotoInstr* GetGoto(BlockReference* target = nullptr, 
                              Block* parent = nullptr, 
                              Instruction* previous = nullptr);
	virtual ~GotoInstr() {
		FreeOperand(target_);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	// Returns the block reference to which the jump is made.
	BlockReference* TargetOp() {
		return target_;
	}

	const BlockReference* TargetOp() const {
		return target_;
	}

	// Returns the block to which the jump is made.
	Block* TargetBlock() {
		DebugValidator::IsNotNull(target_);
		return target_->Target();
	}

	const Block* TargetBlock() const {
		DebugValidator::IsNotNull(target_);
		return target_->Target();
	}

	void SetTargetOp(BlockReference* value) {
        NotifyOperandRemoved(target_, -1);
		RemovePredecessorFrom(target_);
		FreeOperand(target_);

		LinkUser(value);
		target_ = value;
		AddPredecessorTo(value);

        NotifyOperandAdded(value, -1);
	}

	// Returns the type of the statement from which the 'goto' originates.
	GotoOrigin GetGotoOrigin() const {
		return (GotoOrigin)other_;
	}

	void SetGotoOrigin(GotoOrigin value) {
		other_ = (unsigned short)value;
	}

	// Returns 'true' if the instruction originates from a 'break' statement.
	bool IsFromBreak() const {
		return GetGotoOrigin() == GotoOrigin::Break;
	}

	// Returns 'true' if the instruction originates from a 'continue' statement.
	bool IsFromContinue() const {
		return GetGotoOrigin() == GotoOrigin::Continue;
	}

	// Returns 'true' if the instruction originates from a 'goto' statement.
	bool IsFromGoto() const {
		return GetGotoOrigin() == GotoOrigin::Goto;
	}

	// Returns 'true' if the instruction is a loop back-edge.
	bool IsFromLoop() const {
		return GetGotoOrigin() == GotoOrigin::Loop;
	}

	// Returns 'true' if the origin of the instruction is unknown.
	bool HasUnknownOrigin() const {
		return GetGotoOrigin() == GotoOrigin::Unknown;
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return 0; 
    }

	virtual Operand* GetSourceOp(int index) override {
		DebugValidator::Unreachable();
        return nullptr;
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::Unreachable();
        return nullptr;
	}

	virtual void ReplaceSourceOp(int index, Operand* newOp) override {
		DebugValidator::Unreachable();
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the instruction used to call a function.
class CallInstr : public ControlInstr {
public:
	typedef List<Operand*> ArgumentList;

protected:
	Operand* target_;    // The function to call.
	Temporary* result_;  // The operand where the result will be stored.
	ArgumentList* args_; // The list of arguments (if any).
    int id_;

    static int nextId_;  // The Id assigned to the next created 'call'.

	// Bit 0 of 'other_' is a flag indicating whether to use the calling convention
	// from the function or the one specified in the call.

    // Bit 1 indicates if this is a math function from the standard library
    // that should not be converted to an intrinsic.
	
    // The rest of the bits store the calling convention.
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	CallInstr(Operand* target, Operand* result, int argCapacity, 
			  Block* parent, Instruction* previous);

	virtual string ToStringImpl(int level) const override;

public:
	static CallInstr* GetCall(Operand* target = nullptr, Operand* result  = nullptr, 
							  int argCapacity = 0, Block* parent = nullptr, 
							  Instruction* previous = nullptr);
	virtual ~CallInstr();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

    // Returns the Id of the 'call' instruction.
    // Note that this Id is unique only inside the functio
    // (there might be a call with the same Id in another function).
    int Id() const {
        return id_;
    }

	// Returns the function to which the jump is made.
	Operand* TargetOp() {
		return target_;
	}

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

	// Returns the called function, if the target is a function reference.
	// Otherwise 'nullptr' is returned for indirect calls.
	Function* GetCalledFunction() {
		if(auto functionRef = target_->As<FunctionReference>()) {
			return functionRef->Target();
		}
		else return nullptr;
	}

	// Returns the list of arguments, or 'nullptr' if the call has no arguments.
	const ArgumentList* Arguments() const {
		return args_;
	}

    ArgumentList* Arguments() {
		return args_;
	}

	// Returns the number of arguments.
	int ArgumentCount() const {
		return args_ ? args_->Count() : 0;
	}

	// Returns 'true' if the call has any arguments.
	bool HasArguments() const {
		return args_ ? (args_->Count() > 0) : false;
	}

	// Returns the argument operand found a the specified index.
	Operand* GetArgument(int index) {
		DebugValidator::IsTrue(HasArguments());
		return (*args_)[index];
	}

	const Operand* GetArgument(int index) const {
		DebugValidator::IsTrue(HasArguments());
		return (*args_)[index];
	}

	// Adds the specified argument.
	void AddArgument(Operand* argument) {
		if(args_ == nullptr) {
			// Create an argument list now.
			args_ = new ArgumentList();
		}

		LinkUser(argument);
		args_->Add(argument);

        NotifyOperandAdded(argument, args_->Count());
	}

	// Replaces the argument operand found at the specified index with another one.
	void ReplaceArgument(int index, Operand* newOp) {
		DebugValidator::IsTrue(HasArguments());
        NotifyOperandRemoved((*args_)[index], index + 1);
		FreeOperand((*args_)[index]);

		LinkUser(newOp);
		(*args_)[index] = newOp;

        NotifyOperandAdded(newOp, index + 1);
	}

	// Removes the argument found at the specified index.
	void RemoveArgument(int index) {
		DebugValidator::IsTrue(HasArguments());
        NotifyOperandRemoved((*args_)[index], index + 1);

		FreeOperand((*args_)[index]);
		args_->RemoveAt(index);
	}

    // Performs the specified action on each argument.
    // void Predicate(Operand* argument, int argumentIndex);
    template<class Predicate>
    void ForEachArgument(Predicate action) {
        if(args_ == nullptr) {
            return;
        }

        for(int i = 0; i < args_->Count(); i++) {
            if(action((*args_)[i], i) == false) {
                return;
            }
        }
    }

    // Performs the specified action only on the arguments
    // equal with the required argument operand.
    // void Predicate(int argumentIndex);
    template<class Predicate>
    void ForEachEqualArgument(Operand* requiredOp, Predicate action) {
        if(args_ == nullptr) {
            return;
        }

        for(int i = 0; i < args_->Count(); i++) {
            auto argument = (*args_)[i];

            if((argument == requiredOp) && action(i) == false) {
                return;
            }
        }
    }
    
    // Performs the specified action only on the arguments
    // whose type is the required one.
    // void Predicate(Operand* argument, int argumentIndex);
    template<class T, class Predicate>
    void ForEachArgumentOfType(Predicate action) {
        if(args_ == nullptr) {
            return;
        }

        for(int i = 0; i < args_->Count(); i++) {
            auto argument = (*args_)[i];

            if(argument->GetType()->Is<T>()) {
                if(action(argument, i) == false) {
                    return;
                }
            }
        }
    }

	// Returns the operand where the returned value will be stored.
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

	// Returns 'true' if the target of the call is an intrinsic.
	bool IsIntrinsic() const {
		if(auto temp = target_->As<FunctionReference>()) {
            return temp->IsIntrinsic();
		}
		return false;
	} 

    // Returns the called intrinsic, if the target function is an intrinsic,
    // and 'nullptr' otherwise.
    Intrinsic* GetIntrinsic() {
        if(auto temp = target_->As<FunctionReference>()) {
			return temp->Target()->As<Intrinsic>();
		}
        else return nullptr;
    }

    const Intrinsic* GetIntrinsic() const {
        if(auto temp = target_->As<FunctionReference>()) {
            return temp->Target()->As<Intrinsic>();
		}
        else return nullptr;
    }

    // Returns 'true' if the called function is an intrinsic
    // having the specified type.
    template <class T>
    bool IntrinsicIs() const {
        if(auto intrinsic = GetIntrinsic()) {
            return intrinsic->Is<T>();
        }
        else return false;
    }

    // Returns the called intrinsic, if we call an intrinsic
    // having the specified type. Otherwise, 'nullptr' is returned.
    template <class T>
    T* GetIntrinsicAs() {
        if(auto intrinsic = GetIntrinsic()) {
            return intrinsic->As<T>();
        }
        else return false;
    }

    template <class T>
    const T* GetIntrinsicAs() const {
        if(auto intrinsic = GetIntrinsic()) {
            return intrinsic->As<T>();
        }
        else return false;
    }

	// Returns the type of the function that is called.
	const FunctionType* CalledFunctionType() const {
        // The type of the target is 'pointer to function'.
		DebugValidator::IsNotNull(target_);
        auto functPtrType = target_->GetType()->As<PointerType>();
        return functPtrType->PointeeType()->As<FunctionType>();
	}

	// Returns 'true' if the called function has no return value (it's 'void').
	bool IsVoid() const {
		return CalledFunctionType()->IsVoid();
	}

	// Returns 'true' if the call convention of the function is overridden.
	bool HasOverridenCallConvention() const {
		return (other_ & 1) != 0;
	}

	void SetHasOverridenCallConvention(bool value) {
		if(value) other_ |= 1;
		else other_ &= ~1;
	}

    // Returns 'true' if the call to a math-related function
    // is allowed to be promoted to an intrinsic.
    bool IsPromotableMathFunction() const {
        return (other_ & 2) == 0;
    }

    void SetIsPromotableMathFunction(bool value) {
        if(value) other_ &= ~2;
		else other_ |= 2;
    }

	// Returns the overridden call convention.
	CallConventionType CallConvention() const {
		DebugValidator::IsTrue(HasOverridenCallConvention());
		return (CallConventionType)((other_ & ~3) >> 2);
	}
    
	void SetCallConvention(CallConventionType value) {
		other_ |= 1 | ((unsigned short)value << 2);
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return 1 + ArgumentCount(); 
    }

	virtual Operand* GetSourceOp(int index) override {
		if(index == 0) {
            return target_;
        }
		else if((index - 1) < ArgumentCount()) {
			return (*args_)[index - 1];
		}

		DebugValidator::Unreachable();
		return nullptr;
	}

	virtual const Operand* GetSourceOp(int index) const override {
		if(index == 0) {
            return target_;
        }
		else if((index - 1) < ArgumentCount()) {
			return (*args_)[index - 1];
		}
		
		DebugValidator::Unreachable();
		return nullptr;
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
		if(index == 0) {
			SetTargetOp(newOp);
		}
		else if((index - 1) < ArgumentCount()) {
			ReplaceArgument(index - 1, newOp);
		}
		else DebugValidator::Unreachable();
	}

	virtual void ReplaceDestinationOp(Temporary* newOp) override {
		SetResultOp(newOp);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the instruction used to return from a function.
// If the function returns 'void' it can have no operand.
class ReturnInstr : public ControlInstr {
protected:
	Operand* return_; // The operand to be returned.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ReturnInstr(Operand* returnOp, Block* parent, Instruction* previous);

	virtual string ToStringImpl(int level) const override;

public:
	static ReturnInstr* GetReturn(Operand* returnOp = nullptr, 
                                  Block* parent = nullptr,
								  Instruction* previous = nullptr);
	virtual ~ReturnInstr() {
		FreeOperand(return_);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	// Returns the operand with the value returned by the function.
	Operand* ReturnedOp() {
		return return_;
	}

	const Operand* ReturnedOp() const {
		return return_;
	}

	void SetReturnedOp(Operand* value) {
        NotifyOperandRemoved(return_, 0);
		FreeOperand(return_);

		LinkUser(value);
		return_ = value;

        NotifyOperandAdded(value, 0);
	}

	// Returns 'true' if no value is returned.
	bool IsVoid() const {
		return return_ == nullptr;
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return IsVoid() ? 0 : 1; 
    }

	virtual Operand* GetSourceOp(int index) {
		DebugValidator::IsSmaller(index, 1);
		return return_;
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::IsSmaller(index, 1);
		return return_;
	}

	virtual void ReplaceSourceOp(int index, Operand* newOp) override {
		DebugValidator::IsSmaller(index, 1);
		SetReturnedOp(newOp);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};

// Represents a 'case' statement in the switch instruction.
struct SwitchCase {
	__int64 Value;
	BlockReference* Target;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	SwitchCase() : Value(0), Target(nullptr) {}

    SwitchCase(const SwitchCase& other) :
            Value(other.Value), Target(other.Target) {}

	explicit SwitchCase(__int64 value, BlockReference* target) :
            Value(value), Target(target) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    unsigned int GetHashCode() const {
        return (unsigned)Value ^ (unsigned)(Value >> 32);
    }

    bool operator== (const SwitchCase& other) const {
        return Value == other.Value;
    }

    bool operator!= (const SwitchCase& other) const {
        return Value != other.Value;
    }

    bool operator< (const SwitchCase& other) const {
        return Value < other.Value;
    }

    bool operator<= (const SwitchCase& other) const {
        return Value <= other.Value;
    }

    bool operator> (const SwitchCase& other) const {
        return Value > other.Value;
    }

    bool operator>= (const SwitchCase& other) const {
        return Value >= other.Value;
    }
};


// Represents the 'switch' instruction, used to jump to a block based
// on the value of the condition operand.
class SwitchInstr : public ControlInstr {
private:
	Operand* condition_;            // The operand used to select the target block.
	BlockReference* defaultTarget_; // The target if no case values match.
	List<SwitchCase> caseList_;

protected:
	SwitchInstr(Operand* condition, int caseListCapacity, 
                BlockReference* defaultTarget, Block* parent,
                Instruction* previous);

	virtual string ToStringImpl(int level) const override;

	virtual void RemovedFromParent() override {
		RemovePredecessorFrom(defaultTarget_, true /* removeAlways */);		

		for(int i = 0; i < caseList_.Count(); i++) {
			RemovePredecessorFrom(caseList_[i].Target, true /* removeAlways */);
		}

        Instruction::RemovedFromParent();
	}

	virtual void HasNewParent() override {
		AddPredecessorTo(defaultTarget_);

		for(int i = 0; i < caseList_.Count(); i++) {
			AddPredecessorTo(caseList_[i].Target);
		}

        Instruction::HasNewParent();
	}

public:
	static SwitchInstr* GetSwitch(Operand* condition, int caseListCapacity,
								  BlockReference* defaultTarget = nullptr,
								  Block* parent = nullptr, 
                                  Instruction* previous = nullptr);
	virtual ~SwitchInstr() {
		FreeOperand(condition_);
		FreeOperand(defaultTarget_);

		// Free all case targets.
		for(int i = 0; i < caseList_.Count(); i++) {
			FreeOperand(caseList_[i].Target);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual Instruction* Clone() override;

	// Returns the list of 'case' statements.
	const List<SwitchCase>& CaseList() const {
		return caseList_;
	}

    void SortCases() {
        caseList_.Sort();
    }

	// Adds a new case having the specified value and target.
	void AddCase(__int64 value, BlockReference* target) {
		LinkUser(target);
		AddPredecessorTo(target);
		caseList_.Add(SwitchCase(value, target));

        NotifyOperandAdded(target, -1);
	}

	// Returns the case found at the specified index.
	SwitchCase& GetCase(int index) {
		return caseList_[index];
	}

	const SwitchCase& GetCase(int index) const {
		return caseList_[index];
	}

    // Returns the block that is the target associated with the specified value,
    // and if not found, depending on the 'defaultIfNotFound' flag it returns
    // the default target block or 'nullptr'.
    // Note that this performs a linear search of the case list.
    Block* GetTargetForValue(__int64 value, bool defaultIfNotFound = false);

    // Sets 'value' to the value of the case associated with the specified block.
    // If such a case is found it returns 'true', else it returns 'false'.
    // Note that this performs a linear search of the case list.
    bool GetValueForTarget(Block* target, __int64& value);

	// Replaces the case found at the specified index with another one.
	void ReplaceCase(int index, __int64 newValue, BlockReference* newTarget) {
		RemovePredecessorFrom(caseList_[index].Target);
        NotifyOperandRemoved(caseList_[index].Target, -1);
		FreeOperand(caseList_[index].Target);
		LinkUser(newTarget);

		caseList_[index].Value = newValue;
		caseList_[index].Target = newTarget;
		AddPredecessorTo(newTarget);

        NotifyOperandAdded(newTarget, -1);
	}

	// Removes the case found at the specified index.
	void RemoveCase(int index) {
        NotifyOperandRemoved(caseList_[index].Target, -1);
		RemovePredecessorFrom(caseList_[index].Target);

		FreeOperand(caseList_[index].Target);
		caseList_.RemoveAt(index);
	}

	// Returns the number of case targets.
	int CaseCount() const {
		return caseList_.Count();
	}

    // Calls the specified predicate for each case.
    // bool Predicate(SwitchCase switchCase)
    template <class Predicate>
    void ForEachCase(Predicate action) {
        for(int i = 0; i < caseList_.Count(); i++) {
            if(action(caseList_[i]) == false) {
                return; // The user aborted the enumeration.
            }
        }
    }

	// Returns the operand whose value is used to select the target block.
	Operand* ConditionOp() {
		return condition_;
	}

	const Operand* ConditionOp() const {
		return condition_;
	}

	void SetConditionOp(Operand* value) {
        NotifyOperandRemoved(condition_, 0);
		FreeOperand(condition_);
		LinkUser(value);
		condition_ = value;
        NotifyOperandAdded(condition_, 0);
	}

	// Returns the target block reference used when 
	// none of the 'case' values matches with the condition value.
	BlockReference* DefaultTargetOp() {
		return defaultTarget_;
	}

	const BlockReference* DefaultTargetOp() const {
		return defaultTarget_;
	}

	// Returns the target block used when  none of the 'case' 
	// values matches with the condition value.
	Block* DefaultTargetBlock() {
		DebugValidator::IsNotNull(defaultTarget_);
		return defaultTarget_->Target();
	}

	const Block* DefaultTargetBlock() const {
		DebugValidator::IsNotNull(defaultTarget_);
		return defaultTarget_->Target();
	}

	void SetDefaultTargetOp(BlockReference* value) {
		RemovePredecessorFrom(defaultTarget_);
        NotifyOperandRemoved(defaultTarget_, -1);
		FreeOperand(defaultTarget_);

		LinkUser(value);
		defaultTarget_ = value;
		AddPredecessorTo(value);

        NotifyOperandAdded(value, -1);
	}

	// Methods for operand access.
	virtual int SourceOpCount() const override { 
        return 1; 
    }

	virtual Operand* GetSourceOp(int index) override {
		DebugValidator::IsSmaller(index, 1);
		return condition_;
	}

	virtual const Operand* GetSourceOp(int index) const override {
		DebugValidator::IsSmaller(index, 1);
        return condition_;
	}

	virtual void ReplaceSourceOp(int index, Operand* newOp) override {
		DebugValidator::IsSmaller(index, 1);
		SetConditionOp(newOp);
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
#define SELECT_CONTROL
#include "Instructions.def"
#undef instruction
#undef ONLY_SELECTED
#undef SELECT_CONTROL
#undef MAKE_PROMOTION_HELPER

} // namespace IR
#endif