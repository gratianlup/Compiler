// Block.hpp
// Copyright (c) Lup Gratian
//
// Represents a basic-block, which is a series of instructions
// that ends with a branching instruction (goto, if or switch).
// Note that 'call' instructions don't end a basic-block.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_BASIC_BLOCK_HPP
#define PC_IR_BASIC_BLOCK_HPP

#include "Symbol.hpp"
#include "Tagged.hpp"
#include "Instruction.hpp"
#include "IntrusiveList.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class Unit;
class Function;
class BlockReference;
class Block;

// Helper used to access the successors of the block
// without knowing the type of the branching instruction.
class SuccessorHelper {
public:
	// Returns the number of successors defined by the specified instruction.
	static int SuccessorCount(const Instruction* instr);

	// Returns the block reference for the successor found at the specified position.
	static BlockReference* SuccessorRefAt(Instruction* instr, int index);

	// Returns the successor found at the specified position.
	static Block* SuccessorAt(Instruction* instr, int index);

	static const Block* SuccessorAt(const Instruction* instr, int index) {
		return SuccessorAt(const_cast<Instruction*>(instr), index);
	}

    // Sets the specified block as a successor for the branching instruction.
    static void SetSuccessorAt(Instruction* instr, BlockReference* reference, int index);
};


// Represents a basic block, the place where instructions are stored.
class Block : public Symbol, public IntrusiveHeader<Block>,
			  protected IntrusiveList<Instruction>,
              public Tagged<BlockTag> {
private:
	// The type of the list that stores the predecessors.
	// Most blocks don't have more than 4 predecessors, so we use a 'StaticList'.
	typedef StaticList<Block*, 4> PredecessorList;
    typedef List<Instruction*> InstructionList;

	Function* parent_;       // The parent function.
	PredecessorList preds_; // The list with the predecessors.

protected:
	typedef IntrusiveHeader<Block> THeader;
	typedef IntrusiveList<Instruction> IL;

	Block(shared<string> name, Function* parent, Block* previous);

	virtual string ToStringImpl(int level) const override;

    // Method that should notify the associated tags
    // when a new instruction is added to the block.
    virtual void NotifyInstructionAdded(Instruction* instr);

    // Method that should notify the associated tags
    // when an instruction has been removed from the block.
    virtual void NotifyInstructionRemoved(Instruction* instr);

    // Method that should notify the associated tags
    // when the block was added to a new function.
    virtual void NotifyAddedToFunction(Function* function);

    // Method that should notify the associated tags
    // when the block was removed from its parent function.
    virtual void NotifyRemovedFromFunction(Function* function);

public:
	// Allows iterating over all predecessors of a block.
	class PredecessorEnum {
	private:
		const PredecessorList* list_; // The list with the predecessors.
		mutable int position_;         // The current position.

	public:
		PredecessorEnum(const PredecessorList* list);
		PredecessorEnum(const PredecessorEnum& other);

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		// Returns the current predecessor and advances the position.
		// If the last predecessor has been reached it returns 'nullptr'.
		const Block* Next() const;

		Block* Next() {
			auto result = static_cast<const PredecessorEnum*>(this)->Next();
			return const_cast<Block*>(result);
		}

		// Returns 'true' if 'Next' will return a valid predecessor.
		bool IsValid() const {
			return position_ < list_->Count();
		}

		PredecessorEnum& operator= (const PredecessorEnum& other);
	};

	// Allows iterating over all successors of a block.
	class SuccessorEnum {
	private:
		const Instruction* lastInstr_; // The branching instruction with the successors.
		int successors_;               // The number of successors.
		mutable int position_;         // The current position.

	public:
		SuccessorEnum(const Instruction* lastInstr);
		SuccessorEnum(const SuccessorEnum& other);

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		// Returns the current successor and advances the position.
		// If the last successor has been reached it returns 'nullptr'.
		const Block* Next() const;

		Block* Next() {
			auto result = static_cast<const SuccessorEnum*>(this)->Next();
			return const_cast<Block*>(result);
		}

		// Returns 'true' if 'Next' will return a valid successor.
		bool IsValid() const {
			return position_ < successors_;
		}

		SuccessorEnum& operator= (const SuccessorEnum& other);
	};

	typedef PredecessorEnum PE;
	typedef SuccessorEnum SE;

	friend class PredecessorEnum; // Give it access to 'preds_'.
	friend class SuccessorEnum; // Give it access to the last instruction.

	// Factory methods for creating block instances.
	static Block* GetBlock(const string& name, Function* parent = nullptr,
						   Block* previous = nullptr);
	virtual ~Block();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Methods called by Function to notify the tags
    // associated with the block about certain events.
    virtual void NotifyParameterAdded(Variable* parameter);
    virtual void NotifyParameterRemoved(Variable* parameter);
    virtual void NotifyVariableAdded(Variable* variable);
    virtual void NotifyVariableRemoved(Variable* variable);

    // Returns a reference that point to the block.
    // Note that the block must be already attached to a function.
    BlockReference* GetReference();

    // Returns the number of loops the block is part of.
    // Can be used by the frontend to indicate the loop regions,
    // fact that can be exploited by some optimizations.
    int LoopDepth() { 
        return other_;
    }

    void SetLoopDepth(int value) {
        value = value > 100 ? 100 : value;
        other_ = value;
    }

    // Returns 'true' if the frontend indicates
    // that the basic block is found inside a loop.
    bool IsInLoop() { 
        return LoopDepth() > 0;
    }

	// Methods for navigating to the next/previous block.
	Block* NextBlock() { 
        return static_cast<Block*>(THeader::Next()); 
    }
	
    const Block* NextBlock() const { 
        return static_cast<const Block*>(THeader::Next()); 
    }

	Block* PreviousBlock() { 
        return static_cast<Block*>(THeader::Previous()); 
    }
	
    const Block* PreviousBlock() const { 
        return static_cast<const Block*>(THeader::Previous()); 
    }

	// Returns 'true' if this is the first block of the function.
    // The first block is sometimes called the entry block.
	bool IsFirstBlock() const {
		return THeader::Previous() == nullptr;
	}

	// Returns 'true' if this is the last block of the function.
	bool IsLastBlock() const {
		return THeader::Next() == nullptr;
	}

	// Returns the function to which this block belongs.
	Function* ParentFunction() {
		return parent_;
	}

	const Function* ParentFunction() const {
		return parent_;
	}

    // Returns the unit to which the function parent belongs.
    Unit* ParentUnit();

    const Unit* ParentUnit() const;

	void SetParentFunction(Function* value) {
        NotifyRemovedFromFunction(parent_);
		parent_ = value;
        NotifyAddedToFunction(value);
	}

	// Methods for manipulating the list of instructions.
	Instruction* FirstInstruction() { 
        return static_cast<Instruction*>(IL::First()); 
    }
	
	const Instruction* FirstInstruction() const {
		return static_cast<const Instruction*>(IL::First());
	}

	Instruction* LastInstruction() { 
        return static_cast<Instruction*>(IL::Last()); 
    }

	const Instruction* LastInstruction() const {
		return static_cast<const Instruction*>(IL::Last());
	}

	void InsertInstructionFirst(Instruction* value);
	void InsertInstructionLast(Instruction* value);

	void InsertInstruction(Instruction* value) { 
        InsertInstructionLast(value);
    }

	void InsertInstructionBefore(Instruction* value, Instruction* other);
	void InsertInstructionAfter(Instruction* value, Instruction* other);
	void RemoveInstruction(Instruction* value, bool free = false, bool notify = true);

    // Removes all instructions matching the specified predicate.
    // If 'removedInstrs' is specified the removed instructions are added
    // to the list and not deleted, otherwise they are deleted.
    // bool Predicate(Instruction* instr);
    template <class Predicate>
    int RemoveInstructionsMatching(Predicate action, InstructionList* removedInstrs = nullptr) {
        int removedCount = 0;
        auto instr = FirstInstruction();

        while(instr) {
            auto nextInstr = instr->NextInstruction();

            if(action(instr)) {
                removedCount++;

                if(removedInstrs) {
                    removedInstrs->Add(instr);
                    RemoveInstruction(instr, false /* free */);
                }
                else RemoveInstruction(instr, true /* free */);
            }

            instr = nextInstr;
        }

        return removedCount;
    }

	// Returns 'true' if the specified instruction is found in this block.
	bool HasInstruction(const Instruction* instr) const;

    // Returns 'true' if any instruction matches the specified predicate.
    // bool Predicate(Instruction* instr) const;
    template <class Predicate>
    bool HasInstructionMatching(Predicate action) const {
        for(auto instr = FirstInstruction(); instr; instr = instr->NextInstruction()) {
            if(action(instr)) {
                return true;
            }
        }

        return false;
    }

    // Returns 'true' if the block contains instructions of the specified type.
    // bool Predicate(Instruction* instr) const;
    template <class T>
    bool HasInstructionOfType() const {
        for(auto instr = FirstInstruction(); instr; instr = instr->NextInstruction()) {
            if(instr->Is<T>()) {
                return true;
            }
        }

        return false;
    }

	// Returns the branching instruction from the block. 
	// If such an instruction is not found nullptr is returned.
	Instruction* BranchInstruction() {
		if(LastInstruction() && LastInstruction()->IsBranching()) {
			return LastInstruction();
		}
		else return nullptr;
	}

	const Instruction* BranchInstruction() const {
		if(LastInstruction() && LastInstruction()->IsBranching()) {
			return LastInstruction();
		}
		else return nullptr;
	}

	// Removes all instructions from the block, optionally freeing them.
	void ClearInstructions(bool free = false, bool fromDestructor = false);

	// Returns the number of instructions in the block.
	int InstructionCount() const { 
		return IL::Count(); 
	}
    
    // Returns the number of instructions in the block, not considering 'phi'.
    int InstructionCountWithoutPhis() const;

	// Returns 'true' if the block has no instructions.
	bool IsEmpty() const {
		return IL::Count() == 0;
	}

	// Returns 'true' if the block contains a single instruction,
	// and that one is a branching instruction ('if', 'goto', 'switch' or 'ret').
	bool HasOnlyBranching() const {
		return (IsEmpty() == false) && 
               (BranchInstruction() == FirstInstruction());
	}

    // Returns 'true' if this block is the entry point of its parent function.
    bool IsFunctionEntry() const {
        return PreviousBlock() == nullptr;
    }

    // Returns 'true' if this block ends with a 'ret' instruction.
    bool IsFunctionReturn() const {
        return BranchInstruction() &&
               BranchInstruction()->IsReturn();
    }

	void ReplaceInstructionWith(Instruction* oldInstr, Instruction* newInstr) {
		DebugValidator::IsNotNull(oldInstr);
		DebugValidator::IsNotNull(newInstr);
		
		auto previous = oldInstr->PreviousInstruction();
		RemoveInstruction(oldInstr);

		if(previous) {
            InsertInstructionAfter(newInstr, previous);
        }

		else InsertInstructionFirst(newInstr);
	}

	// Replaces the specified instruction with a series of new instructions.
	template <int N>
	void ReplaceInstructionWith(Instruction* oldInstr, 
                                const StaticList<Instruction*, N>& newList) {
		DebugValidator::IsNotNull(oldInstr);

		newList.ForEach([](Instruction* instr) -> bool { 
			DebugValidator::IsNotNull(instr)
			return true;
		});
		
		Instruction* temp = oldInstr->PreviousInstruction();
		RemoveInstruction(oldInstr);

		for(int i = 0; i < N; i++) {
			if(temp) InsertInstructionAfter(newList[i], temp);
			else InsertInstructionFirst(newList[i]);
			temp = newList[i];
		}
	}

	// Calls the specified predicate for each instruction in the block.
	// bool Predicate(Instruction* instr)
	template <class Predicate>
	void ForEachInstruction(Predicate action) {
		Instruction* instr = FirstInstruction();
		
        while(instr) {
			if(action(instr) == false) {
				return; // The user aborted.
			}

			instr = instr->NextInstruction();
		}
	}

    // Calls the specified predicate for each instruction in the block
    // that is not a 'phi' instruction.
    // bool Predicate(Instruction* instr)
    template <class Predicate>
    void ForEachNonPhiInstruction(Predicate action) {
        Instruction* instr = FirstNonPhiInstruction();

        while(instr) {
            if((instr->IsPhi() == false) && (action(instr) == false)) {
                return; // The user aborted.
            }

            instr = instr->NextInstruction();
        }
    }

    // Calls the specified predicate for each instruction
    // in the function that matches the specified type.
    // bool Predicate(Instruction* instr)
    template <class T, class Predicate>
    void ForEachInstructionOfType(Predicate action) {
        Instruction* instr = FirstInstruction();
		
        while(instr) {
            if(auto matchingInstr = instr->As<T>()) {
			    if(action(matchingInstr) == false) {
				    return; // The user aborted.
			    }
            }

			instr = instr->NextInstruction();
		}
    }

    // Calls the specified predicate for each 'phi' instruction in the block.
	// bool Predicate(PhiInstr* instr)
	template <class Predicate>
	void ForEachPhiInstruction(Predicate action) {
		Instruction* instr = FirstInstruction();
		
        if(instr == nullptr) return;

        while(auto phiInstr = instr->As<PhiInstr>()) {
			if(action(phiInstr) == false) {
				return; // The user aborted.
			}

			instr = instr->NextInstruction();
            if(instr == nullptr) break;
		}
	}

	// Returns 'true' if the block has no predecessors,
	// and it's not the entry block of the function (always reachable).
	bool IsUnreachable() const {
		return (preds_.Count() == 0) && 
               (IsFirstBlock() == false);
    }

	// Returns the number of predecessor blocks.
	int PredecessorCount() const {
		return preds_.Count();
	}

	// Returns 'true' if the block has any predecessors.
	int HasPredecessors() const {
		return preds_.Count() > 0;
	}

	// Returns 'true' if this block has a single predecessor block.
	bool HasSinglePredecessor() const {
		return preds_.Count() == 1;
	}

	// Returns the predecessor found at the specified position.
	Block* PredecessorAt(int index) {
		return preds_[index];
	}

	const Block* PredecessorAt(int index) const {
		return preds_[index];
	}

	// Adds the specified block as the predecessor of this block.
	void AddPredecessor(Block* predecessor) {
		DebugValidator::IsNotNull(predecessor);

		// Add it only if not already in the list.
		if(preds_.Contains(predecessor) == false) {
			preds_.Add(predecessor);
		}
	}

	// Removes the predecessor found at the specified position.
	void RemovePredecessorAt(int index) {
		preds_.RemoveAt(index);
	}

	// Removes the specified predecessor.
	void RemovePredecessor(Block* predecessor) {
		DebugValidator::IsNotNull(predecessor);
		preds_.Remove(predecessor);
	}

	// Returns 'true' if the specified block is a predecessor for this block.
	bool HasPredecessor(Block* predecessor) {
		DebugValidator::IsNotNull(predecessor);
		return preds_.Contains(predecessor);
	}

	// Removes all predecessor blocks.
	void RemoveAllPredecessors() {
		preds_.Clear();
	}

	// Replaces the predecessor found at the specified index with a new block.
	void ReplacePredecessor(int index, Block* predecessor) {
		DebugValidator::IsNotNull(predecessor);
		preds_[index] = predecessor;
	}

    // Replaces the specified predecessor with a new block.
    void ReplacePredecessor(Block* oldPredecessor, Block* newPredecessor);

	// Returns an enumerator that can be used to enumerate all the predecessors.
	PE GetPredecessorEnum() {
		return PE(&preds_);
	}

	const PE GetPredecessorEnum() const {
		return PE(&preds_);
	}

	// Calls the specified predicate for each predecessor.
	// bool Predicate(Block* block, int index)
	template <class Predicate>
	void ForEachPredecessor(Predicate action) {
		for(int i = 0; i < preds_.Count(); i++) {
			if(action(preds_[i], i) == false) {
				return; // The user aborted.
			}
		}
	}

    // Calls the specified predicate for each predecessor (const version).
	// bool Predicate(Block* block, int index)
    template <class Predicate>
	void ForEachPredecessor(Predicate action) const {
		for(int i = 0; i < preds_.Count(); i++) {
			if(action(preds_[i], i) == false) {
				return; // The user aborted.
			}
		}
	}

	// Returns the number of successor blocks.
	int SuccessorCount() const {
		if(LastInstruction()) {
            return SuccessorHelper::SuccessorCount(LastInstruction());
        }
		else return 0; // There are no instructions in the block.
	}

	// Returns 'true' if the block has any successors.
	bool HasSuccessors() const {
		return SuccessorCount() > 0;
	}

	// Returns 'true' if this block has a single successor block.
	bool HasSingleSuccessor() const {
		return SuccessorCount() == 1;
	}

	// Returns 'true' if the specified block is a successor for this block.
	bool HasSuccessor(const Block* successor) const;

    // Returns the number of blocks that are jumping
    // to the specified block.
    int SuccessorsJumpingTo(const Block* block) const;
    int SuccessorsJumpingTo(const BlockReference* blockRef) const;

	// Returns the successor found at the specified position.
	Block* SuccessorAt(int index);
	const Block* SuccessorAt(int index) const;

	// Replaces the successor found at the specified index with a new block.
	void ReplaceSuccessor(int index, Block* newSuccessor);
    void ReplaceSuccessor(int index, BlockReference* newSuccessor);

    // Replaces the specified successor block whit a new one.
    // Note that all instances of the old successor are replaced.
    void ReplaceSuccessor(Block* oldSuccessor, Block* newSuccessor);
    void ReplaceSuccessor(Block* oldSuccessor, BlockReference* newSuccessor);

	// Creates a 'goto' instruction that links this block
    // with the specified one. Any branching instruction found
    // in the block will be removed first.
	void LinkWith(Block* successor);
    void LinkWith(BlockReference* successorRef);

	// Unlinks this block from its successors, 
    // and remove the branching instruction.
	void DropLinks();

    // Removes the block from the parent function, optionally freeing it.
    void RemoveFromFunction(bool free = false);

	// Returns the first instruction in the block 
    // that has a type different than 'phi'.
	Instruction* FirstNonPhiInstruction();

    // Returns 'true' if the block starts with at least
    // one 'phi' instruction.
    bool HasPhi() const;

    // Returns the number of 'phi' instructions the block contains.
    int PhiInstructionCount() const;

	// Returns 'true' if the specified instruction is defined in this block.
	bool IsDefinedInBlock(Instruction* instr) const {
		return instr->ParentBlock() == this;
	}

	// Returns 'true' if the specified operand is defined in this block.
	bool IsDefinedInBlock(Operand* op) const {
		if(auto definingInstr = op->DefiningInstruction()) {
			return definingInstr->ParentBlock() == this;
		}

		return false; // Constants and references.
	}

	// Returns an enumerator for the successor blocks.
	SE GetSuccessorEnum() {
		return SE(LastInstruction());
	}

	const SE GetSuccessorEnum() const {
		return SE(LastInstruction());
	}

	// Calls the specified predicate for each successor.
	// bool Predicate(Block* block, int index)
	template <class Predicate>
	void ForEachSuccessor(Predicate action) {
		SE successorEnum = GetSuccessorEnum();
		int index = 0;

		while(successorEnum.IsValid()) {
			if(action(successorEnum.Next(), index++) == false) {
				return; // The user aborted.
			}
		}
	}

    template <class Predicate>
	void ForEachSuccessor(Predicate action) const {
		SE successorEnum = GetSuccessorEnum();
		int index = 0;

		while(successorEnum.IsValid()) {
			if(action(successorEnum.Next(), index++) == false) {
				return; // The user aborted.
			}
		}
	}

	// Splits the this block at the specified instruction,
    // resulting a second block that contains all the instructions
    // starting with it. The blocks are connected using a 'goto'.
	Block* SplitAt(Instruction* instr, const string& newBlockName);

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<Block> {
		static bool Is(const Symbol* symbol) {
			return (Symbol::Kind)symbol->kind_ == Symbol::Kind::Block;
		}

		static Block* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<Block*>(symbol) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif