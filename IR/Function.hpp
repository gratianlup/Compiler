// Function.hpp
// Copyright (c) Lup Gratian
//
// Defines the function class, one of the most important
// concepts of the IR. It consists of a series of basic-blocks,
// local variables and parameter variables.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_FUNCTION_HPP
#define PC_IR_FUNCTION_HPP

#include "Symbol.hpp"
#include "Variable.hpp"
#include "Tagged.hpp"
#include "IRTypes.hpp"
#include "SymbolTable.hpp"
#include "Instruction.hpp"
#include "Block.hpp"
#include "Parameter.hpp"
#include "IntrusiveList.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class Unit;
class CallInstr;
class FunctionReference;


// The calling conventions that can be used to call a function.
enum class CallConventionType {
	Auto,
	Cdecl,
	Stdcall,
	Fastcall
};


// Represents hints regarding inline expansion.
enum class InlineType {
	Auto,
	Always,
	Never
};


// Allows iterating over all instructions in a function.
class InstructionEnumerator {
private:
	mutable const Block* block_;       // Current block;
	mutable const Instruction* instr_; // Current instruction.
	mutable bool valid_;

public:
	InstructionEnumerator(const Block* block);

	InstructionEnumerator(const InstructionEnumerator& other);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the current instruction and advances the pointer.
	// If the last instruction has been reached it returns 'nullptr'.
	const Instruction* Next() const;

	Instruction* Next() {
		auto result = static_cast<const InstructionEnumerator*>(this)->Next();
		return const_cast<Instruction*>(result);
	}

	// Returns 'true' if 'Next' will return a valid instruction.
	bool IsValid() const {
		return valid_;
	}

	InstructionEnumerator& operator= (const InstructionEnumerator& other);
};


// Represents a function definition. Composed from a series of basic blocks,
// each containing at least one instruction. Has a symbol table with the 
// local variables and the parameters.
class Function : public Symbol, protected IntrusiveList<Block>,
                 public Tagged<FunctionTag> {
public:
	typedef StaticList<Variable*, 8> VariableList;
    typedef Dictionary<int, CallInstr*> CallDictionary;
    typedef List<Instruction*> InstructionList;

private:
	Unit* parent_;              // The unit to which the function belongs.
	VariableList params_;       // The list of parameters.
	VariableList variables_;    // The list of local variables.
	SymbolTable symbolTable_;   // Stores all local symbols (variables + blocks).
	ParameterTable paramTable_; // The table with the parameter operands.
    CallDictionary idToCall_;   // Maps an Id to the associated 'call' instruction.
	unsigned nextSymbolId_;     // The ID that should be assigned to the next symbol.

	unsigned char callConvention_ : 3;
	unsigned char inline_         : 3;
	unsigned char isDefinition_   : 1;
	unsigned char isIntrinsic_    : 1;
	unsigned char isStdlib_       : 1;
	unsigned char isNoIndirWrite_ : 1;
	unsigned char isNoIndirRead_  : 1;
    unsigned char isNoState_      : 1;

protected:
	Function(const FunctionType* type, shared<string> name, 
             bool isDef, Unit* parent,SymbolVisibility visibility, 
             CallConventionType callConv, bool isIntr = false);

	virtual string ToStringImpl(int level) const override;

    // Method that should notify the associated tags
    // when a new block is added to the function.
    virtual void NotifyBlockAdded(Block* block);

    // Method that should notify the associated tags
    // when a block has been removed from the function.
    virtual void NotifyBlockRemoved(Block* block);

    // Method that should notify the associated tags
    // when a new parameter is added to the function.
    virtual void NotifyParameterAdded(Variable* parameter);

    // Method that should notify the associated tags
    // when a parameter has been removed from the function.
    virtual void NotifyParameterRemoved(Variable* parameter);

    // Method that should notify the associated tags
    // when a new variable is added to the function.
    virtual void NotifyVariableAdded(Variable* variable);

    // Method that should notify the associated tags
    // when a variable has been removed from the function.
    virtual void NotifyVariableRemoved(Variable* variable);

public:
	typedef InstructionEnumerator IE;
	typedef IntrusiveList<Block> IL;

	// Factory methods for creating function instances.
	static Function* GetFunction(const FunctionType* type, const string& name, 
								 bool isDef = true, Unit* parent = nullptr,
								 SymbolVisibility visibility = SymbolVisibility::Extern,
								 CallConventionType callConv = CallConventionType::Auto);
	virtual ~Function() {
		ClearBlocks(true /* free */, true /* fromDestructor */);
	}
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns a reference that points to the function.
    FunctionReference* GetReference();

	// Returns the ID that should be assigned to the next inserted block.
	unsigned GetNextBlockId() {
		return ++nextSymbolId_;
	}

	// Returns the ID that should be assigned to the next inserted variable.
	unsigned GetNextVariableId() {
		return ++nextSymbolId_;
	}

    unsigned PeekNextVariableId() const {
        return nextSymbolId_ + 1;
    }

    // Returns the 'call' instruction having the specified Id.
    CallInstr* GetCallInstruction(int id) {
        return idToCall_[id];
    }
    
    const CallInstr* GetCallInstruction(int id) const {
        return idToCall_[id];
    }

	// Returns the number of 'call' instructions found in the function.
	int CallInstructionCount() const {
		return idToCall_.Count();
	}

    // Adds the specified 'call' instruction to the map.
    void AddCallInstruction(CallInstr* instr);

    // Removes the 'call' instruction having the specified Id from the map.
    void RemoveCallInstruction(int id);

    // Removes the specified 'call' instruction from the map.
    void RemoveCallInstruction(CallInstr* instr);

    // Returns the type of calling convention to be used when calling this function.
    CallConventionType CallConvention() const {
		return (CallConventionType)callConvention_;
    }

	void SetCallConvention(CallConventionType value) {
		callConvention_ = (unsigned char)value;
	}

	// Returns the type of inline expansion that should be performed.
	InlineType Inline() const {
		return (InlineType)inline_;
	}

	void SetInline(InlineType value) {
		inline_ = (unsigned char)value;
	}

	// Returns 'true' if this is a definition for a function.
	bool IsDefinition() const {
		return isDefinition_;
	}

	void SetIsDefinition(bool value) {
		isDefinition_ = value;
	}

    // Returns 'true' if this is a declaration of a function.
    bool IsDeclaration() const {
        return isDefinition_ == 0;
    }

	// Returns 'true' if the function is a intrinsic (built-in function).
	bool IsIntrinsic() const {
		return isIntrinsic_;
	}

	// Returns 'true' if the function is part of the standard library.
	// Can be used to perform more aggressive analyses and optimizations.
	bool IsFromStdlib() const {
		return isStdlib_;
	}

	void SetIsFromStdlib(bool value) {
		isStdlib_ = value;
	}

	// Returns 'true' if the function is guaranteed to not write 
	// to any memory location loaded from a pointer.
	bool IsNoIndirectWrite() const {
		return isNoIndirWrite_;
	}

    bool IsIndirectWrite() const {
		return isNoIndirWrite_ == 0;
	}

	void SetIsNoIndirectWrite(bool value) {
		isNoIndirWrite_ = value;
	}

	// Returns 'true' if the function is guaranteed to not read
	// to any memory location loaded from a pointer.
	bool IsNoIndirectRead() const {
		return isNoIndirRead_;
	}

    bool IsIndirectRead() const {
		return isNoIndirRead_ == 0;
	}

	void SetIsNoIndirectRead(bool value) {
		isNoIndirRead_ = value;
	}

    // Returns 'true' if the function is guaranteed not to change its return value
    // based on the state of global variables and values read from pointer parameters.
    // All math functions from the standard library are marked with this attribute.
    // Note that such a function may take part in common-subexpression elimination.
    bool IsNoState() const {
		return isNoState_;
	}

    bool IsState() const {
		return isNoState_ == 0;
	}

	void SetIsNoState(bool value) {
		isNoState_ = value;
	}

	// Returns the table of symbols, used to keep track of local variables and blocks.
	SymbolTable& Symbols() {
		return symbolTable_;
	}

	const SymbolTable& Symbols() const {
		return symbolTable_;
	}

	// Returns the unit to which this function belongs.
	Unit* ParentUnit() {
		return parent_;
	}

	const Unit* ParentUnit() const {
		return parent_;
	}

	void SetParentUnit(Unit* value) {
		parent_ = value;
	}

	// Returns an enumerator that can be used to enumerate
    // all instructions in the function (from all basic blocks).
	IE GetInstructionEnum() {
		return IE(FirstBlock());
	}

	const IE GetInstructionEnum() const {
		return IE(FirstBlock());
	}

    // Executes the specified predicate for each instruction in the function.
    // bool Predicate(Instruction* instr)
    template <class Predicate>
    void ForEachInstruction(Predicate action) {
        auto instrEnum = GetInstructionEnum();

        while(instrEnum.IsValid()) {
            auto instr = instrEnum.Next();

            if(action(instr) == false) {
                return; // The user aborted.
            }
        }
    }

    // Executes the specified predicate for each instruction
    // in the function that matches the specified type.
    // bool Predicate(Instruction* instr)
    template <class T, class Predicate>
    void ForEachInstructionOfType(Predicate action) {
        auto instrEnum = GetInstructionEnum();

        while(instrEnum.IsValid()) {
            auto instr = instrEnum.Next();

            if(auto matchingInstr = instr->As<T>()) {
                if(action(matchingInstr) == false) {
                    return; // The user aborted.
                }
            }
        }
    }

    // Executes the specified predicate for each instruction
    // that has uses at least once the operand.
    // bool Predicate(Instruction* instr)
    template <class Predicate>
    void ForEachInstructionUsingOperand(Operand* usedOp, Predicate action) {
        auto instrEnum = GetInstructionEnum();

        while(instrEnum.IsValid()) {
            auto instr = instrEnum.Next();

            if(instr->HasSourceOp(usedOp)) {
                if(action(instr) == false) {
                    return; // The user aborted.
                }
            }
        }
    }

    // Removes all instructions matching the specified predicate.
    // If 'removedInstrs' is specified the removed instructions are added
    // to the list and not deleted, otherwise they are deleted.
    // bool Predicate(Instruction* instr);
    template <class Predicate>
    int RemoveInstructionsMatching(Predicate action, InstructionList* removedInstrs = nullptr) {
        int removedCount = 0;

        ForEachBlock([&](Block* block) -> bool {
            removedCount += block->RemoveInstructionsMatching(action, removedInstrs);
            return true;
        });

        return removedCount;
    }

    // Returns 'true' if any instruction matches the specified predicate.
    // bool Predicate(Instruction* instr) const;
    template <class Predicate>
    bool HasInstructionMatching(Predicate action) const {
        bool found = false;

        ForEachBlock([&](Block* block) -> bool {
            found = block->HasInstructionMatching(action);
            return found == false;
        });

        return found;
    }

    // Returns 'true' if the function contains instructions of the specified type.
    // bool Predicate(Instruction* instr) const;
    template <class T>
    bool HasInstructionOfType() const {
        bool found = false;

        ForEachBlock([&](Block* block) -> bool {
            found = block->HasInstructionOfType<T>(action);
            return found == false;
        });

        return found;
    }

    // Returns 'true' if any instruction uses the specified operand.
    bool HasInstructionUsingOperand(Operand* usedOp) {
        bool found = false;

        ForEachInstructionUsingOperand(usedOp, [&](Instruction* instr) -> bool {
            found = true;
            return false;
        });

        return found;
    }

	// Returns the associated function type.
	const FunctionType* GetType() const {
		return static_cast<const FunctionType*>(type_);
	}

	// Returns the type of the returned value. 
	// This is 'VoidType' if the function has no return value.
	const Type* ReturnType() const {
		return GetType()->ReturnType();
	}

    // Adds the specified parameter to the function.
    // The parameter is assigned an unique ID.
    void AddParameter(Variable* parameter);
    
    // Returns the list of the symbols that act like
    // the parameters of the function.
	const VariableList& Parameters() const {
		return params_;
	}

	// Returns the list with the type of the parameters.
	const List<const Type*>& ParameterTypes() const {
		return GetType()->Parameters();
	}

	// Returns the number of parameters.
	int ParameterCount() const {
		return params_.Count();
	}

	// Returns the parameter operand associated
    // with the specified parameter variable.
	Parameter* GetParameter(Variable* parameter);

	// Returns the parameter operand associated with the parameter
	// found at the specified position.
	Parameter* GetParameter(int index);

    // Returns 'true' if a variable with the specified name is found.
	bool HasParameter(const string* name) const;

	// Returns the parameter with the specified name.
	Variable* GetParameterVariable(const string* name);
	const Variable* GetParameterVariable(const string* name) const;

	// Returns the parameter with the specified ID, or 'nullptr'
    // if such a parameter is not found. Note that this performs a linear search.
	Variable* GetParameterVariableWithId(int id);
	const Variable* GetParameterVariableWithId(int id) const;

    // Returns the parameter with the specified index.
    Variable* GetParameterVariable(int index) {
        return params_[index];
    }

    const Variable* GetParameterVariable(int index) const {
        return params_[index];
    }

    // Returns the index corresponding to the specified parameter.
    int GetParameterIndex(Variable* parameter);

    // Removes the specified parameter variable from the function,
    // optionally deleting it.
	void RemoveParameter(Variable* variable, bool free = true);
	void RemoveParameter(const string* name, bool free = true);
	void RemoveParameterWithId(int id, bool free = true);

    void RemoveParameter(int index, bool free = true) {
        RemoveParameter(params_[index], free);
    }

	// Returns 'true' if the function can have
    // a variable number of parameters.
	bool IsVarargs() const {
		return GetType()->IsVarargs();
	}

    // Adds the specified parameter to the function.
    // The parameter is assigned an unique ID.
    void AddVariable(Variable* variable);

	// Returns the list of local variables.
	VariableList& Variables() {
		return variables_;
	}

	const VariableList& Variables() const {
		return variables_;
	}

	// Returns the number of local variables.
	int VariableCount() const {
		return variables_.Count();
	}

	// Returns 'true' if a variable with the specified name is found.
	bool HasVariable(const string* name) const;

	// Returns the variable with the specified name.
	Variable* GetVariable(const string* name);
	const Variable* GetVariable(const string* name) const;

    // Returns the variable found at the specified index.
    Variable* GetVariable(int index) {
       return variables_[index];
    }
    	
    const Variable* GetVariable(int index) const {
       return variables_[index];
    }

	// Returns the variable with the specified ID, or 'nullptr'
    // if such a variable is not found. Note that this performs a linear search.
	Variable* GetVariableWithId(int id);
	const Variable* GetVariableWithId(int id) const;

	// Removes the specified variable from the function, 
    // optionally deleting it.
	void RemoveVariable(Variable* variable, bool free = true);
	void RemoveVariable(const string* name, bool free = true);
	void RemoveVariable(int id, bool free = true);

    // Performs the specified action on each parameter variable.
    // bool Predicate(Variable* parameterVariable, int parameterIndex)
    template <class Predicate>
    void ForEachParameterVariable(Predicate action) {
        for(int i = 0; i < params_.Count(); i++) {
            if(action(params_[i], i) == false) {
                return;
            }
        }
    }

    // Performs the specified action on each parameter variable
    // having the specified type (int32, int8*, etc.).
    // bool Predicate(Variable* parameterVariable, int parameterIndex)
    template <class T, class Predicate>
    void ForEachParameterVariableOfType(Predicate action) {
        for(int i = 0; i < params_.Count(); i++) {
            auto parameter = params_[i];

            if(parameter->GetType()->Is<T>()) {
                if(action(parameter, i) == false) {
                    return;
                }
            }
        }
    }

    // Performs the specified action on each local variable.
    // bool Predicate(Variable* variable, int variableIndex)
    template <class Predicate>
    void ForEachVariable(Predicate action) {
        for(int i = 0; i < variables_.Count(); i++) {
            if(action(variables_[i], i) == false) {
                return;
            }
        }
    }

    // Performs the specified action on each parameter variable
    // having the specified type (int32, int8*, etc.).
    // bool Predicate(Variable* parameterVariable, int variableIndex)
    template <class T, class Predicate>
    void ForEachVariableOfType(Predicate action) {
        for(int i = 0; i < variables_.Count(); i++) {
            auto variable = variables_[i];

            if(variable->GetType()->Is<T>()) {
                if(action(variable, i) == false) {
                    return;
                }
            }
        }
    }

	// Returns 'true' if the function has no return value.
	bool IsVoid() const {
		DebugValidator::IsNotNull(GetType());
		return GetType()->IsVoid();
	}

	// Methods for manipulating the list of blocks.
	Block* FirstBlock() { 
        return static_cast<Block*>(IL::First()); 
    }

	const Block* FirstBlock() const { 
        return static_cast<const Block*>(IL::First()); 
    }

	Block* LastBlock() {
        return static_cast<Block*>(IL::Last()); 
    }

	const Block* LastBlock() const { 
        return static_cast<const Block*>(IL::Last()); 
    }

	void InsertFirstBlock(Block* value);
	void InsertLastBlock(Block* value);
	void InsertBlock(Block* value) { InsertLastBlock(value); }
	void InsertBlockBefore(Block* value, Block* other);
	void InsertBlockAfter(Block* value, Block* other);
	void RemoveBlock(Block* value, bool free = false, 
                     bool fromDestructor = false);

    // Returns 'true' if the function contains the specified block.
    // Used mostly for debugging purposes.
    bool ContainsBlock(Block* block) const;

	// Removes all blocks from the function, optionally freeing them.
	void ClearBlocks(bool free = false, bool fromDestructor = false);

	// Returns the number of blocks in the function.
	int BlockCount() const { return IL::Count(); }

	// Returns the block with the specified ID, or 'nullptr' if such a block
	// could not be found. Note that a linear search is performed.
	Block* GetBlock(int id);

	const Block* GetBlock(int id) const;

    // Calls the specified predicate for each block in the function.
	// bool Predicate(Block* block)
	template <class Predicate>
    void ForEachBlock(Predicate action) {
    	Block* block = FirstBlock();
		
           while(block) {
    		if(action(block) == false) {
    			return; // The user aborted.
			}
    
    		block = block->NextBlock();
    	}
    }

    // Returns the total number of instructions the function contains.
    int InstructionCount() const;

	// Returns a string representation of all basic blocks.
	string DumpBlocks(int level = 0) const;

	// Returns a string representation of all declared variables,
	// optionally including the function parameters
	string DumpVariables(bool showParams = true, int level = 0) const;

	// Launches an application that shows a graphic representation of the CFG.
	// If 'immDoms' is 'true' the immediate dominator of each block is marked.
	// If 'frontierBlock' is provided, the blocks on its dominance frontier are marked.
	void ViewCFG(bool markUnreachable = true, bool immDoms = false, 
                 Block* frontierBlock = nullptr) const;

	// Launches an application that shows a graphic
    // representation of the dominator tree.
	void ViewDominatorTree() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<Function> {
		static bool Is(const Symbol* symbol) {
			return (Symbol::Kind)symbol->kind_ == Symbol::Kind::Function;
		}

		static Function* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<Function*>(symbol) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif