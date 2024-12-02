// DeadCodeElimination.hpp
// Copyright (c) Lup Gratian
//
// Implements an aggressive dead-code elimination algorithm
// that presumes that all instructions are dead, keeping only
// the ones that can be proven to be required/essential.
// It is also able to eliminate dead blocks using control-dependence information.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_DEAD_CODE_ELIMINATION_HPP
#define PC_OPTIMIZATION_DEAD_CODE_ELIMINATION_HPP

#include "BlockUtils.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
#include "../Analysis/ControlDependenceGraph.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/StdLibRecognizer.hpp"
#include "../Analysis/IntArithmetic.hpp"
#include "../Analysis/TypeInfo.hpp"
#include "../Analysis/GlobalUsersTag.hpp"
#include "../Analysis/OperandInfo.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Log.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class DeadCodeElimination : public Pass {
private:
    typedef List<Instruction*> InstructionList;
    typedef List<Temporary*> TemporaryList;
    typedef StaticList<VariableReference*, 2> VariableList;
    typedef unsigned __int64 Mask;
    typedef IntArithmetic IA;
    typedef Analysis::TypeInfo TI;
    typedef DeadCodeElimination DCE;
    typedef Mask (DCE::*BitEstimator)(Operand* a, Operand* b);
    typedef Dictionary<VariableReference*, InstructionList> PotentiallyDeadDict;
    typedef Dictionary<Instruction*, bool> ProcessedInstructionsDict;
    typedef Dictionary<VariableReference*, bool> VariableReferenceDict;
    typedef Dictionary<CallInstr*, bool> DeadCallDict;

    Function* funct_;
    List<Instruction*> worklist_;
    ProcessedInstructionsDict processedInstrs_;
    PotentiallyDeadDict potentiallyDeadStoresCalls_;
    VariableReferenceDict deadCandidateVariables_;
    VariableReferenceDict loadedVariables_;
    DeadCallDict potentiallyDeadCalls_;
    InstructionList definitelyDeadStores_;
    ControlDependenceGraph dependenceGraph_;
    SparseBitVector activeBlocks_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Executes the algorithm that determines the required instructions,
    // then deletes the ones that are not required.
    void ProcessInstructions();

    // Deletes the instructions that have been determined to not be required.
    void DeleteDeadInstructions();

    // Replaces any call argument that is a dead (to be removed)
    // instruction with the undefined constants. This may happen when
    // the called function doesn't use the parameter at all.
    void ReplaceDeadArguments(Instruction* instr);

    // Returns an undefined constant having the type of the specified operand.
    UndefinedConstant* GetUndefinedConstant(Operand* op);

    // Deletes a branch which is dead. This happens when none of it's
    // successors is required. The branch is replaced with a 'goto'
    // to the immediate post-dominator.
    void DeleteDeadBranch(Instruction* instr);

    // Returns the immediate post-dominator of the specified block.
    Block* GetImmediatePostDominator(Block* block);

    // Returns 'true' if the specified instruction has a side-effect
    // that makes it essential for the correct execution of the function.
    bool IsEssentialInstruction(Instruction* instr);

    // Returns 'true' if the specified 'call' instruction is required
    // because it may affect the external state of the application.
    // Calls to some intrinsics are known to not be required.
    bool IsEssentialCall(CallInstr* instr);

    // Returns 'true' if the specified call has pointers
    // that are written into and thus cannot be eliminated.
    bool WritesIntoPointerParameters(Function* function, CallInstr* instr);

    // Returns 'true' if the specified 'store' instruction is required.
    // A store into a non-address-taken local variable is presumed 
    // to not be required unless a load from that variable is found.
    bool IsEssentialStore(StoreInstr* instr);

    // Returns 'true' if the store to the specified global variable
    // cannot be eliminated because the position might be read.
    bool IsEssentialGlobalStore(StoreInstr* instr, VariableList& globalVariables);

    // Returns 'true' if the operand is a local variable
    // that might not be used at all in the function.
    bool IsPotentiallyUnusedVariable(Operand* op, Instruction* instr);

    // Returns 'true' if the specified operand, or any of the 
    // operands derived from it might be used in an unsafe way.
    bool HasUnsafeUses(VariableReference* variableRef, Function* function);

    bool MarkUnsafeUse(VariableReference* variableRef, bool hasUnsafeUse) {
        deadCandidateVariables_.Add(variableRef, hasUnsafeUse);
        return hasUnsafeUse;
    }

    // Returns 'true' if the specified operand is used in a way
    // that makes it impossible to know when and where it is written.
    bool IsUnsafeUse(Operand* op, Instruction* instr, TemporaryList& worklist);

    // Returns 'true' if the specified operand may escape
    // in the called function if passed as a parameter.
    bool EscapesInCalledFunctions(Operand* op, CallInstr* callInstr);

    bool IsAddressingInstruction(Operand* op) {
        if(auto definingInstr = op->DefiningInstruction()) {
            return definingInstr->IsAddressing();
        }
        else return false;
    }

    // All stores and calls that target the specified variable
    // are marked as being required, because a load was found.
    void MarkStoresCallsAlive(VariableReference* baseVariable);

    // Mark the fact that there is at least on 'load' that targets
    // the specified variable (it means that a 'store' that targets
    // the same variable is required).
    void MarkLoadFromVariable(VariableReference* baseVariable);

    // Add the specified block to the list of blocks that are required.
    // This also adds to the worklist all branching instructions
    // from the blocks that control its execution.
    void MarkBlockActive(Block* block);

    // Return the local variable that acts as the base for a series
    // of addressing instruction, or 'nullptr' if it's not the case.
    VariableReference* GetBaseVariable(Operand* op);

    // Tries to find the variables that act as the base
    // of a series of addressing instructions; it also looks through
    // 'phi' and 'quest' instructions, returning 'true' if all
    // incoming values are based on variables.
    bool GetBaseVariables(Operand* op, VariableList& list, int level = 0);

    // Returns 'true' if there is a 'load' instruction that
    // has the specified variable as a source.
    bool HasLoadFromVariable(VariableReference* variableRef) {
        DebugValidator::IsNotNull(variableRef);
        return loadedVariables_.ContainsKey(variableRef);
    }

    // Adds the 'store' instruction associated with the specified
    // base variable to the list of stores that might be dead.
    void AddPotentiallyDeadStoreCall(VariableReference* baseVariable,
                                     Instruction* instr);

    // Adds to the worklist all the instructions that are required
    // (have a externally-visible side-effect).
    void CollectEssentialInstructions();

    // Adds to the worklist all instruction operands, because they
    // are required. If the instruction is a 'phi' the blocks
    // corresponding to the incoming values are activated.
    void AddOperandsToWorklist(Instruction* instr);

    // Adds to the worklist all incoming 'phi' operands
    // that are instructions, and activates the corresponding block.
    void AddPhiOperandsToWorklist(PhiInstr* instr);

    // Adds to the worklist all arguments that are definitely read
    // or written by the called function.
    void AddCallArgumentsToWorklist(CallInstr* instr);

    // Adds the specified operand to the worklist.
    void AddOperandToWorklist(Operand* op);

    // Adds the specified instruction to the worklist.
    // It ensures that an instruction is processed a single time.
    void AddToWorklist(Instruction* instr);

    // Returns 'true' if the loaded value is used only by a 'store'
    // to the exactly same location.
    bool IsUsedByStoreToSameLocation(LoadInstr* instr);

    // Adds to the worklist any argument that was presumed dead.
    void AddCallPresumedDeadToWorklist(CallInstr* instr);

    // Extracts an instruction from the worklist,
    // or returns 'nullptr' if no the worklist is empty.
    Instruction* RemoveFromWorklist();

    // Returns 'true' if the specified instruction has already been handled.
    bool WasInstructionProcessed(Instruction* instr) {
        return processedInstrs_.ContainsKey(instr);
    }

    // Estimates the bits that are discarded by the specified instruction.
    // For example, 'a >> 8' discards the lowest 8 bits.
    Mask EstimateDeadBits(Instruction* instr);

    // Tries to select the left or right operands of the specified
    // instruction based on the bits that remain alive after
    // some bits will be killed by the user.
    Operand* SelectOperand(Instruction* instr, Mask deadBits,
                           BitEstimator estimator);

    // Methods that estimate the bits that change when
    // an operation is applied between the operands.
    // or
    Mask EstimateSetBits(Operand* a, Operand* b);

    // and
    Mask EstimateResetedBits(Operand* a, Operand* b);

    // xor
    Mask EstimateInvertedBits(Operand* a, Operand* b);

    // add
    Mask EstimateAddedBits(Operand* a, Operand* b);

    // If the instruction performs an operation that affects
    // only bits that are killed by the user we can use directly
    // one of the operands. Returns the selected operand,
    // or 'nullptr' if the more bits than the killed ones are affected.
    Operand* SkipDeadInstruction(Instruction* instr, Mask deadBits);

    __int64 OperandBits(Operand* op) {
        return TI::GetSizeBits(op->GetType());
    }

    void MarkPotentiallyDeadCall(CallInstr* instr, 
                                 VariableReference* variableRef = nullptr) {
        potentiallyDeadCalls_.Add(instr, true);

        if(variableRef) {
            AddPotentiallyDeadStoreCall(variableRef, instr);
        }
    }

    bool IsPotentiallyDeadCall(CallInstr* instr) {
        return potentiallyDeadCalls_.ContainsKey(instr);
    }

    bool IsDefinitelyDeadStore(Instruction* instr) {
        return definitelyDeadStores_.Contains(instr);
    }

    void MarkDefinitelyDeadStore(Instruction* instr) {
        definitelyDeadStores_.Add(instr);
    }

    void InstructionEliminated(Instruction* instr);

public:
    DeadCodeElimination(Function* function) : 
            funct_(function), dependenceGraph_(function) {}

    void Execute();
};

} // namespace Optimization
#endif