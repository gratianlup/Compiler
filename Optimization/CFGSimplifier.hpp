// CFGSimplifier.hpp
// Copyright (c) Lup Gratian
//
// Identifies common CFG patterns and tries to simplify them.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_CFG_SIMPLIFIER_HPP
#define PC_OPTIMIZATION_CFG_SIMPLIFIER_HPP

#include "BlockUtils.hpp"
#include "BlockCloner.hpp"
#include "../IR/Block.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Analysis/CFGWalker.hpp"
#include "../Analysis/IRDominators.hpp"
#include "../Analysis/ConstantFolder.hpp"
#include "../Analysis/IntArithmetic.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/ValueNumber.hpp"
#include "../Analysis/ConstantEstimator.hpp"
#include "../Analysis/ProfileAdjustment.hpp"
#include "../Analysis/SSAReconstruction.hpp"
#include "../Compilation Pass/Pass.hpp"
#include "../Base/Log.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/MakePair.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class CFGSimplifier : public Pass {
private:
    struct BlockHash {
        Block* HashedBlock;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        BlockHash() {}

        BlockHash(Block* block) : HashedBlock(block) {}

        BlockHash(const BlockHash& other) : HashedBlock(other.HashedBlock) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        unsigned GetHashCode() const;

        bool operator== (const BlockHash& other) const;

        bool operator< (const BlockHash& other) const {
            return false;
        }
    };

    enum class TrueFalseResult {
        AlwaysTrue,
        AlwaysFalse,
        Unknown
    };

	// Create a pair that represents a range of switch case values.
    MAKE_PAIR(CaseValueRange, __int64, First, __int64, Last);
	MAKE_PAIR(BlockPair, Block*, Predecessor, int, SuccessorIndex);
    MAKE_PAIR(SuccessorPair, Block*, Successor, Operand*, ReturnedOperand);
    MAKE_PAIR(ControllingBlockPair, Block*, ControllingBlock, bool, ControlledOnTruePath);

    typedef StaticList<Block*, 128> WorkList;
	typedef StaticList<int, 2> BlockIndexList;
    typedef StaticList<__int64, 16> CmpValuesList;
    typedef Dictionary<__int64, bool> CmpValuesDict;
    typedef StaticList<Instruction*, 32> InstructionWorklist;
    typedef StaticList<Operand*, 4> OperandList;
    typedef StaticList<Block*, 8> BlockList;
    typedef StaticList<ControllingBlockPair, 2> ControllingBlockList;
    typedef StaticList<PhiInstr*, 4> PhiInstructionList;
    typedef StaticList<Constant*, 16> ConstantList;
    typedef StaticList<ConstantList, 8> SwitchConstantList;
    typedef StaticList<Instruction*, 8> InstructionList;
	typedef StaticList<BlockPair, 2> CloneBlocksList;
	typedef Dictionary<IntConstant*, bool> ConstantDictionary;
	typedef StaticList<IntConstant*, 8> IntConstantList;
	typedef Dictionary<Operand*, Operand*> OperandMap;
    typedef StaticList<SuccessorPair, 2> SuccessorBlockList;
    typedef IntArithmetic IA;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    //! TODO: THESE SHOULD BE CONTROLS
    static const int MAX_HOISTING_SEARCH_DEPTH       = 5;
    static const int MAX_HOISTING_QUESTIONS          = 2;
	static const int MAX_CLONED_BLOCK_SIZE           = 4;
    static const int MAX_AND_OPERANDS                = 8;
    static const int MIN_PHI_INCOMING_OPERANDS       = 1;
    static const int MAX_PHI_INCOMING_OPERANDS       = 8;
	static const int MAX_CONTROLLING_BLOCK_LEVEL     = 4;
    static const int MAX_CONTROLLING_BLOCK_COUNT     = 2;
	static const int MAX_THREADING_SUCCESORS         = 8;
    static const int MAX_OPERAND_THREADING_LEVEL     = 3;
    static const int MAX_RETURN_BLOCK_EVALUATE_COUNT = 6;
    static const bool USE_CORRELATION_TEST           = true;

    //! TODO: THESE SHOULD BE CONTROLS
    // Enabled optimizations.
    static const bool CONVERT_OR_CHAIN_TO_SWITCH           = true;
    static const bool THREAD_PHI_BLOCKS_TO_DESTINATION     = true;
    static const bool CONVERT_SWITCH_TO_RANGE_TEST         = true;
    static const bool CONVERT_SWITCH_ON_QUESTION           = true;
    static const bool HOIST_COMMON_INSTRUCTIONS            = true;
    static const bool MERGE_NESTED_SWITCH                  = true;
    static const bool CONVERT_SWITCH_TO_LOAD_FROM_GLOBAL   = true;
    static const bool CONVERT_IF_CHAIN_TO_SWITCH           = true;
    static const bool SIMPLIFY_JUMP_TO_RETURN              = true;
    static const bool SIMPLIFY_JUMP_TO_RETURN_WITH_CLONING = true;
    static const bool THREAD_BLOCKS_TO_DESTINATION         = true;
	static const bool THREAD_OVER_SAME_CONDITION_BLOCK     = true;

    ConstantFolder folder_;
    IntProfileAdjustment contEvalLimitAdjustment_;
    SparseBitVector clonedReturnBlocks_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Returns the number of instructions that should be scanned
    // when searching for hoisting candidates in the successor blocks.
    int GetHoistingSearchDepth() const {
        //! TODO: THIS SHOULD TAKE INTO CONSIDERATION THE OPTIMIZATION LEVEL.
        // (less, or none, if low opt level).
        return MAX_HOISTING_SEARCH_DEPTH;
    }

    // Returns 'true' if a more complex test should be used
    // to test if a condition holds if another condition holds too.
    bool UseCorrelationTest() const {
        return USE_CORRELATION_TEST;
    }

    void AddSuccessorsToWorklist(Block* block, WorkList& worklist, 
                                 SparseBitVector& inWorklist);

    // Tries to simplify the specified block using one of the
    // available methods. Returns 'true' if the block could be simplified.
    bool SimplifyBlock(Block* block);

    // Tries to convert a long chain of 'or' instructions that involve
    // only integer constant to a 'switch', which can be lowered
    // more efficiently (jump-table, for example).
    bool ConvertOrChainToSwitch(Block* block);

    // Checks if this is a valid 'or' in an or-chain (sets 'valid').
    // Adds any found comparison instruction to the worklist,
    void HandleOrInOrChain(OrInstr* orInstr, CmpValuesList& cmpValues,
                           InstructionWorklist& worklist,
                           Operand*& compareOp, bool& valid);

    // Checks if the comparison instruction is part of an or-chain 
    // (sets 'valid' and adds the 'case' values to 'addedValues').
    // If 'compareOp' is specified then the comparison must involve
    // this operand, and in all cases other operand must be an integer constant.
    void HandleCmpInOrChain(CmpInstrBase* cmpInstr, CmpValuesList& cmpValues,
                            InstructionWorklist& worklist,
                            Operand*& compareOp, bool& valid, 
                            CmpValuesDict& addedVals);

    // Returns 'true' if the instructions have definitely
    // the same meaning (handles commutative instructions too, 
    // so that 'add a, b' and 'add b, a' are considered the same). 
    bool IsSameInstruction(Instruction* instrA, Instruction* instrB);

    // This tries to hoist the common instructions from the successor blocks.
    // This can be done only if it's safe to speculate the instructions.
    bool HoistCommonInstructions(Block* block);

    // Moves the instruction to 'block' and replaces all 
    // instructions in the list with its result.
    void HoistToBlock(Instruction* instr, Block* block,
                      InstructionList& replacedInstrs);

    bool CanBeHoistedToBlock(Instruction* instr, Block* block);
	
	// Tries to hoist instructions that are very similar
	// by selecting using 'quest' between the operand that is not the same.
    bool HoistUsingQuestion(Instruction* instr, Block* block);

	// Creates the 'quest' instructions which selects
	// between the operand that is not common to the instructions.
    bool CreateHoistingQuestion(Instruction* instr, Instruction* otherInstr, 
                                Operand* commonOp, Operand* otherOpTrue, 
                                Operand* otherOpFalse, Block* block);

    void DetectCommonOperand(Instruction* instr, Instruction* otherInstr, 
                             Operand* &commonOp, Operand* &otherOpTrue, 
                             Operand* &otherOpFalse );

    // Returns the first instruction found in the specified block
    // that is identical to the specified one ('sameInstr').
    // 'maxDepth' is used to limit the number of instructions that are searched.
    Instruction* GetSameInstruction(Block* block, Instruction* sameInstr, 
                                    int maxDepth);

    // Adds the specified value to the list of 'case' values.
    void AddCaseValue(__int64 value, CmpValuesList& cmpValues,
                      CmpValuesDict& addedVals);

    // Tries to convert a 'switch' with 'case' values that are
    // in increasing order and that target the same block
    // to a range test that uses an 'if'.
    bool ConvertSwitchToRangeTest(Block* block);
    
    // Creates a range test for the specified range.
    Operand* CreateSwitchRangeTest(SwitchInstr* switchInstr,
                                   __int64 minValue,__int64 maxValue);

    // Combines the range test results using an 'or'
    // and replaces the 'switch' with an 'if' instruction.
    void ReplaceSwitchByRangeTest(SwitchInstr* switchInstr, 
                                  OperandList& rangeTestResults,
                                  Block* inRangeBlock, Block* notInRangeBlock);

    void ReplaceSwitchByRangeTest(SwitchInstr* switchInstr, 
                                  Operand* rangeTestResult,
                                  Block* inRangeBlock, Block* notInRangeBlock) {
        OperandList rangeTestResults;
        rangeTestResults.Add(rangeTestResult);
        ReplaceSwitchByRangeTest(switchInstr, rangeTestResults,
                                 inRangeBlock, notInRangeBlock);
    }

    // Tries to replace a 'switch' that has a 'quest' as the condition,
    // and the 'quest' operands as its 'case' value with an 'if'.
    bool ConvertSwitchOnQuestion(Block* block);

    // Tries to perform a limited variant of jump-threading.
    // If this block ends with an 'if' and we have predecessors
    // for which we can determine the target block, we can force them 
    // to jump directly to the target.
    bool ThreadPhiBlocksToDestination(Block* block);

    // Returns 'true' if the block is a candidate for jump-threading,
    // meaning that its instruction are used only to compute the successor.
    bool IsThreadingCandidate(Block* block, PhiInstr*& phiInstr,
                              CmpInstrBase*& cmpInstr, IfInstr*& ifInstr);

    // Returns the block that will be the successor
    // if the specified constant is used as the condition operand.
    BlockReference* GetThreadingSuccessor(Operand* op, CmpInstrBase* cmpInstr,
                                          IfInstr* ifInstr);

    // Updates the 'phi' instruction in 'successor' so that
    // all values that are incoming from 'prevPred' should be
    // incoming from 'newPred' too.
    void PatchSuccessor(Block* successor, Block* prevPred, Block* newPred);

	// Removes the 'phi' operands that are incoming from 'incomingBlock'.
	void RemoveIncomingFromBlock(Block* block, Block* incomingBlock);

    // Tries to merge two 'switch' instructions, one of which
    // is nested inside the other one. This can be done only if
    // the 'case' values of the nested switch are not used by its parent.
    bool MergeNestedSwitch(Block* block);

    bool SwitchTargetsPhiBlock(SwitchInstr* switchInstr, Block* newDefaultBlock);

    // Returns 'true' if there is any incoming operand from 'fromBlock'.
    bool HasIncomingFromBlock(Block* fromBlock, Block* toBlock);

    // Any operand that is incoming from 'prevBlock' is modified
    // so that it appears that it is incoming from 'newBlock'.
    void ChangeIncomingBlock(Block* block, Block* prevBlock, Block* newBlock);

    // Tries to convert a 'switch' that only sets the value
    // of a variable to a load from a global variable that is
    // initialized with the values set by the 'switch'.
    bool ConvertSwitchToLoadFromGlobal(Block* block);

    // Adds all 'phi' instructions from 'defaultBlock' to the list.
    // Only 'phi's with integer and floating type are added.
    // Returns 'false' if there are too many 'phi' instructions.
    bool CollectPhiInstructions(Block* defaultBlock, PhiInstructionList& phiInstrs);

    // Adds to 'switchConsts' all constants that are assigned
    // to the variables in each 'case' block.
    // Returns 'false' if the 'switch' cannot be converted.
    bool CollectSwitchConstants(SwitchInstr* switchInstr, Block* defaultBlock,
                                SwitchConstantList& switchConsts, 
                                PhiInstructionList& phiInstrs, __int64& step);

    // Used during Debug builds to validate the collected constants.
    void ValidateSwitchConstants(SwitchConstantList& switchConsts,
                                 PhiInstructionList& phiInstrs);

    // Creates a variable with the values of all assigned variables.
    // We can use a single variable only if all constant have the same type.
    void CreateSameTypeGlobal(SwitchInstr* switchInstr, 
							  PhiInstructionList& phiInstrs,
                              SwitchConstantList& switchConsts, 
                              const Type* type, __int64 step,
                              Block*& loadBlock, OperandList& loadedOps);

    // Creates a global variable for each variable that is assigned
    // in the 'case' blocks. Multiple variables are used because 
    // their type is different.
    void CreateDifferentTypeGlobals(SwitchInstr* switchInstr, 
                                    PhiInstructionList& phiInstrs,
                                    SwitchConstantList& switchConsts, 
                                    __int64 step, Block*& loadBlock, 
                                    OperandList& loadedOps);

    // Creates a global variable whose name is unique.
    GlobalVariable* CreateUniqueVariable(const Type* type, Block* block, int count);

    // Creates a block whose name is unique.
    // The new block is inserted after 'block'.
    Block* CreateUniqueBlock(Block* block);

    // All the incoming operands are removed and instead
    // the operand loaded from the global variable is used.
    // Used by 'ConvertSwitchToLoadFromGlobal'.
    void PatchSwitchPhis(PhiInstructionList& phiInstrs, BlockReference* loadBlockRef,
                         OperandList& loadedOps, Block* defaultBlock,
                         Block* switchBlock);

    // Tries to convert a chain of 'if' instructions
    // to a 'switch', which can be lowered more efficiently 
    // (jump-table, for example). This can be done only if we have
    // equality comparison with an integer constant.
    bool ConvertIfChainToSwitch(Block* block);

	// Tries to collect the blocks that form the 'if' chain
	// and updates the corresponding lists.
	bool CollectIfChain(Block* startBlock, Operand* comparedOp, 
						IntConstantList& intConsts, InstructionList& ifInstrs, 
						BlockList& blocks, CloneBlocksList& cloneBlocks, 
						ConstantDictionary& available,
						Block*& falseBlock, Block*& lastBlock);

	bool IsBlockClonable(Block* block);

    // Checks if the chain of blocks can be converted to a switch.
    bool TryMakeIfChainValid(BlockList& blocks, CloneBlocksList& cloneBlocks,
                          InstructionList& ifInstrs, Block* falseBlock);

	// Clones the blocks and updates the 'blocks' list.
	void CreteBlockClones(CloneBlocksList& cloneBlocks, BlockList& blocks);

    // Returns the integer constant that is compared by the
    // instruction that is the conditional operand of the 'if'
    // that ends the specified block. If 'op' is specified 
    // the comparison should target it.
    IntConstant* GetEqualsComparisonValue(Block* block, Operand*& op);

    // Tries to eliminate a 'goto'/'if' to a block that returns
    // from the function. The possible cases are handled by the methods below.
    bool SimplifyJumpToReturn(Block* block);

    // Tries to eliminate a 'goto' to a block that returns a value,
    // possibly a 'phi' instruction result.
    bool SimplifyGotoToReturn(Block* block, GotoInstr* gotoInstr);

    // Tries to eliminate a 'goto' to a block that returns nothing.
    bool SimplifyGotoToVoidReturn(Block* block, Block* targetBlock);

    // Tries to eliminate an 'if' that goes to two return blocks.
    // See the implementation for the cases it handles.
    bool SimplifyIfToReturn(Block* block, IfInstr* ifInstr);

    bool IsValidGotoReturn(Block* gotoBlock, Block* returnBlock);

    // Tries to eliminate an 'if' that goes to two blocks
    // that contain return instructions, possible a value
    // that is the result of a 'phi' instruction.
    bool SimplifyIfToReturnReturn(Block* block, IfInstr* ifInstr,
                                  Block* trueBlock, Block* falseBlock);

    // Tries to eliminate a 'goto'/'if' to a block that returns
    // from the function by cloning and specializing it.
    bool SimplifyJumpToReturnWithCloning(Block* block);

    // Returns 'true' if cloning the block and specializing
    // the values when incoming from the specified block
    // turns most of the instructions into constants.
    bool ShouldCloneReturnBlock(Block* block, Block* incomingBlock,
                                Operand*& newReturnedOp);
    
    // Scans the block and collects all 'phi' instructions, and an optional
    // "cheap" instruction which is assigned to 'other'. 
    // If "expensive" instructions remain it returns 'false'.
    bool CollectPhisAndOther(Block* returnBlock, PhiInstructionList& phis,
                             Instruction*& other);

    // Looks for blocks that are duplicate (have the same instructions
    // with the same operands) and removes the ones that are duplicates.
    // Duplicate blocks most often consist of a single return instruction.
    void RemoveDuplicateBlocks(Function* function);

    bool CheckIfBlockIsDuplicate(Block* block);

    // Tries to merge two or more basic blocks that are guarded
    // by the same condition. if(E) f(); if(E) g(); -> if(E) { f(); g(); }
    bool ThreadToDestination(Block* block);

	Block* TryEliminateBlock(Block* block, Block* newPredecessorBlock);

	Block* TryJumpThreading(Block* threadedBlock, Block* targetBlock,
                            int targetBlockSuccessorIndex);

    Block* TryCorrelatedJumpThreading(Block* threadedBlock, Block* targetBlock,
                                      Operand* targetCondition, int targetBlockSuccessorIndex,
                                      ControllingBlockList& controllingBlocks);

    Block* TrySwitchJumpThreading(Operand* targetCondition, SwitchInstr* switchInstr,
                                  Block* threadedBlock, Block* targetBlock,
                                  int targetBlockSuccessorIndex);

    TrueFalseResult CompareWithValue(CmpInstrBase* instr, __int64 value);

    Block* TryIfJumpThreading(Operand* targetCondition, IfInstr* ifInstr,
                              Block* threadedBlock, Block* targetBlock,
                              bool controlledOnTruePath);

    Block* TryFalsePathCorrelatedJumpThreading(Operand* correlatedOp, Operand* otherOp, 
											   Block* threadedBlock, Block* targetBlock);

    Block* TryNonCorrelatedJumpThreading(Block* threadedBlock, Block* targetBlock, 
                                         Operand* targetCondition,
                                         ControllingBlockList& controllingBlocks);

    Block* TryNonCorrelatedSwitchJumpThreading(Block* threadedBlock, Block* targetBlock, 
                                               Operand* targetCondition,
                                               ControllingBlockList& controllingBlocks);

    TrueFalseResult TryNonCorrelatedJumpThreadingAnd(Block* threadedBlock, 
                                                     OperandList andOperands,
                                                     ControllingBlockList& controllingBlocks);

    TrueFalseResult TryNonCorrelatedJumpThreadingOr(Block* threadedBlock, 
                                                    OperandList orOperands,
                                                    ControllingBlockList& controllingBlocks);

    Block* SelectThreadingSuccessor(TrueFalseResult result, Block* threadedBlock,
                                    Block* targetBlock);

	bool FindThreadingMembers(Block* threadedBlock, Block* targetBlock, 
							  ControllingBlockList& controllingBlocks,
							  Operand*& targetCondition);

	// 
	Block* ThreadOverBlock(Block* threadedBlock, Block* skippedBlock, 
						 Block* targetBlock);

    Block* CloneAndLinkWithBlock(Block* block, Block* targetBlock);

	bool NeedsThreadingClone(Block* block, Block* targetBlock,
						     bool& hasNonPhiUsers);

	bool MayHaveSideEffects(Instruction* instr);

	void ReconstructSSA(Block* skippedBlock, Block* skippedBlockClone,
						Block* targetBlock);

	void ReconstructSSA(Temporary* skippedOp, Block* skippedBlock, 
						Temporary* skippedOpClone, Block* skippedBlockClone,
						Block* targetBlock);


	bool FindControllingBlock(Block* startBlock, ControllingBlockList& blocks, 
                              int level = 0);

    // Returns 'true' if the 'if' condition represented by 'opA'
    // holds if the condition represented by 'opB' holds too.
    bool IsTrueCorrelatedCondition(Operand* correlatedOp, Operand* otherOp,
                                   Block* incomingBlock, bool testAndOr = true);

    bool IsTrueCorrelatedConditionConst(IntConstant* correlatedIntConst,
                                        IntConstant* otherIntConst,
                                        CmpInstrBase* correlatedCmpInstr, 
                                        CmpInstrBase* otherCmpInstr);

    bool IsTrueCorrelatedConditionInt(CmpInstrBase* correlatedCmpInstr,
                                      CmpInstrBase* otherCmpInstr);

    bool IsTrueCorrelatedConditionAndOr(Operand* correlatedOp, Operand* otherOp,
                                        Block* incomingBlock);

    bool IsTrueCorrelatedConditionAndAnd(OperandList& correlatedAndOperands,
                                         OperandList& otherAndOperands,
                                         Block* incomingBlock);

    bool IsTrueCorrelatedConditionOrAnd(OperandList& correlatedOrOperands,
                                        OperandList& otherAndOperands,
                                        Block* incomingBlock);

    bool IsTrueCorrelatedConditionOrOr(OperandList& correlatedOrOperands,
                                       OperandList& otherOrOperands,
                                       Block* incomingBlock);

    TrueFalseResult IsAlwaysTrueOrFalse(CmpInstrBase* cmpInstr, Block* incomingBlock,
                                        ControllingBlockList* controllingBlocks = nullptr);

    bool IsAlwaysTrue(CmpInstrBase* cmpInstr, Block* incomingBlock) {
        return IsAlwaysTrueOrFalse(cmpInstr, incomingBlock) == TrueFalseResult::AlwaysTrue;
    }

    bool IsAlwaysFalse(CmpInstrBase* cmpInstr, Block* incomingBlock) {
        return IsAlwaysTrueOrFalse(cmpInstr, incomingBlock) == TrueFalseResult::AlwaysFalse;
    }

    // Returns 'true' if the 'if' condition represented by 'opA'
    // definitely doesn't hold if the condition represented by 'opB' holds.
    bool IsFalseCorrelatedCondition(Operand* correlatedOp, Operand* otherOp,
                                    Block* incomingBlock, bool testAndOr = true);

	bool IsFalseCorrelatedConditionConst(IntConstant* correlatedIntConst,
										 IntConstant* otherIntConst,
										 CmpInstrBase* correlatedCmpInstr, 
										 CmpInstrBase* otherCmpInstr);

    bool IsFalseCorrelatedConditionInt(CmpInstrBase* correlatedCmpInstr,
                                       CmpInstrBase* otherCmpInstr,
                                       Block* incomingBlock);

    bool IsFalseCorrelatedConditionAndOr(Operand* correlatedOp, Operand* otherOp,
                                         Block* incomingBlock);

    bool IsFalseCorrelatedConditionOrAnd(OperandList& correlatedOrOperands, 
                                         OperandList& otherAndOperands,
                                         Block* incomingBlock);

    bool IsFalseCorrelatedConditionAndAnd(OperandList& correlatedOrOperands, 
                                          OperandList& otherAndOperands,
                                          Block* incomingBlock);

    bool IsFalseCorrelatedConditionAndOr(OperandList& correlatedOrOperands, 
                                         OperandList& otherAndOperands,
                                         Block* incomingBlock);

    bool IsFalseCorrelatedConditionOrOr(OperandList& correlatedOrOperands, 
                                        OperandList& otherAndOperands,
                                        Block* incomingBlock);

    bool IsNegatedOrder(OrderType order, OrderType negatedOrder);

	bool IsNegatedOrder(CmpInstrBase* cmpInstrA, CmpInstrBase* cmpInstrB) {
		return IsNegatedOrder(cmpInstrA->Order(), cmpInstrB->Order());
	}

    bool SameEqualityOperands(CmpInstrBase* cmpInstrA, 
                              CmpInstrBase* cmpInstrB);

    bool SameOperands(CmpInstrBase* cmpInstrA, 
                      CmpInstrBase* cmpInstrB);

    bool CollectAndOperands(Operand* op, OperandList& andOperands);

    bool CollectOrOperands(Operand* op, OperandList& orOperands);
    
    Operand* ThreadOperand(Operand* op, Block* incomingBlock, 
                           ControllingBlockList* controllingBlocks = nullptr,
                           int level = 0);

    Operand* ThreadArithmetic(ArithmeticInstr* arithmeticInstr, Block* incomingBlock, 
                              ControllingBlockList* controllingBlocks, int level);

    Operand* ThreadQuestion(QuestionInstr* questInstr, Block* incomingBlock, 
                            ControllingBlockList* controllingBlocks, int level);

    Operand* ThreadQuestionOperand(QuestionInstr* questInstr, bool threadFalseOp,
                                   Block* incomingBlock, ControllingBlockList* controllingBlocks, 
                                   int level);

    Instruction* CloneInstruction(Instruction* instr, Block* block,
                                  Instruction* beforeInstr);

	bool ThreadOverSameConditionBlock(Block* block);

    bool TryRemovePhi(PhiInstr* instr);

	bool TryRemovePhis(Block* block);

    bool IsCheapInstruction(Instruction* instr);

    void BlockSimplified(Block* block, int type);

    void BlockThreaded(Block* block, Block* oldTarget, Block* newTarget, 
                       int successorIndex);

	// Implements the 'SafetyInfoClient' interface.
	virtual LanguageInfo* LanguageInfoRequest() override {
		return GetLanguageInfo();
	}

	virtual CallGraph* CallGraphRequest() override {
		//! TODO: return call graph when available
		return nullptr;
	}

public:
    CFGSimplifier(ProfileInfo* profile = nullptr) {
        contEvalLimitAdjustment_ = IntProfileAdjustment(profile);
    }

    void Execute(Function* function);
};

} // namespace Optimization
#endif