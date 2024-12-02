// SafetyInfo.hpp
// Copyright (c) Lup Gratian
//
// Provides information about the safety of certain IR transformations,
// most of all useful for optimization and analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SAFETY_INFO_HPP
#define PC_ANALYSIS_SAFETY_INFO_HPP

#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
#include "../IR/Block.hpp"
#include "../IR/Tags.hpp"
#include "IRDominators.hpp"
#include "ControlDependenceGraph.hpp"
#include "OperandInfo.hpp"
#include "LanguageInfo.hpp"
#include "CallGraph.hpp"
using namespace IR;

namespace Analysis {

// Represents an interface that can be implemented by the Safety Info client
// to provide detailed information about the analyzed function.
// If the requested objects are not provided only a very conservative
// analysis might be performed.
class SafetyInfoClient {
public:
    // Should return the Dominator Tree for the specified function,
    // or 'nullptr' if it is not available/expensive to compute.
    virtual IRDominatorTree* DominatorTreeRequest(Function* function) {
        return nullptr;
    }

    // Should return the Control Dependence Graph for the specified function,
    // or 'nullptr' if it is not available/expensive to compute.
    virtual ControlDependenceGraph* ControlDependenceGraphRequest(Function* function) {
        return nullptr;
    }

	// Should return the Language Information for the compiled language,
	// or 'nullptr' if it is not available.
	virtual LanguageInfo* LanguageInfoRequest() {
		return nullptr;
	}

	// Should return the Call Graph for the translation unit,
	// or 'nullptr' if it is not available.
	virtual CallGraph* CallGraphRequest() {
		return nullptr;
	}
};


class SafetyInfo {
protected:
    SafetyInfoClient* client_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool IsNotControlDependent(Block* block, int level = 0);

public:
    SafetyInfo(SafetyInfoClient* client = nullptr) : client_(client) {}

    virtual ~SafetyInfo() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Returns 'true' if it is certain that the specified instruction is useless.
	// Can be used to perform a simple form of dead code elimination.
	virtual bool IsDefinitelyDead(Instruction* instr);
    
    // Returns 'true' if the specified 'load' instruction can be eliminated
    // without affecting the program semantics.
    virtual bool IsSafeToEliminateLoad(LoadInstr* instr);

    // Returns 'true' if the specified 'store' instruction can be eliminated
    // without affecting the program semantics.
    virtual bool IsSafeToEliminateStore(StoreInstr* instr);

    // Returns 'true' if the specified 'load' instruction
    // can be reordered in regard to other 'load' and 'store' instructions
    // without affecting the program semantics.
    virtual bool IsSafeToReorderLoad(LoadInstr* instr);

    // Returns 'true' if the specified 'store' instruction
    // can be reordered in regard to other 'load' and 'store' instructions
    // without affecting the program semantics.
    virtual bool IsSafeToReorderStore(StoreInstr* instr);

    // Returns 'true' if the specified instruction can be executed
    // without any information about the control flow (i.e it doesn't have
    // any side effects that could affect the execution of the program).
    virtual bool CanBeSpeculated(Instruction* instr, Block* toBlock = nullptr);

    // Returns 'true' if the definition point dominates the specified block.
	// If provided, 'failed' is set if the method failed.
    virtual bool DefinitionDominatesBlock(Operand* op, Block* block, 
										  bool* failed = nullptr);

    // Returns 'true' if block 'a' dominates block 'b'.
	// If provided, 'failed' is set if the method failed.
    virtual bool Dominates(Block* a, Block* b, bool* failed = nullptr);

    // Returns 'true' if instruction 'a' dominates instruction 'b'.
	// If provided, 'failed' is set if the method failed.
    virtual bool Dominates(Instruction* a, Instruction* b, bool* failed = nullptr);

    // Returns 'true' if the definition point of all operands 
    // of the instruction dominate the block.
	// If provided, 'failed' is set if the method failed.
    virtual bool OperandsDominateBlock(Instruction* instr, Block* block, 
									   bool* failed = nullptr);

    // Returns 'true' if the specified instruction can be executed 
    // speculatively in the block. This can happen only if the instruction
    // has no side-effects and if the definition point of the operands used 
    // in the instruction dominate the block.
    virtual bool CanBeSpeculatedInBlock(Instruction* instr, Block* block);

    // Returns 'true' if the specified block is always executed.
    // Note that this ignores any possible exception.
    virtual bool IsAlwaysExecuted(Block* block);

    // Returns 'true' if the specified instruction is always executed
    // Note that this ignores any possible exception.
    virtual bool IsAlwaysExecuted(Instruction* instr);

	// Returns 'true' if the function(s) called by the specified 
	// 'call' instruction may have any side-effects, such as reading/writing
	// global/parameter memory or calling other functions with side-effects.
	// If 'testIndirect' is set the Call Graph is queried for the possible
	// called functions in case of an indirect call thorough pointer.
	virtual bool CallMayHaveSideEffects(CallInstr* instr, bool testIndirectCall = true);

	// Returns 'true' if the specified function may have any side-effects, 
	// such as reading/writing global/parameter memory or calling other 
	// functions with side-effects.
	virtual bool FunctionMayHaveSideEffects(Function* funct);

	virtual bool FunctionMayHaveSideEffects(FunctionReference* functionRef) {
		DebugValidator::IsNotNull(functionRef);
		return FunctionMayHaveSideEffects(functionRef->Target());
	}
};

} // namespace Analysis
#endif