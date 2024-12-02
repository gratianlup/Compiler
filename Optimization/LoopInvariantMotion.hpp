// LoopInvariantMotion.hpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_LOOP_INVARIANT_MOTION_HPP
#define PC_OPTIMIZATION_LOOP_INVARIANT_MOTION_HPP

#include "../Analysis/Loop.hpp"
#include "../Analysis/LoopTag.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class LoopInvariantMotion : public Pass {
private:
	SparseBitVector visitedBlocks_;       // The blocks that have been processed.
	SparseBitVector noWriteAccessBlocks_; // The blocks that don't contain load/store/call instrs.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //
    bool IsLoopInvariant(Instruction* instr, Loop* loop);

    //
    bool IsLoopInvariant(Operand* op, Loop* loop);

    //
    bool IsLoopInvariantLoad(LoadInstr* instr, Loop* loop);

    //
    bool MayBeWrittenInLoop(Operand* op, Loop* loop);

    //
    bool IsLoopInvariantCall(CallInstr* instr, Loop* loop);

	//
	bool IsNotStateDependent(CallInstr* instr);

    //
    bool IsNotStateDependent(Function* function);

    //
    void HoistLoopInvariantInstructions(Loop* loop);

    //
    void HoistLoopInvariantInstructions(Block* block, Loop* loop);

    //
    void MoveToPreheader(Instruction* instr, Loop* loop);

	bool IsProfitableToHoist(Instruction* instr);

public:
    void Execute(Function* function);
};

} // namespace Optimization
#endif