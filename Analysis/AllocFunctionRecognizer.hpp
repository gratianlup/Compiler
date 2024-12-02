// AllocFunctionRecognizer.hpp
// Copyright (c) Lup Gratian
//
// Tries to recognize functions that behave like 'malloc'
// and sets the 'alloc' flag. Helps alias analysis for programs that
// wrap the call to 'malloc'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_ALLOC_FUNCTION_RECOGNIZER_HPP
#define PC_ANALYSIS_ALLOC_FUNCTION_RECOGNIZER_HPP

#include "CFamilyTag.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

class AllocFunctionRecognizer : public Pass, private CallNodeVisitor {
private:
	MAKE_PAIR(CallCheckedPair, CallInstr*, Call, bool, IsChecked);
	typedef StaticList<CallCheckedPair, 2> AllocationList;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Returns 'true' if the specified function might act
	// as a memory allocation routine.
	bool IsCandidate(Function* function);

	// Walks the chain of instructions starting with the specified operand
	// until a call to a memory allocation routine is reached.
	// Any found 'call' is added to 'allocations'.
	// Returns 'true' if all possible calls where found.
	bool FindBaseAllocCalls(Operand* op, AllocationList& allocations,
						    bool phiAllowed = true);

	// Returns 'true' if the specified 'call' instruction calls only
	// functions that allocate memory. 'isChecked' is set to true if
	// the functions guarantee that the returned pointer is not null.
	bool IsAllocationCall(CallInstr* instr, bool& isChecked);

	bool IsAllocationCall(Function* function, bool& isChecked);

	// Returns 'true' if the pointer returned by all called functions 
	// is guaranteed not to be null.
	bool IsCheckedAllocation(AllocationList& allocations);

	bool IsCheckedAllocation(Temporary* op);

	// Returns 'true' if the specified block contains a call
	// to a function that forces the application to close immediately.
	bool IsForcedExitBlock(Block* block);

	Temporary* WithoutPtop(Temporary* temp);

	// Scans the function and adds to the list all allocated pointer operands
	// returned by the function. Returns 'true' if all paths return such pointers.
	bool CollectReturns(Function* function, AllocationList& allocations);

	CFamilyTag* GetOrCreateTag(Function* function);

	virtual bool Visit(CallNode* node, CallGraph* callGraph) override;

public:
	void Execute();
};

} // namespace Analysis
#endif