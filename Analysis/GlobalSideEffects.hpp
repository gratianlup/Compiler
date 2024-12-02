// GlobalSideEffects.hpp
// Copyright (c) Lup Gratian
//
// Determines which global variables are read/written by each function
// and if the function is not dependent on the global program state.
// A function can be marked as having no state if it doesn't 
// has any side effect and its returned value depends only 
// on the parameters (i.e doesn't read global variables or 
// calls functions that are not marked as having no state).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_GLOBAL_SIDE_EFFECTS_HPP
#define PC_OPTIMIZATION_GLOBAL_SIDE_EFFECTS_HPP

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Analysis/CallGraph.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/AliasInfo.hpp"
#include "../Analysis/GlobalConstantsTag.hpp"
#include "../Analysis/GlobalUsersTag.hpp"
#include "../Analysis/GlobalSideEffectsTag.hpp"
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

namespace Analysis {

class GlobalSideEffects : public Pass {
private:
    class NoStateVisitor : public CallNodeVisitor {
    private:
        typedef StaticList<Operand*, 2> OperandList;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        GlobalSideEffects* parent_;
        
        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        // 
        bool IsReadOfConstant(Operand* op);

        // 
        Function* GetFunctionDefinition(CallNode* node);

        // 
        bool GetSingleIndex(Operand* op, Operand* requiredBase, __int64& index);

        // 
        bool NodeGroupHasState(CallNodeGroup* group);

        // 
        bool ExternalFunctionHasState(Function* function, CallInstr* instr,
									  bool& isIndirect);

        // 
        bool HasOnlyAllowedTargets(Operand* op, bool& isIndirect,
								   bool allowGlobalConstants = false);

        // 
        bool ProcessParametersAndVariables(Function* function);

        // 
        bool ProcessInstructions(Function*function, CallGraph* callGraph);

        // 
        bool ProcessLoad(LoadInstr* instr);

        // 
        bool ProcessStore(StoreInstr* instr);

        // 
        bool ProcessCall(CallInstr* instr, CallGraph* callGraph);

        // 
        bool IsDeadGlobalStore(Operand* destinationOp, StoreInstr* instr);

        // 
        bool CollectBaseOperands(Operand* op, OperandList& list, 
								 bool& isIndirect, int level = 0);

        //
        void CreateGlobalEffectsTag(Function* function);

        //
        void MarkGlobalUse(VariableReference* variableRef, Function* function,
                           bool isRead = true, bool isWritten = true,
                           bool isAlwaysWritten = false);

        //
        void MarkGlobalUnknownEffects(Function* function);

        //
        void MarkParameterGlobalUses(CallInstr* instr, Function* calledFunction);

        //
        void MergeGlobalUses(Function* function, Function* calledFunction);

        //
        void MarkAllStateDependent(CallNodeGroup* nodeGroup);

        //
        void MergeAllGlobalEffects(CallNodeGroup* nodeGroup);

        void SaveAlwaysWrittenVariables(Function* function,
                                        StaticList<int, 2>& variables);

        void RestoreAlwaysWrittenVariables(Function* function,
                                           StaticList<int, 2>& variables);

    public:
        NoStateVisitor(GlobalSideEffects* parent) : parent_(parent) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        virtual bool Visit(CallNode* node, CallGraph* callGraph) override;

        virtual bool Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) override;
    };

public:
    void Execute(CallGraph* callGraph);
};

} // namespace Analysis
#endif