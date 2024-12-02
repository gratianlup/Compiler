// ParameterFlags.hpp
// Copyright (c) Lup Gratian
//
// Determines for each pointer parameter if it
// escapes the function, is read and/or is written.
// It sets the 'noescape', 'noread' and 'nowrite' flags.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_PARAMETER_FLAGS_HPP
#define PC_ANALYSIS_PARAMETER_FLAGS_HPP

#include "CallGraph.hpp"
#include "AliasInfo.hpp"
#include "CFGWalker.hpp"
#include "SparseBitVector.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Analysis {

class ParameterFlags : public Pass {
private:
    // Stores information about a tracked operand
    // and about the parameter associated with it.
    struct TrackedOperandInfo {
        Operand* TrackedOperand;
        Variable* ParameterVariable;
        int ParameterIndex;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        TrackedOperandInfo() {}

        TrackedOperandInfo(Operand* op, Variable* paramVar, int index) :
                TrackedOperand(op), ParameterVariable(paramVar), 
				ParameterIndex(index) {}
    };


    // Class that visits the Call Graph nodes in postorder
    // (first the callees, then the callers).
    class EscapedParametersVisitor : public CallNodeVisitor {
    private:
        typedef List<TrackedOperandInfo> TrackedOperandsList;
        typedef StaticList<Parameter*, 8> TrackedParametersList;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        AliasInfo* aliasInfo_;
        ParameterFlags* parent_;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        Function* GetFunctionDefinition(CallNode* node);

        //
        void MakeTrackedParameterList(Function* function, 
                                      TrackedOperandsList& trackedOps,
                                      TrackedParametersList& trackedParams);

        //
        void ProcessInstructions(Function* function, 
                                 TrackedOperandsList& trackedOps,
                                 TrackedParametersList& trackedParams,
                                 CallGraph* callGraph);

        //
        void ProcessCall(CallInstr* instr, TrackedOperandsList& trackedOps,
                         TrackedParametersList& trackedParams, CallGraph* callGraph);

        //
        bool ProcessPointerArgument(CallInstr* instr, Operand* argument, 
                                    int parameterIndex, CallGraph* callGraph,
                                    TrackedOperandsList& trackedOps);

        //
        void ParameterIsModifiedInTargets(CallInstr* instr, int parameterIndex,
                                          CallGraph* callGraph, bool& escapes,
                                          bool& read, bool& written);

        //
        void NodeGroupHasModifiedPointers(CallNodeGroup* nodeGroup,
                                          bool& escapes, bool& read, bool& written);

        //
        void MarkEscapedParameters(Operand* op, TrackedOperandsList& trackedOps);

        void MarkReadParameters(Operand* op, TrackedOperandsList& trackedOps);

        void MarkWrittenParameters(Operand* op, TrackedOperandsList& trackedOps);

        void MarkReadWrittenParameters(Operand* op, TrackedOperandsList& trackedOps);

        void MarkReadParameters(Instruction* instr, TrackedParametersList& trackedParams);

        //
        void AddTrackedOperand(Operand* trackedOp, Operand* candidateOp, 
                               TrackedOperandsList& trackedOps);

        //
        void PatchLoopOperand(PhiInstr* phiInstr, Operand* incomingOp,
                              TrackedOperandsList& trackedOps);

        bool IsOperandTracked(Operand* op, TrackedOperandsList& trackedOps);

        void HasModifiedParameters(CallNode* node, bool& hasEscaped, 
                                   bool& hasRead, bool& hasWwritten);

        void MarkAllParameters(CallNode* node, bool escaped, bool read, bool written);

        void Dump(Function* function);

    public:
        EscapedParametersVisitor(ParameterFlags* parent, AliasInfo* aliasInfo) : 
                parent_(parent), aliasInfo_(aliasInfo) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        virtual bool Visit(CallNode* node, CallGraph* callGraph) override;

        virtual bool Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) override;
    };

public:
    void Execute(CallGraph* callGraph, AliasInfo* aliasInfo);
};

} // namespace Analysis
#endif