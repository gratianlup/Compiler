// PointerParameterAlias.hpp
// Copyright (c) Lup Gratian
//
// Determines the pairs of pointer arguments that may alias
// and propagates this information to the callees.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_POINTER_PARAMETER_ALIAS_HPP
#define PC_ANALYSIS_POINTER_PARAMETER_ALIAS_HPP

#include "CallGraph.hpp"
#include "AliasInfo.hpp"
#include "AllocHelper.hpp"
#include "SparseBitVector.hpp"
#include "ParameterAliasTag.hpp"
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

namespace Analysis {

class PointerParameterAlias : public Pass {
private:
	//! TODO: this should be a control
	static const int MAX_PARAMETERS = 8;
	static const int MAX_POINTER_ARGUMENTS = 8;

    class AliasVisitor : public CallNodeVisitor {
    private:
        typedef StaticList<int, 2> ParameterIndexList;

        AliasInfo* aliasInfo_;
		PointerParameterAlias* parent_;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        // 
        Function* GetFunctionDefinition(CallNode* node);

        //
        ParameterAliasTag* GetOrCreateTag(Variable* variable);

        //
        bool FindAliasedParameters(CallInstr* instr, Operand* argument,
                                   int argumentIndex, ParameterIndexList& parameters);

        //
        void PropagateAliasedParameters(CallSite* callSite, int parameterIndex, 
                                        ParameterIndexList& aliasedParameters);

        // 
        bool AddAliasedParameters(ParameterIndexList& parameters, 
                                  ParameterAliasTag* tag);

        //
        void MarkAllUnknown(CallSite* callSite);

        // 
        void MarkAllUnknown(Function* function);

        // 
        int PointerArgumentCount(CallInstr* instr);

		//
		bool MayBeGlobalVariable(Operand* op);

    public:
        AliasVisitor(AliasInfo* aliasInfo, PointerParameterAlias* parent) : 
				aliasInfo_(aliasInfo), parent_(parent) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
        virtual bool Visit(CallNode* node, CallGraph* callGraph) override;

        virtual bool Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) override;
    };

public:
    void Execute(CallGraph* callGraph, AliasInfo* aliasInfo);
};

} // namespace Analysis
#endif