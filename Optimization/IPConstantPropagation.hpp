// IPConstantPropagation.hpp
// Copyright (c) Lup Gratian
//
// Implements a fast interprocedural constant propagation algorithm.
// Propagates constants from arguments to parameters and from
// return points to the call return values. It also creates ranges
// if a parameter has multiple possible constants.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_IP_CONSTANT_PROPAGATION_HPP
#define PC_OPTIMIZATION_IP_CONSTANT_PROPAGATION_HPP

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Analysis/ParameterConstantsTag.hpp"
#include "../Analysis/CallGraph.hpp"
#include "../Analysis/ConstantFolder.hpp"
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

class IPConstantPropagation : public Pass, public CallNodeVisitor {
private:
    // Stores the constants associated with a parameter.
    struct ParameterInfo {
        StaticList<ConstantFunctionPair, 4> Constants;
        bool IsNotZero;
        bool CanBeUsed;

        ParameterInfo() : IsNotZero(true), CanBeUsed(true) {}
    };

    MAKE_PAIR(ArgumentValuePair, int, ArgumentIndex, Operand*, Value);
    MAKE_PAIR(ParameterConstantPair, Parameter*, ReplacedParameter, Constant*, Value);

    typedef StaticList<int, 2> NonNullPointerList;
    typedef StaticList<ArgumentValuePair, 4> ArgumentConstantPairList;
    typedef StaticList<ParameterConstantPair, 8> ReplacedParameterList;
    typedef Dictionary<CallSite*, ArgumentConstantPairList> CallSiteConstantsDict;
    typedef Dictionary<CallSite*, NonNullPointerList> NonNullPointerDict;
    typedef Dictionary<CallNode*, Constant*> ReturnedConstantsDict;
    typedef StaticList<Instruction*, 16> InstructionList;
    typedef StaticList<FunctionReference*, 4> FunctionList;
    typedef Dictionary<Parameter*, ParameterInfo> ParameterInfoDict;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    CallSiteConstantsDict callSiteConstants_;
    ReturnedConstantsDict returnedConstants_;
    ParameterInfoDict estimatedParameters_;
    NonNullPointerDict nonNullPointerArguments_;
    ConstantFolder folder_;
    bool propagateReturns_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //
    void CollectCallConstants(CallNode* callNode);

    //
    void CollectCallConstants(CallSite* callSite);

    // 
    bool IsEligibleArgument(Operand* op);

    //
    void AddParameterValue(Operand* constant, int index, CallSite* callSite);

    //
    void AddNonNullPointer(int index, CallSite* callSite);

    //
    Constant* MeetParameterConstants(int parameterIndex, CallNode* callNode);

    void MeetIncomingValue(int parameterIndex, CallNode* callNode,
                           CallSite* callingSite, ParameterInfo& paramInfo,
                           bool& isSingleConstant, Constant*& constant,
                           FunctionList& functions);

    void CopyParameterConstants(Parameter* source, ParameterInfo& destination);

    void CreateConstantsTag(Parameter* parameter, ParameterInfo& paramInfo);

    void CreateConstantsRangeTag(Parameter* parameter, ParameterInfo& paramInfo);

    //
    Operand* GetParameterValue(CallSite* callSite, int parameterIndex);

    //
    void ReplaceParameters(CallNode* callNode);

    //
    void ReplaceParameters(ReplacedParameterList& replacedParameters, Function* function);

    //
    void ConstantFoldInstructions(InstructionList& foldWorklist);

    //
    void FoldLoadsFromConstant(Function* function);

    //
    void AddUsersToWorklist(Instruction* instr, InstructionList& foldWorklist);

    //
    void StoreReturnedConstant(CallNode* callNode);

    //
    Constant* IdentifyReturnedConstant(Function* function);

    //
    void ReplaceReturns(CallNode* callNode);

    //
    Constant* MeetReturnedConstants(CallSite* callSite);

    //
    Constant* GetReturnedConstant(CallNodeBase* calledNode);

    // 
    bool IsPointerNotNull(Operand* op);

    virtual bool Visit(CallNode* node, CallGraph* callGraph) override;
    virtual bool Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) override;

public:
    void Execute(CallGraph* callGraph);
};

} // namespace Optimization
#endif