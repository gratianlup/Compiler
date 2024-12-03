// ParameterReturnPromotion.hpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_PARAMETER_RETURN_PROMOTION_HPP
#define PC_OPTIMIZATION_PARAMETER_RETURN_PROMOTION_HPP

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
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

class ParameterReturnPromotion : public Pass, public CallNodeVisitor {
private:
    //
    enum ActionType {
        Action_RemoveParameter,
        Action_PromotePointerParameter,
        Action_ReplaceAggregatePointerParameter,
        Action_RemoveReturn,
        Action_None
    };


    //
    struct Action {
        ActionType Type;
        int ParameterIndex;
        bool UsedByNullComparison;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        Action() : Type(Action_None), UsedByNullComparison(false) {}

        Action(ActionType type, int paramIndex = -1, bool usedByNullComparison = false) :
                Type(type), ParameterIndex(paramIndex),
                UsedByNullComparison(usedByNullComparison) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        bool operator== (const Action& other) const {
            return ((int)Type == (int)other.Type) &&
                   (ParameterIndex == other.ParameterIndex);
        }

        bool operator< (const Action& other) const {
            return ParameterIndex < other.ParameterIndex;
        }
    };

    typedef StaticList<Action, 4> ActionList;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    //
    bool DetermineActions(CallNode* callNode, ActionList& actions);

    bool IsDefinitelyCalled(CallNode* callNode);

    //
    void DeterminePointerAction(int parameterIndex, CallNode* callNode, 
                                ActionList &actions);

    //
    bool ReturnedValueNotUsed(Function* function, CallNode* node);

    //
    bool UsedOnlyByLoadsOrNullComparison(Variable* parameterVariable, Function* function,
                                         bool& usedByNullComparison);

    //
    bool AllIncomingPointersNotNull(CallNode* callNode, int parameterIndex);

    //
    void FoldNullComparisons(Function* function, int parameterIndex);

    //
    void ExecuteActions(CallNode* callNode, ActionList& actions);

    //
    void ExecuteRemoveReturn(CallNode* callNode);

    //
    void ExecuteRemoveParameter(CallNode* callNode, Action action,
                                int& removedParameters, int addedParameters);

    //
    void ExecutePromotePointerParameter(CallNode* callNode, Action action,
                                        int removedParameters, int addedParameters);

    //
    void PromotePointerParameter(Parameter* parameter, Function* function);

    //
    void UpdateFunctionType(CallNode* callNode, ActionList& actions);

    // Methods that implement the CallNodeVisitor interface.
    virtual bool Visit(CallNode* node, CallGraph* callGraph) override;
    virtual bool Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) override;

public:
    void Execute(CallGraph* callGraph);
};

} // namespace Optimization

#endif

// ONLY FOR STATIC!
// Skip varargs
// 
// Parameters not used at all
//   => try remove
//      (do for more params at a time, remove arguments in caller)
//      
// Parameter only read and pointer to basic type
//   => promote to basic type
//      (do for more params at a time, remove loads in callee,
//       add loads in caller and replace arguments)
//    
// Parameter only read and pointer to array/record and sections known
// and at most K (2?) constant section elements read
//   => promote to K parameters of basic type
//      (replace load from section with parameter in callee,
//       add loads in caller, remove pointer argument and add new ones)
// 
// Return value not used in all callers
//   => replace returned value with undef for all returns
//   
// Return value is pointer only read in all callers
//   => replace return type with pointed type, return loaded value instead
//      (do for all returns, replace load from call return in all callers)
