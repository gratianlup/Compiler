// ParameterReturnPromotion.cpp
// Copyright (c) Lup Gratian
//
// Implements the ParameterReturnPromotion pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ParameterReturnPromotion.hpp"

namespace Optimization {

void ParameterReturnPromotion::Execute(CallGraph* callGraph) {
    // Iterate over all functions and try to eliminate/promote
    // the parameters and the return values.
    callGraph->FindRoots(true /* onlyCalledByExternal */);
    callGraph->ReverseInvocationTraversalFromRoots(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterReturnPromotion::Visit(CallNode* node, CallGraph* callGraph) {
    ActionList actions;

    if(DetermineActions(node, actions)) {
        // Sort the actions based on the type and
        // on the index of the parameter (if it's the case).
        actions.Sort();

        ExecuteActions(node, actions);
        UpdateFunctionType(node, actions);
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterReturnPromotion::Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) {
    // Process each function in the group, order doesn't matter.
    nodeGroup->ForEachNode([this, callGraph](CallNodeBase* node) -> bool {
        if(node->IsNodeGroup() == false) {
            Visit(static_cast<CallNode*>(node), callGraph);
        }

        return true;
    });

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterReturnPromotion::DetermineActions(CallNode* callNode, 
                                                ActionList& actions) {
    // Function declarations and functions that could be called
    // by external functions can not be optimized. If the function 
    // is called through an indirect pointer it must be ignored, 
    // because removing/modifying an argument might be invalid
    // if other functions can be called through the same pointer.
    if(callNode->IsCalledByExternalFunctions() ||
       (IsDefinitelyCalled(callNode) == false)) {
        return false;
    }
    
    auto function = callNode->GetFunction();

    if(function->IsDeclaration()) {
        return false;
    }

    // Check which actions can be applied and add them to the list
    // (the order of execution will be established later).
    if(ReturnedValueNotUsed(function, callNode)) {
        actions.Add(Action(Action_RemoveReturn));
    }
    
    // Check each parameter and consider the ones that are
    // not used at all or are only read.
    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsAddressTaken() ||
           parameterVariable->IsEscape()) {
            continue;
        }

        if(parameterVariable->IsPointer()) {
            DeterminePointerAction(i, callNode, actions);
        }
        else if(parameterVariable->IsNoRead()) {
            actions.Add(Action(Action_RemoveParameter, i));
        }
    }

    return actions.IsNotEmpty();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterReturnPromotion::IsDefinitelyCalled(CallNode* callNode) {
    // Check if any of the call sites might be able to call
    // other functions than this one.
    bool valid = true;

    callNode->ForEachCallingSite([&valid](CallSite* callSite) -> bool {
        auto function = callSite->GetCallInstruction()->GetCalledFunction();

        if(function == nullptr) {
            // Call through indirect pointer.
            valid = false;
            return false;
        }

        return true;
    });

    return valid;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterReturnPromotion::DeterminePointerAction(int parameterIndex, 
                                                      CallNode* callNode, 
                                                      ActionList &actions) {
    // Check if the pointer parameter can either be removed completely
    // or promoted to a basic type value. Promotion can be done only
    // if the pointer is not written and all incoming pointer values
    // are definitely not the null pointer (otherwise we might introduce
    // an exception that wasn't in the original application).
    auto function = callNode->GetFunction();
    auto parameterVariable = function->GetParameterVariable(parameterIndex);

    if(parameterVariable->IsNoRead() && parameterVariable->IsNoWrite()) {
        actions.Add(Action(Action_RemoveParameter, parameterIndex));
    }
    else if(parameterVariable->IsNoWrite()) {
        auto pointerType = parameterVariable->GetType()->As<PointerType>();
        bool usedByComparison;

        // Promote a pointer to a basic type to a value.
        if((pointerType->PointeeType()->IsInteger() ||
            pointerType->PointeeType()->IsFloating()) == false) {
                return;
        }

        if(UsedOnlyByLoadsOrNullComparison(parameterVariable, function, usedByComparison) &&
           AllIncomingPointersNotNull(callNode, parameterIndex)) {
            actions.Add(Action(Action_PromotePointerParameter, 
                               parameterIndex, usedByComparison));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterReturnPromotion::UsedOnlyByLoadsOrNullComparison(Variable* parameterVariable,
                                                               Function* function,
                                                               bool& usedByNullComparison) {
    auto parameter = function->GetParameter(parameterVariable);
    bool usedOnlyByLoads = true;
    usedByNullComparison = false;

    function->ForEachInstruction([parameter, &usedOnlyByLoads, &usedByNullComparison]
                                 (Instruction* instr) -> bool {
        if(instr->HasSourceOp(parameter) && (instr->IsLoad() == false)) {
            // We allow the comparison of the pointer with the null constant.
            if(auto ucmpInstr = instr->As<UcmpInstr>()) {
                if(ucmpInstr->IsEquality() &&
                   (ucmpInstr->RightOp()->IsNullConstant() || 
                    ucmpInstr->LeftOp()->IsNullConstant())) {
                    usedByNullComparison = true;
                    return true;
                }
            }

            usedOnlyByLoads = false;
            return false;
        }
        else return true;
    });

    return usedOnlyByLoads;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterReturnPromotion::AllIncomingPointersNotNull(CallNode* callNode, 
                                                          int parameterIndex) {
    // Check if each pointer value that is incoming is not null.
    // An interprocedural constant-propagation step should be run before.
    OperandInfo opInfo(callNode->GetFunction()->ParentUnit(), GetTarget());

    for(int i = 0; i < callNode->CallingSitesCount(); i++) {
        auto callingSite = callNode->GetCallingSite(i);
        DebugValidator::IsFalse(callingSite->IsExternalCallSite());

        auto callInstr = callingSite->GetCallInstruction();
        auto argument = callInstr->GetArgument(parameterIndex);

        if(opInfo.IsPointerNotNull(argument, callInstr->ParentBlock()) == false) {
            return false;
        }
    }

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterReturnPromotion::FoldNullComparisons(Function* function,
                                                   int parameterIndex) {
    // The parameter is used by at least one comparison 
    // that compares it with the null pointer. 'equals' is replaced
    // by 'false' and 'not equals' by 'true'.
    auto parameter = function->GetParameter(parameterIndex);
    auto& constants = function->ParentUnit()->Constants();
    List<UcmpInstr*> deadList;

    function->ForEachInstructionOfType<UcmpInstr>([parameter, &constants, &deadList]
                                                  (UcmpInstr* instr) -> bool {
        if(instr->HasSourceOp(parameter)) {
            if(instr->HasDestinationOp()) {
                if(instr->IsEqual()) {
                    instr->ResultOp()->ReplaceWith(constants.GetInt32(0));
                }
                else if(instr->IsNotEqual()) {
                    instr->ResultOp()->ReplaceWith(constants.GetInt32(1));
                }
            }
            
            deadList.Add(instr);
        }
        return true;
    });

    // Remove any comparison that is not used at all.
    for(int i = 0; i < deadList.Count(); i++) {
        deadList[i]->RemoveFromBlock(true /* free */);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterReturnPromotion::ReturnedValueNotUsed(Function* function, 
                                                    CallNode* callNode) {
    // Check if all callers don't use the returned value.
    // If it's true then there is no reason to compute it,
    // (the 'undef' constant can be returned).
    if(function->IsVoid()) {
        return false;
    }

    bool returnIsUsed = false;

    callNode->ForEachCallingSite([&returnIsUsed](CallSite* callSite) -> bool {
        auto callingInstr = callSite->GetCallInstruction();

        if(callingInstr->ResultOp() &&
           callingInstr->ResultOp()->HasUsers()) {
            returnIsUsed = true;
            return false;
        }
        else return true;
    });

    return returnIsUsed == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterReturnPromotion::ExecuteActions(CallNode* callNode, 
                                              ActionList& actions) {
    // Execute each action, which may modify this function
    // and the call sites the call it.
    int removedParameters = 0;
    int addedParameters = 0;

    actions.ForEach([callNode, &removedParameters, &addedParameters,this]
                    (Action action) -> bool {
        switch(action.Type) {
            case Action_RemoveReturn: {
                ExecuteRemoveReturn(callNode);
                break;
            }
            case Action_RemoveParameter: {
                ExecuteRemoveParameter(callNode, action, 
                                       removedParameters, addedParameters);
                break;
            }
            case Action_PromotePointerParameter: {
                ExecutePromotePointerParameter(callNode, action, 
                                               removedParameters, addedParameters);
                break;
            }
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterReturnPromotion::ExecuteRemoveReturn(CallNode* callNode) {
    // Scan the function for 'ret' instructions and modify
    // them so that they return the undefined constant.
    // Dead Code Elimination will later eliminate the code
    // that computed the returned values.
    auto function = callNode->GetFunction();
    auto& constants = function->ParentUnit()->Constants();
    auto undefConst = constants.GetUndefined(function->ReturnType());

    function->ForEachInstructionOfType<ReturnInstr>([undefConst]
                                      (ReturnInstr* instr) -> bool {
        instr->SetReturnedOp(undefConst);
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterReturnPromotion::ExecuteRemoveParameter(CallNode* callNode, Action action,
                                                      int& removedParameters, 
                                                      int addedParameters) {
    // Remove the argument passed to the parameter
    // from each call site that calls this function.
    int actualIndex = action.ParameterIndex - removedParameters + addedParameters;

    callNode->ForEachCallingSite([actualIndex](CallSite* callSite) -> bool {
        auto callingInstr = callSite->GetCallInstruction();
        callingInstr->RemoveArgument(actualIndex);
        return true;
    });

    // Now remove the parameter from this function. If it is not read
    // it doesn't mean that it is not used in the function, it may
    // be a call argument for a function that doesn't read it too.
    auto function = callNode->GetFunction();
    auto parameter = function->GetParameter(actualIndex);

    callNode->GetFunction()->ForEachInstructionOfType<CallInstr>([parameter, function]
                                                     (CallInstr* instr) -> bool {
        auto innerFunction = function; //! TODO: Workaround for VC++ bug

        instr->ForEachEqualArgument(parameter, [instr, innerFunction]
                                               (int index) -> bool {
            auto& constants = innerFunction->ParentUnit()->Constants();
            auto undefConst = constants.GetUndefined(innerFunction->ReturnType());
            instr->ReplaceArgument(index, undefConst);
            return true;
        });

        return true;
    });
    
    // The function type will be updated later.
    function->RemoveParameter(actualIndex);
    removedParameters++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterReturnPromotion::ExecutePromotePointerParameter(CallNode* callNode, 
                                                              Action action,
                                                              int removedParameters, 
                                                              int addedParameters) {
    // A parameter that points to a basic type (integer/float)
    // is not written into, so it can be passed as a value instead.
    int actualIndex = action.ParameterIndex - removedParameters + addedParameters;
    auto function = callNode->GetFunction();

    // Constant-fold any comparison that compared the pointer
    // with the null pointer.
    FoldNullComparisons(function, actualIndex);

    // Change the parameter type to the pointee, then replace any 'load'
    // that targets it with the actual parameter.
    auto parameter = function->GetParameter(actualIndex);
    PromotePointerParameter(parameter, function);

    // Replace the type of the parameter variable too.
    auto parameterType = parameter->GetType();
    function->GetParameterVariable(actualIndex)->SetType(parameterType);
    
    // For each call site create a 'load' from the argument
    // and pass it instead of the pointer.
    callNode->ForEachCallingSite([parameterType, actualIndex]
                                 (CallSite* callSite) -> bool {
        auto callingInstr = callSite->GetCallInstruction();
        auto argument = callingInstr->GetArgument(actualIndex);

        // Create the 'load' from the current argument.
        auto loadedTemp = Temporary::GetTemporary(parameterType);
        auto loadInstr = LoadInstr::GetLoad(argument, loadedTemp);

        // Insert the 'load' before the call and replace 
        // the argument with the loaded value.
        auto block = callingInstr->ParentBlock();
        block->InsertInstructionBefore(loadInstr, callingInstr);
        callingInstr->ReplaceArgument(actualIndex, loadedTemp);
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterReturnPromotion::PromotePointerParameter(Parameter* parameter, 
                                                       Function* function) {
    // Change the type of the parameter to be the one of the pointee.
    auto pointerType = parameter->GetType()->As<PointerType>();
    DebugValidator::IsNotNull(pointerType);
    parameter->SetType(pointerType->PointeeType());

    // Replace each load from the parameter with the parameter itself.
    // We make a list because the enumerator is immutable.
    StaticList<LoadInstr*, 4> loads;

    function->ForEachInstructionOfType<LoadInstr>([parameter, &loads]
                                                  (LoadInstr* instr) -> bool {
        if(instr->SourceOp() == parameter) {
            loads.Add(instr);
        }

        return true;
    });

    for(int i = 0; i < loads.Count(); i++) {
        if(loads[i]->HasDestinationOp()) {
            loads[i]->GetDestinationOp()->ReplaceWith(parameter);
            loads[i]->RemoveFromBlock(true /* free */);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterReturnPromotion::UpdateFunctionType(CallNode* callNode, 
                                                  ActionList& actions) {
    // Some of the parameters have been change, so we need
    // to update the function type.
    auto function = callNode->GetFunction();
    List<const Type*> newParameters(function->ParameterTypes());
    int removedParams = 0;

    for(int i = 0; i < actions.Count(); i++) {
        if(actions[i].Type == Action_RemoveParameter) {
            newParameters.RemoveAt(actions[i].ParameterIndex - removedParams);
            removedParams++;
        }
    }

    auto& types = function->ParentUnit()->Types();
    auto newFunctionType = types.GetFunction(function->ReturnType(),
                                             newParameters.GetInternal(),
                                             newParameters.Count(), 
                                             function->IsVarargs());
    function->SetType(newFunctionType);
}

} // namespace Optimization