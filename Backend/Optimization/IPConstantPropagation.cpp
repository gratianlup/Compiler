// IPConstantPropagation.hpp
// Copyright (c) Lup Gratian
//
// Implements the IPConstantPropagation pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IPConstantPropagation.hpp"

namespace Optimization {

void IPConstantPropagation::Execute(CallGraph* callGraph) {
    DebugValidator::IsNotNull(callGraph);

    if(callGraph->GetCallNodes().Count() > 0) {
        auto callNode = static_cast<CallNode*>(callGraph->GetCallNodes()[0]);
        auto unit = callNode->GetFunction()->ParentUnit();
        IRGenerator irGen(unit);
        folder_ = ConstantFolder(&irGen, GetTarget());

        // First propagate arguments from each call site
        // to the corresponding parameters.
        propagateReturns_ = false;
        callGraph->FindRoots(true /* onlyCalledByExternal */);
        callGraph->InvocationTraversalFromRoots(this);

        // Propagate constant return values to the callers.
        propagateReturns_ = true;
        callGraph->ReverseInvocationTraversalFromRoots(this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IPConstantPropagation::Visit(CallNode* node, CallGraph* callGraph) {
    // If the call node represents the External/Unknown node
    // or a function declaration just ignore it.
    if(node->IsExternalNode() || node->IsUnknownNode()) {
        return true;
    }

    auto function = node->GetFunction();

    if(function->IsDefinition()) {
        if(propagateReturns_ == false) {
            // First we determine the parameters that are constants,
            // replace them and do some constant-folding and propagation.
            ReplaceParameters(node);

            // Try to constant-fold any load from global variables.
            // This could lead to the propagation of more constants.
            FoldLoadsFromConstant(function);

            // Collect the constant arguments from each call site
            // after the (possible) constants have been propagated to it.
            CollectCallConstants(node);
        }
        else {
            // First replace the returned values with constants,
            // if possible, and do some constant-folding and propagation.
            ReplaceReturns(node);

            // Try to identify the unique returned constant
            // that can be used by the callers.
            StoreReturnedConstant(node);
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IPConstantPropagation::Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) {
    // Don't propagate constants inside groups.
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::CollectCallConstants(CallNode* callNode) {
    // Collect the constants for each call site in the function.
    callNode->ForEachCallSite([this](CallSite* callSite) -> bool {
        CollectCallConstants(callSite);
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::CollectCallConstants(CallSite* callSite) {
    auto callInstr = callSite->GetCallInstruction();

    for(int i = 0; i < callInstr->ArgumentCount(); i++) {
        auto argument = callInstr->GetArgument(i);

        if(IsEligibleArgument(argument)) {
            AddParameterValue(argument, i, callSite);
        }
        else if(argument->IsPointer() && IsPointerNotNull(argument)) {
            AddNonNullPointer(i, callSite);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IPConstantPropagation::IsEligibleArgument(Operand* op) {
    if(op->IsConstant() || op->IsVariableReference()) {
        return true;
    }
    else if(auto parameter = op->As<Parameter>()) {
        return estimatedParameters_.ContainsKey(parameter);
    }
    else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::AddParameterValue(Operand* op, int argumentIndex, 
                                              CallSite* callSite) {
    if(callSiteConstants_.ContainsKey(callSite) == false) {
        callSiteConstants_.Add(callSite, ArgumentConstantPairList());
    }

    auto pair = ArgumentValuePair(argumentIndex, op);
    callSiteConstants_[callSite].Add(pair);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::AddNonNullPointer(int index, CallSite* callSite) {
    if(nonNullPointerArguments_.ContainsKey(callSite) == false) {
        nonNullPointerArguments_.Add(callSite, NonNullPointerList());
    }

    nonNullPointerArguments_[callSite].Add(index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::ReplaceParameters(CallNode* callNode) {
    // Check if there are parameters which are always the same constant
    // and propagate it if it's the case.
    auto function = callNode->GetFunction();
    ReplacedParameterList replacedParameters;

    for(int i = 0; i < function->ParameterCount(); i++) {
        // If the parameter is address-taken we can't replace it
        // because it might be modified in unknown ways inside the function.
        if(function->GetParameterVariable(i)->IsAddressTaken()) {
            continue;
        }

        if(auto constant = MeetParameterConstants(i, callNode)) {
            // Replace the parameter and add all functions that use it
            // to the worklist as constant-folding candidates.
            auto pair = ParameterConstantPair(function->GetParameter(i), constant);
            replacedParameters.Add(pair);
        }
    }

    if(replacedParameters.Count() > 0) {
        ReplaceParameters(replacedParameters, function);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::ReplaceParameters(ReplacedParameterList& replacedParameters,
                                              Function* function) {
    InstructionList foldWorklist;
    auto instructionEnum = function->GetInstructionEnum();

    while(instructionEnum.IsValid()) {
        auto instr = instructionEnum.Next();
        bool replaced = false;

        for(int i = 0; i < instr->SourceOpCount(); i++) {
            // If the operand is a parameter try to replace it.
            // If at least one parameter was replace we try to constant-fold it.
            auto parameter = instr->GetSourceOp(i)->As<Parameter>();

            if(parameter == nullptr) {
                continue;
            }

            for(int j = 0; j < replacedParameters.Count(); j++) {
                if(replacedParameters[j].ReplacedParameter == parameter) {
                    // Found a constant replacement.
                    instr->ReplaceSourceOp(i, replacedParameters[j].Value);
                    replaced = true;
                    break;
                }
            }
        }

        if(replaced && instr->HasDestinationOp() &&
           instr->GetDestinationOp()->HasUsers()) {
            foldWorklist.Add(instr);
        }
    }

    ConstantFoldInstructions(foldWorklist);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::ConstantFoldInstructions(InstructionList& foldWorklist) {
    InstructionList deadInstructions;

    while(foldWorklist.IsNotEmpty()) {
        auto instr = foldWorklist.RemoveLast();

        if(auto result = folder_.Fold(instr)) {
            // All users are added to the worklist because
            // they might fold later too.
            AddUsersToWorklist(instr, foldWorklist);
            instr->GetDestinationOp()->ReplaceWith(result);
            deadInstructions.Add(instr);
        }
    }

    // Remove any dead instructions.
    while(deadInstructions.Count() > 0) {
        deadInstructions.RemoveLast()->RemoveFromBlock(true /* free */);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::AddUsersToWorklist(Instruction* instr, 
                                               InstructionList& foldWorklist) {
    DebugValidator::IsTrue(instr->HasDestinationOp());

    instr->GetDestinationOp()->ForEachUser([&](Instruction* user, int i) -> bool {
        if(user->HasDestinationOp() && user->GetDestinationOp()->HasUsers() &&
           (foldWorklist.Contains(user) == false)) {
                foldWorklist.Add(user);
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Constant* IPConstantPropagation::MeetParameterConstants(int parameterIndex, 
                                                        CallNode* callNode) {
    // Verify the (possible) constant values incoming from each
    // of the functions that calls this one. If all of them
    // are the same the parameter can be replaced by the constant.
    // Even if not all values are the same, if all are constants
    // we record them so that we can build a range fro the parameter.
    ParameterInfo paramInfo;
    bool isSingleConstant = true; // Presume we have a single constant.
    FunctionList functions;
    Constant* constant = nullptr;

    for(int i = 0; i < callNode->CallingSitesCount(); i++) {
        auto callingSite = callNode->GetCallingSite(i);

        if(callingSite->IsExternalCallSite()) {
            // The function is called by an external function,
            // so it isn't safe to use the information gathered here
            // because we don't known which values might be incoming.
            paramInfo.CanBeUsed = paramInfo.IsNotZero = false;
            isSingleConstant = false;
        }

        MeetIncomingValue(parameterIndex, callNode, callingSite, paramInfo,
                          isSingleConstant, constant, functions);
    }

    if(isSingleConstant) {
        // The parameter is definitely an unique constant.
        return constant;
    }
    else {
        // Attach the range information to the parameter.
        auto parameter = callNode->GetFunction()->GetParameter(parameterIndex);
        estimatedParameters_.Add(parameter, paramInfo);

        CreateConstantsTag(parameter, paramInfo);
        return nullptr;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::MeetIncomingValue(int parameterIndex, CallNode* callNode,
                                              CallSite* callingSite, ParameterInfo& paramInfo,
                                              bool& isSingleConstant, Constant*& constant,
                                              FunctionList& functions) {
    auto value = GetParameterValue(callingSite, parameterIndex);

    if(value == nullptr) {
        if(nonNullPointerArguments_.ContainsKey(callingSite) &&
           nonNullPointerArguments_[callingSite].Contains(parameterIndex)) {
                // If we known that the pointer is definitely not null continue.
                isSingleConstant = false;
                return;
        }

        paramInfo.CanBeUsed = paramInfo.IsNotZero = false;
        isSingleConstant = false;
    }
    else if(auto otherConstant = value->As<Constant>()) {
        if(isSingleConstant && (constant == nullptr)) {
            // This is the first time we see a constant.
            constant = otherConstant;
            functions.Add(callingSite->GetFunctionReference());
        }
        else if(constant != otherConstant) {
            // We have more than one constant or parameters
            // were previously involved. Store the constant
            // in the information object if it isn't already there.
            if(isSingleConstant) {
                for(int i = 0; i < functions.Count(); i++) {
                    ConstantFunctionPair pair(constant, functions[i]);
                    paramInfo.Constants.Add(pair);
                }

                paramInfo.IsNotZero = constant->IsZeroInt() == false;
                isSingleConstant = false;
            }

            auto functionRef  = callingSite->GetFunctionReference();
            paramInfo.Constants.Add(ConstantFunctionPair(otherConstant, functionRef));
            paramInfo.IsNotZero &= otherConstant->IsZeroInt() == false;
        }
        else {
            // Remember the function from which it is incoming.
            functions.Add(callingSite->GetFunctionReference());
        }
    }
    else if(auto parameter = value->As<Parameter>()) {
        CopyParameterConstants(parameter, paramInfo);
        isSingleConstant = false;
    }
    else if(value->IsVariableReference()) {
        isSingleConstant = false;
    }
    else {
        // Nothing is known from this call site, presume
        // that any value could be passed.
        paramInfo.CanBeUsed = paramInfo.IsNotZero = false;
        isSingleConstant = false;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::CopyParameterConstants(Parameter* source, 
                                                   ParameterInfo& destination)  {
    DebugValidator::IsTrue(estimatedParameters_.ContainsKey(source));
    auto& otherParaminfo = estimatedParameters_[source];

    for(int i = 0; i < otherParaminfo.Constants.Count(); i++) {
        auto constant = otherParaminfo.Constants[i];

        if(destination.Constants.Contains(constant) == false) {
            destination.Constants.Add(constant);
        }
    }

    destination.IsNotZero &= otherParaminfo.IsNotZero;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::CreateConstantsTag(Parameter* parameter,    
                                               ParameterInfo& paramInfo) {
    // If nothing useful is known about the parameter there is
    // no reason to waste memory by creating the tag.
    // If the tag was created by a previous run keep it.
    if(((paramInfo.Constants.Count() == 0) && (paramInfo.IsNotZero == false)) ||
        parameter->GetVariable()->HasTag<ParameterConstantsTag>()) {
        return;
    }

    // If the parameter is a pointer for which we know
    // it can never be zero create the tag.
    if(parameter->IsPointer() && paramInfo.IsNotZero) {
        auto tag = ParameterConstantsTag::GetIsNotZero(paramInfo.CanBeUsed);
        parameter->GetVariable()->AddTag(tag);
        return;
    }

    // If there are at most 4 known constants
    // store them all, it can improve some optimizations.
    // Otherwise try to extract a range from the constants.
    if(paramInfo.Constants.Count() <= 4) {
        auto tag = ParameterConstantsTag::GetParameterConstants(paramInfo.Constants.Count(),
                                                                paramInfo.IsNotZero,
                                                                paramInfo.CanBeUsed);
        for(int i = 0; i < paramInfo.Constants.Count(); i++) {
            tag->AddConstant(paramInfo.Constants[i].Value,
                             paramInfo.Constants[i].IncomingFunction);
        }

        parameter->GetVariable()->AddTag(tag);
    }
    else if(paramInfo.CanBeUsed) {
        CreateConstantsRangeTag(parameter, paramInfo);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::CreateConstantsRangeTag(Parameter* parameter,    
                                                    ParameterInfo& paramInfo) {
    // If there are many constants determine and use the range
    // (its better than don't knowing anything about the parameter).
    __int64 lowConstant = std::numeric_limits<__int64>::max();
    __int64 highConstant = std::numeric_limits<__int64>::min();

    for(int i = 0; i < paramInfo.Constants.Count(); i++) {
        if(auto intConstant = paramInfo.Constants[i].Value->As<IntConstant>()) {
            if(intConstant->Value() < lowConstant) {
                lowConstant = intConstant->Value();
            }

            if(intConstant->Value() > highConstant) {
                highConstant = intConstant->Value();
            }
        }
        else return;
    }

    auto tag = ParameterConstantsTag::GetParameterRange(Range(lowConstant, highConstant),
                                                        paramInfo.IsNotZero, true);
    parameter->AddTag(tag);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* IPConstantPropagation::GetParameterValue(CallSite* callSite,  
                                                  int parameterIndex) {
    // Check if the parameter value incoming from the call site
    // is a constant. If it is not return 'nullptr'.
    if(callSiteConstants_.ContainsKey(callSite)) {
        auto& constantParams = callSiteConstants_[callSite];

        for(int i = 0; i < constantParams.Count(); i++) {
            if(constantParams[i].ArgumentIndex == parameterIndex) {
                return constantParams[i].Value;
            }
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::FoldLoadsFromConstant(Function* function) {
    InstructionList worklist;

    function->ForEachInstructionOfType<LoadInstr>([&worklist](LoadInstr* instr) -> bool {
        if((instr->SourceOp()->IsLocalVariableRef() == false) &&
           (instr->SourceOp()->IsParameter() == false)) {
            worklist.Add(instr);
        }

        return true;
    });

    if(worklist.Count() > 0) {
        ConstantFoldInstructions(worklist);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::StoreReturnedConstant(CallNode* callNode) {
    if(auto constant = IdentifyReturnedConstant(callNode->GetFunction())) {
        returnedConstants_.Add(callNode, constant);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Constant* IPConstantPropagation::IdentifyReturnedConstant(Function* function) {
    // 'void' functions don't return anything.
    if(function->IsVoid()) {
        return nullptr;
    }

    // Scan all 'ret' instructions and verify if all of them
    // return the same constant or the 'undef' constant.
    Constant* returnedConstant = nullptr;
    auto instructionEnum = function->GetInstructionEnum();

    while(instructionEnum.IsValid()) {
        auto instr = instructionEnum.Next();
        auto retInstr = instr->As<ReturnInstr>();

        if(retInstr == nullptr) {
            continue;
        }

        if(auto constant = retInstr->ReturnedOp()->As<Constant>()) {
            if(constant->IsUndefinedConstant()) {
                // Ignore 'undef'.
                continue;
            }
            else if(returnedConstant == nullptr) {
                // The first found constant.
                returnedConstant = constant;
            }
            else if(returnedConstant != constant) {
                // Not the same constants.
                return nullptr;
            }
        }
        else {
            // No constant returned, give up.
            return nullptr;
        }
    }

    return returnedConstant;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IPConstantPropagation::ReplaceReturns(CallNode* callNode) {
    // Try to replace the returned result
    // for each call side in this function. 
    // If it could be replaced we try to do constant-folding.
    InstructionList foldWorklist;

    callNode->ForEachCallSite([this, &foldWorklist](CallSite* callSite) -> bool {
        // If the returned value is not used skip the call site.
        auto callInstr = callSite->GetCallInstruction();

        if((callInstr->HasDestinationOp() == false) ||
           (callInstr->GetDestinationOp()->HasUsers() == false)) {
            return true;
        }

        if(auto constant = MeetReturnedConstants(callSite)) {
            AddUsersToWorklist(callInstr, foldWorklist);
            callInstr->GetDestinationOp()->ReplaceWith(constant);
        }

        return true;
    });

    if(foldWorklist.Count() > 0) {
        ConstantFoldInstructions(foldWorklist);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Constant* IPConstantPropagation::MeetReturnedConstants(CallSite* callSite) {
    // If the call site calls the External or Unknown node
    // we can't know exactly the returned values.
    if(callSite->CallsExternalFunctions() ||
       callSite->CallsUnknownFunctions()) {
        return nullptr;
    }

    // Check if all called functions return the same constant.
    Constant* returnedConstant = nullptr;

    for(int i = 0; i < callSite->CalledFunctionsCount(); i++) {
        auto calledNode = callSite->GetCalledNode(i);

        if(auto constant = GetReturnedConstant(calledNode)) {
            if(returnedConstant == nullptr) {
                returnedConstant = constant;
            }
            else if(constant != returnedConstant) {
                return nullptr;
            }
        }
        else {
            // No constant is returned.
            return nullptr;
        }
    }

    return returnedConstant;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Constant* IPConstantPropagation::GetReturnedConstant(CallNodeBase* calledNode) {
    // Node groups are not considered.
    if(calledNode->IsNodeGroup()) {
        return nullptr;
    }

    // Check if the function returns a constant.
    auto callNode = static_cast<CallNode*>(calledNode);
    Constant* constant;

    if(returnedConstants_.TryGetValue(callNode, &constant)) {
        return constant;
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IPConstantPropagation::IsPointerNotNull(Operand* op) {
    if(op->IsPointer() == false) {
        return false;
    }
    if(op->IsVariableReference()) {
        return true;
    }
    else if(auto parameter = op->As<Parameter>()) {
        if(estimatedParameters_.ContainsKey(parameter)) {
            auto& parameterInfo = estimatedParameters_[parameter];
            return parameterInfo.IsNotZero && 
                   parameterInfo.CanBeUsed;
        }
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        if(definingInstr->IsAddressing() || definingInstr->IsPtop()) {
            return IsPointerNotNull(definingInstr->GetSourceOp(0));
        }
        else if(auto questInstr = definingInstr->As<QuestionInstr>()) {
            return IsPointerNotNull(questInstr->TrueOp()) &&
                   IsPointerNotNull(questInstr->FalseOp());
        }
        else if(auto phiInstr = definingInstr->As<PhiInstr>()) {
            for(int i = 0; i < phiInstr->OperandCount(); i++) {
                if(phiInstr->GetOperand(i)->IsVariableReference() == false) {
                    return false;
                }
            }

            return true;
        }
    }

    return false;
}

} // namespace Optimization