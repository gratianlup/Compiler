// ParameterFlags.hpp
// Copyright (c) Lup Gratian
//
// Implements the ParameterFlags pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ParameterFlags.hpp"

namespace Analysis {

void ParameterFlags::Execute(CallGraph* callGraph, AliasInfo* aliasInfo) {
    local<EscapedParametersVisitor> visitor = new EscapedParametersVisitor(this, aliasInfo);
    callGraph->FindRoots(true);
    callGraph->ReverseInvocationTraversalFromRoots(visitor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterFlags::EscapedParametersVisitor::Visit(CallNode* node,
                                                     CallGraph* callGraph) {
    // Ignore the External and Unknown nodes
    // and functions without a body.
    auto function = GetFunctionDefinition(node);
    
    if(function == nullptr) {
        return true;
    }

    // Make the list of the parameters that need to be tracked.
    TrackedOperandsList trackedOps;
    TrackedParametersList trackedParams;

    MakeTrackedParameterList(function, trackedOps, trackedParams);
    
    if((trackedOps.Count() == 0) && 
       (trackedParams.Count() == 0)) {
        // Some parameters might escape because their address is taken.
        return true;
    }

    ProcessInstructions(function, trackedOps, trackedParams, callGraph);

#if 0
	//! TODO: use a control
    Dump(function);
#endif

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     ProcessInstructions(Function* function, TrackedOperandsList& trackedOps,
                         TrackedParametersList& trackedParams, CallGraph* callGraph) {
    // Process each instruction and check if the tracked
    // parameter/operands escape because one of them.
    CFGInfo<Block, Function> cfgInfo(function, false /* edgeInfoNeeded */);
    auto& postorderList = cfgInfo.PostorderList();

    for(int i = postorderList.Count() - 1; i >= 0; i--) {
        auto block = const_cast<Block*>(postorderList[i]);

        block->ForEachInstruction([this, &trackedOps, &trackedParams, callGraph]
                                  (Instruction* instr) -> bool {
            // Mark any non-pointer parameters used by the instruction.
            // Call arguments are handled separately.
            if(instr->IsCall() == false) {
                MarkReadParameters(instr, trackedParams);
            }

            // All other tests are for tracked pointer parameters/operands.
            if(trackedOps.Count() == 0) {
                return true; // Continue with next.
            }

            if(auto storeInstr = instr->As<StoreInstr>()) {
                // If the parameter is stored we mark it as escaped.
                MarkEscapedParameters(storeInstr->SourceOp(), trackedOps);

                // Mark the read/written parameters.
                MarkReadParameters(storeInstr->SourceOp(), trackedOps);
                MarkWrittenParameters(storeInstr->DestinationOp(), trackedOps);
            }
            else if(auto callInstr = instr->As<CallInstr>()) {
                ProcessCall(callInstr, trackedOps, trackedParams, callGraph);
            }
            else if(auto retInstr = instr->As<ReturnInstr>()) {
                // If the parameter is stored we mark it as escaped.
                if(retInstr->IsVoid() == false) {
                    MarkEscapedParameters(retInstr->ReturnedOp(), trackedOps);
                }
            }
            else if(auto loadInstr = instr->As<LoadInstr>()) {
                // Mark the read parameters.
                MarkReadParameters(loadInstr->SourceOp(), trackedOps);
            }
            else if(instr->IsComparison() == false) {
                if(instr->IsPtop() || instr->IsAddressing()) {
                    AddTrackedOperand(instr->GetDestinationOp(),
                                      instr->GetSourceOp(0), trackedOps);
                }
                else if(auto phiInstr = instr->As<PhiInstr>()) {
                    // If the 'phi' doesn't operate on pointers (and most don't)
                    // there is no reason to check the incoming operands.
                    if(phiInstr->ResultOp() && phiInstr->ResultOp()->IsPointer()) {
                        for(int i = 0; i < phiInstr->OperandCount(); i++) {
                            auto incomingOp = phiInstr->GetOperand(i);

                            PatchLoopOperand(phiInstr, incomingOp, trackedOps);
                            AddTrackedOperand(phiInstr->GetDestinationOp(),
                                              incomingOp, trackedOps);
                        }
                    }
                }
                else if(auto questInstr = instr->As<QuestionInstr>()) {
                    AddTrackedOperand(questInstr->GetDestinationOp(),
                                      questInstr->TrueOp(), trackedOps);
                    AddTrackedOperand(questInstr->GetDestinationOp(),
                                      questInstr->FalseOp(), trackedOps);
                }
                else {
                    // Any other instruction that uses the parameters
                    // is consider to let them escape.
                    for(int i = 0; i < instr->SourceOpCount(); i++) {
                        MarkEscapedParameters(instr->GetSourceOp(i), trackedOps);
                    }
                }
            }

            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* ParameterFlags::EscapedParametersVisitor::
          GetFunctionDefinition(CallNode* node) {
    // Ignore the External and Unknown nodes.
    if(node->IsExternalNode() || node->IsUnknownNode()) {
        return nullptr;
    }

    // Consider only defined functions.
    auto function = node->GetFunction();

    if(function->IsDefinition() == false) {
        return nullptr;
    }
    else return function;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterFlags::EscapedParametersVisitor::Visit(CallNodeGroup* node, 
                                                     CallGraph* callGraph) {
    // Process each node in the group. If any of the parameters
    // escaped we mark all other ones as escaped.
    bool paramsEscaped = false;
    bool paramsRead = false;
    bool paramsWritten = false;

    for(int i = 0; i < node->NodeCount(); i++) {
        auto childNode = node->GetNode(i);

        if(childNode->IsNodeGroup() == false) {
            auto childCallNode = static_cast<CallNode*>(childNode);
            Visit(childCallNode, callGraph);

            HasModifiedParameters(childCallNode, paramsEscaped,
                                  paramsRead, paramsWritten);
            if(paramsEscaped && paramsRead && paramsWritten) {
                break;
            }
        }
        else { 
            paramsEscaped = true;
            paramsRead = true;
            paramsWritten = true;
            break;
        }
    }

    if(paramsEscaped) {
        // Mark all parameters as escaped, it's a very conservative assumption.
        for(int i = 0; i < node->NodeCount(); i++) {
            auto childNode = node->GetNode(i);

            if(childNode->IsNodeGroup() == false) {
                auto childCallNode = static_cast<CallNode*>(childNode);
                MarkAllParameters(childCallNode, paramsEscaped,
                                  paramsRead, paramsWritten);
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     HasModifiedParameters(CallNode* node, bool& hasEscaped, 
                           bool& hasRead, bool& hasWritten) {
    auto function = GetFunctionDefinition(node);
    
    if(function) {
        for(int i = 0; i < function->ParameterCount(); i++) {
            auto parameterVariable = function->GetParameterVariable(i);
            hasEscaped |= parameterVariable->IsEscape();
            hasRead    |= parameterVariable->IsRead();
            hasWritten |= parameterVariable->IsWrite();
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkAllParameters(CallNode* node, bool escaped, bool read, bool written) {
    auto function = GetFunctionDefinition(node);
    
    if(function) {
        for(int i = 0; i < function->ParameterCount(); i++) {
            auto parameterVariable = function->GetParameterVariable(i);
            parameterVariable->SetIsNoEscape(escaped == false);
            parameterVariable->SetIsNoRead(read == false);
            parameterVariable->SetIsNoWrite(written == false);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     ProcessCall(CallInstr* instr, TrackedOperandsList& trackedOps, 
                 TrackedParametersList& trackedParams, CallGraph* callGraph) {
    // Calls are more complex than the other instructions.
    // We try to use both language information and information
    // about the escaped parameters of all functions that could be
    // called by this call site.
    if(auto parameter = instr->TargetOp()->As<Parameter>()) {
        if(trackedParams.Contains(parameter)) {
            parameter->GetVariable()->SetIsNoRead(false);
        }
    }

    for(int i = 0; i < instr->ArgumentCount(); i++) {
        // Check only pointer arguments. Parameters that were
        // converted to other types are already marked as escaped.
        auto argument = instr->GetArgument(i);
        bool isTracked = false;

        if(argument->IsPointer()) {
            isTracked = ProcessPointerArgument(instr, argument, i,
                                               callGraph, trackedOps);
        }

        if((isTracked == false) && argument->IsParameter()) {
            bool isEscaped;
            bool isRead;
            bool isWritten;

            ParameterIsModifiedInTargets(instr, i, callGraph,
                                         isEscaped, isRead, isWritten);
            if(isRead) {
                auto variable = argument->As<Parameter>()->GetVariable();
                variable->SetIsNoRead(false);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterFlags::EscapedParametersVisitor::
     ProcessPointerArgument(CallInstr* instr, Operand* argument, 
                            int parameterIndex, CallGraph* callGraph,
                            TrackedOperandsList& trackedOps) {
    // Check if this parameter is tracked.
    bool isEscaped;
    bool isRead;
    bool isWritten;
    bool isTracked = false;
    bool modifiedComputed = false;

    for(int j = 0; j < trackedOps.Count(); j++) {
        if(trackedOps[j].TrackedOperand != argument) {
            continue;
        }

        // Check if the parameter escapes in any
        // of the functions that may be called.
        isTracked = true; 

        if(modifiedComputed == false) {
            modifiedComputed = true;
            ParameterIsModifiedInTargets(instr, parameterIndex, callGraph,
                                         isEscaped, isRead, isWritten);
        }

        if(isEscaped) {
            auto parameterVariable = trackedOps[j].ParameterVariable;
            parameterVariable->SetIsNoEscape(false);
            parameterVariable->SetIsNoRead(false);
            parameterVariable->SetIsNoWrite(false);
            trackedOps.RemoveAt(j); // No reason to track it anymore.
            j--;
        }
        else {
            if(isRead) {
                trackedOps[j].ParameterVariable->SetIsNoRead(false);
            }

            if(isWritten) {
                trackedOps[j].ParameterVariable->SetIsNoWrite(false);
            }
        }
    }

    return isTracked;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     ParameterIsModifiedInTargets(CallInstr* instr, int parameterIndex,
                                  CallGraph* callGraph, bool& escapes,
                                  bool& read, bool& written) {
    // If the Unknown node is called we sadly need to presume
    // the parameter escapes/is read/is written. 
    // If the External node is called we can use language information.
    escapes = read = written = false;
    auto callSite = callGraph->GetCallSite(instr);

    if(callSite->CallsUnknownFunctions()) {
        escapes = read = written = true;
        return;
    }

    for(int i = 0; i < callSite->CalledFunctionsCount(); i++) {
        auto node = callSite->GetCalledNode(i);

        // For node groups we are conservative and presume it may escape
        // if there is at least one function with an escaping parameter.
        if(node->IsNodeGroup()) {
            auto nodeGroup = static_cast<CallNodeGroup*>(node);
            NodeGroupHasModifiedPointers(nodeGroup, escapes, read, written);
            
            if(escapes && read && written) {
                return;
            }
            else continue;
        }
        
        // We have a single call node. If it represents the External node
        // we skip it because the external function was already processed
        // or will be processed next.
        auto callNode = static_cast<CallNode*>(node);

        if(callNode->IsExternalNode()) {
            continue;
        }

        // If the function has a definition it was already 
        // processed and can use its escape/read/written information.
        auto calledFunction = callNode->GetFunction();

        if(calledFunction->IsDefinition()) {
            auto parameterVar = calledFunction->GetParameterVariable(parameterIndex);

            escapes |= parameterVar->IsEscape();
            read    |= parameterVar->IsRead();
            written |= parameterVar->IsWrite();
        }
        else if(auto intrinsic = instr->GetIntrinsic()) {
            // 'setMemory', 'copyMemory' and the prefetch intrinsics
            // don't capture their parameters. The source could be written
            // only if it aliases with the destination ('copyMemory' case).
            if(intrinsic->IsMemoryIntrinsic()) {
                if(intrinsic->Is<CopyMemoryIntr>()) {
                    auto destination = CopyMemoryIntr::GetDestination(instr);
                    auto source = CopyMemoryIntr::GetSource(instr);

                    if(aliasInfo_->HasMayAliasWithUnknownSize(destination, source)) {
                        read = written = true;
                        continue;
                    }
                }

                read    |= (parameterIndex == 1); // Source operand.
                written |= (parameterIndex == 0); // Destination operand.
            }
            else { 
                escapes = read = written = true;
                return;
            }
        }
        else if(auto languageInfo = parent_->GetLanguageInfo()) {
            // Try to use language information for declarations.
            // For some standard library function we know they
            // don't capture any parameters.
            read = written = true;
            escapes |= languageInfo->CallMayCaptureParameter(calledFunction,
                                                             instr, parameterIndex);
        }
        else {
            escapes = read = written = true;
            return;
        }

        if(escapes && read && written) {
            return;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     NodeGroupHasModifiedPointers(CallNodeGroup* nodeGroup, bool& escapes, 
                                  bool& read, bool& written) {
    // Check if there is a function in the group
    // with escaping pointer parameters.
    for(int i = 0; i < nodeGroup->NodeCount(); i++) {
        auto node = nodeGroup->GetNode(i);

        // Don't analyze nested node group (they shouldn't really appear anyhow).
        if(node->IsNodeGroup()) {
            escapes = read = written = true;
            return;
        }

        auto callNode = static_cast<CallNode*>(node);
        auto calledFunction = callNode->GetFunction();

        for(int j = 0; j < calledFunction->ParameterCount(); j++) {
            auto parameterVariable = calledFunction->GetParameterVariable(j);

            if(parameterVariable->IsPointer()) {
               escapes |= parameterVariable->IsEscape();
               read    |= parameterVariable->IsRead();
               written |= parameterVariable->IsWrite();
               
               if(escapes && read && written) {
                   return;
               }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MakeTrackedParameterList(Function* function, TrackedOperandsList& trackedOps,
                              TrackedParametersList& trackedParams) {
    // Add to the list all parameters that are pointers.
    // A parameter that has its address taken is marked 
    // as escaped from the start.
    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsAddressTaken()) {
            parameterVariable->SetIsNoEscape(false);
            parameterVariable->SetIsNoRead(false);
            parameterVariable->SetIsNoWrite(false);
        }
        else {
            auto parameter = function->GetParameter(i);
            parameterVariable->SetIsNoRead(true);   // Assume it isn't read.
            parameterVariable->SetIsNoEscape(true); // Assume it doesn't escape.

            // 'nowrite' and 'noescape' are useful only for pointers.
            bool isPointer = parameterVariable->IsPointer();
            parameterVariable->SetIsNoWrite(isPointer);  // Assume it isn't written.
            
            if(isPointer) {
                trackedOps.Add(TrackedOperandInfo(parameter, parameterVariable, i));
            }
            else {
                // For non-pointer parameters we don't have any details.
                trackedParams.Add(parameter);
            }
        }
    }

	// At the same time track what happens with the local variables.
	// We're interested if an address originating from a local variable
	// escapes or not (information used by alias analysis later).
	for(int i = 0; i < function->VariableCount(); i++) {
		auto variable = function->GetVariable(i);

		variable->SetIsNoEscape(true); // Assume it doesn't escape.
		trackedOps.Add(TrackedOperandInfo(variable->GetReference(), variable, i));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     AddTrackedOperand(Operand* targetOp, Operand* candidateOp, 
                       TrackedOperandsList& trackedOps) {
    // Ignore instructions without users and constant candidates.
    if((targetOp == nullptr) || candidateOp->IsConstant()) {
        return;
    }

    // Scan the tracked operands and for each one that matches
    // 'candidateOp' add tracking information for 'targetOp',
    // having associated the parameter of 'candidateOp'.
    int count = trackedOps.Count();

    for(int i = 0; i < count; i++) {
        auto& trackedOp = trackedOps[i];

        if(trackedOp.TrackedOperand == candidateOp) {
            trackedOps.Add(TrackedOperandInfo(targetOp, trackedOp.ParameterVariable, 
                                              trackedOp.ParameterIndex));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     PatchLoopOperand(PhiInstr* phiInstr, Operand* incomingOp,
                      TrackedOperandsList& trackedOps) {
    // When visiting the incoming operands it's possible that
    // not all of them have been analyzed and added to the tracking set
    // in case of a loop. Consider the following example:
    // int* z = p;
    // while(x) {
    //     *z = 0;    // Both 'p' and 'q' must be marked as written,
    //     z = &q[2]; // but '&q[2]' is added only after the store is seen.
    // }
    // 
    // To solve this problem we walk backwards, starting with the incoming operand
    // and collecting all tracked operands found by looking through addressing,
    // 'phi' and 'quest' instructions, then associating them with the incoming one.
    if(incomingOp->IsVariableReference() || incomingOp->IsFunctionReference() ||
       IsOperandTracked(incomingOp, trackedOps)) {
        return;
    }

    StaticList<Operand*, 4> foundTrackedOps;
    StaticList<PhiInstr*, 2> visitedPhis;
    StaticList<Operand*, 8> worklist;

    visitedPhis.Add(phiInstr);
    worklist.Add(incomingOp);

    while(worklist.IsNotEmpty()) {
        auto op = worklist.RemoveLast();

        if(IsOperandTracked(op, trackedOps))  {
            if(foundTrackedOps.Contains(op) == false) {
                foundTrackedOps.Add(op);
            }
        }
        else if(auto definingInstr = op->DefiningInstruction()) {
            if(definingInstr->IsAddressing() || 
               definingInstr->IsPtop()) {
                // Track the base operand only.
                worklist.Add(definingInstr->GetSourceOp(0));
            }
            else if(auto questInstr = definingInstr->As<QuestionInstr>()) {
                // Track both the true and false operands.
                worklist.Add(questInstr->TrueOp());
                worklist.Add(questInstr->FalseOp());
            }
            else if(auto phiInstr = definingInstr->As<PhiInstr>()) {
                // If the 'phi' has already been processed skip it,
                // else we enter an infinite loop.
                if(visitedPhis.Contains(phiInstr) == false) {
                    for(int i = 0; i < phiInstr->OperandCount(); i++) {
                        worklist.Add(phiInstr->GetOperand(0));
                    }
                }

                visitedPhis.Add(phiInstr);
            }
        }
    }

    // Associate all found operands with the incoming operand.
    for(int i = 0; i < foundTrackedOps.Count(); i++) {
        AddTrackedOperand(incomingOp, foundTrackedOps[i], trackedOps);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ParameterFlags::EscapedParametersVisitor::
     IsOperandTracked(Operand* op, TrackedOperandsList& trackedOps) {
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkEscapedParameters(Operand* op, TrackedOperandsList& trackedOps) {
    // Check if the operand is among the tracked ones. 
    // If it is mark the associated parameter as escaped,
    // and sadly we also need to mark it as read and written.
    // A linear search is OK because there are few parameters.
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            auto parameterVariable = trackedOps[i].ParameterVariable;
            parameterVariable->SetIsNoEscape(false);
            parameterVariable->SetIsNoRead(false);
            parameterVariable->SetIsNoWrite(false);
            
            // There is no reason to track it anymore.
            trackedOps.RemoveAt(i); 
            i--;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkReadParameters(Operand* op, TrackedOperandsList& trackedOps) {
    // Check if the operand is among the tracked ones. 
    // If it is mark the associated parameter as being read.
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            trackedOps[i].ParameterVariable->SetIsNoRead(false);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkWrittenParameters(Operand* op, TrackedOperandsList& trackedOps) {
    // Check if the operand is among the tracked ones. 
    // If it is mark the associated parameter as being written.
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            trackedOps[i].ParameterVariable->SetIsNoWrite(false);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkReadWrittenParameters(Operand* op, TrackedOperandsList& trackedOps) {
    // Check if the operand is among the tracked ones. 
    // If it is mark the associated parameter as being both read and written.
    for(int i = 0; i < trackedOps.Count(); i++) {
        if(trackedOps[i].TrackedOperand == op) {
            auto parameterVariable = trackedOps[i].ParameterVariable;
            parameterVariable->SetIsNoRead(false);
            parameterVariable->SetIsNoWrite(false);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::
     MarkReadParameters(Instruction* instr, TrackedParametersList& trackedParams) {
    // Mark any used non-pointer parameter as being read.
    for(int i = 0; i < instr->SourceOpCount(); i++) {
        if(auto parameter = instr->GetSourceOp(i)->As<Parameter>()) {
            if(trackedParams.Contains(parameter)) {
                parameter->GetVariable()->SetIsNoRead(false);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterFlags::EscapedParametersVisitor::Dump(Function* function) {
    string title = "Modified parameters in " + *function->Name();
    string params;

    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsPointer()) {
            if(parameterVariable->HasName()) {
                params += *parameterVariable->Name();
            }
            else {
                params += string::Format(L"Parameter %d", i);
            }

            params += (parameterVariable->IsEscape() ? ": escaped, " : ": not escaped, ");
            params += (parameterVariable->IsRead() ? ": read, " : ": not read, ");
            params += (parameterVariable->IsWrite() ? ": written, " : ": not written, ");
            params += "\n";
        }
    }

    ObjectDumper(params, title).Dump();
}

} // namespace Analysis