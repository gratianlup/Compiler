// GlobalSideEffects.hpp
// Copyright (c) Lup Gratian
//
// Implements the GlobalSideEffects pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "GlobalSideEffects.hpp"

namespace Analysis {

void GlobalSideEffects::Execute(CallGraph* callGraph) {
    local<NoStateVisitor> visitor = new NoStateVisitor(this);
    callGraph->FindRoots(true);
    callGraph->ReverseInvocationTraversalFromRoots(visitor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     Visit(CallNode* node, CallGraph* callGraph) {
    // Ignore the External and Unknown nodes
    // and functions without a body.
    auto function = GetFunctionDefinition(node);

    if(function == nullptr) {
        return true;
    }

    // Presume the function doesn't depend on a global state
    // and try to prove it otherwise by analyzing the instructions
    // that can have a side-effect.
    function->SetIsNoState(true);
    CreateGlobalEffectsTag(function);

	// Presume the function doesn't access indirect memory
	// (through an indirect pointer like '*a->b = 3', where 'b' is a pointer).
	function->SetIsNoIndirectRead(true);
	function->SetIsNoIndirectWrite(true);

    ProcessParametersAndVariables(function);
	ProcessInstructions(function, callGraph);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) {
    // If any of the functions in the group depends on the global state
    // we mark all the other ones as depending too.
    bool dependsOnState = false;

    for(int i = 0; i < nodeGroup->NodeCount(); i++) {
        auto node = nodeGroup->GetNode(i);

        if(node->IsNodeGroup()) {
            dependsOnState = true;
            continue;
        }

        auto callNode = static_cast<CallNode*>(node);
        Visit(callNode, callGraph);

        if(auto function = callNode->GetFunction()) {
            if(function->IsNoState() == false) {
                dependsOnState = true;
                break;
            }
        }
    }

    // Mark all functions as being dependent on the global state.
    if(dependsOnState) {
        MarkAllStateDependent(nodeGroup);
    }

    // The global variable accessed by each function
    // is the union of all variables accessed by the group.
    MergeAllGlobalEffects(nodeGroup);

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffects::NoStateVisitor::
     MarkAllStateDependent(CallNodeGroup* nodeGroup) {
    // As a conservative measure all functions in the group
    // are marked as being state dependent if at least one is.
    for(int i = 0; i < nodeGroup->NodeCount(); i++) {
        auto node = nodeGroup->GetNode(i);

        if(node->IsNodeGroup() == false) {
            auto callNode = static_cast<CallNode*>(node);        

            if(auto function = callNode->GetFunction()) {
                function->SetIsNoState(false);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffects::NoStateVisitor::
     MergeAllGlobalEffects(CallNodeGroup* nodeGroup) {
    // As a conservative measure the global variables accessed
    // by a function is the union of all variables accessed by the group.
    for(int i = 0; i < nodeGroup->NodeCount(); i++) {
        auto nodeA = nodeGroup->GetNode(i);

        if(nodeA->IsNodeGroup()) {
            continue;
        }

        for(int j = 0; j < nodeGroup->NodeCount(); j++) {
            if(i == j) {
                continue;
            }

            auto nodeB = nodeGroup->GetNode(j);

            if(nodeB->IsNodeGroup()) {
                continue;
            }

            auto functionA = static_cast<CallNode*>(nodeA)->GetFunction();
            auto functionB = static_cast<CallNode*>(nodeB)->GetFunction();
            MergeGlobalUses(functionA, functionB);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     ProcessParametersAndVariables(Function* function) {
    // Parameters which escape the function can't be
    // tracked and may be modified in unknown ways.
    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsAddressTaken() &&
           parameterVariable->IsEscape()) {
            function->SetIsNoState(false);
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     ProcessInstructions(Function* function, CallGraph* callGraph) {
    // Process each instruction and check if it can
    // alter the global state of the program.
    // We stop as soon as one of the instructions might modify the state.
    auto instructionEnum = function->GetInstructionEnum();

    while(instructionEnum.IsValid()) {
        auto instr = instructionEnum.Next();

        if(auto storeInstr = instr->As<StoreInstr>()) {
            if(ProcessStore(storeInstr) == false) {
                function->SetIsNoState(false);
            }
        }
        else if(auto loadInstr = instr->As<LoadInstr>()) {
            if(ProcessLoad(loadInstr) == false) {
                function->SetIsNoState(false);
            }
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            if(ProcessCall(callInstr, callGraph) == false) {
                function->SetIsNoState(false);
            }
        }
        else {
            // Any other instruction that has a global variable reference
            // as a source operand forces us to mark the variable
            // as read and written in this function (this could be improved).
            for(int i = 0; i < instr->SourceOpCount(); i++) {
                auto variableRef = instr->GetSourceOp(i)->As<VariableReference>();

                if((variableRef == nullptr) ||
                   (variableRef->IsGlobalVariableRef() == false)) {
                    continue;
                }

                MarkGlobalUse(variableRef, function);
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::ProcessLoad(LoadInstr* instr) {
    // A read value is always the same if it is from a local variable,
    // from a parameter or from a global variable marked as constant.
    // Note that address-taken variables are considered too, because 
    // the fact that they escape/are modified indirectly is detected later.
    if(instr->SourceOp()->IsLocalVariableRef() ||
       instr->SourceOp()->IsParameter()) {
        return true;
    }

    // Verify each variable/parameter that acts as the base operand
    // of a (potentially) series of addressing instructions.
	bool isIndirect = false;

    if(HasOnlyAllowedTargets(instr->SourceOp(), isIndirect, 
							 true /* allowGlobalConstants */)) {
		// Mark the indirect read, if it's the case.
		if(isIndirect) {
			instr->ParentFunction()->SetIsNoIndirectRead(false);
		}

        return true;
    }

    // Mark any global variable that might be read.
    OperandList baseOperands;

    if(CollectBaseOperands(instr->SourceOp(), baseOperands, isIndirect)) {
        for(int i = 0; i < baseOperands.Count(); i++) {
            auto variableRef = baseOperands[i]->As<VariableReference>();

            if(variableRef && variableRef->IsGlobalVariableRef()) {
                MarkGlobalUse(variableRef, instr->ParentFunction(),
                              true /* isRead */, false /* isWritten */);
            }
        }
    }

	// Mark the indirect read, if it's the case.
	if(isIndirect) {
		instr->ParentFunction()->SetIsNoIndirectRead(false);
	}

    // Don't process functions with volatile loads.
    return instr->IsVolatile() == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::ProcessStore(StoreInstr* instr) {
    // A store doesn't modify the global state if it is 
    // inside a local variable or if the position of the written 
	// global variable is not read anywhere.
    if(instr->DestinationOp()->IsLocalVariableRef()) {
        return true;
    }

    // Verify each variable/parameter that acts as the base operand
    // of a (potentially) series of addressing instructions.
    OperandList baseOperands;
    bool onlyWritesToDeadGlobals = true;
	bool writesParams = false;
	bool isIndirect = false;

	if(CollectBaseOperands(instr->DestinationOp(), baseOperands, isIndirect) == false) {
		// Mark the indirect write, if it's the case.
		if(isIndirect) {
			instr->ParentFunction()->SetIsNoIndirectWrite(false);
		}

        return false;
    }

    for(int i = 0; i < baseOperands.Count(); i++) {
        auto baseOp = baseOperands[i];

        if(baseOp->IsGlobalVariableRef()) {
            // Check if this is a write to a never read global variable.
            // (or a single-dimension array/record position).
            if(IsDeadGlobalStore(baseOp, instr)) {
                continue;
            }
            else if(auto variableRef = baseOp->As<VariableReference>()) {
                // Mark the fact the the function writes a global variable.
                MarkGlobalUse(variableRef, instr->ParentFunction(),
                              false /* isRead */, true /* isWritten */,
                              parent_->GetSafetyInfo()->IsAlwaysExecuted(instr));
            }

            onlyWritesToDeadGlobals = false;
        }
		else if(baseOp->IsParameter()) {
			writesParams = true;
		}
    }

	// Mark the indirect write, if it's the case.
	if(isIndirect) {
		instr->ParentFunction()->SetIsNoIndirectWrite(false);
	}

    // Don't process functions with volatile stores.
    return (onlyWritesToDeadGlobals == false) &&
		   (instr->IsVolatile() == false)     &&
		   (writesParams == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     IsDeadGlobalStore(Operand* destinationOp, StoreInstr* instr) {
    // If the store targets a position from a global variable 
    // that is never read in the entire program the store is dead
    // (it should be removed later by the Dead Code Elimination pass).
    auto variableRef = destinationOp->As<VariableReference>();

    if((variableRef == nullptr) || 
       (variableRef->IsGlobalVariableRef() == false)) {
        return false;
    }

    // A tag describing the users should be attached.
    auto globalVariable = variableRef->GetGlobalVariable();
    auto usersTag = globalVariable->GetTag<GlobalUsersTag>();

    if((usersTag == nullptr) || usersTag->HasUnknownPositionRead()) {
        return false;
    }

    // If the store is into a pointer check if there are any reads.
    // For arrays/records check if the single-dimension index
    // is a known constant.
    __int64 index;

    if(instr->DestinationOp() == variableRef) {
        return usersTag->HasNoReads();
    }
    else if(GetSingleIndex(instr->DestinationOp(), destinationOp, index)) {
        return usersTag->HasReadOnPosition(index) == false;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     GetSingleIndex(Operand* op, Operand* requiredBase, __int64& index) {
    // Check if the operand is an 'index'/'field' instruction
    // for a single-dimension array with a constant index.
    if(auto indexInstr = op->DefiningInstrAs<IndexInstr>()) {
        if((indexInstr->BaseOp() == requiredBase) &&
            indexInstr->IndexOp()->IsIntConstant()) {
            auto intConstant = indexInstr->IndexOp()->As<IntConstant>();

            if(intConstant->Value() >= 0) {
                index = intConstant->Value();
                return true;
            }
        }
    }
    else if(auto fieldInstr = op->DefiningInstrAs<FieldInstr>()) {
        if(fieldInstr->BaseOp() == requiredBase) {
            index = fieldInstr->GetFieldIndex();
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     ProcessCall(CallInstr* instr, CallGraph* callGraph) {
    // If any of the functions called at this call site depends/modifies 
    // the global state this one does too. Calls to the Unknown node 
    // are considered to modify the state, while for calls to the External 
    // node we try to prove they do not using intrinsics and language information.
    auto callSite = callGraph->GetCallSite(instr);
    bool callHasState = false;

    if(callSite->CallsUnknownFunctions()) {
        return false;
        MarkGlobalUnknownEffects(instr->ParentFunction());
    }

    // Remember which variables are always written,
    // because this information is lost when merging the results
    // of all possible called functions.
    StaticList<int, 2> alwaysWrittenVars;
    SaveAlwaysWrittenVariables(instr->ParentFunction(), alwaysWrittenVars);

    for(int i = 0; i < callSite->CalledFunctionsCount(); i++) {
        auto node = callSite->GetCalledNode(i);

        // For node groups we are conservative and consider 
        // this function modifies the global state if any
        // function in the group does it too.
        if(node->IsNodeGroup()) {
            auto nodeGroup = static_cast<CallNodeGroup*>(node);

            if(NodeGroupHasState(nodeGroup)) {
                callHasState = true;
            }
            else continue;
        }

        // If the node represents the External node we skip it
        // because the external function was already processed
        // or will be processed next.
        auto callNode = static_cast<CallNode*>(node);

        if(callNode->IsExternalNode()) {
            continue;
        }

        // If the function has a definition it was already 
        // processed and can use its state information.
        auto calledFunction = callNode->GetFunction();
		bool isIndirect;

        if(calledFunction->IsDefinition()) {
            // Mark the global variables that are passed as parameters,
            // then merge the global variable used by the callee.
            MarkParameterGlobalUses(instr, calledFunction);
            MergeGlobalUses(instr->ParentFunction(), calledFunction);

            if(calledFunction->IsNoState() == false) {
                callHasState = true;
            }

			// If the called function has indirect reads/writes
			// this one must be marked as having too.
			if(calledFunction->IsIndirectRead()) {
				instr->ParentFunction()->SetIsNoIndirectRead(false);
			}

			if(calledFunction->IsIndirectWrite()) {
				instr->ParentFunction()->SetIsNoIndirectWrite(false);
			}
        }
        else if(ExternalFunctionHasState(calledFunction, instr, isIndirect) == false) {
			// Mark the fact that indirect memory access might happen.
			if(isIndirect) {
				instr->ParentFunction()->SetIsNoIndirectRead(false);
				instr->ParentFunction()->SetIsNoIndirectWrite(false);
			}

            continue;
        }
        else {
            MarkGlobalUnknownEffects(instr->ParentFunction());
            callHasState = true;
        }
    }

    // Restore the flag for the always written variables.
    RestoreAlwaysWrittenVariables(instr->ParentFunction(), alwaysWrittenVars);
    return callHasState == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     ExternalFunctionHasState(Function* function, CallInstr* instr, bool& isIndirect) {
    // Mark the global variables that are used as parameters.
    MarkParameterGlobalUses(instr, function);
	isIndirect = false;

    // For intrinsics and some external functions from the standard
    // library we known if they depend on the global state.
    if(auto intrinsic = instr->GetIntrinsic()) {
        // Only 'setMemory' and 'copyMemory' intrinsics can modify
        // the global state (note that 'prefetch' is just a hint).
        if(intrinsic->IsMathIntrinsic()        ||
           intrinsic->IsBitwiseIntrinsic()     ||
           intrinsic->IsBoundsCheckIntrinsic() ||
           intrinsic->Is<PrefetchIntr>()) {
            return false;
        }

        // For 'setMemory'/'copyMemory' there is no state dependency
        // if the destination is a local variable or a parameter.
        // For 'copyMemory' we also require that the source
        // is a local variable, parameter or constant global variable.
        if(intrinsic->Is<SetMemoryIntr>() || intrinsic->Is<CopyMemoryIntr>()) {
            if(HasOnlyAllowedTargets(SetMemoryIntr::GetDestination(instr),
                                     isIndirect, false /* allowConsts */) == false) {
                return true;
            }

            if(intrinsic->Is<CopyMemoryIntr>() &&
               HasOnlyAllowedTargets(CopyMemoryIntr::GetSource(instr),
                                     isIndirect, true /* allowConsts */) == false) {
                return true;
            }

            return false;
        }
    }
    else if(auto languageInfo = parent_->GetLanguageInfo()) {
        return languageInfo->CallMayHaveSideEffects(function, &isIndirect);
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     HasOnlyAllowedTargets(Operand* op, bool& isIndirect, bool allowGlobalConstants) {
    // Fast check for the most frequent valid cases.
    if(op->IsLocalVariableRef() || op->IsParameter()) {
		isIndirect = false;
        return true;
    }

    // Check if multiple operands are involved
    // because of 'phi'/'quest' instructions.
    OperandList baseOperands;
	isIndirect = false;

    if(CollectBaseOperands(op, baseOperands, isIndirect) == false) {
        return false;
    }

    for(int i = 0; i < baseOperands.Count(); i++) {
        auto baseOp = baseOperands[i];

        if((baseOp->IsLocalVariableRef() || baseOp->IsParameter()) == false) {
            // Check if this is a read from a global constant variable.
            if(allowGlobalConstants && IsReadOfConstant(baseOp)) {
                continue;
            }

            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::IsReadOfConstant(Operand* op) {
    // Check if the load is from a constant global variable.
    auto variableRef = op->As<VariableReference>();

    if((variableRef == nullptr) || 
       (variableRef->IsGlobalVariableRef() == false)) {
        return false;
    }

    return variableRef->GetGlobalVariable()->IsConstant();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::NodeGroupHasState(CallNodeGroup* group) {
    // Check if any function depends on the global state.
    for(int i = 0; i < group->NodeCount(); i++) {
        auto node = group->GetNode(i);

        if(node->IsNodeGroup()) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalSideEffects::NoStateVisitor::
     CollectBaseOperands(Operand* op, OperandList& list, 
						 bool& isIndirect, int level) {
    // Find all operands that act as the base of a series
    // of addressing instructions, looking through 'phi'/'quest'.
    // If we're too deep in the recursion it's unlikely to find something useful.
    if(level > 6) {
		// We need to be conservative and presume that an
		// indirect access might happen in the unexplored path.
		isIndirect = true;
        return false;
    }

    if(op->HasDefiningInstruction() == false) {
        // Found a base operand, make sure it's not added twice.
        if(list.Contains(op) == false) {
            list.Add(op);
        }

        return true;
    }
    else if(op->DefiningInstrIs<IndexInstr>()   ||
            op->DefiningInstrIs<AddressInstr>() ||
            op->DefiningInstrIs<FieldInstr>() ||
            op->DefiningInstrIs<PtopInstr>()) {
        return CollectBaseOperands(op->DefiningInstruction()->GetSourceOp(0),
                                   list, isIndirect, level + 1);
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        // If we have too many incoming operands give up,
        // it's unlikely to find something useful in each of them.
        if(phiInstr->OperandCount() > 8) {
            return false;
        }

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(CollectBaseOperands(phiInstr->GetOperand(i), 
                                   list, isIndirect, level + 1) == false) {
                return false;
            }
        }

        return true;
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        return CollectBaseOperands(questInstr->TrueOp(), list, isIndirect, level + 1) &&
               CollectBaseOperands(questInstr->FalseOp(), list, isIndirect, level + 1);
    }
	else if(auto loadInstr = op->DefiningInstrAs<LoadInstr>()) {
		// If the operand originates from a 'load'
		// mark the fact that an indirect access is done.
		isIndirect = true;
		return false; // Not a valid base operand though.
	}
    
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* GlobalSideEffects::NoStateVisitor::
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
void GlobalSideEffects::NoStateVisitor::
     CreateGlobalEffectsTag(Function* function) {
    if(function->HasTag<GlobalSideEffectsTag>() == false) {
        function->AddTag(GlobalSideEffectsTag::GetGlobalSideEffects());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffects::NoStateVisitor::
     MarkGlobalUse(VariableReference* variableRef, Function* function, 
                   bool isRead, bool isWritten, bool isAlwaysWritten) {
    DebugValidator::IsNotNull(variableRef);
    DebugValidator::IsTrue(variableRef->IsGlobalVariableRef());

    // Mark the fact that the global variable is used
    // (read, written or both) by the specified function.
    // Create the tag now if it isn't created already.
    auto globalSideEffects = function->GetTag<GlobalSideEffectsTag>();
    DebugValidator::IsNotNull(globalSideEffects);

    auto globalVariable = variableRef->GetGlobalVariable();
    globalSideEffects->AddVariable(globalVariable, isRead, 
                                   isWritten, isAlwaysWritten);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffects::NoStateVisitor::
     MarkGlobalUnknownEffects(Function* function) {
    auto globalSideEffects = function->GetTag<GlobalSideEffectsTag>();
    DebugValidator::IsNotNull(globalSideEffects);
    globalSideEffects->MarkHasUnknownEffects();
	
	// Mark the fact that indirect memory access might happen.
	function->SetIsNoIndirectRead(false);
	function->SetIsNoIndirectWrite(false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffects::NoStateVisitor::
     MarkParameterGlobalUses(CallInstr* instr, Function* calledFunction) {
    // Mark any global variable that is used as a parameter
    // as being read/written in the function (this depends on
    // the 'noread'/'nowrite' flags set on the parameters).
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        auto argument = instr->GetArgument(i);
        OperandList baseOperands;
		bool isIndirect = false;

        CollectBaseOperands(argument, baseOperands, isIndirect);

        for(int i = 0; i < baseOperands.Count(); i++) {
            auto variableRef = baseOperands[i]->As<VariableReference>();

            if(variableRef && variableRef->IsGlobalVariableRef()) {
                auto parameter = calledFunction->GetParameterVariable(i);

                MarkGlobalUse(variableRef, instr->ParentFunction(),
                              parameter->IsRead(), parameter->IsWrite());
            }
        }

		if(isIndirect) {
			instr->ParentFunction()->SetIsNoIndirectRead(false);
			instr->ParentFunction()->SetIsNoIndirectWrite(false);
		}
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffects::NoStateVisitor::
     MergeGlobalUses(Function* function, Function* calledFunction) {
    auto globalSideEffectsA = function->GetTag<GlobalSideEffectsTag>();
    auto globalSideEffectsB = calledFunction->GetTag<GlobalSideEffectsTag>();

    if(globalSideEffectsB == nullptr) {
        globalSideEffectsA->MarkHasUnknownEffects();
    }
    else globalSideEffectsA->Merge(globalSideEffectsB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffects::NoStateVisitor::
     SaveAlwaysWrittenVariables(Function* function, StaticList<int, 2>& variables) {
    auto globalSideEffects = function->GetTag<GlobalSideEffectsTag>();
    DebugValidator::IsNotNull(globalSideEffects);

    if(globalSideEffects->HasAlwaysWrittenVariables()) {
        globalSideEffects->ForEachAlwaysWrittenVariable(
                           [&variables](int index) -> bool {
            variables.Add(index);
            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalSideEffects::NoStateVisitor::
     RestoreAlwaysWrittenVariables(Function* function, StaticList<int, 2>& variables) {
    if(variables.Count() == 0) {
        return;
    }

    // Any variable that was marked as always written
    // before the merge has its flag restored, but only
    // if it wasn't marked as being read by the merge.
    auto globalSideEffects = function->GetTag<GlobalSideEffectsTag>();
    DebugValidator::IsNotNull(globalSideEffects);

    for(int i = 0; i < variables.Count(); i++) {
        if(globalSideEffects->DoesNotReadVariable(variables[i])) {
            globalSideEffects->SetIsAlwaysWritten(variables[i]);
        }
    }
}

} // namespace Analysis