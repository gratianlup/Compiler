// PointerParameterAlias.cpp
// Copyright (c) Lup Gratian
//
// Implements the PointerParameterAlias pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "PointerParameterAlias.hpp"

namespace Analysis {

void PointerParameterAlias::Execute(CallGraph* callGraph, AliasInfo* aliasInfo) {
    local<AliasVisitor> visitor = new AliasVisitor(aliasInfo, this);
    callGraph->FindRoots(true);
    callGraph->InvocationTraversalFromRoots(visitor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterAlias::AliasVisitor::
     Visit(CallNode* node, CallGraph* callGraph) {
    
    // Process each call site in the function determine the alias
    // between the pointer arguments and propagate the information
    // to all possibly called functions.
    auto function = GetFunctionDefinition(node);

    if(function == nullptr) {
        return false;
    }

	// If the function can be called by external functions
	// we need to presume the pointer parameters can point anywhere.
	if(node->IsCalledByExternalFunctions()) {
		MarkAllUnknown(function);
	}

    node->ForEachCallSite([this](CallSite* callSite) -> bool {
        // If the call instruction has more than a few arguments
        // it would take long to take all pairs, so instead
        // be very conservative and mark all parameters in the 
        // possible called functions as aliasing each other.
        auto callInstr = callSite->GetCallInstruction();

        if((callInstr->ArgumentCount() > 255) ||
           (PointerArgumentCount(callInstr) > MAX_POINTER_ARGUMENTS)) {
            MarkAllUnknown(callSite);
            return true;
        }

        for(int i = 0; i < callInstr->ArgumentCount(); i++) {
            auto argument = callInstr->GetArgument(i);

            if(argument->IsPointer()) {
                // We have a pointer argument; determine the alias
                // with all other pointer arguments, then propagate it.
                ParameterIndexList aliasedParameters;

                FindAliasedParameters(callInstr, argument, i, aliasedParameters);
                PropagateAliasedParameters(callSite, i, aliasedParameters);
            }
        }
        
        return true;
    });

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterAlias::AliasVisitor::
     Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) {
    // For functions in groups we are very conservative
    // and presume there is alias between each pointer.
    nodeGroup->ForEachNode([this, callGraph](CallNodeBase* node) -> bool {
        if(node->IsNodeGroup()) {
            return true;
        }

        auto callNode = static_cast<CallNode*>(node);
        auto function = GetFunctionDefinition(callNode);

        if(function) {
            MarkAllUnknown(function);
            Visit(callNode, callGraph);
        }

        return true;
    });

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* PointerParameterAlias::AliasVisitor::
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
ParameterAliasTag* PointerParameterAlias::AliasVisitor::
                   GetOrCreateTag(Variable* variable) {
    auto tag = variable->GetTag<ParameterAliasTag>();

    if(tag == nullptr) {
        // Create the tag now.
        tag = ParameterAliasTag::GetParameterAlias();
        variable->AddTag(tag);
    }

    return tag;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterAlias::AliasVisitor::
     FindAliasedParameters(CallInstr* instr, Operand* argument,
                           int argumentIndex, ParameterIndexList& parameters) {
    // Consider all pointer arguments used by the call.
    // If any of them aliases the the analyzed argument
    // add it to the list. Note that we do a very conservative
    // alias analysis that doesn't know the size of the access.
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        if(i == argumentIndex) {
            continue;
        }

        auto otherArgument = instr->GetArgument(i);

        if(otherArgument->IsPointer() &&
           aliasInfo_->HasAliasWithUnknownSize(argument, otherArgument)) {
            // There may be alias between the parameters.
            parameters.Add(i);
        }
    }

    return parameters.IsNotEmpty();
}
	
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterAlias::AliasVisitor::
     PropagateAliasedParameters(CallSite* callSite, int parameterIndex, 
                                ParameterIndexList& aliasedParameters) {
    // Merge the list of aliased parameters with the ones that
    // (might) have been already marked in each potentially called function.
    callSite->ForEachCalledFunction([&, this](Function* function) -> bool {
        if(function->IsDefinition() && 
           (parameterIndex < function->ParameterCount())) {
            // Note that there is no reason to add the indices
            // if the parameter has been already marked as aliasing all.
            auto variable = function->GetParameterVariable(parameterIndex);
            auto tag = GetOrCreateTag(variable);
            
            if(tag->AliasesAllParameters() == false) {
                // If the argument doesn't alias any other one mark the fact,
                // but only if there have been no aliases found already.
                if(aliasedParameters.IsEmpty()) {
                    if(tag->AliasesParameters() == false) {
                        tag->SetDoesNotAliasParameters(true);
                    }
                }
                else AddAliasedParameters(aliasedParameters, tag);
            }

			// Check if it is guaranteed that the argument doesn't originate 
			// from a global variable (helps alias analysis in some cases).
			auto callInstr = callSite->GetCallInstruction();
			auto argument = callInstr->GetArgument(parameterIndex);

			if(MayBeGlobalVariable(argument)) {
				tag->SetMayPointToGlobalVariables(true);
			}
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterAlias::AliasVisitor::MayBeGlobalVariable(Operand* op) {
	AllocHelper allocHelper(parent_->GetLanguageInfo());
	auto baseOp = allocHelper.GetBaseOperand(op);

	if(op->IsGlobalVariableRef()) {
		return true;
	}
	else return (op->IsLocalVariableRef() ||
				 allocHelper.OriginatesFromAlloc(baseOp)) == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterAlias::AliasVisitor::
     AddAliasedParameters(ParameterIndexList& parameters, ParameterAliasTag* tag) {
    // If the number of aliased parameters is too high
    // simply mark the parameter as aliasing all other ones.
    // (it's unlikely that keeping the list improves precision).
    if(parameters.Count() > MAX_PARAMETERS) {
        tag->SetAliasesAllParameters(true);
        return false;
    }

    for(int i = 0; i < parameters.Count(); i++) {
        tag->AddAliasedParameter(parameters[i]);
    }

    // The limit might have been exceeded because there already
    // were different aliased parameters in the list.
    if(tag->AliasedParameterCount() > 8) {
        tag->SetAliasesAllParameters(true);
        return false;
    }

    // The tag should no longer be marked
    // as having no alias with any other parameter.
    tag->SetDoesNotAliasParameters(false);
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterAlias::AliasVisitor::MarkAllUnknown(CallSite* callSite) {
    // Because the call has more than 255 arguments
    // we give up in analyzing the argument alias.
    auto callInstr = callSite->GetCallInstruction();

    for(int i = 0; i < callInstr->ArgumentCount(); i++) {
        if(callInstr->GetArgument(i)->IsPointer() == false) {
            continue;
        }

        // Mark the parameters of all possibly called functions
        // as aliasing each other.
        callSite->ForEachCalledFunction([this, i](Function* function) -> bool {
            if(function->IsDefinition() && 
               (i < function->ParameterCount())) {
                auto tag = GetOrCreateTag(function->GetParameterVariable(i));
                tag->SetAliasesAllParameters(true);
				tag->SetMayPointToGlobalVariables(true);
            }

            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterAlias::AliasVisitor::MarkAllUnknown(Function* function) {
    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsPointer()) {
            auto tag = GetOrCreateTag(parameterVariable);
            tag->SetAliasesAllParameters(true);
			tag->SetMayPointToGlobalVariables(true);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int PointerParameterAlias::AliasVisitor::PointerArgumentCount(CallInstr* instr) {
    int count = 0;

    instr->ForEachArgument([&count](Operand* argument, int index) -> bool {
        if(argument->IsPointer()) {
            count++;
        }

        return true;
    });

    return count;
}

} // namespace Analysis