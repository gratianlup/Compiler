// StaticPromotion.hpp
// Copyright (c) Lup Gratian
//
// Implements the StaticPromotion class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StaticPromotion.hpp"

namespace Optimization {

void StaticPromotion::Execute(Unit* unit, CallGraph* callGraph) {
    if(auto entryPoint = FindUniqueEntryPoint(unit)) {
        PromoteFunctionsToStatic(callGraph, entryPoint);
        PromoteVariablesToStatic(unit);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StaticPromotion::Execute(Unit* unit) {
    if(auto entryPoint = FindUniqueEntryPoint(unit)) {
        PromoteFunctionsToStatic(unit, entryPoint);
        PromoteVariablesToStatic(unit);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* StaticPromotion::FindUniqueEntryPoint(Unit* unit) {
    // If there is language information available
    // try to find the unique program entry point.
    auto languageInfo = GetLanguageInfo();

    if(languageInfo == nullptr) {
        return nullptr;
    }

    for(auto function = unit->Functions().First(); 
        function; function = function->Next) {
        // Declarations cannot be entry points.
        if(function->Value->IsDeclaration()) {
            continue;
        }

        if(languageInfo->IsProgramEntryPoint(function->Value)) {
            return function->Value;
        }
    }

    // No entry point found, this might be a library.
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StaticPromotion::PromoteFunctionsToStatic(CallGraph* callGraph, 
                                               Function* entryPoint) {
    // Mark all function in the unit as 'static' (internal),
    // except for the unique entry point which must remain external.
    auto& nodes = callGraph->GetCallNodes();
    
    for(int i = 0; i < nodes.Count(); i++) {
        auto callNode = static_cast<CallNode*>(nodes[i]);
        PromoteFunctionToStatic(callNode, entryPoint);
    }

    // Now do the same for node groups.
    auto& nodeGroups = callGraph->GetCallNodeGroups();

    for(int i = 0; i < nodeGroups.Count(); i++) {
        auto nodeGroup = nodeGroups[i];

        for(int j = 0; j < nodeGroup->NodeCount(); j++) {
            auto callNode = static_cast<CallNode*>(nodeGroup->GetNode(j));
            PromoteFunctionToStatic(callNode, entryPoint);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StaticPromotion::PromoteFunctionToStatic(CallNode* callNode, 
                                              Function* entryPoint) {
    // Mark the function as being 'static'. If it has a call
    // from the External node and its address is not taken
    // we can remove the call, allowing some optimizations to run
    // more effectively (interprocedural constant propagation, for example).
    auto function = callNode->GetFunction();

    if(function == entryPoint) {
        return;
    }

    // Mark is 'static' and update the Call Graph.
    function->SetVisibility(SymbolVisibility::Static);

    if(callNode->IsCalledByExternalFunctions() &&
       function->IsAddressNotTaken()) {
        callNode->RemoveCallFromExternal();
        ExternalCallNode::GetExternalNode()->RemoveCalledNode(callNode);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StaticPromotion::PromoteFunctionsToStatic(Unit* unit, Function* entryPoint) {
    unit->ForEachFunction([entryPoint](Function* function) -> bool {
        if(function != entryPoint) {
			function->SetVisibility(SymbolVisibility::Static);
        }

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StaticPromotion::PromoteVariablesToStatic(Unit* unit) {
    for(auto variable = unit->Variables().First(); variable;
        variable = variable->Next) {
        variable->Value->SetVisibility(SymbolVisibility::Static);
    }
}

} //namespace Optimization