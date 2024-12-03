// GlobalPromotion.hpp
// Copyright (c) Lup Gratian
//
// Implements the GlobalPromotion pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "GlobalPromotion.hpp"

namespace Optimization {

void GlobalPromotion::Execute(Unit* unit, CallGraph* callGraph, 
                              AliasInfo* aliasInfo) {
    callGraph_ = callGraph;
    aliasInfo_ = aliasInfo;
    unit_ = unit;

    // Find the global variables/functions that are used
    // as part of variable initializers (they must be marked
    // as being address-taken, for example).
    FindInitializerGlobals();

    // Process all function that have a definition.
    for(auto function = unit->Functions().First(); 
        function; function = function->Next) {
        if(function->Value->IsDeclaration()) {
            continue;
        }

        ProcessInstructions(function->Value);
    }

#if 0
    Dump();
#endif

    // Optimize using the gathered information.
    PromoteFunctionsToStatic();
    PromoteVariablesToConstants();
    RemoveCallsToUnknown();
    RemoveDeadGlobals();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessInstructions(Function* function) {
    // Process each instructions and check if the tracked
    // parameter/operands escape because one of them.
    TrackedReferencesDict trackedRefs;
    CFGInfo<Block, Function> cfgInfo(function, false /* edgeInfoNeeded */);
    auto& postorderList = cfgInfo.PostorderList();

    for(int i = postorderList.Count() - 1; i >= 0; i--) {
        auto block = const_cast<Block*>(postorderList[i]);

        block->ForEachInstruction([this, &trackedRefs](Instruction* instr) -> bool {
            if(auto loadInstr = instr->As<LoadInstr>()) {
                ProcessLoad(loadInstr, trackedRefs);
            }
            else if(auto storeInstr = instr->As<StoreInstr>()) {
                ProcessStore(storeInstr, trackedRefs);
            }
            else if(auto callInstr = instr->As<CallInstr>()) {
                ProcessCall(callInstr, trackedRefs);
            }
            else if(auto retInstr = instr->As<ReturnInstr>()) {
                ProcessReturn(retInstr, trackedRefs);
            }
            else if(auto ptopInstr = instr->As<PtopInstr>()) {
                TrackResultOperand(ptopInstr->ResultOp(), 
                                   ptopInstr->TargetOp(), trackedRefs);
            }
            else if(instr->IsAddressing()) {
                // 'index', 'addr' and 'elem'.
                TrackResultOperand(instr->GetDestinationOp(),
                                   instr->GetSourceOp(0), trackedRefs);
            }
            else if(auto cmpInstr = instr->As<CmpInstrBase>()) {
                ProcessCompare(cmpInstr, trackedRefs);
            }
            else if(auto phiInstr = instr->As<PhiInstr>()) {
                ProcessPhi(phiInstr, trackedRefs);
            }
            else if(auto questInstr = instr->As<QuestionInstr>()) {
                ProcessQuestion(questInstr, trackedRefs);
            }
            else {
                // Mark as address-taken any global used
                // as a source operand for all other instruction types.
                MarkWithUnknownEffect(instr, trackedRefs);
            }

            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::MarkWithUnknownEffect(Instruction* instr, 
                                            TrackedReferencesDict& trackedRefs) {
    // Mark as address-taken any global used
    // as a source operand for all other instruction types.
    for(int i = 0; i < instr->SourceOpCount(); i++) {
        GlobalInfoList globals;

        if(GetGlobalInfo(instr->GetSourceOp(i), trackedRefs, globals)) {
            for(int j = 0; j < globals.Count(); j++) {
                AddUser(globals[j], instr);
                MarkAddressTaken(globals[j]);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::TrackResultOperand(Operand* resultOp, Operand* sourceOp,
                                         TrackedReferencesDict& trackedRefs) {
    // Ignore instructions that have no users.
    if(resultOp == nullptr) {
        return;
    }

    // Any global associated with the source operand
    // is associated with the resulting one too.
    GlobalInfoList globals;

    if(GetGlobalInfo(sourceOp, trackedRefs, globals)) {
        if(trackedRefs.ContainsKey(resultOp) == false) {
            // This is the first time the operand is added.
            trackedRefs.Add(resultOp, globals);
        }
        else {
            // The operand is already tracked, combine the sets.
            auto& previousRefs = trackedRefs[resultOp];

            for(int i = 0; i < globals.Count(); i++) {
                auto global = globals[i];

                if(previousRefs.Contains(global)) {
                    previousRefs.Add(global);
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessLoad(LoadInstr* instr, TrackedReferencesDict& trackedRefs) {
    // We're interested only in global variables.
    GlobalInfoList sourceGlobals;

    if(GetGlobalInfo(instr->SourceOp(), trackedRefs, sourceGlobals)) {
        for(int i = 0; i < sourceGlobals.Count(); i++) {
            auto global = sourceGlobals[i];
            global->HasRead = true;

            AddUser(global, instr);
            AddReadUser(global, instr);

            // If the global is a single-dimension array
            // mark the index of the read element.
            AccessPath path;

            if(FindAccessPath(instr->SourceOp(), path, global->TrackedReference)) {
                if(path.Indices.Count() == 1 && 
				   ((int)path.Indices[0] == path.Indices[0])) {
                    // We use a bitvector to mark the index.
                    global->ReadPositions.SetBit((int)path.Indices[0]);
					continue;
                }
            }

            // Mark the fact that we don't know exactly
            // which elements are read.
            global->HasUnknownPositionRead = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessStore(StoreInstr* instr, TrackedReferencesDict& trackedRefs) {
    // We're interested only in global variables.
    // If the source operand is stored we consider it's address taken.
    GlobalInfoList sourceGlobals;

    if(GetGlobalInfo(instr->SourceOp(), trackedRefs, sourceGlobals)) {
        for(int i = 0; i < sourceGlobals.Count(); i++) {
            AddUser(sourceGlobals[i], instr);
            MarkPassedToUnknown(sourceGlobals[i]);
        }
    }

    // Mark each destination global as being written,
    // and try to determine if it's initialized with constants.
    GlobalInfoList destGlobals;

    if(GetGlobalInfo(instr->DestinationOp(), trackedRefs, destGlobals)) {
        for(int i = 0; i < destGlobals.Count(); i++) {
            auto global = destGlobals[i];
            global->HasWrite = true;

            AddUser(global, instr);
            AddWriteUser(global, instr);
        }

        // Check if the stored value is a constant used to initialize 
        // the global. Sadly we can do this only if we have a single target.
        if(destGlobals.Count() > 1) {
            for(int i = 0; i < destGlobals.Count(); i++) {
                bool unkwnownPosition = destGlobals[i]->TrackedReference->IsArray() || 
                                        destGlobals[i]->TrackedReference->IsRecord();
                destGlobals[i]->HasNonConstantWrite = true;
                destGlobals[i]->HasUnknownPositionWrite = unkwnownPosition;
            }

            return;
        }

        ConstantList constants;
        bool hasOnlyConstants;
        auto destGlobal = destGlobals[0];

        if(FindConstants(instr->SourceOp(), constants, hasOnlyConstants)) {
            AccessPath path;

            if(FindAccessPath(instr->DestinationOp(), path, 
                              destGlobal->TrackedReference)) {
                AddInitializers(destGlobal, path, constants, instr);
                destGlobal->HasNonConstantWrite |= hasOnlyConstants == false;
            }
            else {
                // Mark the fact that a write with variable index targets 
                // the global (it can't be converted to a constant, for example).
                destGlobal->HasUnknownPositionWrite = true;
                destGlobal->HasNonConstantWrite |= hasOnlyConstants == false;
            }
        }
        else {
            bool unkwnownPosition = destGlobal->TrackedReference->IsArray() || 
                                    destGlobal->TrackedReference->IsRecord();
            destGlobal->HasNonConstantWrite = true;
            destGlobal->HasUnknownPositionWrite = unkwnownPosition;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::FindConstants(Operand* op, ConstantList& constants,
                                    bool& hasOnlyConstants) {
    // We consider the obvious case of a constant operand,
    // but also 'phi' and 'quest' instructions with constants.
    if(auto constant = op->As<Constant>()) {
        constants.Add(ConstantTimesPair(constant, 1));
        hasOnlyConstants = true;
        return true;
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        int constantOps = 0;

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(auto constant = phiInstr->GetSourceOp(i)->As<Constant>()) {
                constantOps++;
                AddConstant(constant, constants);
            }
        }

        hasOnlyConstants = constantOps == phiInstr->OperandCount();
        return constantOps > 0;
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        bool trueIsConstant = false;
        bool falseIsConstant = false;

        if(auto constant = questInstr->TrueOp()->As<Constant>()) {
            trueIsConstant = true;
            AddConstant(constant, constants);
        }

        if(auto constant = questInstr->FalseOp()->As<Constant>()) {
            falseIsConstant = true;
            AddConstant(constant, constants);
        }

        hasOnlyConstants = trueIsConstant && falseIsConstant;
        return trueIsConstant || falseIsConstant;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::AddConstant(Constant* constant, ConstantList& constants) {
    for(int i = 0; i < constants.Count(); i++) {
        if(constants[i].Value == constant) {
			// Count how many times the constant appears.
            constants[i].Times++;
            return;
        }
    }

	// This is the first time the constant is added.
    constants.Add(ConstantTimesPair(constant, 1));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::FindAccessPath(Operand* op, AccessPath& path,
                                     Reference* requiredBase) {
    if(op->HasDefiningInstruction() == false) {
        // Reached the base operand, make sure it's the expected one.
        return op == requiredBase;
    }
    
    // Consider 'index', 'addr' and 'field' instructions only.
    auto definingInstr = op->DefiningInstruction();

    if(definingInstr->IsIndex() || definingInstr->IsAddress()) {
        // Only constant indices are valid.
        if(auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>()) {
			// Only integers that fit in 32 bits are accepted.
			if((int)intConst->Value() != intConst->Value()) {
				return false;
			}

            if(FindAccessPath(definingInstr->GetSourceOp(0), path, requiredBase)) {
                path.Indices.Add((int)intConst->Value());
                return true;
            }
        }
    }
    else if(definingInstr->IsField()) {
        if(FindAccessPath(definingInstr->GetSourceOp(0), path, requiredBase)) {
            auto intConst = definingInstr->GetSourceOp(1)->As<IntConstant>();
            path.Indices.Add((int)intConst->Value());
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::AddInitializers(GlobalInfo* globalInfo, AccessPath& path, 
                                      ConstantList& constants, StoreInstr* store) {
    PathConstantsPair* pathConstants = nullptr;

    // Check if we already have initializers for the access path.
    for(int i = 0; i < globalInfo->Initializers.Count(); i++) {
        if(globalInfo->Initializers[i].Path == path) {
            pathConstants = &globalInfo->Initializers[i];
            break;
        }
    }

    // Create the path/constant list pair if required.
    if(pathConstants == nullptr) {
        globalInfo->Initializers.Add(PathConstantsPair(path, store));
        pathConstants = &globalInfo->Initializers.PeekLast();
    }
    else if(pathConstants->Stores.Contains(store) == false) {
        pathConstants->Stores.Add(store);
    }

    // Add the constants, making sure no duplicates appear.
    for(int i = 0; i < constants.Count(); i++) {
        auto constant = constants[i];
        int j;

        for(j = 0; j < pathConstants->Constants.Count(); j++) {
            if(pathConstants->Constants[j].Value == constant.Value) {
                pathConstants->Constants[j].Times += constant.Times;
                break;
            }
        }

        // Added the first time.
        if(j == pathConstants->Constants.Count()) {
            pathConstants->Constants.Add(constant);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessReturn(ReturnInstr* instr, 
                                    TrackedReferencesDict& trackedRefs) {
    if(instr->IsVoid()) {
        return;
    }

    // Any returned variable is marked as address-taken,
    // non-constant and having writes at unknown positions.
    GlobalInfoList returnedGlobals;

    if(GetGlobalInfo(instr->ReturnedOp(), trackedRefs, returnedGlobals)) {
        for(int i = 0; i < returnedGlobals.Count(); i++) {
            AddUser(returnedGlobals[i], instr);
            MarkPassedToUnknown(returnedGlobals[i]);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessCall(CallInstr* instr, 
                                  TrackedReferencesDict& trackedRefs) {
    GlobalInfoList calledGlobals;

    if(GetGlobalInfo(instr->TargetOp(), trackedRefs, calledGlobals)) {
        for(int i = 0; i < calledGlobals.Count(); i++) {
            AddUser(calledGlobals[i], instr);
        }
    }

    for(int i = 0; i < instr->ArgumentCount(); i++) {
        // Check only pointer arguments. Globals that were
        // converted to other types are already marked as escaped.
        auto argument = instr->GetArgument(i);

        if(argument->IsPointer() == false) {
            continue;
        }

        // Get the list of globals associated with the argument.
        GlobalInfoList argumentGlobals;

        if(GetGlobalInfo(argument, trackedRefs, argumentGlobals) == false) {
            // Skip this argument, it's not tracked.
            continue; 
        }

        // Using the Call Graph check what happens with
        // the pointer in each of the potentially called functions.
        // If the Unknown node is called we need to presume
        // the global is read/written/external.
        auto callSite = callGraph_->GetCallSite(instr);

        if(callSite->CallsUnknownFunctions()) {
            for(int i = 0; i < argumentGlobals.Count(); i++) {
                AddUser(argumentGlobals[i], instr);
                MarkPassedToUnknown(argumentGlobals[i]);
            }

            continue;
        }
        
        bool isRead;
        bool isWritten;
        bool isEscaped;
        DetermineSideEffects(callSite, instr, argumentGlobals, i,
                             isRead, isWritten, isEscaped, trackedRefs);

        // No matter in what way the argument may be modified, it will 
        // be marked as address taken, but marked read/write only if 
        // the corresponding flags are set. An exception is when the argument 
        // may escape, because we don't know what might happen with it.
        for(int j = 0; j < argumentGlobals.Count(); j++) {
            AddUser(argumentGlobals[j], instr);
            MarkArgumentGlobal(argumentGlobals[j], isRead, isWritten, isEscaped);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DetermineSideEffects(CallSite* callSite, CallInstr* instr, 
                                           GlobalInfoList& argumentGlobals,
                                           int parameterIndex, bool& isRead, 
                                           bool& isWritten, bool& isEscaped,
                                           TrackedReferencesDict& trackedRefs) {
    // Presume the pointer argument isn't touched at all.
    // We stop as soon as the escaped flag is set.
    isRead = false;
    isWritten = false;
    isEscaped = false;

    // Process each potentially called function.
    for(int i = 0; i < callSite->CalledFunctionsCount(); i++) {
        // If the called node is a group of nodes
        // we consider their worst unified effect.
        auto calledNode = callSite->GetCalledNode(i);

        if(calledNode->IsNodeGroup()) {
            DetermineGroupSideEffects(static_cast<CallNodeGroup*>(calledNode),
                                      isRead, isWritten, isEscaped);
            if(isEscaped) {
                return; // Escaped implies read and written.
            }
            else continue;
        }

        // Skip the External node, the function that is external
        // was/will be processed.
        if(calledNode->IsExternalNode()) {
            continue;
        }

        // If the called function is a definition (has a body)
        // we can use the flags from the parameter.
        // Functions that are only declared are considered
        // to modify/escape the pointer, unless we can prove
        // otherwise using intrinsic/language information.
        auto callNode = static_cast<CallNode*>(calledNode);
        auto function = callNode->GetFunction();

        if(function->IsDefinition()) {
            auto parameterVariable = function->GetParameterVariable(parameterIndex);
            isRead |= parameterVariable->IsRead();
            isWritten |= parameterVariable->IsWrite();
            isEscaped |= parameterVariable->IsEscape();
        }
        else if(DetermineSideEffectsExternal(callSite, instr, argumentGlobals,
                                             parameterIndex, isRead, isWritten, 
                                             isEscaped, trackedRefs) == false) {
            // We don't know anything about the external function.
            isRead = isWritten = isEscaped = true;
            return;
        }

        if(isEscaped) {
            return; // Escaped implies read and written.
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::DetermineSideEffectsExternal(CallSite* callSite, CallInstr* instr, 
                                                   GlobalInfoList& argumentGlobals, 
                                                   int parameterIndex, bool& isRead, 
                                                   bool& isWritten, bool& isEscaped,
                                                   TrackedReferencesDict& trackedRefs) {
    // We handle memory intrinsics and calls to known
    // standard library functions using the language interface.
    if(auto intrinsic = instr->GetIntrinsic()) {
        if(intrinsic->Is<PrefetchIntr>()) {
            // Prefetch is considered to not affect memory.
            return true;
        }
        else if(intrinsic->Is<SetMemoryIntr>()) {
            DetermineSetMemorySideEffects(instr, argumentGlobals, parameterIndex,
                                          isRead, isWritten, isEscaped, trackedRefs);
            return true;
        }
        else if(intrinsic->Is<CopyMemoryIntr>()) {
            DetermineCopyMemorySideEffects(instr, argumentGlobals, parameterIndex,
                                           isRead, isWritten, isEscaped, trackedRefs);
            return true;
        }
    }
    else if(auto languageInfo = GetLanguageInfo()) {
        // Test if the external function is part of the standard library
        // and if it can read/write/escape any of the arguments.
        auto function = callSite->GetFunction();

        for(int i = 0; i < argumentGlobals.Count(); i++) {
            auto global = argumentGlobals[i]->TrackedReference;

            if(isRead == false) {
                isRead |= languageInfo->CallMayReadFromAddress(instr, global, this);
            }

            if(isWritten == false) {
                isWritten |= languageInfo->CallMayWriteToAddress(instr, global, this);
            }

            if(isEscaped == false) {
                isEscaped |= languageInfo->CallMayCaptureParameter(function, instr,
                                                                   parameterIndex);
            }
            else break; // Escaped implies read and written.
        }

        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DetermineSetMemorySideEffects(CallInstr* instr, 
                                                    GlobalInfoList& argumentGlobals, 
                                                    int parameterIndex, bool& isRead, 
                                                    bool& isWritten, bool& isEscaped,
                                                    TrackedReferencesDict& trackedRefs) {
    // Any global that appears as the destination is marked written.
    for(int i = 0; i < argumentGlobals.Count(); i++) {
        auto global = argumentGlobals[i]->HasWrite = true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DetermineCopyMemorySideEffects(CallInstr* instr, 
                                                     GlobalInfoList& argumentGlobals, 
                                                     int parameterIndex, bool& isRead, 
                                                     bool& isWritten, bool& isEscaped,
                                                     TrackedReferencesDict& trackedRefs) {
    // Any global that appears as the destination is marked written.
    // Any global that appears as the source is marked read.
    // Only globals in the source list that alias with
    // destination operand are marked as written.
    for(int i = 0; i < argumentGlobals.Count(); i++) {
        if(parameterIndex == 1) {
            // The source reference might be read from.
            auto global = argumentGlobals[i];
            global->HasRead = true;
            
            auto source = global->TrackedReference;
            auto destination = CopyMemoryIntr::GetDestination(instr);

            if(MightBeAlias(source, destination)) {
                // The source reference might be written to,
                // and the destination reference might be read from.
                global->HasWrite = true;
                MarkDestinationRead(destination, trackedRefs);
            }
        }
        else {
            // The destination reference might be written to.
            argumentGlobals[i]->HasWrite = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::MarkDestinationRead(Operand* destinationOp,
                                          TrackedReferencesDict& trackedRefs) {
    GlobalInfoList destinationGlobals;
    GetGlobalInfo(destinationOp, trackedRefs, destinationGlobals);

    for(int i = 0; i < destinationGlobals.Count(); i++) {
        destinationGlobals[i]->HasRead = true;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DetermineGroupSideEffects(CallNodeGroup* nodeGroup, 
                                                bool& isRead, bool& isWritten, 
                                                bool& isEscaped) {
    // For groups we take a very conservative approach
    // and presume that external functions can modify/escape the argument.
    if(nodeGroup->CallsUnknownFunctions() ||
       nodeGroup->CallsExternalFunctions()) {
        isRead = isWritten = isEscaped = true;
        return;
    }

    for(int i = 0; i < nodeGroup->NodeCount(); i++) {
        // Nested node groups are not tested at all.
        auto childNode = nodeGroup->GetNode(i);

        if(childNode->IsNodeGroup()) {
            isRead = isWritten = isEscaped = true;
            return;
        }

        // Check each pointer parameter of the function
        // and unify their read/write/escape flags.
        auto callNode = static_cast<CallNode*>(childNode);
        auto function = callNode->GetFunction();

        for(int j = 0; j < function->ParameterCount(); j++) {
            auto parameterVariable = function->GetParameterVariable(j);

            if(parameterVariable->IsPointer()) {
                isRead |= parameterVariable->IsRead();
                isWritten |= parameterVariable->IsWrite();
                isEscaped |= parameterVariable->IsEscape();

                if(isEscaped) {
                    return; // Escaped implies read and written.
                }
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessCompare(CmpInstrBase* instr, 
                                     TrackedReferencesDict& trackedRefs) {
    // Any compared global is marked as address-taken.
    GlobalInfoList leftGlobals;
    GlobalInfoList rightGlobals;

    if(GetGlobalInfo(instr->LeftOp(), trackedRefs, leftGlobals)) {
        for(int i = 0; i < leftGlobals.Count(); i++) {
            AddUser(leftGlobals[i], instr);
            leftGlobals[i]->IsAddressTaken = true;
        }
    }

    if(GetGlobalInfo(instr->RightOp(), trackedRefs, rightGlobals)) {
        for(int i = 0; i < rightGlobals.Count(); i++) {
            AddUser(rightGlobals[i], instr);
            rightGlobals[i]->IsAddressTaken = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessPhi(PhiInstr* instr, TrackedReferencesDict& trackedRefs) {
    // Any incoming global is marked as address-taken
    // and we track it further through the 'phi' result operand.
    if(instr->ResultOp() && (instr->ResultOp()->IsPointer() == false)) {
        return;
    }

    for(int i = 0; i < instr->OperandCount(); i++) {
        auto incomingOp = instr->GetOperand(i);
        PatchLoopOperand(instr, incomingOp, trackedRefs);

        GlobalInfoList incomingGlobals;

        if(GetGlobalInfo(incomingOp, trackedRefs, incomingGlobals)) {
            for(int j = 0; j < incomingGlobals.Count(); j++) {
                AddUser(incomingGlobals[j], instr);
                incomingGlobals[j]->IsAddressTaken = true;
            }

            TrackResultOperand(instr->ResultOp(), incomingOp, trackedRefs);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PatchLoopOperand(PhiInstr* phiInstr, Operand* incomingOp,
                                       TrackedReferencesDict& trackedRefs) {
    // When visiting the incoming operands it's possible that
    // not all of them have been analyzed and added to the tracking set
    // in case of a loop. // To solve this problem we walk backwards, 
    // starting with the incoming operand and collecting all tracked references 
    // found by looking through addressing, 'phi' and 'quest' instructions, 
    // then associating them with the incoming one.
    if(incomingOp->IsVariableReference() || incomingOp->IsFunctionReference() ||
        trackedRefs.ContainsKey(incomingOp)) {
        return;
    }

    StaticList<Operand*, 4> foundTrackedOps;
    StaticList<PhiInstr*, 2> visitedPhis;
    StaticList<Operand*, 8> worklist;

    visitedPhis.Add(phiInstr);
    worklist.Add(incomingOp);

    while(worklist.IsNotEmpty()) {
        auto op = worklist.RemoveLast();

        if(trackedRefs.ContainsKey(op))  {
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
        TrackResultOperand(incomingOp, foundTrackedOps[i], trackedRefs);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::ProcessQuestion(QuestionInstr* instr, 
                                      TrackedReferencesDict& trackedRefs) {
    GlobalInfoList trueGlobals;
    GlobalInfoList falseGlobals;

    if(GetGlobalInfo(instr->TrueOp(), trackedRefs, trueGlobals)) {
        for(int i = 0; i < trueGlobals.Count(); i++) {
            AddUser(trueGlobals[i], instr);
            trueGlobals[i]->IsAddressTaken = true;
        }

        TrackResultOperand(instr->ResultOp(), instr->TrueOp(), trackedRefs);
    }

    if(GetGlobalInfo(instr->FalseOp(), trackedRefs, falseGlobals)) {
        for(int i = 0; i < falseGlobals.Count(); i++) {
            AddUser(falseGlobals[i], instr);
            falseGlobals[i]->IsAddressTaken = true;
        }

        TrackResultOperand(instr->ResultOp(), instr->FalseOp(), trackedRefs);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GlobalPromotion::GlobalInfo*
GlobalPromotion::GetGlobalInfo(Reference* reference) {
    // If this is the first time we request the reference info
    // create it now, presuming it has no read/writes.
    if(globalInfo_.ContainsKey(reference) == false) {
        globalInfo_.Add(reference, new GlobalInfo(reference));

        // If the reference is used in an initializer
        // we need to mark it as address-taken.
        if(initializerGlobals_.ContainsKey(reference)) {
            MarkAddressTaken(globalInfo_[reference]);
        }
    }
    
    return globalInfo_[reference];
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::GetGlobalInfo(Operand* op, TrackedReferencesDict& trackedRefs,
                                    GlobalInfoList& foundInfos) {
    if(auto reference = AsGlobalReference(op)) {
        foundInfos.Add(GetGlobalInfo(reference));
        return true;
    }
    else if(trackedRefs.ContainsKey(op)) {
        foundInfos.AddRange(trackedRefs[op]);
        return foundInfos.Count() > 0;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Reference* GlobalPromotion::AsGlobalReference(Operand* op) {
    if(auto reference = op->As<Reference>()) {
        if((reference->IsLocalVariableRef() || 
            reference->IsBlockReference()) == false) {
            return reference;
        }
    }
        
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::AddUser(GlobalInfo* globalInfo, FunctionReference* functionRef) {
    auto pair = globalInfo->Users.Find([functionRef](const UserTimesPair& pair) -> bool {
        return pair.User == functionRef;
    });

    if(pair) {
        const_cast<UserTimesPair*>(pair)->Times++;
    }
    else globalInfo->Users.Add(UserTimesPair(functionRef, 1));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::MarkAddressTaken(GlobalInfo* globalInfo) {
    globalInfo->HasRead = true;
    globalInfo->HasWrite = true;
    globalInfo->IsAddressTaken = true;
    globalInfo->HasNonConstantWrite = true;
    globalInfo->HasUnknownPositionWrite = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::MarkPassedToUnknown(GlobalInfo* globalInfo) {
    MarkAddressTaken(globalInfo);
    globalInfo->IsExternal = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::MarkArgumentGlobal(GlobalInfo* globalInfo, bool isRead, 
                                         bool isWritten, bool isEscaped) {
    // No matter in what way the argument may be modified,
    // it will be marked as address taken, but marked
    // read/write only if the corresponding flags are set.
    // An exception is when the argument may escape,
    // because we don't know what might happen with it.
    if(isEscaped) {
        // Assume the worst (read/written/external/address-taken).
        MarkPassedToUnknown(globalInfo);
    }
    else {
        globalInfo->IsAddressTaken = true;
        globalInfo->HasRead |= isRead;
        globalInfo->HasWrite |= isWritten;

        if(isWritten) {
            // We don't know exactly what values and
            // in which positions they are written.
            globalInfo->HasNonConstantWrite = true;
            globalInfo->HasUnknownPositionWrite = true;
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::MightBeAlias(Operand* a, Operand* b) {
    return aliasInfo_->HasMayAliasWithUnknownSize(a, b);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::IsDefinitelyNoAlias(Operand* a, Operand* b) {
    return aliasInfo_->HasNoAliasWithUnknownSize(a, b);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::FindInitializerGlobals() {
    // Some variables/functions might be used directly as part
    // of a variable initializer. These variables need to be marked 
    // later as being address-taken.
    for(auto variable = unit_->Variables().First();
        variable; variable = variable->Next) {
        auto globalVariable = variable->Value;

        if(globalVariable->HasInitializer() &&
           (globalVariable->HasZeroInitializer() == false)) {
            FindInitializerGlobals(globalVariable->GetInitializer(),
                                   globalVariable->GetReference());
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::FindInitializerGlobals(Initializer* initializer,
                                             VariableReference* globalVariableRef) {
    if(initializer->IsInitializerList()) {
        // Process each child initializer.
        auto initializerList = static_cast<InitializerList*>(initializer);

        for(int i = 0; i < initializerList->Count(); i++) {
            FindInitializerGlobals((*initializerList)[i], globalVariableRef);
        }
    }
    else if(auto reference = initializer->Value()->As<Reference>()) {
        // Add the global variable to the list of users.
        if(initializerGlobals_.ContainsKey(reference) == false) {
            initializerGlobals_.Add(reference, VariableReferenceList());
        }

        // Make sure the global variable appears a single time.
        if(initializerGlobals_[reference].Contains(globalVariableRef) == false) {
            initializerGlobals_[reference].Add(globalVariableRef);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PromoteFunctionsToStatic() {
    // Many functions, although marked 'static', have a call
    // from the External node because their address might be
    // taken and so escape from their parent unit.
    // If we known this can't happen we remove the call.
    auto& nodes = callGraph_->GetCallNodes();

    for(int i = 0; i < nodes.Count(); i++) {
        auto callNode = static_cast<CallNode*>(nodes[i]);
        PromoteFunctionToStatic(callNode);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void  GlobalPromotion::PromoteFunctionToStatic(CallNode* callNode) {
    auto functionRef = callNode->GetFunctionReference();
    GlobalInfo* global = nullptr;
    globalInfo_.TryGetValue(functionRef, &global);

    // If the function appears to be called by an external
    // function, but we proved it can't, so remove the call.
    if(callNode->IsCalledByExternalFunctions()) {
        bool canBePromoted = false;

        if(IsStaticFunction(functionRef, global)) {
            canBePromoted = true;
        }

        if(canBePromoted) {
            callNode->RemoveCallFromExternal();
            ExternalCallNode::GetExternalNode()->RemoveCalledNode(callNode);
        }
    }

    // Update the address-taken flag, it's useful for alias analysis.
    functionRef->Target()->SetIsAddresTaken(global == nullptr ? false :
                                            global->IsAddressTaken);

    // Create a tag that represents the functions/initializers
    // that use this function reference.
    AttachUsersToGlobal(functionRef);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::IsStaticFunction(FunctionReference* functionRef, 
                                       GlobalInfo* globalInfo) {
    // A function marked 'static' can be disconnected from
    // the External node only if its address is not taken.
    return functionRef->IsStatic() &&
           ((globalInfo == nullptr) || (globalInfo->IsAddressTaken == false));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::RemoveCallsToUnknown() {
    // When building the call graph for calls through pointers
    // a call to the Unknown node is added if the address
    // of any of the potential targets is taken.
    // Try to remove these calls using the gathered information.
    auto& callSites = callGraph_->GetCallSites();

    for(int i = 0; i < callSites.Count(); i++) {
        auto callSite = callSites[i];

        if(callSite->CallsUnknownFunctions()) {
            // Make a list with all global variable targets.
            // In most cases it is a single pointer variable.
            // If all targets have their address not taken and
            // are not external we can remove the call to the Unknown node.
            GlobalVariableList targetGlobals;
            auto callInstr = callSite->GetCallInstruction();

            if(FindVariableCallTargets(callInstr->TargetOp(), targetGlobals)) {
                RemoveCallToUnknown(callSite, targetGlobals);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::RemoveCallToUnknown(CallSite* callSite, 
                                          GlobalVariableList& targetGlobals) {
    // Don't try to remove the call to Unknown if it's not allowed
    // by the Call Graph construction algorithm.
    if(callGraph_->CanUnknownBeRemoved(callSite) == false) {
        return;
    }

    // Make sure that all call targets are now marked as being
    // not-address-taken and that they are not external.
    for(int i = 0; i < targetGlobals.Count(); i++) {
        auto globalVariable = targetGlobals[i];

        if((globalVariable->IsStatic() == false) ||
           (globalVariable->IsAddressNotTaken() == false)) {
            return;
        }
    }

    callSite->RemoveCallToUnknown();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::FindVariableCallTargets(Operand* op, GlobalVariableList& globals) {
    // Check for a load form a pointer global variable.
    auto loadInstr = op->DefiningInstrAs<LoadInstr>();

    if(loadInstr == nullptr) {
        return false;
    }
    else op = loadInstr->SourceOp();

    // The most simple case is a global variable reference.
    // We also handle simple 'phi' and 'quest' instructions.
    if(AddGlobalVariable(op, globals)) {
        return true;
    }
    else if(auto phiInstr = op->DefiningInstrAs<PhiInstr>()) {
        // The incoming operands should be either references
        // to global variables or to functions.
        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            auto incomingOp = phiInstr->GetOperand(i);

            if(AddGlobalVariable(incomingOp, globals) == false) {
                // Ignore function references.
                if(incomingOp->IsFunctionReference() == false) {
                    return false;
                }
            }
        }

        return globals.Count() > 0;
    }
    else if(auto questInstr = op->DefiningInstrAs<QuestionInstr>()) {
        if(AddGlobalVariable(questInstr->TrueOp(), globals) == false) {
            if(questInstr->TrueOp()->IsFunctionReference() == false) {
                return false;
            }
        }

        if(AddGlobalVariable(questInstr->FalseOp(), globals) == false) {
            if(questInstr->FalseOp()->IsFunctionReference() == false) {
                return false;
            }
        }

        return globals.Count() > 0;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::AddGlobalVariable(Operand* op, GlobalVariableList& globals) {
    if(auto varibleRef = op->As<VariableReference>()) {
        if(varibleRef->IsGlobalVariableRef()) {
            globals.Add(varibleRef->GetGlobalVariable());
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::PromoteVariablesToConstants() {
    // We try to mark as a constant any variable that
    // is not written into and has an initializer.
    // This is very useful for doing constant folding of loads.
    GlobalInfoList initCandidates;

    for(auto variable = unit_->Variables().First();
        variable; variable = variable->Next) {
        auto globalVariable = variable->Value;
        auto gloablVariableRef = globalVariable->GetReference();
        GlobalInfo* globalInfo;

        if(globalInfo_.TryGetValue(gloablVariableRef, &globalInfo)) {
            // First Update the address-taken flag, it's useful for alias analysis.
            globalVariable->SetIsAddresTaken(globalInfo->IsAddressTaken);

            // Create a tag that represents the functions/initializers
            // that use this global variable reference.
            AttachUsersToGlobal(gloablVariableRef);

            if(PromoteVariableToConstant(globalVariable, globalInfo)) {
                continue;
            }
            else if(AddInitializationCandidate(globalVariable, globalInfo,
                                               initCandidates)) {
                continue;
            }
            else CreateGlobalConstantsTag(globalVariable, globalInfo);
        }
    }

    // Before we can turn initialized variables to constants
    // we must check that all writes that do the initialization
    // appear in the call graph before any of the reads.
    if(initCandidates.Count() > 0) {
        InitializerWritesDominateReads(initCandidates);

        for(int i = 0; i < initCandidates.Count(); i++) {
            CreateInitializedVariable(initCandidates[i]);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::PromoteVariableToConstant(GlobalVariable* globalVariable, 
                                                GlobalInfo* globalInfo) {
    // If the variable has an initializer, is 'static',
    // has no write and doesn't escape the unit it can be marked 'const'.
    if(CanBeMarkedConstant(globalVariable, globalInfo)) {
        // Make it a constant, this may allow constant folding.
        globalVariable->SetIsConstant(true);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::CanBeMarkedConstant(GlobalVariable* globalVariable, 
                                          GlobalInfo* globalInfo) {
    // If the variable has an initializer, is 'static',
    // has no write and doesn't escape the unit it can be marked 'const'.
    return globalVariable->HasInitializer()  && 
           globalVariable->IsStatic()        &&
           (globalInfo->HasWrite == false)   &&
           (globalInfo->IsExternal == false) &&
           (globalInfo->IsAddressTaken == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::AddInitializationCandidate(GlobalVariable* globalVariable, 
                                                 GlobalInfo* globalInfo,
                                                 GlobalInfoList& initCandidates) {
    if(AreAllUsedElementInitialized(globalVariable, globalInfo)) {
        initCandidates.Add(globalInfo);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::AreAllUsedElementInitialized(GlobalVariable* globalVariable, 
                                                   GlobalInfo* globalInfo) {
    // A global variable can be marked as being constant 
    // if the only writes are used to initialize it with constants,
    // all write positions are known and the writes are all performed
    // before any element of the variable is read.
    if(globalVariable->IsStatic()                     && 
       globalInfo->HasWrite                           &&
       (globalInfo->Initializers.Count() >= 1)        &&
       (globalInfo->HasNonConstantWrite == false)     &&
       (globalInfo->HasUnknownPositionWrite == false) &&
       (globalInfo->IsExternal == false)              &&
       (globalInfo->IsAddressTaken == false)) {
        // Make sure each used element is initialized
        // using a single constant.
        if(globalVariable->IsArray() || globalVariable->IsRecord()) {
            return IsAggregateInitialized(globalVariable->GetType(), globalInfo);
        }
        else {
            // A variable that is not an aggregate should have 
            // a single initializer with a single constant.
            return (globalInfo->Initializers.Count() == 1) &&
                   (globalInfo->Initializers[0].Constants.Count() == 1);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::IsAggregateInitialized(const Type* type, GlobalInfo* globalInfo) {
    DebugValidator::IsTrue(type->IsArray() || type->IsRecord());
    DebugValidator::IsLarger(globalInfo->Initializers.Count(), 0);

    // We need to test if all used elements of the aggregate
    // are initialized with a single constant. For arrays
    // this test can be done faster, otherwise a dictionary is used.
    // As a first step sort the initializer list, it's easier to check this way.
    globalInfo->Initializers.Sort();

    if(auto arrayType = type->As<ArrayType>()) {
        if((arrayType->ElementType()->IsArray() ||
            arrayType->ElementType()->IsRecord()) == false) {
            return IsSimpleArrayInitialized(arrayType, globalInfo);
        }
    }

    // If we find a duplicate initializer or multiple constants
    // while adding to the dictionary we stop immediately.
    AccessPathDict accessPaths;

    for(int i = 0; i < globalInfo->Initializers.Count(); i++) {
        auto accessPathPair = globalInfo->Initializers[i];

        if((accessPathPair.Constants.Count() != 1) ||
            accessPaths.ContainsKey(accessPathPair.Path)) {
            return false;
        }
        else accessPaths.Add(accessPathPair.Path, true);
    }

    AccessPath requiredPath;
    return AreAllElementsInitialized(type, accessPaths, requiredPath);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::IsSimpleArrayInitialized(const ArrayType* arrayType, 
                                               GlobalInfo* globalInfo) {
    // Check if all used elements are initialized. Not all of them
    // need to be initialized if we can prove that the non-initialized
    // ones are never read from.
    auto& initializers = globalInfo->Initializers;
    int initPosition = 0;

    for(int i = 0; i < arrayType->Size(); i++) {
        if(initPosition < initializers.Count()) {
            auto& initializer = initializers[initPosition];

            // The index should match perfectly. If it does not
            // try to prove that unset elements are never red
            if(initializer.Path.Indices.Count() != 1) {
                return false;
            }

            if(initializer.Path.Indices[0] != i) {
                for(int j = i; j < initializer.Path.Indices[0]; j++) {
                    if(IsNeverReadFrom(j, globalInfo) == false) {
                        return false;
                    }
                }
                
                i = initializer.Path.Indices[0] - 1;
            }
            else { 
				initPosition++;

				// Check if there was overflow (too many elements).
				if(initPosition < 0) {
					return false;
				}
			}

            // If there is more than one constant continue
            // if we can prove this index is never read from.
            if((initializer.Constants.Count() != 1) &&
               (IsNeverReadFrom(i, globalInfo) == false)) {
                return false;
            }
        }
        else if(IsNeverReadFrom(i, globalInfo) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::IsNeverReadFrom(int index, GlobalInfo* globalInfo) {
    if(globalInfo->HasRead == false) {
        return true;
    }
    
    if(globalInfo->HasUnknownPositionRead) {
        return false;
    }

    return globalInfo->ReadPositions.IsNotSet(index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::AreAllElementsInitialized(const Type* type, 
                                                AccessPathDict& accessPaths,
                                                AccessPath& requiredPath) {
    // For arrays and records we follow the paths to the children.
    if(auto arrayType = type->As<ArrayType>()) {
		// Make sure the array is not too large.
		if((int)arrayType->Size() != arrayType->Size()) {
			return false;
		}

        for(int i = 0; i < arrayType->Size(); i++) {
            requiredPath.Indices.Add(i);

            if(AreAllElementsInitialized(arrayType->ElementType(),
                                         accessPaths, requiredPath) == false) {
                return false;
            }
            else requiredPath.Indices.RemoveLast();
        }
    }
    else if(auto recordType = type->As<RecordType>()) {
        for(int i = 0; i < recordType->FieldCount(); i++) {
            requiredPath.Indices.Add(i);

            if(AreAllElementsInitialized(recordType->GetFieldType(i),
                                         accessPaths, requiredPath) == false) {
                return false;
            }
            else requiredPath.Indices.RemoveLast();
        }
    }
    else {
        // If we arrived at a basic type check if the path is initialized.
        if(accessPaths.ContainsKey(requiredPath) == false) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::InitializerWritesDominateReads(GlobalInfoList& candidates) {
    DebugValidator::IsLarger(candidates.Count(), 0);

    // Remove candidates that have reads and writes in the same functions
    // to simplify the analysis (otherwise a Dominator Tree is required).
    DisqualifySameBlockReadWriteCandidates(candidates);

    if(candidates.Count() == 0) {
        return false;
    }

    // Check for the simple case first: the Call Graph has a single
    // root node where all global variables are initialized.
    // If all reads are in functions that are not external then
    // the values set in the root node will always be visible.
    GlobalInfoList failedCandidates;
    callGraph_->FindRoots(true /* onlyCalledByExternal */, true /* recompute */);

    for(int i = candidates.Count() - 1; i >= 0; i--) {
        if(AreAllWritesInUniqueEntryFunction(candidates[i]) == false) {
            failedCandidates.Add(candidates[i]);
        }
    }

    // If all candidates are defined in the entry function
    // there is nothing left to do.
    if(failedCandidates.Count() == 0) {
        return true;
    }
    
    // Try a more complex test that considers variables
    // initialized in a part of the call graph that is not the root.
    if(auto uniqueRootNode = GetMultipleCallsRoot()) {
        for(int i = 0; i < failedCandidates.Count(); i++) {
            if(InitializerWritesDominateReads(failedCandidates[i], uniqueRootNode)) {
                // The candidate can be promoted to constant.
                failedCandidates.RemoveAt(i);
                i--;
            }
        }
    }

    for(int i = 0; i < failedCandidates.Count(); i++) {
        candidates.Remove(failedCandidates[i]);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::DisqualifySameBlockReadWriteCandidates(GlobalInfoList& candidates) {
    // To simplify the analysis of the initialization candidates
    // we exclude the ones that have reads in the same functions
    // where the initialization writes appear.
    for(int i = 0; i < candidates.Count(); i++) {
        auto candidate = candidates[i];

        for(int j = 0; j < candidate->ReadUsers.Count(); j++) {
            if(candidate->WriteUsers.Contains(candidate->ReadUsers[j])) {
                candidates.RemoveAt(i);
                i--;
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::AreAllWritesInUniqueEntryFunction(GlobalInfo* candidate) {
    // Find the root nodes. If there is a single one we check
    // if all initialization writes are found inside it.
    // If there are multiple ones we consider only the one which
    // contains the writes, while requiring that the other ones don't read.
    if(callGraph_->HasNoRootNodes()) {
        return false;
    }
    else if(callGraph_->HasUniqueRootNode()) {
        return AreAllWritesInFunction(callGraph_->GetRootNode(0), candidate) &&
               AreAllReadsInStaticFunctions(candidate);
    }

    // Find the function which contains the writes.
    // The other ones are verified to not contain reads of the globals.
    CallNodeBase* writeNode = nullptr;

    for(int i = 0; i < callGraph_->RootNodeCount(); i++) {
        auto rootNode = callGraph_->GetRootNode(i);

        if(AreAllWritesInFunction(rootNode, candidate)) {
            writeNode = rootNode;
        }
        else if(FunctionReadsCandidate(rootNode, candidate)) {
            return false;
        }
    }

    if(writeNode) {
        return AreAllReadsInStaticFunctions(candidate);
    }
    
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::AreAllWritesInFunction(CallNodeBase* node, GlobalInfo* candidate) {
    // Don't consider node groups.
    if(node->IsNodeGroup()) {
        return false;
    }

    // All initializer writes must be in the function, and we need
    // to guarantee that they are always executed on all exist paths.
    auto callNode = static_cast<CallNode*>(node);
    if((candidate->WriteUsers.Count() != 1) ||
       (candidate->WriteUsers[0] != callNode->GetFunctionReference())) {
        return false;
    }

    for(int i = 0; i < candidate->Initializers.Count(); i++) {
        auto& stores = candidate->Initializers[i].Stores;

        if((stores.Count() != 1) || 
		   (IsAlwaysExecuted(stores[0]) == false)) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::AreAllReadsInStaticFunctions(GlobalInfo* candidate) {
    // Here we presume that the functions where already promoted to 'static'.
    for(int i = 0; i < candidate->ReadUsers.Count(); i++) {
        auto user = candidate->ReadUsers[i];

        if((user->IsStatic() == false) || 
		    user->IsAddressTaken()) {
            return false;
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::FunctionReadsCandidate(CallNodeBase* rootNode, 
                                             GlobalInfo* candidate, int level) {
    // Check if the function represented by the call node,
    // or functions called by it (up to a certain level) read
    // from the candidate variable.
    if(level > 4) {
        return true;
    }

    if(rootNode->IsNodeGroup()) {
        auto nodeGroup = static_cast<CallNodeGroup*>(rootNode);
        
        for(int i = 0; i < nodeGroup->NodeCount(); i++) {
            if(FunctionReadsCandidate(nodeGroup->GetNode(i), 
									  candidate, level + 1)) {
                return true;
            }
        }
    }
    else {
        // Check if the function appears in the list of reads.
        auto callNode = static_cast<CallNode*>(rootNode);
        auto functionRef = callNode->GetFunctionReference();

        if(candidate->ReadUsers.Contains(functionRef)) {
            return true;
        }

        // Check the functions called by this one, up to a certain level.
        // If the Unknown node appears we presume there might be a read.
        for(int i = 0; i < callNode->CalledFunctionsCount(); i++) {
            auto callSite = callNode->GetCallSite(i);

            if(callSite->CallsUnknownFunctions()) {
                return true;
            }

            for(int j = 0; j < callSite->CalledFunctionsCount(); j++) {
                if(FunctionReadsCandidate(callSite->GetCalledNode(j),
                                          candidate, level + 1)) {
                    return true;
                }
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::CallSiteReadsCandidate(CallSite* callSite, 
                                             GlobalInfo* candidate) {
    if(callSite->CallsUnknownFunctions()) {
        return true;
    }

    for(int i = 0; i < callSite->CalledFunctionsCount(); i++) {
        if(FunctionReadsCandidate(callSite->GetCalledNode(i), candidate)) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
CallNode* GlobalPromotion::GetMultipleCallsRoot() {
    // We're interested in the first call node with multiple call sites
    // starting with the required unique root node (it can also be the root).
    callGraph_->FindRoots(true /* onlyCalledByExternal */, true /* recompute */);

    if(callGraph_->HasNoRootNodes() || 
       callGraph_->HasNoUniqueRootNode()) {
        return nullptr;
    }

    auto node = static_cast<CallNode*>(callGraph_->GetRootNode(0));

    if(node->CallSiteCount() > 1) {
        return node;
    }
    else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::InitializerWritesDominateReads(GlobalInfo* candidate, 
                                                     CallNode* rootNode) {
    // Check if all initializers are written in one of the functions
    // called by 'rootNode'. If the 'call' instruction dominates 
    // all the other calls and it is guaranteed it is always executed
    // the variable can be marked as being constant.
    if(candidate->WriteUsers.Count() > 1) {
        return false;
    }

    // If the functions where the reads are performed are not all static
    // there might be a conflict between the new values and the old ones
    // in case the function is called by an external one.
    if(AreAllReadsInStaticFunctions(candidate) == false) {
        return false;
    }

    for(int i = 0; i < rootNode->CallSiteCount(); i++) {
        // Only if the call site has a single target node
        // we can be sure that the candidate is really initialized.
        auto callSite = rootNode->GetCallSite(i);

        if(callSite->CalledFunctionsCount() != 1) {
            continue;
        }

        // Don't consider node groups.  
        auto calledNode = callSite->GetCalledNode(0);

        if(calledNode->IsNodeGroup()) {
            continue;
        }

        auto callNode = static_cast<CallNode*>(calledNode);
        auto callInstr = callSite->GetCallInstruction();

        if(AreAllWritesInFunction(calledNode, candidate)) {
            // Found the function that initializes the variable.
            // now make sure that all other 'call' instructions
            // are dominated by the this call site, and the call site
            // postdominates the entry point (i.e it is guaranteed
            // that the call is always made and the global is initialized).
            for(int j = 0; j < rootNode->CallSiteCount(); j++) {
                if(i  == j) {
                    continue;
                }
                
                auto otherCallSite = rootNode->GetCallSite(j);
                auto otherCallInstr = otherCallSite->GetCallInstruction();

                if(GetSafetyInfo()->Dominates(callInstr, otherCallInstr) == false) {
                    // Try to prove that the other call site doesn't read
                    // at all from the candidate variable.
                    if(CallSiteReadsCandidate(otherCallSite, candidate)) {
                        return false;
                    }
                }
            }
            
            // Make sure the call site is always executed.
            // If not we might prove that the other call sites are also
            // not executed because they depend on the same control condition.
            if(IsAlwaysExecuted(callInstr)) {
                return true;
            }
            else return AreDependentOnSameCondition(callSite, rootNode);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::IsAlwaysExecuted(Instruction* instr) {
    // If the parent is the entry block the instruction is always executed.
    // Otherwise we use the Control Dependence Graph to prove
    // the instruction is independent of control.
    if(instr->ParentBlock()->IsFunctionEntry()) {
        return true;
    }

    return GetSafetyInfo()->IsAlwaysExecuted(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool GlobalPromotion::AreDependentOnSameCondition(CallSite* callSite, CallNode* rootNode) {
    // Check if the other call sites in the function
    // are dependent on the same condition as 'callSite'.
    // If they are it means that if 'callSite' is not called,
    // the other ones are not called too. For example:
    // if(t < 0) return -1;
    // initVars();
    // useVars();    // Both calls are made only  if 't >= 0'.
    auto callInstr = callSite->GetCallInstruction();
    auto block = callInstr->ParentBlock();

    CreateControlDependenceGraph(callInstr->ParentFunction());
    auto controllingBlocks = dependenceGraph_->GetControllingBlocks(block);

    if(controllingBlocks == nullptr) {
        return false;
    }

    for(int  i = 0; i < rootNode->CallSiteCount(); i++) {
        auto otherCallSite = rootNode->GetCallSite(i);

        if(otherCallSite == callSite) {
            continue;
        }

        auto otherBlock = otherCallSite->GetCallInstruction()->ParentBlock();
        auto otherControllingBlocks = dependenceGraph_->GetControllingBlocks(otherBlock);

        if((otherControllingBlocks == nullptr) ||
            (otherControllingBlocks->Count() != controllingBlocks->Count())) {
            return false;
        }

        for(int j = 0; j < otherControllingBlocks->Count(); j++) {
            if(controllingBlocks->Contains((*otherControllingBlocks)[j]) == false) {
                return false;
            }
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::CreateControlDependenceGraph(Function* function) {
    if((dependenceGraph_ == nullptr) || 
        (dependenceFunction_ != dependenceFunction_)) {
            // The dependence graph will be deleted automatically later.
            dependenceGraph_ = new ControlDependenceGraph(function);
            dependenceGraph_->Build();
            dependenceFunction_ = function;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::CreateInitializedVariable(GlobalInfo* globalInfo) {
    auto variableRef = globalInfo->TrackedReference->As<VariableReference>();
    auto globalVariable = variableRef->GetGlobalVariable();
    DebugValidator::IsNotNull(globalVariable);

    // We consider three types of initialized variables:
    // - basic types (integers, pointers)
    // - single-dimension arrays
    // - any other type of array or record
    if(auto arrayType = globalVariable->GetType()->As<ArrayType>()) {
        if((arrayType->ElementType()->IsArray() ||
            arrayType->ElementType()->IsRecord()) == false) {
            InitializeSimpleArray(arrayType, globalVariable, globalInfo);
        }
        else {
            InitializeAggregate(globalVariable, globalInfo);
        }
    }
    else if(auto recordType = globalVariable->GetType()->As<RecordType>()) {
        InitializeAggregate(globalVariable, globalInfo);
    }
    else {
        // A variable with basic type.
        DebugValidator::AreEqual(globalInfo->Initializers.Count(), 1);
        DebugValidator::AreEqual(globalInfo->Initializers[0].Constants.Count(), 1);

        auto constant = globalInfo->Initializers[0].Constants[0];
        globalVariable->SetInitializer(Initializer::GetInitializer(constant.Value));
    }

    // Mark the variable constant, then remove any store
    // that initialized the variable (they are not needed anymore,
    // they store the same value as the created initializers).
    globalVariable->SetIsConstant(true);
    RemoveInitializationStores(globalInfo);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::InitializeSimpleArray(const ArrayType* arrayType,
                                            GlobalVariable* globalVariable,
                                            GlobalInfo* globalInfo) {
    DebugValidator::IsFalse(arrayType->ElementType()->IsArray() ||
                            arrayType->ElementType()->IsRecord());

    // Create the list with the initializers. If the initializer for
    // an index is not found use the default value (0, 0.0, nullptr);
    auto initializerList = InitializerList::GetList();
    auto& initializers = globalInfo->Initializers;
    int initPosition = 0;

    for(int i = 0; i < arrayType->Size(); i++) {
        if(initPosition < initializers.Count()) {
            auto initializer = initializers[initPosition];

            if(initializer.Path.Indices[0] == i) {
                // The index is initialized.
                auto constant = initializer.Constants[0];
                initializerList->Add(Initializer::GetInitializer(constant.Value));
                initPosition++;
            }
            else {
                // Set each index that is not initialized to the default value.
                for(int j = i; j < initializer.Path.Indices[0]; j++) {
                    auto defaultConst = GetDefaultConstant(arrayType->ElementType());
                    initializerList->Add(Initializer::GetInitializer(defaultConst));
                }
                
                i = initializer.Path.Indices[0] - 1;
            }
        }
        else {
            // The unused end of the array is initialized with the default value.
            auto defaultConst = GetDefaultConstant(arrayType->ElementType());
            initializerList->Add(Initializer::GetInitializer(defaultConst));
        }
    }

    globalVariable->SetInitializer(initializerList);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Constant* GlobalPromotion::GetDefaultConstant(const Type* type) {
    if(type->IsInteger()) {
        return unit_->Constants().GetInt(type, 0);
    }
    else if(type->IsFloating()) {
        return unit_->Constants().GetFloating(type, 0.0);
    }
    else return unit_->Constants().GetNull(type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::InitializeAggregate(GlobalVariable* globalVariable,
                                          GlobalInfo* globalInfo) {
    int currentPosition = 0;
    auto initializer = CreateAggregateInitializer(globalVariable->GetType(),
                                                  globalInfo, currentPosition);
    globalVariable->SetInitializer(initializer);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Initializer> GlobalPromotion::CreateAggregateInitializer(const Type* type, 
                                                                GlobalInfo* globalInfo, 
                                                                int& currentPosition) {
    // If the type is an array or record recourse, until we reach
    // a basic type for which we add the constant at the current position.
    if(auto arrayType = type->As<ArrayType>()) {
        auto initializerList = InitializerList::GetList();

        for(int i = 0; i < arrayType->Size(); i++) {
            initializerList->Add(CreateAggregateInitializer(arrayType->ElementType(),
                                                            globalInfo, currentPosition));
        }

        return initializerList;
    }
    else if(auto recordType = type->As<RecordType>()) {
        auto initializerList = InitializerList::GetList();

        for(int i = 0; i < recordType->FieldCount(); i++) {
            initializerList->Add(CreateAggregateInitializer(recordType->GetFieldType(i),
                                                            globalInfo, currentPosition));
        }

        return initializerList;
    }
    else {
        // Create the initializer for a basic type.
        auto constant = globalInfo->Initializers[currentPosition].Constants[0];
        auto initializer = Initializer::GetInitializer(constant.Value);
        currentPosition++;
        return initializer;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::RemoveInitializationStores(GlobalInfo* globalInfo) {
    // Remove any 'store' that initializes the variable
    // that was turned to a constant.
    globalInfo->Initializers.ForEach([](PathConstantsPair& pair) -> bool {
        pair.Stores.ForEach([](StoreInstr* instr) -> bool {
            instr->RemoveFromBlock(true /* free */);
            return true;
        });

        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::CreateGlobalConstantsTag(GlobalVariable* globalVariable,
                                               GlobalInfo* globalInfo) {
    // For variables not promoted to constants we remember
    // the probable constants and associate them as a tag.
    // When computing the probability of a constant we take into 
    // consideration how many times it appeared in the code:
    // If 'a' appeared 1 time, 'b' 2 times, and 'c' 1 time we have
    // 'a : 25%', 'b : 50%' and 'c : 25%'.
    if(globalInfo->Initializers.Count() == 0) {
        return;
    }

    // Determine the number of times a constant was assigned.
    // The probability is lower if there are unknown writes.
    int totalTimes = globalInfo->HasNonConstantWrite ? 1 : 0;
    int distinctConstants = 0;

    for(int i = 0; i < globalInfo->Initializers.Count(); i++) {
        auto& constants = globalInfo->Initializers[i].Constants;

        for(int j = 0; j < constants.Count(); j++) {
            totalTimes += constants[j].Times;
            distinctConstants++;
        }
    }

    // If we have few constants we take them all. Otherwise
    // sort based on the apparition time and take the first four.
    auto constantsTag = GlobalConstantsTag::GetGlobalConstants();
    List<ConstantTimesPair> candidateConstants;
    
    for(int i = 0; i < globalInfo->Initializers.Count(); i++) {
        auto& constants = globalInfo->Initializers[i].Constants;
            
        for(int j = 0; j < constants.Count(); j++) {
            if(distinctConstants <= 4) {
                float probability = (float)constants[j].Times / (float)totalTimes;
                constantsTag->AddConstant(constants[j].Value, probability);
            }
            else {
                candidateConstants.Add(constants[i]);
            }
        }
    }

    if(distinctConstants > 4) {
        // Select only the most probable constants.
        candidateConstants.Sort();

        for(int i = 0; i < 4; i++) {
            float probability = (float)candidateConstants[i].Times / (float)totalTimes;
            constantsTag->AddConstant(candidateConstants[i].Value, probability);
        }
    }

    // Sort the constant based on their probability descending
    // and if possible mark the fact that the variable's
    // values are only among these constants.
    constantsTag->SortByProbability();
    constantsTag->SetHasOnlyConstants(globalVariable->IsStatic()                 &&
                                      (globalInfo->HasNonConstantWrite == false) &&
                                      (globalInfo->IsExternal == false)          &&
                                      (globalInfo->IsAddressTaken == false)      &&
                                      (distinctConstants <= 4));

    // Attach the tag to the global variable.
    globalVariable->AddTag(constantsTag);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::RemoveDeadGlobals() {
    // After inlining and load constant-folding many functions/variables
    // may be unused. Only static, non-address taken and without users
    // globals can be removed.
    auto variable = unit_->Variables().First();
    
    while(variable) {
        auto nextVariable = variable->Next;
        auto globalVariable = variable->Value;

        if(globalVariable->IsStatic() == false) {
            variable = nextVariable;
            continue;
        }

        auto variableRef = globalVariable->GetReference();

        if(globalInfo_.ContainsKey(variableRef)) {
            if(CanVariableBeRemoved(globalInfo_[variableRef])) {
                globalVariable->Free();
                unit_->Declarations().Remove(globalVariable);
                unit_->Variables().Remove(variable);
            }
        }

        variable = nextVariable;
    }

    // Function declarations can be removed even if
    // they are external/address-taken.
    auto function = unit_->Functions().First();

    while(function) {
        auto nextFunction = function->Next;
        auto functionRef = function->Value->GetReference();
        
        if(globalInfo_.ContainsKey(functionRef)) {
            if(CanFunctionBeRemoved(globalInfo_[functionRef])) {
                function->Value->Free();
                unit_->Declarations().Remove(function->Value);
                unit_->Functions().Remove(function);
            }
        }

        function = nextFunction;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::AttachUsersToGlobal(Reference* reference) {
    // Create a tag that enumerates the functions/initializers
    // that use the reference. If possible, we also record
    // for global variables the positions from which they read.
    GlobalInfo* globalInfo;
    
    if(globalInfo_.TryGetValue(reference, &globalInfo) == false) {
        return;
    }

    // Create the tag and set some of the properties.
    auto readPositions = globalInfo->HasUnknownPositionRead ?
                         nullptr : &globalInfo->ReadPositions;
    auto usersTag = GlobalUsersTag::GetGlobalUsers(globalInfo->HasRead, readPositions,
                                                   globalInfo->HasUnknownPositionRead,
                                                   globalInfo->HasUnknownPositionWrite);
    // Add the function and initializer users.
    globalInfo->Users.ForEach([usersTag](UserTimesPair& pair) -> bool {
        usersTag->AddFunctionUser(pair.User, pair.Times);
        return true;
    });

    if(initializerGlobals_.ContainsKey(reference)) {
        auto& initializers = initializerGlobals_[reference];

        initializers.ForEach([usersTag](VariableReference* variableRef) -> bool {
            usersTag->AddInitializerUser(variableRef);
            return true;
        });
    }
    
    // Finally, attach the tag to the global.
    if(auto globalVariable = reference->GetSymbol()->As<GlobalVariable>()) {
        globalVariable->AddTag(usersTag);
    }
    else if(auto function = reference->GetSymbol()->As<Function>()) {
        function->AddTag(usersTag);
    }
    else DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ControlDependenceGraph* GlobalPromotion::ControlDependenceGraphRequest(Function* function) {
    CreateControlDependenceGraph(function);
    return dependenceGraph_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void GlobalPromotion::Dump() {
    Dictionary<Reference*, bool> dumpedRefs;

    globalInfo_.ForEachValue([&dumpedRefs](GlobalInfo* info) -> bool {
        if(dumpedRefs.ContainsKey(info->TrackedReference) == false) {
            dumpedRefs.Add(info->TrackedReference, true);

            auto symbol = info->TrackedReference->GetSymbol();
            string title = (symbol->IsFunction() ? "Function " : "Variable ") +
                            *symbol->Name();

            string text;
            text += "External: ";
            text += (info->IsExternal ? "Yes" : "No");
            text += "\nHasRead: ";
            text += (info->HasRead ? "Yes" : "No");
            text += "\nHasWrite: ";
            text += (info->HasWrite ? "Yes" : "No");
            text += "\nIsAddressTaken: ";
            text += (info->IsAddressTaken ? "Yes" : "No");
            text += "\nHasNonConstantWrite: ";
            text += (info->HasNonConstantWrite ? "Yes" : "No");
            text += "\nHasUnknownPositionWrite: ";
            text += (info->HasUnknownPositionWrite ? "Yes" : "No");
            text += "\nHasUnknownPositionRead: ";
            text += (info->HasUnknownPositionRead ? "Yes" : "No");
            text += "\nInitializers: ";
            
            for(int i = 0; i < info->Initializers.Count(); i++) {
                auto init = info->Initializers[i];
                text += "\n    {";

                for(int j = 0; j < init.Path.Indices.Count(); j++) {
                    text += string::Format(L"%d ", init.Path.Indices[j]);
                }

                text += "} = ";

                for(int j = 0; j < init.Constants.Count(); j++) {
                    auto constant = init.Constants[j];

                    if(auto intConst = constant.Value->As<IntConstant>()) {
                        text += string::Format(L"%d", intConst->Value());
                    }
                    else if(auto floatConst = constant.Value->As<FloatConstant>()) {
                        text += string::Format(L"%f", floatConst->Value());
                    }
                    else if(constant.Value->IsNullConstant()) {
                        text += "nullptr";
                    }
                    else text += "undef"; 

                    text += " : " + string::Format(L"%d, ", constant.Times) + ", ";
                }
            }

            text += "\nUsers: ";

            for(int i = 0; i < info->Users.Count(); i++) {
                text += "\n    " + *info->Users[i].User->GetSymbol()->Name();
                text += " : " + string::Format(L"%d", info->Users[i].Times);
            }

            if(info->ReadPositions.SetBitsCount() > 0) {
                text += "\nRead positions: ";

                info->ReadPositions.ForEachSetBit([&text](int index) -> bool {
                    text += string::Format(L"%d, ", index);
                    return true;
                });
            }

            text += "\n";
            ObjectDumper(text, title).Dump();
        }

        return true;
    });
}

} // namespace Optimization