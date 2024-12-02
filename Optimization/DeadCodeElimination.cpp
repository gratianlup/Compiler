// DeadCodeElimination.hpp
// Copyright (c) Lup Gratian
//
// Implements the DeadCodeElimination pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DeadCodeElimination.hpp"
#include "../IR/IRPrinter.hpp"

namespace Optimization {

void DeadCodeElimination::Execute() {
    ProcessInstructions();
    DeleteDeadInstructions();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::ProcessInstructions() {
    // Add to the worklist all instructions that are essential
    // for the execution of the function (this includes instructions
    // with side effects, such as 'store', 'call' and 'ret').
    CollectEssentialInstructions();

    // Build the control dependence graph. It is used to activate
    // the blocks that control the execution of another block.
    dependenceGraph_.Build();
    dependenceGraph_.Dump();

    // Iterate until the worklist is empty. An instruction is taken
    // from the worklist and all it's operands that are instructions
    // too are added to the worklist, because they're required.
    while(auto instr = RemoveFromWorklist()) {
        // The block that contains is marked active
        // (this adds to the worklist the branching instructions 
        //  from all blocks that control its execution).
        MarkBlockActive(instr->ParentBlock());
        AddOperandsToWorklist(instr);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::DeleteDeadInstructions() {
    // Delete all instructions that are not marked.
    // If we have a branch to a block that is not active
    // we make the branch jump to the immediate post-dominator
    // of its parent block (the dead blocks are avoided).
    for(auto block = funct_->FirstBlock(); block; block = block->NextBlock()) {
        // Process all instructions and delete the dead ones.
        auto instr = block->FirstInstruction();

        while(instr) {
            auto nextInstr = instr->NextInstruction();

            // Delete the instruction if it's dead.
            // If it's a branch we handle it separately.
            if(WasInstructionProcessed(instr) == false) {
                if(instr->IsBranching() == false) {
                    InstructionEliminated(instr);
                    ReplaceDeadArguments(instr);
                    instr->RemoveFromBlock(true /* free */);
                }
                else DeleteDeadBranch(instr);
            }

            instr = nextInstr;
        }
    }

    // Remove any store that is definitely dead because
    // it stores a previously loaded value to the same location.
    for(int i = 0; i < definitelyDeadStores_.Count(); i++) {
        auto store = static_cast<StoreInstr*>(definitelyDeadStores_[i]);

        if(WasInstructionProcessed(store)) {
            store->RemoveFromBlock(true /* free */);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::ReplaceDeadArguments(Instruction* instr) {
    // If the instruction is dead, but it still has users,
    // they all must be 'call' instructions that use the result
    // as an argument that is never read by the callee.
    // Replace the instruction result with 'undef', but only
    // if the argument is not a pointer (the callee needs to store
    // into a valid memory location to preserve the program semantics).
    if((instr->HasDestinationOp() == false) ||
       (instr->GetDestinationOp()->HasUsers() == false)) {
        return;
    }

    // Collect the 'call' instruction users. The arguments cannot be
    // replaced directly because the enumerator would be invalidated.
    StaticList<CallInstr*, 4> callUsers;
    auto destinationOp = instr->GetDestinationOp();
    
    destinationOp->ForEachUser([&callUsers](Instruction* user, int index) -> bool {
        if(auto callInstr = user->As<CallInstr>()) {
            callUsers.Add(callInstr);
        }

        return true;
    });

    // Replace each argument that is the destination operand 
    // with the undefined constant.
    auto undefinedConst = GetUndefinedConstant(destinationOp);

    for(int i = 0; i < callUsers.Count(); i++) {
        auto callInstr = callUsers[i];

        for(int j = 0; j < callInstr->ArgumentCount(); j++) {
            if(callInstr->GetArgument(j) == destinationOp) {
                callInstr->ReplaceArgument(j, undefinedConst);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UndefinedConstant* DeadCodeElimination::GetUndefinedConstant(Operand* op) {
    DebugValidator::IsTrue(op->HasDefiningInstruction());

    auto unit = op->DefiningInstruction()->ParentFunction()->ParentUnit();
    return unit->Constants().GetUndefined(op->GetType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::DeleteDeadBranch(Instruction* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(instr->IsBranching());

    // A branching instruction is dead if none of its successors
    // are required. In this case we can jump directly to the
    // immediate post-dominator.
    auto block = instr->ParentBlock();
    auto postDom = GetImmediatePostDominator(block);

    // Remove the current branching instruction,
    // replace it with a 'goto' to the postdominator.
    if(postDom) {
        block->LinkWith(postDom);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* DeadCodeElimination::GetImmediatePostDominator(Block* block) {
    auto& postdomTree = dependenceGraph_.GetPostdominatorTree();
    auto postDom = postdomTree.GetImmediateDominator(block);

    return postDom;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::CollectEssentialInstructions() {
    // Add to the worklist all instructions that are essential.
    // These form the initial set of alive instructions.
    funct_->ForEachInstruction([this](Instruction* instr) -> bool {
        if(IsEssentialInstruction(instr)) {
            AddToWorklist(instr);
        }
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* DeadCodeElimination::RemoveFromWorklist() {
    if(worklist_.IsEmpty()) {
        // Nothing left to process.
        return nullptr;
    }

    return worklist_.RemoveLast();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::MarkBlockActive(Block* block) {
    // The instruction's parent block becomes active,
    // the branching instructions in all blocks that control it's execution
    // need to be added to the worklist.
    if(activeBlocks_.IsSet(block->Id()) == false) {
        activeBlocks_.SetBit(block->Id());
        
        auto controllingBlocks = dependenceGraph_.GetControllingBlocks(block);
        if(controllingBlocks == nullptr) {
            return;
        }

        controllingBlocks->ForEach([this](Block* block) -> bool {
            AddToWorklist(block->BranchInstruction());
            return true;
        });
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddToWorklist(Instruction* instr) {
    DebugValidator::IsNotNull(instr);

    // Call arguments that where presumed dead might
    // now be alive because it was found that the argument is used.
    if(auto callInstr = instr->As<CallInstr>()) {
        AddCallPresumedDeadToWorklist(callInstr);
    }

    if(processedInstrs_.ContainsKey(instr) == false) {
        // If this is a 'load' from a local variable
        // add all 'store's to the same variable to the worklist,
        // because they can no longer be considered dead.
        if(auto loadInstr = instr->As<LoadInstr>()) {
            // If the loaded value is only used by a 'store'
            // that stores exactly to the same location of the loaded value
            // then both instructions are dead. Code like this usually
            // appears after certain optimizations.
            if(IsUsedByStoreToSameLocation(loadInstr)) {
                return; // Not marked alive anymore.
            }

            if(auto baseVariable = GetBaseVariable(loadInstr->SourceOp())) {
                MarkLoadFromVariable(baseVariable);
                MarkStoresCallsAlive(baseVariable);
            }
        }

        worklist_.Add(instr);
        processedInstrs_.Add(instr, true);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsUsedByStoreToSameLocation(LoadInstr* loadInstr) {
    if((loadInstr->HasDestinationOp() == false) ||
       (loadInstr->ResultOp()->UserCount() != 1)) {
        return false;
    }

    if(auto storeInstr = loadInstr->ResultOp()->GetUser(0)->As<StoreInstr>()) {
        if((loadInstr->ParentBlock() != storeInstr->ParentBlock()) ||
           (storeInstr->SourceOp() != loadInstr->ResultOp())       ||
           (storeInstr->DestinationOp() != loadInstr->SourceOp())) {
            return false;
        }

        auto instr = static_cast<Instruction*>(storeInstr);

        while(instr != loadInstr->NextInstruction()) {
            if(instr->IsStore() || instr->IsCall()) {
                return false;
            }
            else instr = instr->PreviousInstruction();
        }

        MarkDefinitelyDeadStore(storeInstr);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddCallPresumedDeadToWorklist(CallInstr* instr) {
    if(IsPotentiallyDeadCall(instr) == false) {
        return;
    }

    // Call arguments that where presumed dead might
    // now be alive because it was found that the argument is used.
    for(int i = 0; i < instr->ArgumentCount(); i++) {
        auto argument = instr->GetArgument(i);

        if(argument->IsPointer() == false) {
            continue;
        }

        if(auto baseVariable = GetBaseVariable(argument)) {
            if(HasLoadFromVariable(baseVariable)) {
                if(argument->HasDefiningInstruction()) {
                    AddToWorklist(argument->DefiningInstruction());
                }
                else MarkLoadFromVariable(baseVariable);
            }
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddOperandsToWorklist(Instruction* instr) {
    DebugValidator::IsNotNull(instr);

    // Handle 'phi' and 'call' separately.
    if(auto phiInstr = instr->As<PhiInstr>()) {
        AddPhiOperandsToWorklist(phiInstr);
        return;
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        AddCallArgumentsToWorklist(callInstr);
        return;
    }

    // Estimate the bits that are killed by the instruction.
    Mask deadBits = EstimateDeadBits(instr);

    for(int i = 0; i < instr->SourceOpCount(); i++) {
        auto incomingOp = instr->GetSourceOp(i);
        auto temp = incomingOp->As<Temporary>();
        
        if(temp == nullptr) {
            if(auto variableRef = incomingOp->As<VariableReference>()) {
                MarkLoadFromVariable(variableRef);
                MarkStoresCallsAlive(variableRef);
            }

            continue;
        }

        bool wasDead = false;

        if(deadBits) {
            // Check if the operand is actually dead because the bits
            // modified by it are "killed" by the instruction.
            // We iterate until no more dead operands are found. For example:
            // b = or a, 1   -> dead, sets bit killed by 'shr'
            // c = or b, 2   -> dead, the same
            // d = shr c, 8  -> d = shr a, 8
            auto defInstruction = temp->DefiningInstruction();

            while(auto replacementOp = SkipDeadInstruction(defInstruction, deadBits)) {
                // Replace the original operand.
                instr->ReplaceSourceOp(i, replacementOp);
                defInstruction = replacementOp->DefiningInstruction();
                wasDead = true;
            }
        }

        // The temporary might have been replaced by something
        // that isn't a temporary anymore.
        if(wasDead) {
            temp = instr->GetSourceOp(i)->As<Temporary>();
            
            if(temp == nullptr) {
                continue;
            }
        }

        AddToWorklist(temp->DefiningInstruction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddPhiOperandsToWorklist(PhiInstr* instr) {
    DebugValidator::IsNotNull(instr);

    for(int i = 0; i < instr->OperandCount(); i++) {
        AddOperandToWorklist(instr->GetOperand(i));

        // The block from which the operand is incoming
        // must be processed, make its branching instruction active.
        auto incomingBlock = instr->GetOperandBlock(i);
        AddToWorklist(incomingBlock->BranchInstruction());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddCallArgumentsToWorklist(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);

    // Try to mark live only the arguments that are really
    // used in the called function. We use the 'noread' flag
    // if a single known function can be called.
    auto calledFunction = instr->GetCalledFunction();

    for(int i = 0; i < instr->ArgumentCount(); i++) {
        if(calledFunction) {
            auto parameterVariable = calledFunction->GetParameterVariable(i);
            
            if(parameterVariable->IsNoRead()) {
                continue;
            }
        }
        
        AddOperandToWorklist(instr->GetArgument(i));
    }

    // Add the target if it is not a function reference.
    if(instr->TargetOp()->IsFunctionReference() == false) {
        AddOperandToWorklist(instr->TargetOp());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddOperandToWorklist(Operand* op) {
    if(auto temp = op->As<Temporary>()) {
        AddToWorklist(temp->DefiningInstruction());
    }
    else if(auto variableRef = op->As<VariableReference>()) {
        MarkLoadFromVariable(variableRef);
        MarkStoresCallsAlive(variableRef);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::MarkLoadFromVariable(VariableReference* baseVariable) {
    DebugValidator::IsNotNull(baseVariable);
    
    if(baseVariable->IsLocalVariableRef()) {
        loadedVariables_.Add(baseVariable, true);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::MarkStoresCallsAlive(VariableReference* baseVariable) {
    DebugValidator::IsNotNull(baseVariable);
    
    // Any store that targets the variable must be considered live,
    // because at least in one place the values are loaded.
    if(baseVariable->IsLocalVariableRef()) {
        if(potentiallyDeadStoresCalls_.ContainsKey(baseVariable) == false) {
            return;
        }

        auto& storeCallList = potentiallyDeadStoresCalls_[baseVariable];

        for(int i = 0; i < storeCallList.Count(); i++) {
            if(IsDefinitelyDeadStore(storeCallList[i]) == false) {
                AddToWorklist(storeCallList[i]);
            }
        }

        // The list can now be removed.
        potentiallyDeadStoresCalls_.Remove(baseVariable);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsEssentialInstruction(Instruction* instr) {
    DebugValidator::IsNotNull(instr);

    // An instruction is considered to be essential if it has
    // a side-effect visible externally (store to memory, function call, return).
    if(instr->IsArithmetic() || 
       instr->IsLogical()    ||
       instr->IsConversion()) {
        // The most often cases, handle it first.
        return false;
    }

    if(instr->IsReturn()) {
        // We consider all 'ret' instructions essential.
        return true;
    }
    else if(auto storeInstr = instr->As<StoreInstr>()) {
        return IsEssentialStore(storeInstr);
    }
    else if(auto loadInstr = instr->As<LoadInstr>()) {
        // Only volatile loads are considered essential.
        return loadInstr->IsVolatile();
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        return IsEssentialCall(callInstr);
    }

    // All other instructions don't have visible side-effects.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsEssentialCall(CallInstr* instr) {
    // Calls to some intrinsics can be considered to not have side-effects.
    if(auto intrinsic = instr->GetIntrinsic()) {
        if(intrinsic->IsMathIntrinsic() ||
           intrinsic->IsBitwiseIntrinsic()) {
            return false;
        }

        // A 'copyMemory'/'setMemory' with size 0 should be removed.
        // Otherwise we presume they are dead until we find
        // a load from the targeted variables.
        if(intrinsic->Is<SetMemoryIntr>() || 
           intrinsic->Is<CopyMemoryIntr>()) {
            if(SetMemoryIntr::GetLength(instr)->IsZeroInt()) {
                return false;
            }

            // This can be done only if the target is a local variable.
            if(auto baseVariable = GetBaseVariable(instr->GetArgument(0))) {
                MarkPotentiallyDeadCall(instr, baseVariable);
                return false;
            }
        }

        return true;
    }

    // Calls to pure functions (don't write to memory and their result
    // depends only on the parameters) have no side-effects, unless
    // they write into their pointer parameters.
    auto calledFunct = instr->GetCalledFunction();
    
    if(calledFunct == nullptr) {
        return true;
    }
    else if(calledFunct->IsNoState()) {
        if(WritesIntoPointerParameters(calledFunct, instr) == false) {
            MarkPotentiallyDeadCall(instr);
            return false;
        }
    }

    // Try to use language-specific information.
    if(auto info = GetLanguageInfo()) {
        return info->CallMayHaveSideEffects(instr);
    }

    // All other calls may have side-effects.
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::WritesIntoPointerParameters(Function* function,
                                                      CallInstr* instr) {
    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsPointer() && 
           parameterVariable->IsWrite()) {
            // If the argument is a local variable we presume
            // that the written values are not needed, until proved otherwise.
            if(IsPotentiallyUnusedVariable(instr->GetArgument(i), instr) == false) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsEssentialStore(StoreInstr* instr) {
    // If the store is volatile it is essential.
    if(instr->IsVolatile()) {
        return true;
    }

    // We presume that a store to a local variable is dead,
    // unless we find a load from exactly the same variable.
    // A further condition is that the address of the variable should not be
    // taken, it's difficult to track the aliases exactly.
    if(IsPotentiallyUnusedVariable(instr->DestinationOp(), instr)) {
       return false;
    }

    // If we know exactly which positions of the global variable
    // are read we might be able to eliminate the store.
    VariableList globalVariables;
    
    if(GetBaseVariables(instr->DestinationOp(), globalVariables)) {
        return IsEssentialGlobalStore(instr, globalVariables);
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsPotentiallyUnusedVariable(Operand* op, Instruction* instr) {
    if(auto baseVariable = GetBaseVariable(op)) {
        if(baseVariable->IsAddressTaken() &&
           HasUnsafeUses(baseVariable, instr->ParentFunction())) {
            return false;
        }

        // If we already found a load from this variable give up.
        if(HasLoadFromVariable(baseVariable)) {
            return false;
        }

        AddPotentiallyDeadStoreCall(baseVariable, instr);
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::HasUnsafeUses(VariableReference* variableRef,
                                        Function* function) {
    // Try to use the cache first.
    bool unsafeUse;

    if(deadCandidateVariables_.TryGetValue(variableRef, &unsafeUse)) {
        return unsafeUse;
    }
    else unsafeUse = false;

    // Add to the worklist all destination operands of the
    // instructions that use the variable.
    // Until the worklist is empty, take an operand (temporary)
    // analyze how it is used, then add the users result to the worklist.
    TemporaryList worklist;
    
    function->ForEachInstruction([this, variableRef, &worklist, &unsafeUse]
                                 (Instruction* instr) -> bool {
        if(instr->HasSourceOp(variableRef)) {
            if(IsUnsafeUse(variableRef, instr, worklist)) {
                unsafeUse = true;
                return false;
            }
        }

        return true;
    });

    // If the variable was already marked unsafe
    // there is no point in processing the worklist anymore.
    if(unsafeUse) {
        return MarkUnsafeUse(variableRef, unsafeUse);
    }

    while(worklist.IsNotEmpty() && (unsafeUse == false)) {
        auto temp = worklist.RemoveLast();

        temp->ForEachUser([this, temp, &worklist, &unsafeUse]
                          (Instruction* user, int index) -> bool {
            if(IsUnsafeUse(temp, user, worklist)) {
                unsafeUse = true;
                return false;
            }

            return true;
        });
    }

    return MarkUnsafeUse(variableRef, unsafeUse);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsUnsafeUse(Operand* op, Instruction* instr,
                                      TemporaryList& worklist) {
    // Check the way the operand is used by the instruction.
    // In most cases the use is not dangerous and can be tracked.
    if(auto loadInstr = instr->As<LoadInstr>()) {
        if(loadInstr->IsVolatile()) {
            return true;
        }
    }
    else if(auto storeInstr = instr->As<StoreInstr>()) {
        if(storeInstr->IsVolatile() ||
           (storeInstr->SourceOp() == op)) {
            return true;
        }
    }
    else if(auto callInstr = instr->As<CallInstr>()) {
        // Consider the use dangerous only if used by a parameter
        // whose value escapes in the called function.
        if(EscapesInCalledFunctions(op, callInstr)) {
            return true;
        }
    }
    else if((instr->As<PtopInstr>() || 
             instr->IsAddressing()  ||
             instr->IsComparison()) == false) {
        // Any other instructions beside 'ptop', 'index', 'field', 'addr'
        // 'quest' and comparisons are considered to be dangerous.
        return true;
    }

    // Track what happens with the result of the addressing
    // instructions, 'ptop' and 'quest'.
    if(instr->HasDestinationOp()) {
       if((instr->IsAddressing() || instr->IsPtop() || instr->IsQuestion()) &&
          (worklist.Contains(instr->GetDestinationOp()) == false)) {
            worklist.Add(instr->GetDestinationOp());
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::EscapesInCalledFunctions(Operand* op, CallInstr* callInstr) {
    // Here we consider a call with a single target only.
    auto function = callInstr->GetCalledFunction();

    if(function == nullptr) {
        return true;
    }

    // Check each of the pointers parameters if it has 
    // the operand as an argument and if it escapes.
    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsPointer() &&
           parameterVariable->IsEscape()) {
            if(callInstr->GetArgument(i) == op) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::IsEssentialGlobalStore(StoreInstr* instr, 
                                                 VariableList& globalVariables) {
    // A store to a global variable is dead if we know that
    // the position where we write it's never read in the whole program.
    // We do this only for single-dimension arrays and records.
    bool hasAddressing = false;
    __int64 index;

    if(auto indexInstr = instr->DestinationOp()->DefiningInstrAs<IndexInstr>()) {
        if(IsAddressingInstruction(indexInstr->BaseOp()) == false) {
            if(auto intConstant = indexInstr->IndexOp()->As<IntConstant>()) {
                index = intConstant->Value();
                hasAddressing = index >= 0;
            }
        }
        
        if(hasAddressing == false) {
            return true;
        }
    }
    else if(auto fieldInstr = instr->DestinationOp()->DefiningInstrAs<FieldInstr>()) {
        if(IsAddressingInstruction(fieldInstr->BaseOp()) == false) {
            index = fieldInstr->GetFieldIndex();
            hasAddressing = true;
        }
        else return true;
    }

    // Verify the users tag associated with each global variable.
    // If the tag is missing, or it indicates that the position
    // might be read we must presume the store is not dead.
    for(int i = 0; i < globalVariables.Count(); i++) {
        auto globalVariable = globalVariables[i]->GetGlobalVariable();
        auto usersTag = globalVariable->GetTag<GlobalUsersTag>();

        if(usersTag == nullptr) {
            return true;
        }

        if(usersTag->HasUnknownPositionRead()) {
            return true;
        }

        if(hasAddressing) {
           if(usersTag->HasReadOnPosition(index)) {
                return true;
           }
        }
        else {
            // This is a variable that is not an array/record.
            // The store can be eliminated only if there are no reads at all.
            if(usersTag->HasAnyReads()) {
                return true;
            }
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::AddPotentiallyDeadStoreCall(VariableReference* baseVariable, 
                                                      Instruction* instr) {
    DebugValidator::IsNotNull(baseVariable);
    DebugValidator::IsNotNull(instr);

    if(potentiallyDeadStoresCalls_.ContainsKey(baseVariable) == false) {
        potentiallyDeadStoresCalls_.Add(baseVariable, InstructionList());
    }

    potentiallyDeadStoresCalls_[baseVariable].Add(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VariableReference* DeadCodeElimination::GetBaseVariable(Operand* op) {
    if(auto variableRef = op->As<VariableReference>()) {
        if(variableRef->IsLocalVariableRef()) {
            return variableRef;
        }
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        if(definingInstr->IsAddress() ||
           definingInstr->IsIndex()   ||
           definingInstr->IsField()   ||
           definingInstr->IsPtop()) {
            return GetBaseVariable(definingInstr->GetSourceOp(0));
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeadCodeElimination::GetBaseVariables(Operand* op, VariableList& list, int level) {
    // Stop if we're too deep in recursion, it's unlikely 
    // to find any variables anymore.
    if(level > 4) {
        return false;
    }

    if(auto variableRef = op->As<VariableReference>()) {
        if(variableRef->IsGlobalVariableRef()) {
            list.Add(variableRef);
            return true;
        }
    }
    else if(auto definingInstr = op->DefiningInstruction()) {
        if(definingInstr->IsAddress() ||
           definingInstr->IsIndex()   ||
           definingInstr->IsField()) {
            return GetBaseVariables(definingInstr->GetSourceOp(0), list);
        }
        else if(auto questInstr = definingInstr->As<QuestionInstr>()) {
            return GetBaseVariables(questInstr->TrueOp(), list, level + 1) &&
                   GetBaseVariables(questInstr->FalseOp(), list, level + 1);
        }
        else if(auto phiInstr = definingInstr->As<PhiInstr>()) {
            // All incoming operands should be based on variables.
            // Don't process if there are many incoming operands.
            if(phiInstr->OperandCount() > 8) {
                return false;
            }

            for(int i = 0; i < phiInstr->OperandCount(); i++) {
                if(GetBaseVariables(phiInstr->GetOperand(i), list, level + 1) == false) {
                    return false;
                }
            }

            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask DeadCodeElimination::EstimateDeadBits(Instruction* instr) {
    Mask deadBits = 0;

    // We presume that the constants are always the right operand.
    // This is usually true after peephole optimization.
    switch(instr->GetOpcode()) {
        case Opcode::Mul: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                if(IA::IsPowerOfTwo(intConst->Value())) {
                    // Multiplying by a power of two shifts the top bits.
                    // For example, 'a * 16' shifts the topmost 4 bits.
                    Mask shiftetBits = IA::Log2(intConst->Value());
                    deadBits = ((1ULL << shiftetBits) - 1) << 
                               (OperandBits(intConst) - shiftetBits);
                }
            }
            break;
        }
        case Opcode::Div:
        case Opcode::Udiv: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                if(IA::IsPowerOfTwo(intConst->Value())) {
                    // Division by a power of two shifts the bottom bits.
                    // For example, 'a / 8' shifts the bottom 3 bits.
                    Mask shiftetBits = IA::Log2(intConst->Value());
                    deadBits = (1ULL << shiftetBits) - 1;
                }
            }
            break;
        }
        case Opcode::And: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // The bits that are not set in the constant
                // will be reset, no matter what their value is.
                deadBits = ~intConst->Value();
            }
            break;
        }
        case Opcode::Or: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // The bits that are set in the constant will be set in the result.
                deadBits = intConst->Value();
            }
            break;
        }
        case Opcode::Shr:
        case Opcode::Ushr: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // The bottom bits will be shifted away.
                deadBits = (1ULL << intConst->Value()) - 1;
            }
            break;
        }
        case Opcode::Shl: {
            if(auto intConst = instr->GetSourceOp(1)->As<IntConstant>()) {
                // The topmost bits will be shifted away.
                deadBits = ((1ULL << intConst->Value()) - 1) <<
                           (OperandBits(intConst) - intConst->Value());
            }
            break;
        }
        case Opcode::Trunc: {
            // The top bits are truncated, so their value doesn't matter.
            auto truncInstr = instr->As<TruncInstr>();
            Mask fromBits = OperandBits(truncInstr->TargetOp());
            Mask toBits = TI::GetSizeBits(truncInstr->CastType());
            deadBits = ((1ULL << (fromBits - toBits)) - 1) << toBits;
            break;
        }
    }

    return deadBits;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* DeadCodeElimination::SkipDeadInstruction(Instruction* instr, Mask deadBits) {
    DebugValidator::IsLarger(deadBits, 0);

    if(instr == nullptr) {
        return nullptr;
    }

    // We estimate the bits for some instructions and check
    // if they are among the ones that will be killed.
    switch(instr->GetOpcode()) {
        case Opcode::Or: {
            // Check if the right operand doesn't set anything
            // that will not be killed afterwards.
            return SelectOperand(instr, deadBits, &DCE::EstimateSetBits);
        }
        case Opcode::And: {
            // Check if the right operand doesn't reset anything
            // that will not be killed afterwards.
            return SelectOperand(instr, deadBits, &DCE::EstimateResetedBits);
        }
        case Opcode::Xor: {
            // Check if the right operand doesn't invert anything
            // that will not be killed afterwards.
            return SelectOperand(instr, deadBits, &DCE::EstimateInvertedBits);
        }
        case Opcode::Add: {
            // Check if the right operand doesn't add anything
            // that will not be killed afterwards.
            return SelectOperand(instr, deadBits, &DCE::EstimateAddedBits);
        }
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* DeadCodeElimination::SelectOperand(Instruction* instr, Mask deadBits,
                                            BitEstimator estimator) {
    DebugValidator::IsNotNull(instr);

    // Check if only the left operand should be saved.
    Mask estimatedBits = (this->*estimator)(instr->GetSourceOp(0), 
                                             instr->GetSourceOp(1));

    if((estimatedBits & deadBits) == estimatedBits) {
        return instr->GetSourceOp(0);
    }

    // Do the check for the right operand.
    estimatedBits = (this->*estimator)(instr->GetSourceOp(1), 
                                       instr->GetSourceOp(0));

    if((estimatedBits & deadBits) == estimatedBits) {
        return instr->GetSourceOp(1);
    }

    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask 
DeadCodeElimination::EstimateSetBits(Operand* a, Operand* b) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);

    // Estimate the bits that are zero. If we are not sure
    // if they're zero, we are conservative and assume they are.
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    Mask zeroBitsA;
    Mask oneBitsA;
    opInfo.EstimateZeroBits(a, zeroBitsA);
    opInfo.EstimateOneBits(a, oneBitsA);
    zeroBitsA = zeroBitsA | ~(zeroBitsA | oneBitsA);

    // Estimate the bits that are set in the other operand.
    // We are conservative and any bit that is not one or zero
    // is considered to be one.
    Mask oneBitsB;
    Mask zeroBitsB;
    opInfo.EstimateOneBits(b, oneBitsB);
    opInfo.EstimateZeroBits(b, zeroBitsB);
    oneBitsB = oneBitsB | ~(oneBitsB | zeroBitsB);

    // Any bit that was zero and after the 'or' is one
    // is considered to be set by the instruction.
    Mask typeLimit = IA::GetMinusOneMask(a->GetType()->As<IntegerType>());
    return (zeroBitsA & oneBitsB) & typeLimit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask 
DeadCodeElimination::EstimateResetedBits(Operand* a, Operand* b) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);
    
    // Estimate the bits that are zero. If we are not sure
    // if they're zero, we are conservative and assume they are.
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    Mask zeroBitsA;
    Mask oneBitsA;
    opInfo.EstimateZeroBits(a, zeroBitsA);
    opInfo.EstimateOneBits(a, oneBitsA);
    oneBitsA = oneBitsA | ~(zeroBitsA | oneBitsA);

    // Estimate the bits that are set in the other operand.
    // We are conservative and any bit that is not one or zero
    // is considered to be one.
    Mask oneBitsB;
    Mask zeroBitsB;
    opInfo.EstimateOneBits(b, oneBitsB);
    opInfo.EstimateZeroBits(b, zeroBitsB);
    zeroBitsB = zeroBitsB | ~(oneBitsB | zeroBitsB);

    // Any bit that was zero and after the 'or' is one
    // is considered to be set by the instruction.
    Mask typeLimit = IA::GetMinusOneMask(a->GetType()->As<IntegerType>());
    return (oneBitsA & zeroBitsB) & typeLimit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask
DeadCodeElimination::EstimateInvertedBits(Operand* a, Operand* b) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);

    // Estimate the bits that are not set in operand 'b'.
    // Then loop over all bits and decide if it's possible
    // that they change. For example, if we know that a bit in 'b'
    // is definitely, the bit doesn't change after the 'xor'.
    Mask zeroBitsB;
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    opInfo.EstimateZeroBits(b, zeroBitsB);

    Mask typeLimit = IA::GetMinusOneMask(a->GetType()->As<IntegerType>());
    return ~zeroBitsB & typeLimit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeadCodeElimination::Mask 
DeadCodeElimination::EstimateAddedBits(Operand* a, Operand* b) {
    DebugValidator::IsNotNull(a);
    DebugValidator::IsNotNull(b);

    // Estimate the bits that are added. Any bit that is not
    // definitely zero is presumed to be one.
    OperandInfo opInfo(funct_->ParentUnit(), GetTarget());
    Mask zeroBitsB;
    Mask oneBitsB;
    opInfo.EstimateZeroBits(b, zeroBitsB);
    opInfo.EstimateOneBits(b, oneBitsB);
    oneBitsB = oneBitsB | ~(zeroBitsB | oneBitsB);

    // Estimate the bits that are zero in 'a'.
    Mask zeroBitsA;
    opInfo.EstimateZeroBits(a, zeroBitsA);
    Mask typeLimit = IA::GetMinusOneMask(a->GetType()->As<IntegerType>());

    if(((oneBitsB & zeroBitsA) & typeLimit) != (oneBitsB & typeLimit)) {
        // Not all bits set by 'b' are zero in 'a',
        // so we return a conservative result that will prevent
        // any (potential) invalid optimization.
        return typeLimit;
    }
        
    return (oneBitsB & zeroBitsA) & typeLimit;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeadCodeElimination::InstructionEliminated(Instruction* instr) {
#if 0
	auto block = instr->ParentBlock();
	auto function = instr->ParentFunction();
	string blockName = block && block->HasName() ? *block->Name() : "UNTITLED";
	string functionName = function && function->HasName() ? *function->Name() : "UNTITLED";
	string beforeText = IRPrinter(instr).ToString();
	Log::Warning("DCE in " + functionName + ":" + blockName + ": " +  beforeText);
    //IRPrinter(function).Dump();
#endif
}

} // namespace Optimization