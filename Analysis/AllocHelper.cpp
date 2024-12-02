// AllocHelper.cpp
// Copyright (c) Lup Gratian
//
// Implements the AllocHelper class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "AllocHelper.hpp"

namespace Analysis {

Operand* AllocHelper::GetBaseOperand(Operand* op) {
	DebugValidator::IsNotNull(op);

	// The function is a candidate for tail-recursion elimination.
    if(auto indexInstr = op->DefiningInstrAs<IndexInstr>()) {
        return GetBaseOperand(indexInstr->BaseOp());
    }
    else if(auto addrInstr = op->DefiningInstrAs<AddressInstr>()) {
        return GetBaseOperand(addrInstr->BaseOp());
    }
    else if(auto fieldInstr = op->DefiningInstrAs<FieldInstr>()) {
        return GetBaseOperand(fieldInstr->BaseOp());
    }
    else if(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
        return GetBaseOperand(ptopInstr->TargetOp());
    }
    else return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocHelper::IsAllocCall(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);

    // We ask the language information to answer the query.
    if(auto calledFunction = instr->GetCalledFunction()) {
        if(languageInfo_) {
            return languageInfo_->IsAllocFunction(calledFunction);
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocHelper::OriginatesFromAlloc(Operand* op, AllocCallList* allocList, int level) {
	DebugValidator::IsNotNull(op);

    // Don't recourse too much.
    if(level > 4) {
        return false;
    }

    // Look through addressing instructions and pointer casts.
    auto baseOp = GetBaseOperand(op);

    // Check if we have a call to an allocation function.
    if(auto callInstr = baseOp->DefiningInstrAs<CallInstr>()) {
        bool isAlloc = IsAllocCall(callInstr);

        // Add the call to the list if desired.
        if(isAlloc && allocList) {
            if(allocList->Contains(callInstr) == false) {
                allocList->Add(callInstr);
            }
        }

        return isAlloc;
    }

    if(auto phiInstr = baseOp->DefiningInstrAs<PhiInstr>()) {
        // It's unlikely that all incoming operands of a large 'phi'
        // originate from an allocation call.
        if(phiInstr->OperandCount() > 4) {
            return false;
        }

        for(int i = 0; i < phiInstr->OperandCount(); i++) {
            if(OriginatesFromAlloc(phiInstr->GetOperand(i), 
                                   allocList, level + 1) == false) {
                return false;
            }
        }

        return true;
    }
    else if(auto questInstr = baseOp->DefiningInstrAs<QuestionInstr>()) {
        return OriginatesFromAlloc(questInstr->TrueOp(), allocList, level + 1) &&
               OriginatesFromAlloc(questInstr->FalseOp(), allocList, level + 1);
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AllocHelper::AreAllocCallsIndependent(AllocCallList* listA, 
										   AllocCallList* listB) {
	DebugValidator::IsNotNull(listA);
    DebugValidator::IsNotNull(listB);

    // Because we usually have only one/two calls in each list
    // we do a quadratic search.
    for(int i = 0; i < listB->Count(); i++) {
        if(listA->Contains((*listB)[i])) {
            return false;
        }
    }

    return true;
}

} // namespace Analysis