// SimpleAliasAnalysis.cpp
// Copyright (c) Lup Gratian
//
// Implements the SimpleAliasAnalysis class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "SimpleAliasAnalysis.hpp"

namespace Analysis {

AliasResult SimpleAliasAnalysis::ComputeAlias(AliasLocation locationA, 
                                              AliasLocation locationB) {
    // Check the case when the base operands are the same.
    if(locationA.Base() == locationB.Base()) {
        // If the locations have the same base operand and offset
        // we definitely have a "Must" alias.
        if(locationA.Offset() == locationB.Offset()) {
            return AliasResult::Must;
        }
        else if(locationA.HasKnownSize() && locationB.HasKnownSize() &&
                (RangesOverlap(locationA, locationB) == false)) {
            return AliasResult::None;
        }

        return AliasResult::May;
    }

    // Simple rules that apply for variables.
    auto result = ComputeVariableAlias(locationA, locationB);

    if(result != AliasResult::May) {
        return result;
    }

    // Simple rules for non-escaped variables.
    result = ComputeNonEscapedAlias(locationA, locationB);

    if(result != AliasResult::May) {
        return result;
    }

    // Simple rules that apply for at least one parameter.
    result = ComputeParameterAlias(locationA, locationB);
    
    if(result != AliasResult::May) {
        return result;
    }

    // Simple rules that apply for array/record access.
    result = ComputeAggregateAlias(locationA, locationB);

    if(result != AliasResult::May) {
        return result;
    }

    // Rules which consider 'phi' and 'quest' instructions.
    return ComputePhiQuestionAlias(locationA, locationB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeCallAlias(CallInstr* instr, 
                                                  AliasLocation location) {
    // Here we handle calls to built-in intrinsics.
    if(auto intrinsic = instr->GetIntrinsic()) {
        // Math/bitwise/stack/prefetch intrinsics don't access memory at all.
        if(intrinsic->IsMathIntrinsic()    ||
           intrinsic->IsBitwiseIntrinsic() ||
           intrinsic->IsStackIntrinsic()   ||
           intrinsic->Is<PrefetchIntr>()) {
			return AliasResult::None;
        }
        else if(intrinsic->Is<SetMemoryIntr>() ||
                intrinsic->Is<CopyMemoryIntr>()) {
            // Aliasing depends only on the destination location
            // and on the source location for 'copyMemory'.
            __int64 size = AliasLocation::GetUnknown();
            auto lengthOp = intrinsic->Is<SetMemoryIntr>() ?
                            SetMemoryIntr::GetLength(instr) :
                            CopyMemoryIntr::GetLength(instr);

            if(auto intConst = lengthOp->As<IntConstant>()) {
                // If a constant number of bytes is copied we might get 
                // a better result. For example, if we have
                // int a[8]; memset(a, 0, 4); a[2] = 1;
                // the 'memset' and 'a[2]' don't alias.
                size = intConst->Value();
            }

            auto destinationOp = intrinsic->Is<SetMemoryIntr>() ?
                                 SetMemoryIntr::GetDestination(instr) :
                                 CopyMemoryIntr::GetDestination(instr);

            auto result = Parent()->ComputeAlias(destinationOp, 0, size,
                                                 location.Base(), location.Offset(), 
                                                 location.Size());
            if(result != AliasResult::None) {
				return AliasResult::May;
            }
            else if(intrinsic->Is<SetMemoryIntr>()) {
				return AliasResult::None;
            }

            // The 'copyMemory' intrinsic might read from the location.
            return Parent()->ComputeAlias(CopyMemoryIntr::GetSource(instr), 0, size,
                                          location.Base(), location.Offset(), 
                                          location.Size());
        }
    }

    // Presume the call might modify the location.
    return AliasResult::May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeSingleVariableAlias(VariableReference* variableRef, 
                                                            Operand* other) {
    if(other->IsParameter()) {
        // A local variable can't alias a parameter, even if we have
        // recursive calls (the variable has multiple "instances" on the stack).
        if(variableRef->IsLocalVariableRef()) {
            return AliasResult::None;
        }

        // A global variable can't alias a parameter if it's marked
        // as being non-address-taken.
        if(variableRef->IsGlobalVariableRef() && 
           variableRef->IsAddressNotTaken()) {
            return AliasResult::None;
        }
    }

    // A non-address-taken global variable can't alias any pointer
    // if the pointer doesn't originate from it.
    if(variableRef->IsGlobalVariableRef() && 
       variableRef->IsAddressNotTaken()) {
        if(auto baseOperand = GetBaseOperand(other)) {
            if(baseOperand != variableRef) {
                return AliasResult::None;
            }
            else return AliasResult::Must;
        }
    }

    // A variable can't alias the address returned
    // by a call to a memory allocation routine ('malloc', for example).
    if(AllocHelper(GetLanguageInfo()).OriginatesFromAlloc(other)) {
        return AliasResult::None;
    }

    return AliasResult::May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeVariableAlias(AliasLocation& locationA, 
                                                      AliasLocation& locationB) {
    // If both base operands are local/global variables we can determine
    // the alias relation of the locations. Note that the offsets
    // don't matter because no language standard guarantees an order
    // in memory for the variables, making it (theoretically) impossible
    // to access a variable knowing the address of another one.
    auto variableRefA = locationA.Base()->As<VariableReference>();
    auto variableRefB = locationB.Base()->As<VariableReference>();

    if(variableRefA && variableRefB) {
        // If the variables are different there can't be any alias
        // under standard-conforming code. This also handles the case
        // of local/global variables.
        if(variableRefA != variableRefB) {
            return AliasResult::None;
        }

        // Even if the variable are the same, if the accessed regions
        // don't overlap then we have no alias.
        if(locationA.HasKnownSize() && locationB.HasKnownSize()) {
            if(RangesOverlap(locationA, locationB)) {
                return AliasResult::Must;
            }
            else return AliasResult::None;
        }

        return AliasResult::May;
    }

	// If the base operands are two completely different variables
	// (local or global) there can be no alias.
	auto baseOpA = GetBaseOperand(locationA.Base());
	auto baseOpB = GetBaseOperand(locationB.Base());

	if((baseOpA && baseOpB) &&
	   (baseOpA->IsVariableReference() && baseOpB->IsVariableReference()) &&
	   (baseOpA != baseOpB)) {
		return AliasResult::None;
	}

    // If only one of the base operand is a variable, depending
    // on the other operand we might conclude there is no alias.
    auto variableRef = variableRefA ? variableRefA : variableRefB;
    auto other = variableRefA ? locationB.Base() : locationA.Base();

    if(variableRef) {
        return ComputeSingleVariableAlias(variableRef, other);
    }
        
    return AliasResult::May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeNonEscapedAlias(AliasLocation& locationA, 
                                                        AliasLocation& locationB) {
    bool nonAggPointerA = IsNonAggregatePointer(locationA);
    bool nonAggPointerB = IsNonAggregatePointer(locationB);

    if((nonAggPointerA && nonAggPointerB) == false) {
        auto nonEscapedCandidate = nonAggPointerA ? &locationB : &locationA;
        auto baseOp = GetBaseOperand(nonEscapedCandidate->Base());

        if(auto variableRef = baseOp->As<VariableReference>()) {
            if(variableRef->IsAddressNotTaken() ||
               variableRef->IsNoEscape()) {
                return AliasResult::None;
            }
        }
    }

    return AliasResult::May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeParameterAlias(AliasLocation& locationA, 
                                                       AliasLocation& locationB) {
    // If both base operands are parameters marked 'restrict'
    // we know that the programmer guarantees that they will never
    // point to the same memory locations.
    auto parameterA = locationA.Base()->As<Parameter>();
    auto parameterB = locationB.Base()->As<Parameter>();

    if(parameterA && parameterB) {
        if(AreParametersIndependent(parameterA, parameterB)) {
            return AliasResult::None;
        }
    }

    // If one of the base operand is a parameter and the other one
    // is a global constant we know we have no alias
    // (the parameter cannot point to the global constant,
    //  otherwise the variable would have not been considered a constant).
    auto parameter = parameterA ? parameterA : parameterB;
    auto other = parameterA ? locationB.Base() : locationA.Base();

    if(parameter) {
        if(Parent()->ReadsFromGlobalConstant(other)) {
            return AliasResult::None;
        }
    }

    return AliasResult::May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeAggregateAlias(AliasLocation& locationA, 
                                                       AliasLocation& locationB) {
    // If both locations originate from different non-address-taken 
    // aggregates we can't have any alias.
    VariableReference* variableRefA;
    VariableReference* variableRefB;
    bool notAddressTakenA = IsAddressNotTaken(locationA.Base(), &variableRefA);
    bool notAddressTakenB = IsAddressNotTaken(locationB.Base(), &variableRefB);

    if(notAddressTakenA && notAddressTakenB) {
        if(variableRefA != variableRefB) {
            return AliasResult::None;
        }

        // If the variables are the same we're sure there is no alias
        // if a location is a record field and the other an array element.
        // Note that the record must not be a C 'union'.
        auto indexInstr = locationA.Base()->DefiningInstrIs<IndexInstr>() ?
                          locationA.Base()->DefiningInstrAs<IndexInstr>() :
                          locationB.Base()->DefiningInstrAs<IndexInstr>();
        auto fieldInstr = locationA.Base()->DefiningInstrIs<FieldInstr>() ?
                         locationA.Base()->DefiningInstrAs<FieldInstr>() :
                         locationB.Base()->DefiningInstrAs<FieldInstr>();

        if((indexInstr && fieldInstr) &&
            (IsUnion(fieldInstr->GetRecordType()) == false)) {
            return AliasResult::None;
        }
    }

    // If one of the location is a non-address-taken variable and
    // the other one is a non-aggregate pointer we have no alias
    // (it is not possible for the pointer to point to any part
    //  of the non-address-taken variable, otherwise it would have 
    //  been marked as address-taken).
    if((notAddressTakenA && IsNonAggregatePointer(locationB.Base())) ||
       (notAddressTakenB && IsNonAggregatePointer(locationA.Base()))) {
        return AliasResult::None;
    }

    return AliasResult::May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputeAllocAlias(AliasLocation& locationA, 
                                                   AliasLocation& locationB) {
    // Check if the locations originated from different
    // dynamic allocation function calls. Example:
    // RECORD* p1 = (RECORD*)malloc(sizeof(RECORD));
    // RECORD* p2 = (RECORD*)malloc(sizeof(RECORD));
    // 'p1->a' does not alias 'p2->a' (we're allowed to presume
    // the memory allocator is not faulty).
    auto baseA = GetBaseOperand(locationA.Base());
    auto baseB = GetBaseOperand(locationB.Base());

    List<CallInstr*> callsA;
    List<CallInstr*> callsB;
	AllocHelper allocHelper(GetLanguageInfo());
    bool fromAllocA = allocHelper.OriginatesFromAlloc(baseA, &callsA);
    bool fromAllocB = allocHelper.OriginatesFromAlloc(baseB, &callsB);

    if(fromAllocA && fromAllocB) {
        if(allocHelper.AreAllocCallsIndependent(&callsA, &callsB)) {
            return AliasResult::None;
        }

        // Even if the calls are not independent there is no alias
        // if the accessed ranges do not overlap.
        if(locationA.HasKnownSize() && locationB.HasKnownSize()) {
           if(RangesOverlap(locationA, locationB)) {
                return AliasResult::Must;
           }
           else return AliasResult::None;
        }
    }

    return AliasResult::May;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
AliasResult SimpleAliasAnalysis::ComputePhiQuestionAlias(AliasLocation& locationA, 
                                                         AliasLocation& locationB) {
    // Try to determine the alias relation between pairs
    // of 'phi'/'quest' instructions or a single 'phi'/'quest'
    // and other operands. Make a list of all possible operands
    // and if they are not too many compare each possible pair.
    AliasLocationList listA;
    AliasLocationList listB;
    bool hasPhiQuestA = false;
    bool hasPhiQuestB = false;

    bool validA = CollectPhiQuestionOperands(locationA, listA, hasPhiQuestA);
    bool validB = CollectPhiQuestionOperands(locationB, listB, hasPhiQuestB);

    if((validA && validB) == false) {
        return AliasResult::May;
    }

    // At least one of the locations should be
    // represented by a 'phi'/'quest'.
    if((hasPhiQuestA || hasPhiQuestB) == false) {
        return AliasResult::May;
    }

    // If both tested locations are variable references
    // determine the alias directly, it's faster.
    for(int i = 0; i < listA.Count(); i++) {
        for(int j = 0; j < listB.Count(); j++) {
            auto variableRefA = listA[i].Base()->As<VariableReference>();
            auto variableRefB = listB[j].Base()->As<VariableReference>();

            if(variableRefA && variableRefB) {
                if(variableRefA == variableRefB) {
                    return AliasResult::May;
                }
            }
            else if(Parent()->ComputeAlias(listA[i], listB[j]) != AliasResult::None) {
                return AliasResult::May;
            }
        }
    }

    return AliasResult::None;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SimpleAliasAnalysis::CollectPhiQuestionOperands(AliasLocation& location, 
                                                     AliasLocationList& list,
                                                     bool& hasPhiQuestion) {
    // Add to the list all 'phi' incoming operands or 'quest'
    // true/false operands, but only id testing their alias 
    // cannot end in an infinite loop (a 'phi' can create a loop
    // if we have an incremented pointer, for example).
    if(auto phiInstrA = location.Base()->DefiningInstrAs<PhiInstr>()) {
        if(phiInstrA->OperandCount() > 6) {
            // Too many operands, unlikely to have a no alias result.
            return false;
        }
        else hasPhiQuestion = true;

        for(int i = 0; i < phiInstrA->OperandCount(); i++) {
            auto incomingOp = phiInstrA->GetOperand(i);

            if(IsSafePhiOperand(incomingOp) == false) {
                // We might enter a 'phi' loop, so give up.
                return false;
            }

            list.Add(AliasLocation(incomingOp, location.Offset(), 
                                   location.Size()));
        }
    }
    else if(auto questInstrA = location.Base()->DefiningInstrAs<QuestionInstr>()) {
        hasPhiQuestion = true;
        list.Add(AliasLocation(questInstrA->TrueOp(), location.Offset(), 
                               location.Size()));
        list.Add(AliasLocation(questInstrA->FalseOp(), location.Offset(), 
                               location.Size()));
    }
    else {
        hasPhiQuestion = false;
        list.Add(location);
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SimpleAliasAnalysis::IsSafePhiOperand(Operand* op) {
    auto baseOp = GetBaseOperand(op);

    return baseOp && (baseOp->IsVariableReference() ||
                      baseOp->IsParameter()         ||
                      baseOp->DefiningInstrIs<CallInstr>());
}

} // namespace Analysis