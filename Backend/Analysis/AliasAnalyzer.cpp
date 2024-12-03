// AliasAnalyzer.cpp
// Copyright (c) Lup Gratian
//
// Implements the AliasAnalyzer class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "AliasAnalyzer.hpp"
#include "AliasInfo.hpp"

namespace Analysis {

bool AliasAnalyzer::RangesOverlap(AliasLocation& locationA, 
                                  AliasLocation& locationB) {
    DebugValidator::IsTrue(locationA.HasKnownSize());
    DebugValidator::IsTrue(locationB.HasKnownSize());

    __int64 lowA = locationA.Offset();
    __int64 highA = lowA + locationA.Size();
    __int64 lowB = locationB.Offset();
    __int64 highB = lowB + locationB.Size();
    
    return (lowA < highB) && (highA > lowB);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* AliasAnalyzer::GetBaseOperand(Operand* op) {
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
bool AliasAnalyzer::BaseIsVariable(Operand* op, VariableReference** variableRef) {
	DebugValidator::IsNotNull(op);

    if(auto varRef = GetBaseOperand(op)->As<VariableReference>()) {
        if(variableRef) {
            *variableRef = varRef;
        }

        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::BaseIsNonEscapedLocalVariable(Operand* op, VariableReference** variableRef) {
	DebugValidator::IsNotNull(op);

	if(auto varRef = GetBaseOperand(op)->As<VariableReference>()) {
		// Make sure the variable is local and it's address doesn't escape.
		if(varRef->IsLocalVariableRef() &&
		   varRef->GetLocalVariable()->IsNoEscape()) {
			if(variableRef) {
				*variableRef = varRef;
			}

			return true;
		}
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsNotGlobalVariable(Operand* op) {
	DebugValidator::IsNotNull(op);
	
	// Check if the base operand is a global variable or not.
	// If it is a parameter we try to use information collected 
	// by a previous interprocedural analysis pass.
	auto baseOp = GetBaseOperand(op);

	if(baseOp->IsGlobalVariableRef()) {
		return false;
	}
	else if(baseOp->IsLocalVariableRef() ||
			AllocHelper(GetLanguageInfo()).OriginatesFromAlloc(baseOp)) {
		return true;
	}
	else if(auto param = baseOp->As<Parameter>()) {
		//! TODO: implement
		//if(auto paramAliasTag = 
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsAddressNotTaken(Operand* op, VariableReference** variableRef) {
    DebugValidator::IsNotNull(op);

    // We look through address instructions until we find
    // the variable that acts as the base.
    auto baseOp = GetBaseOperand(op);

    if(auto baseVariableRef = baseOp->As<VariableReference>()) {
        if(baseVariableRef->IsAddressNotTaken()) {
            if(variableRef) {
                *variableRef = baseVariableRef;
            }

            return true;
        }
    }

    // Presume the address may be taken.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsAddressNotTaken(AliasLocation location, 
                                      VariableReference** variableRef) {
    if(location.AddressTaken() != AddressTakenType::Unknown) {
        return location.AddressTaken() == AddressTakenType::No;
    }

    bool isNotTaken = IsAddressNotTaken(location.Base());
    location.SetAddressTaken(isNotTaken ? AddressTakenType::No : 
										  AddressTakenType::Yes);
    return isNotTaken;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsUnion(const RecordType* recordType) {
    DebugValidator::IsNotNull(recordType);

    // Check if two adjacent fields have the same offset.
    for(int i = 1; i < recordType->FieldCount(); i++) {
        if(recordType->GetFieldOffset(i - 1) ==
           recordType->GetFieldOffset(i)) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::IsNonAggregatePointer(Operand* op) {
    op = GetBaseOperand(op);
    
    if(op->DefiningInstrIs<LoadInstr>() || 
       op->DefiningInstrIs<CallInstr>()) {
        return op->GetType()->IsPointer();
    }
    else return op->IsParameter() && 
                op->GetType()->IsPointer();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::AreParametersIndependent(Parameter* parameterA, 
                                             Parameter* parameterB) {
	DebugValidator::IsNotNull(parameterA);
	DebugValidator::IsNotNull(parameterB);

    if(parameterA == parameterB) {
        return false;
    }

    // If both parameters are marked 'restrict' we know 
    // that the programmer guarantees that they will never
    // point to the same memory locations.
    if(parameterA->IsRestrict() && parameterB->IsRestrict()) {
        return true;
    }

    //! TODO: consider interprocedural analysis result.
    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::WritesToGlobalVariables(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.WritesUnknownMemory()  ||
		   result.WritesIndirectMemory() ||
		   result.WritesGlobalMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsFromGlobalVariables(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.WritesUnknownMemory()  ||
		   result.WritesIndirectMemory() ||
		   result.WritesGlobalMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsAndWritesGlobalVariables(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.AccessesUnknownMemory()  ||
		   result.AccessesIndirectMemory() ||
		   (result.ReadsGlobalMemory() && result.WritesGlobalMemory());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsOrWritesGlobalVariables(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.AccessesUnknownMemory()  ||
		   result.AccessesIndirectMemory() ||
		   result.AccessesGlobalMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::WritesToParameters(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.WritesUnknownMemory() ||
		   result.WritesParametersMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsFromParameters(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.ReadsUnknownMemory() ||
		   result.ReadsParametersMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsAndWritesParameters(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.AccessesUnknownMemory() ||
		   (result.ReadsParametersMemory() && result.WritesParametersMemory());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsOrWritesParameters(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.AccessesUnknownMemory() ||
		   result.AccessesParametersMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsIndirectMemory(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.ReadsUnknownMemory() ||
		   result.ReadsIndirectMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::WritesIndirectMemory(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.WritesUnknownMemory() ||
		   result.WritesIndirectMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsAndWritesIndirectMemory(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.AccessesUnknownMemory() ||
		   (result.ReadsIndirectMemory() && result.WritesIndirectMemory());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::ReadsOrWritesIndirectMemory(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	auto result = ComputeCallEffects(instr);

	return result.AccessesUnknownMemory() ||
		   result.AccessesIndirectMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool AliasAnalyzer::DoesNotAccessMemory(CallInstr* instr) {
	DebugValidator::IsNotNull(instr);
	return ComputeCallEffects(instr).DoesNotAccessMemory();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* AliasAnalyzer::WithoutPointerCasts(Operand* op) {
    while(auto ptopInstr = op->DefiningInstrAs<PtopInstr>()) {
        op = ptopInstr->TargetOp();
    }

    return op;
}

} // namespace Analysis