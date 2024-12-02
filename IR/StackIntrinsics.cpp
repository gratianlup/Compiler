// StackIntrinsics.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StackIntrinsics.hpp"

namespace IR {

StackTopIntr* StackTopIntr::GetStackTop(Unit* unit) {
	DebugValidator::IsNotNull(unit);
	
	// See if the intrinsic was already created.
	if(unit->Intrinsics().Contains("#stackTop")) {
		return static_cast<StackTopIntr*>(unit->Intrinsics().Get("#stackTop"));
	}

	// Create the function type and the intrinsic.
	auto returnType = unit->Types().GetPointer(IntegerType::GetInt8());
	auto functionType = unit->Types().GetFunction(returnType, nullptr, 0);
	auto intrinsic = new StackTopIntr(functionType, new string("#stackTop"), unit);
    intrinsic->SetIsNoState(true);
    intrinsic->SetIsNoIndirectWrite(true);
	intrinsic->SetIsNoIndirectRead(true);
	unit->Intrinsics().Add(intrinsic);
	return intrinsic;
}

// ######################################################################################
// IncStackTopIntr
// ######################################################################################
IncStackTopIntr* IncStackTopIntr::GetIncStackTop(Unit* unit) {
	DebugValidator::IsNotNull(unit);
	
	// See if the intrinsic was already created.
	if(unit->Intrinsics().Contains("#incStackTop")) {
		return static_cast<IncStackTopIntr*>(unit->Intrinsics().Get("#incStackTop"));
	}

	// Create the function type and the intrinsic.
	const Type* parameters[1];
	parameters[0] = IntegerType::GetInt64();

	auto functionType = unit->Types().GetFunction(VoidType::GetVoid(), parameters, 1);
	auto intrinsic = new IncStackTopIntr(functionType, new string("#incStackTop"), unit);
	intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("size"), intrinsic));
    intrinsic->SetIsNoState(true);
    intrinsic->SetIsNoIndirectWrite(true);
	intrinsic->SetIsNoIndirectRead(true);
	unit->Intrinsics().Add(intrinsic);
	return intrinsic;
}

// ######################################################################################
// RestoreStackTopIntr
// ######################################################################################
RestoreStackTopIntr* RestoreStackTopIntr::GetRestoreStackTop(Unit* unit) {
	DebugValidator::IsNotNull(unit);
	
	// See if the intrinsic was already created.
	if(unit->Intrinsics().Contains("#restoreStackTop")) {
		return static_cast<RestoreStackTopIntr*>(unit->Intrinsics().Get("#restoreStackTop"));
	}

	// Create the function type and the intrinsic.
	const Type* parameters[1];
	parameters[0] = unit->Types().GetPointer(IntegerType::GetInt8());

	auto functionType = unit->Types().GetFunction(VoidType::GetVoid(), parameters, 1);
	auto intrinsic = new RestoreStackTopIntr(functionType, new string("#restoreStackTop"), unit);
	intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("addr"), intrinsic));
    intrinsic->SetIsNoState(true);
    intrinsic->SetIsNoIndirectWrite(true);
	intrinsic->SetIsNoIndirectRead(true);
	unit->Intrinsics().Add(intrinsic);
	return intrinsic;
}

} // namespace IR