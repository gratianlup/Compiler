// MemoryIntrinsics.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "MemoryIntrinsics.hpp"

namespace IR {

CopyMemoryIntr* CopyMemoryIntr::GetCopyMemory(Unit* unit) {
	DebugValidator::IsNotNull(unit);
	
	// See if the intrinsic was already created.
	if(unit->Intrinsics().Contains("#copyMemory")) {
		return static_cast<CopyMemoryIntr*>(unit->Intrinsics().Get("#copyMemory"));
	}

	// Create the intrinsic now. The function has the following form:
	// funct copyMemory(var dest int8*, var src int8*, var len int64) : void
	const Type* parameters[3];
	parameters[0] = unit->Types().GetPointer(IntegerType::GetInt8());
	parameters[1] = unit->Types().GetPointer(IntegerType::GetInt8());
	parameters[2] = IntegerType::GetInt64();
		
	// Create the function type and the intrinsic.
	auto functionType = unit->Types().GetFunction(VoidType::GetVoid(), parameters, 3);
	auto intrinsic = new CopyMemoryIntr(functionType, new string("#copyMemory"), unit);
    intrinsic->SetIsNoState(true);
	unit->Intrinsics().Add(intrinsic);

	// Add the parameters.
	intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("dest"), intrinsic));
    intrinsic->Parameters()[0]->SetIsNoEscape(true);
	
    intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("src"), intrinsic));
    intrinsic->Parameters()[1]->SetIsNoWrite(true);
    intrinsic->Parameters()[1]->SetIsNoEscape(true);

	intrinsic->AddParameter(Variable::GetVariable(parameters[2], new string("len"), intrinsic));
    intrinsic->Parameters()[2]->SetIsNoWrite(true);
    intrinsic->Parameters()[2]->SetIsNoEscape(true);
	return intrinsic;
}

// ######################################################################################
// SetMemoryIntr
// ######################################################################################
SetMemoryIntr* SetMemoryIntr::GetSetMemory(Unit* unit) {
	DebugValidator::IsNotNull(unit);
	
	// See if the intrinsic was already created.
	if(unit->Intrinsics().Contains("#setMemory")) {
		return static_cast<SetMemoryIntr*>(unit->Intrinsics().Get("#setMemory"));
	}

	// Create the intrinsic now. The function has the following form:
	// funct setMemory(var dest int8*, var val int8, var len int64) : void
	const Type* parameters[3];
	parameters[0] = unit->Types().GetPointer(IntegerType::GetInt8());
	parameters[1] = IntegerType::GetInt8();
	parameters[2] = IntegerType::GetInt64();
		
	// Create the function type and the intrinsic.
	auto functionType = unit->Types().GetFunction(VoidType::GetVoid(), parameters, 3);
	auto intrinsic = new SetMemoryIntr(functionType, new string("#setMemory"), unit);
	unit->Intrinsics().Add(intrinsic);

	// Add the parameters.
	intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("dest"), intrinsic));
	intrinsic->Parameters()[0]->SetIsNoEscape(true);

    intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("val"), intrinsic));
    intrinsic->Parameters()[1]->SetIsNoWrite(true);
    intrinsic->Parameters()[1]->SetIsNoEscape(true);
	
    intrinsic->AddParameter(Variable::GetVariable(parameters[2], new string("len"), intrinsic));
    intrinsic->Parameters()[2]->SetIsNoWrite(true);
    intrinsic->Parameters()[2]->SetIsNoEscape(true);
	return intrinsic;
}

// ######################################################################################
// PrefetchIntr
// ######################################################################################
PrefetchIntr* PrefetchIntr::GetPrefetch(Unit* unit) {
	DebugValidator::IsNotNull(unit);
	
	// See if the intrinsic was already created.
	if(unit->Intrinsics().Contains("#prefetch")) {
		return static_cast<PrefetchIntr*>(unit->Intrinsics().Get("#prefetch"));
	}

	// Create the intrinsic now. The function has the following form:
	// funct prefetch(var addr int8*, var hint int32) : void
	const Type* parameters[2];
	parameters[0] = unit->Types().GetPointer(IntegerType::GetInt8());
	parameters[1] = IntegerType::GetInt32();
		
	// Create the function type and the intrinsic.
	auto functionType = unit->Types().GetFunction(VoidType::GetVoid(), parameters, 2);
	auto intrinsic = new PrefetchIntr(functionType, new string("#prefetch"), unit);
	unit->Intrinsics().Add(intrinsic);

	// Add the parameters.
	intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("addr"), intrinsic));
    intrinsic->Parameters()[1]->SetIsNoWrite(true);
    intrinsic->Parameters()[1]->SetIsNoEscape(true);
	intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("hint"), intrinsic));
	return intrinsic;
}

} // namespace IR