// ReferenceTable.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ReferenceTable.hpp"

namespace IR {

ReferenceTable::~ReferenceTable() {
	// Deallocate all remaining references.
	// Actually all these should be empty; if not it means that there are memory leaks.
	variableRefs_.ForEachValue([](VariableReference* reference) -> bool {
		delete reference;
		return true;
	});

	globalVariableRefs_.ForEachValue([](VariableReference* reference) -> bool {
		delete reference;
		return true;
	});

	functRefs_.ForEachValue([](FunctionReference* reference) -> bool {
		delete reference;
		return true;
	});

	blockRefs_.ForEachValue([](BlockReference* reference) -> bool {
		delete reference;
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VariableReference* ReferenceTable::GetVariableRef(Variable* variable, const Type* type) {
	DebugValidator::IsNotNull(variable);
	DebugValidator::IsNotNull(type);

	// See if a reference for this variable was already created.
	VariableReference* reference;

	if(variableRefs_.TryGetValue(variable, &reference)) {
		return reference;
	}

	// Create the reference now.
	reference = new VariableReference(variable, type, this);
	variableRefs_.Add(variable, reference);
	return reference;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VariableReference* ReferenceTable::GetGlobalVariableRef(GlobalVariable* variable, 
														const Type* type) {
	DebugValidator::IsNotNull(variable);
	DebugValidator::IsNotNull(type);

	// See if a reference for this global variable was already created.
	VariableReference* reference;

	if(globalVariableRefs_.TryGetValue(variable, &reference)) {
		return reference;
	}

	// Create the reference now.
	reference = new VariableReference(variable, type, this);
	globalVariableRefs_.Add(variable, reference);
	return reference;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FunctionReference* ReferenceTable::GetFunctionRef(Function* function, const Type* type) {
	DebugValidator::IsNotNull(function);
	DebugValidator::IsNotNull(type);

	// See if a reference for this function was already created.
	FunctionReference* reference;

	if(functRefs_.TryGetValue(function, &reference)) {
		return reference;
	}

	// Create the reference now.
	reference = new FunctionReference(function, type, this);
	functRefs_.Add(function, reference);
	return reference;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* ReferenceTable::GetBlockRef(Block* block) {
	DebugValidator::IsNotNull(block);

	// See if a reference for this block was already created.
	BlockReference* reference;

	if(blockRefs_.TryGetValue(block, &reference)) {
		return reference;
	}

	// Create the reference now.
	reference = new BlockReference(block, this);
	blockRefs_.Add(block, reference);
	return reference;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::ReleaseVariableRef(VariableReference* reference) {
	DebugValidator::IsNotNull(reference);
	DebugValidator::IsTrue(variableRefs_.ContainsKey(reference->GetLocalVariable()) ||
						   globalVariableRefs_.ContainsKey(reference->GetGlobalVariable()));
	ReferenceReleased(reference);

	if(reference->IsLocalVariableRef()) {
		variableRefs_.Remove(reference->GetLocalVariable());
	}
	else globalVariableRefs_.Remove(reference->GetGlobalVariable());

	// Now it's safe to delete the reference.
	delete reference;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::ReleaseFunctionRef(FunctionReference* reference) {
	DebugValidator::IsNotNull(reference);
	DebugValidator::IsTrue(functRefs_.ContainsKey(reference->Target()));
	
	Function* function = reference->Target();
	delete reference;
	functRefs_.Remove(function);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::ReleaseBlockRef(BlockReference* reference) {
	DebugValidator::IsNotNull(reference);
	DebugValidator::IsTrue(blockRefs_.ContainsKey(reference->Target()));
	
	Block* block = reference->Target();
	delete reference;
	blockRefs_.Remove(block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::UserAdded(Reference* reference) {
	observers_.ForEach([reference](ReferenceObserver* observer) -> bool {
		observer->UserAdded(reference);
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::UserAdded(Reference* reference, Instruction* user) {
	observers_.ForEach([reference, user](ReferenceObserver* observer) -> bool {
		observer->UserAdded(reference, user);
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::UserAdded(Reference* reference, Symbol* user) {
	observers_.ForEach([reference, user](ReferenceObserver* observer) -> bool {
		observer->UserAdded(reference, user);
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::UserRemoved(Reference* reference) {
	observers_.ForEach([reference](ReferenceObserver* observer) -> bool {
		observer->UserRemoved(reference);
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::UserRemoved(Reference* reference, Instruction* user) {
	observers_.ForEach([reference, user](ReferenceObserver* observer) -> bool {
		observer->UserRemoved(reference, user);
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::UserRemoved(Reference* reference, Symbol* user) {
	observers_.ForEach([reference, user](ReferenceObserver* observer) -> bool {
		observer->UserRemoved(reference, user);
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ReferenceTable::ReferenceReleased(Reference* reference) {
	observers_.ForEach([reference](ReferenceObserver* observer) -> bool {
		observer->ReferenceReleased(reference);
		return true;
	});
}

} // namespace IR