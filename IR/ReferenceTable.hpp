// ReferenceTable.hpp
// Copyright (c) Lup Gratian
//
// Defines a table used to store reference operands.
// Provides factory methods for creating unique references.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_REFERENCE_TABLE_HPP
#define PC_IR_REFERENCE_TABLE_HPP

#include "IRType.hpp"
#include "References.hpp"
#include "../Base/String.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class Instruction;


// Base class for all reference event observers.
class ReferenceObserver {
public:
	virtual ~ReferenceObserver() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Method called when a reference is associated with a new user,
	// but the user is not known (it is not a instruction or symbol).
	virtual void UserAdded(Reference* reference) {}

	// Method called when a reference is associated with a symbol.
	virtual void UserAdded(Reference* reference, Symbol* user) {}

	// Method called when a reference is associated with an instruction.
	virtual void UserAdded(Reference* reference, Instruction* user) {}

	// Method called when a reference is removed from a user,
	// but the user is not known (it is not a instruction or symbol).
	virtual void UserRemoved(Reference* reference) {}

	// Method called when a reference is removed from a symbol.
	virtual void UserRemoved(Reference* reference, Symbol* user) {}

	// Method called when a reference is removed from a instruction.
	virtual void UserRemoved(Reference* reference, Instruction* user) {}

	// Method called when the reference has no usersa anymore
	// (it will be freed after all observers are notified).
	virtual void ReferenceReleased(Reference* reference) {}
};


class ReferenceTable {
private:
	Dictionary<Variable*, VariableReference*> variableRefs_;
	Dictionary<Variable*, VariableReference*> globalVariableRefs_;
	Dictionary<Function*, FunctionReference*> functRefs_;
	Dictionary<Block*, BlockReference*> blockRefs_;
	List<ReferenceObserver*> observers_;

public:
	~ReferenceTable();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for notifying the event observers.
	void UserAdded(Reference* reference);
	void UserAdded(Reference* reference, Symbol* user);
	void UserAdded(Reference* reference, Instruction* user);
	void UserRemoved(Reference* reference);
	void UserRemoved(Reference* reference, Instruction* user);
	void UserRemoved(Reference* reference, Symbol* user);
	void ReferenceReleased(Reference* reference);

	// Methods for creating references.
	VariableReference* GetVariableRef(Variable* variable, const Type* type);
	VariableReference* GetGlobalVariableRef(GlobalVariable* variable, const Type* type);
	FunctionReference* GetFunctionRef(Function* function, const Type* type);
	BlockReference* GetBlockRef(Block* block);

	// Methods for releasing references.
	void ReleaseVariableRef(VariableReference* reference);
	void ReleaseFunctionRef(FunctionReference* reference);
	void ReleaseBlockRef(BlockReference* reference);

	// Associates the specified observer object with the reference table.
	void AddObserver(ReferenceObserver* observer) {
		DebugValidator::IsNotNull(observer);
		observers_.Add(observer);
	}

	// Returns the number of observers associated with the reference table.
	int ObserverCount() const {
		return observers_.Count();
	}

	// Returns the observer found at the specified position.
	ReferenceObserver* GetObserver(int index) {
		return observers_[index];
	}

	const ReferenceObserver* GetObserver(int index) const {
		return observers_[index];
	}

	// Removes the specified observer from the list.
	void RemoveObserver(ReferenceObserver* observer) {
		DebugValidator::IsNotNull(observer);
		observers_.Remove(observer);
	}

	// Removes the observer found at the specified position from the list.
	void RemoveObserver(int index) {
		observers_.RemoveAt(index);
	}

	// Removes all asociated observers.
	void ClearObserver() {
		observers_.Clear();
	}
};

} // namespace IR
#endif