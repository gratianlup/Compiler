// GlobalUsersUpdater.hpp
// Copyright (c) Lup Gratian
//
// Implements a class that observes reference add/remove events
// and updates the associated 'GlobalUsersTag' objects.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_GLOBAL_USERS_UPDATER_HPP
#define PC_ANALYSIS_GLOBAL_USERS_UPDATER_HPP

#include "GlobalUsersTag.hpp"
#include "../IR/Function.hpp"
#include "../IR/References.hpp"
#include "../IR/ReferenceTable.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class GlobalUsersUpdater : public ReferenceObserver {
private:
	GlobalUsersTag* GetTag(Reference* reference) {
		if(auto functionRef = reference->As<FunctionReference>()) {
			return functionRef->Target()->GetTag<GlobalUsersTag>();
		}
		else if(auto variableRef = reference->As<VariableReference>()) {
			if(variableRef->IsGlobalVariableRef()) {
				return variableRef->GetGlobalVariable()->GetTag<GlobalUsersTag>();
			}
		}

		return nullptr;
	}

	virtual void UserAdded(Reference* reference, Symbol* user) override {
		if(auto tag = GetTag(reference)) {
			if(auto globalVariable = user->As<GlobalVariable>()) {
				tag->AddInitializerUser(globalVariable->GetReference());
			}
		}
	}

	virtual void UserAdded(Reference* reference, Instruction* user) override {
		if(auto tag = GetTag(reference)) {
			tag->AddFunctionUser(user->ParentFunction()->GetReference());
		}
	}

	virtual void UserRemoved(Reference* reference, Symbol* user) override {
		if(auto tag = GetTag(reference)) {
			if(auto globalVariable = user->As<GlobalVariable>()) {
				tag->RemoveInitializerUser(globalVariable->GetReference());
			}
		}
	}

	virtual void UserRemoved(Reference* reference, Instruction* user) override {
		if(auto tag = GetTag(reference)) {
			tag->RemoveFunctionUser(user->ParentFunction()->GetReference());
		}
	}
};

} // namespace Analysis
#endif