// DeclarationContext.hpp	
// Copyright (c) Lup Gratian
//
// Implements the DeclarationContext class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DeclarationContext.hpp"
#include "Declaration.hpp"

namespace AST {

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeclarationContext* DeclarationContext::ParentBlock() {
	// The global scope has no parent.
	if(IsFileScope()) {
		return this;
	}

	DC* context = parent_;

	while(context && (context->IsBlockScope() == false) && // Stop at the first block
		  (context->IsFunctionScope() == false) &&         // or at the first function block.
		  (context->IsFileScope() == false)) {
		context = context->parent_;
	}

	return context;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeclarationContext* DeclarationContext::ParentLoop(bool self) {
	if(self && IsBlockScope() && (Flags() == BlockFlags::Loop)) return this;
	DC* context = parent_;

	while(context && (context->Flags() != BlockFlags::Loop) &&
		  (context->IsFileScope() == false)) {
		context = context->parent_;
	}

	return context;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeclarationContext* DeclarationContext::ParentSwitch(bool self) {
	if(self && IsBlockScope() && (Flags() == BlockFlags::Switch)) return this;
	DC* context = parent_;

	while(context && (context->Flags() != BlockFlags::Switch) &&
		  (context->IsFileScope() == false)) {
		context = context->parent_;
	}

	return context;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeclarationContext* DeclarationContext::ParentFunction(bool self) {
	if(self && IsFunctionScope()) return this;
	DC* context = parent_;

	while(context && (context->IsFunctionScope() == false) && 
		(context->IsFileScope() == false)) {
		context = context->parent_;
	}

	return context;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DeclarationContext* DeclarationContext::FileContext() {
	if(IsFileScope()) return this;
	DC* context = parent_;

	while(context && (context->IsFileScope() == false)) {
		context = context->parent_;
	}

	return context;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int DeclarationContext::Depth() const {
	int ct = 0;
	DC* context = parent_;

	while(context && (context->IsFileScope() == false)) {
		ct++;
		context = context->parent_;
	}

	return ct;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationContext::IsAncestor(DC* other) const {
	if(other == this) {
        return false;
    }

	while(other) {
		if(other == this) {
			return true; // It is an ancestor.
		}

		other = other->parent_;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<LabelStatement> DeclarationContext::FindLabel(Identifier* id, DC** parentDC) {
	DC* context = this;

	while(context && (context->scope_ != ScopeType::Function)) {
		context = context->parent_;
	}

	if(context) {
		// Found a function context.
		shared<LabelStatement> temp;
		if(context->labels_.TryGetValue(id, &temp)) {
			if(parentDC) {
				*parentDC = context;
			}

			return temp; // Found.
		}
	}
		
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> DeclarationContext::FindTag(Identifier* id, DC** parentDC, bool all) {
	DC* context = this;
	shared<Declaration> temp;

	if(all) {
		// Search all contexts until something is found.
		do {
			if(context->tables_[(int)NamespaceType::Tag].TryGetValue(id, &temp)) {
				if(parentDC) *parentDC = context;
				return temp; // Found.
			}

			context = context->parent_;
		} while(context);
	}
	else {
		// Only this context should be searched.
		if(tables_[(int)NamespaceType::Tag].TryGetValue(id, &temp)) {
			if(parentDC) {
				*parentDC = this;
			}

			return temp;
		}
	}
		
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> DeclarationContext::FindMember(Identifier* id, DC** parentDC) {
	shared<Declaration> temp;

	if(tables_[(int)NamespaceType::Other].TryGetValue(id, &temp)) {
		if(parentDC) {
			*parentDC = this;
		}

		return temp;
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> DeclarationContext::Find(Identifier* id, DC** parentDC, bool all) {
	DC* context = this;
	shared<Declaration> temp;

	if(all) { 
		do {
			if(context->tables_[(int)NamespaceType::Other].TryGetValue(id, &temp)) {
				if(parentDC) {
					*parentDC = context;
				}

				return temp; // Found.
			}

			context = context->parent_;
		} while(context);
	}
	else {
		// Only this context should be searched.
		if(tables_[(int)NamespaceType::Other].TryGetValue(id, &temp)) {
			if(parentDC) {
				*parentDC = this;
			}

			return temp;
		}
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclarationContext::AddLabel(Identifier* id, shared<LabelStatement> label) {
	DebugValidator::IsTrue(IsFunctionScope());
	labels_.Add(id, label);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclarationContext::AddTag(Identifier* id, shared<Declaration> declaration) {
	DebugValidator::IsFalse(IsFunctProtoScope());
	tables_[(int)NamespaceType::Tag].Add(id, declaration);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclarationContext::Add(Identifier* id, shared<Declaration> declaration) {
	tables_[(int)NamespaceType::Other].Add(id, declaration);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string DeclarationContext::ToString(int level) const {
	StringBuilder sb;

	// Enumerate over all declarations and print them.
	sb.AppendFormat(_T("## STANDARD - %d ## "), tables_[(int)NamespaceType::Other].Count());
	tables_[(int)NamespaceType::Other].ForEach([&sb](Table::TPair& pair) -> bool {
		sb.AppendLine(pair.Value->ToString(0));
		sb.AppendLine();
		return true;
	});

	sb.AppendFormat(_T("\n## TAGS - %d ## "), tables_[(int)NamespaceType::Tag].Count());
	tables_[(int)NamespaceType::Tag].ForEach([&sb](Table::TPair& pair) -> bool {
		sb.AppendLine(pair.Value->ToString(0));
		sb.AppendLine();
		return true;
	});

	return sb.ToString();
}

} // namespace AST