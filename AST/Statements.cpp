// Statements.cpp
// Copyright (c) Lup Gratian
//
// Implements the helper classes derived from 'Statement'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "Statements.hpp"
#include "Declaration.hpp"
#include "Expression.hpp"
using namespace Base;

namespace AST {

bool IfStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<IfStatement>()) {
		return cond_->Equals(t->cond_) &&
			   true_->Equals(t->true_) &&
			   false_->Equals(t->false_);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string IfStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("IfStatement: ");

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Condition:");
	sb.Append(cond_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("True:");
	sb.Append(true_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("False:");
	sb.Append(true_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// ForStatement
// ######################################################################################
bool ForStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<ForStatement>()) {
		return initStatement_->Equals(t->initStatement_) &&
			   condStatement_->Equals(t->condStatement_) &&
			   body_->Equals(t->body_) &&
			   incStatement_->Equals(t->incStatement_);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ForStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("IfStatement: ");

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Init:");
	sb.Append(initStatement_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Condition:");
	sb.Append(condStatement_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Increment:");
	sb.Append(incStatement_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Body:");
	sb.Append(body_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// WhileStatement
// ######################################################################################
bool WhileStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<WhileStatement>()) {
		return cond_->Equals(t->cond_) &&
			   body_->Equals(t->body_);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string WhileStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("WhileStatement: ");

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Condition:");
	sb.Append(cond_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Body:");
	sb.Append(body_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// DoStatement
// ######################################################################################
bool DoStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<DoStatement>()) {
		return cond_->Equals(t->cond_) &&
			   body_->Equals(t->body_);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string DoStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("DoStatement: ");

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Condition:");
	sb.Append(cond_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Body:");
	sb.Append(body_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// ContinueStatement
// ######################################################################################
bool ContinueStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<ContinueStatement>()) {
		return location_ == t->location_;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ContinueStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("DoStatement: ");
	return sb.ToString();
}

// ######################################################################################
// BreakStatement
// ######################################################################################
bool BreakStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<BreakStatement>()) {
		return location_ == t->location_;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string BreakStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("BreakStatement: ");
	return sb.ToString();
}

// ######################################################################################
// ReturnStatement
// ######################################################################################
bool ReturnStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<ReturnStatement>()) {
		return returnVal_->Equals(t->returnVal_);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ReturnStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("ReturnStatement: ");
	if(IsVoid()) sb.Append("Void");
	else {
		sb.AppendLine();
		sb.Append(string('\t', level + 1));
		sb.AppendLine("Return:");
		sb.Append(returnVal_->ToString(level + 1));
	}

	return sb.ToString();
}

// ######################################################################################
// LabelStatement
// ######################################################################################
bool LabelStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<LabelStatement>()) {
		return target_->Equals(t->target_) &&
			   *name_ == *t->name_;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string LabelStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("LabelStatement: ");
	sb.Append(name_->Name());

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Target:");
	sb.Append(target_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// CaseStatement
// ######################################################################################
bool CaseStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<CaseStatement>()) {
		return target_->Equals(t->target_) &&
			   value_->Equals(t->value_) &&
			   (isDefault_ == t->isDefault_);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string CaseStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("CaseStatement: ");
	if(isDefault_) sb.Append("Default");

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Target:");
	sb.Append(target_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// SwitchStatement
// ######################################################################################
bool SwitchStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<SwitchStatement>()) {
		if(cond_->Equals(t->cond_) == false) return false;
		if(caseList_.Count() != t->caseList_.Count()) return false;

		for(int i = 0; i < caseList_.Count(); i++) {
			if(caseList_[i]->Equals(t->caseList_[i]) == false) {
				return false;
			}
		}	  

		return true;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string SwitchStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("SwitchStatement: ");
	sb.AppendFormat(_T("%d"), caseList_.Count());
	if(HasDefault()) sb.Append(", has Default");

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Case list:");
	caseList_.ForEach([&sb, level](const shared<CaseStatement> statement) -> bool {
		sb.AppendLine(statement->ToString(level + 2));
		return true;
	});

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SwitchStatement::HasDefault() const {
	bool found = false;
	caseList_.ForEach([&found](const shared<CaseStatement>& statement) -> bool {
		if(statement->IsDefault()) {
			found = true;
			return false;
		}

		return true;
	});

	return found;
}

// ######################################################################################
// CompoundStatement
// ######################################################################################
bool CompoundStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<CompoundStatement>()) {
		if(list_.Count() != t->list_.Count()) return false;

		for(int i = 0; i < list_.Count(); i++) {
			if(list_[i]->Equals(t->list_[i]) == false) {
				return false;
			}
		}	  

		return true;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string CompoundStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("CompoundStatement: ");
	sb.AppendFormat(_T("%d"), list_.Count());

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("Children:");
	list_.ForEach([&sb, level](const shared<CaseStatement> statement) -> bool {
		sb.AppendLine(statement->ToString(level + 2));
		return true;
	});

	return sb.ToString();
}

// ######################################################################################
// NullStatement
// ######################################################################################
bool NullStatement::EqualsImpl(const Statement* other) const {
	return other->Is<NullStatement>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NullStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("NullStatement");
	return sb.ToString();
}

// ######################################################################################
// DeclarationStatement
// ######################################################################################
bool DeclarationStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<DeclarationStatement>()) {
		return base_->Equals(t->base_);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string DeclarationStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.AppendLine("DeclarationStatement");
	sb.Append(base_->ToString(level));
	return sb.ToString();
}

// ######################################################################################
// ExpressionStatement
// ######################################################################################
bool ExpressionStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<ExpressionStatement>()) {
		return base_->Equals(t->base_);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ExpressionStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.AppendLine("ExpressionStatement");
	sb.Append(base_->ToString(level));
	return sb.ToString();
}

// ######################################################################################
// GotoStatement
// ######################################################################################
bool GotoStatement::EqualsImpl(const Statement* other) const {
	if(auto t = other->As<GotoStatement>()) {
		return target_ == t->target_;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string GotoStatement::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.AppendLine("GotoStatement");
	sb.Append(target_->ToString(level));
	return sb.ToString();
}

} // namespace AST