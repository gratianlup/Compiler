// Expressions.cpp
// Copyright (c) Lup Gratian
//
// Implements the helper classes derived from 'Expression'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Expressions.hpp"
#include "Type.hpp"
#include "Types.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace AST {

bool UnaryOperator::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<UnaryOperator>()) {
		return (type_ == t->type_) && (value_ == t->value_) &&
				Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string UnaryOperator::OperatorString(UnaryOpType op) {
	switch(op) {
		case UnaryOpType::Inc:         { return "++"; }
		case UnaryOpType::Dec:         { return "--"; }
		case UnaryOpType::Address:     { return "&";  }
		case UnaryOpType::Indirection: { return "*";  }
		case UnaryOpType::Add:         { return "+";  }
		case UnaryOpType::Sub:         { return "-";  }
		case UnaryOpType::Complement:  { return "~";  }
		case UnaryOpType::Not:         { return "!";  }
	}

	DebugValidator::Unreachable();
	return "";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string UnaryOperator::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("UnaryOperator: ");
	sb.Append(UnaryOperator::OperatorString(type_));
	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Expr:");
	sb.Append(value_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// BinaryOperator
// ######################################################################################
bool BinaryOperator::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<BinaryOperator>()) {
		return (type_ == t->type_) && (leftVal_ == t->leftVal_) &&
			   (rightVal_ == t->rightVal_) && Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int BinaryOperator::Precedence() const {
	switch(type_) {
		case BinaryOpType::Comma:      return 2;  // ,
		case BinaryOpType::Eq:                    // =
		case BinaryOpType::AddEq:                 // +=
		case BinaryOpType::SubEq:                 // -=
		case BinaryOpType::MulEq:                 // *=
		case BinaryOpType::DivEq:                 // /=
		case BinaryOpType::ModEq:                 // %=
		case BinaryOpType::AndEq:                 // &=
		case BinaryOpType::OrEq:                  // |=
		case BinaryOpType::XorEq:                 // =
		case BinaryOpType::ShiftLEq:              // <<=
		case BinaryOpType::ShiftREq:   return 3;  // >>=
		case BinaryOpType::OrOr:       return 5;  // ||
		case BinaryOpType::AndAnd:     return 6;  // &&
		case BinaryOpType::Or:         return 7;  // |
		case BinaryOpType::Xor:        return 8;  // 
		case BinaryOpType::And:        return 9;  // &
		case BinaryOpType::EqEq:                  // ==
		case BinaryOpType::NotEq:      return 10; // !=
		case BinaryOpType::Less:                  // <
		case BinaryOpType::LessEq:                // <=
		case BinaryOpType::Greater:               // >
		case BinaryOpType::GreaterEq:  return 11; // >=
		case BinaryOpType::ShiftR:                // >>
		case BinaryOpType::ShiftL:     return 12; // <<
		case BinaryOpType::Add:                   // +
		case BinaryOpType::Sub:        return 13; // -
		case BinaryOpType::Mul:                   // *
		case BinaryOpType::Div:                   // /
		case BinaryOpType::Mod:        return 14; // %
	}

	DebugValidator::Unreachable();
	return 0;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string BinaryOperator::OperatorString(BinaryOpType op) {
	switch(op) {
		case BinaryOpType::Add:       { return "+";   }
		case BinaryOpType::Sub:       { return "-";   }
		case BinaryOpType::Mul:       { return "*";   }
		case BinaryOpType::Div:       { return "/";   }
		case BinaryOpType::Mod:       { return "%";   }
		case BinaryOpType::Eq:        { return "=";   }
		case BinaryOpType::AddEq:     { return "+=";  }
		case BinaryOpType::SubEq:     { return "-=";  }
		case BinaryOpType::MulEq:     { return "*=";  }
		case BinaryOpType::ModEq:     { return "%=";  }
		case BinaryOpType::DivEq:     { return "/=";  }
		case BinaryOpType::AndEq:     { return "&=";  }
		case BinaryOpType::OrEq:      { return "|=";  }
		case BinaryOpType::ShiftLEq:  { return "<<="; }
		case BinaryOpType::ShiftREq:  { return ">>="; }
		case BinaryOpType::XorEq:     { return "^=";  }
		case BinaryOpType::EqEq:      { return "==";  }
		case BinaryOpType::NotEq:     { return "!=";  }
		case BinaryOpType::AndAnd:    { return "&&";  }
		case BinaryOpType::OrOr:      { return "||";  }
		case BinaryOpType::And:       { return "&";   }
		case BinaryOpType::Or:        { return "|";   }
		case BinaryOpType::Xor:       { return "^";   }
		case BinaryOpType::ShiftR:    { return ">>";  }
		case BinaryOpType::ShiftL:    { return "<<";  }
		case BinaryOpType::Less:      { return "<";   }
		case BinaryOpType::LessEq:    { return "<=";  }
		case BinaryOpType::Greater:   { return ">";   }
		case BinaryOpType::GreaterEq: { return ">=";  }
		case BinaryOpType::Comma:     { return ":";   }
	}

	DebugValidator::Unreachable();
	return "";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string BinaryOperator::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("BinaryOperator: ");
	sb.Append(BinaryOperator::OperatorString(type_));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Left Expr:");
	sb.Append(leftVal_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Right Expr:");
	sb.Append(rightVal_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// NumberConstant
// ######################################################################################
bool NumberConstant::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<NumberConstant>()) {
		return (value_ == t->value_) &&
				Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NumberConstant::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("NumberConstant: ");
	if(IsFloating()) {
		sb.AppendFormat(_T("Float, %f"), value_.FloatValue);
	}
	else {
		sb.AppendFormat(_T("Int, %d"), value_.IntValue);
	}

	return sb.ToString();
}

// ######################################################################################
// CharConstant
// ######################################################################################
bool CharConstant::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<CharConstant>()) {
		return (value_ == t->value_) && Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string CharConstant::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("CharConstant: ");
	if(value_.IsWide) sb.Append("Wide, ");
	sb.Append(value_.Value);
	return sb.ToString();
}

// ######################################################################################
// StringConstant
// ######################################################################################
bool StringConstant::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<StringConstant>()) {
		return (value_ == t->value_) && Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string StringConstant::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("StringConstant: ");
	if(value_.IsWide) sb.Append("Wide, ");
	sb.Append(value_.Value.ToString());
	return sb.ToString();
}

// ######################################################################################
// SubscriptExpression
// ######################################################################################
bool SubscriptExpression::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<SubscriptExpression>()) {
		return base_->Equals(t->base_) && index_->Equals(t->index_) &&
			   Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string SubscriptExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("SubscriptExpression: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Base:");
	sb.Append(base_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Index:");
	sb.Append(index_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// MemberExpression
// ######################################################################################
bool MemberExpression::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<MemberExpression>()) {
		return object_->Equals(t->object_) && (*name_ != *t->name_) &&
			   Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string MemberExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("MemberExpression: ");

	sb.AppendLine();
	sb.Append(string('\t', level + 1));
	sb.AppendLine("- Object:");
	sb.Append(object_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Name:");
	sb.Append(name_->Name());
	return sb.ToString();
}

// ######################################################################################
// CallExpression
// ######################################################################################
bool CallExpression::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<CallExpression>()) {
		if(function_->Equals(t->function_) && Expression::EqualsImpl(other)) {
			if(args_.Count() != t->ArgCount()) {
				return false;
			}
		
			for(int i = 0; i < args_.Count(); i++) {
				if(args_[i]->Equals(t->args_[i]) == false) {
					return false;
				}
			}

			return true;
		}
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FunctionDeclaration* CallExpression::GetDeclaration() {
	if(auto temp = function_->As<CastExpression>()) {
		if(auto temp2 = temp->Target()->As<DeclarationExpression>()) {
			return temp2->Object()->As<FunctionDeclaration>();
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string CallExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("CallExpression: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Function:");
	sb.Append(function_->ToString(level + 1));


	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Arguments:");

	args_.ForEach([&sb, level](shared<Expression>& expr) -> bool {
		sb.AppendLine(expr->ToString(level + 1));
		return true;
	}); 

	return sb.ToString();
}

// ######################################################################################
// SizeofOperator
// ######################################################################################
bool SizeofOperator::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<SizeofOperator>()) {
		return (target_ == t->target_) &&
				Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string SizeofOperator::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("SizeofOperator: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Target:");
	sb.Append(target_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// ConditionalOperator
// ######################################################################################
bool ConditionalOperator::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<ConditionalOperator>()) {
		return condition_->Equals(t->condition_) &&
			   left_->Equals(t->left_) &&
			   right_->Equals(t->right_) &&
			   Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ConditionalOperator::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("ConditionalOperator: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Condition:");
	sb.Append(condition_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Left:");
	sb.Append(left_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Right:");
	sb.Append(right_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// CastExpression
// ######################################################################################
bool CastExpression::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<CastExpression>()) {
		return target_->Equals(t->target_) &&
			   (type_ == t->type_) &&
			   (isExplicit_ == t->isExplicit_) &&
			   Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string CastExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("CastExpression: ");
	if(isExplicit_) sb.Append("Explicit, ");

	switch(type_) {
		case CastType::IntToFloat:	  { sb.Append("IntToFloat");    break; }
		case CastType::FloatToInt:	  { sb.Append("FloatToInt");    break; }
		case CastType::IntToPointer:  { sb.Append("IntToPointer");  break; }
		case CastType::PointerToInt:  { sb.Append("PointerToInt");  break; }
		case CastType::ToVoid:		  { sb.Append("ToVoid");        break; }
		case CastType::IntToInt:	  { sb.Append("IntToInt");      break; }
		case CastType::FloatToFloat:  { sb.Append("FloatToFloat");  break; }
		case CastType::ArrayToPtr:	  { sb.Append("ArrayToPtr");    break; }
		case CastType::FunctionToPtr: { sb.Append("FunctionToPtr"); break; }
		case CastType::RemoveQual:	  { sb.Append("RemoveQual");    break; }
		case CastType::ToPointer:	  { sb.Append("ToPointer");     break; }
		case CastType::Unknown:		  { sb.Append("Unknown");       break; }
	}

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Cast Type:");
	sb.Append(resultType_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Target:");
	sb.Append(target_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// InitializerListExpression
// ######################################################################################
bool InitializerListExpression::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<InitializerListExpression>()) {
		if(list_.Count() != t->list_.Count()) return false;
		if(Expression::EqualsImpl(other) == false) return false;

		for(int i = 0; i < list_.Count(); i++) {
			if(list_[i]->Equals(t->list_[i])) return false;
		}

		return true;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool InitializerListExpression::IsAllZero(Context* context) const {
	for(int i = 0; i < list_.Count(); i++) {
		auto& element = list_[i];

		if(auto initList = element->As<InitializerListExpression>()) {
			if(initList->IsAllZero(context) == false) return false;
		}
		else {
			EvaluationInfo info = list_[i]->Evaluate(context, false);

			if(info.IsIntConstant()) {
				if(info.IntValue() != 0) return false;
			}
			else if(info.IsFloatConstant()) {
				if(info.FloatValue() != 0.0) return false;
			}
			else if(info.IsOtherConstant()) {
				// Null-pointer constants.
				if((list_[i]->ResultType()->IsPointer() == false) ||
					(info.IntValue() != 0) || info.HasVariable()) {
					return false;
				}
			}
            else return false;
		}
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool InitializerListExpression::IsAllConstant(Context* context, bool warn) const {
	for(int i = 0; i < list_.Count(); i++) {
		if(auto temp = list_[i]->As<InitializerListExpression>()) {
			if(temp->IsAllConstant(context, warn) == false) {
				// One of the subitems is not constant.
				return false;
			}
		}
		else {
			EvaluationInfo eval = list_[i]->Evaluate(context, warn);
			if(eval.IsConstant() == false) {
				// The expression is not constant.
				return false;
			}
		}

	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string InitializerListExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("InitializerListExpression: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Items:");

	list_.ForEach([&sb, level](shared<Expression>& expr) -> bool {
		sb.AppendLine(expr->ToString(level + 1));
		return true;
	}); 

	return sb.ToString();
}

// ######################################################################################
// CompoundExpression
// ######################################################################################
bool CompoundExpression::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<CompoundExpression>()) {
		return list_->Equals(t->list_) &&
			   Expression::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string CompoundExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("CompoundExpression: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Children:");
	sb.Append(list_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// DeclarationExpression
// ######################################################################################
bool DeclarationExpression::EqualsImpl(const Expression* other) const {
	if(auto t = other->As<DeclarationExpression>()) {
		return object_->Equals(t->object_) &&
			   Expression::EqualsImpl(other);
	}
	else return false;
}

string DeclarationExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("DeclarationExpression: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Object:");
	sb.Append(object_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// NullExpression
// ######################################################################################
bool NullExpression::EqualsImpl(const Expression* other) const {
	return other->IsNullExpr();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NullExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("NullExpression");
	return sb.ToString();
}

// ######################################################################################
// InvalidExpression
// ######################################################################################
bool InvalidExpression::EqualsImpl(const Expression* other) const {
	return other->IsInvalidExpr();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string InvalidExpression::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("InvalidExpression");
	return sb.ToString();
}

} // namespace AST