// ExpressionTypeSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements various semantic analysis methods for types
// and values involved in expression.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionSemantic.hpp"
#include "DeclarationSemantic.hpp"
#include "StatementSemantic.hpp"
#include "SemanticHolder.hpp"
#include "../AST/TypeString.hpp"
using namespace AST;

namespace Parsing {

ExpressionSemantic::ValueType 
ExpressionSemantic::IsLValueImpl(shared<Expression> expr) {
	if(auto temp = expr->As<DeclarationExpression>()) {
		// Note that function designators are not lvalues.
		if(temp->Object()->IsFieldDecl() ||
		   temp->Object()->IsVariableDecl()) {
			return Value_LValue;
		}
	}
	else if(auto temp = expr->As<MemberExpression>()) {
		// C99:6.5.2.3: . is an lvalue if the left expression is an lvalue.
		// C99:6.5.2.4: -> is always a lvalue.
		if(temp->IsPointer()) return Value_LValue;
		else return IsLValueImpl(temp->Object());
	}
	else if(expr->IsCallExpr()) {
		// Function calls are not lvalues.
		return Value_RValue;
	}
	else if(auto temp = expr->As<UnaryOperator>()) {
		// The unary * operator creates a lvalue (C99:6.5.3.1.1).
		if(temp->Operator() == UnaryOpType::Indirection) {
			return Value_LValue;
		}
	}
	else if(auto temp = expr->As<CastExpression>()) {
		// Any explicit cast yields an rvalue.
		return temp->IsExplicit() ? Value_RValue : IsLValueImpl(temp->Target());
	}
	else if(expr->IsSubscriptExpr()) {
		// [] is always an lvalue.
		return Value_LValue;
	}
	else if(expr->IsCompoundExpr()) {
		// Compound literals ('(int[]){1,2,3}') are always lvalues.
		return Value_LValue;
	}

	// Most expressions are not lvalues.
	return Value_RValue;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionSemantic::IsLValue(shared<Expression> expr, ValueType* type) {
	// Figure out if the expression can be an lvalue or rvalue.
	ValueType valueType = IsLValueImpl(expr);

	// C99:6.3.2.1: an lvalue is an object type
    // or an incomplete that is not 'void'.
	if(expr->ResultType()->IsFunction()) {
		valueType = Value_Function;
	}
	else if(expr->ResultType()->IsVoid()) {
		valueType = Value_Void;
	}

	if(type) *type = valueType;
	return valueType == Value_LValue;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionSemantic::IsModifiableLValue(shared<Expression> expr, ValueType* type) {
	// C99:6.3.2.1: a lvalue that can be modified does not have array type,
	// incomplete type, a 'const' qualifier, and if it's a struct/union,
	// none of it's members (at all levels) should be 'const' qualified.
	ValueType valueType;
	bool invalid = false;

	IsLValue(expr, &valueType);

	if(valueType != Value_LValue) {
		return false; // If it's not an lvalue we're done.
	}

	const Type* exprType = expr->ResultType();

	if(auto temp = exprType->As<QType>()) {
		if(temp->HasConst()) {
			valueType = Value_Const;
			if(type) *type = valueType;
			return false;
		}

		exprType = temp->Base(); // Remove the qualifiers for now.
	}
	
	if(exprType->IsIncomplete()) {
		valueType = Value_Incomplete;
		invalid = true;
	}
	else if(exprType->IsArray()) {
		valueType = Value_Array;
		invalid = true;
	}
	else if(auto temp = exprType->As<StructType>()) {
		if(temp->HasConstMember()) {
			valueType = Value_Const;
			invalid = true;
		}
	}
	else if(auto temp = exprType->As<UnionType>()) {
		if(temp->HasConstMember()) {
			valueType = Value_Const;
			invalid = true;
		}
	}
    else if(auto temp = exprType->As<PointerType>()) {
        if(auto castExpr = expr->As<CastExpression>()) {
            if((castExpr->Type() == CastType::ArrayToPtr) &&
                castExpr->Target()->IsMemberExpr()) {
                // This handles a situation like the following:
                // struct ABC { int a, b[10]; } t.
                // t.b = 0;
                valueType = Value_Array;
		        invalid = true;
            }
        }
    }
	
	if(type) *type = valueType;
	return invalid == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionSemantic::AreTypesCompatible(const Type* a, const Type* b) {
	// The types are compatible if they can be successfully combined.
	// For struct/union we test that the type is the same.
	if(a->IsStruct() || a->IsUnion()) {
		return a == b;
	}
	else return typeComb_.Combine(a, b);
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionSemantic::EmitLValueDiagnostic(ValueType type, LocationInfo location) {
	// Emit an error depending on the value type.
	if(type == Value_Array) {
		diag_->Report(Error::LVALUE_ARRAY_TYPE)<<location;
	}
	else if(type == Value_Function) {
		diag_->Report(Error::LVALUE_FUNCTION_TYPE)<<location;
	}
	else if(type == Value_Void) {
		diag_->Report(Error::LVALUE_VOID_TYPE)<<location;
	}
	else if(type == Value_Const) {
		diag_->Report(Error::LVALUE_CONST_QUALIFIED)<<location;
	}
	else if(type == Value_Incomplete) {
		diag_->Report(Error::LVALUE_INCOMPLETE_TYPE)<<location;
	}
	else {
		diag_->Report(Error::LVALUE_INVALID)<<location;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionSemantic::WarnQualifierDrop(const Type* a, const Type* b,
                                     const Expression* expr,
								     const Identifier* name, bool isArg, 
									 LocationInfo location) {
	// Verifies whether a type qualifier is dropped 
    // when converting between types.
	bool drop = false;

	if(const QType* qualB = b->As<QType>()) {
		if(a->IsQualified() == false) {
			// Ex: 'int* a; const int* b; a = b;'.
			drop = true;
		}
		else {
			const QType* qualA = a->As<QType>();

			if(qualB->HasConst() && (qualA->HasConst() == false)) {
				// Ex: 'volatile int* a; const int* b; a = b;'.
				drop = true;
			}

			if(qualB->HasVolatile() && (qualA->HasVolatile() == false)) {
				// Ex: 'const int* a; volatile int* b; a = b;'.
				drop = true;
			}
		}
	}

	// If no qualifiers have been dropped 
    // we don't need to emit any warning, so return.
	if(drop == false) return;

	// Display the appropriate warning message.
	// If an expression has been provided try to get a variable name from it.
	if(expr) {
		if(auto temp = expr->As<DeclarationExpression>()) {
			name = temp->Object()->Name();
		}
	}

	if(isArg) {
		if(name) diag_->Report(Warning::ARGUMENT_QUALIFIERS_DROPPED)<<*name;
		else diag_->Report(Warning::ARGUMENT_QUALIFIERS_DROPPED)<<location;
	}
	else {
		if(name) diag_->Report(Warning::ASSIGNMENT_QUALIFIERS_DROPPED)<<*name;
		else diag_->Report(Warning::ASSIGNMENT_QUALIFIERS_DROPPED)<<location;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionSemantic::IntFits(__int64 value, TypeKind kind) {
	__int64 truncated = context_->Target()->Truncate(value, kind);
	return truncated == value;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ExpressionSemantic::GetIntRank(const NumberInfo& info) {
	if(info.IsInt())       return 0;
	if(info.IsUInt())      return 1;
	if(info.IsLong())      return 2;
	if(info.IsULong())     return 3;
	if(info.IsLongLong())  return 4;
	if(info.IsULongLong()) return 5;

	DebugValidator::Unreachable();
	return -1; 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int ExpressionSemantic::GetIntRank(const BasicType* type) {
	if(type->IsInt())       return 0;
	if(type->IsUInt())      return 1;
	if(type->IsLong())      return 2;
	if(type->IsULong())     return 3;
	if(type->IsLongLong())  return 4;
	if(type->IsULongLong()) return 5;

	DebugValidator::Unreachable();
	return -1; 
}

} // namespace Parsing