// Expression.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for the expression system.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_EXPRESSION_HPP
#define PC_AST_EXPRESSION_HPP

#include "../Base/String.hpp"
#include "../Common/LocationInfo.hpp"
#include "../Common/Context.hpp"
#include "../Lexer/LexemeParsers.hpp"
#include "Statement.hpp"
#include "Type.hpp"
#include "Visitor.hpp"
using namespace Base;
using namespace Common;
using namespace Lexing;

namespace AST {

enum class ResultType {
	IntConst,
	FloatConst,
	Other, // Used for functions and other types.
	Failed 
};


// Represents the value of the evaluation result.
union ValueUnion {
	__int64 IntValue;
	double FloatValue;

	//-------------------------------------------------------------------------------
	ValueUnion() : IntValue(0) {}
	ValueUnion(__int64 value) : IntValue(value) {}
	ValueUnion(double value) : FloatValue(value) {}
	ValueUnion(const ValueUnion& other) : IntValue(other.IntValue) {}
};


// Provides the result of a constant expression evaluation operation.
class EvaluationInfo {
private:
	ResultType result_;
	ValueUnion value_;
	bool       hasVariable_; // Used only for 'ResultType::Other'.

public:
	EvaluationInfo() : result_(ResultType::Failed), hasVariable_(false) {}

	EvaluationInfo(ResultType result) : result_(result), hasVariable_(false) {}

	EvaluationInfo(ResultType result, ValueUnion value, bool hasVariable = false) :
			result_(result), value_(value), hasVariable_(hasVariable) {}

	EvaluationInfo(const EvaluationInfo& other) :
			result_(other.result_), value_(other.value_), hasVariable_(other.hasVariable_) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool IsIntConstant() const {
		return result_ == ResultType::IntConst;
	}

	bool IsFloatConstant() const {
		return result_ == ResultType::FloatConst;
	}

	bool IsOtherConstant() const {
		return result_ == ResultType::Other;
	}

	bool IsConstant() const {
		return IsIntConstant() || IsFloatConstant() || IsOtherConstant();
	}

	bool HasFailed() const {
		return result_ == ResultType::Failed;
	}

	bool HasVariable() const {
		return hasVariable_;
	}

	ValueUnion Value() const {
		return value_;
	}

	__int64 IntValue() const {
		return value_.IntValue;
	}

	double FloatValue() const {
		return value_.FloatValue;
	}

	void SetResult(ResultType value) {
		result_ = value;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const EvaluationInfo& other) const {
		if(result_ == other.result_) {
			if(result_ == ResultType::IntConst) {
				return value_.IntValue == other.value_.IntValue; 
			}
			else if(result_ == ResultType::FloatConst) {
				return value_.FloatValue == other.value_.FloatValue;
			}
		}
		
		return false;
	}

	bool operator!= (const EvaluationInfo& other) const {
		return !operator== (other);
	}
};


// Forward declarations.
class FieldDeclaration;
class EnumDeclaration;

// Represents the base class for expressions.
// Provides methods to test the type of the expression and to evaluate it.
class Expression : public Visitable {
private:
	Expression();                             // Should not be created.
	Expression(const Expression&);            // Should not be copied.
	Expression& operator=(const Expression&); // Should not be assigned.

protected:
	static const char EXPR_UNARY       = 0;
	static const char EXPR_BINARY      = 1;
	static const char EXPR_DECL        = 2;  // variable used in expression
	static const char EXPR_NUMBER      = 3;  // integer + floating
	static const char EXPR_CHAR        = 4;  // 'a', L'a'
	static const char EXPR_STRING      = 5;  // "abc",L"abc"
	static const char EXPR_SIZEOF      = 6;  // sizeof
	static const char EXPR_SUBSCRIPT   = 7;  // []
	static const char EXPR_CALL        = 8;  // f()
	static const char EXPR_MEMBER      = 9;  // . ->
	static const char EXPR_PAREN       = 10; // ()
	static const char EXPR_CONDITIONAL = 11; // ?:
	static const char EXPR_INIT_LIST   = 12; // {}
	static const char EXPR_CAST        = 13; // implicit and explicit
	static const char EXPR_COMPOUND    = 14; // (int[]) {1,2}
	static const char EXPR_INVALID     = 15; // placeholder for an invalid expression
	static const char EXPR_NULL        = 16; // placeholder

	const Type*  resultType_;  // The type of the expression result.
	LocationInfo location_;    // Where the expression was found.
	char         kind_    : 6; // The type of the expression.
	char         isConst_ : 2; // 0 if not known, 1 - true, 2 - false.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Constructors to be used only by derived classes.
	Expression(int kind, const Type* resType, LocationInfo location) :
			kind_(kind), resultType_(resType), location_(location), isConst_(0) {}

	// Should return 'true' if the expressions are identical.
	virtual bool EqualsImpl(const Expression* other) const;

	// Should generate a string that describes the expression.
	virtual string ToStringImpl(int level) const {
		return "";
	}

public:
	virtual ~Expression() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the type of the expression result.
	const Type* ResultType() const {
		return resultType_;
	}

	void SetResultType(const Type* value) {
		resultType_ = value;
	}

	// Returns the place where the expression start was found.
	LocationInfo Location() const {
		return location_;
	}

	void SetLocation(LocationInfo value) {
		location_ = value;
	}

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return dynamic_cast<T*>(this);
	}

	template <class T>
	const T* As() const {
		return dynamic_cast<T*>(const_cast<Expression*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return As<T>();
	}

	// Returns 'true' if this is an unary expression.
	bool IsUnaryOp() const {
		return kind_ == EXPR_UNARY;
	}

	// Returns 'true' if this is an binary expression.
	bool IsBinaryOp() const {
		return kind_ == EXPR_BINARY;
	}

	// Returns 'true' if this is an integer or floating constant expression.
	bool IsNumberConst() const {
		return kind_ == EXPR_NUMBER;
	}

	// Returns 'true' if this is a character constant expression.
	bool IsCharConst() const {
		return kind_ == EXPR_CHAR;
	}

	// Returns 'true' if this is an string constant expression.
	bool IsStringConst() const {
		return kind_ == EXPR_STRING;
	}

	// Returns 'true' if this is a 'sizeof' expression.
	bool IsSizeofOp() const {
		return kind_ == EXPR_SIZEOF;
	}

	// Returns 'true' if this is an array subscript expression.
	bool IsSubscriptExpr() const {
		return kind_ == EXPR_SUBSCRIPT;
	}

	// Returns 'true' if this is a function call expression.
	bool IsCallExpr() const {
		return kind_ == EXPR_CALL;
	}

	// Returns 'true' if this is a member reference expression.
	bool IsMemberExpr() const {
		return kind_ == EXPR_MEMBER;
	}

	// Returns 'true' if this is a paren expression.
	bool IsParenExpr() const {
		return kind_ == EXPR_PAREN;
	}

	// Returns 'true' if this is a conditional operator ?:.
	bool IsConditionalOp() const {
		return kind_ == EXPR_CONDITIONAL;
	}

	// Returns 'true' if this is a conversion expression.
	bool IsCastExpr() const {
		return kind_ == EXPR_CAST;
	}

	// Returns 'true' if this is a initializer list expression.
	bool IsInitListExpr() const {
		return kind_ == EXPR_INIT_LIST;
	}

	// Returns 'true' if this is a compound expression (anonymous types).
	bool IsCompoundExpr() const {
		return kind_ == EXPR_COMPOUND;
	}

	// Returns 'true' if this is an expression used as a placeholder for an expression
	// that didn't pass the semantic analysis tests.
	bool IsInvalidExpr() const {
		return kind_ == EXPR_INVALID;
	}

	// Returns 'true' if this is an empty (null) expression.
	bool IsNullExpr() const {
		return kind_ == EXPR_NULL;
	}

	// Returns 'true' if this is a reference to a declared variable.
	bool IsDeclarationExpr() const {
		return kind_ == EXPR_DECL;
	}

	// Returns 'true' if this can represent a nullptr pointer constant.
    // If 'relaxed' is set any pointer cast is considered eligible
    // (this allows '(MyStruct*)0' to be a null pointer, 
    //  not only ''(void*)0' as the C standard defines).
	bool IsNullPointer(const Context* context, bool relaxed = false) const;

	// Removes all casts until the real target expression is found.
	Expression* WithoutCasts();

	const Expression* WithoutCasts() const;

	// Returns 'true' if the expressions are identical.
	bool Equals(const Expression* other) const {
		return EqualsImpl(other);
	}

	// Tries to evaluate the expression and returns the result in an 'EvaluationInfo' object.
	EvaluationInfo Evaluate(const Context* context, bool warn, 
					  bool expectICE = false, bool allowVars = true) const;

	// Tries to evaluate the expression and returns the result in an 'EvaluationInfo' object.
	// Checks if the resulting type is an integer.
	EvaluationInfo EvaluateAsICE(const Context* context, bool warn) const;

	// Returns 'true' if this expression can be evaluated to constant at compile-time.
	bool IsConstant(const Context* context, bool allowVars = true) const;

	// Returns 'true' and sets the result if the evaluated expression
	// is a constant, else it returns 'false'.
	bool TryEvaluateConstant(const Context* context, EvaluationInfo* info, bool allowVars = true) const;

	// Returns 'true' if this expression is an ICE (integer constant expression).
	// See C99:6.6 for details.
	bool IsIntegerConstant(const Context* context) const;

	// Marks the expression as not being in an unknown constant state.
	void ResetIsConstant() {
		isConst_ = 0;
	}

	// Marks the expression as being constant or not.
	void SetIsConstant(bool value) {
		if(value) isConst_ = 1; // true
		else isConst_ = 2;      // false
	}

	// Returns the corresponding 'FieldDeclaration' object if this is a bitfield reference.
	shared<FieldDeclaration> GetBitfield() const;

	// Returns 'true' if the expression is not valid.
	// 'ErrorExpr' is used as a placeholder for invalid expressions.
	static bool IsInvalid(const Expression* expr) {
		return (expr == nullptr) || expr->IsInvalidExpr();
	}

	// Returns a string representation of the expression.
	string ToString(int level) const {
		return ToStringImpl(level);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const = 0;
};

} // namespace AST
#endif