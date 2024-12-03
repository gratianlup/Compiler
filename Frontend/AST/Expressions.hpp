// Expressions.hpp
// Copyright (c) Lup Gratian
//
// Defines the helper classes derived from 'Expression'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_EXPRESSIONS_HPP
#define PC_AST_EXPRESSIONS_HPP

#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Lexer/LexemeParsers.hpp"
#include "../Common/Context.hpp"
#include "Expression.hpp"
#include "Types.hpp"
#include "Statement.hpp"
#include "Declarations.hpp"
#include "Visitor.hpp"
using namespace Lexing;
using namespace Base;
using namespace Lexing;
using namespace Common;

namespace AST {

// Represents the unary operator types. 'sizeof' is treated as a separate case.
enum class UnaryOpType {
	Inc,         // ++a
	Dec,         // --a
	Address,     // &a
	Indirection, // *a
	Add,         // +a
	Sub,         // -a
	Complement,  // ~a
	Not,         // !a
};

// Represents an unary operator and the associated statement.
class UnaryOperator : public Expression {
private:
	UnaryOpType type_;
	shared<Expression> value_;
	bool isPostfix_; // Used with ++ and --.

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	UnaryOperator(UnaryOpType type, shared<Expression> value, 
			      const Type* resultType, bool postfix, LocationInfo location) :
			Expression(EXPR_UNARY, resultType, location), 
			type_(type), value_(value), isPostfix_(postfix) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the expression associated with the operator.
	shared<Expression> Value() {
		return value_;
	}

	const shared<Expression> Value() const {
		return value_;
	}

	void SetValue(shared<Expression> value) {
		value_ = value;
	}

	// Returns the type of the operator.
	UnaryOpType Operator() const {
		return type_;
	}

	// Returns 'true' if this is a postfix operator (valid only for ++ and --).
	bool IsPostfix() const {
		return isPostfix_;
	}

	void SetIsPostfix(bool value) {
		isPostfix_ = value;
	}

	// Returns 'true' if the operator is &.
	bool IsAddress() const {
		return type_ == UnaryOpType::Address;
	}

	// Returns 'true' if the operator is *.
	bool IsIndirection() const {
		return type_ == UnaryOpType::Indirection;
	}

	// Returns a string representation of the specified operator.
	static string OperatorString(UnaryOpType op);

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents the binary operator types.
enum class BinaryOpType {
	Add,       // +
	Sub,       // -
	Mul,       // *
	Div,       // /
	Mod,       // %
	Eq,        // =
	AddEq,     // +=
	SubEq,     // -=
	MulEq,     // *=
	ModEq,     // %=
	DivEq,     // /=
	AndEq,     // &=
	OrEq,      // |=
	ShiftLEq,  // <<=
	ShiftREq,  // >>=
	XorEq,     // ^=
	EqEq,      // ==
	NotEq,     // !=
	AndAnd,    // &&
	OrOr,      // ||
	And,       // &
	Or,        // |
	Xor,       // ^
	ShiftR,    // >>
	ShiftL,    // <<
	Less,      // <
	LessEq,    // <=
	Greater,   // >
	GreaterEq, // >=
	Comma      // ,
};

// Represents a binary operator and the two connected statements.
class BinaryOperator : public Expression {
private:
	BinaryOpType type_;
	shared<Expression> leftVal_;
	shared<Expression> rightVal_;

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	BinaryOperator(BinaryOpType type, shared<Expression> leftValue,
			       shared<Expression> rightValue, const Type* resultType,
			       LocationInfo location) :
			Expression(EXPR_BINARY, resultType, location), type_(type),
			leftVal_(leftValue), rightVal_(rightValue) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the left side of the operator.
	shared<Expression> LeftValue() {
		return leftVal_;
	}

	const shared<Expression> LeftValue() const {
		return leftVal_;
	}

	void SetLeftValue(shared<Expression> value) {
		leftVal_ = value;
	}

	// Returns the right side of the operator.
	shared<Expression> RightValue() {
		return rightVal_;
	}

	const shared<Expression> RightValue() const {
		return rightVal_;
	}

	void SetRightValue(shared<Expression> value) {
		rightVal_ = value;
	}

	// Returns the type of the operator.
	BinaryOpType Operator() const {
		return type_;
	}

	// Returns 'true' if this is any of the arithmetic operators.
	bool IsArithmetic() const {
		return ((int)type_ >= (int)BinaryOpType::Add) &&
			   ((int)type_ <= (int)BinaryOpType::Mod);
	}

	// Returns 'true' if this is any of the assignment operators.
	bool IsAssignment() const {
		return ((int)type_ >= (int)BinaryOpType::Eq) &&
			   ((int)type_ <= (int)BinaryOpType::XorEq);
	}

	// Returns 'true' if this is an equality operator (== or !=).
	bool IsEquality() const {
		return (type_ == BinaryOpType::EqEq) || 
               (type_ == BinaryOpType::NotEq);
	}

	// Returns 'true' if this is any of the relational operators.
	bool IsRelational() const {
		return ((int)type_ >= (int)BinaryOpType::Less) &&
			   ((int)type_ <= (int)BinaryOpType::GreaterEq);
	}

	// Returns 'true' if this is a logical operators (&& or ||).
	bool IsLogical() const {
		return (type_ == BinaryOpType::AndAnd) || 
               (type_ == BinaryOpType::OrOr);
	}

	// Returns 'true' if this is any of the binary operators.
	bool IsBinary() const {
		return ((int)type_ >= (int)BinaryOpType::And) &&
			   ((int)type_ <= (int)BinaryOpType::Xor);
	}

	// Returns 'true' if this is a shift operator (<< or >>).
	bool IsShift() const {
		return (type_ == BinaryOpType::ShiftR) || 
               (type_ == BinaryOpType::ShiftL);
	}

	// Returns a number representing the precedence of this operator.
	int Precedence() const;

	// Returns a string representation of the specified operator.
	static string OperatorString(BinaryOpType op);

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents an integer or floating number constant.
class NumberConstant : public Expression {
private:
	NumberInfo value_;

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	NumberConstant(const NumberInfo& value, const Type* resultType, LocationInfo location) :
			Expression(EXPR_NUMBER, resultType, location), value_(value) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Creates a new instance with the specified integer value.
	static shared<NumberConstant> FromInteger(int value, const BasicType* type) {
		NumberInfo info;
		info.IsInteger = true;
		info.SetIntType(IntType_Int);
		info.IntValue = value;

		return new NumberConstant(info, type, LocationInfo());
	}

	// Creates a new instance with the specified floating value.
	static shared<NumberConstant> FromFloating(double value, const BasicType* type) {
		NumberInfo info;
		info.IsInteger = false;
		info.SetFloatType(FloatType_Double);
		info.FloatValue = value;

		return new NumberConstant(info, type, LocationInfo());
	}

	// Returns the constant value.
	const NumberInfo& Value() const {
		return value_;
	}

	NumberInfo& Value() {
		return value_;
	}

	// Returns 'true' if this is an integer constant.
	bool IsInteger() const {
		return value_.IsInteger;
	}

	// Returns 'true' if this is an floating constant.
	bool IsFloating() const {
		return value_.IsInteger == false;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a character constant.
class CharConstant : public Expression {
private:
	CharInfo value_;

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	CharConstant(const CharInfo& value, const Type* resultType, LocationInfo location) :
			Expression(EXPR_CHAR, resultType, location), value_(value) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the constant value.
	const CharInfo& Value() const {
		return value_;
	}

	CharInfo& Value() {
		return value_;
	}

	// Returns 'true' if this is an wide character constant.
	bool IsWide() const {
		return value_.IsWide;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a character constant.
class StringConstant : public Expression {
private:
	StringInfo value_;

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	StringConstant(const StringInfo& value, const Type* resultType, LocationInfo location) :
			Expression(EXPR_STRING, resultType, location), value_(value) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the constant value.
	const StringInfo& Value() const {
		return value_;
	}

	StringInfo& Value() {
		return value_;
	}

	// Returns 'true' if this is an wide character constant.
	bool IsWide() const {
		return value_.IsWide;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents the 'sizeof' operator.
class SizeofOperator: public Expression {
private:
	shared<Expression> target_; // The type for which to compute the size.
	LocationInfo rightLoc_;     // The end location of the expression.

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	SizeofOperator(shared<Expression> target, const Type* resultType, 
			 LocationInfo leftLoc, LocationInfo rightLoc) :
			Expression(EXPR_SIZEOF, resultType, leftLoc),
			target_(target), rightLoc_(rightLoc) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns type for which to compute the size.
	shared<Expression> Target() {
		return target_;
	}

	const shared<Expression> Target() const {
		return target_;
	}

	void SetTarget(shared<Expression> value) {
		target_ = value;
	}

	// Returns the end location of the operator (the closing brace position).
	LocationInfo EndLocation() const {
		return rightLoc_;
	}

	void SetEndLocation(LocationInfo value) {
		rightLoc_ = value;
	}

	// Returns the range covered by the operator.
	RangeInfo Range() const {
		return RangeInfo(location_, rightLoc_);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents an array subscription. Base can be an array or pointer
// and index must be an expression that evaluates to an integer.
class SubscriptExpression: public Expression {
private:
	shared<Expression> base_;  // The expression that acts as the array.
	shared<Expression> index_; // The expression that acts as the index in the array.
	LocationInfo rightLoc_;    // The end location of the expression ].

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	SubscriptExpression(shared<Expression> base, shared<Expression> index,
				        const Type* resultType, LocationInfo leftLoc, LocationInfo rightLoc) :
			Expression(EXPR_SUBSCRIPT, resultType, leftLoc),
			base_(base), index_(index), rightLoc_(rightLoc) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the expression that represents the base of the array.
	shared<Expression> Base() {
		return base_;
	}

	const shared<Expression> Base() const {
		return base_;
	}

	void SetBase(shared<Expression>&value) {
		base_ = value;
	}

	// Returns the expression that represents the index of the array.
	shared<Expression> Index() {
		return index_;
	}

	const shared<Expression> Index() const {
		return index_;
	}

	void SetIndex(shared<Expression> value) {
		index_ = value;
	}

	// Returns the end location of the operator (the closing brace position).
	LocationInfo EndLocation() const {
		return rightLoc_;
	}

	void SetEndLocation(LocationInfo value) {
		rightLoc_ = value;
	}

	// Returns the range covered by the operator.
	RangeInfo Range() const {
		return RangeInfo(location_, rightLoc_);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents the access to the member of a struct/union object.
// Handles both . and -> (for pointers to objects).
class MemberExpression: public Expression {
private:
	bool isPointer_;                  // 'true' if the object is a pointer.
	shared<Expression> object_;       // The base object.
	shared<Identifier> name_;         // The name of the member.
	shared<FieldDeclaration> member_; // The accessed member.

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	MemberExpression(shared<Expression> object, shared<Identifier> name,
			         shared<FieldDeclaration> member, bool isPointer, 
                     const Type* resultType, LocationInfo location) :
			Expression(EXPR_MEMBER, resultType, location),
			object_(object), name_(name), member_(member), isPointer_(isPointer) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the expression that represents the record.
	shared<Expression> Object() {
		return object_;
	}

	const shared<Expression> Object() const {
		return object_;
	}

	void SetObject(shared<Expression> value) {
		object_ = value;
	}

	// Return the name of the member.
	shared<Identifier> Name() {
		return name_;
	}

	const shared<Identifier> Name() const {
		return name_;
	}

	void SetName(shared<Identifier> value) {
		name_ = value;
	}

	// Returns the declaration of the member.
	shared<FieldDeclaration> Member() {
		return member_;
	}

	const shared<FieldDeclaration> Member() const {
		return member_;
	}

	void SetMember(shared<FieldDeclaration> value) {
		member_ = value;
	}

	// Returns 'true' if the member is accessed through a pointer ->.
	bool IsPointer() const {
		return isPointer_;
	}

	void SetIsPointer(bool value) {
		isPointer_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a function call. Contains the declaration of the function
// and the list or arguments.
class CallExpression: public Expression {
private:
	shared<Expression> function_;   // The object that acts as the function.
	List<shared<Expression>> args_; // The function arguments.

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	CallExpression(shared<Expression> function, const Type* resultType, 
                   LocationInfo location) :
			Expression(EXPR_CALL, resultType, location), function_(function) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the function for which the call is made.
	shared<Expression> Function() {
		return function_;
	}

	const shared<Expression> Function() const {
		return function_;
	}

	void SetFunction(shared<Expression> value) {
		function_ = value;
	}

	// Returns the arguments associated with this function call.
	List<shared<Expression>>& Arguments() {
		return args_;
	}

	const List<shared<Expression>>& Arguments() const {
		return args_;
	}

	// Returns the number of arguments.
	int ArgCount() const {
		return args_.Count();
	}

	// Returns the declaration of the called function, if any.
	FunctionDeclaration* GetDeclaration();

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a conditional operator E ? E1 : E2
class ConditionalOperator: public Expression {
private:
	shared<Expression> condition_;
	shared<Expression> left_;
	shared<Expression> right_;

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	ConditionalOperator(shared<Expression> condition, shared<Expression> left,
				        shared<Expression> right, const Type* resultType, 
                        LocationInfo location) :
			Expression(EXPR_CONDITIONAL, resultType, location),
			condition_(condition), left_(left), right_(right) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the expression that represents the condition.
	shared<Expression> Condition() {
		return condition_;
	}

	const shared<Expression> Condition() const {
		return condition_;
	}

	void SetCondition(shared<Expression> value) {
		condition_ = value;
	}

	// Returns the expression that represents the first variant.
	shared<Expression> Left() {
		return left_;
	}

	const shared<Expression> Left() const {
		return left_;
	}

	void SetLeft(shared<Expression> value) {
		left_ = value;
	}

	// Returns the expression that represents the second variant.
	shared<Expression> Right() {
		return right_;
	}

	const shared<Expression> Right() const {
		return right_;
	}

	void SetRight(shared<Expression> value) {
		right_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents the type of cast performed by a conversion expression.
enum class CastType {
	IntToFloat,       // Integer to floating number.
	FloatToInt,       // Floating to integer number.
	IntToPointer,     // Integer converted to a pointer.
	PointerToInt,     // Pointer converted to an integer.
	ToVoid,           // Casting to 'void'.
	IntToInt,         // Between integer types  (ex. short -> int).
	FloatToFloat,     // Between floating types (ex. float -> double).
	ArrayToPtr,       // Used for default promotions.
	FunctionToPtr,    // Used for default promotions.
	RemoveQual,       // Used to remove the qualifiers from the type.
	ToPointer,        // Something converted to a pointer.
	Unknown           
};

// Represents a conversion expression. Both implicit and explicit expressions
// can be represented using this class.
// Implicit conversions are, for example, integer to float promotions.
// Explicit conversions are found in the source file, like '(int)some_float_val',
// '(void*)&some_var' and '*(int*)some_addr'.
class CastExpression : public Expression {
private:
	CastType type_;
	shared<Expression> target_;
	bool isExplicit_;

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	CastExpression(CastType type, shared<Expression> target, const Type* resultType,
				   bool isExplicit, LocationInfo location = LocationInfo()) :
			Expression(EXPR_CAST, resultType, location),
			type_(type), target_(target), isExplicit_(isExplicit) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the type of this conversion.
	CastType Type() const {
		return type_;
	}

	void SetType(CastType value) {
		type_ = value;
	}

	// Returns 'true' if this is a cast found in the source code.
	bool IsExplicit() const {
		return isExplicit_;
	}

	// Returns 'true' if this is a cast automatically introduced by the compiler.
	bool IsImplicit() const {
		return isExplicit_ == false;
	}

	// Returns the expression that is the target of the conversion operation.
	shared<Expression> Target() {
		return target_;
	}

	const shared<Expression> Target() const {
		return target_;
	}

	void SetTarget(shared<Expression> value) {
		target_ = value;
	}

	// Returns 'true' if this is a conversion between number types.
	bool IsArithmetic() const {
		return (type_ == CastType::IntToFloat) || 
               (type_ == CastType::FloatToInt) ||
			   (type_ == CastType::IntToInt)   ||
               (type_ == CastType::FloatToFloat);
	}

	// Returns 'true' if this is a conversion to an integer type.
	bool IsToInteger() const {
		return (type_ == CastType::IntToInt)   || 
               (type_ == CastType::FloatToInt) ||
			   (type_ == CastType::PointerToInt);
	}

	// Returns 'true' if this is a conversion to a floating type.
	bool IsToFloating() const {
		return (type_ == CastType::IntToFloat) ||
               (type_ == CastType::FloatToFloat);
	}

	// Returns 'true' if this is a conversion to a pointer type.
	bool IsToPointer() const {
		return (type_ == CastType::ToPointer)     || 
               (type_ == CastType::ArrayToPtr)    ||
			   (type_ == CastType::FunctionToPtr) || 
               (type_ == CastType::IntToPointer);
	}

	// Returns 'true' if this is a conversion to a void pointer.
	bool IsToVoid() const {
		return type_ == CastType::ToVoid;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents an initializer, Initializers can be used to initialize any object.
// 'int a[] = {1,2,3}' initializes an array of size 3, with the values 1,2,3.
// See C99:6.7.8 for details.
class InitializerListExpression : public Expression {
private:
	List<shared<Expression>> list_; // The list of initialization expressions.
	LocationInfo endLoc_;           // The end position of the initializer list.

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	InitializerListExpression(LocationInfo location) : 
            Expression(EXPR_INIT_LIST, nullptr, location) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the list of initializers.
	List<shared<Expression>>& InitList() {
		return list_;
	}

	const List<shared<Expression>>& InitList() const {
		return list_;
	}

	// Returns the end location of the list (the closing curly position).
	LocationInfo EndLocation() const {
		return endLoc_;
	}

	void SetEndLocation(LocationInfo value) {
		endLoc_ = value;
	}

	// Returns the range covered by the list.
	RangeInfo Range() const {
		return RangeInfo(location_, endLoc_);
	}

	// Returns the number of initializers in the list.
	int Count() const {
		return list_.Count();
	}

	// Returns 'true' if all elements have value 0.
	bool IsAllZero(Context* context) const;

	// Returns 'true' if all the elements are constant values.
	bool IsAllConstant(Context* context, bool warn = false) const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a compound literal. Provides an unnamed object
// of the form '(int []) {1, 2,3}'. Has an initializer list.
class CompoundExpression : public Expression {
private:
	shared<InitializerListExpression> list_;

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	CompoundExpression(shared<InitializerListExpression> list, 
                       const Type* resultType, LocationInfo location) :
			Expression(EXPR_COMPOUND, resultType, location), list_(list) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the list of initializers.
	shared<InitializerListExpression>& InitList() {
		return list_;
	}

	const shared<InitializerListExpression>& InitList() const {
		return list_;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a reference to a variable 
// (basic type, struct/union, enum, function).
class DeclarationExpression : public Expression {
	Declaration* object_;

protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	DeclarationExpression(Declaration* object, const Type* resultType, 
					      LocationInfo location) :
			Expression(EXPR_DECL, resultType, location), object_(object) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the referred object.
	Declaration* Object() {
		return object_;
	}

	const Declaration* Object() const {
		return object_;
	}

	void SetObject(Declaration* value) {
		object_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents an expression used when the semantic analysis
// fails on an expression. For example '(1 << 3.14) + t' fails the check. 
// If we use an 'InvalidExpression' we can continue parsing the rest of the expression, 
// and discover that 't' is not defined.
class InvalidExpression : public Expression {
protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	InvalidExpression() : Expression(EXPR_INVALID, nullptr, Location()) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents an empty expression. Should be used as a placeholder.
class NullExpression : public Expression {
protected:
	virtual bool EqualsImpl(const Expression* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	NullExpression(const Type* resultType) :
			 Expression(EXPR_NULL, resultType, Location()) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};

} // namespace AST
#endif