// Statements.hpp
// Copyright (c) Lup Gratian
//
// Defines the helper classes derived from 'Statement'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_STATEMENTS_HPP
#define PC_AST_STATEMENTS_HPP

#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "Statement.hpp"
#include "Identifier.hpp"
#include "Visitor.hpp"
using namespace Base;

namespace AST {

// Forward declarations.
class Expression;
class Declaration;

// Represents an 'if' statement. Includes the condition, 
// the part that should be executed when the condition is true,
// and optionally, the part for the false case.
class IfStatement : public Statement {
private:
	shared<Expression> cond_;
	shared<Statement> true_;
	shared<Statement> false_;

protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	IfStatement(shared<Expression> cond, shared<Statement> ifTrue,
                shared<Statement> ifFalse, LocationInfo location) :
			Statement(STMT_IF, location), cond_(cond), 
            true_(ifTrue), false_(ifFalse) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the condition inside the if statement.
	shared<Expression> Condition() {
		return cond_;
	}

	const shared<Expression> Condition() const {
		return cond_;
	}

	void SetCondition(shared<Expression> value) {
		cond_ = value;
	}

	// Returns the statement that should be executed if the condition is true.
	shared<Statement> True() {
		return true_;
	}

	const shared<Statement> True() const {
		return true_;
	}

	void SetTrue(shared<Statement> value) {
		true_ = value;
	}

	// Returns the statement that should be executed if the condition is false.
	shared<Statement> False() {
		return false_;
	}

	const shared<Statement> False() const {
		return false_;
	}

	void SetFalse(shared<Statement> value) {
		false_ = value;
	}

	// Returns 'true' if the "then" part is present. 
    // NullStatement is not considered a body.
	bool HasThen() const {
		return true_ && (true_->IsNullStatement() == false);
	}

	// Returns 'true' if the 'else' part is present.
    // NullStatement is not considered a body.
	bool HasElse() const {
		return false_ && (false_->IsNullStatement() == false);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a 'for' statement. All parts, except the body, are optional.
class ForStatement : public Statement {
private:
	// for(initStatement_; condStatement_; incStatement_) body_
	shared<Statement> initStatement_;
	shared<Expression> condStatement_;
	shared<Expression> incStatement_;
	shared<Statement> body_;

protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	ForStatement(shared<Statement> initializer, shared<Expression> cond,
		         shared<Expression> inc, shared<Statement> body,
			     LocationInfo location) :
            Statement(STMT_FOR, location), initStatement_(initializer), 
            condStatement_(cond), incStatement_(inc), body_(body) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the condition inside the if statement.
	shared<Statement> Init() {
		return initStatement_;
	}

	const shared<Statement> Init() const {
		return initStatement_;
	}

	void SetInit(shared<Statement> value) {
		initStatement_ = value;
	}

	// Returns the condition inside the if statement.
	shared<Expression> Condition() {
		return condStatement_;
	}

	const shared<Expression> Condition() const {
		return condStatement_;
	}

	void SetCondition(shared<Expression> value) {
		condStatement_ = value;
	}

	// Returns the condition inside the if statement.
	shared<Expression> Increment() {
		return incStatement_;
	}

	const shared<Expression> Increment() const {
		return incStatement_;
	}

	void SetIncrement(shared<Expression> value) {
		incStatement_ = value;
	}

	// Returns the condition inside the if statement.
	shared<Statement> Body() {
		return body_;
	}

	const shared<Statement> Body() const {
		return body_;
	}

	void SetBody(shared<Statement> value) {
		body_ = value;
	}

	// Returns 'true' if a initialization expression/declaration exists.
	bool HasInit() const {
		return initStatement_;
	}

	// Returns 'true' if a stop condition exists.
	bool HasCondition() const {
		return condStatement_;
	}

	// Returns 'true' if an increment expression exists.
	bool HasIncrement() const {
		return incStatement_;
	}

	// Returns 'true' if a body is available.
    // NullStatement is not considered a body.
	bool HasBody() const {
		return (body_) && (body_->IsNullStatement() == false);
	}

	// Returns 'true' if this an infinite loop 'for(;;)'.
	bool IsInfinite() const {
		return !HasInit() && !HasCondition() && !HasIncrement();
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a 'while' loop statement. 
// Contains the condition and the body.
class WhileStatement : public Statement {
private:
	shared<Expression> cond_;
	shared<Statement> body_;

protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	WhileStatement(shared<Expression> cond, shared<Statement>&body, 
                   LocationInfo location) :
			Statement(STMT_WHILE, location), cond_(cond), body_(body) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the condition inside the if statement.
	shared<Expression> Condition() {
		return cond_;
	}

	const shared<Expression> Condition() const {
		return cond_;
	}

	void SetCondition(shared<Expression> value) {
		cond_ = value;
	}

	// Returns the condition inside the if statement.
	shared<Statement> Body() {
		return body_;
	}

	const shared<Statement> Body() const {
		return body_;
	}

	void SetBody(shared<Statement> value) {
		body_ = value;
	}

	// Returns 'true' if a body is available.
    // NullStatement is not considered a body.
	bool HasBody() const {
		return body_ && (body_->IsNullStatement() == false);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a 'do' loop statement.
// Contains the condition and the body.
class DoStatement : public Statement {
private:
	shared<Statement> body_;
	shared<Expression> cond_;
	LocationInfo whileLoc_; // The location where the 'while' part is located.

protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	DoStatement(shared<Expression> cond, shared<Statement> body,
		        LocationInfo location, LocationInfo whileLocation) :
			Statement(STMT_DO, location), cond_(cond), 
            body_(body), whileLoc_(whileLocation) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the condition inside the if statement.
	shared<Expression> Condition() {
		return cond_;
	}

	const shared<Expression> Condition() const {
		return cond_;
	}

	void SetCondition(shared<Expression> value) {
		cond_ = value;
	}

	// Returns the condition inside the if statement.
	shared<Statement> Body() {
		return body_;
	}

	const shared<Statement> Body() const {
		return body_;
	}

	void SetBody(shared<Statement> value) {
		body_ = value;
	}

	// Returns 'true' if a body is available. 
    // NullStatement is not considered a body.
	bool HasBody() const {
		return body_ && (body_->IsNullStatement() == false);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a 'continue' statement. 
// Contains a pointer to the jump target.
class ContinueStatement : public Statement {
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	ContinueStatement(LocationInfo location) : Statement(STMT_CONTINUE, location){}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a 'break' statement. 
// Contains a pointer to the jump target.
class BreakStatement : public Statement {
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	BreakStatement(LocationInfo location) : Statement(STMT_BREAK, location) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a 'return' statement.
// Either a value or nothing (void) can be returned.
class ReturnStatement : public Statement {
private:
	shared<Expression> returnVal_;
	
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	ReturnStatement(shared<Expression> returnValue, LocationInfo location) :
			Statement(STMT_RETURN, location), returnVal_(returnValue) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the condition inside the if statement.
	shared<Expression> Value() {
		return returnVal_;
	}

	const shared<Expression> Value() const {
		return returnVal_;
	}

	void SetValue(shared<Expression> value) {
		returnVal_ = value;
	}

	// Returns 'true' if nothing is returned, like in 'return;'.
	bool IsVoid() const {
		return returnVal_ != nullptr;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a label statement (some_name:). 
// Contains a pointer to the statement indicated by the label.
class LabelStatement : public Statement {
private:
	shared<Identifier> name_;  // The name of the label.
	shared<Statement> target_; // The statement the label indicates.
	
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	LabelStatement(shared<Identifier> name, shared<Statement> target,
                   LocationInfo location) :
			Statement(STMT_LABEL, location), name_(name), target_(target) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the name of the label.
	shared<Identifier> Name() {
		return name_;
	}

	const shared<Identifier> Name() const {
		return name_;
	}

	void SetName(shared<Identifier> value) {
		name_ = value;
	}

	// Returns the statement indicated by the label.
	shared<Statement> Target() {
		return target_;
	}

	const shared<Statement> Target() const {
		return target_;
	}

	void SetTarget(shared<Statement> value) {
		target_ = value;
	}

	// Returns 'true' if this is just a reference. 
    // References are created when the label appears 
    // in a 'goto' and it wasn't found yet.
	bool IsReference() const {
		return target_ == nullptr;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a goto statement.
class GotoStatement : public Statement {
private:
	shared<Statement> target_; // The statement the label indicates.
	
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	GotoStatement(shared<Statement> target, LocationInfo location) :
			Statement(STMT_GOTO, location), target_(target) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the statement indicated by the label.
	shared<Statement> Target() {
		return target_;
	}

	const shared<Statement> Target() const {
		return target_;
	}

	void SetTarget(shared<Statement> value) {
		target_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a case statement. Can appear only in a 'switch'.
// This class acts like a 'default' statement if a flag is set.
class CaseStatement : public Statement {
private:
	// case value_ : target_
	shared<Expression> value_; // The value associated with the case.
	shared<Statement> target_; // The statement the 'case' indicates.
	SwitchStatement* parent_;  // The associated 'switch' statement.
	bool isDefault_;           // True if it's the 'default' case.
	
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	CaseStatement(shared<Expression> value, shared<Statement> target, 
			      SwitchStatement* parent, bool isDefault, LocationInfo location) :
			Statement(STMT_CASE, location), value_(value), target_(target), 
            isDefault_(isDefault), parent_(parent) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this is a 'default' statement.
	bool IsDefault() const {
		return isDefault_;
	}

	// Returns the statement indicated by the label.
	shared<Expression> Value() {
		return value_;
	}

	const shared<Expression> Value() const {
		return value_;
	}

	void SetValue(shared<Expression> value) {
		value_ = value;
	}

	// Returns the statement indicated by the label.
	shared<Statement> Target() {
		return target_;
	}

	const shared<Statement> Target() const {
		return target_;
	}

	void SetTarget(shared<Statement> value) {
		target_ = value;
	}

	SwitchStatement* Parent() {
		return parent_;
	}

	const SwitchStatement* Parent() const {
		return parent_;
	}

	void SetParent(SwitchStatement* value) {
		parent_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a switch statement. Contains the branch condition
// and the list of optional case statements.
class SwitchStatement : public Statement {
private:
	// switch(cond_) { list_/default_ }
	shared<Expression> cond_;              // The condition to evaluate.
	List<shared<CaseStatement>> caseList_; // The list of case statements.
	shared<Statement> body_;               // The body of the 'switch'.
	
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	SwitchStatement(shared<Expression> cond, LocationInfo location) :
			Statement(STMT_SWITCH, location), cond_(cond) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the statement indicated by the label.
	shared<Expression> Condition() {
		return cond_; 
	}

	const shared<Expression> Condition() const {
		return cond_;
	}

	void SetCondition(shared<Expression> value) {
		cond_ = value;
	}

	// Returns the list of statements contained in the 'switch'.
	List<shared<CaseStatement>>& CaseList() {
		return caseList_; 
	}

	const List<shared<CaseStatement>>& CaseList() const {
		return caseList_;
	}

	// Returns the number of 'case' statements (without 'default').
	int CaseCount() const {
		return caseList_.Count() - (HasDefault() ? 1 : 0);
	}

	// Returns 'true' if there is a 'default' statement in the case list.
	bool HasDefault() const;

	// Returns the body of this 'switch'.
	shared<Statement> Body() {
		return body_;
	}

	const shared<Statement> Body() const {
		return body_;
	}

	void SetBody(shared<Statement> value) {
		body_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a compound statement, which is a group of other statements.
class CompoundStatement : public Statement {
private:
	List<shared<Statement>> list_; // The list of case statements.
	
protected:
    virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	CompoundStatement(LocationInfo location) :
			Statement(STMT_COMPOUND, location) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the list of child statements.
	List<shared<Statement>>& Children() {
		return list_; 
	}

	const List<shared<Statement>>& Children() const {
		return list_;
	}

	// Returns the child number.
	int Count() const {
		return list_.Count();
	}

	// Returns 'true' if there are no children.
	bool IsEmpty() const {
		return list_.Count() == 0;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents an empty statement. 
// For example, in 'while(a--);' the ; is the null statement.
class NullStatement : public Statement {
private:
	bool isError_; // 'true' if this statement marks an error.
	
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	NullStatement(LocationInfo location, bool isError = false) : 
			Statement(STMT_NULL, location), isError_(isError) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this statement is the result of an error.
	bool IsError() const {
		return isError_;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// An adapter that allows declarations to be considered statements.
class DeclarationStatement : public Statement {
private:
	shared<Declaration> base_;
	
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	DeclarationStatement(shared<Declaration> base, LocationInfo location) :
			Statement(STMT_DECL, location), base_(base) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the  associated declaration.
	shared<Declaration> Base() {
		return base_; 
	}

	const shared<Declaration> Base() const {
		return base_;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// An adapter that allows expressions to be considered statements.
class ExpressionStatement : public Statement {
private:
	shared<Expression> base_;
	
protected:
	virtual bool EqualsImpl(const Statement* other) const;
	virtual string ToStringImpl(int level) const override;

public:
	ExpressionStatement(shared<Expression> base, LocationInfo location) :
			Statement(STMT_EXPR, location), base_(base) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the list of child statements.
	shared<Expression> Base() {
		return base_; 
	}

	const shared<Expression> Base() const {
		return base_;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};

} // namespace AST
#endif