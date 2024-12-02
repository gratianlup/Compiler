// Declarations.hpp
// Copyright (c) Lup Gratian
//
// Defines all the helper classes derived from Declaration.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_DECLARATIONS_HPP
#define PC_AST_DECLARATIONS_HPP

#include "Declaration.hpp"
#include "Expression.hpp"

namespace AST {

// Forward declarations.
class EnumType;
class FunctionType;
class EnumDeclaration;
class StructUnionDeclaration;
class DeclarationContext;
class StructUnionType;
class StructType;
class UnionType;
class TypedefType;


// Represents the declaration of a variable (like in 'int a = 5').
// Optionally, it can have an initialization expression.
// A variable declaration becomes a definition if it has an initializer.
class VariableDeclaration : public Declaration {
private:
	shared<Expression> init_;
	bool tentative_;

protected:
	virtual string ToStringImpl(int level) const override;
	virtual bool EqualsImpl(const Declaration* other) const override;

public:
	VariableDeclaration(shared<Identifier> ident, const Type* type,
				        LocationInfo start, LocationInfo end,
				        shared<Expression> initializer, bool def = false, 
				        Declaration* previous = nullptr,
                        Declaration* next = nullptr) :
			Declaration(DECL_VARIABLE, ident, type, start, end, def, previous, next),
            init_(initializer), tentative_(false) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated initialization expression.
	shared<Expression> Initializer() {
		return init_;
	}

	const shared<Expression> Initializer() const {
		return init_;
	}

	void SetInitializer(shared<Expression> value) {
		init_ = value;
	}

	// Returns 'true' if 'extern' or 'static' is set.
	bool HasStorage() const {
		return (storage_ == StorageType::Static) || (storage_ == StorageType::Extern);
	}

	// Returns 'true' if the variable has external or internal linkage.
	bool IsGlobal() const {
		return (linkage_ == LinkageType::External) || (linkage_ == LinkageType::Internal);
	}

	// Returns 'true' if the variable is a tentative definition (has a default
	// initializer that should be used if no explicit initializer is found
	// while linking the translation units).
	bool IsTentative() const {
		return tentative_;
	}

	void SetIsTentative(bool value) {
		tentative_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a declaration of a function.
class FunctionDeclaration : public Declaration {
private:
	shared<CompoundStatement> body_;           // The body of the function. Only for definitions.
	shared<DeclarationContext> bodyContext_;   // Contains the declarations in the body.
	List<shared<VariableDeclaration>> params_; // The parameters of the function.
	bool isStatic_ : 1;
	bool isInline_ : 1;

protected:
	virtual string ToStringImpl(int level) const override;
	virtual bool EqualsImpl(const Declaration* other) const override;

public:
	FunctionDeclaration(shared<Identifier> ident, const Type* type,
				        LocationInfo start, LocationInfo end, 
                        bool isStatic = false, bool isInline = false, 
                        Declaration* previous = nullptr, Declaration* next = nullptr) :
			Declaration(DECL_FUNCTION, ident, type, start, end, false, previous, next),
			isStatic_(isStatic), isInline_(isInline) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated function type.
	const FunctionType* DeclarationType() const;

	// Returns the list of variable declaration that form the parameters.
	List<shared<VariableDeclaration>>& Parameters() {
		return params_;
	}

	const List<shared<VariableDeclaration>>& Parameters() const {
		return params_;
	}

	// Returns the number of parameters.
	int ParameterCount() const {
		return params_.Count();
	}

	// Returns 'true' is the function is declared as 'static'.
	bool IsStatic() const {
		return isStatic_;
	}

	void SetIsStatic(bool value) {
		isStatic_ = value;
	}

	// Returns 'true' is the function is declared as 'inline'.
	bool IsInline() const {
		return isInline_;
	}

	void SetIsInline(bool value) {
		isInline_ = value;
	}

	// Returns the associated body (valid only if this is a definition).
	shared<CompoundStatement> Body() {
		return body_;
	}

	const shared<CompoundStatement> Body() const {
		return body_;
	}

	void SetBody(shared<CompoundStatement> value) {
		body_ = value;
	}

	// Returns the context associated with the body of the function.
	shared<DeclarationContext> BodyContext() {
		return bodyContext_;
	}

	const shared<DeclarationContext> BodyContext() const {
		return bodyContext_;
	}

	void SetBodyContext(shared<DeclarationContext> value) {
		bodyContext_ = value;
	}

	// Returns 'true' if the function is globally visible (external linkage).
	bool IsGlobal() const;

	// Returns 'true' if the function is the application entry point.
	bool IsMain() const;

	// Returns 'true' if 'extern' or 'static' is set.
	bool HasStorage() const {	
		return (storage_ == StorageType::Static) || (storage_ == StorageType::Extern);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a declaration of a constant in an enumeration.
// The declaration can have an optional value.
class EnumConstDeclaration : public Declaration {
private:
	EnumDeclaration* parent_;
	shared<Expression> valueExpr_;

protected:
	virtual string ToStringImpl(int level) const override;
	virtual bool EqualsImpl(const Declaration* other) const override;

public:
	EnumConstDeclaration(shared<Identifier> ident, const Type* type,
				         LocationInfo start, LocationInfo end, 
                         EnumDeclaration* parent, shared<Expression> value = nullptr) :
			Declaration(DECL_ENUM_CONST, ident, type, start, end, true),
			parent_(parent), valueExpr_(value) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the parent enumeration declaration.
	EnumDeclaration* Parent() {
		return parent_;
	}

	const EnumDeclaration* Parent() const {
		return parent_;
	}

	// Returns 'true' if the constant has a value.
	bool HasValue() const {
		return valueExpr_;
	}

	// Returns the expression that represents the constant value.
	shared<Expression> ValueExpr() {
		return valueExpr_;
	}

	const shared<Expression> ValueExpr() const {
		return valueExpr_;
	}

	void SetValueExpr(shared<Expression> value) {
		valueExpr_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents an enumeration declaration. Contains a list of the constants
// that represents the body of the enum. The constants can also be found
// in the context of the enum (not in a separate context, because of leakage).
class EnumDeclaration : public Declaration {
protected:
	virtual string ToStringImpl(int level) const override;
	virtual bool EqualsImpl(const Declaration* other) const override;

public:
	EnumDeclaration(shared<Identifier> ident, const Type* type,
			 LocationInfo start, LocationInfo end, bool def = false, 
			 Declaration* previous = nullptr, Declaration* next = nullptr) :
			 Declaration(DECL_ENUM, ident, type, start, end, def, previous, next) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated function type.
	const EnumType* DeclarationType() const;
	EnumType* DeclarationType();

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents the declaration of a field in a struct/union.
// Fields can have bit-fields (like a:12). '12' is the 'width' of the bit-field.
// A bit-field (and only a bit-field) can be unnamed.
class FieldDeclaration : public Declaration {
private:
	StructUnionDeclaration* parent_; // The struct/union to which this filed belongs.
	char bitfield_;           // -1 means no bitfield.

protected:
	virtual bool EqualsImpl(const Declaration* other) const override;
	virtual string ToStringImpl(int level) const override;

public:
	FieldDeclaration(shared<Identifier> ident, const Type* type, 
                     int bitfield, StructUnionDeclaration* parent, 
                     LocationInfo start, LocationInfo end, 
			         Declaration* previous = nullptr, Declaration* next = nullptr) :
			 Declaration(DECL_ENUM, ident, type, start, end, true, previous, next),
			 parent_(parent), bitfield_(bitfield) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the struct/union to which this filed belongs.
	StructUnionDeclaration* Parent() {
		return parent_;
	}

	const StructUnionDeclaration* Parent() const {
		return parent_;
	}

	// Returns 'true' if the field has an associated bit-field.
	bool IsBitfield() const {
		return bitfield_ >= 0;
	}

	// Returns 'true' if the bit-field is unnamed (:0).
	bool IsUnnamedBitfield() const {
		return IsBitfield() && (ident_ == nullptr);
	}

	// Returns the expression that represents the bit-field.
	int Bitfield() const {
		return bitfield_;
	}

	void SetBitfield(int value) {
		bitfield_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents the declaration of a struct or an union.
class StructUnionDeclaration : public Declaration {
protected:
	// The constructor should be used only by derived classes.
	StructUnionDeclaration(int kind, shared<Identifier> ident, const Type* type,
					       LocationInfo start, LocationInfo end, bool def = false, 
					       Declaration* previous = nullptr, Declaration* next = nullptr) :
            Declaration(kind, ident, type, start, end, def, previous, next) {}

	virtual bool EqualsImpl(const Declaration* other) const override;

public:
	// Returns the associated struct/union type.
	const StructUnionType* DeclarationType() const;
	StructUnionType* DeclarationType();
};


// Represents the declaration of a struct.
// struct A;           - declaration.
// struct A { int t; } - definition.
class StructDeclaration : public StructUnionDeclaration {
protected:
	virtual string ToStringImpl(int level) const override;

public:
	StructDeclaration(shared<Identifier> ident, const Type* type,
			          LocationInfo start, LocationInfo end, bool def = false, 
			          Declaration* previous = nullptr, Declaration* next = nullptr) :
            StructUnionDeclaration(DECL_STRUCT, ident, type, 
                                   start, end, def, previous, next) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated struct type.
	const StructType* DeclarationType() const;
	StructType* DeclarationType();

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents the declaration of an union.
class UnionDeclaration : public StructUnionDeclaration {
protected:
	virtual string ToStringImpl(int level) const override;

public:
	UnionDeclaration(shared<Identifier> ident, const Type* type,
			         LocationInfo start, LocationInfo end, bool def = false, 
			          Declaration* previous = nullptr, Declaration* next = nullptr) :
			  StructUnionDeclaration(DECL_UNION, ident, type, 
                                     start, end, def, previous, next) {}
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated union type.
	const UnionType* DeclarationType() const;
	UnionType* DeclarationType();

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents the declaration of a typedef (type synonym).
class TypedefDeclaration : public Declaration {
protected:
	virtual string ToStringImpl(int level) const override;
	virtual bool EqualsImpl(const Declaration* other) const override;

public:
	TypedefDeclaration(shared<Identifier> ident, const Type* type,
				       LocationInfo start, LocationInfo end) :
			    Declaration(DECL_TYPEDEF, ident, type, start, end, true) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated typedef type.
	const TypedefType* DeclarationType() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a list of declarations with 
// the same type specifiers (like 'int a, b, c, d[2], *p').
class DeclarationList : public Declaration {
private:
	List<shared<Declaration>> list_;

protected:
	virtual string ToStringImpl(int level) const override;
	virtual bool EqualsImpl(const Declaration* other) const override;

public:
	DeclarationList(LocationInfo startLocation) :
			Declaration(DECL_LIST, nullptr, nullptr, startLocation, startLocation, true) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the list of declarations.
	List<shared<Declaration>>& Declarations() {
		return list_;
	}

	const List<shared<Declaration>>& Declarations() const {
		return list_;
	}

	// Returns the number of declarations.
	int Count() const {
		return list_.Count();
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};

} // namespace AST
#endif