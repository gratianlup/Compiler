// Types.hpp
// Copyright (c) Lup Gratian
//
// Defines all the helper classes derived from Type.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_TYPES_HPP
#define PC_AST_TYPES_HPP

#include "../Base/DebugValidator.hpp"
#include "../Base/List.hpp"
#include "../Common/TargetData.hpp"
#include "../Base/ObjectHash.hpp"
#include "Type.hpp"
#include "Statements.hpp"
#include "Declarations.hpp"
#include "Visitor.hpp"
using namespace Base;
using namespace Common;

namespace AST {

// Represents a basic, built-in type (char, int, float, etc.).
// For each basic type a 'BasicType' object is created and maintained
// during the duration of the compilation and references to it are used.
class BasicType : public Type {
private:
	static const shared<BasicType> types_[TypeKind::END];
	TypeKind type_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	BasicType();                             // Direct creation not allowed.
	BasicType(const BasicType&);             // Copying not allowed.
	BasicType& operator =(const BasicType&); // Assignment not allowed.

	// Constructor used to initialize the static array.
	BasicType(TypeKind kind) : type_(kind), Type(TYPE_BASIC) {}

	// Returns a reference to the specified type.
	static BasicType* GetType(TypeKind kind) {
		return types_[(int)kind];
	}

protected:
	virtual bool IsIncompleteImpl() const override;
	virtual bool EqualsImpl(const Type* type) const override;
	virtual string ToStringImpl(int level) const override;

public:
	virtual ~BasicType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Factory methods used to get a reference to a basic type.
	static BasicType* GetBool()      { return GetType(TypeKind::Bool);      }
	static BasicType* GetChar()      { return GetType(TypeKind::Char);      }
	static BasicType* GetUChar()     { return GetType(TypeKind::UChar);     }
	static BasicType* GetWChar()     { return GetType(TypeKind::WChar);     }
	static BasicType* GetString()    { return GetType(TypeKind::String);    }
	static BasicType* GetWString()   { return GetType(TypeKind::WString);   }
	static BasicType* GetShort()     { return GetType(TypeKind::Short);     }
	static BasicType* GetUShort()    { return GetType(TypeKind::UShort);    }
	static BasicType* GetInt()       { return GetType(TypeKind::Int);       }
	static BasicType* GetUInt()      { return GetType(TypeKind::UInt);      }
	static BasicType* GetLong()      { return GetType(TypeKind::Long);      }
	static BasicType* GetULong()     { return GetType(TypeKind::ULong);     }
	static BasicType* GetLongLong()  { return GetType(TypeKind::LongLong);  }
	static BasicType* GetULongLong() { return GetType(TypeKind::ULongLong); }
	static BasicType* GetFloat()     { return GetType(TypeKind::Float);     }
	static BasicType* GetDouble()    { return GetType(TypeKind::Double);    }
	static BasicType* GetVoid()      { return GetType(TypeKind::Void);      }
	static BasicType* GetSizeT()     { return GetType(TypeKind::ULongLong); }
	static BasicType* GetPtrDiffT()  { return GetType(TypeKind::LongLong);  }

	// Methods that verify the kind of the basic type.
	bool IsBool()      const { return type_ == TypeKind::Bool;      }
	bool IsChar()      const { return type_ == TypeKind::Char;      }
	bool IsUChar()     const { return type_ == TypeKind::UChar;     }
	bool IsWChar()     const { return type_ == TypeKind::WChar;     }
	bool IsString()    const { return type_ == TypeKind::String;    }
	bool IsWString()   const { return type_ == TypeKind::WString;   }
	bool IsShort()     const { return type_ == TypeKind::Short;     }
	bool IsUShort()    const { return type_ == TypeKind::UShort;    }
	bool IsInt()       const { return type_ == TypeKind::Int;       }
	bool IsUInt()      const { return type_ == TypeKind::UInt;      }
	bool IsLong()      const { return type_ == TypeKind::Long;      }
	bool IsULong()     const { return type_ == TypeKind::ULong;     }
	bool IsLongLong()  const { return type_ == TypeKind::LongLong;  }
	bool IsULongLong() const { return type_ == TypeKind::ULongLong; }
	bool IsFloat()     const { return type_ == TypeKind::Float;     }
	bool IsDouble()    const { return type_ == TypeKind::Double;    }
	bool IsVoid()      const { return type_ == TypeKind::Void;      }

	TypeKind GetKind() const {
		return (TypeKind)type_;
	}

	// Return 'true' if this is a signed type (all types of int).
	bool IsSignedInteger() const {
		return IsShort()  || IsInt()  || IsLong()  || IsLongLong();
	}

	// Return 'true' if this is an unsigned integer type (all types of int).
	bool IsUnsignedInteger() const {
		return IsUShort() || IsUInt() || IsULong() || IsULongLong();
	}

	// Return 'true' if this is an integral type (all types of int).
	bool IsInteger() const {
		return IsSignedInteger() || IsUnsignedInteger();
	}

	// Return 'true' if this is a floating type.
	bool IsFloating() const {
		return IsFloat() || IsDouble();
	}

	// Returns the rank of the basic type, as specified in C99:6.3.1.1.
	// char < short < int < long < long long. Signed and unsigned numbers
	// have the same rank.
	int Rank() const;

	// Returns 'true' if the rank of this type is smaller than the rank of the other.
	bool RankBelow(const BasicType* other) const {
		return Rank() <= other->Rank();
	}
	
	// Returns 'true' if the rank of this type is larger than the rank of the other.
	bool RankAbove(const BasicType* other) const {
		return Rank() >= other->Rank();
	}

	// Returns 'true' if the rank of this type is the same as the rank of the other.
	bool SameRank(const BasicType* other) const {
		return Rank() == other->Rank();
	}

	// Returns the size (in bytes) of the type under the specified target.
	int Size(const TargetData* target) const;

	// Returns the alignment of the type under the specified target.
	int GetAlignment(const TargetData* target) const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a pointer. It includes a reference to the pointed type.
// *int is represented as PointerType -> BasicType(int).
// **int is represented as PointerType -> PointerType -> BasicType(int).
class PointerType : public Type {
private:
	const Type* pointee_; // The pointed type.

protected:
	virtual bool IsVariableImpl() const override;
	virtual bool EqualsImpl(const Type* type) const override;
	virtual string ToStringImpl(int level) const override;

public:
	PointerType(const Type* pointee) : 
			Type(TYPE_POINTER), pointee_(pointee) {}

	virtual ~PointerType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the pointed type.
	const Type* PointeeType() const {
		return pointee_;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}

	// Computes the hash code for a pointer having the specified properties.
	static ObjectHash GetHashCode(const Type* pointee);
};


// Forward declaration used by 'ArrayType'.
class VarArrayType;

// Represents the base class for arrays. Used for arrays with known, constant size
// (like 'int a[3 + sizeof(int)]') and incomplete arrays (like 'int a[];').
// Variable length arrays use VarArrayType.
// The element type can be another 'ArrayType', resulting a matrix, and so on.
// int a[3] is represented as ArrayType(3) -> BasicType(int).
// int a[3][5] is represented as ArrayType(3) -> ArrayType(5) -> BasicType(int).
class ArrayType : public Type {
private:
	// The size of the array. Equal to 0 if not known.
	// Used only by this class, not by VarArrayType.
	__int64 size_;

protected:
	const Type* elemType_; // The type of the element.
	Qualifier qual_;
	char incomplete_ : 1;  // 'true' if the size is not known.
	char static_     : 1;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool IsIncompleteImpl() const override;
	virtual bool IsVariableImpl() const override;
	virtual bool EqualsImpl(const Type* type) const override;
	virtual string ToStringImpl(int level) const override;

public:
	static const __int64 INVALID_SIZE = -1;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ArrayType(const Type* elementType, const Qualifier& qual, bool incomplete,
			  __int64 size, bool isStatic) : 
			Type(TYPE_ARRAY), elemType_(elementType), qual_(qual), size_(size),
			incomplete_(incomplete), static_(isStatic) {}

	virtual ~ArrayType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this is a variable length array.
	virtual bool IsVariableArray() const {
		return false;
	}

	// Returns this type as a variable array type.
	VarArrayType* AsVariable() {
		return reinterpret_cast<VarArrayType*>(this);
	}

	const VarArrayType* AsVariable() const {
		return reinterpret_cast<const VarArrayType*>(this);
	}

	// Returns the type of the array element.
	const Type* ElementType() const {
		return elemType_;
	}

	// Returns 'true' if the length of the array has not been provided.
	bool IsIncomplete() const {
		return incomplete_ ;
	}

	// Returns the constant value that represents the length of the array.
	__int64 Size() const {
		return size_;
	}

	// Returns the associated qualifiers. Used when converting the array to a pointer.
	Qualifier Qualifiers() const {
		return qual_;
	}

	// Returns 'true' if 'static' is set on the array type.
	bool IsStatic() const {
		return static_;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}

	// Computes the hash code for an array having the specified properties.
	static ObjectHash GetHashCode(const Type* elementType, bool incomplete, __int64 size,
								bool isStatic, Qualifier qual);
};


// Represents a variable length array (appears only in the C99 standard).
// Incomplete arrays are represented as [*], and complete like [n], like in
// 'int a[m]', where m is a variable.
class VarArrayType : public ArrayType {
private:
	shared<Expression> sizeExpr_; // The expression used to initialize the array.

protected:
	virtual bool EqualsImpl(const Type* type) const override;
	virtual string ToStringImpl(int level) const override;

public:
	VarArrayType(const Type* elementType, shared<Expression> sizeExpr,
				 const Qualifier& qual, bool isStatic) :
			ArrayType(elementType, qual, false /* incomplete */, 0, isStatic),
			sizeExpr_(sizeExpr) {}

	virtual ~VarArrayType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this is a variable length array.
	virtual bool IsVariableArray() const {
		return true;
	}

	// Returns the statement that computes the length of the array.
	const shared<Expression> SizeExpression() const {
		return sizeExpr_;
	}

	// Returns 'true' if the array is of the form [*].
	bool IsStart() const {
		return sizeExpr_ == nullptr;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}

	// Computes the hash code for a variable array having the specified properties.
	static ObjectHash GetHashCode(const Type* elementType, shared<Expression> sizeExpr,
								bool isStatic, Qualifier qual);
};


// Represents a function. If the function has a return type, a list of parameters,
// a body (if the function is defined), and various flags.
class FunctionType : public Type {
private:
	List<const Type*> params_; // The types of the function parameters.
	const Type* returnType_;   // The type returned by the function.
	bool isVarargs_;           // 'true' if the function ends in ellipsis ...
	
protected:   
	virtual bool EqualsImpl(const Type* type) const override;
	virtual string ToStringImpl(int level) const override;

public:
	FunctionType(const Type* returnType, bool varargs_) :
			Type(TYPE_FUNCTION), returnType_(returnType), isVarargs_(varargs_) {}

	FunctionType(const Type* returnType, const List<const Type*>& parameters, bool varargs_ ) :
			Type(TYPE_FUNCTION), returnType_(returnType), 
			isVarargs_(varargs_), params_(parameters) {}

	virtual ~FunctionType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the type returned by the function.
	const Type* ReturnType() const {
		return returnType_;
	}

	// Returns the list of variable declaration that form the parameters.
	const List<const Type*>& Parameters() const {
		return params_;
	}

	// Returns the number of parameters.
	int ParameterCount() const {
		return params_.Count();
	}

	// Returns 'true' if the function has return type void (as in 'void f()').
	bool IsVoid() const;

	// Returns 'true' if the function accepts a variable number of parameters.
	bool IsVarargs() const {
		return isVarargs_;
	}
	
	// Returns 'true' if the function has a prototype  (has parameters).
	bool HasPrototype() const {
		return params_.Count() > 0;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}

	// Computes the hash code for a function having the specified properties.
	static ObjectHash GetHashCode(const Type* returnType, bool isVarargs,
								const List<const Type*>* paramTypes);
};


// Represents an enumeration. Contains a pointer to the type used to represent
// the enumeration constants and the list of constants.
class EnumType : public Type {
private:
	List<shared<EnumConstDeclaration>> constants_; // The enumeration constants.
	const BasicType* constType_;            // The type used to store the values.
	EnumDeclaration* decl_;                        // The associated declaration.

protected:
	virtual bool IsIncompleteImpl() const override;
	virtual bool EqualsImpl(const Type* type) const override;
	virtual string ToStringImpl(int level) const override;

public:
	EnumType(const BasicType* constType) :
			Type(TYPE_ENUM), constType_(constType), decl_(nullptr) {}

	virtual ~EnumType() {
		// Nothing to do here, the shared pointers takes care of all members.
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the type used to store the enumeration.
	const BasicType* ConstType() const {
		return constType_;
	}

	void SetConstType(const BasicType* value) {
		constType_ = value;
	}

	// Returns the list of the constants associated with the enumeration.
	List<shared<EnumConstDeclaration>>& Constants() {
		return constants_;
	}

	const List<shared<EnumConstDeclaration>>& Constants() const {
		return constants_;
	}

	// Returns the number of the associated constants.
	int ConstantCount() const {
		return constants_.Count();
	}

	// Returns 'true' if at least one of the constants has an assigned value.
	bool HasAssignedConstant() const;

	// Returns the associated declaration.
	EnumDeclaration* ParentDeclaration() {
		return decl_;
	}

	const EnumDeclaration* ParentDeclaration() const {
		return decl_;
	}

	void SetParentDeclaration(EnumDeclaration* value) {
		decl_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a base class for struct and union.
// Contains a list of the field that form the struct/union.
class StructUnionType : public Type {
protected:
	List<shared<FieldDeclaration>> fields_;  // The struct fields.
	shared<DeclarationContext> bodyContext_; // Contains the declarations in the body.
	StructUnionDeclaration* decl_;           // The associated declaration.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Constructor to be used only by derived classes.
	StructUnionType(int type) :	Type(type), decl_(nullptr) {}

	virtual bool EqualsImpl(const Type* type) const override;
	virtual bool IsIncompleteImpl() const override;
	virtual string ToStringImpl(int level) const override;

public:
	// Returns the list of the fields of struct.
	List<shared<FieldDeclaration>>& Fields() {
		return fields_;
	}

	const List<shared<FieldDeclaration>>& Fields() const {
		return fields_;
	}

	// Returns the number of the fields.
	int FieldCount() const {
		return fields_.Count();
	}

	// Returns the associated declaration.
	StructUnionDeclaration* ParentDeclaration() {
		return decl_;
	}

	const StructUnionDeclaration* ParentDeclaration() const {
		return decl_;
	}

	void SetParentDeclaration(StructUnionDeclaration* value) {
		decl_ = value;
	}

	// Returns the context associated with the body of the struct/union.
	shared<DeclarationContext> BodyContext() {
		return bodyContext_;
	}

	const shared<DeclarationContext> BodyContext() const {
		return bodyContext_;
	}

	void SetBodyContext(shared<DeclarationContext> value) {
		bodyContext_ = value;
	}

	// Returns 'true' if at least one of the fields is a bit-field.
	bool HasBitfield() const;

	// Returns 'true' if at least one of the fields is a unnamed bit-field.
	bool HasUnnamedBitfield() const;

	// Returns true if there is a flexible array at the end.
	// struct A { int a; int b[] };  'b' is a flexible array.
	bool HasFlexArray(bool children) const;

	// Returns 'true' if any field (at any level) has a 'const' qualifier.
	// Used to determine modifiable lvalues.
	bool HasConstMember() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a struct.
class StructType : public StructUnionType {
protected:
	virtual string ToStringImpl(int level) const override;

public:
	StructType() : StructUnionType(TYPE_STRUCT) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated declaration.
	StructDeclaration* ParentDeclaration();
	const StructDeclaration* ParentDeclaration() const;

	void SetParentDeclaration(StructUnionDeclaration* value) {
		decl_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents an union.
class UnionType : public StructUnionType {
protected:
	virtual string ToStringImpl(int level) const override;

public:
	UnionType() : StructUnionType(TYPE_UNION) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated declaration.
	UnionDeclaration* ParentDeclaration();
	const UnionDeclaration* ParentDeclaration() const;

	void SetParentDeclaration(StructUnionDeclaration* value) {
		decl_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}
};


// Represents a typedef (a synonym for another type).
// typedef int ABC - Parent = BasicType(int), Inner = BasicType(int).
// typedef ABC XYZ - Parent = TypedefType(ABC), Inner = BasicType(int).
class TypedefType : public Type {
private:
	const Type* parent_;
	const Type* inner_;  // The type found when stripping all 'typedef's.

protected:
	virtual bool EqualsImpl(const Type* type) const override;
	virtual bool IsVariableImpl() const override;
	virtual string ToStringImpl(int level) const override;

public:
	TypedefType(const Type* parent) :Type(TYPE_TYPEDEF), parent_(parent) {
		if(auto t = parent->As<TypedefType>()) {
			inner_ = t->Inner();
		}
		else inner_ = parent;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the parent of the definition. Can be another 'TypedefType'.
	const Type* Parent() const {
		return parent_;
	}

	// Returns the innermost parent (a 'Type' that is not a 'TypedefType').
	const Type* Inner() const {
		return inner_;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const override {
		v->Visit(this);
	}

	// Computes the hash code for a typedef having the specified parent.
	static ObjectHash GetHashCode(const Type* parentType);
};

} // namespace AST
#endif