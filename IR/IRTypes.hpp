// Types.hpp
// Copyright (c) Lup Gratian
//
// Defines all the types supported by the IR.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_TYPES_HPP
#define PC_IR_TYPES_HPP

#include "IRType.hpp"
#include "Visitor.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/List.hpp"
#include "../Base/ObjectHash.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class TypeTable;

// Defines the types of supported integers.
enum class IRIntegerKind {
	Int8,
	Int16,
	Int32,
	Int64
};


// Defines the types of supported floating values.
enum class IRFloatingKind {
	Float,
	Double
};


// Represents an integer type (i8...i64).
// Instances of this class can be obtained through the static factory methods.
class IntegerType : public Type {
private:
	// The instances of the 4 integer types.
	static const shared<IntegerType> types_[4];

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// The type is stored in the 'other_' field of the base class.
	IntegerType(IRIntegerKind kind) : Type((int)Kind::Integer, (int)kind) {}

protected:
	virtual string ToStringImpl(int level) const override;

public:
	// Factory methods used to get a reference to a basic type.
	static const IntegerType* GetInt(IRIntegerKind kind) {
		return types_[(int)kind];
	}

	static const IntegerType* GetInt8()  { return GetInt(IRIntegerKind::Int8);  }
	static const IntegerType* GetInt16() { return GetInt(IRIntegerKind::Int16); }
	static const IntegerType* GetInt32() { return GetInt(IRIntegerKind::Int32); }
	static const IntegerType* GetInt64() { return GetInt(IRIntegerKind::Int64); }

	// Returns the integer type that has the specified number of bytes.
	// If no such integer is found 'nullptr' is returned.
	static const IntegerType* GetHavingSize(int bytes) {
		switch(bytes) {
			case 1: return GetInt(IRIntegerKind::Int8);
			case 2: return GetInt(IRIntegerKind::Int16);
			case 4: return GetInt(IRIntegerKind::Int32);
			case 8: return GetInt(IRIntegerKind::Int64);
			default: return nullptr;
		}
	}

	virtual ~IntegerType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the subtype.
	IRIntegerKind GetSubtype() const {
		return (IRIntegerKind)other_;
	}

	// Methods that verify the kind of the basic type.
	bool IsInt8()  const { return (IRIntegerKind)other_ == IRIntegerKind::Int8;  }
	bool IsInt16() const { return (IRIntegerKind)other_ == IRIntegerKind::Int16; }
	bool IsInt32() const { return (IRIntegerKind)other_ == IRIntegerKind::Int32; }
	bool IsInt64() const { return (IRIntegerKind)other_ == IRIntegerKind::Int64; }

	// Returns the rank of the integer type (255 if not an integer).
	// i8 < i16 < i32 < i64.
	int Rank() const {
		return other_;
	}

	// Returns 'true' if the rank of this type is larger than the rank of the other.
	bool RankAbove(const IntegerType* other) const {
		return Rank() > other->Rank();
	}

	// Returns 'true' if the rank of this type is smaller than the rank of the other.
	bool RankBelow(const IntegerType* other) const {
		return Rank() < other->Rank();
	}

	// Returns 'true' if the rank of this type is the same as the rank of the other.
	bool SameRank(const IntegerType* other) const {
		return Rank() == other->Rank();
	}

	// Returns the size (in bytes) of the integer type.
	int Size() const {
		switch((IRIntegerKind)other_) {
			case IRIntegerKind::Int8:  return 1;
			case IRIntegerKind::Int16: return 2;
			case IRIntegerKind::Int32: return 4;
			case IRIntegerKind::Int64: return 8;
		}

		DebugValidator::Unreachable();
		return 0;
	}

	// Returns the size (in bits) of the integer type.
	int SizeInBits() const {
		return Size() * 8;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents a floating type (float/double).
// Instances of this class can be obtained through the static factory methods.
class FloatingType : public Type {
private:
	static const shared<FloatingType> types_[2];

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// The type is stored in the 'other_' field of the base class.
	FloatingType(IRFloatingKind kind) : Type((int)Kind::Floating, (int)kind) {}

protected:
	virtual string ToStringImpl(int level) const override;

public:
	// Factory methods used to get a reference to a basic type.
	static FloatingType* GetFloating(IRFloatingKind kind) {
		return types_[(int)kind];
	}

	static const FloatingType* GetFloat()  { return GetFloating(IRFloatingKind::Float);  }
	static const FloatingType* GetDouble() { return GetFloating(IRFloatingKind::Double); }

	virtual ~FloatingType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the subtype.
	IRFloatingKind GetSubtype() const {
		return (IRFloatingKind)other_;
	}

	// Methods that verify the kind of the basic type.
	bool IsFloat()  const { return (IRFloatingKind)other_ == IRFloatingKind::Float;  }
	bool IsDouble() const { return (IRFloatingKind)other_ == IRFloatingKind::Double; }

	// Returns the rank of the integer type (255 if not an integer).
	// i8 < i16 < i32 < i64.
	int Rank() const {
		return other_;
	}

	// Returns 'true' if the rank of this type is larger than the rank of the other.
	bool RankAbove(const FloatingType* other) const {
		return Rank() > other->Rank();
	}

	// Returns 'true' if the rank of this type is smaller than the rank of the other.
	bool RankBelow(const FloatingType* other) const {
		return Rank() < other->Rank();
	}

	// Returns 'true' if the rank of this type is the same as the rank of the other.
	bool SameRank(const FloatingType* other) const {
		return Rank() == other->Rank();
	}

	// Returns the size (in bytes) of the floating type.
	int Size() const {
		switch((IRFloatingKind)other_) {
            case IRFloatingKind::Float:  return 4;
            case IRFloatingKind::Double: return 8;
        }
    
        DebugValidator::Unreachable();
        return 0;
	}

    // Returns the size (in bits) of the floating type.
	int SizeInBits() const {
		return Size() * 8;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents the 'void' type. Should be used as the return type for 'void' functions.
// Instances of this class can be obtained through the static factory method.
class VoidType : public Type {
private:
	static const shared<VoidType> instance_;

protected:
	VoidType() : Type((int)Kind::Void) {}

	virtual string ToStringImpl(int level) const override;

public:
	// Factory methods used to get a reference to a basic type.
	static const VoidType* GetVoid() { 
		return instance_; 
	}

	virtual ~VoidType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents a pointer. It can point to any other type.
class PointerType : public Type {
private:
	const Type* pointee_; // The pointed type.

protected:
	virtual string ToStringImpl(int level) const override;

	PointerType(const Type* pointee) : Type((int)Kind::Pointer), pointee_(pointee) {}

public:
	friend class TypeTable; // Give it access to the constructor.

	virtual ~PointerType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the pointed type.
	const Type* PointeeType() const {
		return pointee_;
	}

	// Computes the hash code for a pointer having the specified properties.
	static ObjectHash GetHashCode(const Type* pointee);

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents an array. The element type should not be another array.
class ArrayType : public Type {
private:
	__int64 size_;         // The number of elements.
	const Type* elemType_; // The type of the element.

protected:
	virtual string ToStringImpl(int level) const override;

	ArrayType(const Type* elementType, __int64 size) :
			Type((int)Kind::Array), elemType_(elementType), size_(size) {}

public:
	friend class TypeTable; // Give it access to the constructor.

	virtual ~ArrayType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the type of the array element.
	const Type* ElementType() const {
		return elemType_;
	}

	// Returns the number of elements the array contains.
	__int64 Size() const {
		return size_;
	}

	// Computes the hash code for an array having the specified properties.
	static ObjectHash GetHashCode(const Type* elementType, __int64 size);

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents a function type. It can have additional attributes, like the calling convention.
class FunctionType : public Type {
private:
	typedef List<const Type*> TParamList;

	const Type* returnType_; // The type of the returned value.
	TParamList params_;     // The list of parameters.

	// Varargs info stored in 'other_'.

protected:
	virtual string ToStringImpl(int level) const override;
	
	FunctionType(const Type* returnType) : 
			Type((int)Kind::Function), returnType_(returnType) {}

	FunctionType(const Type* returnType, const Type** parameters,
                 int paramCount, bool isVarargs) : 
			Type((int)Kind::Function, isVarargs), 
			returnType_(returnType), params_(parameters, paramCount) {}

public:
	friend class TypeTable; // Give it access to the constructor.

	virtual ~FunctionType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the type of the returned value.
	const Type* ReturnType() const {
		return returnType_;
	}

    // Returns 'true' if the function returns nothing.
    bool HasVoidReturn() const {
        return returnType_->IsVoid();
    }

	// Returns the list of parameters.
	const TParamList& Parameters() const {
		return params_;
	}

	// Returns the number of parameters.
	int ParameterCount() const {
		return params_.Count();
	}

	// Returns 'true' if the function has variable number of arguments.
	bool IsVarargs() const {
		return other_;
	}

	// Returns 'true' if the function has no return type (it's 'void').
	bool IsVoid() const {
		return returnType_->IsVoid();
	}

	// Computes the hash code for a pointer having the specified properties.
	static ObjectHash GetHashCode(const Type* returnType, const Type** parameters, 
								  int paramCount, bool isVarargs);

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Represents a field of a record type, which is a combination of the
// type of the field and the offset (in bytes) from the start of the record.
struct RecordField {
	const Type* FieldType;
	__int64  FieldOffset; 

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	RecordField() : FieldType(nullptr), FieldOffset(0) {}

	RecordField(const Type* type, __int64 offset = 0) : 
			FieldType(type), FieldOffset(offset) {}
};


// Represents a record (struct/union in C). Contains a list of the fields,
// each fields being a pair of <type, offset>. 'offset' is the distance, in bytes,
// from the start of the record. All fields in an 'union' should have offset 0.
class RecordType : public Type {
private:
	List<RecordField> fields_;

protected:
	RecordType() : Type((int)Kind::Record) {}

	RecordType(const List<RecordField>& fields) : 
			Type((int)Kind::Record), fields_(fields) {}

	RecordType(int fieldCount) : 
			Type((int)Kind::Record), fields_(fieldCount) {}

	virtual string ToStringImpl(int level) const override;

public:
	friend class TypeTable; // Give it access to the constructor.

	virtual ~RecordType() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the field list.
	List<RecordField>& Fields() {
		return fields_;
	}

	const List<RecordField>& Fields() const {
		return fields_;
	}

	// Returns the field number.
	int FieldCount() const {
		return fields_.Count();
	}

    // Returns the type of the specified field.
    const Type* GetFieldType(int index) const {
        return fields_[index].FieldType;
    }

	const Type* GetFieldType(__int64 index) const {
        return fields_[(int)index].FieldType;
    }

    // Returns the offset of the specified field.
    __int64 GetFieldOffset(int index) const {
        return fields_[index].FieldOffset;
    }

	__int64 GetFieldOffset(__int64 index) const {
        return fields_[(int)index].FieldOffset;
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};

// Creates the helper class needed for 'As<T>'/'Is<T>'.
#define make_promotion_helper(NAME) \
	namespace Detail { \
		template <> \
		struct TypePromoter<NAME##Type> { \
			static bool Is(const Type* type) { \
				return (Type::Kind)type->kind_ == Type::Kind::##NAME; \
			} \
			static const NAME##Type* As(const Type* type) { \
				return Is(type) ? static_cast<const NAME##Type*>(type) : nullptr; \
			} \
		}; \
	}

make_promotion_helper(Integer)
make_promotion_helper(Floating)
make_promotion_helper(Void)
make_promotion_helper(Pointer)
make_promotion_helper(Array)
make_promotion_helper(Function)
make_promotion_helper(Record)
#undef make_promotion_helper

} // namespace IR
#endif