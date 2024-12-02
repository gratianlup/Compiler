// TypeAlignment.hpp
// Copyright (c) Lup Gratian
//
// Defines the class used to compute the alignment of types.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_TYPE_ALIGNMENT_HPP
#define PC_AST_TYPE_ALIGNMENT_HPP

#include "../Common/Context.hpp"
#include "Types.hpp"
#include "Visitor.hpp"
using namespace Common;

namespace AST {

class TypeAlignment : public Visitor {
private:
	const Type* type_;
	const Context*    context_;
	int         alignment_;

public:
	TypeAlignment(const Type* type, const Context* context) : 
			context_(context), type_(type), alignment_(0) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	int GetAlignment();

	virtual void Visit(const QType        *type);
	virtual void Visit(const BasicType	  *type);
	virtual void Visit(const PointerType  *type);
	virtual void Visit(const ArrayType	  *type);
	virtual void Visit(const VarArrayType *type);
	virtual void Visit(const FunctionType *type);
	virtual void Visit(const EnumType	  *type);
	virtual void Visit(const StructType	  *type);
	virtual void Visit(const UnionType	  *type);
	virtual void Visit(const TypedefType  *type);
};

} // namespace AST
#endif