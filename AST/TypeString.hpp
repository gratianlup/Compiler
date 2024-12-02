// TypeString.hpp
// Copyright (c) Lup Gratian
//
// Creates a string representation of a particular type.
// The string is as close as possible to the one found in the source file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_TYPE_STRING_HPP
#define PC_AST_TYPE_STRING_HPP

#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "Visitor.hpp"
#include "Types.hpp"
#include "Expressions.hpp"
using namespace Base;

namespace AST {

class TypeString : public Visitor {
private:
	StringBuilder sb_;
	bool printElemType_;

public:
	TypeString(const Type* startType);

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

	// Used to represent the size of expression of a VLA.
	virtual void Visit(const UnaryOperator	           *expr);
	virtual void Visit(const BinaryOperator	           *expr);
	virtual void Visit(const NumberConstant	           *expr);
	virtual void Visit(const CharConstant	           *expr);
	virtual void Visit(const StringConstant	           *expr);
	virtual void Visit(const SizeofOperator	           *expr);
	virtual void Visit(const SubscriptExpression       *expr);
	virtual void Visit(const MemberExpression	       *expr);
	virtual void Visit(const CallExpression		       *expr);
	virtual void Visit(const ConditionalOperator       *expr);
	virtual void Visit(const CastExpression		       *expr);
	virtual void Visit(const InitializerListExpression *expr);
	virtual void Visit(const DeclarationExpression     *expr);
	virtual void Visit(const InvalidExpression         *expr);
	virtual void Visit(const CompoundExpression	       *expr);

	string ToString() const {
		return sb_.ToString();
	}
};

} // namespace AST
#endif