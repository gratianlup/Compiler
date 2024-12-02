// Visitor.hpp
// Copyright (c) Lup Gratian
//
// Defines the classes that must be used by the Visitor system.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_VISITOR_HPP
#define PC_AST_VISITOR_HPP

namespace AST {

// Forward declarations.
class Visitor;
class QType;
class BasicType;
class PointerType;
class ArrayType;
class VarArrayType;
class FunctionType;
class EnumType;
class StructType;
class UnionType;
class StructUnionType;
class TypedefType;

class VariableDeclaration;
class FunctionDeclaration;
class EnumConstDeclaration;
class EnumDeclaration;
class FieldDeclaration;
class StructDeclaration;
class UnionDeclaration;
class TypedefDeclaration;
class DeclarationList;

class UnaryOperator;
class BinaryOperator;
class NumberConstant;
class CharConstant;
class StringConstant;
class SizeofOperator;
class SubscriptExpression;
class MemberExpression;
class CallExpression;
class ConditionalOperator;
class CastExpression;
class InitializerListExpression;
class DeclarationExpression;
class CompoundExpression;
class InvalidExpression;
class NullExpression;

class IfStatement;
class ForStatement;
class WhileStatement;
class DoStatement;
class ContinueStatement;
class BreakStatement;
class ReturnStatement;
class LabelStatement;
class GotoStatement;
class CaseStatement;
class SwitchStatement;
class CompoundStatement;
class NullStatement;
class DeclarationStatement;
class ExpressionStatement;


// All classes that can be visited must derive from this class.
class Visitable {
public:
	virtual ~Visitable() {}
	virtual void Accept(Visitor* v) const = 0;
};


// By default the dispatch methods do nothing.
class Visitor {
public:
	virtual ~Visitor() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual void Visit(const QType           *type) {}
	virtual void Visit(const BasicType	     *type) {}
	virtual void Visit(const PointerType     *type) {}
	virtual void Visit(const ArrayType	     *type) {}
	virtual void Visit(const VarArrayType    *type) {}
	virtual void Visit(const FunctionType    *type) {}
	virtual void Visit(const EnumType	     *type) {}
	virtual void Visit(const StructType	     *type) {}
	virtual void Visit(const UnionType	     *type) {}
	virtual void Visit(const StructUnionType *type) {}
	virtual void Visit(const TypedefType     *type) {}

	virtual void Visit(const VariableDeclaration  *declaration) {}
	virtual void Visit(const FunctionDeclaration  *declaration) {}
	virtual void Visit(const EnumConstDeclaration *declaration) {}
	virtual void Visit(const EnumDeclaration      *declaration) {}
	virtual void Visit(const FieldDeclaration     *declaration) {}
	virtual void Visit(const StructDeclaration    *declaration) {}
	virtual void Visit(const UnionDeclaration     *declaration) {}
	virtual void Visit(const TypedefDeclaration   *declaration) {}
	virtual void Visit(const DeclarationList      *declaration) {}

	virtual void Visit(const UnaryOperator	           *expr) {}
	virtual void Visit(const BinaryOperator	           *expr) {}
	virtual void Visit(const NumberConstant	           *expr) {}
	virtual void Visit(const CharConstant	           *expr) {}
	virtual void Visit(const StringConstant	           *expr) {}
	virtual void Visit(const SizeofOperator	           *expr) {}
	virtual void Visit(const SubscriptExpression       *expr) {}
	virtual void Visit(const MemberExpression	       *expr) {}
	virtual void Visit(const CallExpression		       *expr) {}
	virtual void Visit(const ConditionalOperator       *expr) {}
	virtual void Visit(const CastExpression		       *expr) {}
	virtual void Visit(const InitializerListExpression *expr) {}
	virtual void Visit(const DeclarationExpression     *expr) {}
	virtual void Visit(const CompoundExpression	       *expr) {}
	virtual void Visit(const InvalidExpression	       *expr) {}
	virtual void Visit(const NullExpression		       *expr) {}
	
	virtual void Visit(const IfStatement		  *statement) {}
	virtual void Visit(const ForStatement		  *statement) {}
	virtual void Visit(const WhileStatement		  *statement) {}
	virtual void Visit(const DoStatement	      *statement) {}
	virtual void Visit(const ContinueStatement	  *statement) {}
	virtual void Visit(const BreakStatement		  *statement) {}
	virtual void Visit(const ReturnStatement      *statement) {}
	virtual void Visit(const LabelStatement		  *statement) {}
	virtual void Visit(const CaseStatement		  *statement) {}
	virtual void Visit(const SwitchStatement	  *statement) {}
	virtual void Visit(const GotoStatement	      *statement) {}
	virtual void Visit(const CompoundStatement	  *statement) {}
	virtual void Visit(const NullStatement		  *statement) {}
	virtual void Visit(const DeclarationStatement *statement) {}
	virtual void Visit(const ExpressionStatement  *statement) {}
};

} // namespace Visitor
#endif