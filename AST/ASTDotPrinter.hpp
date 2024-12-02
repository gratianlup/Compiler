// ASTDotPrinter.hpp
// Copyright (c) Lup Gratian
//
// Transforms the AST into a .dot file to be used by Graphviz.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_DOT_PRINTER_HPP
#define PC_AST_DOT_PRINTER_HPP

#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/Dictionary.hpp"
#include "Types.hpp"
#include "Expressions.hpp"
#include "Statements.hpp"
#include "Attributes.hpp"
#include "Visitor.hpp"
#include "Unit.hpp"
#include <iostream>
#include <fstream>
using namespace Base;

namespace AST {

class ASTDotPrinter : public Visitor {
private:
	Dictionary<size_t, int> statementIndex_;
	Dictionary<size_t, int> declIndex_;
	Dictionary<size_t, int> typeIndex_;
	std::wofstream fout;
	int nodes_;
	int clusters_;
	int identLevel_;
	bool showTypes_;
	bool useClusters_;
	bool expandFunct_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Helper methods.
	void Write(const string& s);
	void MakeTopLevelDecl(int i);
	void MakeType(const string& label);
	void MakeOperator(const string& label);
	void MakeUnaryOperator(const string& label);
	void MakeCast(const string& label);
	void MakeReference(const string& label);
	void MakeConstant(const string& label);
	void MakeExpression(const string& label, const string& color = _T("thistle"));
	void MakeStatement(const string& label);
	void MakeNullStatement(const string& label, bool isError);
	void MakeLoop(const string& label);
	void MakeDeclaration(const string& label);
	void MakeDeclarationLink(int a, int b, bool aDecl);
	void MakeLink(int a, int b, const string& label = _T(""), bool dashed = false);
	void MakeAttribute(const string& label);
	void BeginStatementCluster();
	void BeginDeclarationCluster();
	void BeginExpressionCluster();
	void BeginCallCluster();
	void BeginCastCluster();
	void EndCluster();
	void BeginGraph();
	void EndGraph();
	void PrintAttribute(const Attribute* attr);
	void PrintAttributes(const Declaration* declaration, int parent);

public:
	ASTDotPrinter(const string& path, bool showTypes, bool useClusters);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Prints the specified object.
	void Print(Visitable* start, bool expandFunct = true);

	// Prints the whole translation unit.
	void Print(Unit* unit, bool expandFunct = true);

	virtual void Visit(const QType        *type) override;
	virtual void Visit(const BasicType	  *type) override;
	virtual void Visit(const PointerType  *type) override;
	virtual void Visit(const ArrayType	  *type) override;
	virtual void Visit(const VarArrayType *type) override;
	virtual void Visit(const FunctionType *type) override;
	virtual void Visit(const EnumType	  *type) override;
	virtual void Visit(const StructType	  *type) override;
	virtual void Visit(const UnionType	  *type) override;
	virtual void Visit(const TypedefType  *type) override;

	virtual void Visit(const VariableDeclaration  *declaration) override;
	virtual void Visit(const FunctionDeclaration  *declaration) override;
	virtual void Visit(const EnumConstDeclaration *declaration) override;
	virtual void Visit(const EnumDeclaration      *declaration) override;
	virtual void Visit(const FieldDeclaration     *declaration) override;
	virtual void Visit(const StructDeclaration    *declaration) override;
	virtual void Visit(const UnionDeclaration     *declaration) override;
	virtual void Visit(const TypedefDeclaration   *declaration) override;
	virtual void Visit(const DeclarationList      *declaration) override;

	virtual void Visit(const UnaryOperator		       *expr) override;
	virtual void Visit(const BinaryOperator		       *expr) override;
	virtual void Visit(const NumberConstant	           *expr) override;
	virtual void Visit(const CharConstant		       *expr) override;
	virtual void Visit(const StringConstant	           *expr) override;
	virtual void Visit(const SizeofOperator		       *expr) override;
	virtual void Visit(const SubscriptExpression       *expr) override;
	virtual void Visit(const MemberExpression	       *expr) override;
	virtual void Visit(const CallExpression		       *expr) override;
	virtual void Visit(const ConditionalOperator       *expr) override;
	virtual void Visit(const CastExpression		       *expr) override;
	virtual void Visit(const InitializerListExpression *expr) override;
	virtual void Visit(const DeclarationExpression     *expr) override;
	virtual void Visit(const CompoundExpression	       *expr) override;
	virtual void Visit(const InvalidExpression         *expr) override;
	virtual void Visit(const NullExpression		       *expr) override;

	virtual void Visit(const IfStatement		  *statement) override;
	virtual void Visit(const ForStatement		  *statement) override;
	virtual void Visit(const WhileStatement		  *statement) override;
	virtual void Visit(const DoStatement		  *statement) override;
	virtual void Visit(const ContinueStatement	  *statement) override;
	virtual void Visit(const BreakStatement		  *statement) override;
	virtual void Visit(const ReturnStatement	  *statement) override;
	virtual void Visit(const LabelStatement		  *statement) override;
	virtual void Visit(const CaseStatement		  *statement) override;
	virtual void Visit(const SwitchStatement	  *statement) override;
	virtual void Visit(const GotoStatement		  *statement) override;
	virtual void Visit(const CompoundStatement	  *statement) override;
	virtual void Visit(const NullStatement		  *statement) override;
	virtual void Visit(const DeclarationStatement *statement) override;
	virtual void Visit(const ExpressionStatement  *statement) override;
};

} // namespace AST
#endif