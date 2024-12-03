// ExpressionSemantic.hpp
// Copyright (c) Lup Gratian
//
// Defines the methods that perform semantic analysis on expressions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_EXPRESSION_SEMANTIC_HPP
#define PC_PARSING_EXPRESSION_SEMANTIC_HPP

#include "../Base/DebugValidator.hpp"
#include "../Common/Context.hpp"
#include "../AST/DeclarationContext.hpp"
#include "../AST/Declaration.hpp"
#include "../AST/Declarations.hpp"
#include "../AST/Expression.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/Type.hpp"
#include "../AST/Types.hpp"
#include "../Lexer/Token.hpp"
#include "../AST/Unit.hpp"
#include "../AST/TypeCombiner.hpp"
#include "../AST/TypeManager.hpp"
#include "ParserHelpers.hpp"
#include "Semantic.hpp"
#include <limits>

using namespace Base;
using namespace AST;
using namespace Common;
using namespace Lexing;

namespace Parsing {

class ExpressionSemantic : public Semantic {
protected:
	// Used by 'IsLValue' to indicate the type of the lvalue.
	// 'Value_LValue' means that the expression is a LValue.
	// The other constants are used for diagnostic purposes.
	enum ValueType {
		Value_LValue,
		Value_RValue,
		Value_Array,
		Value_Function,
		Value_Void,
		Value_Incomplete,
		Value_Const
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Context* context_;
	Diagnostic* diag_;
	TypeManager* types_;
	TypeCombiner typeComb_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Checks that a value doesn't overflow the specified integer type.
	bool IntFits(__int64 value, TypeKind kind);

	// Gets the rank of the specified integer. Used by 'ParseNumber' to choose
	// an the smallest integer type that can store the number.
	int GetIntRank(const NumberInfo& info);

	int GetIntRank(const BasicType* type);

	// 
	virtual shared<Expression> PromoteFunctionExpr(shared<Expression> expr);

	// 
	virtual shared<Expression> PromoteArrayExpr(shared<Expression> expr);

	// 
	virtual shared<Expression> 
	PromoteFunctionArrayExpr(shared<Expression> expr, bool removeQualifiers = true,
							 bool* changed = nullptr);

	// 
	virtual const Type* PromoteToInt(const Type* type);

	// 
	virtual shared<Expression> 
	PromoteArgumentDefault(shared<Expression> expr);

	// 
	virtual shared<Expression> 
	UsualConversion(shared<Expression> expr, bool removeQualifiers = true);

	// 
	virtual const BasicType* UnsignedFromSigned(const BasicType* type);

	// 
	virtual bool UsualBinaryConversion(shared<Expression>& left,
                                       shared<Expression>& right,
									   bool changeLeft = true);
	   
	// 
	virtual ValueType IsLValueImpl(shared<Expression> expr);

	// 
	virtual bool IsLValue(shared<Expression> expr, 
                          ValueType* type = nullptr);

	// 
	virtual bool IsModifiableLValue(shared<Expression> expr, 
                                    ValueType* type = nullptr);

	virtual bool AreTypesCompatible(const Type* a, const Type* b);

	// 
	virtual void EmitLValueDiagnostic(ValueType type, LocationInfo location);

	// 
	virtual void DiagnoseBinaryOp(shared<Expression> left,
        shared<Expression> right,
								  DiagnosticCode code, BinaryOpType opType);

	// 
	virtual void DiagnoseBinaryOp(const Type* left, shared<Expression> right,
								  DiagnosticCode code, BinaryOpType opType);

	// 
	virtual BinaryOpType BinaryOpFromToken(TokenKind kind);

	// 
	virtual void WarnQualifierDrop(const Type* a, const Type* b, 
                                   const Expression* expr, const Identifier* name, 
                                   bool isArg, LocationInfo location);

    //
	virtual Declaration* CheckBuiltinFunc(Identifier& ident, 
                                          shared<DeclarationContext> context);

    //
    virtual Declaration* CreateBuiltinFunc(shared<DeclarationContext> context);

public:
	ExpressionSemantic(Context* context, TypeManager* typeMan) :
			context_(context), diag_(&context->Diagnostic()),
			types_(typeMan), typeComb_(typeMan) {}

	virtual ~ExpressionSemantic() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// 
	virtual shared<Expression> 
	HandleNumber(NumberInfo& number, LocationInfo location);

	// 
	virtual shared<Expression> 
	HandleCharacter(CharInfo& number, LocationInfo location);

	// 
	virtual shared<Expression> 
	HandleString(StringInfo& number, LocationInfo location);

	// 
	virtual shared<Expression> 
	IntegerPromotion(shared<Expression> expr, bool* changed = nullptr);

	// 
	virtual shared<Expression> 
	CreateImplicitCast(shared<Expression> expr, const Type* newType,
                       CastType castType);

	// 
	virtual shared<Expression> 
	HandleIdentifier(Identifier& ident, shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleBinaryOp(TokenKind kind, shared<Expression> left, 
                   shared<Expression> right, LocationInfo operandLocation, 
                   shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleUnaryOp(TokenKind kind, shared<Expression> target, 
                  shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleIncDecOp(shared<Expression> target, bool isIncrement, bool isPostfix,
				   shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandlePostfixIncDecOp(shared<Expression> target, bool isIncrement, 
						  shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandlePrefixAddSubOp(shared<Expression> target, bool isAdd, 
					     shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleComplementOp(shared<Expression> target, 
                       shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleNotOp(shared<Expression> target, 
                shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleSizeof(shared<Expression> target, LocationInfo startLocation,
				 LocationInfo endLoc, shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleAddressOf(shared<Expression> target, 
                    shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleIndirection(shared<Expression> target, 
                      shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleCast(const Type* castType, shared<Expression> expr,
               LocationInfo castLoc, shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleCompoundLiteral(const Type* type, shared<InitInfo> initializer, 
                          LocationInfo location, shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleSubscript(shared<Expression> base, shared<Expression> index, 
						   LocationInfo leftLoc, LocationInfo rightLoc,
						   shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleMember(shared<Expression> object, shared<Identifier> name,
				 bool isPointer, shared<DeclarationContext> context);

	// 
	virtual void SuggestMember(const StructUnionType* type,
                               shared<Identifier> name);

	// 
	virtual shared<Expression> 
	HandleCallBegin(shared<Expression> target,
                    shared<DeclarationContext> context);

	// 
	virtual shared<Expression> 
	HandleCallEnd(shared<Expression> callExpr, LocationInfo startLocation, 
				  LocationInfo endLoc, shared<DeclarationContext> context);

	// 
	virtual bool
	HandlePrototypeArguments(CallExpression* callExpr, 
                             const FunctionType* functionType);

	// 
	virtual const Type*
	HandleMultiplicativeOp(shared<Expression>& left, shared<Expression>& right,
						   BinaryOpType opType, bool inAssignment, 
                           shared<DeclarationContext> context);
	
	// 
	virtual const Type*
	HandleComparisonOp(shared<Expression>& left, shared<Expression>& right,
					   BinaryOpType opType, shared<DeclarationContext> context,
					   LocationInfo operandLocation);

	// 
	virtual const Type*
	HandleAdditiveOp(shared<Expression>& left, shared<Expression>& right,
					 BinaryOpType opType, bool inAssignment, 
                     shared<DeclarationContext> context);

	// 
	virtual const Type*
	HandleBitwiseOp(shared<Expression>& left, shared<Expression>& right,
					BinaryOpType opType, bool inAssignment,
                    shared<DeclarationContext> context);

	// 
	virtual bool
	AreQualifiersCompatible(const Type* a, const Type* b);

	// 
	virtual const Type*
	HandleAssignmentOp(shared<Expression>& left, shared<Expression>& right,
					   BinaryOpType opType, shared<DeclarationContext> context);

	// 
	virtual const Type*
	HandleShiftOp(shared<Expression>& left, shared<Expression>& right,
				  BinaryOpType opType, bool inAssignment, 
                  shared<DeclarationContext> context);

	// 
	virtual const Type*
	HandleLogicalOp(shared<Expression>& left, shared<Expression>& right,
				    BinaryOpType opType, shared<DeclarationContext> context);

	// 
	virtual const Type*
	HandleCommaOp(shared<Expression>& left, shared<Expression>& right,
				  BinaryOpType opType, shared<DeclarationContext> context);

	// 
	virtual bool
	IsSimpleAssignmentValid(const Type* left, shared<Expression>& right,
						    const Type* compoundType);

	// 
	virtual shared<Expression>
	HandleConditionalOp(shared<Expression>& condition, shared<Expression>& left,
						shared<Expression>& right, LocationInfo operandLocation);

	// 
	virtual const Type*
	HandleConditionalPointers(const Type* leftType, const Type* rightType, 
							  shared<Expression>& left, shared<Expression>& right,
							  LocationInfo operandLocation);
};

} // namespace Parsing
#endif