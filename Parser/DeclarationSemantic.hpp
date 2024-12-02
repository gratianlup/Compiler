// DeclarationSemantic.hpp
// Copyright (c) Lup Gratian
//
// Defines the semantic analysis handler for declarations.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_DECLARATION_SEMANTIC_HPP
#define PC_PARSING_DECLARATION_SEMANTIC_HPP

#include "../Base/DebugValidator.hpp"
#include "../Common/Context.hpp"
#include "../AST/DeclarationContext.hpp"
#include "../AST/Declaration.hpp"
#include "../AST/Declarations.hpp"
#include "../AST/Expression.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/Type.hpp"
#include "../AST/Types.hpp"
#include "../AST/Unit.hpp"
#include "../AST/TypeCombiner.hpp"
#include "../AST/TypeManager.hpp"
#include "ParserHelpers.hpp"
#include "Semantic.hpp"
using namespace Base;
using namespace AST;
using namespace Common;

namespace Parsing {

// Represents the type a tag can have.
enum TagType {
	Tag_Enum,
	Tag_Struct,
	Tag_Union
};


// Represents the type of the tag declaration.
enum TagDeclType {
	Tag_Declaration,
	Tag_Definition,
	Tag_Reference
};


class DeclarationSemantic : public Semantic {
protected:
	typedef DeclaratorInfo DI;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Context* context_;
	Diagnostic* diag_;
	TypeManager* types_;
	TypeCombiner typeComb_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Verifies if the two tags represent the same type of objects.
	virtual bool SameTagType(TagType a, shared<Declaration> b);

	// Reports an error about the redefinition of the specified tag.
	virtual void
	ReportTagRedefinition(TagType type, shared<Identifier> name, LocationInfo location);

	// Extracts a type from the given specifier info.
	// Returns a 'QType' with qualifiers applied if needed.
	virtual const Type* TypeFromSpecifiers(SpecifierInfo& info);

	// Returns a type adjusted for use as a function parameter.
	// Used for array and function types.
	virtual const Type* AdjustType(const Type* type);

	// Tries to combine two function declarations that have the same name
	// and are in the same scope. Modifications are made directly on 'first'.
	virtual const FunctionType*
	CombineFunctionDeclarations(const FunctionType* firstType, 
                                const FunctionDeclaration* previous);

	// Extracts the storage-class specifier from the given specifier info.
	virtual StorageType StorageFromSpec(SpecifierInfo& info);

	// Creates and validates a pointer type.
	virtual const Type*
	MakePointer(const Type* pointee,  Qualifier& qual, 
                shared<DeclarationContext> context);

	// Creates and validates an array type (handles VLA's too).
	virtual const Type*
	MakeArray(const Type* elementType, DI::ArrayInfo& info, LocationInfo location, 
			  shared<DeclarationContext> context, bool first);

	// Creates and validates a function type.
	virtual const Type*
	MakeFunction(const Type* returnType, DI* info, 
                 shared<DeclarationContext> context);

	// Creates the declaration of the specified tag. If it is a reference,
	// the type is taken from a previous declaration.
	virtual shared<Declaration> 
	CreateTagDecl(TagType type, shared<Identifier> name, LocationInfo location, 
				  const Type* declType, DeclarationContext* context);

	virtual bool
	CheckRedefinition(StorageType storage, shared<DeclarationContext> context, 
					  shared<Identifier> name);

	// Initializers a scalar value
	virtual shared<Expression> 
	InitializeScalar(const Type* type, shared<InitInfo> initializer, 
                     shared<InitContext> context);

	// 
	virtual shared<Expression>
	InitializeArray(const ArrayType* type, shared<InitContext> context);

	// 
	virtual shared<Expression> 
	InitializeStructUnion(const StructUnionType* type, 
                          shared<InitContext> context);

	// 
	virtual shared<Expression> 
	InitializeType(const Type* type, shared<InitContext> context, 
                   bool ignoreDesignators = false);

	// 
	virtual shared<Expression>
	InitializeTypeSimple(const Type* type, shared<InitInfo> initializer, 
						 shared<InitContext> context); 

	// 
	virtual shared<Expression>
	FillWithDefault(const Type* type, shared<InitContext> context);

	// 
	virtual shared<Expression> 
	HandleArrayDesignator(shared<Designator> designator, const Type* type,
						  shared<InitContext> context);

	// 
	virtual shared<Expression> 
	HandleFieldDesignator(shared<Designator> designator, const Type* type,
						  shared<InitContext> context);
	
	// 
	virtual shared<Expression> 
	HandleDesignator(shared<Designator> designator, const Type* type,
				     shared<InitContext> context);

	// 
	virtual shared<Expression>
	AsStringInitializer(shared<InitInfo> initializer);

	// 
	virtual shared<Expression> 
	InitializeString(const Type* type, shared<Expression> initializer,
					 shared<InitContext> context);

	// 
	virtual bool 
	CheckStringCompatibility(const Type* type, shared<Expression> initializer,
						     shared<InitContext> context);

	// Reports a diagnostic for struct/union declarations.
	virtual void ReportStructDiagnostic(DiagnosticCode code, shared<DI> declaration);

	// 
	virtual void ApplyAttributes(Declaration* declaration, SpecifierInfo& info,
								 Declaration* previous = nullptr);

public:
	DeclarationSemantic(Context* context, TypeManager* typeMan) :
			context_(context), diag_(&context->Diagnostic()), 
			types_(typeMan), typeComb_(typeMan) {}

	virtual ~DeclarationSemantic() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Verifies if a tag (struct/union/enum) is valid. Handles declarations,
	// definitions and references.
	virtual shared<Declaration> 
	HandleTagDecl(shared<DeclarationContext> context, TagType type, 
                  TagDeclType declType, bool hasName, shared<Identifier> name, 
                  LocationInfo location, bool& isReference);
		
	// Verifies the name and value of an 'enum' constant.
	virtual shared<EnumConstDeclaration> 
	HandleEnumConst(shared<DeclarationContext> context, 
                    shared<EnumDeclaration> enumDecl, shared<Identifier> name, 
                    bool hasValue, shared<Expression> valueExpr,
					LocationInfo startLocation, LocationInfo endLoc);

    // Evaluates the ICE that represents the enumeration constant.
    __int64 EvalueteEnumConst(shared<EnumConstDeclaration> enumConst);

	// Verifies a list of 'enum' constants. Sets the final values for all
	// constants and determines the basic type that represents the 'enum'.
	virtual bool
	HandleEnumList(shared<DeclarationContext> context, 
                   shared<EnumDeclaration> enumDecl, LocationInfo location);

	// Verifies a function parameter. Checks the type and if it's unique.
	virtual bool 
	HandleFunctionParam(DI::ParameterInfo* parameter, 
                        shared<DeclarationContext> context);	

	// Creates a type based on the given declarator sequence and specifier info.
	virtual const Type*
	MakeType(shared<DI> declarator, SpecifierInfo& info, 
             shared<DeclarationContext> context);

	// Creates a declaration for the specified declarator.
	// Validates the declaration and applies the initializer, if provided.
	virtual shared<Declaration> 
	HandleDeclaratorBegin(shared<DI> declaration, SpecifierInfo& info, 
						  shared<DeclarationContext> context, 
                          bool& isRedeclaration);

	// 
	virtual shared<Declaration> 
	HandleDeclaratorInitializer(shared<Declaration> declaration, 
                                SpecifierInfo& info,
								shared<InitInfo> initializer, 
                                shared<DeclarationContext> context);

	// 
	virtual shared<Declaration>
	HandleDeclaratorEnd(shared<Declaration> declaration, SpecifierInfo& info, 
					    shared<DeclarationContext> context);

	// Creates and validates 'TypedefDeclaration' object.
    // Combines two declarations if necessary.
	virtual shared<Declaration> 
	HandleTypedef(shared<DI> declaration, SpecifierInfo& info, const Type* type,
				  shared<DeclarationContext> context, Declaration* previous);

	// Creates and validates 'ArrayDecl' object. 
    // Combines two declarations if necessary.
	virtual shared<Declaration> 
	HandleArrayDeclarator(shared<DI> declaration, SpecifierInfo& info, 
                          const ArrayType* type, shared<DeclarationContext> context, 
                          Declaration* previous, bool isInvalid);

	// Creates and validates 'FunctionfDecl' object. 
    // Combines two declarations if necessary.
	virtual shared<Declaration> 
	HandleFunctionDeclarator(shared<DI> declaration, SpecifierInfo& info, 
							 const FunctionType* functionType, 
                             shared<DeclarationContext> context, 
							 Declaration* previous, bool isInvalid);

	// Validates the prototype of the 'main' function declaration/definition.
	virtual bool ValidateMain(shared<DI> declaration, SpecifierInfo& info, 
							  const FunctionType* functionType, 
                              shared<DeclarationContext> context);

	// Validates and applies the initializer to the declaration.
	// Most of the initializer handling is done in 'InitSemantic.cpp'.
	virtual bool
	HandleInitializer(shared<Declaration> declaration, SpecifierInfo& info, 
                      const Type* type, shared<InitInfo> initializer, 
                      shared<DeclarationContext> context);

	// Makes an incomplete array type complete by applying an initializer.
	const Type* CompleteWithInitializer(const Type* type, 
                                        shared<Expression> initExpr);

	// Determines the linkage and storage class for the specified declaration.
	// It depends on the presence of extern/static 
    // and on the previous declaration. (if any).
	virtual bool
	HandleLinkage(shared<DI> declaration, SpecifierInfo& info, const Type* type, 
				  Declaration* previous, shared<DeclarationContext> context, 
				  StorageType& storage, LinkageType& linkage);

	// Creates and validates a declaration for an aggregate field.
	virtual shared<FieldDeclaration>
	HandleFieldDeclaration(shared<DI> declaration, SpecifierInfo& info, 
						   shared<StructUnionDeclaration> parent, 
                           shared<DeclarationContext> context,
						   shared<Expression> bitfield, LocationInfo startLocation,
						   LocationInfo endLoc);

	// Validates a bitfield declaration.
	virtual shared<FieldDeclaration> 
	HandleBitfield(shared<DI> declaration, const Type* declType, 
				   shared<StructUnionDeclaration> parent, 
                   shared<DeclarationContext> context,
				   shared<Expression> bitfield, LocationInfo startLocation,
				   LocationInfo endLoc);

	// Validates a list of aggregate field declarations.
	virtual bool HandleFieldList(shared<StructUnionDeclaration> declaration, 
                                 shared<DeclarationContext> context,
								 LocationInfo startLocation);

	// Creates a type from the specified declarator info and specifiers.
	virtual const Type*
	HandleTypename(shared<DI> declaration, SpecifierInfo& info, 
                   shared<DeclarationContext> context);	

	// 
	virtual bool 
	HandleFunctionDefinitionBegin(shared<Declaration> declaration, 
                                  SpecifierInfo& info, 
								  shared<DeclarationContext> context);

	// 
	virtual bool 
	HandleFunctionDefinitionEnd(shared<Declaration> declaration, 
                                shared<Statement> bodyStatement,
								shared<DeclarationContext> context);

	//
	virtual bool HandleUnitEnd(shared<DeclarationContext> context, Unit& unit);

	// 
	virtual void AddDefaultInitializer(VariableDeclaration* declaration, 
                                       shared<DeclarationContext> context);

	// Returns 'true' if the expression (and all it's subexpressions) are constants.
	virtual bool AllConstant(shared<Expression> expr);

	// 
	virtual shared<Expression> 
	MakeInitializer(const Type* type, shared<InitContext> context);

    //
    virtual void CreatePrototypeVariables(DeclaratorInfo* declaration, 
                                          shared<DeclarationContext> context);
};

} // namespace Parsing
#endif