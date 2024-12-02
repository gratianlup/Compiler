// UnitGenerator.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_UNIT_GEN_HPP
#define PC_IR_GENERATOR_UNIT_GEN_HPP

#include "GeneratorHelpers.hpp"
#include "NameGenerator.hpp"
#include "TypeGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "GeneratorEvents.hpp"
#include "../AST/Types.hpp"
#include "../AST/Unit.hpp"
#include "../AST/Attributes.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/TypeAlignment.hpp"
#include "../AST/StructLayout.hpp"
#include "../IR/IRTypes.hpp"
#include "../IR/Symbols.hpp"
#include "../IR/Unit.hpp"
#include "../IR/TypeTable.hpp"
#include "../IR/Intrinsic.hpp"
#include "../IR/References.hpp"
#include "../IR/ConstantTable.hpp"
#include "../IR/Tags.hpp"
#include "../Common/Context.hpp"
#include "../Common/TargetData.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/Stack.hpp"
using namespace Base;
using namespace AST;

namespace IRGenerator {

class UnitGenerator {
public:
	// The maximum number of fields of a 'struct' can have 
	// so that it's a candidate for 'struct' parameter expansion.
	static const int STRUCT_EXPANSION_MAX_FIELDS = 4;

	// The maximum number of nested 'struct' types accepted for parameter expansion.
	static const int STRUCT_EXPANSION_MAX_LEVELS = 2;

private:
	// Maps a global variable to it's IR representation.
	Dictionary<const VariableDeclaration*, IR::GlobalVariable*> globalVars_;   
	// Maps a function to it's IR representation.
	Dictionary<const FunctionDeclaration*, shared<FunctionHolder>> functions_; 
	// Caches the results of 'IsSimpleRecord'.
	Dictionary<SimpleRecord, bool> simpleRecords_; 

	IR::Unit* irUnit_;                  // The resulting IR unit.
	Unit* unit_;                        // The Unit for which we generate code.
	Context* context_;                  // The context of the compilation.
	const Common::TargetData* target_;  // The target of the compilation.
	shared<TypeGenerator> typeGen_;     // Helper that creates IR types.
	shared<LayoutCache> layouts_;       // Caches the layout of a record.
	FunctionHolder* currentFunction_;   // The function for which code is now generated.
	Stack<FunctionHolder*> functStack_; // The functions for which code is now generated.
	GeneratorEvents* events_;           // The associated event listener.
    NameGenerator nameGen_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Return the alignment, in bytes, of the specified variable.
	int GetAlignment(const Declaration* declaration);

	// Returns the module visibility of the specified variable or function.
	IR::SymbolVisibility GetVisibility(const Declaration* declaration);

	// Returns the DLL visibility of the specified variable or function.
	// Checks if the associated attribute is set.
	IR::DllVisibility GetDllVisibility(const Declaration* declaration);

	// Returns the section in which the specified variable should be put.
	// Checks if the associated attribute is set.
	shared<string> GetSection(const Declaration* declaration);

	// Returns the 'inline' type of the specified function.
	// Checks if the associated attribute is set.
	IR::InlineType GetInline(const Declaration* declaration);

	// Returns the calling convention that should be used when calling the specified function.
	// Checks if the associated attribute is set.
	bool GetCallConvention(const Declaration* declaration, 
                           IR::CallConventionType& type);
	
	// Returns the the object referred in the specified initializer expression.
	// Used for initializers that involve pointers and other variables.
	// If 'expectInt' is set it returns 'nullptr' if a subscript or member expression
	// is found, because their target object should be ignored in this case.
	const Expression* FindBaseObject(const Expression* expr,
                                     bool expectInt = false);

	// Creates a string initializer from the specified string constant.
	shared<IR::Initializer> 
	GetStringInitializer(const StringConstant* str, 
                         const ArrayType* targetType = nullptr);

	// Creates an integer initializer. Also handles cases like pointer converted to integer.
	shared<IR::Initializer> 
	GetIntegerInitializer(const Type* destType, EvaluationInfo& eval, 
                          const Expression* base);

	// Creates a floating-point initializer.
	shared<IR::Initializer> 
	GetFloatingInitializer(const Type* destType, EvaluationInfo& eval, 
                           const Expression* base);

	// Creates an initializer for a pointer. Also handles cases like integer to pointer.
	shared<IR::Initializer> 
	GetPointerInitializer(const Type* destType, EvaluationInfo& eval, 
                          const Expression* base);

	// Creates an initializer that should be used with arrays 
    // initialized with string constants.
	shared<IR::Initializer> 
	GetArrayStringInitializer(const Type* destType, EvaluationInfo& eval, 
                              const Expression* base);
	
	// Creates an integer constant that represents a bitfield.
	void CreateBitfield(__int64 value, const IR::Type* type, 
                        IR::InitializerList* irInitList);

	// Creates an initializer list for a struct/union type.
	void GetRecordInitializer(const StructUnionType* destType, 
                              const InitializerListExpression* initList, 
							  IR::InitializerList* irInitList);

	// Creates an initializer appropriate for the specified expression 
	// and destination type, inserting casts if necessarily.
	shared<IR::Initializer> GetInitializer(const Expression* initializer, 
                                           const Type* destType);

	// Creates an initializer from the specified expression
	// and associates it with the global variable.
	void AddInitializer(IR::GlobalVariable* irVariable, 
                        const Expression* initExpr, const Type* variableType);

	// Creates and associates the variable initializer
    // with the specified global variable.
	void AddInitializer(IR::GlobalVariable* irVariable, 
                        const VariableDeclaration* variable);

	// Creates the function declaration and adds it to the IR Unit.
	// The function is added now only if it's a definition or it's marked as 'extern'.
	void GenerateFunctionDeclaration(const FunctionDeclaration* functDecl);

	// Creates and adds the parameters to the IR function.
	// This also expands "simple" records as individual parameters.
	void GenerateFunctionParameters(const FunctionDeclaration* functDecl, 
                                    FunctionHolder* functHolder);

	// Generates a parameter having the specified type and name.
	IR::Variable* GenerateParameter(const IR::Type* irType, const Type* type,
                                    string* name, IR::Function* irFunct);

	// Expands a 'struct' type into individual function parameters,
	// one for each field. Also handles nested 'struct' types.
	void ExpandStructType(const StructType* structType, string* name,
						  VariableDeclaration* parameter, 
                          FunctionHolder* functHolder);

	// Creates the variable declaration and adds it to the IR Unit.
	// The variable is added now only if it's marked as 'extern'.
	IR::GlobalVariable* GenerateVariableDeclaration(const VariableDeclaration* variableDecl);

	// Makes the specified function the active function and begins generating code for it.
	// If the function has not been added to the IR yet it's added now.
	void BeginFunction(const FunctionDeclaration* function);

	// Helper that tests if the specified record meets the criteria for a "simple record".
	bool IsSimpleRecordImpl(const StructUnionType* recordType, int& fields, int level,
						    int maxFields, int maxLevels, bool unionAllowed = true);

	// Returns 'true' if the specified 'struct' is considered "simple".
	bool IsSimpleStruct(const StructUnionType* recordType, int& fields, int level,
						int maxFields, int maxLevels, bool unionAllowed = true);

	// Returns 'true' if the specified 'union' is considered "simple".
	bool IsSimpleUnion(const StructUnionType* recordType, int& fields, int level,
					   int maxFields, int maxLevels);

	// Adds the specified function to the IR. 
	// The function is added before the one that is being generated (if it's the case).
	void AddFunction(IR::Function* irFunct, FunctionHolder* holder);

	// Marks a function as originating from a standard library header,
	// if it's the cases. Allows the backend to perform more aggressive optimizations.
	void MarkFromStdlib(IR::Function* function, const FunctionDeclaration* functDecl);

public:
	UnitGenerator(Unit* unit, Context* context, IR::TypeTable* types, 
				  IR::ConstantTable* consts, IR::IntrinsicTable* intrinsics,
				  GeneratorEvents* events = nullptr);

	~UnitGenerator();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Generates code for all declarations in the unit (types, variables and functions).
	// Type definitions (typedef) are emitted if 'emitTypedef' is set.
	IR::Unit* Generate(bool emitTypedef = false);

	// Methods for accessing the helper objects.
	Unit*                     GetUnit()    { return unit_;    }
	IR::Unit*                 GetIRUnit()  { return irUnit_;  }
	LayoutCache*              GetLayouts() { return layouts_; }
	TypeGenerator*            GetTypeGen() { return typeGen_; }
	Context*                  GetContext() { return context_; }
	const Common::TargetData* GetTarget()  { return target_;  }
	GeneratorEvents*          GetEvents()  { return events_;  }

	// Creates a reference to the specified global variable.
	IR::VariableReference* GetVariableReference(IR::GlobalVariable* irVariable);

	// Creates a reference to the variable associated with the specified declaration.
	IR::VariableReference* GetVariableReference(const VariableDeclaration* variableDecl);

	// Creates a reference to the variable indicated by the expression.
	IR::VariableReference* GetVariableReference(const DeclarationExpression* variableRef);

	// Returns 'true' if the specified variable has been translated.
	bool HasVariable(const VariableDeclaration* variableDecl);
	
	// Adds the specified variable to the unit as a global variable.
	void AddVariable(const VariableDeclaration* variableDecl);

	// Adds the attributes associated with the variable to it's IR counterpart.
	void AddAttributes(IR::Variable* irVariable, const VariableDeclaration* variableDecl);
	void AddAttributes(IR::GlobalVariable* irVariable, const VariableDeclaration* variableDecl);

	// Adds location information to the variable (can be used by analysis like
	// the identification of variables that have no defined value before used).
	void AddVariableLocation(IR::Variable* irVariable, const VariableDeclaration* variableDecl);

	// Creates a reference to the specified function.
	IR::FunctionReference* GetFunctionReference(IR::Function* irFunct);

	// Creates a reference to the function associated with the specified declaration.
	IR::FunctionReference* GetFunctionReference(const FunctionDeclaration* variable);

	// Creates a reference to the function indicated by the expression.
	IR::FunctionReference* GetFunctionReference(const DeclarationExpression* functionRef);

	// Adds the specified value as a global constant value.
	// The name is obtained by combining the names of the parent function and variable.
	IR::VariableReference* AddConstant(IR::Variable* variable, Expression* value, 
									   IR::Function* function);

	// Adds the specified variable to the IR Unit.
	IR::VariableReference* AddConstant(IR::GlobalVariable* variable);

	// Creates a IR 'StringConstant' object from the specified string.
	// If a target array is specified the string is trimmed/extended with '\0'
	// until the size of the string and the size of the array match.
	IR::StringConstant* GetStringConstant(const StringConstant* str, 
										  const ArrayType* targetType = nullptr);

	// Returns 'true' if the specified record has a simple struct/union
	// (there are at most 'maxFields' fields with basic/pointer type and
	// at most 'maxLevels' nested record that also are simple).
	bool IsSimpleRecord(const StructUnionType* recordType, int maxFields, int maxLevels,
					    bool unionAllowed = true);

	// Returns the field that is the largest in the specified 'union'.
	// 'fieldUnit' (if provided) is set to the index of the unit where the field is placed
	const FieldDeclaration* GetLargestUnionField(const StructUnionType* unionType, 
										  int* fieldUnit = nullptr);

	// Returns 'true' if the type is a 'struct' "simple" enough so that
	// it can be expanded into a series of variables, one for each field.
	bool IsExpandableStruct(const Type* type);

	// Adds the specified symbol as a typename to the IR Unit.
	void AddTypename(IR::Symbol* typeName);
};

} // namespace IRGenerator
#endif