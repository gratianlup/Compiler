// FunctionGenerator.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_FUNCTION_GEN_HPP
#define PC_IR_GENERATOR_FUNCTION_GEN_HPP

#include "GeneratorHelpers.hpp"
#include "NameGenerator.hpp"
#include "TypeGenerator.hpp"
#include "ExpressionGenerator.hpp"
#include "StatementGenerator.hpp"
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
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/IRGenerator.hpp"
#include "../IR/MemoryIntrinsics.hpp"
#include "../Common/Context.hpp"
#include "../Common/TargetData.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/Queue.hpp"
#include "../Base/Stack.hpp"
using namespace Base;
using namespace AST;
using namespace Common;

namespace IRGenerator {

// Forward declarations.
class UnitGenerator;
struct ExpandedField;


class FunctionGenerator {
private:
	// The type of the elements used to initialize a variable.
	enum class FieldInitializerType {
		Zero,
		Constant,
		Variable
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// The element count limit for arrays/records that should be initialized with
	// a constant value without emitting a global constant and copying it's value.
	static const int AGGREGATE_CONSTANT_LIMIT = 16;

	// The maximum number of nested record types so that the above property to hold.
	static const int SIMPLE_RECORD_MAX_LEVELS = 2;

	 // Maps a local variable to it's IR representation.
	Dictionary<const VariableDeclaration*, IR::Variable*> localVars_;
	// Maps an expanded field to it's associated IR variable.
	Dictionary<ExpandedField, IR::Variable*> expandedFields_;
	// Maps a record type to a queue of variables that can be used to store temporary data.
	Dictionary<const StructUnionType*, Queue<RecordSlot>> recordSlots_;
	// A list of the used "record slot" temporary variables.
	List<RecordSlot> usedRecordSlots_;
	// Maps a labeled statement to it's IR block.
	Dictionary<const LabelStatement*, IR::Block*> labeledBlocks_;
	// Maps a 'case'/'default' statement to it's IR block.
	Dictionary<const Statement*, IR::Block*> caseBlocks_;
	// Maps a 'typedef' with VLA type to a helper with details about the generated code.
	Dictionary<const Type*, VLATypedefInfo> vlaTypedefs_;
	// Maps a variable with VLA type to a helper with details about it's associated IR variable.
	Dictionary<const VariableDeclaration*, VLAInfo> vlaVariables_;
	// Maps a VLA type to the operand that has the result of it's generated size expression.
	Dictionary<const VarArrayType*, IR::Operand*> vlaSizeExprOps_;
	// Maps a VLA type to the operand that has the size of the element type.
	Dictionary<const ArrayType*, IR::Operand*> vlaElemSizeOps_;
	// A stack with information about the loops, as they appear in the source code.
	// Used for 'do', 'while', 'for' and 'switch'.
	Stack<LoopInfo> loops_;
	// A stack of helpers with the VLA context for each statement with VLA variables.
	Stack<VLAContext> vlaContexts_;
	// A stack with the statements that should be considered the parents of the current one.
	Stack<const Statement*> parentStatements_;
	IR::Unit* irUnit_;          // The resulting IR unit.
	Unit* unit_;                // The Unit for which we generate code.
	Context* context_;          // The context of the compilation.
	const TargetData* target_;  // The target of the compilation.
	TypeGenerator* typeGen_;    // Helper that creates IR types.
	LayoutCache* layouts_;      // Caches the layout of a record.
	UnitGenerator* unitGen_;    // The Unit Generator (the parent).
	const FunctionDeclaration* funct_; // The function for which we generate code.
	IR::Function* irFunct_;     // The IR function in which the code is generated.
	IR::Block* activeBlock_;    // The block that is currently active.
	IR::IRGenerator irGen_;     // Helper used to generate IR objects.
    NameGenerator nameGen_;     // The generator for unique IR names.
	bool namedTemp_;            // 'true' if the temporaries should be named. Only in debug mode.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the corresponding IR type of the specified type.
	const IR::Type* GetIRType(const Type* type);

	// Returns a constant operand with the value obtained by evaluating
	// the specified expression.
	IR::Operand* GetConstant(const IR::Type* type, const Expression* value, 
							 IR::Block* block);

	// Returns a constant operand with the value 0.
	// For pointer types it returns a null-pointer constant.
	IR::Operand* GetZeroConstant(const IR::Type* type);

	// Returns the number of values in the specified initialization list,
	// including the ones in the nested lists.
	int FunctionGenerator::GetInitListValueCount(InitializerListExpression* initList);

	// Generates code that copies the data of a global variable 
    // to a variable on the stack. Uses the 'copyMemory' intrinsic.
	void CopyGlobalToStack(IR::Operand* destOp, IR::Operand* sourceOp,
						   const Type* destType, IR::Block* block);

	// Generates code that initialized a variable with the specified initializer.
	// If 'withZero' is set, the variable is initialized with the value 0
	// (or a null-pointer constant if it has pointer type).
	// Large arrays and records are emitted as global variables and referenced.
	void InitializeWithConstant(IR::Variable* irVariable, 
                                InitializerListExpression* initializer,
								const Type* arrayType, bool withZero, IR::Block* block);

	// Initializes the specified array/record with 0. Uses the 'setMemory' intrinsic.
	void InitLargeAggregateWithZero(IR::VariableReference* variableRef, 
                                    InitializerListExpression* initializer,
					                const Type* destType, bool withZero, IR::Block* block);

	// Initializes the specified array/record using a large, constant initializer.
	// Creates a global constant and generates code that copies it on the stack
	// by using the 'copyMemory' intrinsic.
	void InitLargeAggregateWithConst(IR::Variable* irVariable, 
                                     InitializerListExpression* initializer,
								     const Type* arrayType, IR::Block* block);

	// Initializes the specified array/record using an initializer
	// that doesn't have only constants as it's elements.
	void InitializeAggregate(IR::Operand* destOp, const Type* destType, 
                             InitializerListExpression* initList, 
                             FieldInitializerType initType, IR::Block* block, 
                             ExpressionGenerator* exprGen);

	// Generates a sequence of instructions that select the element at 'index'
	// from the specified array/record. Returns the operand with the generated address.
	IR::Operand*
	GenerateElementAddress(__int64 index, IR::Operand* baseAddr,
						   const IR::Type* irElemPtrType, IR::Block* block, 
                           bool onRecord);

	// Initializes an element of an array/record with the expression
	// at position 'index' from the specified initializer list.
	// Also handles nested array/record types.
	void InitializeElement(IR::Operand* elemAddr, __int64 index, 
                           const Type* elementType, const IR::Type* irElemType,
						   InitializerListExpression* initList, 
                           FieldInitializerType initType, IR::Block* block,
						   ExpressionGenerator* exprGen);

	// Initializes an array with the specified string constant.
	// If the string is small no global is created, each element is set individually.
	void InitArrayWithString(IR::Operand* destOp, const ArrayType* arrayType,
							 const StringConstant* stringConst, IR::Block* block);

	// Creates a global variable with the specified string, then generates a sequence
	// of instructions that convert it's reference to a pointer.
	IR::Operand* GeneratePointerToString(const Expression* stringConst,
									 const IR::Type* destType, IR::Block* block);

	// Returns 'true' if the array is "simple" (contains few elements,
	// and if the element type is a record it should also be simple).
	bool IsSimpleArray(const ArrayType* arrayType, InitializerListExpression* initList);

	// Adds the parameters of the function to the map of local variables.
	// If a parameter with 'struct' type has been expanded, 
    // it's added to 'expandedParams'.
	void GenerateParameters(List<ExpandedField>& expandedFields,
							List<VariableDeclaration*>& expandedParams);

	// Reconstructs an expanded 'struct' by copying the values from the associated
	// parameters to the corresponding fields.
	void InitializeExpandedParameters(List<VariableDeclaration*>& expandedParams);

	// Creates a variable that can be used to store temporary record values.
	// Used when calling functions with a record parameter, and for functions
	// that return a record value.
	RecordSlot CreateRecordSlot(const StructUnionType* recordType);

	// Returns 'true' if 'expr' is a compound expression, or such an expression
	// is found in the subtree, at any level.
	bool HasCompoundExpression(const Expression* expr);

	// Inserts a return instruction in the last block of the function,
	// in case that no branching instruction is found. '0' is used 
    // as the default value, and 'void' for functions that have 'void' return type.
	void InsertLastReturn();

	// Creates a context that describes the VLA variables in the current statement.
	// This ensures that only one context is created for each statement.
	VLAContext* GetOrCreateVLAContext();

	// Verifies if a VLA context was created for the current statement,
	// and if true it copies a pointer to it in 'context'.
	// Returns 'false' if a VLA context was not found.
	bool FunctionGenerator::GetVLAContext(VLAContext** context);

	// Generates a call to the 'incStackTop' intrinsic, which increments
	// the address of the top of the stack.
	void UpdateNextVLAAddress(IR::Operand* sizeOp);

	// Restores the address of the top of the stack to the value from the moment
	// the block associated with the current statement was entered.
	// Uses the statement's associated VLA context.
	void RestoreStackState();

    // Generates the code that restores the stack pointer.
    void GenerateStackRestoreCode(VLAContext& vlaContext);

	void GenerateVLAPointer(const VariableDeclaration* variableDecl, int ptrCount,
							const ArrayType* vlaType, const IR::Type* irElemPtrType);

	// Generates code that computes the size of the specified VLA type.
	// If 'addToMap' is 'true' it adds each intermediate computation (one for each
	// nested VLA type) to a map (used by 'sizeof', for example.
	// If 'searchMap' is 'true' it tries to use a computation that was already performed.
	IR::Operand* GenerateVLASizeImpl(const Type* type, bool addToMap, bool searchMap);

	// Creates an object that describes the context when an event is generated.
	GeneratorContext GetGeneratorContext(const Expression* expr = nullptr);

public:
	FunctionGenerator(UnitGenerator* unitGen);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Generates code for all the statements and expressions in the specified function.
	void Generate(const FunctionDeclaration* function, IR::Function* irFunct,
				  List<ExpandedField>& expandedFields);

	// Methods for accessing the helper objects.
	IR::Unit*           GetIRUnit()     { return irUnit_;  }
	Unit*               GetUnit()       { return unit_;    }
	UnitGenerator*      GetUnitGen()    { return unitGen_; }
	IR::Function*       GetIRFunction() { return irFunct_; }
	TypeGenerator*      GetTypeGen()    { return typeGen_; }
	LayoutCache*        GetLayouts()    { return layouts_; }
	IR::IRGenerator*    GetIRGen()      { return &irGen_;  }
	const TargetData*   GetTarget()     { return target_;  }
	Context*            GetContext()    { return context_; }
    NameGenerator*      GetNameGen()    { return &nameGen_; }
    const FunctionDeclaration* GetFunction()   { return funct_; }

	// Returns the block in which the generated code is currently added.
	IR::Block* ActiveBlock() {
		return activeBlock_;
	}

	// Creates a new block having the specified name.
	// If the name is not already unique it's made unique by appending a number.
	// Note that the block is not inserted into the list of basic blocks.
	IR::Block* CreateBlock(const string& name, bool inLoop = false);

    IR::Block* CreateBlock(BlockName name, bool inLoop = false);

	// Inserts the specified block into the lists of basic blocks.
	// This also makes the block the active one.
	void InsertAndMakeActive(IR::Block* block);

	// Adds the specified variable to the function and generates code
	// for the initializer (if any) in the specified block.
	void AddVariable(const VariableDeclaration* variable, IR::Block* block);

	// Initializes the specified variable, either by a constant values or
	// by values computed at runtime. The generated code is added to the given 'block'.
	void InitializeVariable(IR::Variable* irVariable, Expression* initializer,
							const Type* destType, IR::Block* block);

	// Creates and adds to the IR unit a string constant.
	// Automatically assigns a name that is unique in the IR unit.
	IR::VariableReference* AddStringConstant(const StringConstant* stringConst,
											 const ArrayType* targetType = nullptr);

	// Adds a variable to the function that has the specified type and name.
	// The name is made unique if it's isn't already.
	IR::VariableReference* AddVariable(const IR::Type* Type, const string& name);

	// Returns a reference that points to the specified local variable.
	IR::VariableReference* GetVariableReference(const VariableDeclaration* variable);

	// Returns a reference that points to the variable pointed by the expression.
	IR::VariableReference* GetVariableReference(const DeclarationExpression* expr);

	// Returns a reference that points to the specified local variable.
	IR::VariableReference* GetVariableReference(IR::Variable* irVariable);

	// Returns a reference to the variable associated with the specified
	// expanded field, or 'nullptr' if the field was not expanded.
	IR::VariableReference* GetFieldReference(const FieldDeclaration* field,
											 const VariableDeclaration* variable);

	// Returns a reference to a variable that can be used to store temporary records
	// (the return of a function, for example). A new slot is created only if no unused
	// slots for the specified type is found in the associated "unused slot queue".
	IR::VariableReference* GetRecordSlot(const StructUnionType* recordType);

	// Adds all used slots to their corresponding "unused slot queues",
	// so they can be used later.
	void ReleaseRecordSlots();

	// Adds a block that is associated with a labeled statement.
	// The block gets the name of the label, and it's added to a map (used by 'goto').
	void AddLabeledBlock(const LabelStatement* statement, IR::Block* block);

	// Returns the block associated with the specified label, or 'nullptr' if no
	// such block is found (used by 'goto').
	IR::Block* GetLabeledBlock(const LabelStatement* statement);

	// Returns the 'LoopInfo' object that it's at the top of the "loop stack".
	// 'LoopDepth' should be used before calling this method.
	LoopInfo& CurrentLoop() {
		return loops_.Peek();
	}

    // Returns the level at which the current loop is found.
    int LoopDepth() {
        return loops_.Count();
    }

	// Pushes the specified 'LoopInfo' object on the "loop stack".
	void PushLoopInfo(const LoopInfo& info) {
		loops_.Push(info);
	}

	// Pops the loop from the top of the "loop stack".
	void PopLoopInfo() {
		loops_.Pop();
	}

	// Adds a block associated with the specified statement to the "case block map".
	void AddCaseBlock(const Statement* statement, IR::Block* block) {
		caseBlocks_.Add(statement, block);
	}

	// Returns the block associated with the specified statement.
	// Note that this should always return a valid block.
	IR::Block* GetCaseBlock(const Statement* statement) {
		DebugValidator::IsTrue(caseBlocks_.ContainsKey(statement));
		return caseBlocks_[statement];
	}

	// Adds the specified information object that holds the operand 
    // that stores the computed size of the VLA from the 'typedef'
    // (for example, 'typedef int A[E];').
	void AddVLATypedef(const VLATypedefInfo& info) {
		vlaTypedefs_.Add(info.VLAType, info);
	}

	// Returns 'true' if the specified VLA type originates from a 'typedef'.
	// Note that this also means that the size has been already computed.
	bool HasVLATypedef(const Type* type) {
		return vlaTypedefs_.ContainsKey(type);
	}

	// Returns the information object associated with the specified VLA type.
	// 'HasVLATypedef' shall be used before calling this method.
	VLATypedefInfo GetVLATypedef(const Type* type) {
		return vlaTypedefs_[type];
	}

	// Generates code that computes the size of the VLA 
    // associated with the specified 'typedef'.
	void GenerateVLATypedef(const TypedefDeclaration* typedefDecl);

	// Adds the specified VLA variable to the function. Generates code 
    // that computes the size of the VLA, including nested VLA, if it's the case.
	void AddVLAVariable(const VariableDeclaration* variableDecl);

	// Returns an information object contains the size operand and the variable
	// that acts as the base address for the specified VLA variable.
	VLAInfo GetVLAInfo(const VariableDeclaration* variable) {
		DebugValidator::IsTrue(variable->DeclarationType()->WithoutQualifiers()->IsVariable());
		return vlaVariables_[variable];
	}

	// Adds the operand that contains the computed size 
    // of the specified VLA type to a map.
	void AddVLASizeExprOperand(const VarArrayType* vlaType, IR::Operand* sizeOp) {
		vlaSizeExprOps_.Add(vlaType, sizeOp);
	}

	// Returns 'true' if the the size of the specified 
    // VLA type has been already computed.
	bool HasVLASizeExprOperand(const VarArrayType* vlaType) {
		return vlaSizeExprOps_.ContainsKey(vlaType);
	}

	// Returns the operand that contains the computed size for the specified VLA type.
	// 'HasVLASizeExprOperand' should be used before calling this method.
	IR::Operand* GetVLASizeExprOperand(const VarArrayType* vlaType) {
		return vlaSizeExprOps_[vlaType];
	}

	// Adds the operand that contains the size for the element of the specified VLA.
	void AddVLAElemSizeOperand(const ArrayType* vlaType, IR::Operand* sizeOp) {
		vlaElemSizeOps_.Add(vlaType, sizeOp);
	}

	// Returns 'true' if the the element size for the specified
    // VLA has been already computed.
	bool HasVLAElemSizeOperand(const ArrayType* vlaType) {
		return vlaElemSizeOps_.ContainsKey(vlaType);
	}

	// Returns the operand that contains the size for the element of the specified VLA.
	// 'HasVLAElemSizeOperand' should be used before calling this method.
	IR::Operand* GetVLAElemSizeOperand(const ArrayType* vlaType) {
		return vlaElemSizeOps_[vlaType];
	}

	// Pushes the specified statement on the "parent stack".
    // For example, in '{S1, {S2, S3}', the external compound statement 
    // is the parent of all it's children.
	void PushParentStatement(const Statement* statement) {
		parentStatements_.Push(statement);
	}

	// Removes the current parent statement from the "parent stack".
	void PopParentStatement();

	// Returns the current parent statement.
	const Statement* ParentStatement(int position = 0) {
		return parentStatements_.Peek(position);
	}

	// Pushes a VLA context on the stack. Used to store the top
    // of the stack when the associated parent statement was entered,
    // so it can be restored when the is exited.
	void PushVLAContext(const VLAContext& context) {
		vlaContexts_.Push(context);
	}

	// Returns 'true' if at least a VLA context is active.
	bool HasVLAContext() {
		return vlaContexts_.Count() > 0;
	}

	// Returns the latest VLA context.
	VLAContext& TopVLAContext() {
		return vlaContexts_.Peek();
	}

	// Pops the top object from the VLA context stack.
	void PopVLAContext() {
		vlaContexts_.Pop();
	}

    // Restores the stack state if the current statement contains VLAs.
    void RestoreVLAStack(bool popAllArrays = false, bool popLoopArrays = false);

	// Returns the innermost type of a VLA type. For 'int a[E1][E2]' it's 'int'.
	const Type* GetArrayInnerType(const ArrayType* vlaType);

	// See 'GenerateVLASizeImpl'.
	IR::Operand* GenerateVLASize(const ArrayType* vlaType, bool addToMap = true,
								 bool searchMap = false);

	// Methods to inform the observers about events in the code generator.
	void BeforeLoad(IR::Operand* op, const Expression* expr,
                    const Type* sourceType = nullptr);
	void AfterLoad(IR::LoadInstr* instr, const Expression* expr,
                   const Type* sourceType = nullptr);

	void BeforeStore(IR::Operand* op, const Expression* expr,
                     const Type* destType = nullptr, const Type* sourceType = nullptr);
	void AfterStore(IR::StoreInstr* instr, const Expression* expr,
                    const Type* destType = nullptr, const Type* sourceType = nullptr);
	
	void BeforeCall(IR::Operand* op, const Expression* expr = nullptr);
	void AfterCall(IR::CallInstr* instr, const Expression* expr = nullptr);
	
	void BeforeIf(IR::Operand* op);
	void AfterIf(IR::IfInstr* instr, IR::Block* trueBlock, IR::Block* falseBlock);
	
	void BeforeLoop(LoopKind loopKind);
	void AfterLoop(LoopKind loopKind, IR::Block* headerBlock,
				   IR::Block* bodyBlock, IR::Block* incrementBlock);
};

} // namespace IRGenerator
#endif
