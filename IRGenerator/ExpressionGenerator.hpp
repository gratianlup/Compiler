// ExpressionGenerator.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_EXPRESSION_GEN_HPP
#define PC_IR_GENERATOR_EXPRESSION_GEN_HPP

#include "NameGenerator.hpp"
#include "TypeGenerator.hpp"
#include "../AST/Types.hpp"
#include "../AST/Unit.hpp"
#include "../AST/Attributes.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/TypeSize.hpp"
#include "../AST/StructLayout.hpp"
#include "../IR/IRTypes.hpp"
#include "../IR/Symbols.hpp"
#include "../IR/Unit.hpp"
#include "../IR/TypeTable.hpp"
#include "../IR/Intrinsic.hpp"
#include "../IR/Constants.hpp"
#include "../IR/References.hpp"
#include "../IR/IRGenerator.hpp"
#include "../Common/Context.hpp"
#include "../Common/TargetData.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
using namespace Base;
using namespace AST;
using namespace Common;

namespace IRGenerator {

// Forward declarations.
class FunctionGenerator;


class ExpressionGenerator : public Visitor {
private:
	// The maximum numbers of fields with basic type so that a record
	// a record still considered a "simple" record.
	static const int SIMPLE_RECORD_MAX_FIELDS = 4;

	// The maximum number of nested records a record can have
	// so that it's still considered a "simple" record.
	static const int SIMPLE_RECORD_MAX_LEVELS = 2;

	// Used to temporarily store the result of an expression.
	IR::Operand* result_;            
    const Expression* currentExpr_;
	// Set if the target of a call that returns a record is known. 
	// In this case the returned values can be copied directly into the variable.
	IR::Operand* recordReturnTarget_;
	FunctionGenerator* functGen_; // The associated function generator.
	TypeGenerator* typeGen_; // Helper that creates IR types.
	IR::IRGenerator* irGen_; // Helper used to generate IR objects.
	IR::Block* activeBlock_; // The block that is currently active.
	bool namedTemp_;         // 'true' if the temporaries should be named. Only in debug mode.
	bool address_;           // 'true' if the address of an array element is being computed.
	bool membAddress_;       // 'true' if the address of an record element is being computed.
	bool lvalue_;            // 'true' if the expression is in a 'lvalue' position.
	bool vla_;               // 'true' if the expression involves a VLA.
	bool resetVLAAllowed_;   // If 'true' then 'vla_' can't be set to 'false' by 'ResetFlags'.
    bool markFunctionAddressTaken_; // If 'true' referred function are marked address-taken.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void Visit(const Expression* expr);

	// Returns a binary mask that can be used to extract the value of the specified
	// bitfield. Example: for a bitfield of size 3, at offset 2, it returns 11100.
	__int64 GetBitfieldMask(FieldInfo& fieldInfo);

	// Returns a binary mask that can be used to isolate the data in the unit that
	// is not part of the specified bitfield. 
    // Used the eliminate the old value of the bitfield.
	__int64 GetReverseBitfieldMask(FieldInfo& fieldInfo);

	// Returns the corresponding IR type of the specified type.
	const IR::Type* GetIRType(const Type* type);

	// Returns the IR representation for the specified type.
	const BasicType* GetBasicType(const Type* type);

	// Sets/resets a flag that indicates that only the address
    // of an array/record is desired.
	void SetAddress() { address_ = true; }
	void ResetAddress() { address_ = false; }
	bool ShouldLoadAddress() { return address_ == false; }

	// Sets/resets a flag that indicates that only the address of a member is desired.
	void SetMemberAddress() { membAddress_ = true; }
	void ResetMemberAddress() { membAddress_ = false; }
	bool ShouldLoadMemberAddress() { return membAddress_ == false; }

	// Sets/resets a flag that indicates that the expression should be generated
	// as an lvalue; prevents loading the value on the left side of an assignment.
	void SetLvalue()   { lvalue_ = true;  }
	void ResetLvalue() { lvalue_ = false; }
	bool ShouldLoad()  { return lvalue_ == false; }

	void SetVLA()   { vla_ = true;  }
	void ResetVLA() { vla_ = false; }
	bool HasVLA()   { return vla_; }

	void SetResetVLAAllowed(bool value) {
		resetVLAAllowed_ = value;
	}

    void MarkFunctionAddressTaken() {
        markFunctionAddressTaken_ = true;
    }

    void DontMarkFunctionAddressTaken() {
        markFunctionAddressTaken_ = false;
    }

    bool ShouldMarkFunctionAddressTaken() const {
        return markFunctionAddressTaken_;
    }

	// Resets all the "load" flags.
	void ResetFlags(bool ignoreVLA = false) {
		ResetLvalue();
		ResetAddress();
		ResetMemberAddress();

		// Reset the VLA flag if we're requested and allowed.
		if(resetVLAAllowed_ && (ignoreVLA == false)) {
			ResetVLA();
		}
	}

    // Generates the code for the specified expression
    // at the end of the active block.
    IR::Operand* GenerateExpression(Expression* expr, bool resetFlags = true,
                                    bool ignoreVla = false) {
        Visit(expr);
        
        if(resetFlags) {
            ResetFlags(ignoreVla);
        }

        return result_;
    }

	// Returns 'true' if the specified type is marked as volatile.
	// If 'testPointer' is 'true' it also looks through a pointer ('volatile char*').
	bool IsVolatile(const Type* type, bool testPointer);

	// Sets the 'volatile' attribute on the specified 'store'/'load' instruction,
	// in case the type is marked as 'volatile'.
	void SetVolatile(IR::Instruction* instr, const Type* type, bool testPointer);

	// Reloads the value at the specified address.
	// Used when storing to a 'volatile' type.
	void ReloadVolatileValue(IR::Operand* valueAddrOp, const Type* valueType);

	// Returns 'true' if the specified expression is a compile-time constant.
	// 'eval' contains the result of the evaluated expression.
	bool IsConstant(const Expression* expr, EvaluationInfo& eval);

	// Verifies if the specified expression is a compile-time constant, and if true
	// it generates a constant IR operand, instead of instructions that compute the value.
	// Performs a simple form of constant folding ('a + 2 + 4' becomes 'a + 6').
	bool GenerateConstant(const Expression* expr);
    
    // Generates the code for an expression that references 
    // a local/global variable declaration.
    void GenerateVariableDeclarationExpression(const DeclarationExpression* expr,
                                               const VariableDeclaration* variable);

	// ##################################################################################
	// Binary operators
	// ##################################################################################
	// If enabled in the options, sets a flag that indicates to the backend that
	// signed integer overflow is undefined, allowing more aggressive optimizations.
	IR::Instruction* SetOverflowFlag(IR::Instruction* instr);

	// Sets the mode in which the specified floating-point instructions is performed.
	// Can allow more aggressive optimizations.
	IR::Instruction* SetFloatingMode(IR::Instruction* instr);

	// Generates code for binary operators (+, -, *, /, etc.).
	// This is mostly a method that dispatches to specialized methods.
	void GenerateBinaryOp(const BinaryOperator* op, IR::Operand* leftOp,
						  IR::Operand* rightOp, const Type* resultType);

	// Generates code for + and - operators for integer and floating types.
	void GenerateAdditiveOp(const BinaryOperator* op, IR::Operand* leftOp,
							IR::Operand* rightOp, const Type* resultType);

	// Generates code for + and - operators involving pointers.
	// Forwards to specialized methods to handle the 3 possible cases
	// ('pointer - pointer', 'pointer - integer', and 'pointer + integer').
	void GeneratePointerAdditiveOp(const BinaryOperator* op, IR::Operand* lefxtOp,
								   IR::Operand* rightOp, const Type* resultType);

	// Verifies if the expression is an unnecessary cast from integer to pointer.
	// If 'true' 'targetType' is set to the type of the cast target.
	bool IsImplicitIntToPtrCast(const Expression* expr, 
								const Type** targetType = nullptr);

	// Generates code for the 'pointer + integer' case; uses the 'addr' instruction.
	void GeneratePointerIntAdd(const BinaryOperator* op, IR::Operand* leftOp,
								    IR::Operand* rightOp, const Type* pointerType,
									const Type* intType, const Type* resultType);

	// Generates code for the 'pointer - integer' case; uses the 'addr' instruction.
	void GeneratePointerIntSub(const BinaryOperator* op, IR::Operand* leftOp,
							   IR::Operand* rightOp, const Type* pointerType,
							   const Type* intType, const Type* resultType);

	// Generates code for the 'pointer - pointer' case;.
	void GeneratePointerPtrSub(const BinaryOperator* op, IR::Operand* leftOp,
							   IR::Operand* rightOp, const Type* ptrType1,
							   const Type* ptrType2, const Type* resultType);

	// Generates code for *, / and % operators, for both integer and floating types.
	// Unsigned integers use special instructions ('udiv', 'umod').
	void GenerateMultiplicativeOp(const BinaryOperator* op, IR::Operand* leftOp,
								  IR::Operand* rightOp, const Type* resultType);

	// Generates code for &, |, ^, << and >> operators.
	// Signed numbers use a the 'shr' instruction (arithmetic right shift), 
	// while for // unsigned numbers use the 'ushr' instruction (logical right shift).
	void GenerateBitwiseOp(const BinaryOperator* op, IR::Operand* leftOp,
						   IR::Operand* rightOp, const Type* resultType);

	// Generates code for ==, !=, <, <=, > and >= operators, for integer, floating
	// and pointer types. Comparison involving pointers use the 'ucmp' instruction,
	// and comparison of a pointer to 0 uses the 'nullptr' IR constant.
	void GenerateComparisonOp(const BinaryOperator* op, IR::Operand* leftOp,
							  IR::Operand* rightOp, const Type* resultType);

	// Generates code that stores the specified value to the destination operand.
	// If the destination is a record 'GenerateRecordAssignment' is used.
	// Else if the destination is a field, 'StoreToBitfield' is used.
	// If the destination has 'volatile' type the 'store' instruction is appropriately marked.
	void GenerateStore(IR::Operand* destOp, IR::Operand* sourceOp,
					   const Type* destType, const Type* sourceType,
					   FieldDeclaration* bitfield = nullptr);

	// Generates code for an assignment expression. Handles both simple values and records.
	// Marks the 'store' as 'volatile' if the type of the destination is 'volatile'.
	void GenerateAssignment(const BinaryOperator* op);

	// Generates code for a compound assignment (+=, <<=, etc.). Also handles bitfields 
	// as the lvalue. Uses the appropriate binary operator method to generate the operation.
	void GenerateCompoundAssignment(const BinaryOperator* op);

	// Tries to eliminate a cast from a logical or comparison expression 
	// that has one of it's operands a constant number.
	bool SimplifyBinaryOpCastNumber(shared<Expression>& numbValue, 
									shared<Expression>& otherValue,
									const Type** newNumbType);

	// Generates a cast of the right operand to the type of the left one,
    // but only in case it's needed.
	const IR::Type* GenerateCastToLeft(IR::Operand*& leftOp, const Type* oldLeftType,
									   IR::Operand*& rightOp, const Type* oldRightType);

    // Changes the type of the specified operand. If the operand is a constant,
    // a new constant having the appropriate type is returned.
    IR::Operand* PatchOperandType(IR::Operand* op, const IR::Type* newType);

	// ##################################################################################
	// Unary operators
	// ##################################################################################
	// Generates code for the -E operator. Handled integer and floating types.
	void GenerateNegation(const UnaryOperator* op);

	// Generates code for the ~ operator.
	void GenerateComplement(const UnaryOperator* op);

	// Generates code for the ! operator. Handles integer, floating and pointer type.
	// For pointer types a comparison against the 'nullptr' constant is generated.
	void GenerateNot(const UnaryOperator* op);

	// Generates code for the ++ and -- operands (pre and postfix variants).
	// Handles integer and floating types, and pointers by using 'GeneratePointerIncDec'.
	void GenerateIncDec(const UnaryOperator* op);

	// Generates code for incrementing/decrementing a pointer. Uses the 'addr' instruction.
	void GeneratePointerIncDec(const UnaryOperator* op, IR::Operand* targetOp);

	// Generates code for the & expression, that takes the address of the target.
	void GenerateAddress(const UnaryOperator* op);

	// Generates code for the * expression, that loads the value of the target.
	// If the expression is in an lvalue position the value is not loaded, only
	// the it's address is generated. This is also the case for records.
	void GenerateIndirection(const UnaryOperator* op);

	// Generates code that loads the base of an array subscript expression.
	// Also works with VLAs, in which case 'baseVLAType' is set to the VLA type of the base.
	// 'castToPointer' is set to 'true' if the base is a 'array-to-pointer' cast.
	IR::Operand* GenerateSubscriptBase(const SubscriptExpression* expr, bool& castToPointer,
									   const VarArrayType*& baseVLAType);

	// ##################################################################################
	// Cast expressions
	// ##################################################################################
	// Generates code for an 'integer-to-float' cast. 
	// Uses the 'itof' instruction if the number is signed, and 'uitof' otherwise.
	void GenerateIntToFloat(const CastExpression* expr);

	// Generates code for an 'float-to-integer' cast.
	// Uses the 'ftoi' instruction if the integer is signed, and 'ftoui' otherwise.
	void GenerateFloatToInt(const CastExpression* expr);

	// Generates code for conversions between integers. If the rank is the same
	// nothing is done (even if one is signed and the other not).
	void GenerateIntToInt(const CastExpression* expr);

	// Generates code for a conversion between 'float' and 'double'.
	// 'ftrunc' is used for 'double -> float' and 'fext' for 'float -> double'.
	void GenerateFloatToFloat(const CastExpression* expr);

	// Generates code that converts an expression to a pointer. 
	// Handles the 'array to pointer', 'integer to pointer' and 'pointer to pointer' cases.
	void GenerateToPointer(const CastExpression* expr);

	// Generates code that converts a pointer to an integer. Conversion to 'bool'
	// is special, because we can't use the 'ptoi' instruction; instead a sequence
	// of instruction that compute something like 'bool b = p != 0' must be generated.
	void GeneratePointerToInt(const CastExpression* expr);

	// Tries to eliminate some unnecessary casts that are introduced by the C standard.
	// Returns 'false' if no such optimization could be applied.
	bool GenerateCastOptimized(const CastExpression* expr);

	// ##################################################################################
	// Records (and bitfields)
	// ##################################################################################
	// Generates code that loads a bitfield found at the specified address.
	void LoadFromBitfield(IR::Operand* addressOp, FieldDeclaration* field, FieldInfo& info);

	// Generates optimized code that loads a bitfield found at the specified address.
	// Can be used when the bitfield is a multiple of 8, aligned at a byte boundary
	// (in 'struct T { int a:4, b:4, c:8, d:16; };' - 'c' and 'd' can be optimized.
	bool LoadFromBitfieldOptimized(IR::Operand* addressOp, FieldInfo& info);

	// Generates optimized code that stores a value into a bitfield.
	// Can be used when the bitfield is a multiple of 8, aligned at a byte boundary.
	// See 'LoadFromBitfieldOptimized' for example.
	IR::Instruction*
	StoreToBitfieldOptimized(IR::Operand* addressOp, IR::Operand* valueOp, 
							 FieldDeclaration* field, FieldInfo& fieldInfo);

	// Generates code that copies the record at address 'sourceOp' to 'destOp'.
	// This copies each field individually and can be used only with "simple records".
	void GenerateRecordAssignmentOptimized(IR::Operand* destOp, IR::Operand* sourceOp, 
										   const StructUnionType* destType, 
										   bool fromExpanded = false,
										   const VariableDeclaration* recordVar = nullptr);

	// Generates code that copies a field from the record at address 'sourceOp'
	// to the one at 'destOp'. This also handles nested records.
	void GenerateFieldCopy(IR::Operand* destOp, IR::Operand* sourceOp, int fieldUnit,
						   const FieldDeclaration* field, bool fromExpanded = false,
						   const VariableDeclaration* recordVar = nullptr);

    // Adds a 'NameTag' to the specified instruction that
    // indicates the name of the selected field (used for debugging).
    void AppendFieldName(IR::FieldInstr* fieldInstr, FieldInfo& fieldInfo);

	// ##################################################################################
	// Function Call
	// ##################################################################################
	// Verifies if the type of the function type matches the one found in the IR unit,
	// and if not introduces a cast to the function type that should be used instead.
	IR::Operand* PatchCallTarget(IR::Operand* functOp, const CallExpression* callExpr);

	// Generates code for each of the arguments of the specified call expression.
	// If the called function returns a record, 'recordRetAddr' is passed as an argument,
	// where the callee needs to copy the returned record.
	void GenerateCallArguments(const CallExpression* callExpr, IR::CallInstr* callInstr,
							   IR::Operand* recordRetAddr);

	// Generates code for an argument that has record type.
	// Note that "simple records" are handled by 'GenerateExpandedStructArg'.
	void GenerateRecordArgument(IR::CallInstr* callInstr, IR::Operand* addressOp, 
								Expression* argument);

	// Generates code for an argument that is a "simple record", whose fields have been
	// expanded into individual argument; each such field in passed as a distinct argument.
	void GenerateExpandedStructArg(IR::CallInstr* callInstr, IR::Operand* addressOp,
								   const StructType* structType);

	// Generates code for the value returned by the callee. For 'void' nothing is generated,
	// while for records a slot (temporary variable) is used (and 'hasRecordReturn' is set).
	IR::Operand* GenerateReturnTarget(const Type* returnType, bool& hasRecordReturn);

	// Sets the calling convention for the specified call expression,
	// in case an 'CallConventionAttribute' was used to override the default call convention.
	void SetCallConvention(IR::CallInstr* callInstr, const CallExpression* callExpr);

    // Sets a flag that indicates if a math function from the standard library
    // is allowed to be promoted to an intrinsic and to be optimized.
    void SetCallPromotionPolicy(IR::CallInstr* callInstr, const CallExpression* callExpr);

	// ##################################################################################
	// Logical operators
	// ##################################################################################
	// Generates code for a && and || expression. Creates a temporary variable in which
	// the result (0 or 1) is placed, and blocks like for an 'if-else' statement.
	void GenerateLogical(const BinaryOperator* op);

	// Tries to evaluate the logical expression at compile time and use the result.
	bool GenerateLogicalOptimized(const BinaryOperator* op);

	// ##################################################################################
	// VLA (variable-length arrays)
	// ##################################################################################
	// Generates code that computes the size of a VLA type.
	void GenerateVLASizeof(const SizeofOperator* op);

	// Generates code that computes the offset for the specified index in a VLA array.
	IR::Operand* GenerateVLAOffset(const ArrayType* vlaType, 
                                   IR::Operand* indexOp = nullptr,
								   const Type* indexType = nullptr);

	// Generates code that negates the specified operand.
	IR::Operand* GenerateNegatedOperand(IR::Operand* op);

	// Generates code that increments/decrements a VLA pointer.
	void GenerateVLAPointerIncDec(const UnaryOperator* op, IR::Operand* targetOp,
								  const ArrayType* vlaType);

	// Generates code that changes a VLA pointer by the specified offset.
	void GenerateVLAPointerIntCommon(const BinaryOperator* op, IR::Operand* leftOp,
								     IR::Operand* rightOp, const Type* intType,
								     const ArrayType* vlaType, IR::Operand*& baseOp,
									 IR::Operand*& offsetOp);

	// Generates code that increments a VLA pointer by a specified integer.
	void GenerateVLAPointerIntAdd(const BinaryOperator* op, IR::Operand* leftOp,
								  IR::Operand* rightOp, const Type* pointerType,
								  const Type* intType, const Type* resultType,
								  const ArrayType* vlaType);

	// Generates code that decrements a VLA pointer by a specified integer.
	void GenerateVLAPointerIntSub(const BinaryOperator* op, IR::Operand* leftOp,
								  IR::Operand* rightOp, const Type* pointerType,
								  const Type* intType, const Type* resultType,
								  const ArrayType* vlaType);

	// Generates the specified expression in the block, stores the result
	// in the temporary variable, then generates a 'goto' to the continuation block.
	// Used when generating the conditional ?: operator.
	void GenerateConditionalBlock(IR::Block* block, const Expression* expr, 
								  IR::Block* contBlock, IR::VariableReference* tempVariableRef);

    // Tries to generate a simplified form of the conditional operator ?:.
    bool GenerateConditionalOptimized(const ConditionalOperator *expr);

	// Methods that implement the Visitor pattern.
	virtual void Visit(const UnaryOperator		   *expr) override;
	virtual void Visit(const BinaryOperator		   *expr) override;
	virtual void Visit(const NumberConstant	       *expr) override;
	virtual void Visit(const CharConstant		   *expr) override;
	virtual void Visit(const StringConstant	       *expr) override;
	virtual void Visit(const SizeofOperator		   *expr) override;
	virtual void Visit(const SubscriptExpression   *expr) override;
	virtual void Visit(const MemberExpression	   *expr) override;
	virtual void Visit(const CallExpression		   *expr) override;
	virtual void Visit(const ConditionalOperator   *expr) override;
	virtual void Visit(const CastExpression		   *expr) override;
	virtual void Visit(const DeclarationExpression *expr) override;
	virtual void Visit(const CompoundExpression	   *expr) override;

public:
	ExpressionGenerator(FunctionGenerator* functGen);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Generates code for the specified expression, and if it's the case,
	// returns the operand that contains the result of the expression.
	IR::Operand* Generate(const Expression* expr);

	// Generate code to initialize the variable with the specified expression.
	void StoreInitializer(IR::Variable* irVariable, const Expression* initializer);

	// Generate code that copies the result of the specified expression to 'destOp'.
	void StoreInitializer(IR::Operand* destOp, const Expression* initializer);

	// Generates code that stores a value into a bitfield.
	IR::Instruction* StoreToBitfield(IR::Operand* addressOp, IR::Operand* valueOp, 
									 FieldDeclaration* field, FieldInfo& fieldInfo);

    bool AccessIsAtByteOffset(FieldInfo& fieldInfo) {
        return fieldInfo.IsByteSize() && 
               ((fieldInfo.UnitOffset() % fieldInfo.Size()) == 0);
    }

	// Generates code that copies the value from the parameters associated with
	// an expanded "simple record" to the location indicated by 'structRef'.
	void InitializeExpandedStruct(IR::Operand* structRef, const VariableDeclaration* structVar);

	// Generates code that loads a value having integer, floating or pointer type.
	IR::Operand* LoadSimpleValue(IR::Operand* addressOp, const Type* type);

    IR::Operand* LoadSimpleValue(IR::Operand* addressOp);

	// Generates code that copies the record at address 'sourceOp' to 'destOp'.
	// Uses an optimized variant for "simple records".
	void GenerateRecordAssignment(IR::Operand* destOp, IR::Operand* sourceOp, 
								  const Type* destType);

	// Tries to eliminate some unnecessary casts that involve binary operators.
	void SimplifyBinaryOpCasts(shared<Expression>& leftValue, 
							   shared<Expression>& rightValue,
							   const Type** newLeftType = nullptr,
							   const Type** newRightType = nullptr);

	// Generates code that convert from one integer type to the other.
	void GenerateIntToIntCast(IR::Operand*& op, const IR::IntegerType* newType, 
							  const Type* oldType);

    // Sets the record that should be used as a target 
    // when a record assignment is generated.
    void SetRecordReturnTarget(IR::Operand* target) {
        recordReturnTarget_ = target;
    }

    // Returns the expression that is being processed.
    const Expression* CurrentExpression() const {
        return currentExpr_;
    }
};

} // namespace IRGenerator
#endif