// GeneratorHelpers.hpp
// Copyright (c) Lup Gratian
//
// Defines various objects used by the code generator.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_HELPERS_HPP
#define PC_IR_GENERATOR_HELPERS_HPP

#include "../AST/Types.hpp"
#include "../AST/Unit.hpp"
#include "../AST/Expressions.hpp"
#include "../IR/IRTypes.hpp"
#include "../IR/Symbols.hpp"
#include "../IR/Unit.hpp"
#include "../IR/TypeTable.hpp"
#include "../IR/Intrinsic.hpp"
#include "../IR/References.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
using namespace Base;
using namespace AST;
using namespace Common;

namespace IRGenerator {

// Used to store the parameter that acts as the field of an expanded 'struct'.
struct ExpandedField {
	const FieldDeclaration* Field;       // The expanded field.
	const VariableDeclaration* Variable; // The expanded variable (with 'struct' type).
	IR::Variable* Parameter;             // Where the field was expanded.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ExpandedField() : Field(nullptr), Variable(nullptr), Parameter(nullptr) {}

	ExpandedField(const FieldDeclaration* field, const VariableDeclaration* variable, 
				  IR::Variable* parameter = nullptr) :
			Field(field), Variable(variable), Parameter(parameter) {}

	ExpandedField(const ExpandedField& other) :
			Field(other.Field), Variable(other.Variable),
            Parameter(other.Parameter) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	unsigned GetHashCode() const {
		return (unsigned)Field ^
			   (unsigned)Variable;
	}

	bool operator== (const ExpandedField& other) const {
		return (Field == other.Field) &&
			   (Variable == other.Variable);
	}

	bool operator< (const ExpandedField& other) const { return false; }
};


// Holds the IR representation of a function, together with the nodes
// from the linked list in which it was inserted.
struct FunctionHolder {
	IR::Function* Function;
	IR::Unit::FunctionNode* FunctionNode;
	IR::Unit::SymbolNode* FunctionSymbolNode;
	List<ExpandedField> Fields;
	bool Generated;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	FunctionHolder() : Function(nullptr), FunctionNode(nullptr), 
					   FunctionSymbolNode(nullptr), Generated(false) {}

	FunctionHolder(IR::Function* function, IR::Unit::FunctionNode* functNode = nullptr,
					IR::Unit::SymbolNode* functSymNode = nullptr) :
		Function(function), FunctionNode(functNode), 
        FunctionSymbolNode(functSymNode), Generated(false) {}

	~FunctionHolder() {
		// If the function was not used it can be freed.
		if(Function->ParentUnit() == nullptr) {
			Function->Free();
		}
	}
};


// Used as the key for the "simple record" info cache.
struct SimpleRecord {
	const StructUnionType* Record;
	int MaxFields;
	int MaxLevels;
	bool UnionAllowed;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	SimpleRecord() : Record(nullptr), MaxFields(0),
                     MaxLevels(0), UnionAllowed(false) {}

	SimpleRecord(const StructUnionType* record, int maxFields, int maxLevels,
					bool unionAllowed) :
			Record(record), MaxFields(maxFields), MaxLevels(maxLevels),
			UnionAllowed(unionAllowed) {}

	SimpleRecord(const SimpleRecord& other) :
			Record(other.Record), MaxFields(other.MaxFields), 
			MaxLevels(other.MaxLevels), UnionAllowed(other.UnionAllowed)  {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	unsigned GetHashCode() const {
		return (unsigned)Record ^ MaxFields ^ MaxLevels ^ 
			   (unsigned)UnionAllowed;
	}

	bool operator== (const SimpleRecord& other) const {
		return (Record == other.Record)        &&
				(MaxFields == other.MaxFields) &&
				(MaxLevels == other.MaxLevels) &&
				(UnionAllowed == other.UnionAllowed);
	}

	bool operator< (const SimpleRecord& other) const { return false; }
};


// Stores a pair consisting of a variable and it's IR representation.
struct VariablePair {
	VariableDeclaration* Variable;
	IR::Variable* IRVariable;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	VariablePair() : Variable(nullptr), IRVariable(nullptr) {}

	VariablePair(VariableDeclaration* variable, IR::Variable* irVariable) :
			Variable(variable), IRVariable(irVariable) {}

	VariablePair(const VariablePair& other) :
			Variable(other.Variable), IRVariable(other.IRVariable) {}
};


// Used to store info about the variable where a record
// can be temporarily stored when making a 'call'.
struct RecordSlot {
	const StructUnionType* RecordType;
	IR::Variable* IRVariable;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	RecordSlot() {}

	RecordSlot(const StructUnionType* type, IR::Variable* irVariable) :
			RecordType(type), IRVariable(irVariable) {}

	RecordSlot(const RecordSlot& other) :
			RecordType(other.RecordType), IRVariable(other.IRVariable) {}
};


// Used to store the information necessary to implement
// the 'break'/'continue' statements.
struct LoopInfo {
	IR::Block* HeaderBlock;         // The loop header block.
	IR::Block* ContinuationBlock;   // The block immediately after the whole loop.
    const Statement* BodyStatement; // The statement that acts as the loop body.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	LoopInfo() : HeaderBlock(nullptr), ContinuationBlock(nullptr) {}

	LoopInfo(IR::Block* header, IR::Block* cont, const Statement* bodyStatement) :
			HeaderBlock(header), ContinuationBlock(cont),
            BodyStatement(bodyStatement) {}

	LoopInfo(const LoopInfo& other) :
			HeaderBlock(other.HeaderBlock), 
            ContinuationBlock(other.ContinuationBlock),
            BodyStatement(other.BodyStatement) {}
};


// Used to store information about the computed size 
// of a VLA originated from a 'typedef'.
struct VLATypedefInfo {
	IR::Operand* SizeOperand;
	const ArrayType* VLAType;
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	VLATypedefInfo() {}

	VLATypedefInfo(IR::Operand* sizeOp, const ArrayType* vlaType) :
			SizeOperand(sizeOp), VLAType(vlaType) {}

	VLATypedefInfo(const VLATypedefInfo& other) :
			SizeOperand(other.SizeOperand), VLAType(other.VLAType) {}
};


// Used to store information about a variable-length array.
struct VLAInfo {
	IR::Operand* SizeOperand;
	IR::Variable* BaseAddressVariable;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	VLAInfo() : BaseAddressVariable(nullptr) {}

	VLAInfo(IR::Operand* sizeOp, IR::Variable* baseAddrVar) :
			SizeOperand(sizeOp), BaseAddressVariable(baseAddrVar) {}

	VLAInfo(const VLAInfo& other) :
			SizeOperand(other.SizeOperand), 
            BaseAddressVariable(other.BaseAddressVariable) {}
};


// Stores the VLAs that have been declared in a statement.
struct VLAContext {
	const Statement* Parent;        // The associated statement.
	IR::Variable* TopStackVariable; // The top of the stack when the context was entered.
	IR::Operand* StackTopOp;        // The operand that contains the top of the stack.
	int VLACount;                   // The number of VLAs declared in this context.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	VLAContext() : Parent(nullptr), TopStackVariable(nullptr), VLACount(0) {}

	VLAContext(const Statement* parent, IR::Variable* topVar, IR::Operand* topOp) :
			Parent(parent), TopStackVariable(topVar), StackTopOp(topOp), VLACount(0) {}

	VLAContext(const VLAContext& other) :
			Parent(other.Parent), TopStackVariable(other.TopStackVariable), 
			StackTopOp(other.StackTopOp), VLACount(other.VLACount) {}
};

} // namespace IRGenerator
#endif