// IRVisitor.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_VISITOR_HPP
#define PC_IR_VISITOR_HPP

namespace IR {

// Forward declarations.
class Visitor;
class Instruction;
class ArithmeticInstr;
class ConversionInstr;
class LogicalInstr;
class ControlInstr;

#define instruction(TYPE, CAT, NAME) class TYPE##Instr;
#include "Instructions.def"
#undef instruction

class Type;
class IntegerType;
class FloatingType;
class VoidType;
class PointerType;
class ArrayType;
class FunctionType;
class RecordType;

class Operand;
class Temporary;
class Reference;
class FunctionReference;
class BlockReference;
class VariableReference;
class Parameter;
class Constant;
class IntConstant;
class FloatConstant;
class StringConstant;
class NullConstant;
class UndefinedConstant;

class Symbol;
class Variable;
class GlobalVariable;
class Block;
class Function;
class Intrinsic;

class Tag;
class NameTag;

// All classes that can be visited must derive from this class.
class Visitable {
public:
	virtual ~Visitable() {}
	virtual void Accept(Visitor* v) = 0;
};


// By default the methods dispatch to the base class handlers.
// For example:
// AddInstr -> ArithmeticInstr -> Instruction
// IntConstant -> Constant -> Operand
class Visitor {
public:
	virtual ~Visitor() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual void Visit(Instruction*     instr) {}
	virtual void Visit(ArithmeticInstr* instr);
	virtual void Visit(ConversionInstr* instr);
	virtual void Visit(LogicalInstr*    instr);
	virtual void Visit(ControlInstr*    instr);

	#define instruction(TYPE, CAT, NAME) virtual void Visit(TYPE##Instr* instr);
	#include "Instructions.def"
	#undef instruction

	virtual void Visit(Type*         type) {}
	virtual void Visit(IntegerType*  type);
	virtual void Visit(FloatingType* type);
	virtual void Visit(VoidType*     type);
	virtual void Visit(PointerType*  type);
	virtual void Visit(ArrayType*    type);
	virtual void Visit(FunctionType* type);
	virtual void Visit(RecordType*   type);

	virtual void Visit(Operand*           op) {}
	virtual void Visit(Temporary*         op);
	virtual void Visit(Reference*         reference);
	virtual void Visit(FunctionReference* reference);
	virtual void Visit(BlockReference*    reference);
	virtual void Visit(VariableReference* reference);
	virtual void Visit(Parameter*         parameter);
	virtual void Visit(Constant*          op);
	virtual void Visit(IntConstant*       op);
	virtual void Visit(FloatConstant*     op);
	virtual void Visit(StringConstant*    op);
	virtual void Visit(NullConstant*      op);
	virtual void Visit(UndefinedConstant* op);

	virtual void Visit(Symbol*         sym) {}
	virtual void Visit(Variable*       sym);
	virtual void Visit(GlobalVariable* sym);
	virtual void Visit(Block*          sym);
	virtual void Visit(Function*       sym);
	virtual void Visit(Intrinsic*      sym);

	virtual void Visit(Tag*            tag) {}
};

} // namespace IR
#endif