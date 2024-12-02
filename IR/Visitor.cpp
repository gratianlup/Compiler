// Visitor.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Visitor.hpp"
#include "Instructions.hpp"
#include "IRTypes.hpp"
#include "Symbols.hpp"
#include "Operand.hpp"
#include "Constants.hpp"
#include "Temporary.hpp"
#include "References.hpp"
#include "Intrinsic.hpp"
#include "Parameter.hpp"
#include "Tags.hpp"

namespace IR {

void Visitor::Visit(ArithmeticInstr* instr) { Visit(static_cast<Instruction*>(instr)); }
void Visitor::Visit(ConversionInstr* instr) { Visit(static_cast<Instruction*>(instr)); }
void Visitor::Visit(LogicalInstr* instr)    { Visit(static_cast<Instruction*>(instr)); }
void Visitor::Visit(ControlInstr* instr)    { Visit(static_cast<Instruction*>(instr)); }

// Exclude instructions in the "other" category at this step.
#define NO_OTHER
#define instruction(TYPE, CAT, NAME) \
	void Visitor::Visit(TYPE##Instr* instr) { Visit(static_cast<CAT##Instr*>(instr)); }
#include "Instructions.def"
#undef instruction
#undef NO_OTHER

// Now consider the "other" instructions.
#define ONLY_SELECTED
#define SELECT_OTHER
#define instruction(TYPE, CAT, NAME) \
	void Visitor::Visit(TYPE##Instr* instr) { Visit(static_cast<Instruction*>(instr)); }
#include "Instructions.def"
#undef instruction
#undef SELECT_OTHER
#undef ONLY_SELECTED

void Visitor::Visit(IntegerType*  type) { Visit(static_cast<Type*>(type)); }
void Visitor::Visit(FloatingType* type) { Visit(static_cast<Type*>(type)); }
void Visitor::Visit(VoidType*     type) { Visit(static_cast<Type*>(type)); }
void Visitor::Visit(PointerType*  type) { Visit(static_cast<Type*>(type)); }
void Visitor::Visit(ArrayType*    type) { Visit(static_cast<Type*>(type)); }
void Visitor::Visit(FunctionType* type) { Visit(static_cast<Type*>(type)); }
void Visitor::Visit(RecordType*   type) { Visit(static_cast<Type*>(type)); }

void Visitor::Visit(Temporary* op)          { Visit(static_cast<Operand*>(op));    }
void Visitor::Visit(Reference* reference)   { Visit(static_cast<Operand*>(reference));   }
void Visitor::Visit(FunctionReference* reference) { Visit(static_cast<Reference*>(reference)); }
void Visitor::Visit(BlockReference*    reference) { Visit(static_cast<Reference*>(reference)); }
void Visitor::Visit(VariableReference* reference) { Visit(static_cast<Reference*>(reference)); }
void Visitor::Visit(Parameter* parameter)   { Visit(static_cast<Operand*>(parameter)); }

void Visitor::Visit(Constant*          op) { Visit(static_cast<Operand*>(op));  }
void Visitor::Visit(IntConstant*       op) { Visit(static_cast<Constant*>(op)); }
void Visitor::Visit(FloatConstant*     op) { Visit(static_cast<Constant*>(op)); }
void Visitor::Visit(StringConstant*    op) { Visit(static_cast<Constant*>(op)); }
void Visitor::Visit(NullConstant*      op) { Visit(static_cast<Constant*>(op)); }
void Visitor::Visit(UndefinedConstant* op) { Visit(static_cast<Constant*>(op)); }

void Visitor::Visit(Variable*       sym) { Visit(static_cast<Symbol*>(sym));   }
void Visitor::Visit(GlobalVariable* sym) { Visit(static_cast<Symbol*>(sym));   }
void Visitor::Visit(Block*          sym) { Visit(static_cast<Symbol*>(sym));   }
void Visitor::Visit(Function*       sym) { Visit(static_cast<Symbol*>(sym));   }
void Visitor::Visit(Intrinsic*      sym) { Visit(static_cast<Function*>(sym)); }

} // namespace IR