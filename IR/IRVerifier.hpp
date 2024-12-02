// IRVerifier.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_SEMANTIC_HPP
#define PC_IR_SEMANTIC_HPP

#include "Instructions.hpp"
#include "Symbols.hpp"
#include "Unit.hpp"
#include "Operand.hpp"
#include "Temporary.hpp"
#include "Constants.hpp"
#include "References.hpp"
#include "Visitor.hpp"
#include "Tags.hpp"
#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

// Represents the category (type) of the error.
enum ErrorType {
	Error_Variable,
	Error_Function,
	Error_Block,
	Error_Instruction,
	Error_Argument
};


// The errors that can be emitted by the verifier.
namespace Error {
	#define error(NAME) static const int NAME = __LINE__;
	#include "IRVerifierErrors.def"
	#undef error
}


// Represents a message used to signal and provide details about an error.
class VerifierError {
public:
	ErrorType Type;
	int Error;
	const Function* ParentFunction;
	const string* BlockName;
	const string* SymbolName;
	void* Object;
	int InstructionIndex;
	bool IsGlobal;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	VerifierError(ErrorType type, int error, void* obj, const Function* function = nullptr,
				  const string* block = nullptr, int instrIndex = -1, int argIndex = -1) :
			Type(type), IsGlobal(false), Error(error), Object(obj), ParentFunction(function), 
			BlockName(block), InstructionIndex(instrIndex), SymbolName(nullptr) {}

	VerifierError(bool global, int error, void* obj, const  string* name, 
				  const Function* function = nullptr) :
			Type(Error_Variable), IsGlobal(global), Error(error), Object(obj), 
			ParentFunction(function), BlockName(nullptr), InstructionIndex(-1),
            SymbolName(name) {}

	VerifierError(int error, void* obj, const Function* function, 
                  const string* parameter, int paramIndex) :
			Type(Error_Argument), IsGlobal(true), Error(error), Object(obj), 
			ParentFunction(function), BlockName(nullptr), InstructionIndex(-1),
            SymbolName(nullptr) {}
};


// Base class for objects that can handle error messages.
class VerifierErrorHandler {
public:
	virtual ~VerifierErrorHandler() {}
	virtual void Handle(VerifierError message) = 0;
};


// Default verifier handler that prints the message on the console.
class DefaultVerifierHandler : public VerifierErrorHandler {
protected:
	Dictionary<int, string> verifierMessages_;

public:
	DefaultVerifierHandler();

	void Handle(VerifierError message);
};


class IRVerifier : public Visitor {
private:
	VerifierErrorHandler* handler_;
	Unit* unit_;
	Function* currentFunct_;
	Block* currentBlock_;
	int errorCount_;       // The total number of errors.
	int instrIndex_;       // The index of the current instruction.
	int branchInstrCount_; // The number of branching instruction in the current block.
	bool debugBreak_;      // 'true' if 'assert' should be called if a check fails.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods that check the specified condition, and if 'false' generate
	// an appropriate message, depending on the arguments.
	// All these methods return 'true' if the condition wasn't met.
	bool CheckFunction(bool cond, int error, void* obj, const string* function);
	bool CheckBlock(bool cond, int error, void* obj, 
					const string* function, const string* block);
	bool CheckGlobal(bool cond, int error, void* obj, const string* name);
	bool CheckVariable(bool cond, int error, void* obj, const string* name);
	bool CheckInstruction(bool cond, int error, void* obj);
	bool CheckArgument(bool cond, int error, void* obj, int argIndex);

	// Validates an initializer (calls 'CheckInitializerList' for array/record types).
	void CheckInitializer(const Type* type, Initializer* initializer, Symbol* symbol);

	// Validates each element of an array/record type.
	void CheckInitializerList(const Type* type, InitializerList* initList, Symbol* symbol);

	// Validates a comparison instruction.
	void CheckCompare(CmpInstrBase* instr, bool isInt);

public:
	IRVerifier(Unit* unit, VerifierErrorHandler* handler, bool debugBreak = true);

    IRVerifier(Function* function, VerifierErrorHandler* handler, bool debugBreak = true);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual void Visit(Symbol*         symbol);
	virtual void Visit(Variable*       symbol);
	virtual void Visit(GlobalVariable* symbol);
	virtual void Visit(Block*          symbol);
	virtual void Visit(Function*       symbol);

	virtual void Visit(Temporary*         op);
	virtual void Visit(VariableReference* op);
	virtual void Visit(FunctionReference* op);
	virtual void Visit(BlockReference*    op);
	virtual void Visit(IntConstant*       op);
	virtual void Visit(FloatConstant*     op);
	virtual void Visit(StringConstant*    op);
	virtual void Visit(NullConstant*      op);
	virtual void Visit(UndefinedConstant* op);

	virtual void Visit(ArithmeticInstr* instr);
	virtual void Visit(ConversionInstr* instr);
	virtual void Visit(LogicalInstr*    instr);

	virtual void Visit(LoadInstr*    instr);
	virtual void Visit(StoreInstr*   instr);
	virtual void Visit(AddressInstr* instr);
	virtual void Visit(IndexInstr*   instr);
	virtual void Visit(FieldInstr* instr);
	virtual void Visit(CmpInstr*     instr);
	virtual void Visit(UcmpInstr*    instr);
	virtual void Visit(FcmpInstr*    instr);

	virtual void Visit(IfInstr*     instr);
	virtual void Visit(GotoInstr*   instr);
	virtual void Visit(CallInstr*   instr);
	virtual void Visit(ReturnInstr* instr);
	virtual void Visit(SwitchInstr* instr);
};

} // namespace IR
#endif