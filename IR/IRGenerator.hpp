// IRGenerator.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_HPP
#define PC_IR_GENERATOR_HPP

#include "IRTypes.hpp"
#include "Instructions.hpp"
#include "Symbols.hpp"
#include "Operand.hpp"
#include "Temporary.hpp"
#include "References.hpp"
#include "Constants.hpp"
#include "Unit.hpp"
#include "Tags.hpp"
#include "Intrinsics.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuffer.hpp"
using namespace Base;

namespace IR {

class IRGenerator {
private:
	Unit* unit_;
	bool namedTemp_;
	Dictionary<Opcode, __int64> opcodeCount_;
	Instruction* afterInstr_;

    Instruction* GetIP(Instruction* instr) {
		if(instr) return instr;
		else return afterInstr_;
	}

	template <class T>
	T* SetIP(T* instr) {
		if(afterInstr_) {
			afterInstr_ = instr;
		}

		return instr;
	}

public:
	IRGenerator() : namedTemp_(false), afterInstr_(nullptr) {}

	IRGenerator(Unit* unit, bool namedTemp = false) : 
			unit_(unit), namedTemp_(namedTemp), afterInstr_(nullptr) {
		DebugValidator::IsNotNull(unit);
        if(namedTemp_) ResetNames();
	}

	IRGenerator(const IRGenerator& other) : 
			unit_(other.unit_), namedTemp_(other.namedTemp_),
			afterInstr_(other.afterInstr_) {
		if(namedTemp_) ResetNames();
	}

	IRGenerator& operator= (const IRGenerator& other) {
		if(namedTemp_) ResetNames();
		unit_ = other.unit_;
		namedTemp_ = other.namedTemp_;
		return *this;
	}

	Unit* GetUnit() {
		return unit_;
	}
							
	const Unit* GetUnit() const {
		return unit_;
	}

    Instruction* GetInsertionPoint() {
        return GetIP(nullptr);
    }

	void SetInsertionPoint(Instruction* instr) {
		afterInstr_ = instr;
	}
				
	// ##################################################################################
	// TYPES
	// ##################################################################################
	// Methods for constructing simple types.
	const IntegerType*   GetInt8()   { return IntegerType::GetInt8();    }
	const IntegerType*   GetInt16()  { return IntegerType::GetInt16();   }
	const IntegerType*   GetInt32()  { return IntegerType::GetInt32();   }
	const IntegerType*   GetInt64()  { return IntegerType::GetInt64();   }
	const FloatingType*  GetFloat()  { return FloatingType::GetFloat();  }
	const FloatingType*  GetDouble() { return FloatingType::GetDouble(); }
	const VoidType*      GetVoid()   { return VoidType::GetVoid();       }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for constructing pointer types.
	const PointerType* GetPointer(const Type* pointee) {
		return unit_->Types().GetPointer(pointee);
	}

	const PointerType* GetInt8Pointer()   { return GetPointer(GetInt8());   }
	const PointerType* GetInt16Pointer()  { return GetPointer(GetInt16());  }
	const PointerType* GetInt32Pointer()  { return GetPointer(GetInt32());  }
	const PointerType* GetInt64Pointer()  { return GetPointer(GetInt64());  }
	const PointerType* GetFloatPointer()  { return GetPointer(GetFloat());  }
	const PointerType* GetDoublePointer() { return GetPointer(GetDouble()); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for constructing array types.
	const ArrayType* GetArray(const Type* elementType, __int64 size) {
		return unit_->Types().GetArray(elementType, size);
	}

	const ArrayType* GetInt8Array(__int64 size)   { return GetArray(GetInt8(),   size); }
	const ArrayType* GetInt16Array(__int64 size)  { return GetArray(GetInt16(),  size); }
	const ArrayType* GetInt32Array(__int64 size)  { return GetArray(GetInt32(),  size); }
	const ArrayType* GetInt64Array(__int64 size)  { return GetArray(GetInt64(),  size); }
	const ArrayType* GetFloatArray(__int64 size)  { return GetArray(GetFloat(),  size); }
	const ArrayType* GetDoubleArray(__int64 size) { return GetArray(GetDouble(), size); }
														   
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for constructing function types.
	const FunctionType* GetFunction(const Type* returnType, const Type** parameters = nullptr, 
								    int paramCount = 0, bool isVarargs = false) {
		return unit_->Types().GetFunction(returnType, parameters, paramCount, isVarargs);
	}
	
	// Methods for constructing methods having a specific return type.
	#define FUNCTION(TYPE) \
		const FunctionType* \
		Get##TYPE##Function(const Type** parameters = nullptr, int paramCount = 0, \
					        bool isVarargs = false) { \
		return GetFunction(Get##TYPE(), parameters, paramCount, isVarargs); \
	}

	FUNCTION(Int8);
	FUNCTION(Int16);
	FUNCTION(Int32);
	FUNCTION(Int64);
	FUNCTION(Float);
	FUNCTION(Double);
	FUNCTION(Void);

	#undef FUNCTION

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for constructing record types.
	const RecordType* GetRecord(const List<RecordField>& fields) {
		return unit_->Types().GetRecord(fields);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for working with named types.
	Symbol* AddNamed(const string& name, const Type* type) {
		Symbol* typeName = Symbol::GetTypename(type, const_cast<string&>(name), &unit_->Symbols());
		return unit_->AddTypename(typeName);
	}

	bool ContainsNamed(const string& name) {
		return unit_->Types().ContainsNamed(&const_cast<string&>(name));
	}

	const Type* GetNamed(const string& name) {
		return unit_->Types().GetNamed(&const_cast<string&>(name));
	}

	void RemoveNamed(const string& name) {
		unit_->Types().RemoveNamed(&const_cast<string&>(name));
	}

	void ClearNamed() {
		return unit_->Types().ClearNamed();
	}

	Symbol* GetNamedRecord(const string& name, const List<RecordField>& fields) {
		return AddNamed(name, GetRecord(fields));
	}

	Symbol* GetNamedFunction(const string& name, const Type* returnType, 
						     const Type** parameters = nullptr, int paramCount = 0, 
						     bool isVarargs = false) {
		auto functionType = GetFunction(returnType, parameters, paramCount, isVarargs);
		return AddNamed(name, functionType);
	}

	// ##################################################################################
	// SYMBOLS
	// ##################################################################################
	Block* GetBlockSymbol(const string& name, Function* parent = nullptr, 
						  Block* previous = nullptr) {
		return Block::GetBlock(name, parent, previous);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Function* GetFunctionSymbol(const FunctionType* type, const string& name, 
							    bool isDef = true, Unit* parent = nullptr,
							    SymbolVisibility visibility = SymbolVisibility::Extern,
							    CallConventionType callConv = CallConventionType::Cdecl) {
		return Function::GetFunction(type, name, isDef, parent ? parent : unit_,
									 visibility, callConv);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Variable* GetVariableSymbol(const Type* type, const string& name, 
								Function* parent = nullptr,
								SymbolVisibility visibility = SymbolVisibility::Auto) {
		return Variable::GetVariable(type, new string(name), parent, visibility);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	GlobalVariable* GetGlobalSymbol(const Type* type, const string& name, 
							       shared<Initializer> initializer = nullptr, 
							       SymbolTable* parent = nullptr, 
							       SymbolVisibility visibility = SymbolVisibility::Extern) {
		auto global = GlobalVariable::GetGlobal(type, name, initializer,
												parent, visibility);
		if(parent) unit_->AddVariable(global);
		return global;
	}
	
	GlobalVariable* GetGlobalZeroSymbol(const Type* type, const string& name, 
										SymbolTable* parent = nullptr, 
										SymbolVisibility visibility = SymbolVisibility::Extern) {
		auto global = GlobalVariable::GetZeroInitialized(type, name, parent, visibility);
		if(parent) unit_->AddVariable(global);
		return global;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	shared<Initializer> GetInitializer(Operand* value, __int64 adjust = 0,
									   InitConversion conversion = InitConversion::None,
									   const Type* conversionType = nullptr) {
		return Initializer::GetInitializer(value, adjust, conversion, conversionType);
	}
									  

	// ##################################################################################
	// OPERANDS
	// ##################################################################################
	Temporary* GetTemporary(const Type* type, Instruction* definingInstr = nullptr) {
		return Temporary::GetTemporary(type, definingInstr);
	}

	Temporary* GetIntTemp(IRIntegerKind kind, Instruction* definingInstr = nullptr) { 
		return GetTemporary(IntegerType::GetInt(kind), definingInstr);
	}

	Temporary* GetFloatingTemp(IRFloatingKind kind, Instruction* definingInstr = nullptr) { 
		return GetTemporary(FloatingType::GetFloating(kind), definingInstr);
	}

	Temporary* GetPointerTemp(const Type* pointee, Instruction* definingInstr = nullptr) {
		return GetTemporary(GetPointer(pointee), definingInstr); 
	}

	Temporary* GetInt8Temp(Instruction* definingInstr = nullptr)   { 
		return GetTemporary(GetInt8(), definingInstr);   
	}

	Temporary* GetInt16Temp(Instruction* definingInstr = nullptr)  { 
		return GetTemporary(GetInt16(), definingInstr);  
	}
	
	Temporary* GetInt32Temp(Instruction* definingInstr = nullptr)  { 
		return GetTemporary(GetInt32(), definingInstr);  
	}
	
	Temporary* GetInt64Temp(Instruction* definingInstr = nullptr)  {
		return GetTemporary(GetInt64(), definingInstr);  
	}
	
	Temporary* GetFloatTemp(Instruction* definingInstr = nullptr)  {
		return GetTemporary(GetFloat(), definingInstr);  
	}
	
	Temporary* GetDoubleTemp(Instruction* definingInstr = nullptr) {
		return GetTemporary(GetDouble(), definingInstr); 
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	VariableReference* GetVariableRef(Variable* variable, const Type* type = nullptr) {
		DebugValidator::IsNotNull(variable);
		
		if(type == nullptr) {
			auto pointerType = GetPointer(variable->GetType());
			return unit_->References().GetVariableRef(variable, pointerType);
		}
		else return unit_->References().GetVariableRef(variable, type);
	}

	VariableReference* GetVariableRef(GlobalVariable* variable, const Type* type = nullptr) {
		DebugValidator::IsNotNull(variable);
		
		if(type == nullptr) {
			auto pointerType = GetPointer(variable->GetType());
			return unit_->References().GetGlobalVariableRef(variable, pointerType);
		}
		else return unit_->References().GetGlobalVariableRef(variable, type);
	}

	FunctionReference* GetFunctionRef(Function* function) {
		auto functPtrType = GetPointer(function->GetType());
		return unit_->References().GetFunctionRef(function, functPtrType);
	}

	BlockReference* GetBlockRef(Block* block) {
		return unit_->References().GetBlockRef(block);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for constructing constant operands.
	IntConstant* GetIntConst(const Type* type, __int64 value) {
		return unit_->Constants().GetInt(type, value);
	}

	IntConstant* GetInt8Const(__int64 value)  { return GetIntConst(GetInt8(), value);  }
	IntConstant* GetInt16Const(__int64 value) { return GetIntConst(GetInt16(), value); }
	IntConstant* GetInt32Const(__int64 value) { return GetIntConst(GetInt32(), value); }
	IntConstant* GetInt64Const(__int64 value) { return GetIntConst(GetInt64(), value); }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	FloatConstant* GetFloatingConst(const Type* type, double value) {
		return unit_->Constants().GetFloating(type, value);
	}

	FloatConstant* GetFloatConst(double value) {
		return unit_->Constants().GetFloat(value);
	}

	FloatConstant* GetDoubleConst(double value) {
		return unit_->Constants().GetDouble(value);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StringConstant* GetStringConst(const StringBuffer& value, const Type* type = nullptr) {
		if(type == nullptr) {
			return unit_->Constants().GetString(GetInt8Pointer(), value);
		}
		else return unit_->Constants().GetString(type, value);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	NullConstant* GetNullConst(const Type* type) {
		return unit_->Constants().GetNull(type);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	UndefinedConstant* GetUndefinedConst(const Type* type) {
		return unit_->Constants().GetUndefined(type);
	}

	// ##################################################################################
	// INSTRUCTIONS
	// ##################################################################################
	// Methods for constructing arithmetic and logical instructions.
	ArithmeticInstr* GetArithmetic(Opcode opcode, Operand* left = nullptr, 
								   Operand* right = nullptr, Operand* result= nullptr, 
								   Block* block = nullptr, Instruction* previous = nullptr) {
		return SetIP(ArithmeticInstr::GetArithmetic(opcode, left, right, result,
                                                    nullptr, GetIP(previous)));
	}

	LogicalInstr* GetLogical(Opcode opcode, Operand* left = nullptr, 
								   Operand* right = nullptr, Operand* result= nullptr, 
								   Block* block = nullptr, Instruction* previous = nullptr) {
		return SetIP(LogicalInstr::GetLogical(opcode, left, right, result,
                                              nullptr, GetIP(previous)));
	}

	#define ARITHMETIC_LOGICAL(NAME) \
		NAME##Instr* \
		Get##NAME(Operand* left = nullptr, Operand* right = nullptr, \
			      Operand* result= nullptr, Block* block = nullptr, \
			      Instruction* previous = nullptr) { \
			return SetIP(AddName(NAME##Instr::Get##NAME(left, right, result, block, \
														GetIP(previous)))); \
		}

	ARITHMETIC_LOGICAL(Add)
	ARITHMETIC_LOGICAL(Sub)
	ARITHMETIC_LOGICAL(Mul)
	ARITHMETIC_LOGICAL(Div)
	ARITHMETIC_LOGICAL(Udiv)
	ARITHMETIC_LOGICAL(Mod)
	ARITHMETIC_LOGICAL(Umod)
	ARITHMETIC_LOGICAL(Fadd)
	ARITHMETIC_LOGICAL(Fsub)
	ARITHMETIC_LOGICAL(Fmul)
	ARITHMETIC_LOGICAL(Fdiv)
	ARITHMETIC_LOGICAL(Shl)
	ARITHMETIC_LOGICAL(Shr)
	ARITHMETIC_LOGICAL(Ushr)
	ARITHMETIC_LOGICAL(And)
	ARITHMETIC_LOGICAL(Or)
	ARITHMETIC_LOGICAL(Xor)
	#undef ARITHMETIC_LOGICAL

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for constructing conversion instructions.
    ConversionInstr* GetConversion(Opcode opcode, Operand* target = nullptr, 
								   const Type* castType = nullptr, Operand* result= nullptr, 
								   Block* block = nullptr, Instruction* previous = nullptr) {
		return SetIP(ConversionInstr::GetConversion(opcode, target, castType, result,
                                                    nullptr, GetIP(previous)));
	}

	#define CONVERSION(NAME) \
		NAME##Instr* \
		Get##NAME(Operand* target = nullptr, const Type* castType = nullptr, \
			      Operand* result = nullptr, Block* block = nullptr, \
			      Instruction* previous = nullptr) { \
			return SetIP(AddName(NAME##Instr::Get##NAME(target, castType, result, block, \
														GetIP(previous)))); \
		}

	CONVERSION(Trunc)
	CONVERSION(Zext)
	CONVERSION(Sext)
	CONVERSION(Ftoi)
	CONVERSION(Ftoui)
	CONVERSION(Itof)
	CONVERSION(Uitof)
	CONVERSION(Ftrunc)
	CONVERSION(Fext)
	CONVERSION(Ptoi)
	CONVERSION(Itop)
	CONVERSION(Ptop)
	#undef CONVERSION

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for constructing control instructions.
	CmpInstr* GetCmp(OrderType order, Operand* left = nullptr, 
					 Operand* right = nullptr, Operand* result = nullptr,
					 Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(CmpInstr::GetCmp(order, left, right, result, parent,
											  GetIP(previous))));
	}

	UcmpInstr* GetUcmp(OrderType order, Operand* left = nullptr, 
					   Operand* right = nullptr, Operand* result = nullptr,
					   Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(UcmpInstr::GetUcmp(order, left, right, result, parent, 
												GetIP(previous))));
	}

	FcmpInstr* GetFcmp(OrderType order, Operand* left = nullptr, 
					   Operand* right = nullptr, Operand* result = nullptr,
					   Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(FcmpInstr::GetFcmp(order, left, right, result, parent, 
												GetIP(previous))));
	}

	#define COMPARE(NAME, ORDER, SUFFIX) \
		NAME##Instr* Get##NAME##SUFFIX(Operand* left = nullptr, \
									   Operand* right = nullptr, \
									   Operand* result = nullptr, \
									   Block* parent = nullptr, \
									   Instruction* previous = nullptr) { \
			return Get##NAME(ORDER, left, right, result, parent, previous); \
		}

	// CmpInstr* CmpLT(...)
	COMPARE(Cmp,  OrderType::Less,           LT)
	COMPARE(Cmp,  OrderType::LessOrEqual,    LTE)
	COMPARE(Cmp,  OrderType::Greater,        GT)
	COMPARE(Cmp,  OrderType::GreaterOrEqual, GTE)
	COMPARE(Cmp,  OrderType::Equal,          EQ)
	COMPARE(Cmp,  OrderType::NotEqual,       NEQ)

	// UcmpInstr* UcmpLTE(...)
	COMPARE(Ucmp, OrderType::Less,           LT)
	COMPARE(Ucmp, OrderType::LessOrEqual,    LTE)
	COMPARE(Ucmp, OrderType::Greater,        GT)
	COMPARE(Ucmp, OrderType::GreaterOrEqual, GTE)
	COMPARE(Ucmp, OrderType::Equal,          EQ)
	COMPARE(Ucmp, OrderType::NotEqual,       NEQ)

	// FcmpInstr* FcmpEQ(...)
	COMPARE(Fcmp, OrderType::Less,           LT)
	COMPARE(Fcmp, OrderType::LessOrEqual,    LTE)
	COMPARE(Fcmp, OrderType::Greater,        GT)
	COMPARE(Fcmp, OrderType::GreaterOrEqual, GTE)
	COMPARE(Fcmp, OrderType::Equal,          EQ)
	COMPARE(Fcmp, OrderType::NotEqual,       NEQ)
	#undef COMPARE

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	IfInstr* GetIf(Operand* condition, BlockReference* trueTarget = nullptr,
				   BlockReference* falseTarget = nullptr, Block* parent = nullptr,
				   Instruction* previous = nullptr) {
		return SetIP(AddName(IfInstr::GetIf(condition, trueTarget, falseTarget, parent,
											GetIP(previous))));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	GotoInstr* GetGoto(BlockReference* target, Block* parent = nullptr,
					   Instruction* previous = nullptr) {
		return SetIP(AddName(GotoInstr::GetGoto(target, parent, GetIP(previous))));
	}

	GotoInstr* GetGoto(Block* block, Block* parent = nullptr, 
					   Instruction* previous = nullptr) {
		BlockReference* blockRef = GetBlockRef(block);
		return SetIP(AddName(GotoInstr::GetGoto(blockRef, parent, GetIP(previous))));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	CallInstr* GetCall(Operand* target, Operand* result  = nullptr, int argCapacity = 0,
					   Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(CallInstr::GetCall(target, result, argCapacity, parent, 
												GetIP(previous))));
	}

	CallInstr* GetCall(Function* function, Operand* result  = nullptr, int argCapacity = 0,
					   Block* parent = nullptr, Instruction* previous = nullptr) {
		FunctionReference* functionRef = GetFunctionRef(function);
		return SetIP(GetCall(functionRef, result, argCapacity, parent, GetIP(previous)));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ReturnInstr* GetReturn(Operand* result = nullptr, Block* parent = nullptr,
						   Instruction* previous = nullptr) {
		return SetIP(AddName(ReturnInstr::GetReturn(result, parent, GetIP(previous))));
	}

	ReturnInstr* GetVoidReturn(Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(ReturnInstr::GetReturn(nullptr, parent, GetIP(previous))));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods for constructing other instructions.
	LoadInstr* GetLoad(Operand* source, Operand* result = nullptr, 
					   Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(LoadInstr::GetLoad(source, result, parent, 
												GetIP(previous))));
	}

	LoadInstr* GetLoad(Variable* source, Operand* result = nullptr,
					   Block* parent = nullptr, Instruction* previous = nullptr) {
		Operand* varOp = GetVariableRef(source);
		return SetIP(AddName(LoadInstr::GetLoad(varOp, result, parent, 
												GetIP(previous))));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StoreInstr* GetStore(Operand* dest, Operand* source, Block* parent = nullptr, 
						 Instruction* previous = nullptr) {
		return SetIP(AddName(StoreInstr::GetStore(dest, source, parent, 
												  GetIP(previous))));
	}

	StoreInstr* GetStore(Variable* dest, Operand* source, Block* parent = nullptr,
						 Instruction* previous = nullptr) {
		Operand* varOp = GetVariableRef(dest);
		return SetIP(AddName(StoreInstr::GetStore(varOp, source, parent, 
												  GetIP(previous))));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	AddressInstr* GetAddress(Operand* base, Operand* index, Operand* result, 
							 Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(AddressInstr::GetAddress(base, index, result, parent, 
													  GetIP(previous))));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	IndexInstr* GetIndex(Operand* base, Operand* index, Operand* result,
						 Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(IndexInstr::GetIndex(base, index, result, parent, 
												  GetIP(previous))));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	FieldInstr* GetField(Operand* base, Operand* index, Operand* result,
							 Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(FieldInstr::GetField(base, index, result, parent, 
													  GetIP(previous))));
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	SwitchInstr* GetSwitch(Operand* cond, int caseListCapacity,
						   BlockReference* defaultTarget = nullptr,
						   Block* parent = nullptr, Instruction* previous = nullptr) {
		return SetIP(AddName(SwitchInstr::GetSwitch(cond, caseListCapacity, 
													defaultTarget, parent, 
													GetIP(previous))));
	}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    QuestionInstr* GetQuestion(Operand* cond, Operand* left, Operand* right, Operand* result, 
                               Block* parent = nullptr, Instruction* previous = nullptr) {
        return SetIP(AddName(QuestionInstr::GetQuestion(cond, left, right, result, 
                                                        parent, GetIP(previous))));
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    PhiInstr* GetPhi(Operand* result = nullptr, int opCapacity = 0,
                     Block* parent = nullptr, Instruction* previous = nullptr) {
        return SetIP(AddName(PhiInstr::GetPhi(result, opCapacity, 
                                              parent, GetIP(previous))));
    }

	// ##################################################################################
	// TAGS
	// ##################################################################################
	void ResetNames() {
		opcodeCount_.Clear();

		#define instruction(TYPE, CAT, NAME) opcodeCount_.Add(Opcode::##TYPE, 1);
		#include "Instructions.def"
		#undef instruction
	}

	NameTag* GetName(Opcode opcode) {
		string opcodeStr = Instruction::OpcodeString(opcode);
		string name = string::Format(L"%s%d", opcodeStr.Chars(), opcodeCount_[opcode]++);
		return NameTag::GetName(name);
	}

	void AddName(Operand* op, Instruction* instr) {
        if(auto temp = op->As<Temporary>()) {
            temp->AddTag(GetName(instr->GetOpcode()));
        }
	}

	void AddName(Operand* op, Opcode opcode) {
        if(auto temp = op->As<Temporary>()) {
            temp->AddTag(GetName(opcode));
        }
	}

	void AddName(Operand* op, const string& name) {
        if(auto temp = op->As<Temporary>()) {
            temp->AddTag(NameTag::GetName(name));
        }
	}

	template <class T>
	T* AddName(T* instr) {
		if(instr && namedTemp_) {
			// Only for temporaries that are the destination of an instruction.
			if(instr->HasDestinationOp() && instr->GetDestinationOp() &&
			   instr->GetDestinationOp()->IsTemporary()) {
				AddName(instr->GetDestinationOp(), instr);
			}
		}

		return instr;
	}

	#define NAME_TAG(TYPE) \
		NameTag* Get##TYPE##Name() { \
			return GetName(Opcode::##TYPE); \
		}

	#define instruction(TYPE, CAT, NAME) NAME_TAG(TYPE)
	#include "Instructions.def"
	#undef instruction
	#undef NAME_TAG

	// ##################################################################################
	// INTRINSICS
	// ##################################################################################
	CallInstr* GetSetMemoryCall(Operand* destOp, Operand* valueOp, Operand* lengthOp,
							    Block* parent = nullptr, Instruction* previous = nullptr) {
		auto intrinsic = SetMemoryIntr::GetSetMemory(unit_);
		auto callInstr = GetCall(intrinsic, nullptr /* result */, 3, parent, previous);
		callInstr->AddArgument(destOp);
		callInstr->AddArgument(valueOp);
		callInstr->AddArgument(lengthOp);
		return callInstr;
	}

	CallInstr* GetCopyMemoryCall(Operand* destOp, Operand* valueOp,
								 Operand* lengthOp, Block* parent = nullptr,
								 Instruction* previous = nullptr) {
		auto intrinsic = CopyMemoryIntr::GetCopyMemory(unit_);
		auto callInstr = GetCall(intrinsic, nullptr /* result */, 3, parent, previous);
		callInstr->AddArgument(destOp);
		callInstr->AddArgument(valueOp);
		callInstr->AddArgument(lengthOp);
		return callInstr;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	CallInstr* GetStackTopCall(Operand* resultOp, Block* parent = nullptr,
							   Instruction* previous = nullptr) {
		auto intrinsic = StackTopIntr::GetStackTop(unit_);
		return GetCall(intrinsic, resultOp, 0, parent, previous);
	}

	CallInstr* GetIncStackTopCall(Operand* valueOp, Block* parent = nullptr,
								  Instruction* previous = nullptr) {
		auto intrinsic = IncStackTopIntr::GetIncStackTop(unit_);
		auto callInstr = GetCall(intrinsic, nullptr /* result */, 1, parent, previous);
		callInstr->AddArgument(valueOp);
		return callInstr;
	}

	CallInstr* GetRestoreStackTopCall(Operand* valueOp, Block* parent = nullptr,
									  Instruction* previous = nullptr) {
		auto intrinsic = RestoreStackTopIntr::GetRestoreStackTop(unit_);
		auto callInstr = GetCall(intrinsic, nullptr /* result */, 1, parent, previous);
		callInstr->AddArgument(valueOp);
		return callInstr;
	}
};

} // namespace IR
#endif