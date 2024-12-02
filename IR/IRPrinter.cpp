// IRPrinter.cpp
// Copyright (c) Lup Gratian
//
// Implements the IRPrinter class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IRPrinter.hpp"
#include "../Analysis/GlobalConstantsTag.hpp"
#include "../Analysis/GlobalUsersTag.hpp"
#include "../Analysis/KnownBitsTag.hpp"
#include "../Analysis/TypeClassTag.hpp"
#include "../Analysis/CFamilyTag.hpp"
#include "../Analysis/GlobalSideEffectsTag.hpp"
#include "../Analysis/ParameterConstantsTag.hpp"
#include "../Analysis/ParameterAliasTag.hpp"

namespace IR {

IRPrinter::IRPrinter(Unit* unit, bool printTypes) : 
		tempIndex_(0), currentFunct_(nullptr), inInitializer_(false), 
		printTypes_(printTypes), printLabel_(true), unit_(unit),
		printFunctTypeArrows_(true), printBool_(true) {
	BuildTypenameMap(unit);
	enum LastSymbol { None, Var, Funct, FunctDecl, TypeName } last = None;

	for(auto declaration = unit->Declarations().First(); declaration; 
        declaration = declaration->Next) {
		Symbol* symbol = declaration->Value;

		if(symbol->IsTypename()) {
			AppendLine();
			printFunctTypeArrows_ = false;
			symbol->Accept(this);
			printFunctTypeArrows_ = true;
			AppendLine();
			last = TypeName;
		}
		else if(auto function = symbol->As<Function>()) {
			// Skip over intrinsics.
			if(function->IsIntrinsic()) {
                continue;
            }

			bool isFunctDef = function->IsDefinition();
			if((isFunctDef && (last != Funct)) || (last != FunctDecl)) {
				AppendLine();
			}

			function->Accept(this);
			last = isFunctDef ? Funct : FunctDecl;
			if(isFunctDef) AppendLine();
			AppendLine();
		}
		else {
			if(last != Var) AppendLine();
			symbol->Accept(this);
			AppendLine();
			last = Var;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IRPrinter::IRPrinter(const Type* type) : 
		tempIndex_(0), currentFunct_(nullptr), unit_(nullptr), printBool_(true) {
	const_cast<Type*>(type)->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IRPrinter::IRPrinter(Symbol* symbol, bool printTypes, bool printLabel) : 
		tempIndex_(0), currentFunct_(nullptr), unit_(nullptr),
		printTypes_(printTypes), printLabel_(printLabel), printBool_(true) {
	symbol->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IRPrinter::IRPrinter(Operand* op, bool printTypes, bool printLabel) : 
		tempIndex_(0), currentFunct_(nullptr), unit_(nullptr),
		printTypes_(printTypes), printLabel_(printLabel), printBool_(true) {
	op->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IRPrinter::IRPrinter(Instruction* instr, bool printTypes, bool printLabel) :
		tempIndex_(0), currentFunct_(nullptr), unit_(nullptr),
		printTypes_(printTypes), printLabel_(printLabel), printBool_(true) {
	if(instr->ParentFunction() == nullptr) {
		instr->Accept(this);
		return;
	}

	// Used to get the names of the temporaries, if tagged.
	unit_ = instr->ParentFunction()->ParentUnit();

	// Populate the temporary map so that we get the correct names.
	// We walk all instructions and create the temporaries indexes.
	auto instrEnum = instr->ParentFunction()->GetInstructionEnum();

	while(instrEnum.IsValid()) {
		auto currentInstr = instrEnum.Next();
			
		// Walk all operands, and if temporaries are found,
		// add them to the map (if not already added).
		int opCount = currentInstr->SourceOpCount();

		for(int i = 0; i < opCount; i++) {
			Operand* op = currentInstr->GetSourceOp(i);

			if((op == nullptr) || (op->IsTemporary() == false)) {
                continue;
            }

			// Add it only if it's not already added.
			if(tempToIndex_.ContainsKey(op) == false) {
				tempToIndex_.Add(op, ++tempIndex_);
			}
		}

		// Populate only until the instruction to be executed is found.
		if(currentInstr == instr) break;
	}

	// Print the instruction now.
	instr->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::BuildTypenameMap(Unit* unit) {
	// Builds a map used to check if a type was introduced using a typename.
	// If it was, instead of the type we print <typename>.
	// Example: type ABC = record { int32, int8 }
	//          var test <ABC> = {4094, 24}
	for(auto p = unit->Typenames().First(); p; p = p->Next) {
		typeToName_.Add(p->Value->GetType(), p->Value);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendComment(const string& text, int space) {
	sb_.Append(' ', space);
	Append("// ");
	Append(text);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendType(const Type* type, bool allowTypenames, bool allowRecords) {
	// If the type originates from a typename we print <typename>.
	// Example: type ABC = record { int32, int8 }
	//          var test <ABC> = {4094, 24}
	// Note we don't print typenames for simple types (integer, floating, void).
	Symbol* typeName;
	
	if(type == nullptr) {
        Append("/* MISSING */");
        return;
    }

	if(allowTypenames && typeToName_.TryGetValue(type, &typeName) &&
		((type->IsInteger() || type->IsFloating() || type->IsVoid()) == false)) {
		Append("<");
		Append(*typeName->Name());
		Append(">");
	}
	else {
		// Print the type, but only if it's not a record. The problem is that a record
		// can contain a pointer to itself, and we could get a stack overflow.
		if((unit_ && allowRecords) || (type->IsRecord() == false)) {
			const_cast<Type*>(type)->Accept(this);
		}
		else Append("<RECORD>");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendType(Operand* op, bool allowTypenames) {
	if(op) AppendType(op->GetType(), allowTypenames, false);
	else Append("/* MISSING */");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTemp(Temporary* temp) {
	int index;

	// If the temporary has a name tag use it. 
	// Else use a name of the form t1, t2, ...
    if(auto nameTag = temp->GetTag<NameTag>()) {
        Append(nameTag->Name());
		if(temp->IsBoolean() && printBool_) Append(" bool");
		return;
	}

	if(tempToIndex_.TryGetValue(temp, &index) == false) {
		index = ++tempIndex_;
		tempToIndex_.Add(temp, index);
	}

	sb_.AppendFormat(L"t%d", index);
	if(temp->IsBoolean() && printBool_) Append(" bool");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendVisibility(SymbolVisibility visibility) {
	// 'extern' is not printed, because all global symbols are 'extern' by default.
	switch(visibility) {
		case SymbolVisibility::Static:    { Append(" static");    break; }
		case SymbolVisibility::Tentative: { Append(" tentative"); break; }
		default: return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendDllVisibility(DllVisibility dllVis) {
	switch(dllVis) {
		case DllVisibility::Import: { Append(" dllimport"); break; }
		case DllVisibility::Export: { Append(" dllexport"); break; }
		default: return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendAlignment(int alignment) {
	if(alignment != 0) {
		sb_.AppendFormat(L" align(%d)", alignment);
		return true;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendVolatile(bool status) {
	if(status) Append(" volatile");
	return status;
}

bool IRPrinter::AppendRestrict(bool status) {
	if(status) Append(" restrict");
	return status;
}

bool IRPrinter::AppendConstant(bool status) {
	if(status) Append(" const");
	return status;
}

bool IRPrinter::AppendNoWrite(bool status) {
	if(status) Append(" nowrite");
	return status;
}

bool IRPrinter::AppendNoIndirectWrite(bool status) {
	if(status) Append(" noindirwrite");
	return status;
}

bool IRPrinter::AppendNoIndirectRead(bool status) {
	if(status) Append(" noindirread");
	return status;
}

bool IRPrinter::AppendNoRead(bool status) {
	if(status) Append(" noread");
	return status;
}

bool IRPrinter::AppendNoRem(bool status) {
	if(status) Append(" norem");
	return status;
}

bool IRPrinter::AppendNoEscape(bool status) {
	if(status) Append(" noescape");
	return status;
}

bool IRPrinter::AppendNoAddressTaken(bool status) {
    if(status) Append(" noaddrtaken");
    return status;
}

bool IRPrinter::AppendNoState(bool status) {
	if(status) Append(" nostate");
	return status;
}

bool IRPrinter::AppendNoReturn(bool status) {
	if(status) Append(" noreturn");
	return status;
}

bool IRPrinter::AppendUso(bool status) {
	if(status) Append(" uso");
	return status;
}

bool IRPrinter::AppendIntrinsic(bool status) {
	if(status) Append(" intr");
	return status;
}

bool IRPrinter::AppendFromStdlib(bool status) {
	if(status) Append(" stdlib");
	return status;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendCallConvention(CallConventionType conversionType) {
	switch(conversionType) {
		case CallConventionType::Cdecl:    { Append(" cdecl"); break; }
		case CallConventionType::Stdcall:  { Append(" stdcall"); break; }
		case CallConventionType::Fastcall: { Append(" fastcall"); break; }
		default: return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendInline(InlineType inlineType) {
	switch(inlineType) {
		case InlineType::Always: { Append(" forceinline "); break; }
		case InlineType::Never:  { Append(" noinline");     break; }
		default: return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendInitializer(Initializer* initializer) {
	// VALUE adjust(NUMBER)
	// CONVERSION ( VALUE ) adjust(NUMBER)
	bool converted = false;

	if(initializer->Conversion() == InitConversion::PointerToInt) {
		sb_.Append("ptoi(");
		converted = true;
	}
	else if(initializer->Conversion() == InitConversion::IntToPointer) {
		sb_.Append("itop(");
		converted = true;
	}
	else if(initializer->Conversion() == InitConversion::PointerToPointer) {
		sb_.Append("ptop(");
		converted = true;
	}

	initializer->Value()->Accept(this);

	// Append the type to which the value was converted.
	if(converted) {
		AppendComma();
		AppendSpace();
		AppendType(initializer->ConversionType(), true, false);
		AppendCloseParen();
	}

	if(initializer->Adjustment() != 0) {
		sb_.AppendFormat(L" adjust(%d)", initializer->Adjustment());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendInitializer(InitializerList* initList) {
	// { VALUE1, VALUE2, ... }
	AppendOpenCurly();

	for(int i = 0; i < initList->Count(); i++) {
		if(i > 0) {
			AppendComma();
			AppendSpace();
		}

		Initializer* initializer = (*initList)[i];
		if(initializer->IsInitializerList()) {
			AppendInitializer(static_cast<InitializerList*>(initializer));
		}
		else AppendInitializer(initializer);
	}

	AppendCloseCurly();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendCompare(CmpInstrBase* instr) {
	// RESULT = OPCODE ORDER, LEFT-OPERAND, RIGHT-OPERAND
	if(instr->ResultOp()) {
		instr->ResultOp()->Accept(this);
		AppendSpace();
		AppendEqual();
		AppendSpace();
	}

	Append(instr->OpcodeString());
	AppendSpace();

	switch(instr->Order()) {
		case OrderType::Less:           { Append("lt");  break; }
		case OrderType::LessOrEqual:    { Append("lte"); break; }
		case OrderType::Greater:        { Append("gt");  break; }
		case OrderType::GreaterOrEqual: { Append("gte"); break; }
		case OrderType::Equal:          { Append("eq");  break; }
		case OrderType::NotEqual:       { Append("neq"); break; }
	}

	AppendSpace();
	TryAppend(instr->LeftOp());

	AppendComma();
	AppendSpace();
	TryAppend(instr->RightOp());

	if(printTypes_) {
		AppendComment();
		AppendType(instr->LeftOp());
		Append(", ");
		AppendType(instr->RightOp());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(IntegerType* type) {
	switch(type->GetSubtype()) {
		case IRIntegerKind::Int8:  { Append("int8");  break; }
		case IRIntegerKind::Int16: { Append("int16"); break; }
		case IRIntegerKind::Int32: { Append("int32"); break; }
		case IRIntegerKind::Int64: { Append("int64"); break; }
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(FloatingType* type) {
	if(type->IsFloat()) Append("float");
	else Append("double");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(VoidType* type) {
	Append("void");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(PointerType* type) {
	// First print the pointee, then the *
	// This also handles multiple pointers (pointer to pointer, etc.).
	AppendType(type->PointeeType(), true, false);
	Append("*");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(ArrayType* type) {
	// [SIZE TYPE]. Between the two components there is a space character.
	Append("[");
	sb_.AppendFormat(L"%d ", type->Size());
	AppendType(type->ElementType(), true, false);
	Append("]");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(FunctionType* type) {
	// funct (PARAM_1, PARAM_2, ...) : RETURN_TYPE
	if(printFunctTypeArrows_) AppendOpenArrow();
	Append("funct ");
	AppendOpenParen(); // (

	for(int i = 0; i < type->ParameterCount(); i++) {
		if(i > 0) {
			AppendComma();
			AppendSpace();
		}

		AppendType(type->Parameters()[i], true, false);
	}

	// ... is appended at the end of the parameter list if the function is varargs.
	if(type->IsVarargs()) {
		Append(", ...");
	}

	AppendCloseParen();
	AppendSpace();
	AppendColon();
	AppendSpace();
	AppendType(type->ReturnType(), true, false);
	if(printFunctTypeArrows_) AppendCloseArrow();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(RecordType* type) {
	// record {
	//		FIELD1-TYPE offset(OFFSET_1-VALUE),
	//      ...
	//		FIELDN-TYPE offset(OFFSET_N-VALUE)
	// }
	Append("record ");
	AppendOpenCurly();
	AppendLine();

	for(int i = 0; i < type->FieldCount(); i++) {
		auto field = type->Fields()[i];

		AppendTab();
		AppendType(field.FieldType, true, false /* allowRecords */);
		sb_.AppendFormat(L" offset(%d)", field.FieldOffset);

		if(i != (type->FieldCount() - 1)) {
			AppendComma();
		}

		AppendLine();
	}

	AppendCloseCurly();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(Symbol* symbol) {
	// This is a type name.
	// type IDENTIFIER = TYPE
	Append("type ");
	TryAppend(symbol->Name());
	Append(" = ");
	AppendType(symbol->GetType(), false /* allowTypenames */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(Variable* variable) {
	// var IDENTIFIER TYPE [ATTRIBUTE_1 ... ATTRIBUTE_N]
	Append("var ");

	if(variable->HasName()) {
		TryAppend(variable->Name());
		AppendSpace();
	}

	AppendType(variable->GetType());

	// Append all possible attributes.
	AppendAlignment(variable->Alignment());
	AppendRestrict(variable->IsRestrict());
    AppendNoWrite(variable->IsNoWrite());
    AppendNoRead(variable->IsNoRead());
    AppendNoEscape(variable->IsNoEscape());
    AppendNoAddressTaken(variable->IsAddressNotTaken());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(GlobalVariable* variable) {
	// var IDENTIFIER TYPE [ATTRIBUTE_1 ... ATTRIBUTE_N] INITIALIZER
	// INITIALIZER -> = INIT-VALUE | = { INIT-VALUE1, ..., INIT-VALUEN } | = 0 | EPS
	Append("var ");
	TryAppend(variable->Name());
	AppendSpace();
	AppendType(variable->GetType());

	// Append all possible attributes.
	AppendAlignment(variable->Alignment());
	AppendVisibility(variable->Visibility());
	AppendDllVisibility(variable->GetDllVisibility());
    AppendNoAddressTaken(variable->IsAddressNotTaken());
	AppendConstant(variable->IsConstant());
	
	// Append the initializer.
	if(variable->HasZeroInitializer()) {
		AppendSpace();
		Append(" = 0");
	}
	else if(variable->HasInitializer()) {
		inInitializer_ = true;
		Append(" = ");

		if(variable->HasSimpleInitializer()) {
			AppendInitializer(variable->GetInitializer());
		}
		else AppendInitializer(variable->GetInitializerList());

		inInitializer_ = false;
	}

    // Append any associated tag.
    AppendTags(variable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(Block* block) {
	// label NAME: // preds = {a,b,...}
	//		INSTR_1
	//      ...
	//      INSTR_N
	Append("label ");
	TryAppend(block->Name());
	AppendColon();

	// Append the names of the predecessors.
    AppendComment("preds = {");
	StringBuilder predecessorStr;

	block->ForEachPredecessor([&predecessorStr, block]
                              (Block* predecessorBlock, int index) -> bool {
		if(predecessorBlock->HasName()) {
			predecessorStr.Append(*predecessorBlock->Name());

			if(index != (block->PredecessorCount() - 1)) {
				predecessorStr.Append(", ");
			}
		}
		
		return true;
	});

	Append(predecessorStr.ToString());
    AppendCloseCurly();

    // Append any associated tag.
    AppendTags(block, false);
	AppendLine();

	for(auto instr = block->FirstInstruction(); instr; 
        instr = instr ->NextInstruction()) {
		AppendTab();
		instr->Accept(this);
		AppendLine();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(Function* function) {
	// funct [ATTRIBUTE1 ...] NAME (PARAM_1, PARAM_2, ...) : RETURN-TYPE {
	// BLOCK_1
	// ...
	// BLOCK_N
	// }
	// Temporaries start from 0 for each function.
	ResetTempInfo();
	currentFunct_ = function;
	Append("funct");

	if(function->IsDefinition() == false) {
		Append(" decl");
	}

	// Append all possible attributes.
    AppendIntrinsic(function->IsIntrinsic());
	AppendAlignment(function->Alignment());
	AppendVisibility(function->Visibility());
	AppendDllVisibility(function->GetDllVisibility());
	AppendCallConvention(function->CallConvention());
	AppendInline(function->Inline());
    AppendFromStdlib(function->IsFromStdlib());
	AppendNoIndirectWrite(function->IsNoIndirectWrite());
	AppendNoIndirectRead(function->IsNoIndirectRead());
    AppendNoState(function->IsNoState());
    AppendNoAddressTaken(function->IsAddressNotTaken());

	// Append the name and parameters.
	AppendSpace();
	TryAppend(function->Name());
	AppendOpenParen();

	for(int i = 0; i < function->ParameterCount(); i++) {
		if(i > 0) {
			AppendComma();
			AppendSpace();
		}

		function->Parameters()[i]->Accept(this);
	}

	// ... is appended at the end of the parameter list if the function is varargs.
	if(function->IsVarargs()) {
		Append(", ...");
	}

	AppendCloseParen();

	// Append the return type.
	AppendSpace();
	AppendColon();
	AppendSpace();
	AppendType(function->ReturnType());

    // Append any associated tags.
    AppendSpace();
    AppendOpenCurly();
    AppendLine();
    bool hadTags = AppendTags(function);

    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->HasTags()) {
            if(i > 0 || hadTags) {
                AppendLine();
            }

            AppendComment(*parameterVariable->Name() + ": ");
            AppendTags(parameterVariable);
            hadTags = true;
        }
    }

    if(hadTags) {
        AppendLine();
    }

	// If it's only a declaration we stop here.
	if(function->IsDefinition() == false) {
        return;
    }

	// Append all declared variables.
	IRPrinter* printer = this;

	function->Variables().ForEach([printer](Symbol* symbol) -> bool {
		// If the variable is a parameter don't show it again.
		auto variable = symbol->As<Variable>();
		if(variable->IsParameter() == false) {
			printer->AppendTab();
			symbol->Accept(printer);
			printer->AppendLine();
		}

		return true;
	});

	if(function->VariableCount() > 0) {
		// At least a variable has been added, leave a blank line.
		AppendLine();
	}

	for(auto block = function->FirstBlock(); block; block = block->NextBlock()) {
		if(block->Previous()) AppendLine();
		block->Accept(this);
	}

	AppendCloseCurly();
	currentFunct_ = nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(Operand* op) {
	if(op->IsVariableReference() || op->IsGlobalVariableRef()) {
		Append("&");
		TryAppendName(op->GetSymbol());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(Temporary* op) {
	AppendTemp(op);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(FunctionReference* op) {
	TryAppendName(op->Target());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(BlockReference* op) {
	// If we're in a function we print only the name of the block.
	// In an initializer we use: label FUNCT-NAME.BLOCK-NAME
	if(inInitializer_ && printLabel_) {
		Append("label ");
		TryAppendName(op->ParentFunction());
		AppendDot();
		TryAppendName(op->Target());
	}
	else TryAppendName(op->Target());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(Parameter* op) {
	// @variableName
	Append("@");
    auto variable = op->GetVariable();
    
    if(variable->HasName()) {
        Append(*variable->Name());
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(IntConstant* op) {
	sb_.AppendFormat(L"%lld", op->Value());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(FloatConstant* op) {
	sb_.AppendFormat(L"%f", op->Value());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(StringConstant* op) {
	Append("\"");
	StringBuilder builder(op->Value().Length());
	auto& value = op->Value();

	for(int i = 0; i < value.Length(); i++) {
		wchar_t ch = value[i];

		switch(ch) {
			default:   { builder.Append(ch);     break; }
			case '\0': { builder.Append("\\0");  break; }
			case '\\': { builder.Append("\\\\"); break; }
			case '\'': { builder.Append("\\\'"); break; }
			case '"':  { builder.Append("\\\""); break; }
			case '\?': { builder.Append("\\?");  break; }
			case '\a': { builder.Append("\\a");  break; }
			case '\b': { builder.Append("\\b");  break; }
			case '\f': { builder.Append("\\f");  break; }
			case '\n': { builder.Append("\\n");  break; }
			case '\r': { builder.Append("\\r");  break; }
			case '\t': { builder.Append("\\t");  break; }
			case '\v': { builder.Append("\\v");  break; }
		}
	}

	Append(builder.ToString());
	Append("\"");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(NullConstant* op) {
	sb_.Append("nullptr");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(UndefinedConstant* op) {
	sb_.Append("undef");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(ArithmeticInstr* instr) {
	// RESULT = OPCODE SOURCE_1, SOURCE_2 attributeList
	if(instr->ResultOp()) {
		printBool_ = true;
		instr->ResultOp()->Accept(this);
		printBool_ = false;
		AppendSpace();
		AppendEqual();
		AppendSpace();
	}

	Append(instr->OpcodeString());
	AppendSpace();
	TryAppend(instr->LeftOp());
	AppendComma();
	AppendSpace();
	TryAppend(instr->RightOp());

	// Append the attributes.
	if(instr->IsFloatArithmetic()) {
		if(instr->GetFPMode() == FloatMode::Safe) {
			Append(" fpsafe");
		}
		else if(instr->GetFPMode() == FloatMode::Fast) {
			Append(" fpfast");
		}
	}
	else {
		AppendUso(instr->HasUndefinedOverflow());
        AppendNoRem(instr->HasNoRemainder());
	}

    bool hasComment = false;

	if(printTypes_) {
        hasComment = true;
		AppendComment();
		AppendType(instr->LeftOp());
		Append(", ");
		AppendType(instr->RightOp());
	}

    // Append any associated tags.
    hasComment |= AppendTags(instr, hasComment == false);
    AppendTags(instr->ResultOp(), hasComment == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(ConversionInstr* instr) {
	// RESULT = OPCODE TARGET, CAST-TYPE
	if(instr->ResultOp()) {
		printBool_ = true;
		instr->ResultOp()->Accept(this);
		printBool_ = false;
		AppendSpace();
		AppendEqual();
		AppendSpace();
	}

	Append(instr->OpcodeString());
	AppendSpace();
	TryAppend(instr->TargetOp());
	AppendComma();
	AppendSpace();
	AppendType(instr->CastType());
    bool hasComment = false;

	if(printTypes_) {
        hasComment = true;
		AppendComment();
		AppendType(instr->TargetOp());
		Append(" -> ");
		AppendType(instr->CastType());
	}

    // Append any associated tags.
    hasComment |= AppendTags(instr, hasComment == false);
    AppendTags(instr->ResultOp(), hasComment == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(LogicalInstr* instr) {
	// RESULT = OPCODE SOURCE_1, SOURCE_2
	if(instr->ResultOp()) {
		printBool_ = true;
		instr->ResultOp()->Accept(this);
		printBool_ = false;
		AppendSpace();
		AppendEqual();
		AppendSpace();
	}

	Append(instr->OpcodeString());
	AppendSpace();
	TryAppend(instr->LeftOp());
	AppendComma();
	AppendSpace();
	TryAppend(instr->RightOp());
    bool hasComment = false;

	if(printTypes_) {
        hasComment = true;
		AppendComment();
		AppendType(instr->LeftOp());
		Append(", ");
		AppendType(instr->RightOp());
	}

    // Append any associated tags.
    hasComment |= AppendTags(instr, hasComment == false);
    AppendTags(instr->ResultOp(), hasComment == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(LoadInstr* instr) {
	// RESULT = load SOURCE [volatile]
	if(instr->ResultOp()) {
		printBool_ = true;
		instr->ResultOp()->Accept(this);
		printBool_ = false;
		AppendSpace();
		AppendEqual();
		AppendSpace();
	}

	Append(instr->OpcodeString());
	AppendSpace();
	TryAppend(instr->SourceOp());

	// Add a notification if we load from a 'volatile' variable.
	if(instr->IsVolatile()) Append(" volatile");
    bool hasComment = false;

	if(printTypes_) {
        hasComment = true;
		AppendComment();
		AppendType(instr->SourceOp());

		if(instr->ResultOp()) {
			Append(" -> ");
			AppendType(instr->ResultOp());
		}
	}

    // Append any associated tags.
    hasComment |= AppendTags(instr, hasComment == false);
    AppendTags(instr->ResultOp(), hasComment == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(StoreInstr* instr) {
	// store DEST, OPERAND [volatile]
	Append(instr->OpcodeString());
	AppendSpace();

	TryAppend(instr->DestinationOp());
	AppendComma();
	AppendSpace();
	TryAppend(instr->SourceOp());
	
	// Add a notification if we load from a 'volatile' variable.
	if(instr->IsVolatile()) Append(" volatile");
    bool hasComment = false;

	if(printTypes_) {
        hasComment = true;
		AppendComment();

		AppendType(instr->DestinationOp());
		Append(" <- ");
		AppendType(instr->SourceOp());
	}

    // Append any associated tags.
    AppendTags(instr, hasComment == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(AddressInstr* instr) {
	// RESULT = addr TARGET, INDEX-OPERAND
	if(instr->ResultOp()) {
		printBool_ = true;
		instr->ResultOp()->Accept(this);
		printBool_ = false;
		AppendSpace();
		AppendEqual();
		AppendSpace();
	}

	Append(instr->OpcodeString());
	AppendSpace();
	TryAppend(instr->BaseOp());
	AppendComma();
	AppendSpace();
	TryAppend(instr->IndexOp());
    bool hasComment = false;


	if(printTypes_) {
        hasComment = true;
		AppendComment();
		AppendType(instr->BaseOp());
		AppendComma();
		AppendSpace();
		AppendType(instr->IndexOp());

		if(instr->ResultOp()) {
			Append(" -> ");
			AppendType(instr->ResultOp());
		}
	}

    if(instr->IsField()) {
        if(auto nameTag = instr->GetTag<NameTag>()) {
            hasComment = true;
            AppendComment("Field name: " + nameTag->Name());
        }
    }

    // Append any associated tags.
    hasComment |= AppendTags(instr, hasComment == false);
    AppendTags(instr->ResultOp(), hasComment == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(IndexInstr* instr) {
	Visit(static_cast<AddressInstr*>(instr));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(FieldInstr* instr) {
	Visit(static_cast<AddressInstr*>(instr));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(CmpInstr* instr) {
	AppendCompare(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(UcmpInstr* instr) {
	AppendCompare(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(FcmpInstr* instr) {
	AppendCompare(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(IfInstr* instr) {
	// if OPERAND, LABEL_1, LABEL_2
	Append(instr->OpcodeString());
	AppendSpace();
	TryAppend(instr->ConditionOp());

	AppendComma();
	AppendSpace();
	TryAppend(instr->TrueTargetOp());

	AppendComma();
	AppendSpace();
	TryAppend(instr->FalseTargetOp());

    // Append any associated tags.
    AppendTags(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(GotoInstr* instr) {
	// goto LABEL
	Append(instr->OpcodeString());
	AppendSpace();
    if(instr->TargetOp()) instr->TargetOp()->Accept(this);
    else Append("/* MISSING */");

	if(instr->HasUnknownOrigin() == false) {
		AppendComment();
		AppendSpace();

		switch(instr->GetGotoOrigin()) {
			case GotoOrigin::Break:    { Append("break");    break; }
			case GotoOrigin::Continue: { Append("continue"); break; }
			case GotoOrigin::Goto:     { Append("goto");     break; }
			case GotoOrigin::Loop:     { Append("loop");     break; }
		}
	}

    // Append any associated tags.
    AppendTags(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(CallInstr* instr) {
	// RESULT = call [ATTRIBUTE_1 ... ATTRIBUTE_N] TARGET, ARG_1, ARG_2, ...
	if(instr->ResultOp()) {
		printBool_ = true;
		instr->ResultOp()->Accept(this);
		printBool_ = false;
		AppendSpace();
		AppendEqual();
		AppendSpace();
	}

	Append(instr->OpcodeString());
	AppendSpace();

	// Append the calling convention and 'intrinsic' if the target is an intrinsic.
	bool needsSpace = false;
    bool hasComment = false;

	if(instr->HasOverridenCallConvention()) {
		AppendCallConvention(instr->CallConvention());
		AppendSpace();
		needsSpace = true;
	}

	if(instr->IsIntrinsic()) {
		if(needsSpace) AppendSpace();
		Append("intrinsic");
		AppendSpace();
	}

	// Append the target name and the arguments.
	instr->TargetOp()->Accept(this);

	for(int i = 0; i < instr->ArgumentCount(); i++) {
		AppendComma();
		AppendSpace();
		(*instr->Arguments())[i]->Accept(this);
	}

	if(printTypes_) {
        hasComment = true;
		AppendComment();
		AppendType(instr->CalledFunctionType()->ReturnType());
	}

    // Append any associated tags.
    hasComment |= AppendTags(instr, hasComment == false);
    AppendTags(instr->ResultOp(), hasComment == false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(ReturnInstr* instr) {
	// ret OPERAND
	Append(instr->OpcodeString());
	AppendSpace();

	if(instr->IsVoid() == false) {
		instr->ReturnedOp()->Accept(this);

		if(printTypes_) {
			AppendComment();
			AppendType(instr->ReturnedOp());
		}
	}
	else Append("void");

    // Append any associated tags.
    AppendTags(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(SwitchInstr* instr) {
	// switch OPERAND, CONTINUATION_BLOCK { 
	//		NUMBER_1 : BLOCK_NAME_1, 
	//      ...,
	//      NUMBER_N : BLOCK_NAME_N,
	//      [default: BLOCK_NAME]
	// }
	Append(instr->OpcodeString());
	AppendSpace();

	if(instr->ConditionOp()) {
		instr->ConditionOp()->Accept(this);
	}

	// Print the case list.
	AppendSpace();
	AppendOpenCurly();
	AppendLine();
	auto& list = instr->CaseList();

	for(int i = 0; i < list.Count(); i++) {
		if(i > 0) {
			AppendComma();
			AppendLine();
		}

		AppendTab(2);
		sb_.AppendFormat(L"%lld", list[i].Value);
		AppendColon();
		AppendSpace();

		if(list[i].Target) {
			list[i].Target->Accept(this);
		}
	}

	AppendComma();
	AppendLine();
	AppendTab(2);
	Append("default: ");

	if(instr->DefaultTargetOp()) {
		instr->DefaultTargetOp()->Accept(this);
	}

	AppendLine();
	AppendTab();
	AppendCloseCurly();

    // Append any associated tags.
    AppendTags(instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(PhiInstr* instr) {
	// phiInstr -> value = phi phiOpList
	// phiOpList -> phiOp , phiOp | phiOp
	// phiOp -> { value , identifier }
	if(instr->ResultOp()) {
		printBool_ = true;
		instr->ResultOp()->Accept(this);
		printBool_ = false;
		AppendSpace();
		AppendEqual();
		AppendSpace();
	}

	Append(instr->OpcodeString());
	AppendSpace();

	for(int i = 0; i < instr->OperandCount(); i++) {
		AppendOpenCurly();
		instr->GetOperand(i)->Accept(this);
		AppendComma();
		instr->GetOperandBlockReference(i)->Accept(this);
		AppendCloseCurly();
		
		if(i < (instr->OperandCount() - 1)) {
			AppendComma();
			AppendSpace();
		}
	}

    // Append any associated tags.
    AppendComment();
    AppendTags(instr);
    AppendTags(instr->ResultOp(), false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Visit(QuestionInstr* instr) {
    // result = quest conditionOp, trueOp, falseOp
    if(instr->ResultOp()) {
        printBool_ = true;
        instr->ResultOp()->Accept(this);
        printBool_ = false;
        AppendSpace();
        AppendEqual();
        AppendSpace();
    }

    Append(instr->OpcodeString());
    AppendSpace();
    TryAppend(instr->ConditionOp());
    AppendComma();
    AppendSpace();
    TryAppend(instr->TrueOp());
    AppendComma();
    AppendSpace();
    TryAppend(instr->FalseOp());

    // Append any associated tags.
    AppendTags(instr);
    AppendTags(instr->ResultOp(), false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendTags(Tagged<Tag>* tagged, bool startComment) {
    if(tagged == nullptr) {
        return false;
    }

    // Don't display name tags.
    if(tagged->TagCount() > (tagged->HasTag<NameTag>() ? 1 : 0)) {
        string tagName;

        if(startComment) {
            AppendComment("Tags = [", 1);
        }
        else Append(", Tags = [");

        tagged->ForEachTag([this](Tag* tag) -> bool {
            AppendTag(tag);
            return true;
        });

        Append(" ]");
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendTags(Tagged<FunctionTag>* tagged, bool startComment) {
    if(tagged == nullptr) {
        return false;
    }

    // Don't display name tags.
    if(tagged->TagCount() > (tagged->HasTag<NameTag>() ? 1 : 0)) {
        if(startComment) {
            AppendComment("Function Tags = [", 1);
        }
        else Append(", Function Tags = [");

        tagged->ForEachTag([this](FunctionTag* tag) -> bool {
            AppendTag(static_cast<Tag*>(tag));
            return true;
        });

        Append(" ]");
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendTags(Tagged<BlockTag>* tagged, bool startComment) {
    if(tagged == nullptr) {
        return false;
    }

    // Don't display name tags.
    if(tagged->TagCount() > (tagged->HasTag<NameTag>() ? 1 : 0)) {
        if(startComment) {
            AppendComment("Block Tags= [", 1);
        }
        else Append(", Block Tags = [");

        tagged->ForEachTag([this](BlockTag* tag) -> bool {
            AppendTag(static_cast<Tag*>(tag));
            return true;
        });

        Append(" ]");
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRPrinter::AppendTags(Tagged<InstructionTag>* tagged, bool startComment) {
    if(tagged == nullptr) {
        return false;
    }

    // Don't display name tags.
    if(tagged->TagCount() > (tagged->HasTag<NameTag>() ? 1 : 0)) {
        if(startComment) {
            AppendComment("Instruction Tags = [", 1);
        }
        else Append(", Instruction Tags = [");

        tagged->ForEachTag([this](InstructionTag* tag) -> bool {
            AppendTag(static_cast<Tag*>(tag));
            return true;
        });

        Append(" ]");
        return true;
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Tag* tag) {
    DebugValidator::IsNotNull(tag);

    if(auto globalConstants = tag->As<Analysis::GlobalConstantsTag>()) {
        AppendTag(globalConstants);
    }
    else if(auto globalUsers = tag->As<Analysis::GlobalUsersTag>()) {
        AppendTag(globalUsers);
    }
    else if(auto knownBits = tag->As<Analysis::KnownBitsTag>()) {
        AppendTag(knownBits);
    }
    else if(auto typeClass = tag->As<Analysis::TypeClassTag>()) {
        AppendTag(typeClass);
    }
    else if(auto cFamily = tag->As<Analysis::CFamilyTag>()) {
        AppendTag(cFamily);
    }
    else if(auto globalEffects = tag->As<Analysis::GlobalSideEffectsTag>()) {
        AppendTag(globalEffects);
    }
    else if(auto parameterConsts = tag->As<Analysis::ParameterConstantsTag>()) {
        AppendTag(parameterConsts);
    }
    else if(auto pointerAlias = tag->As<Analysis::ParameterAliasTag>()) {
        AppendTag(pointerAlias);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Analysis::GlobalConstantsTag* tag) {
    Append(" Constants = {");

    for(int i = 0; i < tag->ConstantCount(); i++) {
        tag->GetConstant(i)->Accept(this);
        Append(string::Format(L" (%.2f%%)", tag->GetProbability(i) * 100));

        if(i != (tag->ConstantCount() - 1)) {
            AppendComma();
            AppendSpace();
        }
    }

    if(tag->HasOnlyConstants()) {
        Append(", Has Only Constants");
    }

    Append("}");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Analysis::GlobalUsersTag* tag) {
    Append(" Users = {");
    Append(" Functions = " + string::Format(L"%d", tag->FunctionUserCount()));
    Append(", Inits = " + string::Format(L"%d", tag->InitializerUserCount()));
    
    if(tag->AreReadPositionsKnown()) {
        Append(", Reads = [");

        tag->ReadPositions()->ForEachSetBit([this](int index) -> bool {
            Append(string::Format(L"%d", index) + ", ");
            return true;
        });

        Append("]");
    }

    if(tag->HasUnknownPositionWrite()) {
        Append(", Unknown Reads");
    }
    if(tag->HasUnknownPositionWrite()) {
        Append(", Unknown Writes");
    }
    if(tag->HasNoReads()) {
        Append(", No Reads");
    }

    Append("}");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Analysis::KnownBitsTag* tag) {
    Append(" Known Bits = {");

    if(tag->IsAllUnknownBits()) {
        Append(" All Unknown");
    }
    else {
        if(tag->HasZeroBitInfo()) {
            Append(" Zero Bits = ");

            if(tag->ZeroBits() == 0) {
                Append("None");
            }
            else {
                for(int i = 63; i >= 0; i--) {
                    Append(tag->IsZeroBit(i) ? "1" : "0");
                }
            }
        }

        if(tag->HasOneBitInfo()) {
            Append(" One Bits = ");

            if(tag->OneBits() == 0) {
                Append("None");            
            }
            else {
                for(int i = 63; i >= 0; i--) {
                    Append(tag->IsOneBit(i) ? "1" : "0");
                }
            }
        }
    }

    Append(" }");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Analysis::TypeClassTag* tag) {
    Append(" Type Class = {");

    if(tag->IsUniversalType()) {
        Append("Universal");
    }
    else Append(string::Format(L"%X", (void*)tag));

    Append("}");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Analysis::CFamilyTag* tag) {
    Append(" C Family = {");

    if(tag->HasAlloca()) {
        Append(" HasAlloca");
    }

    if(tag->HasLongjmp()) {
        Append(" HasLongjmp");
    }

    if(tag->HasSetjmp()) {
        Append(" HasSetjmp");
    }

    if(tag->IsAllocLike()) {
        Append(" IsAllocLike");
    }

    if(tag->IsCheckedAllocLike()) {
        Append(" IsCheckedAllocLike");
    }

    Append("}");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Analysis::GlobalSideEffectsTag* tag) {
    Append(" Global Effects = { ");

    if(unit_) {
        for(auto variable = unit_->Variables().First();
            variable; variable = variable->Next) {
            if(tag->ReadsOrWritesVariable(variable->Value)) {
                Append(*variable->Value->Name());
                AppendOpenParen();

                if(tag->ReadsVariable(variable->Value)) {
                    Append("r");
                }

                if(tag->AlwaysWritesVariable(variable->Value)) {
                    Append("wa");
                }
                else if(tag->WritesVariable(variable->Value)) {
                    Append("w");
                }

                AppendCloseParen();
                AppendSpace();
            }
        }
    }
    else Append("No Unit");

    if(tag->HasUnknownEffects()) {
        Append("Unknown ");
    }

    Append("}");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Analysis::ParameterConstantsTag* tag) {
    Append(" Parameter Constants = { ");

    if(tag->IsRange()) {
        Append(string::Format(L"Range = [%d, %d] ",
               tag->GetRange().Low.Constant, tag->GetRange().High.Constant));
    }
    else tag->ForEachConstant([this](Constant* c, FunctionReference* f) -> bool {
        c->Accept(this);
        Append(" (");
        Append(*f->Target()->Name());
        Append("), ");
        return true;
    });

    if(tag->IsNotZero()) {
        Append("Not Zero, ");
    }

    if(tag->IsSafeToUse() == false) {
        Append("Not Safe ");
    }

    Append("} ");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::AppendTag(Analysis::ParameterAliasTag* tag) {
    Append(" Parameter Alias = { ");

    if(tag->AliasesAllParameters()) {
        Append("All ");
    }
    else if(tag->DoesNotAliasParameters()) {
        Append("None, ");
    }
    else tag->ForEachAliasedParameter([this](int index) -> bool {
        Append(string::Format(L"%d, ", index));
        return true;
    });

	if(tag->MayPointToGlobalVariables()) {
		Append("Points to Global");
	}

    Append("} ");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRPrinter::Dump() const {
    ObjectDumper(ToString(), "IR Printer").Dump();
}

} // namespace IR