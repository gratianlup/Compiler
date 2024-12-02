// IRPrinter.hpp
// Copyright (c) Lup Gratian
//
// Implements the IR printer which creates a text representation
// of the IR and writes it to a file/string.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_PRINTER_HPP
#define PC_IR_PRINTER_HPP

#include "Instructions.hpp"
#include "Symbols.hpp"
#include "Unit.hpp"
#include "Operand.hpp"
#include "Temporary.hpp"
#include "Constants.hpp"
#include "References.hpp"
#include "Parameter.hpp"
#include "Visitor.hpp"
#include "Tags.hpp"
#include "Tagged.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace Base;

// Forward declarations.
namespace Analysis {
    class GlobalConstantsTag;
    class GlobalUsersTag;
    class KnownBitsTag;
    class TypeClassTag;
    class CFamilyTag;
    class GlobalSideEffectsTag;
    class ParameterConstantsTag;
    class ParameterAliasTag;
}

namespace IR {

class IRPrinter : public Visitor {
private:
	StringBuilder sb_;
	bool printTypes_;
	bool inInitializer_;
	bool printLabel_;
	bool printFunctTypeArrows_;
	int tempIndex_;
	Unit* unit_;
	Dictionary<Operand*, int> tempToIndex_;       // Map with all temporaries in a function.
	Dictionary<const Type*, Symbol*> typeToName_; // Mapping from type to type name.
	Function* currentFunct_;
	bool printBool_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Builds a map used to print types defined by the user.
	void BuildTypenameMap(Unit* unit);

	// Resets the index of the next temporary when entering a new function.
	void ResetTempInfo() {
		tempIndex_ = 0;
		tempToIndex_.Clear();
	}

	// Appends the specified text.
	void Append(const string& text) { 
		sb_.Append(text); 
	}

	// Appends the specified object, or "MISSING" if the object is nullptr.
	void TryAppend(Visitable* v) {
		if(v) v->Accept(this);
		else sb_.Append("/* MISSING */");
	}

	void TryAppend(string* s) {
		if(s) sb_.Append(*s);
		else sb_.Append("/* MISSING */");
	}

	void TryAppendName(Symbol* s) {
		if(s && s->HasName()) sb_.Append(*s->Name());
		else sb_.Append("/* MISSING */");
	}

	// Helper methods for constructing common text fragments.
	void AppendComma()          { sb_.Append(",");     }
	void AppendColon()          { sb_.Append(":");     }
	void AppendOpenParen()      { sb_.Append("(");     }
	void AppendCloseParen()     { sb_.Append(")");     }
	void AppendOpenCurly()      { sb_.Append("{");     }
	void AppendCloseCurly()     { sb_.Append("}");     }
	void AppendOpenArrow()      { sb_.Append("<");     }
	void AppendCloseArrow()     { sb_.Append(">");     }
	void AppendDot()            { sb_.Append(".");     }
	void AppendTab(int n = 1)   { sb_.Append('\t', n); }
	void AppendSpace(int n = 1) { sb_.Append(' ',  n); }
	void AppendEqual()          { sb_.Append("=");     }
	void AppendLine()           { sb_.AppendLine();    }

	// Appends a comment line.
	void AppendComment(const string& text, int space = 1);

	void AppendComment() {
		AppendComment("", 1);
	}

	// Appends the string representation of the specified type.
	void AppendType(const Type* type, bool allowTypenames = true,
                    bool allowRecords = true);

	// Appends the string representation of the type associated with the specified operand.
	void AppendType(Operand* op, bool allowTypenames = true);

	// Appends the name of the specified temporary. The names have the form
	// 't1', 't2', ..., 'tN' and are distinct for each function.
	void AppendTemp(Temporary* temp);

	// Methods for appending attribute information.
	bool AppendVisibility(SymbolVisibility visibility);
	bool AppendDllVisibility(DllVisibility dllVis);
	bool AppendAlignment(int alignment);
	bool AppendVolatile(bool status);
	bool AppendRestrict(bool status);
	bool AppendConstant(bool status);
    bool AppendNoWrite(bool status);
	bool AppendNoIndirectWrite(bool status);
	bool AppendNoIndirectRead(bool status);
    bool AppendNoRead(bool status);
    bool AppendNoEscape(bool status);
    bool AppendNoAddressTaken(bool status);
    bool AppendIntrinsic(bool status);
    bool AppendFromStdlib(bool status);
    bool AppendNoState(bool status);
    bool AppendNoRem(bool status);
    bool AppendUso(bool status);
    bool AppendNoReturn(bool status);
	bool AppendCallConvention(CallConventionType conversionType);
	bool AppendInline(InlineType inlineType);
	void AppendInitializer(Initializer* initializer);
	void AppendInitializer(InitializerList* initializer);
    bool AppendTags(Tagged<Tag>* tagged, bool startComment = true);
    bool AppendTags(Tagged<FunctionTag>* tagged, bool startComment = true);
    bool AppendTags(Tagged<BlockTag>* tagged, bool startComment = true);
    bool AppendTags(Tagged<InstructionTag>* tagged, bool startComment = true);
    void AppendTag(Tag* tag);
    void AppendTag(Analysis::GlobalConstantsTag* tag);
    void AppendTag(Analysis::KnownBitsTag* tag);
    void AppendTag(Analysis::TypeClassTag* tag);
    void AppendTag(Analysis::CFamilyTag* tag);
    void AppendTag(Analysis::GlobalUsersTag* tag);
    void AppendTag(Analysis::GlobalSideEffectsTag* tag);
    void AppendTag(Analysis::ParameterConstantsTag* tag);
    void AppendTag(Analysis::ParameterAliasTag* tag);
    
	// Appends information about a comparison instruction.
	void AppendCompare(CmpInstrBase* instr);

public:
	IRPrinter(Unit* unit, bool printTypes = false);
	IRPrinter(const Type* type);
	IRPrinter(Symbol* symbol, bool printTypes = false, bool printLabel = false);
	IRPrinter(Operand* op, bool printTypes = false, bool printLabel = false);
	IRPrinter(Instruction* instr, bool printTypes = false, bool printLabel = false);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual void Visit(IntegerType*  type) override;
	virtual void Visit(FloatingType* type) override;
	virtual void Visit(VoidType*     type) override;
	virtual void Visit(PointerType*  type) override;
	virtual void Visit(ArrayType*    type) override;
	virtual void Visit(FunctionType* type) override;
	virtual void Visit(RecordType*   type) override;

	virtual void Visit(Symbol*         symbol) override;
	virtual void Visit(Variable*       symbol) override;
	virtual void Visit(GlobalVariable* symbol) override;
	virtual void Visit(Block*          symbol) override;
	virtual void Visit(Function*       symbol) override;

	virtual void Visit(Operand*           op) override;
	virtual void Visit(Temporary*         op) override;
	virtual void Visit(FunctionReference* op) override;
	virtual void Visit(BlockReference*    op) override;
	virtual void Visit(Parameter*         op) override;
	virtual void Visit(IntConstant*       op) override;
	virtual void Visit(FloatConstant*     op) override;
	virtual void Visit(StringConstant*    op) override;
	virtual void Visit(NullConstant*      op) override;
	virtual void Visit(UndefinedConstant* op) override;

	virtual void Visit(ArithmeticInstr* instr) override;
	virtual void Visit(ConversionInstr* instr) override;
	virtual void Visit(LogicalInstr*    instr) override;

	virtual void Visit(LoadInstr*    instr) override;
	virtual void Visit(StoreInstr*   instr) override;
	virtual void Visit(AddressInstr* instr) override;
	virtual void Visit(IndexInstr*   instr) override;
	virtual void Visit(FieldInstr*   instr) override;
	virtual void Visit(CmpInstr*     instr) override;
	virtual void Visit(UcmpInstr*    instr) override;
	virtual void Visit(FcmpInstr*    instr) override;

	virtual void Visit(IfInstr*       instr) override;
	virtual void Visit(GotoInstr*     instr) override;
	virtual void Visit(CallInstr*     instr) override;
	virtual void Visit(ReturnInstr*   instr) override;
	virtual void Visit(SwitchInstr*   instr) override;
	virtual void Visit(PhiInstr*      instr) override;
    virtual void Visit(QuestionInstr* instr) override;

	string ToString() const {
		return sb_.ToString();
	}

	// Returns the index associated with the specified temporary operand.
	int GetTemporaryIndex(Temporary* temp) {
		int index;

		if(tempToIndex_.TryGetValue(temp, &index)) {
			return index;
		}
		else return -1;
	}

	// Prints all information to the console.
	void Dump() const;
};

} // namespace IR
#endif