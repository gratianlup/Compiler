// NameGenerator.hpp
// Copyright (c) Lup Gratian
//
// Generates an unique name for a variable, record, typename or temporary.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_NAME_GEN_HPP
#define PC_IR_GENERATOR_NAME_GEN_HPP

#include "../IR/SymbolTable.hpp"
#include "../Base/List.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IRGenerator {
    
enum class BlockName {
    IfCont,
    IfFalse,
    IfTrue,
    InfLoop,
    InfCont,
    CondCont,
    CondTrue,
    CondFalse,
    AndRight,
    OrRight,
    ForCont,
    ForTest,
    ForInc,
    ForBody,
    WhileBody,
    WhileHeader,
    WhileCont,
    TermCont,
    Block,
    SwitchCase,
    SwitchCont,
    GotoDest,
    LogicalTrue,
    LogicalFalse,
	LogicalCont,
    DoBody,
    DoFoo,
    DoCont
};


class NameGenerator {
private:
	string name_;
    int ifContCount_;
    int ifFalseCount_;
    int ifTrueCount_;
    int infLoopCount_;
    int infContCount_;
    int condContCount_;
    int condTrueCount_;
    int condFalseCount_;
    int andRightCount_;
    int orRightCount_;
    int forContCount_;
    int forTestCount_;
    int forIncCount_;
    int forBodyCount_;
    int whileBodyCount_;
    int whileHeaderCount_;
    int whileContCount_;
    int termContCount_;
    int blockCount_;
    int recordCount_;
    int switchCaseCount_;
    int switchContCount_;
    int gotoDestCount_;
    int logicalTrueCount_;
    int logicalFalseCount_;
    int logicalContCount_;
    int logicalVarCount_;
    int condTempCount_;
    int compoundCount_;
    int doBodyCount_;
    int doFooCount_;
    int doContCount_;

    string GenerateCountedName(const string& name, int& count);

public:
    NameGenerator();

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Generates a name of the form header_sep_prefix_sep_function_sep_name,
	// where _sep_ is the separator.
    string GetName(IR::SymbolTable* symbols, const string& name, const string& function = "", 
				   const string& prefix = "", const string& separator = "_", 
				   const string& header = "#");

	// Methods for generation various common names.
    string GetBlockName(BlockName name);

    string GetRecordSlotName();

    string GetLogicalVarName();

    string GetCondTempName();

    string GetCompoundName();
};

} // namespace IRGenerator 
#endif