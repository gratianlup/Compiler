// NameGenerator.cpp
// Copyright (c) Lup Gratian
//
// Implements the NameGenerator class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "NameGenerator.hpp"

namespace IRGenerator {

NameGenerator::NameGenerator() : 
        ifContCount_(0), 
        ifFalseCount_(0), 
        ifTrueCount_(0), 
        infLoopCount_(0),
        infContCount_(0), 
        condContCount_(0), 
        condTrueCount_(0), 
        condFalseCount_(0), 
        andRightCount_(0), 
        orRightCount_(0), 
        forContCount_(0), 
        forIncCount_(0),
        forBodyCount_(0),
        forTestCount_(0), 
        whileBodyCount_(0), 
        whileHeaderCount_(0),
        whileContCount_(0),
        recordCount_(0),
        termContCount_(0),
        blockCount_(0),
        switchCaseCount_(0),
        switchContCount_(0),
        gotoDestCount_(0),
        logicalTrueCount_(0),
        logicalFalseCount_(0),
        logicalContCount_(0),
        logicalVarCount_(0),
        condTempCount_(0),
        compoundCount_(0),
        doBodyCount_(0),
        doFooCount_(0),
        doContCount_(0) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NameGenerator::GetName(IR::SymbolTable* symbols, const string& name, 
                              const string& function, const string& prefix, 
                              const string& separator, const string& header) {
    DebugValidator::IsNotNull(symbols);
	int count = 1;
    string generated;

	do {
		StringBuilder sb(prefix.Length() > 0 ? header : "");
		bool hasPrefix = false;
		bool hasFunction = false;

		// Append the prefix.
		if(prefix.Length() > 0) {
			sb.Append(prefix);
			hasPrefix = true;
		}

		// Append the function name.
		if(function.Length() > 0) {
			if(hasPrefix) sb.Append(separator);
			hasPrefix = false;
			sb.Append(function);
			hasFunction = true;
		}

		// We append the count if the generated name is already used.
		if(name.Length() > 0) {
			if(hasPrefix || hasFunction) sb.Append(separator);
			sb.Append(name);
		}

		// Try o make it unique my appending the iteration count.
		if(count > 1) {
			sb.AppendFormat(L"%d", count);
		}

		generated = sb.ToString();
		count++;
	} while(symbols->Contains(&generated));

    return generated;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NameGenerator::GenerateCountedName(const string& name, int& count) {
    string generated = string::Format(L"%s%d", name.Chars(), count);
    count++;
    return generated;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NameGenerator::GetBlockName(BlockName name) {
    switch(name) {
        case BlockName::IfCont:       return GenerateCountedName("#if_cont",      ifContCount_);
        case BlockName::IfFalse:      return GenerateCountedName("#if_false",     ifFalseCount_);
        case BlockName::IfTrue:       return GenerateCountedName("#if_true",      ifTrueCount_);
        case BlockName::InfLoop:      return GenerateCountedName("#inf_loop",     infLoopCount_);
        case BlockName::InfCont:      return GenerateCountedName("#inf_cont",     infContCount_);
        case BlockName::CondCont:     return GenerateCountedName("#cond_cont",    condContCount_);
        case BlockName::CondTrue:     return GenerateCountedName("#cond_true",    condTrueCount_);
        case BlockName::CondFalse:    return GenerateCountedName("#cond_false",   condFalseCount_);
        case BlockName::AndRight:     return GenerateCountedName("#and_right",    andRightCount_);
        case BlockName::OrRight:      return GenerateCountedName("#or_right",     orRightCount_);
        case BlockName::ForCont:      return GenerateCountedName("#for_cont",     forContCount_);
        case BlockName::ForTest:      return GenerateCountedName("#for_test",     forTestCount_);
        case BlockName::ForInc:       return GenerateCountedName("#for_inc",      forIncCount_);
        case BlockName::ForBody:       return GenerateCountedName("#for_body",    forBodyCount_);
        case BlockName::WhileBody:    return GenerateCountedName("#while_body",   whileBodyCount_);
        case BlockName::WhileHeader:  return GenerateCountedName("#while_header", whileHeaderCount_);
        case BlockName::WhileCont:    return GenerateCountedName("#while_cont",   whileContCount_);
        case BlockName::TermCont:     return GenerateCountedName("#term_cont",    termContCount_);
        case BlockName::Block:        return GenerateCountedName("#block",        blockCount_);
        case BlockName::SwitchCase:   return GenerateCountedName("#switch_case",  switchCaseCount_);
        case BlockName::SwitchCont:   return GenerateCountedName("#switch_cont",  switchContCount_);
        case BlockName::GotoDest:     return GenerateCountedName("#goto_dest",    gotoDestCount_);
        case BlockName::LogicalTrue:  return GenerateCountedName("#logical_true", logicalTrueCount_);
        case BlockName::LogicalFalse: return GenerateCountedName("#logical_false",logicalFalseCount_);
		case BlockName::LogicalCont:  return GenerateCountedName("#logical_cont", logicalContCount_);
        case BlockName::DoBody:       return GenerateCountedName("#do_body",      doBodyCount_);
        case BlockName::DoFoo:        return GenerateCountedName("#do_foo",       doFooCount_);
        case BlockName::DoCont:       return GenerateCountedName("#do_cont",      doContCount_);
        default: DebugValidator::Unreachable();
    }

    return ""; // Should not reached.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NameGenerator::GetRecordSlotName() {
    return GenerateCountedName("#slot", recordCount_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NameGenerator::GetLogicalVarName() {
    return GenerateCountedName("#logical_var", logicalVarCount_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NameGenerator::GetCondTempName() {
    return GenerateCountedName("#cond_temp", condTempCount_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string NameGenerator::GetCompoundName() {
    return GenerateCountedName("#compound", compoundCount_);
}

} // namespace IRGenerator