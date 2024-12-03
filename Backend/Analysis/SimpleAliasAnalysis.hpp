// SimpleAliasAnalysis.hpp
// Copyright (c) Lup Gratian
//
// Implements a simple rule-based alias analysis.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SIMPLE_ALIAS_ANALYSIS_HPP
#define PC_ANALYSIS_SIMPLE_ALIAS_ANALYSIS_HPP

#include "AliasInfo.hpp"
#include "AliasAnalyzer.hpp"
#include "CFamilyTag.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/StaticList.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Intrinsics.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class SimpleAliasAnalysis : public AliasAnalyzer {
private:
    typedef StaticList<AliasLocation, 4> AliasLocationList;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Handles pairs consisting of a variable and
    // an operand of other type.
    AliasResult ComputeSingleVariableAlias(VariableReference* variableRef, 
                                           Operand* otherOp);
    
    // Handles pairs consisting of two variables.
    AliasResult ComputeVariableAlias(AliasLocation& locationA, 
                                     AliasLocation& locationB);

    // Handles pairs consisting of at least one variable
    // that is marked as being address-taken or non-escaping.
    AliasResult ComputeNonEscapedAlias(AliasLocation& locationA, 
                                       AliasLocation& locationB);

    // Handles pairs consisting of two parameters
    // or a parameter and an operand of other type.
    AliasResult ComputeParameterAlias(AliasLocation& locationA, 
                                      AliasLocation& locationB);

    // Handles operands originated from aggregate variables.
    AliasResult ComputeAggregateAlias(AliasLocation& locationA, 
                                      AliasLocation& locationB);

    // Handles operands originated from a memory allocation
    // function, like 'malloc' for C languages.
    AliasResult ComputeAllocAlias(AliasLocation& locationA, 
                                  AliasLocation& locationB);

    // Handles operands that are the result of 'phi' or 'quest'
    // instructions (any possible combinations).
    AliasResult ComputePhiQuestionAlias(AliasLocation& locationA, 
                                        AliasLocation& locationB);

    // Adds to the list all operands of the 'phi'/'quest' instruction
    // indicated by the location. Returns 'false' if no such operands
    // were found or it is unsafe to perform the analysis.
    bool CollectPhiQuestionOperands(AliasLocation& location, 
                                    AliasLocationList& list,
                                    bool& hasPhiQuestion);

    // Returns 'true' if the specified incoming 'phi' operand
    // may not lead to an infinite loop when processing (phi cycle).
    bool IsSafePhiOperand(Operand* op);

public:
    SimpleAliasAnalysis(AliasInfo* parent) : AliasAnalyzer(parent) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual AliasResult ComputeAlias(AliasLocation locationA, 
                                     AliasLocation locationB) override;

    virtual AliasResult ComputeCallAlias(CallInstr* instr, 
                                         AliasLocation location) override;

    virtual bool HandlesCalls() const override {
        return true;
    }

    virtual string Name() override {
        return "Simple Alias Analysis";
    }
};

} // namespace Analysis
#endif