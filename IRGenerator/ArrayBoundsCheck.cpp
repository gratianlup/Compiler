// ArrayBoundsCheck.hpp
// Copyright (c) Lup Gratian
//
// Implements array range check code emission.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ArrayBoundsCheck.hpp"
#include "UnitGenerator.hpp"
#include "FunctionGenerator.hpp"

namespace IRGenerator {

IR::IndexInstr* ArrayBoundsCheck::GetIndex(IR::Operand* op) {
    // Try to find the 'index' instruction that generates the operand.
    // Note that we ignore 'ptop' conversions.
    auto instr = op->DefiningInstruction();

    while(instr && (instr->IsIndex() == false)) {
        if(auto ptopInstr = instr->As<IR::PtopInstr>()) {
            instr = ptopInstr->TargetOp()->DefiningInstruction();
        }
        else if(auto fieldInstr = instr->As<IR::FieldInstr>()) {
            instr = fieldInstr->BaseOp()->DefiningInstruction();
        }
        else if(auto addrInstr = instr->As<IR::AddressInstr>()) {
            instr = addrInstr->BaseOp()->DefiningInstruction();
        }
        else break;
    }

    return instr ? instr->As<IR::IndexInstr>() : nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string* ArrayBoundsCheck::GetVariableName(IR::Operand* op) {
    if(auto variableRef = op->As<IR::VariableReference>()) {
        auto symbol = variableRef->GetSymbol();
        return (symbol && symbol->HasName()) ? symbol->Name() : nullptr;
    }
    else if(auto instr = op->DefiningInstruction()) {
        if(instr->IsAddressing()) {
            // addr/index/field
            auto addrInstr = static_cast<IR::AddressInstr*>(instr);
            return GetVariableName(addrInstr->BaseOp());
        }
        else if(auto ptopInstr = instr->As<IR::PtopInstr>()) {
            return GetVariableName(ptopInstr->TargetOp());
        }
    }
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* ArrayBoundsCheck::GetOrInsertName(string* name,
                                                         FunctionGenerator* functGen) {
    // Check if the specified name was already emitted as a global constant.
    IR::VariableReference* nameRef;

    if(names_.TryGetValue(name, &nameRef)) {
        return nameRef;
    }

    // Create a global constant variable initialized with the specified text.
    auto irGen = functGen->GetIRGen();
    auto strType = irGen->GetArray(IR::IntegerType::GetInt8(), name->Length() + 1);
    auto nameConst = irGen->GetStringConst(StringBuffer(*name), strType);
    auto initializer = irGen->GetInitializer(nameConst);
    string varName = string::Format(L"#range%d", names_.Count());

    auto stringConstVar = irGen->GetGlobalSymbol(strType, varName, initializer,
											     nullptr, IR::SymbolVisibility::Static);
	stringConstVar->SetIsConstant(true);
    nameRef = functGen->GetUnitGen()->AddConstant(stringConstVar);
    names_.Add(name, nameRef);
    return nameRef;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ArrayBoundsCheck::EmitIndexChecks(IR::IndexInstr* indexInstr, TList& testOps,
                                       IR::IRGenerator* irGen, 
                                       FunctionGenerator* functGen) {
    // We could emit two checks: 'index >= 0' and 'index < ARRAY_SIZE',
    // but we can emit a single one if we use unsigned comparison.
    // If 'index' is negative it will be interpreted as a very large number,
    // and then the 'index < ARRAY_SIZE' test will fail.
    auto indexOp = indexInstr->IndexOp();
    auto sizeConst = irGen->GetInt32Const(indexInstr->GetArrayType()->Size());

    auto testOp = irGen->GetInt32Temp();
    testOp->SetIsBoolean(true);
    irGen->GetUcmpLT(indexOp, sizeConst, testOp, functGen->ActiveBlock());
    testOps.Add(testOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ArrayBoundsCheck::EmitBoundsCheck(GeneratorContext& context, IR::Operand* op) {
    StaticList<IR::Operand*, 8> testOps;
    auto functGen = context.CurrentGenerator;
    auto irGen = functGen->GetIRGen();

    // Create the bounds checks. For 'a[i][j]' we create two checks, for example.
    while(true) {
        auto indexInstr = GetIndex(op);
        if(indexInstr == nullptr) break;

        EmitIndexChecks(indexInstr, testOps, irGen, functGen);
        op = indexInstr->BaseOp();
    }

    if(testOps.Count() > 0) {
        // Combine all the tests. For example,
        // t1 = ucmp lt i, MAX_I   ->   t3 = and t1, t2
        // t2 = ucmp lt j, MAX_J      
        auto combinedOp = testOps[0];

        for(int i = 1; i < testOps.Count(); i++) {
            auto temp = irGen->GetInt32Temp();
            irGen->GetAnd(combinedOp, testOps[i], temp, functGen->ActiveBlock());
            combinedOp = temp;
        }

        // Emit the call to the intrinsic who checks 'combinedOp'.
        FileDetails* details;
        auto exprLocation = context.CurrentExpression->Location();
        functGen->GetContext()->FileMgr().GetDetails(exprLocation.File(), details);

        auto fileName = GetOrInsertName(&details->Path(), functGen);
        auto varName = GetVariableName(op);

        // We need to create the 'file name' and 'variable name' arguments first,
        // because they involve 'ptop' instructions that should be emitted
        // before the 'call'.
        IR::Operand* fileOp = irGen->GetTemporary(irGen->GetInt8Pointer());
        irGen->GetPtop(fileName, irGen->GetInt8Pointer(), fileOp, 
                       functGen->ActiveBlock());

        // Variable name, if available.
        IR::Operand* nameOp;

        if(varName) {
            nameOp = irGen->GetTemporary(irGen->GetInt8Pointer());
            irGen->GetPtop(GetOrInsertName(varName, functGen), 
                           irGen->GetInt8Pointer(), nameOp, 
                           functGen->ActiveBlock());
        }
        else nameOp = irGen->GetNullConst(irGen->GetInt8Pointer());

        auto intrinsic = IR::BoundsCheckIntrinsic::GetBoundsCheck(functGen->GetIRUnit());
        auto callInstr = irGen->GetCall(intrinsic, nullptr, 4, functGen->ActiveBlock());
        callInstr->SetResultOp(irGen->GetInt32Temp());

        // Now add the arguments.
        callInstr->AddArgument(combinedOp);
        callInstr->AddArgument(fileOp);
        callInstr->AddArgument(nameOp);
        callInstr->AddArgument(irGen->GetInt32Const(exprLocation.Line()));
    }
}

} // namespace IRGenerator