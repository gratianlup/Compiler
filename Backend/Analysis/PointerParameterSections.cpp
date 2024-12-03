// PointerParameterSections.hpp
// Copyright (c) Lup Gratian
//
// Implements the PointerParameterSections pass.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "PointerParameterSections.hpp"

namespace Analysis {

void PointerParameterSections::Execute(CallGraph* callGraph) {
    local<ParameterSectionsVisitor> visitor = new ParameterSectionsVisitor(this);
    callGraph->FindRoots(true);
    callGraph->ReverseInvocationTraversalFromRoots(visitor);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     Visit(CallNode* node, CallGraph* callGraph) {
    // Ignore the External and Unknown nodes
    // and functions without a body.
    auto function = GetFunctionDefinition(node);

    if(function == nullptr) {
        return true;
    }

    // 
    TrackedOperandsList trackedOps;
    MakeTrackedParameterList(function, trackedOps);

    if(trackedOps.IsNotEmpty()) {
        ProcessInstructions(function, callGraph, trackedOps);
    }

#if 0
    Dump(function);
#endif
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) {
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* PointerParameterSections::ParameterSectionsVisitor::
          GetFunctionDefinition(CallNode* node) {
    // Ignore the External and Unknown nodes.
    if(node->IsExternalNode() || node->IsUnknownNode()) {
        return nullptr;
    }

    // Consider only defined functions.
    auto function = node->GetFunction();

    if(function->IsDefinition() == false) {
        return nullptr;
    }
    else return function;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     MakeTrackedParameterList(Function* function, TrackedOperandsList& trackedOps) {
    for(int i = 0; i < function->ParameterCount(); i++) {
        auto parameterVariable = function->GetParameterVariable(i);

        if(parameterVariable->IsAddressNotTaken()) {
            auto parameter = function->GetParameter(i);
            trackedOps.Add(TrackedOperandInfo(parameter, parameter, i));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     ProcessInstructions(Function* function, CallGraph* callGraph,
                         TrackedOperandsList& trackedOps) {
    auto instructionEnum = function->GetInstructionEnum();

    while(instructionEnum.IsValid()) {
        auto instr = instructionEnum.Next();

        if(auto loadInstr = instr->As<LoadInstr>()) {
            ProcessLoad(loadInstr, trackedOps);
        }
        else if(auto storeInstr = instr->As<StoreInstr>()) {
            ProcessStore(storeInstr, trackedOps);
        }
        else if(auto callInstr = instr->As<CallInstr>()) {
            ProcessCall(callInstr, trackedOps);
        }
        else if(auto questInstr = instr->As<QuestionInstr>()) {
            ProcessQuestion(questInstr, trackedOps);
        }
        else if(auto phiInstr = instr->As<PhiInstr>()) {
            ProcessPhi(phiInstr, trackedOps);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     ProcessLoad(LoadInstr* instr, TrackedOperandsList& trackedOps) {
         // 
    ParameterList parameters;
    AddressInstr* lastAddressingInstr;
    bool hasPtop;

    if(FindTrackedParameters(instr->SourceOp(), parameters, 
                             lastAddressingInstr, hasPtop, trackedOps)) {
        if(hasPtop) {
            MarkAllWithUnknownSections(parameters);
        }
        else CreateParameterSections(parameters, instr, lastAddressingInstr);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     ProcessStore(StoreInstr* instr, TrackedOperandsList& trackedOps) {
         // 
    AddressInstr* lastAddressingInstr;
    ParameterList parameters;
    bool hasPtop;

    if(FindTrackedParameters(instr->DestinationOp(), parameters, 
                             lastAddressingInstr, hasPtop, trackedOps)) {
        if(hasPtop) {
            MarkAllWithUnknownSections(parameters);
        }
        else CreateParameterSections(parameters, instr, lastAddressingInstr);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     ProcessCall(CallInstr* instr, TrackedOperandsList& trackedOps) {
    // We mark the sections accessed by 'copyMemory' and 'setMemory'
    // (for 'copyMemory' both the source and the destination are considered).
    if(auto intrinsic = instr->GetIntrinsic()) {
        ProcessIntrinsic(intrinsic, instr, trackedOps);
        return;
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     ProcessIntrinsic(Intrinsic* intrinsic, CallInstr* instr,
                      TrackedOperandsList& trackedOps) {
    // For calls to the 'setMemory' and 'copyMemory' intrinsics
    // the accessed regions can be sometimes determined.
    auto function = instr->ParentFunction();
    
    if(auto setMemoryIntr = intrinsic->As<SetMemoryIntr>()) {
        ElementSection sizeElement;
        auto sizeOp = SetMemoryIntr::GetLength(instr);
        auto destOp = SetMemoryIntr::GetDestination(instr);
        bool hasSize = CreateSizeElementSection(sizeOp, function, sizeElement);

        CreateMemoryAccessSection(destOp, true /* isWrite */, hasSize, 
                                  sizeElement, instr, trackedOps);
    }
    else if(auto copyMemoryIntr = intrinsic->As<CopyMemoryIntr>()) {
        ElementSection sizeElement;
        auto sizeOp = CopyMemoryIntr::GetLength(instr);
        auto destOp = CopyMemoryIntr::GetDestination(instr);
        auto sourceOp = CopyMemoryIntr::GetSource(instr);
        bool hasSize = CreateSizeElementSection(sizeOp, function, sizeElement);

        CreateMemoryAccessSection(destOp, true /* isWrite */, hasSize, 
                                  sizeElement, instr, trackedOps);
        CreateMemoryAccessSection(sourceOp, false /* isWrite */, hasSize, 
                                  sizeElement, instr, trackedOps);

        // If the destination might alias the source we also
        // mark the source as having written sections.
        if(MightBeAlias(destOp, sourceOp)) {
            CreateMemoryAccessSection(sourceOp, true/* isWrite */, hasSize, 
                                      sizeElement, instr, trackedOps);
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     CreateSizeElementSection(Operand* sizeOp, Function* function,
                              ElementSection& sizeElement) {
    // Check if the size of a 'setMemory'/'copyMemory' call
    // can be expressed in the form of a element section
    // (it is a constant, a parameter or other combination).
    Operand* sizeBaseOp = nullptr;
    Operand* sizeFactorOp = nullptr;
    Operand* sizeAdjustment = nullptr;
    Opcode sizeOperation;

    return FindSectionComponents(sizeOp, sizeBaseOp, sizeFactorOp, 
                                 sizeOperation, sizeAdjustment) &&
           CreateElementSection(sizeBaseOp, sizeFactorOp, sizeOperation,
                                sizeAdjustment, function, sizeElement);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
    CreateMemoryAccessSection(Operand* accessedOp, bool isWrite, 
                              bool hasKnownSize, ElementSection& sizeElement,
                              Instruction* instr, TrackedOperandsList& trackedOps) {
    // Mark the accessed section for parameters used by a call to
    // 'setMemory'/'copyMemory' as a source/destination (depends on 'isWrite').
    // The section is of the form [startElement, startElement + sizeElement - 1], 
    // where 'startElement' is 0 if no addressing instructions are applied 
    // on the parameters.
    AddressInstr* lastAddressingInstr = nullptr;
    ParameterList parameters;
    bool hasPtop;

    if(FindTrackedParameters(accessedOp, parameters, lastAddressingInstr, 
                             hasPtop, trackedOps) == false) {
        return;
    }

    // If the accessed size is not known the parameters
    // are marked as having unknown sections.
    if(hasKnownSize == false) {
        MarkAllWithUnknownSections(parameters);
        return;
    }

    // Check if the parameters are not accessed directly and
    // try to handle some cases that occur more often.
    bool valid = false;
    
    if(lastAddressingInstr) {
        Operand* destinationBaseOp = nullptr;
        Operand* destinationFactorOp = nullptr;
        Operand* destinationAdjustment = nullptr;
        Opcode destinationOperation;
        ElementSection startElement;

        if(FindSectionComponents(accessedOp, destinationBaseOp, 
                                 destinationFactorOp, destinationOperation,
                                 destinationAdjustment) &&
           CreateElementSection(destinationBaseOp, destinationFactorOp, 
                                destinationOperation, destinationAdjustment,
                                instr->ParentFunction(), startElement)) { 
            // Try to compute the offset of the last element accessed
            // as 'startElement + sizeElement - 1'.
            auto unit = instr->ParentFunction()->ParentUnit();
            ElementSection oneConstSection(GetOneInt(unit));
            ElementSection endElement;

            if(startElement.Add(sizeElement, endElement, unit) &&
               endElement.Subtract(oneConstSection, endElement, unit)) {
                valid = true;
                MarkAccessedRange(parameters, RangeSection(startElement, endElement),
                                  instr, isWrite, true /* isByte */);
            }
        }
    }
    else {
        // The parameters are used directly, we just need to 
        // create the byte section [0, sizeElement - 1].
        auto unit = instr->ParentFunction()->ParentUnit();
        ElementSection oneConstSection(GetOneInt(unit));
        ElementSection startElement(GetZeroInt(unit));
        ElementSection endElement;

        if(endElement.Subtract(oneConstSection, endElement, unit)) {
            valid = true;
            MarkAccessedRange(parameters, RangeSection(startElement, endElement),
                              instr, isWrite, true /* isByte */);
        }
    }

    // If the accessed section could not be completely identified
    // mark all parameters as having unknown sections.
    if(valid == false) {
        MarkAllWithUnknownSections(parameters);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     ProcessQuestion(QuestionInstr* instr, TrackedOperandsList& trackedOps) {
    // Skip dead instructions.
    if(instr->HasDestinationOp() == false) {
        return;
    }

    TrackResultOperand(instr->ResultOp(), instr->TrueOp(), trackedOps);
    TrackResultOperand(instr->ResultOp(), instr->FalseOp(), trackedOps);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     ProcessPhi(PhiInstr* instr, TrackedOperandsList& trackedOps) {
    // Skip dead instructions.
    if(instr->HasDestinationOp() == false) {
        return;
    }

    for(int i = 0; i < instr->OperandCount(); i++) {
        TrackResultOperand(instr->ResultOp(), instr->GetOperand(i), trackedOps);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
        TrackResultOperand(Operand* resultOp, Operand* sourceOp,
                           TrackedOperandsList& trackedOps) {
    // Any parameter associated with the source operand
    // is now associated with the resulting one too.
    if(sourceOp->IsConstant() || sourceOp->IsVariableReference()) {
        return;
    }

    int count = trackedOps.Count();

    for(int i = 0; i < count; i++) {
        if(trackedOps[i].TrackedOperand == sourceOp) {
            trackedOps.Add(TrackedOperandInfo(resultOp, 
                                              trackedOps[i].TrackedParameter,
                                              trackedOps[i].ParameterIndex));
        }
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     FindTrackedParameters(Operand* op, ParameterList& parameters,
                           AddressInstr*& lastAddressingInstr, bool& hasPtop,
                           TrackedOperandsList& trackedOps) {
    // Skip over any addressing instruction, remembering the last one.
    // When the base operand is reached check if it is a tracked operand.
    hasPtop = false; // Presume there is no pointer conversion.

    while(op->HasDefiningInstruction()) {
        auto definingInstr = op->DefiningInstruction();

        if(definingInstr->IsAddressing()) {
            lastAddressingInstr = static_cast<AddressInstr*>(definingInstr);
            op = definingInstr->GetSourceOp(0);
        }
        else if(definingInstr->IsPtop()) {
            // It is too complex to track the sections
            // of parameters converted to other pointer types.
            hasPtop = true;
            op = definingInstr->GetSourceOp(0);
        }
        else break;
    }

    // Add to the list all parameters associated
    // with the found base operand.
    for(int i = 0; i < trackedOps.Count(); i++) {
        auto trackedOp = trackedOps[i];

        if(trackedOp.TrackedOperand == op) {
            if(parameters.Contains(trackedOp.TrackedParameter) == false) {
                parameters.Add(trackedOp.TrackedParameter);
            }
        }
    }

    return parameters.IsNotEmpty();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     CreateParameterSections(ParameterList& parameters, Instruction* accessInstr,
                             AddressInstr* lastAddressingInstr) {
    // If all parameters are already marked as having
    // unknown sections don't bother creating a new one.
    if(AllWithUnknownSections(parameters)) {
        return;
    }

    // Find the operands that form the access index.
    // We look for expressions like 'baseOp OP factorOp'.
    Operand* baseOp = nullptr;
    Operand* factorOp = nullptr;
    Operand* adjustmentOp = nullptr;
    Opcode operation;
    bool created = false;

    if(FindSectionComponents(lastAddressingInstr->IndexOp(),
                             baseOp, factorOp, operation, adjustmentOp)) {
        if(factorOp == nullptr) {
            // The simplest case is when no factor is used.
            if(baseOp->IsIntConstant() || baseOp->IsParameter()) {
                // We have something like 'p[2]' or 'p[a]'.
                MarkNoFactorElement(baseOp, adjustmentOp, parameters, accessInstr);
                created = true;
            }
            else {
                // The base operand might be a 'phi'/'quest' result
                // or a global variable for which we known the possible
                // constants/parameters.
                created = CreateVariableWithNoFactorElement(baseOp, adjustmentOp,
                                                            parameters, accessInstr);
            }
        }
        else if(factorOp->IsParameter()) {
            // We could have something like 'p[2 * a]' or 'p[a + b]',
            // or a variable operand as the base ('phi'/'quest'/global variable).
            created = MarkParameterFactorElement(baseOp, factorOp, operation,
                                                 adjustmentOp, parameters, accessInstr);
        }
        else if(factorOp->IsConstant()) {
            // We could have something like 'p[a * 4]' or 'p[2 + 4]',
            // or a variable operand as the base ('phi'/'quest'/global variable).
            // If both operands are constant they are folded.
            created = MarkConstantFactorElement(baseOp, factorOp, operation,
                                                adjustmentOp, parameters, accessInstr);
        }
        else {
            // The factor operand might be a 'phi'/'quest'/global variable.
            // If both operands are variable we create pairs with
            // each constant/parameter value (under a certain limit).
            created = CreateVariableWithFactorElement(baseOp, factorOp, operation,
                                                      adjustmentOp, parameters, accessInstr);
        }
    }

    if(created == false) {
        // The exact section(s) couldn't be determined, mark all parameters
        // as being accessed in unknown sections.
        MarkAllWithUnknownSections(parameters);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     MarkNoFactorElement(Operand* baseOp, Operand* adjustmentOp,
                         ParameterList& parameters, Instruction* accessInstr) {
    DebugValidator::IsNotNull(baseOp);

    //
    if(auto intConstant = baseOp->As<IntConstant>()) {
        auto element = ElementSection(intConstant);
        MarkAccessedSection(parameters, element, accessInstr);
    }
    else if(auto parameter = baseOp->As<Parameter>()) {
        auto function = accessInstr->ParentFunction();
        auto element = ElementSection(GetParameterIndex(parameter, function));
        MarkAccessedSection(parameters, element, accessInstr);
    }
    else DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     MarkParameterFactorElement(Operand* baseOp, Operand* factorOp, 
                                Opcode operation, Operand* adjutmentOp,
                                ParameterList& parameters, Instruction* accessInstr) {
    DebugValidator::IsNotNull(baseOp);
    DebugValidator::IsNotNull(factorOp);
    DebugValidator::IsTrue(factorOp->IsParameter());

    //
    auto factorParameter = factorOp->As<Parameter>();
    auto function = accessInstr->ParentFunction();
    int factorIndex = GetParameterIndex(factorParameter, function);

    if(auto intConstant = baseOp->As<IntConstant>()) {
        auto element = ElementSection(intConstant, operation, factorIndex);
        MarkAccessedSection(parameters, element, accessInstr);
        return true;
    }
    else if(auto parameter = baseOp->As<Parameter>()) {
        int baseIndex = GetParameterIndex(parameter, function);
        auto element = ElementSection(baseIndex, operation, factorIndex);
        MarkAccessedSection(parameters, element, accessInstr);
        return true;
    }
    else {
        //! TODO: global var/phi?
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     MarkConstantFactorElement(Operand* baseOp, Operand* factorOp, 
                               Opcode operation, Operand* adjustmentOp,
                               ParameterList& parameters, Instruction* accessInstr) {
    DebugValidator::IsNotNull(baseOp);
    DebugValidator::IsNotNull(factorOp);
    DebugValidator::IsTrue(factorOp->IsIntConstant());
    auto factorConstant = factorOp->As<IntConstant>();

    if(auto intConstant = baseOp->As<IntConstant>()) {
        //! TODO: evaluate constant
        //auto element = ElementSection(intConstant, operation, factorIndex);
        //MarkAccessedSection(parameters, element, accessInstr);
        return true;
    }
    else if(auto parameter = baseOp->As<Parameter>()) {
        auto function = accessInstr->ParentFunction();
        int baseIndex = GetParameterIndex(parameter, function);
        auto element = ElementSection(baseIndex, operation, factorConstant);
        MarkAccessedSection(parameters, element, accessInstr);
        return true;
    }
    else {
        // var
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
    CreateVariableWithFactorElement(Operand* baseOp, Operand* factorOp, 
                                    Opcode operation, Operand* adjustmentOp,
                                    ParameterList& parameters, Instruction* accessInstr) {
    DebugValidator::IsNotNull(baseOp);
    DebugValidator::IsNotNull(factorOp);

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     CreateVariableWithNoFactorElement(Operand* baseOp, Operand* adjustmentOp,
                                       ParameterList& parameters, Instruction* accessInstr) {
    // The index operand is not a constant or a parameter.
    // We treat two cases here: a global variable constant having
    // one or more possible constants and induction variables.
    if(baseOp->IsGlobalVariableRef()) {
        auto globalVariable = baseOp->As<VariableReference>()->GetGlobalVariable();
        return CreateGlobalVariableSections(globalVariable, adjustmentOp,
                                            parameters, accessInstr);
    }
    else if(auto phiInstr = baseOp->DefiningInstrAs<PhiInstr>()) {
        // The 'phi' might be either an induction variable
        // or a series of constants/parameters.
        if(phiInstr->HasOnlyParametersOrConstants()) {
            return CreatePhiSections(phiInstr, adjustmentOp,
                                     parameters, accessInstr);
        }

        //! TODO: induction var
    }
    else if(auto questInstr = baseOp->DefiningInstrAs<QuestionInstr>()) {
        //! TODO: 
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     CreateGlobalVariableSections(GlobalVariable* globalVariable, Operand* adjustmentOp,
                                  ParameterList& parameters, Instruction* accessInstr) {
    // If the global variable has its possible constants known
    // we can create "may" section elements using them
    // (it's better than having the pointer marked as "unknown").
    auto constantsTag = globalVariable->GetTag<GlobalConstantsTag>();

    if((constantsTag == nullptr) ||
       (constantsTag->HasOnlyConstants() == false)) {
        return false;
    }

    constantsTag->ForEachConstant([this, &parameters, accessInstr]
                                  (Constant* constant, float probability) -> bool {
        if(auto intConstant = constant->As<IntConstant>()) {
            auto element = ElementSection(intConstant);
            MarkAccessedSection(parameters, element, accessInstr, true /* forceMay */);
        }

        return true;
    });

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     CreatePhiSections(PhiInstr* phiInstr, Operand* adjustmentOp,
                       ParameterList& parameters, Instruction* accessInstr) {
    // Add each incoming constant/parameter as an accessed element.
    for(int i = 0; i < phiInstr->OperandCount(); i++) {
        auto incomingOp = phiInstr->GetOperand(i);

        if(incomingOp->IsIntConstant() || incomingOp->IsParameter()) {
            MarkNoFactorElement(incomingOp, adjustmentOp,
                                parameters, accessInstr);
        }
    }

    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     FindSectionComponents(Operand* indexOp, Operand*& baseOp, Operand*& factorOp, 
                           Opcode& operation, Operand*& adjustment) {
    // The index might be a simple constant or parameter,
    // or an arithmetic instruction of the form 'a OP b', where 
    // at least one of 'a' or 'b' should be a constant or a parameter.
    if(indexOp->IsConstant()  || 
       indexOp->IsParameter() ||
       indexOp->IsVariableReference()) {
        baseOp = indexOp;
        factorOp = nullptr;
        return true;
    }
    else if(indexOp->HasDefiningInstruction()) {
        auto definingInstr = indexOp->DefiningInstruction();

        if(definingInstr->IsArithmetic()) {
            auto arithmeticInstr = definingInstr->As<ArithmeticInstr>();
            baseOp = arithmeticInstr->LeftOp();
            factorOp = arithmeticInstr->RightOp();
            operation = arithmeticInstr->GetOpcode();
            return true;
        }
        else if(definingInstr->IsPhi() || definingInstr->IsQuestion()) {
            baseOp = indexOp;
            factorOp = nullptr;
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     CreateElementSection(Operand* baseOp, Operand* factorOp, 
                          Opcode operation, Operand* adjustment,
                          Function* function, ElementSection& result) {
    DebugValidator::IsNotNull(baseOp);

    // Create an element section if it is possible
    // (the operands are constants or parameters).
    auto baseConstant = baseOp->As<IntConstant>();
    auto baseParameter = baseOp->As<Parameter>();
    auto factorConstant = factorOp ? factorOp->As<IntConstant>() : nullptr;
    auto factorParameter = factorOp ? factorOp->As<Parameter>() : nullptr;

    if(factorOp == nullptr) {
        if(baseConstant) {
            result = ElementSection(baseConstant);
            return true;
        }
        else if(baseParameter) {
            result = ElementSection(GetParameterIndex(baseParameter, function));
            return true;
        }
    }
    else if(factorConstant) {
        if(baseParameter) {
            // p + 2
            int paramIndex = GetParameterIndex(baseParameter, function);
            result = ElementSection(paramIndex, operation, factorConstant);
            return true;
        }
    }
    else if(factorParameter) {
        if(baseConstant) {
            // 2 + p
            int paramIndex = GetParameterIndex(factorParameter, function);
            result = ElementSection(baseConstant, operation, paramIndex);
            return true;
        }
        else if(baseParameter) {
            // a + b
            int baseParamIndex = GetParameterIndex(baseParameter, function);
            int factorParamIndex = GetParameterIndex(factorParameter, function);
            result = ElementSection(baseParamIndex, operation, factorParamIndex);
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     AllWithUnknownSections(ParameterList& parameters) {
    bool allUnknown = true;

    parameters.ForEach([this, &allUnknown](Parameter* parameter) -> bool {
        auto aliasTag = GetParameterAliasTag(parameter);

        if(aliasTag->Sections().HasUnknownSections() == false) {
            allUnknown = false;
            return false;
        }
        else return true;
    });

    return allUnknown;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     MarkAllWithUnknownSections(ParameterList& parameters) {
    parameters.ForEach([this](Parameter* parameter) -> bool {
        auto aliasTag = GetParameterAliasTag(parameter);
        aliasTag->Sections().MarkHasUnknownSections();
        return true;
    });
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     MarkAccessedSection(ParameterList& parameters, ElementSection element,
                         Instruction* accessInstr, bool forceMay) {
    for(int i = 0; i < parameters.Count(); i++) {
        auto aliasTag = GetParameterAliasTag(parameters[i]);
        bool isMay = forceMay || (IsAlwaysExecuted(accessInstr) == false);

        SectionProperties properties(accessInstr->IsLoad() /* isRead */,
                                     accessInstr->IsStore() /* isWrite */, isMay);
        aliasTag->AddElementSection(element, properties);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::
     MarkAccessedRange(ParameterList& parameters, RangeSection range,
                       Instruction* accessInstr, bool isWrite, bool isByte) {
    for(int i = 0; i < parameters.Count(); i++) {
        auto aliasTag = GetParameterAliasTag(parameters[i]);
        bool isMay = IsAlwaysExecuted(accessInstr) == false;

        SectionProperties properties(isWrite == false, isWrite, isMay, isByte);
        aliasTag->AddRangeSection(range, properties);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ParameterAliasTag* PointerParameterSections::ParameterSectionsVisitor::
                   GetParameterAliasTag(Parameter* parameter) {
    auto parameterVarialbe = parameter->GetVariable();
    auto aliasTag = parameterVarialbe->GetTag<ParameterAliasTag>();

    if(aliasTag == nullptr) {
        aliasTag = ParameterAliasTag::GetParameterAlias();
        parameterVarialbe->AddTag(aliasTag);
    }

    return aliasTag;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int PointerParameterSections::ParameterSectionsVisitor::
    GetParameterIndex(Parameter* parameter, Function* function) {
    auto parameterVariable = parameter->GetVariable();

    for(int i = 0; i < function->ParameterCount(); i++) {
        if(parameterVariable == function->GetParameterVariable(i)) {
            return i;
        }
    }

    DebugValidator::Unreachable();
    return -1;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerParameterSections::ParameterSectionsVisitor::
     MightBeAlias(Operand* opA, Operand* opB) {
    return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void PointerParameterSections::ParameterSectionsVisitor::Dump(Function* function) {
    string text;

    for(int i = 0; i < function->ParameterCount(); i++) {
        auto variable = function->GetParameterVariable(i);
        auto aliasTag = variable->GetTag<ParameterAliasTag>();

        if(aliasTag == nullptr) {
            continue;
        }

        text += *variable->Name();

        if(aliasTag->Sections().HasUnknownSections()) {
            text += " (Unknown)";
        }
        else text += string::Format(L" (Elements: %d, Ranges: %d):\n",
                                    aliasTag->Sections().ElementSections().Count(),
                                    aliasTag->Sections().RangeSections().Count());

        auto& elements = aliasTag->Sections().ElementSections();
        auto& ranges = aliasTag->Sections().RangeSections();

        for(int j = 0; j < elements.Count(); j++) {
            text += "\t";
            text += DumpElement(elements[j].Section);
            text += DumpProperties(elements[j].Properties);
        }

        text += "\n";

        for(int j = 0; j < ranges.Count(); j++) {
            text += "\t[";
            text += DumpElement(ranges[j].Section.LowBound());
            text += ", ";
            text += DumpElement(ranges[j].Section.HighBound());
            text += "], ";
            text += DumpProperties(ranges[j].Properties);
        }
    }

    ObjectDumper(text, "Parameter Sections for " + *function->Name()).Dump();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string PointerParameterSections::ParameterSectionsVisitor::
       DumpElement(ElementSection section) {
    string text;

    if(section.HasConstantBase()) {
        text += string::Format(L"%d", section.BaseAsConstant()->Value());
    }
    else text += string::Format(L"p%d", section.BaseAsParameter());

    if(section.IsOperation()) {
        text += " " + Instruction::OpcodeString(section.Operation());

        if(section.HasConstantFactor()) {
            text += string::Format(L" %d", section.FactorAsConstant()->Value());
        }
        else text += string::Format(L" p%d", section.FactorAsParameter());
    }

    return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string PointerParameterSections::ParameterSectionsVisitor::
       DumpProperties(SectionProperties properties) {
    string text = " (";

    if(properties.IsRead()) text += "r";

    if(properties.IsWritten()) {
        if(properties.IsMust()) {
            text += "wa";
        }
        else text += "w";
    }

    text += " )\n";
    return text;
}

} // namespace Analysis