// PointerParameterSections.hpp
// Copyright (c) Lup Gratian
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_POINTER_PARAMETER_SECTIONS_HPP
#define PC_ANALYSIS_POINTER_PARAMETER_SECTIONS_HPP

#include "CallGraph.hpp"
#include "SparseBitVector.hpp"
#include "ParameterAliasTag.hpp"
#include "GlobalConstantsTag.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

class PointerParameterSections : public Pass {
private:
    // Stores information about a tracked parameter
    // or about an operand that originated from a parameter.
    struct TrackedOperandInfo {
        Operand* TrackedOperand;     // The tracked operand.
        Parameter* TrackedParameter; // The associated parameter.
        int ParameterIndex;          // The index of the associated parameter.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        TrackedOperandInfo() {}

        TrackedOperandInfo(Operand* op, Parameter* parameter, int index) :
                TrackedOperand(op), TrackedParameter(parameter),
                 ParameterIndex(index) {}
    };


    // Class that visits the Call Graph nodes in postorder
    // (first the callees, then the callers).
    class ParameterSectionsVisitor : public CallNodeVisitor {
    private:
        typedef List<TrackedOperandInfo> TrackedOperandsList;
        typedef StaticList<Parameter*, 4> ParameterList;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        PointerParameterSections* parent_;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        Function* GetFunctionDefinition(CallNode* node);

        void MakeTrackedParameterList(Function* function, 
                                      TrackedOperandsList& trackedOps);

        void TrackResultOperand(Operand* resultOp, Operand* sourceOp,
                                TrackedOperandsList& trackedOps);

        void ProcessInstructions(Function* function, CallGraph* callGraph,
                                 TrackedOperandsList& trackedOps);

        void ProcessLoad(LoadInstr* instr, TrackedOperandsList& trackedOps);

        void ProcessStore(StoreInstr* instr, TrackedOperandsList& trackedOps);

        void ProcessCall(CallInstr* instr, TrackedOperandsList& trackedOps);

        void ProcessQuestion(QuestionInstr* instr, TrackedOperandsList& trackedOps);

        void ProcessPhi(PhiInstr* instr, TrackedOperandsList& trackedOps);

        void ProcessIntrinsic(Intrinsic* intrinsic, CallInstr* instr,
                              TrackedOperandsList& trackedOps);

        bool CreateSizeElementSection(Operand* sizeOp, Function* function,
                                      ElementSection& sizeElement);

        void CreateMemoryAccessSection(Operand* accessedOp, bool isWrite, 
                                       bool hasKnownSize, ElementSection& sizeElement,
                                       Instruction* instr, TrackedOperandsList& trackedOps);

        bool FindTrackedParameters(Operand* op, ParameterList& parameters,
                                   AddressInstr*& lastAddressingInstr, bool& hasPtop,
                                   TrackedOperandsList& trackedOps);

        void CreateParameterSections(ParameterList& parameters, Instruction* accessInstr,
                                     AddressInstr* lastAddressingInstr);

        bool CreateElementSection(Operand* baseOp, Operand* factorOp, 
                                  Opcode operation, Operand* adjustment,
                                  Function* function, ElementSection& result);

        void MarkNoFactorElement(Operand* baseOp, Operand* adjustmentOp, 
                                 ParameterList& parameters, Instruction* accessInstr);

        bool MarkParameterFactorElement(Operand* baseOp, Operand* factorOp, 
                                        Opcode operation, Operand* adjustmentOp, 
                                        ParameterList& parameters, 
                                        Instruction* accessInstr);

        bool MarkConstantFactorElement(Operand* baseOp, Operand* factorOp, 
                                       Opcode operation, Operand* adjustmentOp, 
                                       ParameterList& parameters, 
                                       Instruction* accessInstr);

        bool CreateVariableWithFactorElement(Operand* baseOp, Operand* factorOp, 
                                             Opcode operation, Operand* adjustmentOp, 
                                             ParameterList& parameters, 
                                             Instruction* accessInstr);

        bool CreateVariableWithNoFactorElement(Operand* baseOp, Operand* adjustmentOp, 
                                               ParameterList& parameters,  
                                               Instruction* accessInstr);

        bool CreateGlobalVariableSections(GlobalVariable* globalVariable, 
                                          Operand* adjustmentOp, 
                                          ParameterList& parameters, 
                                          Instruction* accessInstr);

        bool CreatePhiSections(PhiInstr* phiInstr, Operand* adjustmentOp,
                               ParameterList& parameters, Instruction* accessInstr);

        bool AllWithUnknownSections(ParameterList& parameters);

        void MarkAllWithUnknownSections(ParameterList& parameters);

        void MarkAccessedSection(ParameterList& parameters, ElementSection element,
                                 Instruction* accessInstr, bool forceMay = false);

        void MarkAccessedRange(ParameterList& parameters, RangeSection range,
                               Instruction* accessInstr, bool isWrite, bool isByte);

        bool FindSectionComponents(Operand* indexOp, Operand*& baseOp,
                                   Operand*& factorOp, Opcode& operation,
                                   Operand*& adjustment);

        int GetParameterIndex(Parameter* parameter, Function* function);

        bool IsAlwaysExecuted(Instruction* instr) {
            return instr->ParentBlock()->IsFunctionEntry();
        }

        ParameterAliasTag* GetParameterAliasTag(Parameter* parameter);

        bool MightBeAlias(Operand* opA, Operand* opB);

        const IntConstant* GetZeroInt(Unit* unit) {
            return unit->Constants().GetInt32(0);
        }
        
        const IntConstant* GetOneInt(Unit* unit) {
            return unit->Constants().GetInt32(1);
        }

        string DumpElement(ElementSection section);

        string DumpProperties(SectionProperties properties);

        void Dump(Function* function);

    public:
        ParameterSectionsVisitor(PointerParameterSections* parent) : 
                parent_(parent) {}

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        virtual bool Visit(CallNode* node, CallGraph* callGraph) override;

        virtual bool Visit(CallNodeGroup* nodeGroup, CallGraph* callGraph) override;
    };

public:
    void Execute(CallGraph* callGraph);
};

} // namespace Analysis
#endif




// use ParameterAliasTag
// bitvector for constant elements
// simple MAY/MUST (entry block or simple postdominance estimate)
// if we know MUST we can eliminate before stores to the MUST target
//    a[0], a[3], ...
//
// MUST_IF a certain condition is true
// for simple cases like p > C (param, order, constant/param)
// 
// single parameters with possible adjustment as + - * /
//    a[p], a[p + 2], a[p * 4], a[(p * 3) + 1] ...
//    (a OP b) + C
// 
// ranges of the form [low, high, step], without step if not constant
// [0, 30], [0, n], [0, n + 2]
