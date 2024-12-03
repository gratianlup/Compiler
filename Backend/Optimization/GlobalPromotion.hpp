// GlobalPromotion.hpp
// Copyright (c) Lup Gratian
//
// Implements a pass that tries to promote global variables to constants
// by finding the possible values, computes the address-taken
// and external variables/functions, the list of users, the locations
// of a global variable that are written, and many other facts.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_OPTIMIZATION_GLOBAL_PROMOTION_HPP
#define PC_OPTIMIZATION_GLOBAL_PROMOTION_HPP

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Analysis/CallGraph.hpp"
#include "../Analysis/SparseBitVector.hpp"
#include "../Analysis/AliasInfo.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/ControlDependenceGraph.hpp"
#include "../Analysis/GlobalConstantsTag.hpp"
#include "../Analysis/GlobalUsersTag.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Compilation Pass/Pass.hpp"
using namespace IR;
using namespace Base;
using namespace Analysis;
using namespace CompilationPass;

namespace Optimization {

class GlobalPromotion : public Pass, protected AliasResultProvider {
private:
    // Stores the indices of the addressing instructions
    // that compute the address of an aggregate component.
    // index (index agg, 1), 2  ->  1,2
    struct AccessPath {
        StaticList<int, 2> Indices; // The accessed components.

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        unsigned GetHashCode() const {
            // Uses the FNV hash algorithm. 
            unsigned hash = 2166136261;

            for(int i = 0; i < Indices.Count(); i++) {
                hash = (hash * 16777619) ^ (unsigned)Indices[i];
            }

            return hash;
        }
        
        bool operator== (const AccessPath& other) const {
            if(Indices.Count() != other.Indices.Count()) {
                return false;
            }

            for(int i = 0; i < Indices.Count(); i++) {
                if(Indices[i] != other.Indices[i]) {
                    return false;
                }
            }

            return true;
        }

        bool operator< (const AccessPath& other) const {
            // Compare the common indices.
            int count = std::min(Indices.Count(), other.Indices.Count());

            for(int i = 0; i < count; i++) {
                if(Indices[i] < other.Indices[i]) {
                    return true;
                }
                else if(Indices[i] > other.Indices[i]) {
                    return false;
                }
            }

            return Indices.Count() < other.Indices.Count();
        }
    };


    // Create a pair formed from a constant and the number of times it was found.
    // The pairs can be compared and sorted in a descending order based on 'Times'.
    MAKE_PAIR_ORDERED_SECOND_DESC(ConstantTimesPair, Constant*, Value, int, Times);

    // Create a pair formed from an access path 
    // and a list of associated constants assigned to the path.
    struct PathConstantsPair {
        AccessPath Path;
        StaticList<ConstantTimesPair, 2> Constants;
        StaticList<StoreInstr*, 2> Stores;

        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        PathConstantsPair() {}

        PathConstantsPair(AccessPath path, StoreInstr* store = nullptr) : Path(path) {
            if(store) {
                Stores.Add(store);
            }
        }


        // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        bool operator== (const PathConstantsPair& other) const {
            return Path == other.Path;
        }

        bool operator< (const PathConstantsPair& other) const {
            return Path < other.Path;
        }
    };


    // Stores information collected about a global symbol.
    struct GlobalInfo {
        Reference* TrackedReference;
        bool IsExternal;
        bool HasRead;
        bool HasWrite;
        bool IsAddressTaken;
        bool HasNonConstantWrite;
        bool HasUnknownPositionWrite;
        bool HasUnknownPositionRead;
        SparseBitVector ReadPositions;
        List<PathConstantsPair> Initializers;
        List<UserTimesPair> Users;
        List<FunctionReference*> ReadUsers;
        List<FunctionReference*> WriteUsers;

        GlobalInfo();

        GlobalInfo(Reference* trackedRef) : 
                TrackedReference(trackedRef), IsExternal(false), HasRead(false),
                HasWrite(false), IsAddressTaken(false), HasNonConstantWrite(false),
                HasUnknownPositionWrite(false), HasUnknownPositionRead(false) {}
    };


    typedef StaticList<VariableReference*, 2> VariableReferenceList;
    typedef StaticList<GlobalInfo*, 2> GlobalInfoList;
    typedef Dictionary<Reference*, GlobalInfo*> GlobalInfoDict;
    typedef Dictionary<Operand*, GlobalInfoList> TrackedReferencesDict;
    typedef Dictionary<Reference*, VariableReferenceList> InitializerGlobalsDict;
    typedef Dictionary<AccessPath, bool> AccessPathDict;
    typedef Dictionary<string, bool> StringDictionary;
    typedef List<ConstantTimesPair> ConstantList;
    typedef StaticList<GlobalVariable*, 2> GlobalVariableList;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - -
    Unit* unit_;
    CallGraph* callGraph_;
    AliasInfo* aliasInfo_;
    GlobalInfoDict globalInfo_;
    shared<ControlDependenceGraph> dependenceGraph_;
    Function* dependenceFunction_;
    InitializerGlobalsDict initializerGlobals_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  - -
    // 
    GlobalInfo* GetGlobalInfo(Reference* reference);

    // 
    bool GetGlobalInfo(Operand* op, TrackedReferencesDict& trackedRefs,
                       GlobalInfoList& foundInfos);

    // 
    Reference* AsGlobalReference(Operand* op);

    // 
    void AddUser(GlobalInfo* globalInfo, FunctionReference* functionRef);

    // 
    void AddUser(GlobalInfo* globalInfo, Instruction* instr) {
        AddUser(globalInfo, instr->ParentFunction()->GetReference());
    }

    // 
    void AddReadUser(GlobalInfo* globalInfo, FunctionReference* functionRef) {
        if(globalInfo->ReadUsers.Contains(functionRef) == false) {
            globalInfo->ReadUsers.Add(functionRef);
        }
    }

    // 
    void AddReadUser(GlobalInfo* globalInfo, Instruction* instr) {
        AddReadUser(globalInfo, instr->ParentFunction()->GetReference());
    }

    // 
    void AddWriteUser(GlobalInfo* globalInfo, FunctionReference* functionRef) {
        if(globalInfo->WriteUsers.Contains(functionRef) == false) {
            globalInfo->WriteUsers.Add(functionRef);
        }
    }

    // 
    void AddWriteUser(GlobalInfo* globalInfo, Instruction* instr) {
        AddWriteUser(globalInfo, instr->ParentFunction()->GetReference());
    }

    // 
    void FindInitializerGlobals();

    // 
    void FindInitializerGlobals(Initializer* initializer, 
                                VariableReference* globalVariableRef);

    // 
    void TrackResultOperand(Operand* resultOp, Operand* sourceOp,
                            TrackedReferencesDict& trackedRefs);

    // 
    void ProcessInstructions(Function* function);

    void MarkWithUnknownEffect(Instruction* instr, TrackedReferencesDict& trackedRefs);

    // 
    void ProcessStore(StoreInstr* instr, TrackedReferencesDict& trackedRefs);

    // 
    void ProcessLoad(LoadInstr* instr, TrackedReferencesDict& trackedRefs);

    // 
    void ProcessReturn(ReturnInstr* instr, TrackedReferencesDict& trackedRefs);

    // 
    void ProcessCall(CallInstr* instr, TrackedReferencesDict& trackedRefs);

    // 
    void ProcessCompare(CmpInstrBase* instr, TrackedReferencesDict& trackedRefs);

    // 
    void ProcessPhi(PhiInstr* instr, TrackedReferencesDict& trackedRefs);

    //
    void PatchLoopOperand(PhiInstr* phiInstr, Operand* incomingOp,
                          TrackedReferencesDict& trackedRefs);

    // 
    void ProcessQuestion(QuestionInstr* instr, TrackedReferencesDict& trackedRefs);

    // 
    bool FindConstants(Operand* op, ConstantList& constants,
                       bool& hasOnlyConstants);

    // 
    void AddConstant(Constant* constant, ConstantList& constants);

    // 
    bool FindAccessPath(Operand* op, AccessPath& path, Reference* requiredBase);

    // 
    void AddInitializers(GlobalInfo* globalInfo, AccessPath& path, 
                         ConstantList& constants, StoreInstr* store);

    // 
    void DetermineSideEffects(CallSite* callSite, CallInstr* instr, 
                              GlobalInfoList& argumentGlobals, int parameterIndex, 
                              bool& isRead, bool& isWritten, bool& isEscaped,
                              TrackedReferencesDict& trackedRefs);

    // 
    bool DetermineSideEffectsExternal(CallSite* callSite, CallInstr* instr, 
                                      GlobalInfoList& argumentGlobals, 
                                      int parameterIndex, bool& isRead, 
                                      bool& isWritten, bool& isEscaped,
                                      TrackedReferencesDict& trackedRefs);

    // 
    void DetermineSetMemorySideEffects(CallInstr* instr, GlobalInfoList& globals, 
                                       int parameterIndex, bool& isRead, 
                                       bool& isWritten, bool& isEscaped,
                                       TrackedReferencesDict& trackedRefs);

    // 
    void DetermineCopyMemorySideEffects(CallInstr* instr, GlobalInfoList& globals, 
                                        int parameterIndex, bool& isRead, 
                                        bool& isWritten, bool& isEscaped,
                                        TrackedReferencesDict& trackedRefs);

    void MarkDestinationRead(Operand* destinationOp, 
                             TrackedReferencesDict& trackedRefs);

    // 
    void DetermineGroupSideEffects(CallNodeGroup* nodeGroup, bool& isRead, 
                                   bool& isWritten, bool& isEscaped);

    // 
    void MarkAddressTaken(GlobalInfo* globalInfo);

    // 
    void MarkPassedToUnknown(GlobalInfo* globalInfo);

    // 
    void MarkArgumentGlobal(GlobalInfo* globalInfo, bool isRead, 
                            bool isWritten, bool isEscaped);

    // 
    void PromoteFunctionsToStatic();

    // 
    void PromoteFunctionToStatic(CallNode* callNode);

    // 
    void PromoteVariablesToConstants();

    // 
    bool PromoteVariableToConstant(GlobalVariable* globalVariable, 
                                   GlobalInfo* globalInfo);

    // 
    bool CanBeMarkedConstant(GlobalVariable* globalVariable, 
                             GlobalInfo* globalInfo);

    // 
    bool AddInitializationCandidate(GlobalVariable* globalVariable, 
                                    GlobalInfo* globalInfo, 
                                    GlobalInfoList& initCandidates);

    // 
    void CreateInitializedVariable(GlobalInfo* globalInfo);

    void RemoveInitializationStores(GlobalInfo* globalInfo);

    // 
    bool AreAllUsedElementInitialized(GlobalVariable* globalVariable, 
                                      GlobalInfo* globalInfo);

    // 
    bool IsAggregateInitialized(const Type* type, GlobalInfo* globalInfo);

    // 
    bool IsSimpleArrayInitialized(const ArrayType* arrayType, 
                                  GlobalInfo* globalInfo);

    // 
    bool IsNeverReadFrom(int index, GlobalInfo* globalInfo);

    // 
    bool AreAllElementsInitialized(const Type* type, AccessPathDict& accessPaths,
                                   AccessPath& requiredPath);

    // 
    bool CanBeInitialized(GlobalVariable* globalVariable,
                          GlobalInfo* globalInfo);

    // 
    bool InitializerWritesDominateReads(GlobalInfoList& candidates);

    // 
    bool InitializerWritesDominateReads(GlobalInfo* globalInfo, CallNode* rootNode);

    // 
    bool AreDependentOnSameCondition(CallSite* callSite, CallNode* rootNode);

    // 
    void CreateControlDependenceGraph(Function* function);

    // 
    bool IsAlwaysExecuted(Instruction* instr);

    // 
    void DisqualifySameBlockReadWriteCandidates(GlobalInfoList& candidates);

    // 
    bool AreAllWritesInUniqueEntryFunction(GlobalInfo* candidate);

    // 
    bool AreAllWritesInFunction(CallNodeBase* node, GlobalInfo* candidate);

    // 
    bool AreAllReadsInStaticFunctions(GlobalInfo* candidate);

    // 
    bool FunctionReadsCandidate(CallNodeBase* rootNode, 
                                GlobalInfo* candidate, int level = 0);

    // 
    bool CallSiteReadsCandidate(CallSite* callSite, GlobalInfo* candidate);

    // 
    CallNode* GetMultipleCallsRoot();

    // 
    void InitializeSimpleArray(const ArrayType* arrayType,
                               GlobalVariable* globalVariable,
                               GlobalInfo* globalInfo);

    // 
    void InitializeAggregate(GlobalVariable* globalVariable,
                             GlobalInfo* globalInfo);

    // 
    shared<Initializer> CreateAggregateInitializer(const Type* type, 
                                                   GlobalInfo* globalInfo, 
                                                   int& currentPosition);

    // 
    Constant* GetDefaultConstant(const Type* type);

    // 
    void CreateGlobalConstantsTag(GlobalVariable* globalVariable,
                                  GlobalInfo* globalInfo);

    // 
    void RemoveCallsToUnknown();

    // 
    void RemoveCallToUnknown(CallSite* callSite, 
                             GlobalVariableList& targetGlobals);

    // 
    bool FindVariableCallTargets(Operand* op, GlobalVariableList& globals);

    // 
    bool AddGlobalVariable(Operand* op, GlobalVariableList& globals);

    // 
    void RemoveDeadGlobals();

    // 
    bool CanVariableBeRemoved(GlobalInfo* globalInfo) {
        return (globalInfo->IsExternal == false) &&
               (globalInfo->IsAddressTaken == false) &&
               globalInfo->Users.IsEmpty();
    }

    // 
    bool CanFunctionBeRemoved(GlobalInfo* globalInfo) {
        return globalInfo->Users.IsEmpty();
    }

    // 
    void AttachUsersToGlobal(Reference* reference);

    // 
    bool IsStaticFunction(FunctionReference* functionRef, GlobalInfo* globalInfo);

    // 
    bool IsStaticPromotableFunction(FunctionReference* functionRef, GlobalInfo* global, 
                                    bool promoteAll, FunctionReference* entryPointRef);

    void Dump();

    // Methods that implement the AliasResultProvider interface.
    virtual bool MightBeAlias(Operand* a, Operand* b) override;
    virtual bool IsDefinitelyNoAlias(Operand* a, Operand* b) override;

    // Methods that implement the SafetyInfoClient interface.
    virtual ControlDependenceGraph* ControlDependenceGraphRequest(Function* function) override;

public:
    void Execute(Unit* unit, CallGraph* callGraph, AliasInfo* aliasInfo);
};

} // namespace Optimization
#endif