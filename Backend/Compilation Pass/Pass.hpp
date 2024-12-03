// Pass.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMPILATION_PASS_PASS_HPP
#define PC_COMPILATION_PASS_PASS_HPP

#include "../Targets/TargetInfo.hpp"
#include "../Analysis/LanguageInfo.hpp"
#include "../Analysis/CFamilyLanguageInfo.hpp"
#include "../Analysis/SafetyInfo.hpp"
#include "../Analysis/CallGraph.hpp"
#include "../IR/Instruction.hpp"
using namespace Target;
using namespace Analysis;

//enum PassFamily { Analysis, Loop, Redundancy, CFG, Scalar, Interprocedural, etc }

//? pass should report if anything changed

namespace CompilationPass {

class MockTarget : public TargetInfo {
    virtual IR::IRIntegerKind GetPointerType() const override {
        return IR::IRIntegerKind::Int64;
    }

    // Returns the size (in bytes) of the pointer type.
    virtual int GetPointerSize() const override {
        return 8;
    }

    // Returns the size (in bits) of the pointer type.
    virtual int GetPointerSizeInBits() const override {
        return 64;
    }

    // Returns the default alignment for the specified integer type.
    virtual int GetAlignment(IR::IRIntegerKind kind) const override {
        return 4;
    }

    // Returns the default alignment for the specified floating type.
    virtual int GetAlignment(IR::IRFloatingKind kind) const override {
        return 8;
    }

    // Returns the default alignment for the pointer type.
    virtual int GetPointerAlignment() const override {
        return 8;
    }

    virtual int AvailableRegisters() const override {
        return 6;
    }

    virtual int MultimediaAlignment() const override {
        return 16;
    }

    // Returns the required alignment for variables the are used
    // by the multimedia instructions of the architecture.
    virtual int EstimatedInstructionLatency(IR::Instruction* instr) const override {
        return 1;
    }

    // Returns an estimate for the latency of the instruction.
    // The minimum value should be 1.
    virtual int EstimatedInstructionSize(IR::Instruction* instr) const override {
        return 1;
    }
};


class Pass : public SafetyInfoClient {
private:
	TargetInfo* target_;
    CFamilyLanguageInfo languageInfo_;
    SafetyInfo safetyInfo_;
	CallGraph* callGraph_;

    // Implements the SafetyInfoClient interface.
    virtual IRDominatorTree* DominatorTreeRequest(Function* function) override {
        //! TODO: take the domtree if available
        return nullptr;
    }

public:
    Pass() : target_(new MockTarget()), safetyInfo_(this), callGraph_(nullptr) {}

    virtual ~Pass() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const TargetInfo* GetTarget() const {
		return target_;
	}

	TargetInfo* GetTarget() {
		return target_;
	}

    LanguageInfo* GetLanguageInfo() {
        return &languageInfo_;
    }

    SafetyInfo* GetSafetyInfo() {
        return &safetyInfo_;
    }

	CallGraph* GetCallGraph() {
		return callGraph_;
	}

	void SetCallGraph(CallGraph* graph) {
		callGraph_ = graph;
	}

    // Initialize
    // Reset ?
    // Destroy
};

} // namespace CompilationPass
#endif