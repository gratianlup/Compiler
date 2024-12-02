// IRSummary.hpp
// Copyright (c) Lup Gratian
//
// Implements a summary for the code in a function.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_IR_SUMMARY_HPP
#define PC_ANALYSIS_IR_SUMMARY_HPP

#include "Summary.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"

namespace Analysis {

class IRSummary : public Summary {
private:
    int blockCount_;
    int instructionCount_;
    int variableCount_;
    int maxBlockSize_;
    short gotoCount_;
    short ifCount_;
    short switchCount_;
    short returnCount_;
    short constReturnCount_;
    short ifOnParamCount_;
    short switchOnParamCount_;
	short paramReturnCount_;
	short callCount_;
    
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	IRSummary() {
		Reset();
	}

public:
	static const int Id = 0xf95ef567;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static IRSummary* GetIRSummary() {
		return new IRSummary();
	}

	virtual int GetId() const override {
		return Id;
	}

    int BlockCount() const { 
        return blockCount_; 
    }
    
    void SetBlockCount(int value) { 
        blockCount_ = value; 
    }

    int InstructionCount() const { 
        return instructionCount_; 
    }

    void SetInstructionCount(int value) { 
        instructionCount_ = value; 
    }

    int VariableCount() const { 
        return variableCount_; 
    }

    void SetVariableCount(int value) { 
        variableCount_ = value; 
    }

    int MaxBlockSize() const { 
        return maxBlockSize_; 
    }

    void SetMaxBlockSize(int value) { 
        maxBlockSize_ = value; 
    }

    short GotoCount() const { 
        return gotoCount_; 
    }

    void SetGotoCount(short value) { 
        gotoCount_ = value; 
    }

    short IfCount() const { 
        return ifCount_; 
    }

    void SetIfCount(short value) { 
        ifCount_ = value; 
    }

    short SwitchCount() const { 
        return switchCount_; 
    }
    
    void SetSwitchCount(short value) { 
        switchCount_ = value; 
    }

    short ReturnCount() const { 
        return returnCount_; 
    }

    void SetReturnCount(short value) { 
        returnCount_ = value; 
    }

    short ConstantReturnCount() const { 
        return constReturnCount_; 
    }

    void SetConstantReturnCount(short value) { 
        constReturnCount_ = value; 
    }

    short IfOnParameterCount() const { 
        return ifOnParamCount_; 
    }

    void SetIfOnParameterCount(short value) { 
        ifOnParamCount_ = value; 
    }

    short SwitchOnParameterCount() const { 
        return switchOnParamCount_; 
    }

    void SetSwitchOnParameterCount(short value) {
        switchOnParamCount_ = value; 
    }

	short ReturnedParameterCount() const { 
        return paramReturnCount_; 
    }

    void SetReturnedParameterCount(short value) {
		paramReturnCount_ = value; 
    }

	short CallCount() const { 
        return callCount_; 
    }

    void SetCallCount(short value) {
		callCount_ = value; 
    }

	void Reset() {
		blockCount_= 0; 
		instructionCount_= 0; 
		variableCount_= 0;
		maxBlockSize_= 0; 
		gotoCount_= 0; 
		ifCount_= 0; 
		switchCount_= 0; 
		returnCount_= 0; 
		constReturnCount_= 0;
		ifOnParamCount_= 0; 
		switchOnParamCount_ = 0;
		callCount_ = 0;
	}

	virtual string ToStringImpl(int level) const override {
		StringBuilder sb("IR Summary\n");
		sb.AppendFormat(L"    Block count: %d\n", blockCount_);
		sb.AppendFormat(L"    Instruction count: %d\n", instructionCount_);
		sb.AppendFormat(L"    Variable count: %d\n", variableCount_);
		sb.AppendFormat(L"    Max Block size: %d\n", maxBlockSize_);
		sb.AppendFormat(L"    If count: %d\n", ifCount_);
		sb.AppendFormat(L"    Goto count: %d\n", gotoCount_);
		sb.AppendFormat(L"    Switch count: %d\n", switchCount_);
		sb.AppendFormat(L"    If on Param count: %d\n", ifOnParamCount_);
		sb.AppendFormat(L"    Switch on Param count: %d\n", switchOnParamCount_);
		sb.AppendFormat(L"    Return count: %d\n", returnCount_);
		sb.AppendFormat(L"    Constant Return count: %d\n", constReturnCount_);
		sb.AppendFormat(L"    Param Return count: %d\n", paramReturnCount_);
		sb.AppendFormat(L"    Call count: %d\n", callCount_);
		return sb.ToString();
	}
};

} // namespace Analysis

namespace Analysis {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SummaryPromoter<Analysis::IRSummary> {
		static bool Is(const Summary* summary) {
			return summary->GetId() == Analysis::IRSummary::Id;
		}

		static Analysis::IRSummary* As(Summary* summary) {
			return Is(summary) ? static_cast<Analysis::IRSummary*>(summary) : nullptr;
		}
	};
} // namespace Detail
} // namespace Analysis
#endif