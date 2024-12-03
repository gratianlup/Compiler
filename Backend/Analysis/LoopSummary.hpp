// LoopSummary.hpp
// Copyright (c) Lup Gratian
//
// Implements a summary for the loops in a function.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_LOOP_SUMMARY_HPP
#define PC_ANALYSIS_LOOP_SUMMARY_HPP

#include "Summary.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"

namespace Analysis {

class LoopSummary : public Summary {
private:
	short loopCount_;
	short maxLoopDepth_;
	short maxLoopBlocks_;
	short maxLoopChildren_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	LoopSummary() {
		Reset();
	}

public:
	static const int Id = 0x38120984;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static LoopSummary* GetLoopSummary() {
		return new LoopSummary();
	}

	virtual int GetId() const override {
		return Id;
	}

	int LoopCount() const { 
		return loopCount_; 
    }
    
    void SetLoopCount(int value) { 
        loopCount_= value; 
    }

	int MaximumLoopDepth() const { 
		return maxLoopDepth_; 
    }
    
    void SetMaximumLoopDepth(int value) { 
        maxLoopDepth_= value; 
    }

	int MaximumLoopBlocks() const { 
		return maxLoopBlocks_; 
    }
    
    void SetMaximumLoopBlocks(int value) { 
        maxLoopBlocks_= value; 
    }

	int MaximumLoopChildren() const { 
		return maxLoopChildren_; 
    }
    
    void SetMaximumLoopChildren(int value) { 
        maxLoopChildren_= value; 
    }

	void Reset() {
		loopCount_ = 0;
		maxLoopDepth_ = 0;
		maxLoopBlocks_ = 0;
		maxLoopChildren_ = 0;
	}

	virtual string ToStringImpl(int level) const override {
		StringBuilder sb("Loop Summary\n");
		sb.AppendFormat(L"    Loop count: %d\n", loopCount_);
		sb.AppendFormat(L"    Max loop depth: %d\n", maxLoopDepth_);
		sb.AppendFormat(L"    Max loop blocks: %d\n", maxLoopBlocks_);
		sb.AppendFormat(L"    Max loop children: %d\n", maxLoopChildren_);
		return sb.ToString();
	}
};

} // namespace Analysis

namespace Analysis {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SummaryPromoter<Analysis::LoopSummary> {
		static bool Is(const Summary* summary) {
			return summary->GetId() == Analysis::LoopSummary::Id;
		}

		static Analysis::LoopSummary* As(Summary* summary) {
			return Is(summary) ? static_cast<Analysis::LoopSummary*>(summary) : nullptr;
		}
	};
} // namespace Detail
} // namespace Analysis
#endif