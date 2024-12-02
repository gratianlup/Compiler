// SideEffectsSummary.hpp
// Copyright (c) Lup Gratian
//
// Implements a summary for the side-effects when calling a function.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SIDE_EFFECTS_SUMMARY_HPP
#define PC_ANALYSIS_SIDE_EFFECTS_SUMMARY_HPP

#include "Summary.hpp"
#include "SparseBitVector.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"

namespace Analysis {

class SideEffectsSummary : public Summary {
private:
	SparseBitVector paramFlags_;
	unsigned isDefinition_   : 1;
	unsigned isNoState_      : 1;
	unsigned isNoIndirWrite_ : 1;
	unsigned isNoIndirRead_  : 1;
	unsigned readsGlobals_   : 1;
	unsigned writesGlobals_  : 1;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	SideEffectsSummary() {
		Reset();
	}

public:
	static const int Id = 0xe0525a58;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static SideEffectsSummary* GetSideEffectsSummary() {
		return new SideEffectsSummary();
	}

	virtual int GetId() const override {
		return Id;
	}

	bool IsDefinition() const {
		return isDefinition_;
	}

	void SetIsDefinition(bool value) {
		isDefinition_ = value;
	}

	bool IsNoState() const {
		return isNoState_;
	}

	void SetIsNoState(bool value) {
		isNoState_ = value;
	}

	bool IsNoIndirectWrite() const {
		return isNoIndirWrite_;
	}

	void SetIsNoIndirectWrite(bool value) {
		isNoIndirWrite_ = value;
	}

	bool IsNoIndirectRead() const {
		return isNoIndirRead_;
	}

	void SetIsNoIndirectRead(bool value) {
		isNoIndirRead_ = value;
	}

	bool ReadsGlobalVariables() const {
		return readsGlobals_;
	}

	void SetReadsGlobalVariables(bool value) {
		readsGlobals_ = value;
	}

	bool WritesGlobalVariables() const {
		return writesGlobals_;
	}

	void SetWritesGlobalVariables(bool value) {
		writesGlobals_ = value;
	}

	bool ParameterIsNoRead(int index) const {
		return paramFlags_.IsSet(index * 3);
	}

	void SetParameterIsNoRead(int index) {
		paramFlags_.SetBit(index * 3);
	}

	bool ParameterIsNoWrite(int index) const {
		return paramFlags_.IsSet(index * 3 + 1);
	}

	void SetParameterIsNoWrite(int index) {
		paramFlags_.SetBit(index * 3 + 1);
	}

	bool ParameterIsNoEscape(int index) const {
		return paramFlags_.IsSet(index * 3 + 2);
	}

	void SetParameterIsNoEscape(int index) {
		paramFlags_.SetBit(index * 3 + 2);
	}

	void Reset() {
		paramFlags_.Clear();
		isDefinition_   = false;
		isNoState_      = false;
		isNoIndirWrite_ = false;
		isNoIndirRead_  = false;
		readsGlobals_   = true; // Wors-case scenario.
		writesGlobals_  = true;
	}
	
	virtual string ToStringImpl(int level) const override {
		StringBuilder sb("Side Effects Summary\n");
		sb.AppendFormat(L"    Is Definition: %d\n", isDefinition_);
		sb.AppendFormat(L"    Is No State: %d\n", isNoState_);
		sb.AppendFormat(L"    Is No Indirect Write: %d\n", isNoIndirWrite_);
		sb.AppendFormat(L"    Is No Indirect Read: %d\n", isNoIndirRead_);
		sb.AppendFormat(L"    Reads Globals: %d\n", readsGlobals_);
		sb.AppendFormat(L"    Writes Globals: %d\n", writesGlobals_);
		return sb.ToString();
	}
};

} // namespace Analysis

namespace Analysis {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SummaryPromoter<Analysis::SideEffectsSummary> {
		static bool Is(const Summary* summary) {
			return summary->GetId() == Analysis::SideEffectsSummary::Id;
		}

		static Analysis::SideEffectsSummary* As(Summary* summary) {
			return Is(summary) ? static_cast<Analysis::SideEffectsSummary*>(summary) : nullptr;
		}
	};
} // namespace Detail
} // namespace Analysis
#endif