// SummaryManager.hpp
// Copyright (c) Lup Gratian
//
// Implements the module that manages all function summaries.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SUMMARY_MANAGER_HPP
#define PC_ANALYSIS_SUMMARY_MANAGER_HPP

#include "Summary.hpp"
#include "CallGraph.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Function.hpp"
#include "../IR/References.hpp"
#include "../Base/Log.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/DebugValidator.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class SummaryManager : public UnitObserver {
private:
	typedef StaticList<Summary*, 2> SummaryList;
	typedef Dictionary<FunctionReference*, SummaryList*> SummaryDictionary;

	SummaryDictionary summaries_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	SummaryList* GetList(FunctionReference* functionRef) {
		SummaryList* list;

		if(summaries_.TryGetValue(functionRef, &list)) {
			return list;
		}
		else return nullptr;
	}

	const SummaryList* GetList(FunctionReference* functionRef) const {
		SummaryList* list;

		if(summaries_.TryGetValue(functionRef, &list)) {
			return list;
		}
		else return nullptr;
	}

	SummaryList* GetOrCreateList(FunctionReference* functionRef) {
		SummaryList* list;

		if(summaries_.TryGetValue(functionRef, &list) == false) {
			list = new SummaryList();
			functionRef->AddUser();
			summaries_.Add(functionRef, list);
		}

		return list;
	}

	virtual void FunctionRemoved(Function* funct) {
		// The summaries are no longer needed.
		RemoveSummaries(funct->GetReference());
	}

public:
	~SummaryManager() {
		RemoveAllSummaries();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Adds the specified summary to the list associated with the function reference.
    // Note that each summary class should be represented by a single summary.
	void AddSummary(Summary* summary, FunctionReference* functionRef) {
		DebugValidator::IsNotNull(summary);
		DebugValidator::IsNotNull(functionRef);
		DebugValidator::IsFalse(HasSummary(summary->GetId(), functionRef));

		auto list = GetOrCreateList(functionRef);
		list->Add(summary);
	}

	void AddSummary(Summary* summary, CallNode* callNode) {
		DebugValidator::IsNotNull(summary);
		DebugValidator::IsNotNull(callNode);
		AddSummary(summary, callNode->GetFunctionReference());
	}

	// Returns 'true' if a summary having the specified class 
	// is associated with the function reference.
	template <class T>
	bool HasSummary(FunctionReference* functionRef) const {
		DebugValidator::IsNotNull(functionRef);

		if(auto list = GetList(functionRef)) {
			for(int i = 0; i < list->Count(); i++) {
				if((*list)[i]->Is<T>()) {
					return true;
				}
			}
		}

		return false;
	}

	template <class T>
	bool HasSummary(CallNode* callNode) const {
		DebugValidator::IsNotNull(callNode);
		return HasSummary<T>(callNode->GetFunctionReference());
	}

	// Returns 'true' if a summary having the specified Id
	// is associated with the function reference.
	bool HasSummary(int summaryId, FunctionReference* functionRef) const {
		DebugValidator::IsNotNull(functionRef);

		if(auto list = GetList(functionRef)) {
			for(int i = 0; i < list->Count(); i++) {
				if((*list)[i]->GetId() == summaryId) {
					return true;
				}
			}
		}

		return false;
	}

	bool HasSummary(int summaryId, CallNode* callNode) const {
		DebugValidator::IsNotNull(callNode);
		return HasSummary(summaryId, callNode->GetFunctionReference());
	}

	// Returns the number of summaries associated
	// with the specified function reference.
	int SummaryCount(FunctionReference* functionRef) const {
		DebugValidator::IsNotNull(functionRef);

		if(auto list = GetList(functionRef)) {
			return list->Count();
		}

		return 0;
	}

	int SummaryCount(CallNode* callNode) const {
		DebugValidator::IsNotNull(callNode);
		return SummaryCount(callNode->GetFunctionReference());
	}

	// Returns 'true' if there are summaries associated
	// with the specified function reference.
	bool HasSummaries(FunctionReference* functionRef) const {
		return SummaryCount(functionRef) > 0;
	}

	bool HasSummaries(CallNode* callNode) const {
		DebugValidator::IsNotNull(callNode);
		return HasSummaries(callNode->GetFunctionReference());
	}

	// Returns the summary having the specified class
	// associated with the function reference, or 'nullptr'
	// if such a summary doesn't exist.
	template <class T>
	T* GetSummary(FunctionReference* functionRef) {
		DebugValidator::IsNotNull(functionRef);

		if(auto list = GetList(functionRef)) {
			for(int i = 0; i < list->Count(); i++) {
				if(auto summary = (*list)[i]->As<T>()) {
					return summary;
				}
			}
		}

		return nullptr;
	}

	template <class T>
	T* GetSummary(CallNode* callNode) {
		DebugValidator::IsNotNull(callNode);
		return GetSummary<T>(callNode->GetFunctionReference());
	}

	// Returns the summary having the specified class
	// associated with the function reference, or 'nullptr'
	// if such a summary doesn't exist (constant version).
	template <class T>
	const T* GetSummary(FunctionReference* functionRef) const {
		DebugValidator::IsNotNull(functionRef);

		if(auto list = GetList(functionRef)) {
			for(int i = 0; i < list->Count(); i++) {
				if(auto summary = (*list)[i]->As<T>()) {
					return summary;
				}
			}
		}

		return nullptr;
	}

	template <class T>
	const T* GetSummary(CallNode* callNode) const {
		DebugValidator::IsNotNull(callNode);
		return GetSummary<T>(callNode->GetFunctionReference());
	}

	// Returns the summary found at the specified position
	// which is associated with the function reference.
	Summary* GetSummary(int index, FunctionReference* functionRef) {
		DebugValidator::IsNotNull(functionRef);
		DebugValidator::IsTrue(HasSummaries(functionRef));

		auto list = GetList(functionRef);
		return (*list)[index];
	}

	Summary* GetSummary(int index, CallNode* callNode) {
		DebugValidator::IsNotNull(callNode);
		return GetSummary(index, callNode->GetFunctionReference());
	}

	// Returns the summary found at the specified position
	// which is associated with the function reference (constant version).
	const Summary* GetSummary(int index, FunctionReference* functionRef) const {
		DebugValidator::IsNotNull(functionRef);
		DebugValidator::IsTrue(HasSummaries(functionRef));

		auto list = GetList(functionRef);
		return (*list)[index];
	}

	const Summary* GetSummary(int index, CallNode* callNode) const {
		DebugValidator::IsNotNull(callNode);
		return GetSummary(index, callNode->GetFunctionReference());
	}

	// Performs the specified action on each summary
	// associated with the function reference.
	// bool Predicate(Summary* summary);
	template <class Predicate>
    void ForEachSummary(FunctionReference* functionRef, Predicate action) {
		DebugValidator::IsNotNull(functionRef);

        if(auto list = GetList(functionRef)) {
			for(int i = 0; i < list->Count(); i++) {
				if(action((*list)[i]) == false) {
					return;
				}
			}
        }
    }

	template <class Predicate>
    void ForEachSummary(CallNode* callNode, Predicate action) {
		DebugValidator::IsNotNull(callNode);
		ForEachSummary<Predicate>(callNode->GetFunctionReference(), action);
	}

	// Performs the specified action on each summary
	// associated with the function reference (constant version).
	// bool Predicate(Summary* summary);
	template <class Predicate>
    void ForEachSummary(FunctionReference* functionRef, Predicate action) const {
		DebugValidator::IsNotNull(functionRef);

        if(auto list = GetList(functionRef)) {
			for(int i = 0; i < list->Count(); i++) {
				if(action((*list)[i]) == false) {
					return;
				}
			}
        }
    }

	template <class Predicate>
    void ForEachSummary(CallNode* callNode, Predicate action) const {
		DebugValidator::IsNotNull(callNode);
		ForEachSummary<Predicate>(callNode->GetFunctionReference(), action);
	}

	// Removes all summaries associated with the specified function reference.
	void RemoveSummaries(FunctionReference* functionRef) {
		DebugValidator::IsNotNull(functionRef);

		if(auto list = GetList(functionRef)) {
			while(list->IsNotEmpty()) {
				auto summary = list->RemoveLast();
				summary->Free();
			}

			summaries_.Remove(functionRef);
			functionRef->Free();
			delete list;
		}
	}

	void RemoveSummaries(CallNode* callNode) {
		DebugValidator::IsNotNull(callNode);
		RemoveSummaries(callNode->GetFunctionReference());
	}

	// Removes the summary having the specified class
	// which is associated with the function reference.
	// Returns 'true' if such a summary was found and removed.
	template <class T>
	bool RemoveSummary(FunctionReference* functionRef) {
		DebugValidator::IsNotNull(functionRef);

		if(auto list = GetList(functionRef)) {
			for(int i = 0; i < list->Count(); i++) {
				auto summary = (*list)[i];

				if(summary->Is<T>()) {
					list.RemoveAt(i);
					summary->Free();
					return true;
				}
			}
		}

		return false;
	}

	template <class T>
	bool RemoveSummary(CallNode* callNode) {
		DebugValidator::IsNotNull(callNode);
		RemoveSummary<T>(callNode->GetFunctionReference());
	}

	// Remove the specified summary which is associated
	// with the function reference.
	void RemoveSummary(Summary* summary, FunctionReference* functionRef) {
		DebugValidator::IsNotNull(summary);
		DebugValidator::IsNotNull(functionRef);
		DebugValidator::IsTrue(HasSummaries(functionRef));
	
		auto list = GetList(functionRef);
		list->Remove(summary);
		summary->Free();
	}
	
	void RemoveSummary(Summary* summary, CallNode* callNode) {
		DebugValidator::IsNotNull(callNode);
		RemoveSummary(summary, callNode->GetFunctionReference());
	}

	// Removes all summaries associated with any function.
	void RemoveAllSummaries() {
		summaries_.ForEach([](SummaryDictionary::TPair& pair) -> bool {
			pair.Value->ForEach([](Summary* summary) -> bool {
				summary->Free();
				return true;
			});

			delete pair.Value;
			pair.Key->Free();
			return true;
		});

		summaries_.Clear();
	}

	// Removes all summaries having the specified class 
	// associated any function.
	template <class T>
	void RemoveAllSummaries() {
		summaries_.ForEachValue([](SummaryList* list) -> bool {
			for(int i = 0; i < list->Count(); i++) {
				auto summary = (*list)[i];

				if(summary->Is<T>()) {
					summary->Free();
					list->RemoveAt(i);
					i--;
				}
			}

			return true;
		});
	}

	string ToString() const {
		StringBuilder sb;

		summaries_.ForEach([&sb](SummaryDictionary::TPair& pair) -> bool {
			auto functionName = pair.Key->Target()->Name();
			sb.AppendLine("--------------------------------");
			sb.Append("FUNCTION ");
			sb.AppendLine(*functionName + ": ");

			pair.Value->ForEach([&sb](Summary* summary) -> bool {
				sb.AppendLine(summary->ToString());
				sb.AppendLine();
				return true;
			});

			return true;
		});

		return sb.ToString();
	}

	void Dump() {
		ObjectDumper(ToString(), "Summary Manager - All summaries").Dump();
	}
};

} // namespace Analysis
#endif