// IntrusiveList.hpp
// Copyright (c) Lup Gratian
//
// Defines and implements a doubly-linked list whose elements
// have the 'next' and 'previous' pointers included (intrusive).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_INTRUSIVE_LIST_HPP
#define PC_IR_INTRUSIVE_LIST_HPP

#include "../Base/SharedPointer.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;

namespace IR {

// Stores the 'previous' and 'next' pointer directly into the object.
template <class T>
class IntrusiveHeader {
protected:
	typedef IntrusiveHeader<T> THeader;

	THeader* previous_;
	THeader* next_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	IntrusiveHeader() : previous_(nullptr), next_(nullptr) {}
	IntrusiveHeader(THeader* previous) : previous_(previous), next_(nullptr) {}
	IntrusiveHeader(THeader* previous, THeader* next) : previous_(previous), next_(next) {}

	virtual ~IntrusiveHeader() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the previous item, if any.
	THeader* Previous() {
		return previous_;
	}

	const THeader* Previous() const {
		return previous_;
	}

	void SetPrevious(THeader* value) {
		previous_ = value;
	}

	// Returns the next item, if any.
	THeader* Next() {
		return next_;
	}

	const THeader* Next() const {
		return next_;
	}

	void SetNext(THeader* value) {
		next_ = value;
	}
};


// Provides methods for working with intrusive doubly-linked lists.
template <class T>
class IntrusiveList {
protected:
	typedef IntrusiveHeader<T> THeader;

	bool IsNull(THeader* value) {
		return value == nullptr;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	THeader* first_;
	THeader* last_;
	int count_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	IntrusiveList() : first_(nullptr), last_(nullptr), count_(0) {}

	virtual ~IntrusiveList() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	THeader* First() {
		return first_;
	}

	const THeader* First() const {
		return first_;
	}

	// Returns the last items in the block.
	THeader* Last() {
		return last_;
	}

	const THeader* Last() const {
		return last_;
	}

	// Inserts the specified items at the beginning of the list.
	void InsertFirst(THeader* value) {
		DebugValidator::IsFalse(IsNull(value));
		
		if(first_) {
			value->SetNext(first_);
			first_->SetPrevious(value);
		}
		else last_ = value;

		first_ = value;
		count_++;

        // Postconditions.
        DebugValidator::AreNotEqual(first_->Next(), value);
        DebugValidator::IsNull(first_->Previous());
	}

	// Inserts the specified items at the end of the list.
	void InsertLast(THeader* value) {
		DebugValidator::IsFalse(IsNull(value));
		
		if(last_) {
			last_->SetNext(value);
			value->SetPrevious(last_);
		}
		else first_ = value;

		last_ = value;
		count_++;

        // Postconditions.
        DebugValidator::AreNotEqual(last_->Previous(), value);
        DebugValidator::IsNull(last_->Next());
	}

	void Insert(THeader* value) {
		InsertLast(value);
	}

	// Inserts the specified item after the other one.
	void InsertAfter(THeader* value, THeader* other) {
		DebugValidator::IsLarger(count_, 0);
		DebugValidator::IsFalse(IsNull(value));
		DebugValidator::IsFalse(IsNull(other));
        DebugValidator::AreNotEqual(value, other);
		value->SetNext(other->Next());

		if(other->Next()) {
			other->Next()->SetPrevious(value);
		}

		value->SetPrevious(other);
		other->SetNext(value);
		count_++;

		if(other == last_) {
			last_ = value;
		}
        
        // Postconditions.
        DebugValidator::AreEqual(other->Next(), value);
        DebugValidator::AreEqual(value->Previous(), other);
	}

	// Inserts the specified items before the other one.
	void InsertBefore(THeader* value, THeader* other) {
		DebugValidator::IsLarger(count_, 0);
		DebugValidator::IsFalse(IsNull(value));
		DebugValidator::IsFalse(IsNull(other));
        DebugValidator::AreNotEqual(value, other);
		value->SetPrevious(other->Previous());

		if(other->Previous()) {
			other->Previous()->SetNext(value);
		}

		value->SetNext(other);
		other->SetPrevious(value);
		count_++;

		if(other == first_) {
			first_ = value;
		}

        // Postconditions.
        DebugValidator::AreEqual(other->Previous(), value);
        DebugValidator::AreEqual(value->Next(), other);
	}

	// Removes the specified items from the list.
	void Remove(THeader* value) {
		DebugValidator::IsLarger(count_, 0);
		DebugValidator::IsFalse(IsNull(value));
		
		if((value == first_) && (value == last_)) {
			// This is the only node in the linked list.
			first_ = last_ = nullptr;
		}
		else if(value == first_) {
			value->Next()->SetPrevious(nullptr);
			first_ = value->Next();
		}
		else if(value == last_) {
			value->Previous()->SetNext(nullptr);
			last_ = value->Previous();
		}
		else {
			// The node is in the middle of the linked list.
			value->Previous()->SetNext(value->Next());
			value->Next()->SetPrevious(value->Previous());
		}

        value->SetPrevious(nullptr);
        value->SetNext(nullptr);
		count_--;
	}

public:
	// Removes all items from the linked list.
	void Clear() {
		// Frees the nodes of the list from last to first.
		// This prevents a possible stack-overflow with a large number of instructions.
		THeader* temp = last_;

		while(temp) {
			THeader* tempPrev = temp->Previous();
			if(tempPrev) tempPrev->SetNext(nullptr);
			temp = tempPrev;
		}

		Reset();
	}

	// Resets the list. Note that the elements of the list still remain linked.
	void Reset() {
		first_ = nullptr;
		last_ = nullptr;
		count_ = 0;
	}

	// Returns the number of items in the linked list.
	int Count() const {
		return count_;
	}
};

} // namespace IR
#endif