// Queue.hpp
// Copyright (c) Lup Gratian
//
// Represents a queue based on a linked list.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_QUEUE_HPP
#define PC_BASE_QUEUE_HPP

#include "LinkedList.hpp"
#include "DebugValidator.hpp"

namespace Base {

template <class T, class Validator = DebugValidator>
class Queue : protected LinkedList<T, Validator> {
public:
	typedef typename Queue<T, Validator> TQueue;
	typedef typename T Type;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes an empty stack.
	Queue() : LinkedList() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds an object to the end of the queue.
	void Enqueue(const T& item) {
		AddLast(item);
	}

	// Removes and returns the object at the beginning.
	T Dequeue() {
		Validator::IsLarger(count_, 0);
		T item = first_->Value;
		RemoveFirst();
		return item;
	}
	
	// Returns the object at the beginning of the queue without removing it.
	T Peek() {
		Validator::IsLarger(count_, 0);
		return first_->Value;
	}

	// Gets the number of items in the queue.
	__int64 Count() const {
		return LinkedList::Count();
	}

    // Returns 'true' if the queue contains elements.
    bool IsNotEmpty() const {
        return LinkedList::Count() > 0;
    }

    // Returns 'true' if the queue contains no elements.
    bool IsEmpty() const {
        return LinkedList::Count() == 0;
    }

	// Removes all items from the queue.
	void Clear() {
		LinkedList::Clear();
	}
	
	// Determines whether an item is in the queue.
	bool Contains(const T& item) const {
		return LinkedList::Contains(item);
	}

	// Copies the entire queue to an array, starting at a particular array index.
	void CopyTo(T* dest, __int64 index) const {
		return LinkedList::CopyTo(dest, index);
	}

	// Dequeues while the specified condition is true.
	template <class Predicate>
	void DequeueWhile(Predicate condition) {
		while(count_ > 0) {
			if(condition(Peek())) {
				Dequeue();
			}
			else break;
		}
	}

	// Dequeues while the specified condition is false;
	template <class Predicate>
	void DequeueUntil(Predicate action) {
		while(count_ > 0) {
			if(action(Peek()) == false) {
				Dequeue();
			}
			else break;
		}
	}
};

} // namespace Base
#endif