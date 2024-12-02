// Stack.hpp
// Copyright (c) Lup Gratian
//
// Represents a stack based on a linked list.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STACK_HPP
#define PC_BASE_STACK_HPP

#include "LinkedList.hpp"
#include "DebugValidator.hpp"

namespace Base {

template <class T, class Validator = DebugValidator>
class Stack : protected LinkedList<T, Validator> {
public:
	typedef typename Stack<T, Validator> TStack;
	typedef typename T Type;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes an empty stack.
	Stack() : LinkedList() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Inserts an object at the top of the stack.
	void Push(const T& item) {
		AddLast(item);
	}

	// Removes and returns the object at the top of stack.    
	T Pop() {
		Validator::IsLarger(count_, 0);
		T item = last_->Value;
		RemoveLast();
		return item;
	}
	
	// Returns the object at the top of the stack without removing it.
    // If 'position' is different from zero it returns the
    // element found at the specified position relative to the top.
	T& Peek(int position = 0) {
		Validator::IsLarger(count_, 0);

        auto element = last_;

        while(position > 0) {
            DebugValidator::IsNotNull(element->Previous);
            element = element->Previous;
            position--;
        }

		return element->Value;
	}

	// Gets the number of items in the stack.
	int Count() const {
		return LinkedList::Count();
	}

	// Removes all items from the Stack.
	void Clear() {
		LinkedList::Clear();
	}
	
	// Determines whether an item is in the Stack.
	bool Contains(const T& item) const {
		return LinkedList::Contains(item);
	}

	// Copies the entire Stack to an array, starting at a particular array index.
	void CopyTo(T* dest, int index) const {
		return LinkedList::CopyTo(dest, index);
	}

    // Performs the specified action on each item.
    template <class Predicate>
    void ForEach(Predicate action) {
        LinkedList::ForEach(action);
    }

    template <class Predicate>
    void ForEach(Predicate action) const {
        LinkedList::ForEach(action);
    }
};

} // namespace Base
#endif