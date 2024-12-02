// StaticStack.hpp
// Copyright (c) Lup Gratian
//
// Represents a fixed-size stack.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STATIC_STACK_HPP
#define PC_BASE_STATIC_STACK_HPP

#include "DebugValidator.hpp"

namespace Base {

template <class T, int Size, class Validator = DebugValidator>
class StaticStack {
private:
	T array_[Size];
	int count_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	typedef typename StaticStack<T, Size, Validator> TStack;
	typedef typename T Type;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const int SIZE = Size;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StaticStack() : count_(0) {}

	~StaticStack() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Inserts an object at the top of the stack.
	void Push(const T& item) {
		Validator::IsSmallerOrEqual(count_ + 1, Size);
		array_[count_++] = item;
	}

	// Removes and returns the object at the top of stack.    
	T Pop() {
		Validator::IsLarger(count_, 0);
		return array_[--count_];
	}

	// Returns the object at the top of the stack without removing it.
	T& Peek() {
		Validator::IsLarger(count_, 0);
		return array_[count_ - 1];
	}

	// Gets the number of items in the stack.
	int Count() const {
		return count_;
	}

	// Removes all items from the Stack.
	void Clear() {
		for(int i = 0; i < count_; i++) {
			((T*)&array_[i])->~T(); // Call destructor.
		}

		count_ = 0;
	}

	// Determines whether an item is in the Stack.
	bool Contains(const T& item) const {
		for(int i = 0; i < count_; i++) {
			if(array_[i] == item) {
				return true;
			}
		}

		return false;
	}

	// Copies the entire Stack to an array, starting at a particular array index.
	void CopyTo(T* dest, int index) const {
		for(int i = 0; i < count_; i++) {
			dest[index + i] = array_[i];
		}
	}

	// Pops while the specified condition is true.
	template <class Predicate>
	void PopWhile(Predicate condition) {
		while(count_ > 0) {
			if(condition(Peek())) {
				Pop();
			}
			else break;
		}
	}

	// Pops while the specified condition is false.
	template <class Predicate>
	void PopUntil(Predicate condition) {
		while(count_ > 0) {
			if(condition(Peek()) == false) {
				Pop();
			}
			else break;
		}
	}
};

} // namespace Base
#endif