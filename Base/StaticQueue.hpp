// StaticQueue.hpp
// Copyright (c) Lup Gratian
//
// Represents a fixed-size queue based on an array.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STATIC_QUEUE_HPP
#define PC_BASE_STATIC_QUEUE_HPP

#include "DebugValidator.hpp"

namespace Base {

template <class T, int Size, class Validator = DebugValidator>
class StaticQueue {
private:
	T array_[Size];
	int count_;
	int first_;
	int last_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	typedef typename StaticQueue<T, Size, Validator> TQueue;
	typedef typename T Type;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const int SIZE = Size;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StaticQueue() : count_(0), first_(0), last_(0) {}

	~StaticQueue() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds an object to the end of the queue.
	void Enqueue(const T& item) {
		Validator::IsSmallerOrEqual(count_ + 1, Size);
		array_[last_] = item;
		last_ = (last_ + 1) % Size;
		count_++;
	}

	// Removes and returns the object at the beginning.
	T Dequeue() {
		Validator::IsLarger(count_, 0);
		T item = array_[first_];
		first_ = (first_ + 1) % Size;
		count_--;
		return item;
	}

	// Returns the object at the beginning of the queue without removing it.
	T Peek() const {
		Validator::IsLarger(count_, 0);
		return array_[first_];
	}

	// Gets the number of items in the queue.
	int Count() const {
		return count_;
	}

	// Removes all items from the queue.
	void Clear() {
		if(first_ <= last_) {
			for(int i = first_; i < last_; i++) {
				((T*)&array_[i])->~T(); // Call destructor.
			}
		}
		else {
			for(int i = first_; i < Size; i++) {
				((T*)&array_[i])->~T(); // Call destructor.
			}

			for(int i = 0; i < last_; i++) {
				((T*)&array_[i])->~T(); // Call destructor.
			}
		}

		count_ = first_ = last_ = 0;
	}

	// Determines whether an item is in the queue.
	bool Contains(const T& item) const {
		if(first_ <= last_) {
			for(int i = first_; i < last_; i++) {
				if(array_[i] == item) {
					return true;
				}
			}
		}
		else {
			for(int i = first_; i < Size; i++) {
				if(array_[i] == item) {
					return true;
				}
			}

			for(int i = 0; i < last_; i++) {
				if(array_[i] == item) {
					return true;
				}
			}
		}

		return false;
	}

	// Copies the entire queue to an array, starting at a particular array index.
	void CopyTo(T* dest, int index) const {
		int pos = 0;

		if(first_ <= last_) {
			for(int i = first_; i < last_; i++) {
				dest[index + pos] = array_[i];
				pos++;
			}
		}
		else {
			for(int i = first_; i < Size; i++) {
				dest[index + pos] = array_[i];
				pos++;
			}

			for(int i = 0; i < last_; i++) {
				dest[index + pos] = array_[i];
				pos++;
			}
		}
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

	// Dequeues while the specified condition is false.
	template <class Predicate>
	void DequeueUntil(Predicate condition) {
		while(count_ > 0) {
			if(condition(Peek()) == false) {
				Dequeue();
			}
			else break;
		}
	}
};

} // namespace Base
#endif