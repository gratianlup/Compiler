// Heap.hpp
// Copyright (c) Lup Gratian
//
// Represents a heap (priority queue) based on a List.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_HEAP_HPP
#define PC_BASE_HEAP_HPP

#include "DebugValidator.hpp"
#include "DefaultComparer.hpp"
#include "List.hpp"
#include <algorithm>

namespace Base {

template <class T, bool UseValue = false, class Comparer = DefaultComparer<T, UseValue>,
		  class Validator = DebugValidator>
class Heap : protected List<T, Validator> {
private:
	int Parent(int index) {
		return (index / 2);
	}
	
	int Left(int index) {
		return (index * 2);
	}

	int Right(int index) {
		return (index * 2) + 1;
	}
	
	// Rebuilds the basic structure of a heap.
	void RebuildHeap(int index) {
		int left = Left(index);
		int right = Right(index);
		int max;

		if((left < count_) && (Comparer::Compare(array_[left], array_[index]) > 0)) {
			max = left;
		}
		else {
			max = index;
		}

		if((right < count_) && (Comparer::Compare(array_[right], array_[max]) > 0)) {
			max = right;
		}

		if(max != index) {
			std::swap(array_[index], array_[max]);
			RebuildHeap(max);
		}
	}

	// Creates a heap structure of the items.    
	void ConstructHeap() {
		for(int index = count_ / 2; index >= 0; index--) {
			RebuildHeap(index);
		}
	}

public:
	typedef typename Heap<T, UseValue, Comparer, Validator> THeap;
	typedef typename T Type;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes an empty Heap.
	Heap() : List(int(0)) {}

	// Initializes a Heap with the given capacity.    
	Heap(int capacity) : List(int(capacity)) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds the specified item to the Heap.
	void Add(const T& item) {
		List::Add(item);
		int index = count_ - 1;

		while((index > 0) && (Comparer::Compare(array_[Parent(index)], item) < 0)) {
			std::swap(array_[index], array_[Parent(index)]);
			index = Parent(index);
		}
	}

	// Extracts the greatest item from the Heap.
	T Extract() {
		Validator::IsLarger(count_, 0);
		T item = array_[0];

		// Remove the item from the heap and rebuild.
		std::swap(array_[0], array_[count_ - 1]);
		List::RemoveAt(count_ - 1);
		RebuildHeap(0);

		return item;
	}

	// Gets the greatest item from the heap without removing it.
	T Peek() const {
		Validator::IsLarger(count_, 0);
		return array_[0];
	}
	
	// Removes the specified item from the Heap.
	bool Remove(const T& item) {
		int index = List::IndexOf(item);

		if(index != List::INVALID_INDEX) {
			// Remove the item and reconstruct the heap.
			List::RemoveAt(index);
			ConstructHeap();

			return true;
		}

		return false; // Not found.
	}
	
	// Removes all items from the Heap.
	void Clear() {
		List::Clear();
	}

	// Gets the number of items contained in the Heap.    
	int Count() const {
		return List::Count();
	}

	// Determines whether the Heap contains the specified item.    
	bool Contains(const T& item) const {
		return List::Contains(item);
	}

	// Copies all elements of the Heap to an array, starting at a particular array index.
	void CopyTo(T* destination, int index) const {
		List::CopyTo(destination, index);
	}

	// Extracts while the specified condition is true.
	template <class Predicate>
	void ExtractWhile(Predicate condition) {
		while(count_ > 0) {
			if(condition(Peek())) {
				Extract();
			}
		}
		else break;
	}

	// Extracts while the specified condition is false.
	template <class Predicate>
	void ExtractUntil(Predicate condition) {
		while(count_ > 0) {
			if(condition(Peek()) == false) {
				Extract();
			}
		}
		else break;
	}
};

} // namespace Base
#endif