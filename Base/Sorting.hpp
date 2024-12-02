// Sorting.hpp
// Copyright (c) Lup Gratian
//
// Provides a series of sorting algorithms.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_SORTING_HPP
#define PC_BASE_SORTING_HPP

#include <algorithm> // For 'swap'.

namespace Base {

// Selection sort. To be used when the number of items is small,
// but not known at compile time.
template <class T, class Comparer>
class SelectionSort {
public:
	static inline void Sort(T* items, int count) {
		for(int i = 0; i < count; i++) {
			for(int j = i + 1; j < count; j++) {
				if(Comparer::Compare(items[i], items[j]) > 0) {
					std::swap(items[i], items[j]);
				}
			}
		}
	}

	void operator() (T* items, int count) {
		Sort(items, count);
	}
};


// Default sorting algorithm. Uses STL Sort.
template <class T, class Comparer>
class DefaultSort {
public:
	static inline void Sort(T* items, int count) {
		if(count >= 2) {
			std::sort(items, items + count);
		}
	}

	void operator() (T* items, int count) {
		Sort(items, count);
	}
};


// Swaps the items specified by the template numbers I and J if item[I] > item[J].
template <class T, class Comparer, int I, int J>
class ItemSwap {
public:
	static inline void CompareAndSwap(T *items) {
		if(Comparer::Compare(items[I], items[J]) > 0) {
			std::swap(items[I], items[J]);
		}
	}
};

// The loop that compares the items.
template <class T, class Comparer, int I, int J>
class BubbleSortLoop {
private:
	enum { go = (J <= I - 2) };

public:
	static inline void Loop(T *items)	{
		ItemSwap<T, Comparer, J, J + 1>::CompareAndSwap(items);
		BubbleSortLoop<T, Comparer, go ? I : 0, go ? (J + 1) : 0>::Loop(items);
	}
};

// Specialization so that the loop stops when reaching index 0.
template <class T, class Comparer>
class BubbleSortLoop<T, Comparer, 0, 0> {
public:
	static inline void Loop(T *items) { }
};

// Implements Bubble sort with loop unrolling (using template meta programming).
template <class T, class Comparer, int N>
class BubbleSort {
public:
	static inline void Sort(T* items) {
		BubbleSortLoop<T, Comparer, N - 1, 0>::Loop(items);
		BubbleSort<T, Comparer, N - 1>::Sort(items);
	}

	void operator() (T* items, int count) {
		Sort(items);
	}
};

// Specialization so that recursion stops when reaching the end of the array.
template <class T, class Comparer>
class BubbleSort<T, Comparer, 1> {
public:
	static inline void Sort(T* items) { }
};

} // namespace Base
#endif