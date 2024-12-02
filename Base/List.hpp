// List.hpp
// Copyright (c) Lup Gratian
//
// Represents a strongly-typed list of objects that can be accessed by index.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_LIST_HPP
#define PC_BASE_LIST_HPP

#include "ListConst.hpp"
#include "DebugValidator.hpp"
#include "LocalPointer.hpp"
#include "DefaultComparer.hpp"
#include "Sorting.hpp"
#include <algorithm>
#include <cmath>

namespace Base {

// The code size might be reduced using the following pattern:
//template <class T>
//struct Promoter {
//	typedef T Type;
//	typedef T Base;
//};
//
//template <class T>
//struct Promoter<T*> {
//	typedef void* Type;
//	typedef T Base;
//};
//
//template <class T, class Validator = DebugValidator>
//class List : protected ListImpl<typename Promoter<T>::Type, Validator> {
//	typedef typename Promoter<T>::Type Promoted;
//
//	void Add(const Promoted& value) {
//		ListImpl::Add(value);
//	}
//};

template <class T, class Validator = DebugValidator>
class List : public ListConst {
public:
	typedef typename List<T, Validator> TList;
	typedef typename T Type;
	
protected:
	T* array_;
	int capacity_;
	int count_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Allocates memory to hold the specified number of items.
	void AllocateArray(int length) {
		if(length == 0) {
            return;
        }

		// Allocate and set initial capacity.
		array_ = new T[length];
		capacity_ = length;

		for(int i = 0; i < length; i++) {
			array_[i] = T();
		}
	}

	// Deallocates the memory.    
	void ReleaseArray() {
		delete[] array_;
		array_ = nullptr;
		capacity_ = 0;
		count_ = 0;
	}

	// Reallocates the array to hold the specified number of items.    
	void ResizeArray(int value) {
		Validator::IsLargerOrEqual(value, count_);
		
		if(array_ == nullptr) {
			AllocateArray(value); // Allocate a new array.
		}
		else {
			// Allocate a new array and copy the contents of the current one.
			local<T> temp(new T[value]);
			std::copy(array_, array_ + count_, temp.Raw());

			delete[] array_;
			array_ = temp.Get();

			for(int i = count_; i < value; i++) {
				array_[i] = T();
			}

			capacity_ = value;
		}
	}

	// Ensures that the array is large enough to hold the required number of items.
	// If not, the array is resized by a grow factor.    
	void EnsureCapacity(int required) {
		if(capacity_ < required) {
			int newCapacity = std::max(required, capacity_ + 
									   (int)floor((double)capacity_ * GROW_FACTOR));
			ResizeArray(newCapacity);
		}
	}

	// Shifts right all elements starting with the specified index.
	void ShiftRight(int index, int steps) {
		Validator::IsSmallerOrEqual(index + steps, capacity_);
		std::copy_backward(&array_[index], &array_[count_], &array_[count_ + steps]);

		// Assign the default value where there were objects.
		for(int i = index; i < (index + steps); i++) {
			array_[i] = T();
		}
	}
	
	// Shifts left all elements starting with the specified index.    
	void ShiftLeft(int index, int steps) {
		Validator::IsLargerOrEqual(index + steps, steps);
		std::copy(&array_[index + steps], &array_[count_], &array_[index]);

		// Assign the default value where there were objects.
		for(int i = 0; i < steps; i++) {
			array_[count_ - 1 - i] = T();
		}
	}

public:
	static const int INVALID_INDEX = -1;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes a new instance that is empty and has the default initial capacity.
	List() : array_(nullptr), capacity_(0), count_(0) {
		AllocateArray(DEFAULT_CAPACITY);
	}

	// Initializes a new instance that is empty and has the specified capacity.
	explicit List(int capacity) : array_(nullptr), capacity_(0), count_(0) {
		AllocateArray(capacity);
	}

	// Initializes the list with a copy of the items from the given List    
	List(const TList& source) : array_(nullptr), count_(0) {
		ReleaseArray();

        if(source.count_ > 0) {
		    AllocateArray(source.count_);
		    AddRange(source.array_, source.count_);
        }

		capacity_ = source.count_;
		count_ = source.count_;
	}

	List(T* data, int count) : array_(nullptr), count_(0) {
		if(data == nullptr) {
			AllocateArray(DEFAULT_CAPACITY);
		}
		else {
			ReleaseArray();
			AllocateArray(count);
			AddRange(data, count);
			capacity_ = count;
			count_ = count;
		}
	}

	// Implements move semantics.
	List(TList&& source) {
		array_ = source.array_;
		capacity_ = source.capacity_;
		count_ = source.count_;

		source.array_ = nullptr;
		source.capacity_ = 0;
		source.count_ = 0;
	}

	~List() {
		ReleaseArray();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds an item to the end of the List.
	void Add(const T& item) {
		EnsureCapacity(count_ + 1);
		array_[count_++] = item;
	}

    // Add an item to the end of the List (using move semantics).
	void Add(T&& item) {
		EnsureCapacity(count_ + 1);
		array_[count_++] = std::move(item);
	}

	// Adds the items of the specified array to the end of the List.
	void AddRange(T* items, int itemNumber) {
		Validator::IsNotNull(items);
		InsertRange(count_, items, itemNumber);
	}

	// Adds the items of the specified array to the end of the List.
	void AddRange(const TList& list) {
		InsertRange(count_, list);
	}

	// Removes all items from the List.
	void Clear() {
		// Call the destructors for all objects.
		for(int i = 0; i < count_; i++) {
			((T*)&array_[i])->~T();
		}

		// Don't release the memory, just set the count to 0.
		count_ = 0;
	}

	// Returns the object found at the specified position.
	T& ObjectAt(int index) {
		Validator::IsSmaller(index, count_);
		return array_[index];
	}

	const T& ObjectAt(int index) const {
		Validator::IsSmaller(index, count_);
		return array_[index];
	}

	// Inserts an item at the specified index.
	void Insert(int index, const T& item) {
		Validator::IsSmallerOrEqual(index, count_);
		EnsureCapacity(count_ + 1);

		if(index != count_) {
			// We need to move the other items in order to make place.
			ShiftRight(index, 1);
		}

		// Insert the item.
		array_[index] = item;
		count_++;
	}

	// Inserts a range of objects starting with the specified index.
	void InsertRange(int index, const T* items, int itemNumber) {
		Validator::IsNotNull(items);
		Validator::IsSmallerOrEqual(index, count_);

		if(itemNumber == 0) {
            return; // Return if there are no items to insert.
        }

		EnsureCapacity(count_ + itemNumber);

		if(index < count_) {
			// We need to move the other items in order to make place.
			ShiftRight(index, itemNumber);
		}

		// Copy the items.
		for(int i = 0; i < itemNumber; i++) {
			array_[index + i] = items[i];
		}

		count_ += itemNumber;
	}

	// Inserts a range of objects starting with the specified index.
	void InsertRange(int index, const TList& list) {
		InsertRange(index, list.array_, list.count_);
	}

	// Searches for the specified item and returns the index of the first occurrence
	// within the range of items that starts at the specified index and contains 
	// the specified number of elements    
	int IndexOf(const T& item, int index, int count) const {
		Validator::IsSmallerOrEqual(index + count, count_);
		int lastIndex = index + count;

		for(int i = index; i < lastIndex; i++) {
			if(array_[i] == item) return i;
		}

		return INVALID_INDEX; // Not found.
	}

	// Searches for the specified item and returns the index of the first occurrence
	// within the range of  items that starts at the specified index.
	int IndexOf(const T& item, int index) const {
		return IndexOf(item, index, count_ - index);
	}
	
	// Searches for the specified item and returns the index of the first occurrence.
	int IndexOf(const T& item) const {
		return IndexOf(item, 0);
	}

	// Searches for the specified item and returns the index of the last occurrence	
	// within the range that contains the specified number of items and ends 
	// at the specified index.
	int LastIndexOf(const T& item, int index, int count) const {
		Validator::IsSmaller(index, count_);
		Validator::IsLargerOrEqual(index + 1, count);
		int lastIndex = index - count;

		for(int i = index; i > lastIndex; i--) {
			if(array_[i] == item) return i;
		}

		return INVALID_INDEX; // Not found.
	}
	
	// Searches for the specified item and returns the index of the last occurrence
	// within the range that ends at the specified index.
	int LastIndexOf(const T& item, int index) const {
		return LastIndexOf(item, index, index + 1);
	}
	
	// Searches for the specified item and returns the index of the last occurrence.
	int LastIndexOf(const T& item) const {
		return LastIndexOf(item, count_ - 1);
	}

	// Determines whether the specified element is in the List.
	bool Contains(const T& item) const {
		return IndexOf(item) != INVALID_INDEX;
	}

    // Determines whether an element matching the specified predicate
    // is found in the List.
    template <class Predicate>
    bool ContainsMatching(Predicate action) const {
        return FindIndex(action) != INVALID_INDEX;
    }

	// Removes the item at the specified index.    
	void RemoveAt(int index) {
		Validator::IsSmaller(index, count_);

		// Call the destructor on the item
		((T*)&array_[index])->~T();

		if(index < (count_ - 1)) {
			// We need to shift left the array
			ShiftLeft(index, 1);
		}

		count_--;
	}

	// Removes the first occurrence of the specified item.    
	bool Remove(const T& item) {
		int i = IndexOf(item);

		if(i != INVALID_INDEX) {
			RemoveAt(i);
			return true;
		}

		return false; // Not found.
	}

    // Removes and returns the last element from the list.
    T RemoveLast() {
        Validator::IsLarger(count_, 0);
        
        int lastIndex = count_ - 1;
        T item = array_[lastIndex];
        RemoveAt(lastIndex);
        return item;
    }

    // Returns the last element, but without removing it.
    T& PeekLast() {
        Validator::IsLarger(count_, 0);
        return array_[count_ - 1];
    }

    const T& PeekLast() const {
        Validator::IsLarger(count_, 0);
        return array_[count_ - 1];
    }
        
    // Removes a range of items from the List.    
	void RemoveRange(int index, int count) {
		Validator::IsSmallerOrEqual(index + count, count_);
		
		// Call the destructor on all items.
		for(int i = 0; i < count; i++) {
			((T*)&array_[index + i])->~T();
		}

		if((index + count) < count_) {
			// We need to shift left the array.
			ShiftLeft(index, count);
		}
		else {
			// Assign the default value where there were objects.
			for(int i = 0; i < count; i++) {
				array_[index + i] = T();
			}
		}

		count_ -= count;
	}

	// Returns the internal array used to store the values.
	T* GetInternal() {
		return array_;
	}

	const T* GetInternal() const {
		return array_;
	}

	// Searches a range in the sorted List for an item using the specified comparer.    
	template <class Comparer>
	int BinarySearch(int index, int count, const T& item) const {
		int middle;
		int left = index;
		int right = index + count;

		do {
			middle = (right - left) / 2 + left; // Prevent overflow.
			int result = Comparer::Compare(item, array_[middle]);

			if(result == 0) {
                return middle;
            }
			else if(result < 0) {
				right = middle - 1; // Search on the left.
			}
			else {
				left = middle + 1; // Search on the right.
			}
		} while (left <= right);

		return INVALID_INDEX; // Not found.
	}

	// Searches a range in the sorted List for an item using the default comparer.
	int BinarySearch(int index, int count, const T& item) const {
		return BinarySearch<DefaultComparer<T>>(index, count, item);
	}
	
	// Searches the sorted List for an item using the specified comparer.	
	template <class Comparer>
	int BinarySearch(const T& item) const {
		return BinarySearch<Comparer>(0, count_, item);
	}

	// Searches the sorted List for an item using the default comparer.    
	int BinarySearch(const T& item) const {
		// Search using the DefaultComparer.
		return BinarySearch<DefaultComparer<T>>(0, count_, item);
	}

	// Sorts the range using the specified sorting algorithm.    
	template <class SortAlgorithm>
	void Sort(int index, int count, SortAlgorithm algorithm) {
		Validator::IsSmallerOrEqual(index + count, count_);
		algorithm(&array_[index], count);
	}

	// Sorts the range using the specified comparer
    // and the default sorting algorithm.    
	template <class Comparer>
	void Sort(int index, int count) {
		// Use STL Sort.
		Sort(index, count, DefaultSort<T, Comparer>());
	}

	// Sorts the range using the default comparer and sorting algorithm.
	void Sort(int index, int count) {
		Sort<DefaultComparer<T>>(index, count);
	}
	
	// Sorts the entire List using the specified sorting algorithm.
	template <class SortAlgorithm>
	void Sort(SortAlgorithm algorithm) {
		Sort(0, count_, algorithm);
	}

	// Sorts the entire List using "Bubble-sort" and doing loop unrolling.	
	template <int N>
	void Sort() {
		BubbleSort<T, DefaultComparer<T>, N>::Sort(array_);
	}

	// Sorts the entire List using the specified comparer
    // and default sorting algorithm.
	template <class Comparer>
	void Sort() {
		Sort<Comparer>(0, count_);
	}
	
	// Sorts the entire List using the default de comparer and sorting algorithm.
	void Sort() {
		Sort<DefaultComparer<T>>(0, count_);
	}

	// Searches for an item that matches the conditions defined 
    // by the specified predicate, and returns the first occurrence 
    // within the entire List.
	template <class Predicate>
	const T* Find(Predicate match) const {
		for(int i = 0; i < count_; i++) {
			if(match(array_[i])) {
				return &array_[i];
			}
		}

		return nullptr; // Not found.
	}

	// Searches for an item that matches the conditions defined
    // by the specified predicate, and returns the last occurrence 
    // within the entire List.
	template <class Predicate>
	const T* FindLast(Predicate match) const {
		for(int i = count_ - 1; i >= 0; i--) {
			if(match(array_[i]))  {
				return &array_[i];
			}
		}

		return nullptr; // Not found.
	}

	// Searches for an item that matches the conditions defined by the specified 
	// predicate, and returns the of the first occurrence within the range that 
	// starts at the specified index and contains the specified number of items.    
	template <class Predicate>
	int FindIndex(int startIndex, int count, Predicate match) const {
		Validator::IsSmallerOrEqual(startIndex + count, count_);
		int lastIndex = startIndex + count;

		for(int i = startIndex; i < lastIndex; i++) {
			if(match(array_[i])) {
				return i;
			}
		}

		return INVALID_INDEX; // Not found.
	}

	// Searches for an item that matches the conditions defined by the specified 
	// predicate, and returns the of the first occurrence within the range that 
	// starts at the specified index.
	template <class Predicate>
	int FindIndex(int startIndex, Predicate match) const {
		return FindIndex<Predicate>(startIndex, count_ - startIndex, match);
	}
	
	// Searches for an item that matches the conditions 
    // defined by the specified predicate.
	template <class Predicate>
	int FindIndex(Predicate match) const {
		return FindIndex<Predicate>(0, match);
	}

	// Searches for an item that matches the conditions defined by the specified 
	// predicate, and returns the index of the last occurrence within the range 
	// that contains the specified number of elements and ends at the specified index.    
	template <class Predicate>
	int FindLastIndex(int startIndex, int count, Predicate match) const {
		Validator::IsLargerOrEqual(startIndex + 1, count);
		int last = startIndex - count;

		for(int i = startIndex; i > last; i--) {
			if(match(array_[i])) {
				return i;
			}
		}

		return INVALID_INDEX; // Not found.
	}

	// Searches for an item that matches the conditions defined by the specified 
	// predicate, and returns the index of the last occurrence within the range 
	// that ends at the specified index.    
	template <class Predicate>
	int FindLastIndex(int startIndex, Predicate match) const {
		return FindLastIndex<Predicate>(startIndex, startIndex + 1, match);
	}
	
	// Searches for an item that matches the conditions defined by the specified 
	// predicate, and returns the index of the last occurrence within the range 
	// that ends at the specified index.
	template <class Predicate>
	int FindLastIndex(Predicate match) const {
		return FindLastIndex<Predicate>(count_ - 1, match);
	}
	
	// Retrieves all the elements that match the conditions 
    // defined by the specified predicate.
	template <class Predicate>
	TList FindAll(Predicate match) const {
		// Create a list and populate it.
		TList foundItems(0);

		for(int i = 0; i < count_; i++) {
			if(match(array_[i])) {
				foundItems.Add(array_[i]);
			}
		}

		return foundItems;
	}

	// Determines whether the List contains items that match the conditions 
	// defined by the specified predicate.    
	template <class Predicate>
	bool Exists(Predicate match) const {
		return FindIndex(match) != INVALID_INDEX;
	}
	
	// Performs the specified action on each item of the List.
	template <class Predicate>
	void ForEach(Predicate action) const {
		for(int i = 0; i < count_; i++) {
			if(action(array_[i]) == false) {
				break;
			}
		}
	}

    template <class Predicate>
    void ForEach(Predicate action) {
        for(int i = 0; i < count_; i++) {
            if(action(array_[i]) == false) {
                break;
            }
        }
    }

	// Removes the all the items that match the conditions
    // defined by the specified predicate.    
	template <class Predicate>
	int RemoveAll(Predicate match) {
		int removedItems = 0;
		
		for(int i = 0; i < count_; i++) {
			if(match(array_[i])) {
				RemoveAt(i--);
				removedItems++;
			}
		}

		return removedItems;
	}

	// Performs the specified action on each item of the List.    
	template <class Predicate>
	bool TrueForAll(Predicate match) const {
		for(int i = 0; i < count_; i++) {
			if(match(array_[i]) == false) {
				return false;
			}
		}

		return true;
	}

	// Copies a range of items to an array, starting at the specified index.
	void CopyTo(int sourceIndex, T *destination, int index, int count) const {
		Validator::IsNotNull(destination);
		Validator::IsSmallerOrEqual(sourceIndex + count, count_);
		int destPos = 0;

		while(destPos < count) {
			destination[index + destPos] = array_[sourceIndex];
			sourceIndex++;
			destPos++;
		}
	}
	
	// Copies the entire List to an array, starting at the specified index.
	void CopyTo(T *destination, int index) const {
		CopyTo(0, destination, index, count_);
	}
	
	// Copies the entire List to an array.
	void CopyTo(T *destination) const {
		CopyTo(destination, 0);
	}
	
	// Creates a List with a deep-copy of the specified range.
	TList GetRange(int index, int count) const {
		Validator::IsSmallerOrEqual(index + count, count_);
		TList list(count);
		list.AddRange(&array_[index], count);
		return list;
	}

	// Reverses the order of the items in the specified range.    
	void Reverse(int index, int count) {
		Validator::IsSmallerOrEqual(index + count, count_);
		
		for(int i = 0; i < count / 2; i++) {
			std::swap(array_[index + count - i - 1],
					  array_[index + i]);
		}
	}

	// Reverses the order of all items in the List.    
	void Reverse() {
		Reverse(0, count_);
	}

	// Sets the capacity to the actual number of elements, 
	// if that number is less than a threshold value.    
	void TrimExcess() {
		// See if we should trim the array.
		if((count_ < SELECTION_SORT_SIZE) || 
		   ((double)count_ / (double)capacity_ < TRIM_THRESHOLD)) {
			ResizeArray(count_);
		}
	}

	// Gets the number of items contained in the List.	    
	int Count() const {
		return count_;
	}

    // Returns 'true' if the list contains elements.
    bool IsNotEmpty() const {
        return count_ > 0;
    }

    // Returns 'true' if the list contains no elements.
    bool IsEmpty() const {
        return count_ == 0;
    }
	
	// Gets the total number of elements the data structure
    // can hold without resizing.
	int Capacity() const {
		return capacity_;
	}

	void SetCapacity(int value) {
		Validator::IsLargerOrEqual(value, count_);
		ResizeArray(value);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const T& operator[] (int index) const {
		Validator::IsSmaller(index, count_);
		return array_[index];
	}

	T& operator[] (int index) {
		Validator::IsSmaller(index, count_);
		return array_[index];
	}

	TList& operator= (const TList& source) {
		// Call the constructor.
		new(this) List<T, Validator>(source);
		return *this;
	}

	// Implements move semantics.   
	TList& operator= (TList&& source) {
        Clear();
        ReleaseArray();

		array_ = source.array_;
		capacity_ = source.capacity_;
		count_ = source.count_;

		source.array_ = nullptr;
		source.capacity_ = 0;
		source.count_ = 0;
		return *this;
	}

    bool operator== (const TList& other) const {
        if(count_ != other.count_) {
            return false;
        }

        for(int  i = 0; i < count_; i++) {
            if(array_[i] != other.array_[i]) {
                return false;
            }
        }

        return true;
    }

    bool operator!= (const TList& other) const {
        return operator== (other) == false;
    }
};

} // namespace Base
#endif