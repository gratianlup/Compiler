// StaticList.hpp
// Copyright (c) Lup Gratian
//
// Represents a fixed-size strongly-typed list of objects that can be accessed by index.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STATIC_LIST_HPP
#define PC_BASE_STATIC_LIST_HPP

#include "DebugValidator.hpp"
#include "DefaultComparer.hpp"
#include "TypeInfo.hpp"
#include "Sorting.hpp"
#include "List.hpp"

namespace Base {

template <class T, int Size, class Validator = DebugValidator>
class StaticList {
private:
	T array_[Size];      // The static array, used if the number of items < 'Size'.
	List<T>* largeList_; // List used when the number of items is large.
	int count_;          // The number of items, valid only if 'largeList_' is not used.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void ShiftRight(int index, int steps) {
		Validator::IsSmallerOrEqual(index + steps, Size);
		std::copy_backward(&array_[index], &array_[count_], &array_[count_ + steps]);

		// Assign the default value where there were objects.
		for(int i = index; i < (index + steps); i++) {
			array_[i] = T();
		}
	}

	void ShiftLeft(int index, int steps) {
		Validator::IsLargerOrEqual(index + steps, steps);
		std::copy(&array_[index + steps], &array_[count_], &array_[index]);

		// Assign the default value where there were objects.
		for(int i = 0; i < steps; i++) {
			array_[count_ - 1 - i] = T();
		}
	}

	void SwitchToLargeList(int required) {
		// Don't do anything if we already switched, or if there are enough
		// free elements in the static array.
		if(largeList_ || ((count_ + required) <= Size)) return;
		largeList_ = new List<T>(Size + 4);

		// Copy the items to the large list.
		largeList_->AddRange(array_, Size);
	}

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	typedef typename StaticList<T, Size> TList;
	typedef typename T Type;
    
	static const int INVALID_INDEX = -1;
	static const int SIZE = Size;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StaticList() : count_(0), largeList_(nullptr) {}

	explicit StaticList(int count) : count_(0), largeList_(nullptr) {
		if(count > Size) {
			largeList_ = new List<T>(count);
		}
	}

	StaticList(const TList& other) : count_(other.count_), largeList_(nullptr) {
		if(other.largeList_) {
			// Make a copy of the list.
			largeList_ = new List<T>(*other.largeList_);
		}
		else std::copy(other.array_, other.array_ + count_, array_);
	}

	StaticList(T* data, int length = Size) : count_(length), largeList_(nullptr) {
		if(length > Size) {
			largeList_ = new List<T>(data, length);
		}
		else std::copy(data, data + count_, array_);
	}

	~StaticList() {
		Clear();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void Add(const T& item) {
		SwitchToLargeList(1);

		if(largeList_) {
            largeList_->Add(item);
        }
		else array_[count_++] = item;
	}

	void AddRange(T* items, int itemNumber) {
		Validator::IsNotNull(items);
		SwitchToLargeList(itemNumber);

		if(largeList_) {
            largeList_->AddRange(items, itemNumber);
        }
		else InsertRange(count_, items, itemNumber);
	}

	void AddRange(const TList& list) {
		InsertRange(count_, list);
	}

	void Clear() {
		if(largeList_) {
			// Clear and downgrade to the static array.
			largeList_->Clear();
			delete largeList_;
			largeList_ = nullptr;
			count_ = 0;
		}
		else {
			// Call the destructors for all objects.
			for(int i = 0; i < count_; i++) {
				((T*)&array_[i])->~T();
			}

			count_ = 0;
		}
	}

	void Insert(int index, const T& item) {
		SwitchToLargeList(1);

		if(largeList_) {
            largeList_->Insert(index, item);
        }
		else {
			if(index != count_) {
				// We need to move the other items in order to make place.
				ShiftRight(index, 1);
			}

			array_[index] = item;
			count_++;
		}
	}

	void InsertRange(int index, const T* items, int itemNumber) {
		Validator::IsNotNull(items);

		if(itemNumber == 0) {
            return;
        }

		SwitchToLargeList(itemNumber);

		if(largeList_) {
            largeList_->InsertRange(index, items, itemNumber);
        }
		else {
			if(index < count_) {
				// We need to move the other items in order to make place.
				ShiftRight(index, itemNumber);
			}

			for(int i = 0; i < itemNumber; i++) {
				array_[index + i] = items[i];
			}

			count_ += itemNumber;
		}
	}

	void InsertRange(int index, const TList& list) {
		InsertRange(index, list.array_, list.count_);
	}

	int IndexOf(const T& item, int index, int count) const {
		if(largeList_) {
            return largeList_->IndexOf(item,  index, count);
        }
		else {
			int lastIndex = index + count;

			for(int i = index; i < lastIndex; i++) {
				if(array_[i] == item) return i;
			}

			return INVALID_INDEX; // Not found.
		}
	}

	int IndexOf(const T& item, int index) const {
		if(largeList_) {
            return largeList_->IndexOf(item,  index);
        }
		else return IndexOf(item, index, count_ - index);
	}

	int IndexOf(const T& item) const {
		if(largeList_) {
            return largeList_->IndexOf(item);
        }
		else return IndexOf(item, 0);
	}

	int LastIndexOf(const T& item, int index, int count) const {
		if(largeList_) {
            return largeList_->LastIndexOf(item, index, count);
        }
		else {
			int lastIndex = index - count;

			for(int i = index; i > lastIndex; i--) {
				if(array_[i] == item) return i;
			}

			return INVALID_INDEX; // Not found.
		}
	}

	int LastIndexOf(const T& item, int index) const {
		if(largeList_) {
            return largeList_->LastIndexOf(item, index);
        }
		else return LastIndexOf(item, index, index + 1);
	}

	int LastIndexOf(const T& item) const {
        if(largeList_) {
            return largeList_->LastIndexOf(item);
        }
		else return LastIndexOf(item, count_);
	}

	bool Contains(const T& item) const {
		return IndexOf(item) != INVALID_INDEX;
	}

    template <class Predicate>
    bool ContainsMatching(Predicate action) const {
        return FindIndex(action) != INVALID_INDEX;
    }

	void RemoveAt(int index) {
		if(largeList_) {
            largeList_->RemoveAt(index);
        }
		else {
			// Call the destructor on the item
			((T*)&array_[index])->~T();

			if(index < (count_ - 1)) {
				ShiftLeft(index, 1);
			}

			count_--;
		}
	}

	bool Remove(const T& item) {
		if(largeList_) {
            return largeList_->Remove(item);
        }
		else {
			int i = IndexOf(item);

			if(i != INVALID_INDEX) {
				RemoveAt(i);
				return true;
			}

			return false; // Not found.
		}
	}

    // Removes and returns the last element from the list.
    T RemoveLast() {
        if(largeList_) {
            return largeList_->RemoveLast();
        }
        else {
            Validator::IsLarger(count_, 0);
        
            int lastIndex = count_ - 1;
            T item = array_[lastIndex];
            RemoveAt(lastIndex);
            return item;
        }
    }

	void RemoveRange(int index, int count) {
		if(largeList_) {
            largeList_->RemoveRange(index, count);
        }
		else {
			// Call the destructor on all items.
			for(int i = 0; i < count; i++) {
				((T*)&array_[index + i])->~T();
			}

			if((index + count) < count_) {
				ShiftLeft(index, count);
			}

			count_ -= count;
		}
	}

    // Returns the last element, but without removing it.
    T& PeekLast() {
        if(largeList_) {
            return largeList_->PeekLast();
        }
        else {
            Validator::IsLarger(count_, 0);
            return array_[count_ - 1];
        }
    }

    const T& PeekLast() const {
        if(largeList_) {
            return largeList_->PeekLast();
        }
        else {
            Validator::IsLarger(count_, 0);
            return array_[count_ - 1];
        }
    }

	template <class SortAlgorithm>
	void Sort(int index, int count, SortAlgorithm algorithm) {
		if(largeList_) {
            largeList_->Sort<SortAlgorithm>(index, count, algorithm);
        }
		else algorithm(&array_[index], count);
	}
  
	template <class Comparer>
	void Sort(int index, int count) {
		// Use STL Sort.
		Sort(index, count, DefaultSort<T, Comparer>());
	}

	void Sort(int index, int count) {
		Sort<DefaultComparer<T>>(index, count);
	}
	
	template <class SortAlgorithm>
	void Sort(SortAlgorithm algorithm) {
		Sort(0, count_, algorithm);
	}

	// Should be used only if the list is small (< Size elements).
	template <int N>
	void Sort() {
		DebugValidator::IsNull(largeList_);
		BubbleSort<T, DefaultComparer<T>, N>::Sort(array_);
	}

	template <class Comparer>
	void Sort() {
		Sort<Comparer>(0, count_);
	}
	
	void Sort() {
		Sort<DefaultComparer<T>>(0, count_);
	}

	template <class Comparer>
	int BinarySearch(int index, int count, const T& item) const {
		if(largeList_) {
            return largeList_->BinarySearch<Comparer>(index, count, item);
        }
		else {
			int middle;
			int left = index;
			int right = index + count;

			do {
				middle = (right - left) / 2 + left;
				int result = Comparer::Compare(item, array_[middle]);

				if(result == 0) {
                    return middle;
                }
				else if(result < 0) {
					right = middle - 1; // Search on the left.
				}
				else left = middle + 1; // Search on the right.
			} while (left <= right);

			return INVALID_INDEX; // Not found.
		}
	}

	int BinarySearch(int index, int count, const T& item) const {
		return BinarySearch<DefaultComparer<T>>(index, count, item);
	}
	
	template <class Comparer>
	int BinarySearch(const T& item) const {
		return BinarySearch<Comparer>(0, count_, item);
	}
   
	int BinarySearch(const T& item) const {
		// Search using the DefaultComparer.
		return BinarySearch<DefaultComparer<T>>(0, count_, item);
	}

	template <class Predicate>
	const T* Find(Predicate match) const {
        if(largeList_) {
            return largeList_->Find<Predicate>(match);
        }
		else {
			for(int i = 0; i < count_; i++) {
				if(match(array_[i])) {
					return &array_[i];
				}
			}

			return nullptr; // Not found.
		}
	}

	template <class Predicate>
	const T* FindLast(Predicate match) const {
		if(largeList_) {
            return largeList_->FindLast<Predicate>(match);
        }
		else {
			for(int i = count_ - 1; i >= 0; i--) {
				if(match(array_[i]))  {
					return &array_[i];
				}
			}

			return nullptr; // Not found.
		}
	}
 
	template <class Predicate>
	int FindIndex(int startIndex, int count, Predicate match) const {
		if(largeList_) {
            return largeList_->FindIndex<Predicate>(startIndex, count, match);
        }
		else {
			int lastIndex = startIndex + count;

			for(int i = startIndex; i < lastIndex; i++) {
				if(match(array_[i])) {
					return i;
				}
			}

			return INVALID_INDEX; // Not found.
		}
	}

	template <class Predicate>
	int FindIndex(int startIndex, Predicate match) const {
		return FindIndex<Predicate>(startIndex, count_ - startIndex, match);
	}
	
	template <class Predicate>
	int FindIndex(Predicate match) const {
		return FindIndex<Predicate>(0, match);
	}
 
	template <class Predicate>
	int FindLastIndex(int startIndex, int count, Predicate match) const {
		if(largeList_) {
            return largeList_->FindLastIndex<Predicate>(startIndex, count, match);
        }
		else {
			int last = startIndex - count;

			for(int i = startIndex; i > last; i--) {
				if(match(array_[i])) {
					return i;
				}
			}

			return INVALID_INDEX; // Not found.
		}
	}

	template <class Predicate>
	int FindLastIndex(int startIndex, Predicate match) const {
		return FindLastIndex<Predicate>(startIndex, startIndex + 1, match);
	}
	
	template <class Predicate>
	int FindLastIndex(Predicate match) const {
		return FindLastIndex<Predicate>(count_ - 1, match);
	}
 
	template <class Predicate>
	bool Exists(Predicate match) const {
		return FindIndex(match) != INVALID_INDEX;
	}
	
	template <class Predicate>
	void ForEach(Predicate action) const {
		if(largeList_) {
            largeList_->ForEach<Predicate>(action);
        }
		else {
			for(int i = 0; i < count_; i++) {
				if(action(array_[i]) == false) {
					break;
				}
			}
		}
	}

    template <class Predicate>
    void ForEach(Predicate action) {
        if(largeList_) {
            largeList_->ForEach<Predicate>(action);
        }
        else {
            for(int i = 0; i < count_; i++) {
                if(action(array_[i]) == false) {
                    break;
                }
            }
        }
    }

	template <class Predicate>
	int RemoveAll(Predicate match) {
		if(largeList_) {
            return largeList_->RemoveAll<Predicate>(match);
        }
		else {
			int removedItems = 0;
		
			for(int i = 0; i < count_; i++) {
				if(match(array_[i])) {
					RemoveAt(i--);
					removedItems++;
				}
			}

			return removedItems;
		}
	}

	template <class Predicate>
	bool TrueForAll(Predicate match) const {
		if(largeList_) {
            return largeList_->TrueForAll<Predicate>(match);
        }
		else {
			for(int i = 0; i < count_; i++) {
				if(match(array_[i]) == false) {
					return false;
				}
			}

			return true;
		}
	}

	void CopyTo(int sourceIndex, T *destination, int index, int count) const {
		Validator::IsNotNull(destination);

		if(largeList_) {
            largeList_->CopyTo(sourceIndex, destination, index, count);
        }
		else {
			int destPos = 0;

			while(destPos < count) {
				destination[index + destPos] = array_[sourceIndex];
				sourceIndex++;
				destPos++;
			}
		}
	}
	
	void CopyTo(T *destination, int index) const {
		CopyTo(0, destination, index, count_);
	}
	
	void CopyTo(T *destination) const {
		CopyTo(destination, 0);
	}

	void Reverse(int index, int count) {
		if(largeList_) {
            largeList_->Reverse(index, count);
        }
		else {
			for(int i = 0; i < count / 2; i++) {
				std::swap(array_[index + count - i - 1],
						  array_[index + i]);
			}
		}
	}

	void Reverse() {
		Reverse(0, count_);
	}

	int Count() const {
		if(largeList_) return largeList_->Count();
		else return count_;
	}

    bool IsNotEmpty() const {
        if(largeList_) return largeList_->IsNotEmpty();
		else return count_ > 0;
    }

    bool IsEmpty() const {
        if(largeList_) return largeList_->IsEmpty();
		else return count_ == 0;
    }

	T* Array() {
		if(largeList_) return largeList_->GetInternal();
		else return array_;
	}

	void SetCount(int count) {
		if(largeList_) largeList_->ResizeArray(count);
		else count_ = count;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const T& operator[] (int index) const {
		if(largeList_) return largeList_->operator[] (index);
		else return array_[index];
	}

	T& operator[] (int index) {
		if(largeList_) return largeList_->operator[] (index);
		else return array_[index];
	}

    TList& operator= (const TList& other) {
        if(&other != this) {
            Clear();
            count_ = other.count_;

            if(other.largeList_) {
			    // Make a copy of the list.
			    largeList_ = new List<T>(*other.largeList_);
		    }
		    else std::copy(other.array_, other.array_ + count_, array_);
        }

        return *this;
    }

    bool operator== (const TList& other) const {
        if(largeList_ && other.largeList_) {
            // Both lists use a large list.
            return *largeList_ == *other.largeList_;
        }
        else if((largeList_ || other.largeList_) == false) {
            // Both lists don't use yet a large list.
            if(count_ != other.count_) {
                return false;
            }

            for(int i = 0; i < count_; i++) {
                if(array_[i] != other.array_[i]) {
                    return false;
                }
            }

            return true;
        }
        else {
            // One of the lists uses a large list.
            auto largeList = largeList_ ? largeList_->GetInternal() : 
                                          other.largeList_->GetInternal();
            auto list = largeList_ ? array_ : other.array_;
            int largeListCount = largeList_ ? largeList_->Count() :
                                              other.largeList_->Count();
            int listCount = largeList_ ? other.Count() : count_;

            if(largeListCount != listCount) {
                return false;
            }

            for(int i = 0; i < listCount; i++) {
                if(largeList[i] != list[i]) {
                    return false;
                }
            }

            return true;
        }
    }

    bool operator!= (const TList& other) const {
        return operator== (other) == false;
    }
};

} // namespace Base
#endif