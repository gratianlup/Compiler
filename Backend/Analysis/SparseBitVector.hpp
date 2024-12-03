// SparseBitVector.hpp
// Copyright (c) Lup Gratian
//
// Implements a dynamic bit vector that is optimized for space, meaning that only
// the bits that are used are stored. This allows storing bits at position with
// large distances between them, taking much less memory than 'BitVector'.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SPARSE_BIT_VECTOR_HPP
#define PC_ANALYSIS_SPARSE_BIT_VECTOR_HPP

#include "BitVectorBase.hpp"
#include "../Base/LinkedList.hpp"
#include <memory>
using namespace Base;

namespace Analysis {

// Uses a doubly-linked list of 'BitRange' objects, each node being able to store
// the state of 'BITS_PER_RANGE' bits. The nodes are ordered in increasing order
// relative to the start index, like in the following example:
// [0 => BPR] -> [4*BPR => 5*BPR] -> ..., where 'BPR' is 'BITS_PER_RANGE'.
// To increase the speed of repeated accesses in the same region,
// the last used node is remembered and checked first.
class SparseBitVector : public BitVectorBase {
private:
	// Represents an interval of bits. The number of bits can be configured using 'Size'.
	template <int Size>
	struct BitRange {
		__int64 Data[Size];
		int StartBit;

		BitRange(int start) : StartBit(start) {
			std::memset(Data, 0, sizeof(__int64) * Size);
		}

		BitRange(const BitRange& other) : StartBit(other.StartBit) {
			std::memcpy(Data, other.Data, sizeof(__int64) * Size);
		}
	};

    // Constants that define the size/number of bits for a range.
    static const int ELEMENTS_PER_RANGE = 32;
    static const int BITS_PER_ELEMENT = (sizeof(__int64) * 8);
    static const int BITS_PER_RANGE = ELEMENTS_PER_RANGE * BITS_PER_ELEMENT;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	typedef BitRange<ELEMENTS_PER_RANGE> MultipleBitRange;
	typedef LinkedList<MultipleBitRange> BitNodeList;
	typedef BitNodeList::TNode BitNode;
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	BitNodeList nodes_;         // The list of ranges.
	mutable BitNode* lastNode_; // The last range accessed (used for caching).

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the index of the start bit for the specified range.
	inline int GetStartBit(int range) const {
		return range * BITS_PER_RANGE;
	}

	// Returns the range in which the specified bit position is found.
	inline int GetRange(int position) const {
		return position / BITS_PER_RANGE;
	}

	// Returns 'true' if the specified bit position is covered by the range.
	inline bool IsInRange(int position, const MultipleBitRange& range) const {
		return (position >= range.StartBit) &&
			   (position < (range.StartBit + BITS_PER_RANGE));
	}

	// Returns the index of the element in which the specified bit is found.
	inline int ElementIndex(int rangePos) const {
		return rangePos / BITS_PER_ELEMENT;
	}

	// Returns the offset relative to the element in which the specified bit is found.
	inline int ElementOffset(int rangePos) const {
		return rangePos % BITS_PER_ELEMENT;
	}

	// Sets the bit associated with the specified range to 1.
	inline void SetBit(int position, MultipleBitRange& range) {
		int rangePos = position - range.StartBit;
		int elemIndex = ElementIndex(rangePos);
		int elemOffset = ElementOffset(rangePos);

		range.Data[elemIndex] = SetBitImpl(range.Data[elemIndex], elemOffset);
	}

	// Sets the bit associated with the specified range to 0.
	inline void ResetBit(int position, MultipleBitRange& range) {
		int rangePos = position - range.StartBit;
		int elemIndex = ElementIndex(rangePos);
		int elemOffset = ElementOffset(rangePos);

		range.Data[elemIndex] = ResetBitImpl(range.Data[elemIndex], elemOffset);
	}

	// Inverts the state of the bit associated with the specified range.
	inline void InvertBit(int position, MultipleBitRange& range) {
		int rangePos = position - range.StartBit;
		int elemIndex = ElementIndex(rangePos);
		int elemOffset = ElementOffset(rangePos);

		range.Data[elemIndex] = InvertBitImpl(range.Data[elemIndex], elemOffset);
	}
	
	// Returns 'true' if the bit associated with the specified range is set to 1.
	inline bool IsSet(int position, const MultipleBitRange& range) const {
		int rangePos = position - range.StartBit;
		int elemIndex = ElementIndex(rangePos);
		int elemOffset = ElementOffset(rangePos);

		return IsSetImpl(range.Data[elemIndex], elemOffset);
	}
	
	// Returns the number of bits that are set to 1 in the specified range.
	int SetBitsCount(const MultipleBitRange& range) const {
		int count = 0;

		for(int i = 0; i < ELEMENTS_PER_RANGE; i++) {
			count += BitsSet(range.Data[i]);
		}

		return count;
	}
	
	// Returns 'true' if at least one bit is set to 1 in the specified range.
	bool HasSetBits(const MultipleBitRange& range) const {
		for(int i = 0; i < ELEMENTS_PER_RANGE; i++) {
			if(range.Data[i] != 0) {
                return true;
            }
		}

		return false;
	}
	
	// Returns the index of the first bit set to 1 in the specified range.
	// If no bit is found, or it's before 'startIndex', -1 is returned.
	int FirstSetBit(const MultipleBitRange& range, int startIndex) const {
		int elemIndex = 0;
		__int64 mask = -1;

		if(startIndex >= range.StartBit + BITS_PER_RANGE) {
			// A valid bit can't be found in this range.
			return -1;
		}
		else if(IsInRange(startIndex, range)) {
			// It's possible that some bits from the first element should be ignored.
			int rangePos = startIndex - range.StartBit;
			int elemOffset = ElementOffset(rangePos);
			elemIndex = ElementIndex(rangePos);
			mask = ~(((__int64)1 << elemOffset) - 1);
		}

		for(int i = elemIndex; i < ELEMENTS_PER_RANGE; i++) {
			int result = BitVectorBase::FirstSetBit(range.Data[i] & mask);

			if(result != -1) {
				return range.StartBit + (i * BITS_PER_ELEMENT) + result;
			}

			// The rest of the element's don't need to be masked.
			mask = -1;
		}

		return -1; // No set bit was found.
	}

	// Returns the nearest range to the specified position,
    // based on the last accessed range.
	BitNode* GetNearestRangeFast(int position) const {
		MultipleBitRange lastRange = lastNode_->Value;

		// Check if we're in the last accessed range.
		if(IsInRange(position, lastRange)) {
			return lastNode_;
		}
		else if(position > lastRange.StartBit) {
			// We need to search forward, fro ranges with a higher start bit.
			BitNode* last = lastNode_;
			BitNode* node = lastNode_->Next;

			while(node) {
				if(IsInRange(position, node->Value)) {
					return node;
				}
				else if(node->Value.StartBit > position) {
					return node;
				}

				last = node;
				node = node->Next;
			}

			return last;
		}
		else {
			// We need to search backwards.
			BitNode* last = lastNode_;
			BitNode* node = lastNode_->Previous;

			while(node) {
				if(IsInRange(position, node->Value)) {
					return node;
				}
				else if((node->Value.StartBit + BITS_PER_RANGE) < position) {
					return node;
				}

				last = node;
				node = node->Previous;
			}

			return last;
		}
	}

	// Returns the nearest range to the specified position.
	BitNode* GetNearestRange(int position) const {
		// First try to use the last accessed range.
		if(lastNode_) {
            return GetNearestRangeFast(position);
        }
		
        if(nodes_.Count() == 0) {
            return nullptr;
        }

		// Start from the beginning of the list and search the position.
		BitNode* node = nodes_.First();
		BitNode* lastNode = nullptr;

		while(node) {
			if(IsInRange(position, node->Value)) {
				// Found a range which contains the position.
				return node;
			}
			else if(node->Value.StartBit > position) {
				// Found a range that is greater -> this one is the closest.
				return node;
			}

			lastNode = node;
			node = node->Next;
		}

		return lastNode;
	}

	// Returns a range that contains the bit found at the specified position.
	// If such a range doesn't exist it is created now.
	BitNode* GetOrInsertRange(int position) {
		BitNode* nearest = GetNearestRange(position);

		if(nearest) {
			if(IsInRange(position, nearest->Value)) {
				// A valid range was found.
				return nearest;
			}
			else {
				MultipleBitRange newRange(GetStartBit(GetRange(position)));

				if(position > nearest->Value.StartBit) {
					// The new range should be after the nearest.
					return nodes_.AddAfter(nearest, newRange);
				}
				else return nodes_.AddBefore(nearest, newRange);
			}
		}

		// This is the first range added.
		MultipleBitRange newRange(GetStartBit(GetRange(position)));
		return nodes_.Add(newRange);
	}

	// Sets the next or previous range of the specified one 
    // as the last accessed range.
	void SetNewLastNode(BitNode* node) {
		// Replace the last accessed node with its predecessor or successor
		// (which one proves not to be null).
		if(node != lastNode_) {
            return;
        }
		else if(node->Previous) {
            lastNode_ = node->Previous;
        }
		else lastNode_ = node->Next;
	}

	// Removes the specified range if it has no bits set.
	void TryRemoveRange(BitNode* node) {
		// If no bits remain set after we reset this one remove the range.
		// This should be something that doesn't happen to often.
		if(HasSetBits(node->Value) == false) {
			SetNewLastNode(node);
			nodes_.Remove(node);
		}
	}

	// Performs a bitwise AND between all the bits of the specified ranges.
	void AndRanges(MultipleBitRange& a, const MultipleBitRange& b) {
		for(int i = 0; i < ELEMENTS_PER_RANGE; i++) {
			a.Data[i] &= b.Data[i];
		}
	}

	// Performs a bitwise OR between all the bits of the specified ranges.
	void OrRanges(MultipleBitRange& a, const MultipleBitRange& b) {
		for(int i = 0; i < ELEMENTS_PER_RANGE; i++) {
			a.Data[i] |= b.Data[i];
		}
	}

	// Returns 'true' if all the bits in the specified ranges have the same state.
	bool RangesAreEqual(const MultipleBitRange& a, const MultipleBitRange& b) const {
		for(int i = 0; i < ELEMENTS_PER_RANGE; i++) {
			if(a.Data[i] != b.Data[i]) return false;
		}

		return true;
	}

public:
	SparseBitVector() : lastNode_(nullptr) {}

    SparseBitVector(const SparseBitVector& other) : lastNode_(nullptr) {
        // Clone the ranges, if required.
        for(auto node = other.nodes_.First(); node; node = node->Next) {
            nodes_.Add(node->Value);
        }
    }

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Sets to 1 the bit found at the specified position.
	void SetBit(int position) {
		BitNode* node = GetOrInsertRange(position);
		SetBit(position, node->Value);
		lastNode_ = node;
	}

	// Sets to 0 the bit found at the specified position.
	void ResetBit(int position) {
		BitNode* node = GetOrInsertRange(position);
		ResetBit(position, node->Value);
		lastNode_ = node;
		TryRemoveRange(node);
	}

	// Inverts the bit found at the specified position.
	void InvertBit(int position) {
		BitNode* node = GetOrInsertRange(position);
		InvertBit(position, node->Value);
		lastNode_ = node;
		TryRemoveRange(node);
	}

	// Returns 'true' if the the bit found at the specified position is set.
	bool IsSet(int position) const {
		BitNode* nearest = GetNearestRange(position);
		
        if(nearest == nullptr) {
            return false;	
        }
        else lastNode_ = nearest;

		if(IsInRange(position, nearest->Value)) {
			// Found a valid range, now check the bit.
			return IsSet(position, nearest->Value);
		}
		
        return false;
	}

    bool IsNotSet(int position) const {
        return IsSet(position) == false;
    }

	// Returns the total number of set bits.
	int SetBitsCount() const {
		int count = 0;
		BitNode* node = nodes_.First();

		while(node) {
			count += SetBitsCount(node->Value);
			node = node->Next;
		}

		return count;
	}
	
	// Returns 'true' if there are bits that are set in the vector.
	bool HasBitsSet() const {
		BitNode* node = nodes_.First();

		while(node) {
			if(HasSetBits(node->Value)) {
                return true;
            }
		}

		return false;
	}

	// Returns 'true' if none of the bits in the vector are set.
	bool HasNoBitsSet() const {
		BitNode* node = nodes_.First();

		while(node) {
			if(HasSetBits(node->Value)) {
                return false;
            }
		}

		return true;
	}

	// Sets all bits in the vector to 0.
	void Clear() {
		// Remove all ranges.
		nodes_.Clear();
		lastNode_ = nullptr;
	}

	// Performs a bitwise AND between the this and the specified object.
	void And(const SparseBitVector& other) {
		BitNode* a = nodes_.First();
		BitNode* b = other.nodes_.First();

		// If the other vector has no nodes we're node.
		if(b == nullptr) {
			Clear();
			return;
		}

		while(a && b) {
			if(a->Value.StartBit == b->Value.StartBit) {
				// The ranges are the same.
				AndRanges(a->Value, b->Value);
				a = a->Next;
				b = b->Next;
			}
			else if(a->Value.StartBit > b->Value.StartBit) {
				// Go to the next range from 'other'.
				b = b->Next;
			}
			else {
				// There is no range that matches this one -> remove it.
				BitNode* temp = a;
				a = a->Next;
				SetNewLastNode(temp);
				nodes_.Remove(temp);
			}
		}

		// All the ranges that remain need to be removed 
        // (no ranges in 'other' match).
		while(a) {
			BitNode* temp = a;
			a = a->Next;
			SetNewLastNode(temp);
			nodes_.Remove(temp);
		}
	}

	// Performs a bitwise OR between the this and the specified object.
	void Or(const SparseBitVector& other) {
		BitNode* a = nodes_.First();
		BitNode* b = other.nodes_.First();

		// If the other vector has no nodes we're node.
		if(b == nullptr) {
            return;
        }

		while(a && b) {
			if(a->Value.StartBit == b->Value.StartBit) {
				// The ranges are the same.
				OrRanges(a->Value, b->Value);
				a = a->Next;
				b = b->Next;
			}
			else if(a->Value.StartBit > b->Value.StartBit) {
				// Add range 'b' before 'a'.
				nodes_.AddBefore(a, MultipleBitRange(b->Value));
				b = b->Next;
			}
			else {
				// Advance to the next range in this vector.
				a = a->Next;
			}
		}

		// Add all the remaining ranges from 'b'.
		while(b) {
			nodes_.AddLast(MultipleBitRange(b->Value));
			b = b->Next;
		}
	}

	// Returns the index of the first set bit, or -1 if no bit is set.
	// If 'startIndex' is specified any set bit before it is ignored.
	int FirstSetBit(int startIndex = 0) const {
		// Find the nearest range.
		BitNode* nearest = GetNearestRange(startIndex);
		
		if(nearest == nullptr) {
            return -1; // No ranges.
        }
		else if(startIndex >= (nearest->Value.StartBit + BITS_PER_RANGE)) {
			// We're outside the largest range, give up.
			return -1;
		}

		while(nearest) {
			int result = FirstSetBit(nearest->Value, startIndex);

			if(result != -1) {
                return result;
            }
			else nearest = nearest->Next;
		}

		return -1; // No set bit was found.
	}

	// Calls the specified predicate for each of the set bits.
	// bool Predicate(int index)
	template <class Predicate>
	void ForEachSetBit(Predicate action) const {
		// We don't use 'FirstSetBit' to iterate over the set bit
        // because it should be faster to test all bits in all the ranges 
        // (when testing the first bits of a range most of it is brought 
        // into the cache, which should speed things up a lot).
		BitNode* node = nodes_.First();

		while(node) {
			MultipleBitRange& range = node->Value;

			for(int element = 0; element < ELEMENTS_PER_RANGE; element++) {
				// Test each bit in the range.
				for(int bit = 0; bit < BITS_PER_ELEMENT; bit++) {
					if(IsSetImpl(range.Data[element], bit)) {
                        int index = range.StartBit + (element * BITS_PER_ELEMENT) + bit;

					    if(action(index) == false) {
						    return; // The user aborted.
                        }
                    }
				}
			}

			node = node->Next;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the state of the specified bit.
	bool operator[] (int position) const {
		return IsSet(position);
	}

	bool operator== (const SparseBitVector& other) const {
		BitNode* a = nodes_.First();
		BitNode* b = other.nodes_.First();

		while(a && b) {
			if(a->Value.StartBit != b->Value.StartBit) {
				// The ranges can't be equal.
				return false;
			}
			else if(RangesAreEqual(a->Value, b->Value) == false) {
				// The ranges are not equal.
				return false;
			}
		}

		// Both vectors should have the same number of ranges.
        return (a == nullptr) && (b == nullptr);
	}

	bool operator!= (const SparseBitVector& other) const {
		return operator== (other) == false;
	}

	// Methods that allow a SparseBitVector to be manipulated using the bitwise operators.
	SparseBitVector& operator&= (const SparseBitVector& other) {
		And(other);
		return *this;
	}

	SparseBitVector& operator|= (const SparseBitVector& other) {
		Or(other);
		return *this;
	}

    SparseBitVector& operator= (const SparseBitVector& other) {
        if(&other != this) {
            nodes_.Clear();
            lastNode_ = nullptr;

            for(auto node = other.nodes_.First(); node; node = node->Next) {
                nodes_.Add(node->Value);
            }
        }

        return *this;
    }
};

} // namespace Analysis
#endif