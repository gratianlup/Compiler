// Dictionary.hpppp
// Copyright (c) Lup Gratian
//
// Represents a collection of key-value pairs.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_DICTIONARY_HPP
#define PC_BASE_DICTIONARY_HPP

#include "DictionaryConst.hpp"
#include "DebugValidator.hpp"
#include "DefaultComparer.hpp"
#include <cmath>
#include <cstdlib>
#include <limits>

namespace Base {

template <class TKey, class TValue, bool UseValue = false,
		  class Comparer = DefaultComparer<TKey, UseValue>,
		  class Validator = DebugValidator>
class Dictionary : public DictionaryConst {
private:
	template <class T>
	class Node {
	public:
		T Value;
		Node* Next;
		Node* Previous;

		Node(const T& value) : Value(value), Next(nullptr), Previous(nullptr) {}
	};

public:
	template <class K, class V, class Comparer>
	class Pair {
	public:
		typedef typename Pair<K, V, Comparer> TPair;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		K Key;
		V Value;

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		Pair() {}

		Pair(const K &key, const V &value) : Key(key), Value(value) {}

		// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
		bool operator== (const TPair &other) const {
			return Comparer::Compare(other.Key, Key) == 0;
		}

		bool operator!= (const TPair &other) const {
			return Comparer::Compare(other.Key, Key) != 0;
		}

		bool operator< (const TPair &other) const {
			return Comparer::Compare(other.Key, Key) < 0;
		}

		bool operator<= (const TPair &other) const {
			return Comparer::Compare(other.Key, Key) <= 0;
		}

		bool operator> (const TPair &other) const {
			return Comparer::Compare(other.Key, Key) > 0;
		}

		bool operator>= (const TPair &other) const {
			return Comparer::Compare(other.Key, Key) >= 0;
		}
	};

	typedef typename Pair<TKey, TValue, Comparer> TPair;
	typedef typename Dictionary<TKey, TValue, UseValue, Comparer, Validator> TDict;
	typedef typename TKey KeyType;
	typedef typename TValue ValueType;

private:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	typedef typename Node<TPair> TNode;

	TNode** table_;
	int count_;
	int size_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Verifies if the specified number is prime.
	bool IsPrime(int n) {
		if(n % 2 != 0) {
			int limit = (int)std::sqrt((double)n);

			for(int i = 3; i < limit; i += 2) {
				if((n % i) == 0) {
					return false; // Not prime!
				}
			}

			return true;
		}
		
		return n == 2; // 2 is also prime!
	}

	// Gets a prime number larger or equal to the specified size.
	int GetTableSize(int size) {
		// Find the first prime number larger than the specified value.
		for(int i = 0; i < PRIME_NUMBERS; i++) {
			if(PRIMES[i] > size) {
				return PRIMES[i];
			}
		}

		// No enough large prime could be found, search for the first prime grater 
		// than the value. We use the OR operation to make sure we start with an odd number.
		int limit = std::numeric_limits<int>::max();

		for(int i = (size | 1); i < limit; i += 2) {
			if(IsPrime(i)) {
				return i;
			}
		}

		return size;
	}

	// Allocates a node that has the specified key and value.
	TNode* AllocateNode(const TKey& key, const TValue& value) {
		return new TNode(TPair(key, value));
	}

	// Releases the memory used by the specified node.    
	void ReleaseNode(TNode* node){
		delete node;
	}

	// Allocates a table large enough to store the specified number 
	// of Node object pointers. The pointers are all set to nullptr.
	TNode** AllocateTable(int size) {
		// Allocate an array of Node pointers.
		TNode** table_ = new TNode*[size];

		// Set all pointers to nullptr.
		for(int i = 0; i < size; i++) {
			table_[i] = nullptr;
		}

		return table_;
	}

	// Releases the used memory by the specified hash table.    
	void ReleaseTable(TNode*** table) {
		delete[] *table;
		*table = nullptr;
	}
	
	// Computes the hash code of the specified key, depending on the size
	// of the current hash table.
	unsigned ComputeHashCode(const TKey& key) const {
		// We use the Comparer in order to compute the hash code.
		unsigned hash = Comparer::GetHashCode(key);
		return hash % size_;
	}

	// Searches for the pair that has the specified hash value and key.
	TNode* FindPair(unsigned hash, const TKey& key) const {
		TNode* node = table_[hash];

		// Search all entries that have the specified hash code.
		while(node) {
			if(Comparer::Compare(node->Value.Key, key) == 0) {
				return node; // Found!
			}

			node = node->Next; // Advance to next node.
		}

		return nullptr; // Not found.
	}

	
	// Add add the specified pair Node to the hash table.
	void AddPair(unsigned hash, TNode* pairNode) {
		if(table_[hash] == nullptr) {
			// This is the first entry in the specified position.
			table_[hash] = pairNode;
		}
		else {
			// There are already items inserted at this position. We resolve this
			// using a linked list.
			pairNode->Next = table_[hash];
			table_[hash]->Previous = pairNode;
			table_[hash] = pairNode;
		}

		count_++;
	}
	
	// Add a new pair that has the specified key and value to the hash table.
	void AddPair(unsigned hash, const TKey& key, const TValue& value) {
        TNode* node = FindPair(hash, key);

        if(node) {
            node->Value.Value = value;
            return;
        }

		// Allocate a new node to store the pair.
		node = AllocateNode(key, value);
		AddPair(hash, node);
	}

	// Determines if the hash table should be resized, based on the load factor.
	bool ShouldResize(int newCount)	{
		return ((double)newCount / (double)size_) >= MAX_LOAD_FACTOR;
	}

	// Resize the hash table so it can store the specified number of pairs.	
	// It rehashes all existing pairs.
	void ResizeTable(int newCount) {
		// Don't resize if load factor isn't too high.
		if(ShouldResize(newCount) == false) {
			return;
		}

		// We create a new table in which we rehash all existing items.
		// After that, we can remove the old table. Note that we copy the nodes
		// from the old table to the new one, so we don't make unnecessary allocations.
		TNode** oldTable = table_;
		int oldSize = size_;

		size_ = GetTableSize(newCount * 2);
		table_ = AllocateTable(size_);
		count_ = 0;

		if(oldTable == nullptr) {
			return; // We don't need to copy anything.
		}

		// Rehash all pairs.
		for(int i = 0; i < oldSize; i++) {
			TNode* node = oldTable[i];

			while(node) {
				// Save the next node and destroy the links between this node and others.
				TNode* nextNode = node->Next;
				node->Previous = nullptr;
				node->Next = nullptr;

				// Add the pair to the new table.
				unsigned newHash = ComputeHashCode(node->Value.Key);
				AddPair(newHash, node);

				node = nextNode; // Advance to the next node.
			}
		}

		ReleaseTable(&oldTable); // Deallocate the old table.
	}

	// Removes the specified pair from the hash table.    
	void RemovePair(unsigned hash, TNode* node) {
		if(node->Previous == nullptr) {
			table_[hash] = node->Next; // First entry.
			
			if(node->Next) {
				node->Next->Previous = nullptr;
			}
		}
		else if(node->Next == nullptr) {
			node->Previous->Next = nullptr; // Last entry.
		}
		else {
			// Remove in the middle.
			node->Previous->Next = node->Next;
			node->Next->Previous = node->Previous;
		}

		// Deallocate node, decrement count and increment version.
		ReleaseNode(node);
		count_--;
	}

    void CopyFromOther(const TDict& other) {
        if(other.count_ > 0) {
		    for(int i = 0; i < other.size_; i++) {
			    TNode* node = other.table_[i];

			    while(node) {
				    // Add a copy of the pair.
				    Add(node->Value.Key, node->Value.Value);
				    node = node->Next; // Advance to the next node.
			    }
		    }
        }
    }

public:
	// Initializes an empty Dictionary with the default size.
	Dictionary() : table_(nullptr), size_(32), count_(0) {
		ResizeTable(size_);
	}

	// Initializes a Dictionary with the specified size.
	// Should be a prime number.    
	explicit Dictionary(int size) : table_(nullptr), size_(size), count_(0)	{ 
		ResizeTable(size_);
	}

	// Initializes the Dictionary with a copy of the specified Dictionary.    
	Dictionary(const Dictionary<TKey, TValue>& other) :
		table_(nullptr), size_(0), count_(0) {
		ResizeTable(other.size_);

		// Copy all pairs from the dictionary.
        CopyFromOther(other);
	}

	// Constructor with move semantics.
	Dictionary(Dictionary<TKey, TValue>&& other) :
			table_(other.table_), size_(other.size_), count_(other.count_) {
		other.table_ = nullptr;
		other.count_ = 0;
	}

	virtual ~Dictionary() {
		if(table_) {
			Clear();
			ReleaseTable(&table_);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds the specified pair to the Dictionary.
	void Add(const TKey& key, const TValue& value) {
		ResizeTable(count_ + 1); // Resize the table if it's needed

		unsigned hash = ComputeHashCode(key);
		AddPair(hash, key, value);
	}

	// Determines whether the Dictionary contains the specified key.
	bool ContainsKey(const TKey& key) const	{
        if(count_ == 0) {
			return false;
		}

		unsigned hash = ComputeHashCode(key);
		return FindPair(hash, key);
	}

	// Determines whether the Dictionary contains the specified value.
	// Performs a linear O(n) search.    
	bool ContainsValue(const TValue& value) const {
        if(count_ == 0) {
			return false;
		}

		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];

			while(node) {
				if(Comparer::Compare(node->Value.Value, value) == 0) {
					return true; // Found!
				}

				node = node->Next; // Advance to next node.
			}
		}

		return false; // Not found.
	}
	
	// Removes all pairs from the Dictionary.
	void Clear() {
        if(count_ == 0) return;

		// We delete all nodes in all table locations.
		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];

			while(node) {
				TNode* temp = node->Next;
				ReleaseNode(node);
				node = temp;
			}

			table_[i] = nullptr; // Reset the table entry.
		}

		count_ = 0;
	}

	// Removes the pair with the specified key. Returns true if the key could be found.
	bool Remove(const TKey& key) {
        if(count_ == 0) {
			return false;
		}

		unsigned hash = ComputeHashCode(key);
		TNode* node = FindPair(hash, key);

		if(node) RemovePair(hash, node);
		return node != nullptr;
	}

	// Gets the value associated with the specified key. 
    // Returns true if the key could be found.
	bool TryGetValue(const TKey& key, TValue *value) const {
		Validator::IsNotNull(value);

        if(count_ == 0) {
			return false;
		}

		unsigned hash = ComputeHashCode(key);
		TNode* node = FindPair(hash, key);

		if(node) *value = node->Value.Value;
		return node;
	}
	
	// Copies all elements of to an array, starting at a particular array index.
	void CopyTo(TPair* destination, int index) const {
		Validator::IsNotNull(destination);

		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];

			while(node)	{
				destination[index++] = node->Value;
				node = node->Next; // Advance to the next node.
			}
		}
	}

	// Gets the number of key/value pairs contained in the Dictionary.
	int Count() const {
		return count_;
	}
	
	// Performs the specified action on each key/value pair.
	// bool Predicate(const TPair& pair);
	template <class Predicate>
	void ForEach(Predicate action) const {
		if(count_ == 0) {
			return;
		}

		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];

			while(node) {
				action(node->Value);
				node = node->Next; // Advance to the next node.
			}
		}
	}

	// Performs the specified action on each key/value pair.
	// bool Predicate(TPair& pair);
	template <class Predicate>
	void ForEach(Predicate action) {
		if(count_ == 0) {
			return;
		}

		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];

			while(node) {
				action(node->Value);
				node = node->Next; // Advance to the next node.
			}
		}
	}

	// Performs the specified action on each value.
	// bool Predicate(const TValue& value);
	template <class Predicate>
	void ForEachValue(Predicate action) const {
		if(count_ == 0) {
			return;
		}

		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];

			while(node) {
				action(node->Value.Value);
				node = node->Next; // Advance to the next node.
			}
		}
	}

	// Performs the specified action on each value.
	// bool Predicate(TValue& value);
	template <class Predicate>
	void ForEachValue(Predicate action) {
		if(count_ == 0) {
			return;
		}

		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];

			while(node) {
				action(node->Value.Value);
				node = node->Next; // Advance to the next node.
			}
		}
	}

	// Removes all all key/value pairs that match the specified predicate.
	// bool Predicate(TPair& pair);
	template <class Predicate>
	int RemoveAll(Predicate match) {
		if(count_ == 0) {
			return 0;
		}

		int removedItems = 0;

		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];
			TNode* next = node ? node->Next : nullptr;

			while(node) {
				if(match(node->Value)) {
					Remove(node->Value.Key);
					removedItems++;
				}

				node = next;
			}
		}

		return removedItems;
	}

	// Return 'true' if all key/value pairs match the specified predicate.
	// bool Predicate(const TPair& pair);
	template <class Predicate>
	bool TrueForAll(Predicate match) const {
		if(count_ == 0) {
			return false;
		}
		
		for(int i = 0; i < size_; i++) {
			TNode* node = table_[i];

			while(node) {
				if(match(node->Value) == false) {
                    return false;
                }

				node = node->Next; // Advance to the next node.
			}
		}

		return true;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    TDict& operator= (const TDict& other) {
        Clear();
        ResizeTable(other.size_);
        CopyFromOther(other);
        return *this;
    }

    // Implements move semantics.
    TDict& operator= (TDict&& other) {
        Clear();
        ReleaseTable(&table_);
        
        table_ = other.table_;
        count_ = other.count_;
        size_ = other.size_;

        other.table_ = nullptr;
        other.count_ = 0;
        return *this;
    }

	const TValue& operator[] (const TKey& key) const {
        Validator::IsLarger(count_, 0);
		unsigned hash = ComputeHashCode(key);
		TNode* node = FindPair(hash, key);
		
        Validator::IsNotNull(node);
		return node->Value.Value;
	}

	TValue& operator[] (const TKey& key) {
        Validator::IsLarger(count_, 0);
		unsigned hash = ComputeHashCode(key);
		TNode* node = FindPair(hash, key);
		
        Validator::IsNotNull(node);
		return node->Value.Value;
	}
};

} // namespace Base
#endif