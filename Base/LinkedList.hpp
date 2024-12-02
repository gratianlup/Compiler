// LinkedList.hpp
// Copyright (c) Lup Gratian
//
// Represents a doubly linked list.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_LINKED_LIST_HPP
#define PC_BASE_LINKED_LIST_HPP

#include "DebugValidator.hpp"
#include "DefaultComparer.hpp"
#include "TypeInfo.hpp"
#include <cmath>
#include <cstdlib>

namespace Base {

template <class T, class Validator = DebugValidator>
class LinkedList {
public:
	template <class T>
	class Node {
	public:
		T Value;
		Node* Next;
		Node* Previous;

		Node(const T& value) : Value(value), Next(nullptr), Previous(nullptr) {}
	};

	typedef typename Node<T> TNode;
	typedef typename LinkedList<T, Validator> TList;
	typedef typename T Type;

private:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class Predicate>
	struct FindLastImpl {
		static TNode* Do(Predicate match, const TList& list) {
			TNode* node = list.last_;
		
			while(node) {
				if(match(node->Value)) return node;
				node = node->Previous;
			}

			return nullptr; // Not found.
		}
	};

	template <class Predicate>
	struct FindImpl {
		static TNode* Do(Predicate match, const TList& list) {
			TNode* node = list.first_;
		
			while(node) {
				if(match(node->Value)) return node;
				node = node->Next;
			}

			return nullptr; // Not found.
		}
	};

protected:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	TNode* first_;
	TNode* last_;
	int count_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Allocates a new node and calls its constructor.
	TNode* AllocateNode(const T& item) {
		return new TNode(item);
	}
	
	// Releases the memory used by the specified node.
	void ReleaseNode(TNode* node) {
		((T*)&node->Value)->~T(); // Call destructor.
		delete node;
	}

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes an empty LinkedList.
	LinkedList() : first_(nullptr), last_(nullptr), count_(0) {}
	
	virtual ~LinkedList() {
		Clear();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds the specified new node at the start of the LinkedList.
	TNode* AddFirst(TNode* node) {
		Validator::IsNotNull(node);
		
		if(first_ == nullptr) {
			// This is the first inserted node.
			first_ = last_ = node;
		}
		else {
			// Insert before the first one
			node->Next = first_;
			first_->Previous = node;
			first_ = node;
		}

		count_++;
		return node;
	}

	// Adds a new node containing the specified value at the start of the LinkedList.    
	TNode* AddFirst(const T& value) {
		AddFirst(AllocateNode(value));
		return first_;
	}

	// Adds the specified new node at the end of the LinkedList.    
	TNode* AddLast(TNode* node) {
		Validator::IsNotNull(node);
		
		if(last_ == nullptr) {
			// This is the first inserted node.
			first_ = last_ = node;
		}
		else {
			// Insert after the last one.
			node->Previous = last_;
			last_->Next = node;
			last_ = node;
		}

		count_++;
		return node;
	}

	// Adds a new node containing the specified value at the end of the LinkedList.    
	TNode* AddLast(const T& value) {
		AddLast(AllocateNode(value));
		return last_;
	}

	// Adds a new node containing the specified value at the end of the LinkedList.    
	TNode* Add(const T& value) {
		return AddLast(value);
	}

	// Adds the specified new node before the specified existing node in the LinkedList.    
	TNode* AddBefore(TNode* node, TNode* newNode) {
		Validator::IsNotNull(node);
		Validator::IsNotNull(newNode);

		newNode->Next = node;
		newNode->Previous = node->Previous;

		if(node->Previous) {
			node->Previous->Next = newNode;
		}

		node->Previous = newNode;

		if(node == first_) {
			// Make it the first node
			first_ = newNode;
		}

		count_++;
		return newNode;
	}

	// Adds a new node containing the specified value before the specified existing node.
	TNode* AddBefore(TNode* node, const T& value) {
		return AddBefore(node, AllocateNode(value));
	}

	// Adds the specified new node after the specified existing node in the LinkedList.    
	TNode* AddAfter(TNode* node, TNode* newNode) {
		Validator::IsNotNull(node);
		Validator::IsNotNull(newNode);

		newNode->Previous = node;
		newNode->Next = node->Next;

		if(newNode->Next) {
			newNode->Next->Previous = newNode;
		}

		node->Next = newNode;

		if(node == last_) {
			// Make it the first node.
			last_ = newNode;
		}

		count_++;
		return newNode;
	}

	// Adds a new node containing the specified value after the specified existing node.    
	TNode* AddAfter(TNode* node, const T& value) {
		return AddAfter(node, AllocateNode(value));
	}

	// Determines whether an item is in the LinkedList.
	bool Contains(const T& value) const {
		TNode* node = first_;
		
		while(node) {
			if(node->Value == value) return true;
			node = node->Next;
		}

		return false;
	}

	// Removes all items from the LinkedList.
	void Clear() {
		while(first_) {
			TNode* temp = first_;
			first_ = first_->Next;

			ReleaseNode(temp); // Release memory!
		}

		// Reset the linked list.
		first_ = nullptr;
		last_ = nullptr;
		count_ = 0;
	}
	
	// Removes the specified node from the LinkedList.
	void Remove(TNode* node) {
		Validator::IsNotNull(node);

		if((node == first_) && (node == last_)) {
			// This is the only node in the linked list.
			first_ = last_ = nullptr;
		}
		else if(node == first_) {
			node->Next->Previous = nullptr;
			first_ = node->Next;
		}
		else if(node == last_) {
			node->Previous->Next = nullptr;
			last_ = node->Previous;
		}
		else {
			// The node is in the middle of the linked list.
			node->Previous->Next = node->Next;
			node->Next->Previous = node->Previous;
		}

		ReleaseNode(node); // Release used memory.
		count_--;
	}

	// Removes the first occurrence of the specified value from the LinkedList.    
	bool Remove(const T& value) {
		TNode* node = Find([&value](const T& other) -> bool {
            return other == value;
        });

		if(node == nullptr) {
            return false; // Not found.
        }

		Remove(node); // Found, remove it.
		return true;
	}
	
	// Removes the node at the start of the list.
	void RemoveFirst() {
		Validator::IsLarger(count_, 0);
		Remove(first_); 
	}

	// Removes the node at the end of the LinkedList.    
	void RemoveLast() {
		Validator::IsLarger(count_, 0);
		Remove(last_);
	}

	// Copies the entire LinkedList to an array, starting at a particular array index.
	void CopyTo(T* dest, int index) const {
		Validator::IsNotNull(dest);
		int count = count_;
		int position = 0;
		TNode* node = first_;

		while((count > 0) && (node)) {
			dest[index + position] = node->Value;
			node = node->Next;
			position++;
			count--;
		}
	}	

	// Gets the first node of the LinkedList.
	TNode* First() const {
		return first_;
	}

	// Gets the last node of the LinkedList.
	TNode* Last() const {
		return last_;
	}

	// Gets the number of items contained in the LinkedList.	    
	int Count() const {
		return count_;
	}

	// Searches for an item that matches the conditions defined by the specified predicate,
	// and returns the last occurrence within the entire List    
	template <class Predicate>
	TNode* FindLast(Predicate match) const {
		return FindLastImpl<Predicate>::Do(match, *this);
	}

	// Searches for an item that matches the conditions defined by the specified predicate,
	// and returns the first occurrence within the entire List. 
	template <class Predicate>
	TNode* Find(Predicate match) const {
		return FindImpl<Predicate>::Do(match, *this);
	}

	// Performs the specified action on each item.
	template <class Predicate>
	void ForEach(Predicate action) const {
		TNode* node = first_;

		while(node) {
			action(node->Value);
			node = node->Next; // Advance to the next node.
		}
	}

	// Removes the all the items that match the conditions defined by the specified predicate. 
	template <class Predicate>
	int RemoveAll(Predicate match) {
		int removedItems = 0;
		TNode* node = first_;

		while(node) {
			TNode* next = node->Next;

			if(match(node->Value)) {
				Remove(node);
				removedItems++;
			}

			node = next;
		}

		return removedItems;
	}

	// Performs the specified action on each item.   
	template <class Predicate>
	bool TrueForAll(Predicate match) const {
		TNode* node = first_;

		while(node) {
			if(match(node->Value) == false) return false;
			node = node->Next; // Advance to the next node.
		}

		return true;
	}
};

} // namespace Base
#endif