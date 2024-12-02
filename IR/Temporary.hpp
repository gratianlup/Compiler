// Temporary.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_TEMPORARY_HPP
#define PC_IR_TEMPORARY_HPP

#include "Operand.hpp"
#include "IRType.hpp"
#include "Tagged.hpp"
#include "../Base/DebugValidator.hpp"
#include <memory>
using namespace Base;

namespace IR {

// Forward declarations.
class Instruction;


// List used when the number of users is small.
class SmallUserList {
private:
	Instruction** users_;
	int capacity_;

public:
	// Creates a small user list with the specified capacity.
	SmallUserList(int capacity) : 
			users_(new Instruction*[capacity]), capacity_(capacity) {}

	~SmallUserList() {
		delete[] users_;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// The maximum number of users that should be stored in such a list.
	static const int MAX_COUNT = 8;

	// Returns the recommended capacity for the specified number of users.
	static int GetCapacity(int count);

	// Resizes the user array so that it can store at most 'newCapacity' users.
	void Resize(int newCapacity, int oldCapacity);

	// Returns the user found at the specified index.
	Instruction* GetUser(int index) {
		return users_[index];
	}

	const Instruction* GetUser(int index) const {
		return users_[index];
	}

	// Adds the specified user.
	bool AddUser(Instruction* user, int index);

	// Returns 'true' if the specified user is found in the list.
	bool HasUser(const Instruction* user, int count) const;

	// Replaces the user at the specified index with a new one.
	void ReplaceUser(int index, Instruction* newUser) {
		users_[index] = newUser;
	}

	// Removes the user at the specified index from the list.
	void RemoveUser(int index, int listCount);

	// Removes the specified user from the list.
	void RemoveUser(Instruction* user, int listCount);
};


// The node in the 'LargeUserList' linked list.
struct UserNode {
	Instruction* User;
	UserNode* Next;

	UserNode() : User(nullptr), Next(nullptr) {}
	UserNode(Instruction* user, UserNode* next = nullptr) : 
			User(user), Next(next) {}
};


// List used when the number of users is large.
class LargeUserList {
private:
	UserNode* first_;
	int count_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	UserNode* AllocateNode(Instruction* user) {
		return new UserNode(user);
	}

	void FreeNode(UserNode* node) {
		delete node;
	}

	void RemoveNode(UserNode* node, UserNode* prevNode);

public:
	LargeUserList() : first_(nullptr), count_(0) {}

	// Creates a 'LargeUserList' object initialized with the users from
	// the specified 'SmallUserList' objects.
	LargeUserList(SmallUserList& other, int count);

	~LargeUserList();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the number of users in this list.
	int UserCount() const {
		return count_;
	}

	// Returns the node that holds the first user. Useful for implementing enumerators.
	UserNode* GetFirstUser() {
		return first_;
	}

	Instruction* GetUser(int index);
	const Instruction* GetUser(int index) const;
	bool AddUser(Instruction* user);
	bool HasUser(const Instruction* user) const;
	void ReplaceUser(int index, Instruction* newUser);
	void RemoveUser(int index);
	void RemoveUser(Instruction* user);
};


// A helper that enumerates over a list of users.
class UserEnumerator {
private:
	enum class UserType {
		Single,
		SmallList,
		LargeList
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const Temporary* temp_;
	mutable UserNode* currentNode_; // Used for large user list.
	mutable int currentIndex_;      // Used for small user list.
	int userCount_;                 // Used for small user list.
	UserType type_;                 // The type of the user list.
	mutable bool valid_;

public:
	UserEnumerator(const Temporary* temp);

	UserEnumerator(const UserEnumerator& other);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the current user and advances the pointer.
	// If the last user has been reached it returns 'nullptr'.
	const Instruction* Next() const;

	Instruction* Next() {
		auto result = static_cast<const UserEnumerator*>(this)->Next();
		return const_cast<Instruction*>(result);
	}

	// Returns 'true' if 'Next' will return a valid user.
	bool IsValid() const {
		return valid_;
	}

	UserEnumerator& operator= (const UserEnumerator& other);
};


// Represents a temporary that can be used to store the result of an instruction.
// Contains a def-use chain that is optimized for space and iteration speed
// (for a small number of users an array is used; when the number grows
// it's changed to a linked list).
class Temporary : public Operand, public Tagged<Tag> {
private:
	// The type of the operation performed on a user list.
	enum class ResizeType {
		None,
		ToSingleUser,
		ToSmallList,
		ToLargeList
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	union {
		Instruction* singleUser_;  // 'other_' == 0
		SmallUserList* smallList_; // 'other_' <= SmallUserList::MAX_COUNT
		LargeUserList* largeList_; // otherwise
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods to query info about the list of users.
	bool HasFewUsers() const   { return other_ <= SmallUserList::MAX_COUNT; }

	// Expands the list of user so that it can hold at least 'newCount' users.
	ResizeType ExpandUserList(int newCount, int oldCount);

	// Contracts the list of user so that it can hold at least 'newCount' users.
	ResizeType ContractUserList(int newCount, int oldCount);

	// Implements a for-each for the user list using a predicate.
	template <class Predicate>
	void ForEachUserImpl(Predicate action) {
		UserEnumerator userEnum = GetUserEnumerator();
		int index = 0;

		while(userEnum.IsValid()) {
			Instruction* user = userEnum.Next();
			if(action(user, index) == false) {
				// The user wants to stop.
				break;
			}

			index++;	
		}
	}

protected:
	Temporary(const Type* type, Instruction* definingInstr);

	// Frees the temporary. A temporary is freed only if it has no users
	// and no associated defining instruction.
	virtual void FreeImpl() override;

    virtual string ToStringImpl(int level) const override;

public:
	friend class UserEnumerator; // Gives it access to the user lists.

	// Factory methods for creating temporary instances.
	static Temporary* GetTemporary(const Type* type, Instruction* definingInstr = nullptr);

	virtual ~Temporary();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds the specified user to the list of users.
	// Returns 'true' if the user could be added (was not already in the list).
	bool AddUser(Instruction* user);

	// Returns the user found at the specified index.
	// Note that this method may not be optimal; use an iterator instead.
	Instruction* GetUser(int index);

	const Instruction* GetUser(int index) const {
		return const_cast<Temporary*>(this)->GetUser(index);
	}

	// Returns the number of instructions that use this temporary.
	int UserCount() const {
		if(HasFewUsers()) return other_; // IsValid for single user too.
		else return largeList_->UserCount();
	}

    // Returns 'true' if the temporary is used as a source by a single instruction
    // (not that it may be used multiple times by it - consider a 'call', for example).
    bool HasSingleUser() const {
        return other_ == 1; 
    }

    // Returns 'true' if the temporary is not used as a source by any instruction
    // (the defining instruction may be dead in this case).
    bool HasNoUser() const {
        return other_ == 0;
    }

    // Returns 'true' if the temporary is used as a source by at least one instruction.
    bool HasUsers() const {
        return other_ > 0;
    }
    
    // Returns 'true' if the specified instruction uses this temporary.
	bool HasUser(const Instruction* user) const;

	// Replaces the user found at the specified index with another user.
	void ReplaceUser(int index, Instruction* newUser);

	// Removes the user found at the specified index from the user list.
	void RemoveUser(int index);

	// Removes the specified user from the user list.
	void RemoveUser(Instruction* user);

	// Changes the operands of all users so that the specified operand
	// is used instead of this temporary.
	void ReplaceWith(Operand* op);

	// Returns an enumerator for the list of users.
	// This should be the preferred method for accessing the list of users.
	UserEnumerator GetUserEnumerator() {
		return UserEnumerator(this);
	}

	const UserEnumerator GetUserEnumerator() const {
		return UserEnumerator(this);
	}

	// Performs the specified action on each user.
    // void Predicate(Instruction* user, int userIndex)
	template <class Predicate>
	void ForEachUser(Predicate action) {
		ForEachUserImpl<Predicate>(action);
	}

	template <class Predicate>
	void ForEachUser(Predicate action) const {
		ForEachUserImpl<Predicate>(action);
	}

	// Returns the number of users that match the conditions of the specified predicate.
	template <class Predicate>
	int CountMatchingUsers(Predicate match) const {
		UserEnumerator userEnum = GetUserEnumerator();
		int count = 0;

		while(userEnum.IsValid()) {
			Instruction* user = userEnum.Next();
			if(match(user)) count++;
		}

		return count;
	}

	// Returns 'true' if at least one user is found that matches 
	// the conditions of the specified predicate.
	template <class Predicate>
	bool HasMatchingUser(Predicate match) const {
		UserEnumerator userEnum = GetUserEnumerator();

		while(userEnum.IsValid()) {
			Instruction* user = userEnum.Next();
			if(match(user)) return true;
		}

		return false;
	}

	// Returns 'true' if all users match the conditions of the specified predicate.
	template <class Predicate>
	int TrueForAllUsers(Predicate cond) const {
		UserEnumerator userEnum = GetUserEnumerator();

		while(userEnum.IsValid()) {
			Instruction* user = userEnum.Next();
			if(cond(user) == false) return false;
		}

		return true;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<Temporary> {
		static bool Is(const Operand* op) {
			return (Operand::Kind)op->kind_ == Operand::Kind::Temp;
		}

		static Temporary* As(Operand* op) {
			return Is(op) ? static_cast<Temporary*>(op) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif