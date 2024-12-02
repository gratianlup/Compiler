// Temporary.hpp
// Copyright (c) Lup Gratian
//
// Implements the Temporary class and the helpers.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Temporary.hpp"
#include "Instruction.hpp"
#include "Tags.hpp"

namespace IR {

int SmallUserList::GetCapacity(int count) {
	DebugValidator::IsSmallerOrEqual(count, MAX_COUNT);
	
	switch(count) {
		case 0:  return 0;
		case 1:  return 1;
		case 2:  return 2;
		case 3:  return 4;
		case 4:  return 4;
		case 5:  return 8;
		case 6:  return 8;
		case 7:  return 8;
		case 8:  return 8;
		case 9:  return 12;
		case 10: return 12;
		case 11: return 12;
		case 12: return 12;
		default: return 16; // For all other cases.
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SmallUserList::Resize(int newCapacity, int oldCapacity) {
	// Create a new array and copy the existing users.
	// Don't resize if the current capacity is less than double the new capacity
	// (this reduces the memory traffic and allocation).
	if((newCapacity > oldCapacity) || (newCapacity < (capacity_ / 2))) {
		Instruction** newUsers_ = new Instruction*[newCapacity];
		int limit = std::min(newCapacity, oldCapacity);
		std::memcpy(newUsers_, users_, limit * sizeof(Instruction*));

		// Delete the old array.
		delete[] users_;
		users_ = newUsers_;
		capacity_ = newCapacity;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SmallUserList::AddUser(Instruction* user, int index) {
	users_[index] = user;
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SmallUserList::HasUser(const Instruction* user, int count) const {
	for(int i = 0; i < count; i++) {
		if(users_[i] == user) return true;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SmallUserList::RemoveUser(int index, int listCount) {
	users_[index] = nullptr;

	// Shift all users to the left with 1 position.
	for(int i = index; i < (listCount - 1); i++) {
		users_[i] = users_[i + 1];
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SmallUserList::RemoveUser(Instruction* user, int listCount) {
	for(int i = 0; i < listCount; i++) {
		if(users_[i] == user) {
			RemoveUser(i, listCount);
			return;
		}
	}

	DebugValidator::Unreachable(); // Means the user is not in the list.
}

// ######################################################################################
// LargeUserList
// ######################################################################################
LargeUserList::LargeUserList(SmallUserList& other, int count) : 
		first_(nullptr), count_(0) {
	for(int i = 0; i < count; i++) {
		AddUser(other.GetUser(i));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
LargeUserList::~LargeUserList() {
	// Deallocate the memory used by the nodes.
	while(first_) {
		UserNode* next = first_->Next;
		FreeNode(first_);
		first_ = next;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LargeUserList::RemoveNode(UserNode* node, UserNode* prevNode) {
	if(prevNode) prevNode->Next = node->Next;
    else first_ = node->Next;

	FreeNode(node);
	count_--;
	if(count_ == 0) first_ = nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* LargeUserList::GetUser(int index) {
	DebugValidator::IsSmaller(index, count_);
	int ct = 0;
	UserNode* node = first_;

	while(node) {
		if(ct == index) {
			return node->User; // Found the user.
		}

		ct++;
		node = node->Next;
	}
		
	DebugValidator::Unreachable(); // Means the index is invalid.
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LargeUserList::AddUser(Instruction* user) {
	// Append at the beginning of the list.
	// It's the fastest way, and the order doesn't matter (this is treated like a set).
	UserNode* newNode = AllocateNode(user);
	newNode->Next = first_;
	first_ = newNode;
	count_++;
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool LargeUserList::HasUser(const Instruction* user) const {
	UserNode* node = first_;

	while(node) {
		if(node->User == user) return true;
		node = node->Next;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LargeUserList::ReplaceUser(int index, Instruction* newUser) {
	int ct = 0;
	UserNode* node = first_;

	while(node) {
		if(ct == index) {
			// Found the user, now replace it.
			node->User = newUser;
			return;
		}

		ct++;
		node = node->Next;
	}

	DebugValidator::Unreachable(); // Means the index is invalid.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LargeUserList::RemoveUser(int index) {
	int ct = 0;
	UserNode* prevNode = nullptr;
	UserNode* node = first_;

	while(node) {
		if(ct == index) {
			// Found the user, now delete it it.
			RemoveNode(node, prevNode);
			return;
		}

		ct++;
		prevNode = node;
		node = node->Next;
	}

	DebugValidator::Unreachable(); // Means the index is invalid.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void LargeUserList::RemoveUser(Instruction* user) {
	UserNode* prevNode = nullptr;
	UserNode* node = first_;

	while(node) {
		if(node->User == user) {
			// Found the user, now delete it it.
			RemoveNode(node, prevNode);
			return;
		}

		prevNode = node;
		node = node->Next;
	}

	DebugValidator::Unreachable(); // Means the index is invalid.
}

// ######################################################################################
// UserEnumerator
// ######################################################################################
UserEnumerator::UserEnumerator(const Temporary* temp) : 
		temp_(temp), valid_(true) {
	userCount_ = temp_->UserCount();

	// Select the type of the list of users.
	if(temp->HasUsers() == false) {
		valid_ = false;
	}
	else if(temp->HasSingleUser()) {
		type_ = UserType::Single;
	}
	else if(temp->HasFewUsers()) {
		type_ = UserType::SmallList;
		currentIndex_ = 0;
	}
	else {
		type_ = UserType::LargeList;
		currentNode_ = temp_->largeList_->GetFirstUser();
	}
}

UserEnumerator::UserEnumerator(const UserEnumerator& other) : 
		temp_(other.temp_), userCount_(other.userCount_), 
		currentIndex_(other.currentIndex_), currentNode_(other.currentNode_),
		type_(other.type_), valid_(other.valid_) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Instruction* UserEnumerator::Next() const {
	if(valid_ == false) return nullptr;

	Instruction* user; // The current user.

	if(type_ == UserType::Single) {
		user = temp_->singleUser_;
		valid_ = false;
	}
	else if(type_ == UserType::SmallList) {
		user = temp_->smallList_->GetUser(currentIndex_);
		currentIndex_++;
		valid_ = currentIndex_ < userCount_;
	}
	else {
		user = currentNode_->User;
		currentNode_ = currentNode_->Next;
		valid_ = currentNode_ != nullptr;
	}

	return user;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UserEnumerator& UserEnumerator::operator= (const UserEnumerator& other) {
	if(&other == this) return *this;

	temp_ = other.temp_;
	userCount_ = other.userCount_;
	currentIndex_ = other.currentIndex_;
	currentNode_ = other.currentNode_;
	type_ = other.type_;
	valid_ = other.valid_;
	return *this;
}

// ######################################################################################
// Temporary
// ######################################################################################
Temporary::~Temporary() {
	// If we have user lists, free the memory allocated to them.
	if(HasSingleUser()) {
		return;
	}
	else if(HasFewUsers()) {
		delete smallList_;
	}
	else delete largeList_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Temporary::Temporary(const Type* type, Instruction* definingInstr) :
		Operand(type, (int)Kind::Temp, definingInstr), singleUser_(nullptr) {} 
		
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Temporary* Temporary::GetTemporary(const Type* type, Instruction* definingInstr) {
	return new Temporary(type, definingInstr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Temporary::FreeImpl() {
	// A temporary is freed only if it has no users and no defining instruction.
	if(defInstr_) return;
	if(UserCount() > 0) return;

	// Not used anymore, can be freed.
	delete this;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Temporary::ResizeType Temporary::ExpandUserList(int newCount, int oldCount) {
	if(newCount <= SmallUserList::MAX_COUNT) {
		// We're expanding from a single user to a small list, 
		// or from a small list to a larger small list.
		int oldCapacity = SmallUserList::GetCapacity(oldCount);
		int newCapacity = SmallUserList::GetCapacity(newCount);

		if(oldCount == 1) {
			Instruction* temp = singleUser_;
			smallList_ = new SmallUserList(newCapacity);
			smallList_->AddUser(temp, 0);
		}
		else smallList_->Resize(newCapacity, oldCapacity);

		return ResizeType::ToSmallList;
	}

	// If we're here then we expand from a small list to a large list,
	// or already have a large list, case in which we do nothing.
	if(oldCount <= SmallUserList::MAX_COUNT) {
		SmallUserList* temp = smallList_;
		largeList_ = new LargeUserList(*temp, other_);
		delete temp; // Can be freed now.
	}

	return ResizeType::ToLargeList;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Temporary::ResizeType Temporary::ContractUserList(int newCount, int oldCount) {
	// If the new capacity is 1 we can get rid of the list.
	if(newCount == 1) {
		if(oldCount <= SmallUserList::MAX_COUNT) {
			Instruction* user = smallList_->GetUser(0);
			delete smallList_;
			singleUser_ = user;
		}
		else {
			Instruction* user = largeList_->GetUser(0);
			delete largeList_;
			singleUser_ = user;
		}

		return ResizeType::ToSingleUser;
	}
	else if(newCount <= SmallUserList::MAX_COUNT) {
		// We're contracting from a large list to a small list,
		// or from a larger small list to a smaller one.
		if(oldCount <= SmallUserList::MAX_COUNT) {
			int oldCapacity = SmallUserList::GetCapacity(oldCount);
			int newCapacity = SmallUserList::GetCapacity(newCount);
				
			// Resize only if really needed.
			if(newCapacity != oldCapacity) {
				smallList_->Resize(newCapacity, oldCapacity);
			}
		}
		else {
			LargeUserList* temp = largeList_;
			int newCapacity = SmallUserList::GetCapacity(newCount);
			smallList_ = new SmallUserList(newCapacity);

			// Copy the users from the large list to the small one.
			DebugValidator::IsSmallerOrEqual(temp->UserCount(), newCapacity);
			UserNode* node = temp->GetFirstUser();
			int ct = 0;

			while(node) {
				smallList_->AddUser(node->User, ct);
				node = node->Next;
				ct++;
			}

			// The large list can now be deleted.
			delete temp;
		}

		return ResizeType::ToSmallList;
	}

	return ResizeType::ToLargeList;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Temporary::AddUser(Instruction* user) {
	DebugValidator::IsNotNull(user);
	
	// If the user is already in the set don't add it again.
	if(HasUser(user)) return false;

	if(other_ == 0) {
		// This is the first user.
		singleUser_ = user;
		other_ = 1;
		return true;
	}
	else if(HasFewUsers()) {
		// It's possible that the list must be resized.
		int currentCount = other_;
		int currentCapacity = SmallUserList::GetCapacity(currentCount);
		ResizeType resize = ResizeType::None;

		if(currentCapacity < (other_ + 1)) {
			resize = ExpandUserList(other_ + 1, currentCount);
		}

		// If the list has not been resized, or after it has been resized
		// it's still a small list, add the user now.
		if(resize != ResizeType::ToLargeList) {
			smallList_->AddUser(user, other_);
			other_++;
			return true;
		}
	}

	// If we're here it means we have a large list.
	largeList_->AddUser(user);
	other_ = SmallUserList::MAX_COUNT + 1; // Mark that a large list is used.
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* Temporary::GetUser(int index) {
	DebugValidator::IsSmaller(index, UserCount());
	
	if(HasSingleUser()) {
		return singleUser_;
	}
	else if(HasFewUsers()) {
		return smallList_->GetUser(index);
	}
	else return largeList_->GetUser(index);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Temporary::HasUser(const Instruction* user) const {
	DebugValidator::IsNotNull(user);
	
	if(HasUsers() == false) return false;
	else if(HasSingleUser()) {
		return singleUser_ == user;
	}
	else if(HasFewUsers()) {
		return smallList_->HasUser(user, other_);
	}
	else return largeList_->HasUser(user);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Temporary::ReplaceUser(int index, Instruction* newUser) {
	DebugValidator::IsNotNull(newUser);
	DebugValidator::IsSmaller(index, UserCount());
	
	if(HasSingleUser()) {
		singleUser_ = newUser;
	}
	else if(HasFewUsers()) {
		smallList_->ReplaceUser(index, newUser);
	}
	else largeList_->ReplaceUser(index, newUser);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Temporary::RemoveUser(int index) {
	DebugValidator::IsSmaller(index, UserCount());
	
	if(HasSingleUser()) {
		singleUser_ = nullptr;
		other_ = 0;
	}
	else if(HasFewUsers()) {
		smallList_->RemoveUser(index, other_);
		other_--;
		ContractUserList(other_, other_ + 1);
	}
	else {
		largeList_->RemoveUser(index);
		int newCount = largeList_->UserCount();

		if(ContractUserList(newCount, newCount + 1) == ResizeType::ToSmallList) {
			// We use now a small list, move the count.
			other_ = newCount;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Temporary::RemoveUser(Instruction* user) {
	DebugValidator::IsTrue(HasUser(user));
	
	if(HasSingleUser()) {
		singleUser_ = nullptr;
		other_ = 0;
	}
	else if(HasFewUsers()) {
		smallList_->RemoveUser(user, other_);
		other_--;
		ContractUserList(other_, other_ + 1);
	}
	else {
		largeList_->RemoveUser(user);
		int newCount = largeList_->UserCount();

		if(ContractUserList(newCount, newCount + 1) == ResizeType::ToSmallList) {
			// We use now a small list, move the count.
			other_ = newCount;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Temporary::ReplaceWith(Operand* op) {
	DebugValidator::IsNotNull(op);
	if(op == this) return; // Nothing to do in this case.
    int skipped = 0;

	while((UserCount() - skipped) > 0) {
		Instruction* user = GetUser(skipped);

        if(user == defInstr_) {
            skipped++;
            continue;
        }
        
		for(int i = 0; i < user->SourceOpCount(); i++) {
			if(user->GetSourceOp(i) == this) {
				// Found an operand that is this temporary;
				// replace it with the specified operand.
				user->ReplaceSourceOp(i, op);
			}
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Temporary::ToStringImpl(int level) const {
    // We prefer to use the Name Tag, if it's available.
    if(auto nameTag = GetTag<NameTag>()) {
        return nameTag->Name();
    }
    else return "temp";
}

} // namespace IR