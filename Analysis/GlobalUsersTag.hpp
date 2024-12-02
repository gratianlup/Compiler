// GlobalUsersTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag that can be used to associate constant values
// with a global variable. This can be helpful for certain heuristics.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_GLOBAL_USERS_TAG_HPP
#define PC_ANALYSIS_GLOBAL_USERS_TAG_HPP

#include "SparseBitVector.hpp"
#include "../IR/Constants.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Tag.hpp"
#include "../Base/DefaultComparer.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Create a pair representing an user (function reference)
// and the number of times the global is used inside it.
// It can be sorted ascendently by 'Times'.
MAKE_PAIR_ORDERED_SECOND(UserTimesPair, FunctionReference*, User, int, Times);

class GlobalUsersTag : public FunctionTag {
private:
    StaticList<UserTimesPair, 4> users_;
    StaticList<VariableReference*, 1> initializerUsers_;
    SparseBitVector* readPositions_;
    unsigned char hasRead_              : 1;
    unsigned char unknownPositionRead_  : 1;
    unsigned char unknownPositionWrite_ : 1;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    GlobalUsersTag();                                  // Should not be created.
	GlobalUsersTag(const GlobalUsersTag&);             // Should not be copied.
	GlobalUsersTag& operator= (const GlobalUsersTag&); // Should not be assigned.

    GlobalUsersTag(bool hasRead, SparseBitVector* readPositions,
                   bool unknownPositionRead, bool unknownPositionWrite) :
        hasRead_(hasRead),
        unknownPositionRead_(unknownPositionRead), 
        unknownPositionWrite_(unknownPositionWrite),
        readPositions_(nullptr) {
        // Create a copy of the position bitvector if required.
        if(readPositions && readPositions->HasBitsSet()) {
            readPositions_ = new SparseBitVector(*readPositions);
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    UserTimesPair* FindFunctionUser(FunctionReference* functionRef) {
        auto pair = users_.Find([functionRef](const UserTimesPair& pair) -> bool {
                        return pair.User == functionRef;
                    });

        return const_cast<UserTimesPair*>(pair);
    }

    const UserTimesPair* FindFunctionUser(FunctionReference* functionRef) const {
        return users_.Find([functionRef](const UserTimesPair& pair) -> bool {
            return pair.User == functionRef;
        });
    }

public:
    static const int Id = 0x6f670c19;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    ~GlobalUsersTag() {
        if(readPositions_) {
            delete readPositions_;
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

    // Creates a new tag, optionally setting the read positions
    // and flags marking the fact that there are reads/writes
    // at unknown positions.
    static GlobalUsersTag* GetGlobalUsers(bool hasRead = true,
                                          SparseBitVector* readPositions = nullptr,
                                          bool unknownPositionRead = false,
                                          bool unknownPositionWrite = false) {
        return new GlobalUsersTag(hasRead, readPositions, unknownPositionRead, 
                                  unknownPositionWrite);
    }

    // Returns the number of functions and initializers that use the global.
    int UserCount() const {
        return users_.Count() + initializerUsers_.Count();
    }

    // Returns the number of functions that use the global.
    int FunctionUserCount() const {
        return users_.Count();
    }

    // Returns the number of initializers that use the global.
    int InitializerUserCount() const {
        return initializerUsers_.Count();
    }

    // Returns 'true' if any function or initializer use the global.
    bool HasUsers() const {
        return (users_.Count() > 0) || (initializerUsers_.Count() > 0);
    }

    // Returns 'true' if any initializer uses the global.
    bool HasInitializerUsers() const {
        return initializerUsers_.Count() > 0;
    }

    // Returns 'true' if any function uses the global.
    bool HasFunctionUsers() const {
        return users_.Count() > 0;
    }

    // Returns 'true' if no function or initializer uses the global.
    bool HasNoUsers() const {
        return (users_.Count() == 0) && (initializerUsers_.Count() == 0);
    }

    // Returns 'true' if no function uses the global.
    bool HasNoFunctionUsers() const {
        return users_.Count() == 0;
    }

    // Returns 'true' if no initializer uses the global.
    bool HasNoInitializerUsers() const {
        return initializerUsers_.Count() == 0;
    }

    // Returns the number of times the global is used
    // in all functions that use it.
    int TotalFunctionUsages() const {
        int usages = 0;

        users_.ForEach([&usages](const UserTimesPair& pair) -> bool {
            usages += pair.Times;
            return true;
        });

        return usages;
    }

    // Returns the number of times the global is used
    // in all the functions and initializers that use it a.
    int TotalUsages() const {
        return TotalUsages() + initializerUsers_.Count();
    }

    // Returns the function user found at the specified position.
    FunctionReference* GetUser(int index) {
        return users_[index].User;
    }

    const FunctionReference* GetUser(int index) const {
        return users_[index].User;
    }

    // Returns the number of times the global is used inside
    // the function found at the specified position.
    int GetUserTimes(int index) const {
        return users_[index].Times;
    }

    // Returns the number of times the global is used inside
    // the specified function, or 0 if the function is not an user.
    int GetUserTimes(FunctionReference* functionRef) {
        DebugValidator::IsNotNull(functionRef);

        if(auto pair = FindFunctionUser(functionRef)) {
            return pair->Times;
        }
        else return 0;
    }

    // Performs the specified action on each function user.
    // bool Predicate(FunctionReference* functionRef, int times)
    template <class Predicate>
    void ForEachFunctionUser(Predicate action) {
        for(int i = 0; i < users_.Count(); i++) {
            if(action(users_[i].User, users_[i].Times) == false) {
                return;
            }
        }
    }

    // Adds the specified function as an user of the global,
    // having 'times' usages inside the function. If the function
    // is already added only the usage count is incremented.
    void AddFunctionUser(FunctionReference* functionRef, int times = 1) {
        DebugValidator::IsNotNull(functionRef);
        DebugValidator::IsLarger(times, 0);

        if(auto pair = FindFunctionUser(functionRef)) {
            pair->Times += times;
        }
        else {
            users_.Add(UserTimesPair(functionRef, times));
            functionRef->AddUser();
        }
    }

    // Increments the number of times the global
    // is used inside the specified function.
    void AddFunctionUsages(FunctionReference* functionRef, int times = 1) {
        DebugValidator::IsNotNull(functionRef);
        DebugValidator::IsNotNull(FindFunctionUser(functionRef));
        DebugValidator::IsLarger(times, 0);
        FindFunctionUser(functionRef)->Times += times;
    }

    // Returns 'true' if the specified function is an user of the global.
    bool HasFuntionUser(FunctionReference* functionRef) {
        DebugValidator::IsNotNull(functionRef);
        return FindFunctionUser(functionRef) != nullptr;
    }

    // Removes the specified function from the user list.
    void RemoveFunctionUser(FunctionReference* functionRef) {
        DebugValidator::IsNotNull(functionRef);

        for(int i = 0; i < users_.Count(); i++) {
            if(users_[i].User == functionRef) {
				users_[i].Times--;

				if(users_[i].Times == 0) {
					// Remove it completely.
					users_[i].User->Free();
					users_.RemoveAt(i);
				}

                return;
            }
        }

        DebugValidator::Unreachable();
    }

    // Removes the user found at the specified position.
    void RemoveFunctionUser(int index) {
        users_[index].User->Free();
        users_.RemoveAt(index);
    }

    // Decrements the number of times the global is used in the specified function.
    // If the usage count reaches zero the function is removed from the user list.
    void RemoveFunctionUsages(FunctionReference* functionRef, int times) {
        DebugValidator::IsNotNull(functionRef);
        DebugValidator::IsLarger(times, 0);
        DebugValidator::IsSmallerOrEqual(times, GetUserTimes(functionRef));

        auto pair = FindFunctionUser(functionRef);
        pair->Times -= times;

        if(pair->Times == 0) {
            RemoveFunctionUser(functionRef);
        }
    }

    // Removes all function users.
    void ClearFunctionUsers() {
        users_.ForEach([](UserTimesPair& pair) -> bool {
            pair.User->Free();
            return true;
        });

        users_.Clear();
    }

    // Removes all initializer users.
    void ClearIninitializerUsers() {
        initializerUsers_.ForEach([](VariableReference* variableRef) -> bool {
            variableRef->Free();
            return true;
        });

        initializerUsers_.Clear();
    }

    // Removes all function and initializer users.
    void ClearUsers() {
        users_.Clear();
        initializerUsers_.Clear();
    }

    VariableReference* GetInitializerUser(int index) {
        return initializerUsers_[index];
    }

    const VariableReference* GetInitializerUser(int index) const {
        return initializerUsers_[index];
    }

    void AddInitializerUser(VariableReference* variableRef) {
        DebugValidator::IsNotNull(variableRef);

        if(initializerUsers_.Contains(variableRef) == false) {
            initializerUsers_.Add(variableRef);
            variableRef->AddUser();
        }
    }

    // Returns 'true' if the global is used as part
    // of the initializer associated with the specified variable.
    bool HasInitializerUser(VariableReference* variableRef) {
        return initializerUsers_.Contains(variableRef);
    }

    // Removes the specified initializer user.
    void RemoveInitializerUser(VariableReference* variableRef) {
        DebugValidator::IsNotNull(variableRef);
        DebugValidator::IsTrue(HasInitializerUser(variableRef));
        initializerUsers_.Remove(variableRef);
        variableRef->Free();
    }

    // Removes the initializer user found at the specified position.
    void RemoveInitializerUser(int index) {
        initializerUsers_[index]->Free();
        initializerUsers_.RemoveAt(index);
    }

    // Performs the specified action on each initializer user.
    // bool Predicate(VariableRefernce* variableRef)
    template <class Predicate>
    void ForEachInitializerUser(Predicate action) {
        for(int i = 0; i < users_.Count(); i++) {
            if(action(initializerUsers_[i]) == false) {
                return;
            }
        }
    }

    // Returns 'true' if there are any reads from the global
    // at statically-unknown positions.
    bool HasUnknownPositionRead() const {
        return unknownPositionRead_;
    }

    void SetHasUnknownPositionRead(bool value) {
        unknownPositionRead_ = value;
    }

    // Returns 'true' if there are any writes to the global
    // at statically-unknown positions.
    bool HasUnknownPositionWrite() const {
        return unknownPositionWrite_;
    }

    void SetHasUnknownPositionWrite(bool value) {
        unknownPositionWrite_ = value;
    }

    // Returns 'true' if the positions at which reads take place
    // are statically-known and can be queried.
    bool AreReadPositionsKnown() const {
        return readPositions_ != nullptr;
    }

    // Returns 'true' if the positions at which reads take place
    // are not statically-known and can not be queried.
    bool AreReadPositionsNotKnown() const {
        return readPositions_ == nullptr;
    }

    // Returns the read positions as a bit vector.
    SparseBitVector* ReadPositions() {
        return readPositions_;
    }

    const SparseBitVector* ReadPositions() const {
        return readPositions_;
    }

    // Returns 'true' if there is a read from the specified position.
    bool HasReadOnPosition(__int64 position) const {
		if((int)position != position) {
			return true;
		}

        if(unknownPositionRead_) {
            return true;
        }
        else if(readPositions_) {
            return readPositions_->IsSet((int)position);
        }
        else return false;
    }

    // Marks that there is a read from the specified position.
    void SetReadOnPosition(int position) {
        DebugValidator::IsNotNull(readPositions_);
        readPositions_->SetBit(position);
    }

    // Marks that there is no read from the specified position.
    void ResetReadOnPosition(int position) {
        DebugValidator::IsNotNull(readPositions_);
        readPositions_->ResetBit(position);
    }

    // Returns 'true' if it's possible that the global has reads.
    bool HasAnyReads() const {
        return hasRead_;
    }

    // Returns 'true' if it's certain hat the global has no reads.
    bool HasNoReads() const {
        return hasRead_ == false;
    }

    bool operator== (const GlobalUsersTag& other) const {
        if((users_ != other.users_) ||
           (unknownPositionRead_ != other.unknownPositionRead_) ||
           (unknownPositionWrite_ != other.unknownPositionWrite_)) {
            return false;
        }

        if(hasRead_ != other.hasRead_) {
            return false;
        }
        else if((readPositions_ == nullptr) != (other.readPositions_ == nullptr)) {
            return false;
        }
        else if(readPositions_ && other.readPositions_) {
            return *readPositions_ == *other.readPositions_;
        }
        else return true;
    }

    bool operator!= (const GlobalUsersTag& other) const {
        return operator==(other) == false;
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct TagPromoter<Analysis::GlobalUsersTag> {
		static bool Is(const Tag* tag) {
			return tag->GetId() == Analysis::GlobalUsersTag::Id;
		}

		static Analysis::GlobalUsersTag* As(Tag* tag) {
			return Is(tag) ? static_cast<Analysis::GlobalUsersTag*>(tag) : nullptr;
		}
	};
} // namespace Detail
} // namespace IR
#endif