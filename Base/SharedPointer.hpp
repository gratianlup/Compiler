// SharedPointer.hpp
// Copyright (c) Lup Gratian
//
// Smart pointer that keeps track of the number of owners. 
// When the number reaches 0 the pointed object is automatically deleted.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_SHARED_POINTER_HPP
#define PC_BASE_SHARED_POINTER_HPP

#include "../Abstraction/Platform.hpp"
#include "DebugValidator.hpp"
#include "LocalPointer.hpp"
#include "PointerReleaser.hpp"
using namespace Abstraction;

namespace Base {

// Policy for pointers that are used only in a single-threaded context.
struct STPolicy {
	typedef int CounterType;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static int Increment(CounterType* counter) {
		(*counter)++;
		return *counter;
	}
	
	static int Decrement(CounterType* counter) {
		(*counter)--;
		return *counter;
	}
};


// Policy for pointers that can be used in a multi-thread context.
struct MTPolicy {
	typedef volatile int CounterType;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static int Increment(CounterType* counter) {
		return Interlocked::Increment(counter);
	}

	static int Decrement(CounterType* counter) {
		return Interlocked::Decrement(counter);
	}
};


template <class T, bool IsArray = false, class CounterPolicy = STPolicy, 
		  class Validator = DebugValidator>
class shared {
private:
	typedef typename shared<T, IsArray, CounterPolicy, Validator> TShared;
	typedef typename CounterPolicy::CounterType TCounter;
	typedef typename local<T, IsArray, Validator> TLocal;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	TCounter* counter_;
	T* pointer_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void TryRelease() {
		// If this is the last reference delete the pointed object and the counter.
		if(counter_ && (CounterPolicy::Decrement(counter_) == 0)) {
			delete counter_;
			PointerReleaser<T, IsArray>::Release(pointer_);
		}

		counter_ = nullptr;
		pointer_ = nullptr;
	}

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	shared(T* ptr, TCounter* ct) : pointer_(ptr), counter_(ct) {}

	shared() : pointer_(nullptr), counter_(nullptr) {}

	shared(T* p) : counter_(new TCounter()), pointer_(p) {
		*counter_ = 1;
	}

	template <class U>
	shared(U* p) : counter_(new TCounter()), pointer_(static_cast<T*>(p)) {
		*counter_ = 1;
	}
	
	template <class U>
	shared(const U* p) : counter_(new TCounter()), pointer_(static_cast<T*>(const_cast<T*>(p))) {
		*counter_ = 1;
	}

	shared(const TShared& other) : 
			counter_(other.counter_), pointer_(other.pointer_) {
		if(counter_) {
			CounterPolicy::Increment(counter_);
		}
	}
	
	shared(TLocal& other) : pointer_(other.Get()), counter_(new TCounter()) {
		*counter_ = 1;
	}

	template <class U>
	shared(local<U, IsArray, Validator>& other) : 
		pointer_(static_cast<T*>(other.Get())), counter_(new TCounter()) {
		*counter_ = 1;
	}

	template <class U>
	shared(const shared<U, IsArray, CounterPolicy, Validator>& other) {
		other.Copy<T>(&counter_, &pointer_);
		if(counter_) {
			CounterPolicy::Increment(counter_);
		}
	}

	~shared() {
		TryRelease();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	T& operator* () const {
		Validator::IsNotNull(pointer_);
		return *pointer_;
	}

	T* operator-> () const {
		Validator::IsNotNull(pointer_);
		return pointer_;
	}

	bool operator! () const {
		return pointer_;
	}

	operator T* () const {
		return pointer_;
	}

	TShared& operator= (const TShared& other) {
		// Check for self-assignment.
		if(pointer_ == other.pointer_) {
			return *this;
		}

		TryRelease(); // Release the current object.
		pointer_ = other.pointer_;
		counter_ = other.counter_;

		if(counter_) {
			CounterPolicy::Increment(counter_);
		}

		return *this;
	}

	//TShared& operator= (T* other) {
	//	// Release the current object.
	//	TryRelease();

	//	pointer_ = other;
	//	counter_ = new TCounter();
	//	*counter_ = 1;
	//	return *this;
	//}

	TShared& operator= (TLocal& other) {
		return operator= (other.Get());
	}

	template <class U>
	TShared& operator= (const shared<U, IsArray, CounterPolicy, Validator>& other) {
		TryRelease(); // Release the current object.
		pointer_ = static_cast<T*>(other.Raw());
		counter_ = other.Counter();

		if(counter_) {
			CounterPolicy::Increment(counter_);
		}

		return *this;
	}

	friend bool operator== (const TShared& first, const TShared& second) {
		return first.pointer_ == second.pointer_;
	}

	friend bool operator== (const TShared& first, T* second) {
		return first.pointer_ == second;
	}

	friend bool operator== (const T* first, const TShared& second) {
		return first == second.pointer_;
	}

	template <class U>
	friend bool operator== (const TShared& first, U* second) {
		return first.pointer_ == second;
	}

	template <class U>
	friend bool operator== (U* first, const TShared& second) {
		return first == second.pointer_;
	}

	template <class U>
	bool operator== (const TShared& other) {
		return pointer_ == other.pointer_;
	}

	friend bool operator!= (const TShared& first, const TShared& second) {
		return first.pointer_ != second.pointer_;
	}

	friend bool operator!= (const TShared& first, T* second) {
		return first.pointer_ != second;
	}

	friend bool operator!= (T* first, const TShared& second) {
		return first != second.pointer_;
	}

	template <class U>
	friend bool operator!= (const TShared& first, U* second) {
		return first.pointer_ != second;
	}

	template <class U>
	friend bool operator!= (U* first, const TShared& second) {
		return first != second.pointer_;
	}

	bool operator< (const TShared& other) const {
		return pointer_ < other.pointer_;
	}

	bool operator<= (const TShared& other) const {
		return pointer_ <= other.pointer_;
	}

	bool operator> (const TShared& other) const {
		return pointer_ > other.pointer_;
	}

	bool operator>= (const TShared& other) const {
		return pointer_ >= other.pointer_;
	}

	T* Get() {
		Validator::IsNotNull(pointer_);
		T* temp = pointer_;

		if(CounterPolicy::Decrement(counter_) == 0) {
			delete counter_;
			// It' the responsibility of the caller to free the object now.
		}
		
		pointer_ = nullptr;
		counter_ = nullptr;
		return temp; 
	}

	template<class U>
	shared<U, IsArray, CounterPolicy, Validator> As() const {
		U* pointer = dynamic_cast<U*>(pointer_);
		TCounter* counter = counter_;

		if(counter_) {
			CounterPolicy::Increment(counter);
		}

		return shared<U, IsArray, CounterPolicy, Validator>(pointer, counter);
	}

	template<class U>
	shared<U, IsArray, CounterPolicy, Validator> AsStatic() const {
		U* pointer = static_cast<U*>(pointer_);
		TCounter* counter = counter_;

		if(counter_) {
			CounterPolicy::Increment(counter);
		}

		return shared<U, IsArray, CounterPolicy, Validator>(pointer, counter);
	}

	T* Raw() const {
		return pointer_;
	}

	TCounter* Counter() const {
		return counter_;
	}

	void Reset() {
		TryRelease(); // Release the current object.
	}

	template <class U>
	void Copy(TCounter** counter, U** pointer) const {
		*counter = counter_;
		*pointer = static_cast<U*>(pointer_);
	}
};


// Specialization for array types.
template <class T, class CounterPolicy = STPolicy, class Validator = DebugValidator>
class sharedVect : public shared<T, true, CounterPolicy, Validator> {
public:
	typedef typename shared<T, true, CounterPolicy, Validator> TShared;

	sharedVect() : shared() {}

	sharedVect(T* p) : shared(p) {}

	template <class U>
	sharedVect(U* p) : shared(p) {}
	
	sharedVect(const TShared& other) : shared(other) {}
	
	sharedVect(local<T, true, Validator>& other) : shared(other) {}

	template <class U>
	sharedVect(local<U, true, Validator>& other) : shared(other) {}

	template <class U>
	sharedVect(const shared<U, true, CounterPolicy, Validator>& other) : shared(other) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	T& operator* () const {
		return shared::operator* ();
	}

	T* operator-> () const {
		return shared::operator-> ();
	}

	bool operator! () const {
		return  shared::operator! ();
	}

	operator T* () const {
		return shared::operator T* ();
	}

	TShared& operator= (const TShared& other) {
		shared::operator= (other);
		return *this;
	}

	TShared& operator= (T* other) {
		shared::operator= (other);
		return *this;
	}

	TShared& operator= (local<T, true, Validator>&other) {
		shared::operator= (other.Get());
		return *this;
	}

	template <class U>
	TShared& operator= (const shared<U, true, CounterPolicy, Validator>& other) {
		shared::operator=<U> (other);
		return *this;
	}

	bool operator< (const TShared& other) const {
		return shared::operator< (other);
	}

	bool operator<= (const TShared& other) const {
		return shared::operator<= (other);
	}

	bool operator> (const TShared& other) const {
		return shared::operator> (other);
	}

	bool operator>= (const TShared& other) const {
		return shared::operator>= (other);
	}
};

} // namespace Base
#endif