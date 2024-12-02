// Interlocked.hpp
// Copyright (c) Lup Gratian
//
// Smart pointer that guarantees that the pointed object is always deleted.
// Ownership cannot be changed.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_LOCAL_POINTER_H
#define PC_BASE_LOCAL_POINTER_H

#include "DebugValidator.hpp"
#include "PointerReleaser.hpp"
#include "NoCopy.hpp"

namespace Base {

template <class T, bool IsArray = false, class Validator = DebugValidator>
class local : private NoCopy {
private:
	typedef typename local<T, IsArray, Validator> TLocal;
	friend class TLocal;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	T* pointer_;

public:
	local() : pointer_(nullptr) {}

	local(T* object) : pointer_(object) {}

	template <class U>
	local(U* object) : pointer_(static_cast<T*>(object)) {}

	~local() {
		PointerReleaser<T, IsArray>::Release(pointer_);
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

	operator void* () const  {
		return pointer_;
	}

	TLocal& operator= (TLocal& other) {
		if(pointer_ == other.pointer_) {
			return *this;
		}

		if(pointer_) {
			PointerReleaser<T, IsArray>::Release(pointer_);
		}

		pointer_ = other.Get();
		return *this;
	}

	TLocal& operator= (T* other) {
		if(pointer_ == other) {
			return *this;
		}

		if(pointer_) {
			PointerReleaser<T, IsArray>::Release(pointer_);
		}

		pointer_ = other;
		return *this;
	}

	template <class U>
	TLocal& operator= (local<U, IsArray, Validator>& other) {
		return operator= (static_cast<T*>(other.pointer_));
	}

	template <class U>
	TLocal& operator= (U* other) {
		return operator= (static_cast<T*>(other));
	}

	friend bool operator== (const TLocal& first, const TLocal& second) {
		return first.pointer_ == second.pointer_;
	}

	friend bool operator== (const TLocal& first, T* second) {
		return first.pointer_ == second;
	}

	friend bool operator== (T* first, const TLocal& second) {
		return first == second.pointer_;
	}

	template <class U>
	friend bool operator== (const TLocal& first, U* second) {
		return first.pointer_ == second;
	}

	template <class U>
	friend bool operator== (U* first, const TLocal& second) {
		return first_ == second.pointer_;
	}

	template <class U>
	bool operator== (const TLocal& other) {
		return pointer_ == other.pointer_;
	}

	friend bool operator!= (const TLocal& first, const TLocal& second) {
		return first.pointer_ == second.pointer_;
	}

	friend bool operator!= (const TLocal& first, T* second) {
		return first.pointer_ != second;
	}

	friend bool operator!= (T* first, const TLocal& second) {
		return first != second.pointer_;
	}

	template <class U>
	friend bool operator!= (const TLocal& first, const U* second) {
		return first.pointer_ != second;
	}

	template <class U>
	friend bool operator!= (U* first, const TLocal& second) {
		return first_ != second.pointer_;
	}

	bool operator< (const TLocal& other) const {
		return pointer_ < other.pointer_;
	}

	bool operator<= (const TLocal& other) const {
		return pointer_ <= other.pointer_;
	}

	bool operator> (const TLocal& other) const {
		return pointer_ > other.pointer_;
	}

	bool operator>= (const TLocal& other) const {
		return pointer_ >= other.pointer_;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	T* Get() {
		Validator::IsNotNull(pointer_);
		T* temp = pointer_;
		pointer_ = nullptr;
		return temp; 
	}

	T* Raw() const {
		return pointer_;
	}

	void Reset() {
		PointerReleaser<T, IsArray>::Release(pointer_);
		pointer_ = nullptr;
	}
};


template <class T, class Validator = DebugValidator>
class localVect : public local<T, true, Validator> {
public:
	typedef local<T, true, Validator> TLocal;
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	localVect() : local() {}

	localVect(T* object) : local(object) {}

	template <class U>
	localVect(U* object) : local(object) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	T& operator* () const {
		return local::operator* ();
	}

	T* operator-> () const {
		return local::operator-> ();
	}

	bool operator! () const {
		return local::operator! ();
	}

	operator T* () const {
		return local::operator T* ();
	}

	operator void* () const {
		return local::operator void* ();
	}
	
	TLocal& operator= (TLocal& other) {
		local::operator= (other);
		return *this;
	}

	TLocal& operator= (T* other) {
		local::operator= (other);
		return *this;
	}

	template <class U>
	TLocal& operator= (local<U, true, Validator>& other) {
		local::operator=<U> (other);
		return *this;
	}

	template <class U>
	TLocal& operator= (U* other) {
		local::operator=<U> (other);
		return *this;
	}

	bool operator< (const TLocal& other) const {
		return local::operator< (other);
	}

	bool operator<= (const TLocal& other) const {
		return local::operator<= (other);
	}

	bool operator> (const TLocal& other) const {
		return local::operator> (other);
	}

	bool operator>= (const TLocal& other) const {
		return local::operator>= (other);
	}
};

} // namespace Base
#endif