// DefaultComparer.hpp
// Copyright (c) Lup Gratian
//
// The default comparer and hash code provider.
// If the object doesn't provide its own hash code implementation (by having the method
// 'unsigned GetHashCode() const' as a member), a default implementation is used.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_DEFAULT_COMPARER_HPP
#define PC_BASE_DEFAULT_COMPARER_HPP

#include "TypeInfo.hpp"

namespace Base {

// Calculates the hash code for the given data using the "One-at-a-time" function.
struct HashCalculator {
	static unsigned GetHashCode(const void* address, size_t length) {
		const unsigned char* addrPtr = (unsigned char*)address;
		unsigned hash = 0;
		size_t i;

		for(i = 0; i < length; i++) {
			hash += addrPtr[i];
			hash += (hash << 10);
			hash ^= (hash >> 6);
		}

		hash += (hash << 3);
		hash ^= (hash >> 11);
		hash += (hash << 15);
		return hash;
	}
};


namespace Detail {
	// Checks if the type has a method with exactly the following signature:
	// unsigned GetHashCode() const
	template <class U>
	struct HashDetector {
		typedef char One;
		typedef struct { char a[2]; } Two;

		template <class V, unsigned (V::*)() const>
		struct Helper {};

		template <class V>
		static One Test(Helper<V, &V::GetHashCode>*);

		template <class V>
		static Two Test(...);

		enum { IsPresent = (sizeof(Test<U>(0)) == sizeof(One)) };
	};


	// Computes a hash value base on the address of a pointer.
	template <class T>
	struct PointerHashCalculator {
		static unsigned GetHashCode(T ptr) {
			unsigned hash = (unsigned)ptr;

			if(sizeof(T) > 4) {
				// For 64 bit pointers.
				hash ^= (unsigned)(((size_t)ptr) >> 32);
			}

			return hash;
		}
	};


	// For user types that don't have an implementation.
	template <class T, bool BasicType, bool HashImplemented, bool IsPointer, bool UseValue>
	struct HashProvider {
		static unsigned GetHashCode(const T& a) {
			return HashCalculator::GetHashCode((unsigned char*)const_cast<T*>(&a),
											   sizeof(TypeInfo<T>::BaseType));
		}
	};


	// For pointer user types that don't have an implementation,
	// when it's allowed to use the pointed value.
	template <class T>
	struct HashProvider<T, false, false, true, true> { 
		static unsigned GetHashCode(const T& a) {
			return HashProvider<Compound<T>::BaseType, false, false, false, true>::GetHashCode(*a);
		}
	};

	
	// For pointer user types that don't have an implementation,
	// when it's not allowed to use the pointed value.
	template <class T>
	struct HashProvider<T, false, false, true, false> { 
		static unsigned GetHashCode(const T& a) {
			return PointerHashCalculator<T>::GetHashCode(a);
		}
	};


	// For user types that provide their own implementation.
	template <class T, bool UseValue>
	struct HashProvider<T, false, true, false, UseValue> {
		static unsigned GetHashCode(const T& a) {
			return a.GetHashCode();
		}
	};


	// For pointer user types that provide their own implementation,
	// when it's allowed to use the pointed value.
	template <class T>
	struct HashProvider<T, false, true, true, true> {
		static unsigned GetHashCode(const T& a) {
			return a->GetHashCode();
		}
	};


	// For pointer user types that provide their own implementation,
	// when it's not allowed to use the pointed value (treated like a simple pointer).
	template <class T>
	struct HashProvider<T, false, true, true, false> {
		static unsigned GetHashCode(const T& a) {
			return PointerHashCalculator<T>::GetHashCode(a);
		}
	};


	// For built-in types smaller or equal in size to an 'int' (int, char, etc.).
	template <class T, bool UseValue>
	struct HashProvider<T, true, false, false, UseValue> {
		static unsigned GetHashCode(const T& a) {
			return (unsigned)a;
		}
	};


	template <bool UseValue>
	struct HashProvider<double, true, false, false, UseValue> {
		static unsigned GetHashCode(const double& a) {
			return (unsigned)a ^ (unsigned)(((__int64)a) >> 32);
		}
	};


	template <bool UseValue>
	struct HashProvider<__int64, true, false, false, UseValue> {
		static unsigned GetHashCode(const __int64& a) {
			return (unsigned)a ^ (unsigned)(a >> 32);
		}
	};


	// For built-in pointer types (int*), when it's allowed to use the pointed value.
	template <class T>
	struct HashProvider<T, true, false, true, true> {
		static unsigned GetHashCode(const T& a) {
			return HashProvider<Compound<T>::BaseType, true, false, false, true>::GetHashCode(*a);
		}
	};


	// For built-in pointer types (int*), when it's not allowed to use the pointed value.
	template <class T>
	struct HashProvider<T, true, false, true, false> {
		static unsigned GetHashCode(const T& a) {
			return PointerHashCalculator<T>::GetHashCode(a);
		}
	};


	// Compares types.
	template <class T, bool IsPointer, bool UseValue>
	struct Comparer {
		static int Compare(const T& a, const T& b) {
			if(a == b) return 0;
			else if(a < b) return -1;
			else return 1;
		}
	};


	// Compares the pointed value.
	template <class T>
	struct Comparer<T, true, true> {
		static int Compare(const T& a, const T& b) {
			if(*a == *b) return 0;
			else if(*a < *b) return -1;
			else return 1;
		}
	};
}

template <class T, bool UseValue = false>
class DefaultComparer {
public:
	// Compares the given objects using the inequality operators.
	static int Compare(const T& a, const T& b) {
		return Detail::Comparer<T, TypeInfo<T>::IsPointer, UseValue>::Compare(a, b);
	}

	// Gets the hash code of the specified object.
	static unsigned GetHashCode(const T& object) {
		return Detail::HashProvider<T, TypeInfo<TypeInfo<T>::BaseType>::IsBasic, 
									Detail::HashDetector<TypeInfo<T>::BaseType>::IsPresent,
									TypeInfo<T>::IsPointer, UseValue>::GetHashCode(object);
	}
};

} // namespace Base
#endif 