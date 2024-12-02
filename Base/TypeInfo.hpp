// TypeInfo.hpp
// Copyright (c) Lup Gratian
//
// Provides compile-time information about the specified type.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_TYPE_INFO_HPP
#define PC_BASE_TYPE_INFO_HPP
namespace Base {

namespace Detail {
	template <class U>
	struct BasicType {
		enum { Yes = 0, No = 1 };
	};

	template <>
	struct BasicType<bool> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<char> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<unsigned char> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<wchar_t> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<short> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<unsigned short> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<int> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<unsigned> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<__int64> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<unsigned __int64> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<float> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<double> {
		enum { Yes = 1, No = 0 };
	};

	template <>
	struct BasicType<long double> {
		enum { Yes = 1, No = 0 };
	};

	
	// This method exploits the fact that functions can't be passed
	// as array parameters (SFINAE).
	template <class U>
	struct FunctionTest {
		typedef char One;
		typedef struct { char a[2]; } Two;

		template <class V>
		static One Test(...);
		
		template <class V>
		static Two Test(V (*)[1]); // Arrays of functions are illegal.

		enum { Yes = (sizeof(Test<U>(0)) == sizeof(One)) };
	};

	template <class U>
	struct FunctionTest<U&> {
		enum { Yes = 0 };
	};

	template <>
	struct FunctionTest<void> {
		enum { Yes = 0 };
	};

	template <>
	struct FunctionTest<void const> {
		enum { Yes = 0 };
	};


	template <class U>
	struct Compound {
		enum { IsPointer = 0, IsReference = 0, IsArray = 0,
			   IsFunction = FunctionTest<U>::Yes, IsPtrToMember = 0 };
		typedef U BaseType;   // U  -> U
		typedef U BottomType; // U  -> U
	};

	template <class U>
	struct Compound<U*> { // Specialization for pointers.
		enum { IsPointer  = 1, IsReference   = 0, IsArray = 0,
			   IsFunction = 0, IsPtrToMember = 0 };
		typedef U BaseType;
		typedef typename Compound<U>::BottomType BottomType; 
		// ^ Recursively instantiates Compound<U> types until the base type has been found.
	};

	template <class U>
	struct Compound<U&> { // Specialization for references.
		enum { IsPointer  = 0, IsReference   = 1, IsArray = 0,
			   IsFunction = 0, IsPtrToMember = 0 };
		typedef U BaseType;
		typedef typename Compound<U>::BottomType BottomType;
	};

	template <class U>
	struct Compound<const U&> { // Specialization for const references.
		enum { IsPointer  = 0, IsReference   = 1, IsArray = 0,
			   IsFunction = 0, IsPtrToMember = 0 };
		typedef U BaseType;
		typedef typename Compound<U>::BottomType BottomType;
	};

	template <class U, size_t N> // Specialization for arrays.
	struct Compound<U[N]> {
		enum { IsPointer  = 0, IsReference   = 0, IsArray = 1,
			   IsFunction = 0, IsPtrToMember = 0 };
		typedef U BaseType;
		typedef typename Compound<U>::BottomType BottomType;
	};

	template <class U> // Specialization for empty arrays.
	struct Compound<U[]> {
		enum { IsPointer  = 0, IsReference   = 0, IsArray = 1,
			   IsFunction = 0, IsPtrToMember = 0 };
		typedef U BaseType;
		typedef typename Compound<U>::BottomType BottomType;
	};

	template <class U, class C> // Specialization for pointers to members.
	struct Compound<U C::*> {
		enum { IsPointer  = 0, IsReference   = 0, IsArray = 1,
			   IsFunction = 0, IsPtrToMember = 1 };
		typedef U BaseType;
		typedef typename Compound<U>::BottomType BottomType;
	};
}


template <class T>
struct TypeInfo {
	enum { 
		IsBasic       = Detail::BasicType<T>::Yes, 
		IsNotBasic    = Detail::BasicType<T>::No,
		IsPointer     = Detail::Compound<T>::IsPointer,
		IsReference   = Detail::Compound<T>::IsReference,
		IsArray       = Detail::Compound<T>::IsArray,
		IsFunction    = Detail::Compound<T>::IsFunction,
		IsPtrToMember = Detail::Compound<T>::IsPtrToMember 
	};

	typedef typename Detail::Compound<T>::BaseType BaseType;
	typedef typename Detail::Compound<T>::BottomType BottomType;
};

} // namespace Base
#endif