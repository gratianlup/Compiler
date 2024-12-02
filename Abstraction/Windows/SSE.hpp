// SSE.hpp
// Copyright (c) Lup Gratian
//
// Provides support for SSE2 multimedia extensions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_SSE_HPP
#define PC_ABSTRACTION_SSE_HPP

#include <emmintrin.h>
#include "../../Base/TypeInfo.hpp"
#include "../../Base/StaticList.hpp"

namespace Abstraction {

// Should be used to align data on a 16-byte boundary.
#define SSE_ALIGN __declspec(align(16))
static const int SSE_ALIGNMENT = 16;


class SSE {
private:
	template <class T, int Index>
	struct AlignmentValidator {
		enum { IsValid = ((Index * sizeof(T)) % SSE_ALIGNMENT) == 0 };
	};


	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct AddImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 16 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 16) {
				*((__m128i*)&c[Index + i]) = _mm_add_epi8(*((__m128i*)&a[Index + i]), 
														  *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct AddImpl<T, sizeof(short), Size, Index> { // 8 16-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 8 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 8) {
				*((__m128i*)&c[Index + i]) = _mm_add_epi16(*((__m128i*)&a[Index + i]), 
														   *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct AddImpl<T, sizeof(int), Size, Index> { // 4 32-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 4 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 4) {
				*((__m128i*)&c[Index + i]) = _mm_add_epi32(*((__m128i*)&a[Index + i]), 
														   *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct AddImpl<T, sizeof(__int64), Size, Index> { // 2 64-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 2 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 2) {
				*((__m128i*)&c[Index + i]) = _mm_add_epi64(*((__m128i*)&a[Index + i]), 
														   *((__m128i*)&b[Index + i]));
			}
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct MaxImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 16 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 16) {
				*((__m128i*)&c[Index + i]) = _mm_max_epu8(*((__m128i*)&a[Index + i]), 
														  *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct MaxImpl<T, sizeof(short), Size, Index> { // 8 16-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 8 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 8) {
				*((__m128i*)&c[Index + i]) = _mm_max_epi16(*((__m128i*)&a[Index + i]), 
														   *((__m128i*)&b[Index + i]));
			}
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct MinImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 16 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 16) {
				*((__m128i*)&c[Index + i]) = _mm_min_epu8(*((__m128i*)&a[Index + i]), 
														  *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct MinImpl<T, sizeof(short), Size, Index> { // 8 16-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 8 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 8) {
				*((__m128i*)&c[Index + i]) = _mm_min_epi16(*((__m128i*)&a[Index + i]), 
														   *((__m128i*)&b[Index + i]));
			}
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct SubImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 16 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 16) {
				*((__m128i*)&c[Index + i]) = _mm_sub_epi8(*((__m128i*)&a[Index + i]), 
														  *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct SubImpl<T, sizeof(short), Size, Index> { // 8 16-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 8 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 8) {
				*((__m128i*)&c[Index + i]) = _mm_sub_epi16(*((__m128i*)&a[Index + i]), 
														   *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct SubImpl<T, sizeof(int), Size, Index> { // 4 32-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 4 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 4) {
				*((__m128i*)&c[Index + i]) = _mm_sub_epi32(*((__m128i*)&a[Index + i]), 
														   *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct SubImpl<T, sizeof(__int64), Size, Index> { // 2 64-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 2 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 2) {
				*((__m128i*)&c[Index + i]) = _mm_sub_epi64(*((__m128i*)&a[Index + i]), 
												   *((__m128i*)&b[Index + i]));
			}
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct EqImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 16 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 16) {
				*((__m128i*)&c[Index + i]) = _mm_cmpeq_epi8(*((__m128i*)&a[Index + i]), 
															*((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct EqImpl<T, sizeof(short), Size, Index> { // 8 16-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 8 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 8) {
				*((__m128i*)&c[Index + i]) = _mm_cmpeq_epi16(*((__m128i*)&a[Index + i]), 
															 *((__m128i*)&b[Index + i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct EqImpl<T, sizeof(int), Size, Index> { // 4 32-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 4 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 4) {
				*((__m128i*)&c[Index + i]) = _mm_cmpeq_epi32(*((__m128i*)&a[Index + i]), 
															 *((__m128i*)&b[Index + i]));
			}
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct XorImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(T::SIZE % 16 == 0, "Size must be a multiple of 16.");
			
			int length = T::SIZE * TypeSize;
			char* aPtr = (char*)a.Array() + (TypeSize * Index);
			char* bPtr = (char*)b.Array() + (TypeSize * Index);
			char* cPtr = (char*)c.Array() + (TypeSize * Index);

			for(int i = 0; i < length; i += 16) {
				*((__m128i*)&cPtr[i]) = _mm_xor_si128(*((__m128i*)&aPtr[i]), 
										   	          *((__m128i*)&bPtr[i]));
			}
		}
	};

	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct AndImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(T::SIZE % 16 == 0, "Size must be a multiple of 16.");
			
			int length = T::SIZE * TypeSize;
			char* aPtr = (char*)a.Array() + (TypeSize * Index);
			char* bPtr = (char*)b.Array() + (TypeSize * Index);
			char* cPtr = (char*)c.Array() + (TypeSize * Index);

			for(int i = 0; i < length; i += 16) {
				*((__m128i*)&cPtr[i]) = _mm_and_si128(*((__m128i*)&aPtr[i]), 
										   	          *((__m128i*)&bPtr[i]));
			}
		}
	};

	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct NandImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(T::SIZE % 16 == 0, "Size must be a multiple of 16.");
			
			int length = T::SIZE * TypeSize;
			char* aPtr = (char*)a.Array() + (TypeSize * Index);
			char* bPtr = (char*)b.Array() + (TypeSize * Index);
			char* cPtr = (char*)c.Array() + (TypeSize * Index);

			for(int i = 0; i < length; i += 16) {
				*((__m128i*)&cPtr[i]) = _mm_andnot_si128(*((__m128i*)&aPtr[i]), 
										   	             *((__m128i*)&bPtr[i]));
			}
		}
	};

	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct NandImpl2 {
		static void Execute(T& a, __m128i mask, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(T::SIZE % 16 == 0, "Size must be a multiple of 16.");
			
			int length = T::SIZE * TypeSize;
			char* aPtr = (char*)a.Array() + (TypeSize * Index);
			char* cPtr = (char*)c.Array() + (TypeSize * Index);

			for(int i = 0; i < length; i += 16) {
				*((__m128i*)&cPtr[i]) = _mm_andnot_si128(*((__m128i*)&aPtr[i]), mask);
			}
		}
	};

	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct OrImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(T::SIZE % 16 == 0, "Size must be a multiple of 16.");
			
			int length = T::SIZE * TypeSize;
			char* aPtr = (char*)a.Array() + (TypeSize * Index);
			char* bPtr = (char*)b.Array() + (TypeSize * Index);
			char* cPtr = (char*)c.Array() + (TypeSize * Index);

			for(int i = 0; i < length; i += 16) {
				*((__m128i*)&cPtr[i]) = _mm_or_si128(*((__m128i*)&aPtr[i]), 
										   	         *((__m128i*)&bPtr[i]));
			}
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct GtImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 16 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 16) {
				*((__m128i*)&c[i]) = _mm_cmpgt_epi8(*((__m128i*)&a[i]), 
												    *((__m128i*)&b[i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct GtImpl<T, sizeof(short), Size, Index> { // 8 16-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 8 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 8) {
				*((__m128i*)&c[i]) = _mm_cmpgt_epi16(*((__m128i*)&a[i]), 
												     *((__m128i*)&b[i]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct GtImpl<T, sizeof(int), Size, Index> { // 4 32-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 4 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 4) {
				*((__m128i*)&c[i]) = _mm_cmpgt_epi32(*((__m128i*)&a[i]), 
												     *((__m128i*)&b[i]));
			}
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class T, int TypeSize, int Size, int Index> // 16 8-bit numbers.
	struct LtImpl {
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 16 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 16) {
				*((__m128i*)&c[i + Index]) = _mm_cmpgt_epi8(*((__m128i*)&a[i + Index]), 
															*((__m128i*)&b[i + Index]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct LtImpl<T, sizeof(short), Size, Index> { // 8 16-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 8 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 8) {
				*((__m128i*)&c[i + Index]) = _mm_cmplt_epi16(*((__m128i*)&a[i + Index]), 
															 *((__m128i*)&b[i + Index]));
			}
		}
	};

	template <class T, int Size, int Index>
	struct LtImpl<T, sizeof(int), Size, Index> { // 4 32-bit numbers.
		static void Execute(T& a, T& b, T& c) {
			static_assert(AlignmentValidator<T::Type, Index>::IsValid, "Invalid index.");
			static_assert(TypeInfo<T::Type>::IsBasic, "Not a basic type.");
			static_assert(Size % 4 == 0, "Size must be a multiple of 16.");
			
			for(int i = 0; i < Size; i += 4) {
				*((__m128i*)&c[i + Index]) = _mm_cmplt_epi32(*((__m128i*)&a[i + Index]), 
															 *((__m128i*)&b[i + Index]));
			}
		}
	};

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	template <class T, int Size, int Index>
	static void Add(T& a, T& b, T& c) {
		AddImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Add(T& a, T& b, T& c) {
		AddImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Add(T& a, T& b, T& c) {
		AddImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Max(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(short), "Max not available for this type");
		MaxImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Max(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(short), "Max not available for this type");
		MaxImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Max(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(short), "Max not available for this type");
		MaxImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Min(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(short), "Min not available for this type");
		MinImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Min(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(short), "Min not available for this type");
		MinImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Min(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(short), "Min not available for this type");
		MinImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Sub(T& a, T& b, T& c) {
		SubImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Sub(T& a, T& b, T& c) {
		SubImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Sub(T& a, T& b, T& c) {
		SubImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Eq(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Max not available for this type");
		EqImpl<T, sizeof(T::Type)>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Eq(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Max not available for this type");
		EqImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Eq(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Max not available for this type");
		EqImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Xor(T& a, T& b, T& c) {
		XorImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Xor(T& a, T& b, T& c) {
		XorImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Xor(T& a, T& b, T& c) {
		XorImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void And(T& a, T& b, T& c) {
		AndImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void And(T& a, T& b, T& c) {
		AndImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void And(T& a, T& b, T& c) {
		AndImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Nand(T& a, T& b, T& c) {
		NandImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Nand(T& a, T& b, T& c) {
		NandImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Nand(T& a, T& b, T& c) {
		NandImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Or(T& a, T& b, T& c) {
		OrImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Or(T& a, T& b, T& c) {
		OrImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Or(T& a, T& b, T& c) {
		OrImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Neq(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Neq not available for this type");
		
		__m128i mask;
		EqImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
		mask.m128i_u64[0] = mask.m128i_u64[1] = ~0;
		NandImpl2<T, sizeof(T::Type), Size, Index>::Execute(c, mask, c);
	}

	template <class T, int Size>
	static void Neq(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Neq not available for this type");
		
		__m128i mask;
		EqImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
		mask.m128i_u64[0] = mask.m128i_u64[1] = ~0;
		NandImpl2<T, sizeof(T::Type), Size, 0>::Execute(c, mask, c);
	}

	template <class T>
	static void Neq(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Neq not available for this type");
		
		__m128i mask;
		EqImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
		mask.m128i_u64[0] = mask.m128i_u64[1] = ~0;
		NandImpl2<T, sizeof(T::Type), T::SIZE, 0>::Execute(c, mask, c);
	}

	template <class T, int Size, int Index>
	static void Gt(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Gt not available for this type");
		GtImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Gt(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Gt not available for this type");
		GtImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Gt(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Gt not available for this type");
		GtImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Gte(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Gte not available for this type");
		
		T temp(T::SIZE);
		GtImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
		EqImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, temp);
		OrImpl<T, sizeof(T::Type), Size, Index>::Execute(c, temp, c);
	}

	template <class T, int Size>
	static void Gte(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Gte not available for this type");
		
		T temp(T::SIZE);
		GtImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
		EqImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, temp);
		OrImpl<T, sizeof(T::Type), Size, 0>::Execute(c, temp, c);
	}

	template <class T>
	static void Gte(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Gte not available for this type");
		
		T temp(T::SIZE);
		GtImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
		EqImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, temp);
		OrImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(c, temp, c);
	}

	template <class T, int Size, int Index>
	static void Lt(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Lt not available for this type");
		LtImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
	}

	template <class T, int Size>
	static void Lt(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Lt not available for this type");
		LtImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
	}

	template <class T>
	static void Lt(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Lt not available for this type");
		LtImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
	}

	template <class T, int Size, int Index>
	static void Lte(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Lte not available for this type");
		
		T temp(T::SIZE);
		LtImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, c);
		EqImpl<T, sizeof(T::Type), Size, Index>::Execute(a, b, temp);
		OrImpl<T, sizeof(T::Type), Size, Index>::Execute(c, temp, c);
	}

	template <class T, int Size>
	static void Lte(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Lte not available for this type");
		
		T temp(T::SIZE);
		LtImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, c);
		EqImpl<T, sizeof(T::Type), Size, 0>::Execute(a, b, temp);
		OrImpl<T, sizeof(T::Type), Size, 0>::Execute(c, temp, c);
	}

	template <class T>
	static void Lte(T& a, T& b, T& c) {
		static_assert(sizeof(T::Type) <= sizeof(int), "Lte not available for this type");
		
		T temp(T::SIZE);
		LtImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, c);
		EqImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(a, b, temp);
		OrImpl<T, sizeof(T::Type), T::SIZE, 0>::Execute(c, temp, c);
	}

	template <int Length>
	static void Fill(void* dest, void* pattern) {
		static_assert(Length % 16 == 0, "Length is not a multiple of 16.");
		char* destPtr = (char*)dest;

		for(int i = 0; i < Length; i += 16) {
			_mm_stream_si128((__m128i*)&destPtr[i], *((__m128i*)pattern));
		}
	}

	template <int Length>
	static void Copy(void* dest, void* source) {
		static_assert(Length % 16 == 0, "Length is not a multiple of 16.");
		
		char* destPtr = (char*)dest;
		char* sourcePtr = (char*)source;

		for(int i = 0; i < Length; i += 16) {
			__m128i data = _mm_load_si128((__m128i*)&sourcePtr[i]);
			_mm_stream_si128((__m128i*)&destPtr[i], data);
		}
	}
};

} // namespace Abstraction
#endif