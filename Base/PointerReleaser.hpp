// PointerReleaser.hpp
// Copyright (c) Lup Gratian
//
// Provides policies for releasing objects owned by smart pointers.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_POINTER_RELEASER_H
#define PC_BASE_POINTER_RELEASER_H

namespace Base {

template <class U, bool IsArray>
struct PointerReleaser {
	static void Release(U* pointer) {
		delete pointer;
	}
};


// Array version of the releaser.
template <class U>
struct PointerReleaser<U, true> {
	static void Release(U* pointer) {
		delete[] pointer;
	}
};

} // namespace Base
#endif