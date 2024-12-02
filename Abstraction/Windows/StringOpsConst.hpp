// StringOpsConst.hpp
// Copyright (c) Lup Gratian
//
// Constants used by StringOps.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_STRING_OPS_CONST_HPP
#define PC_ABSTRACTION_STRING_OPS_CONST_HPP

namespace Abstraction {

#ifdef _UNICODE
struct StringOpsConst {
	static const int WHITESPACE_CHARS_NUMBER;
	static const wchar_t WHITESPACE_CHARS[];
};
#else
struct StringOpsConst {
	static const int WHITESPACE_CHARS_NUMBER;
	static const char WHITESPACE_CHARS[];
};
#endif

} // namespace Abstraction
#endif