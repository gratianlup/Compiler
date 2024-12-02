// IOConst.cpp
// Copyright (c) Lup Gratian
//
// Constants used by StringOps.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StringOpsConst.hpp"

namespace Abstraction {

// Definitions of the white space characters.
#ifdef _UNICODE
	const int StringOpsConst::WHITESPACE_CHARS_NUMBER = 23;

	const wchar_t StringOpsConst::WHITESPACE_CHARS[23] = {
		0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x20, 0x85, 0xA0,
		0x1680, 0x2000, 0x2001, 0x2002,
		0x2003, 0x2004, 0x2007, 0x2008,
		0x2009, 0x200A, 0x200B, 0x2028,
		0x2029, 0x3000, 0xFEFF
	};
#else
	int StringOpsConst::WHITESPACE_CHARS_NUMBER = 8;

	template <class T>
	char StringOpsConst::WHITESPACE_CHARS[8] = {
		0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x20, 0x85, 0xA0
	};
#endif // _UNICODE

} // namespace Abstraction