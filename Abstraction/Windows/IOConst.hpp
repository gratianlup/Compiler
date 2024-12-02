// IOConst.hpp
// Copyright (c) Lup Gratian
//
// Constants used by IO.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_IO_CONST_HPP
#define PC_ABSTRACTION_IO_CONST_HPP

#include "../../Base/String.hpp"
using namespace Base;

namespace Abstraction {

struct IOConst {
	static const string PATH_SEPARATOR;
	static const string::TChar INVALID_PATH_CHARS[];
	static const int INVALID_PATH_CHAR_NUMBER;
	static const string::TChar INVALID_FILE_NAME_CHARS[];
	static const int INVALID_FILE_NAME_CHAR_NUMBER;
};

} // namespace Abstraction
#endif