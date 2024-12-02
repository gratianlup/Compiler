// Errors.hpp
// Copyright (c) Lup Gratian
//
// Defines the errors constants used by the compiler.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_ERRORS_HPP
#define PC_COMMON_ERRORS_HPP

#include "ConstantGen.hpp"

namespace Common {

namespace Error {
	#define error(name) const int name = GenError<__LINE__>::Value
	#include "Errors.def"
	#undef error
}

} // namespace Common
#endif

