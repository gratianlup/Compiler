// Warnings.hpp
// Copyright (c) Lup Gratian
//
// Defines the warning constants used by the compiler.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_WARNINGS_HPP
#define PC_COMMON_WARNINGS_HPP

#include "ConstantGen.hpp"

namespace Common {

namespace Warning {
	#define warning(name) const int name = GenWarning<__LINE__>::Value
	#include "Warnings.def"
	#undef warning
}

} // namespace Common
#endif