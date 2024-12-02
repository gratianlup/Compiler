// Info.hpp
// Copyright (c) Lup Gratian
//
// Defines the information constants used by the compiler.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_INFO_HPP
#define PC_COMMON_INFO_HPP

#include "ConstantGen.hpp"

namespace Common {

namespace Info {
	#define info(name) const int name = GenInfo<__LINE__>::Value
	#include "Info.def"
	#undef info
}

} // namespace Common
#endif