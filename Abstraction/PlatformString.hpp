// PlatformString.hpp
// Copyright (c) Lup Gratian
//
// Provides string-related features.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_PLATFORM_STRING_HPP
#define PC_ABSTRACTION_PLATFORM_STRING_HPP

#ifdef PLATFORM_LINUX

#else
	#include "Windows\StringOps.hpp"
#endif

#endif