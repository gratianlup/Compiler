// PlatformTime.hpp
// Copyright (c) Lup Gratian
//
// Provides time-related features. 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_PLATFORM_TIME_HPP
#define PC_ABSTRACTION_PLATFORM_TIME_HPP

#ifdef PLATFORM_LINUX

#else
	#include "Windows\Time.hpp"
#endif

#endif