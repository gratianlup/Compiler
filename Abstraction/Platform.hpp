// Platform.hpp
// Copyright (c) Lup Gratian
//
// Provides hardware and OS-dependent features.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifdef PLATFORM_LINUX
	// ...
#else
	#include "Windows\Interlocked.hpp"
	#include "Windows\IO.hpp"
	#include "Windows\SSE.hpp"
	#include "Windows\Float.hpp"
	#include "Windows\Integer.hpp"
	#include "Windows\Console.hpp"
	#include "Windows\ProcessHelper.hpp"
#endif