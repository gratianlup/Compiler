// Log.hpp
// Copyright (c) Lup Gratian
//
// Defines constants for Log.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Log.hpp"

namespace Base {

// Static members.
List<LogTarget*> Log::targets_ = List<LogTarget*>();
int Log::indent_ = 0;

}