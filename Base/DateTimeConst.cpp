// DateTimeConst.cpp
// Copyright (c) Lup Gratian
//
// Constants for DateTime.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DateTimeConst.hpp"

namespace Base {

const int DateTimeConst::DAYS_BEFORE_MONTH[] = {
	0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
};

const int DateTimeConst::DAYS_BEFORE_MONTH_LY[] = { // Leap year.
	0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 
};

} // namespace Base