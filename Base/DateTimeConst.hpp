// DateTimeConst.hpp
// Copyright (c) Lup Gratian
//
// Constants used by DateTime.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_DATE_TIME_CONST_HPP
#define PC_DATE_TIME_CONST_HPP

namespace Base {

struct DateTimeConst {
	static const int DAYS_BEFORE_MONTH[];
	static const int DAYS_BEFORE_MONTH_LY[]; // For leap years.
};

} // namespace Base
#endif