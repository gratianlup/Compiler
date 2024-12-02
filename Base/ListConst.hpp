// ListConst.hpp
// Copyright (c) Lup Gratian
//
// Constants for List.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_LIST_CONST_HPP
#define PC_BASE_LIST_CONST_HPP

namespace Base {

struct ListConst {
	static const int DEFAULT_CAPACITY    = 4;
	static const int SELECTION_SORT_SIZE = 32;
	static const double GROW_FACTOR;
	static const double TRIM_THRESHOLD;
};

} // namespace Base
#endif