// IOConst.cpp
// Copyright (c) Lup Gratian
//
// Constants used by IO.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IOConst.hpp"
#include "../../Base/String.hpp"
using namespace Base;

namespace Abstraction {

const string IOConst::PATH_SEPARATOR = string(_T("\\"));

const int IOConst::INVALID_PATH_CHAR_NUMBER = 36;

const string::TChar IOConst::INVALID_PATH_CHARS[] = {
	1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12,
	13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 26, 27, 28, 29, 30, 31, _T('\"'), _T('<'), _T('>'), _T('|'), _T('\0')
};

const int IOConst::INVALID_FILE_NAME_CHAR_NUMBER = 41;

const string::TChar IOConst::INVALID_FILE_NAME_CHARS[] = {
	1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12,
	13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
	25, 26, 27, 28, 29, 30, 31, _T('\"'), _T('<'), _T('>'), _T('|'), 
	_T('\0'), _T(':'), _T('*'), _T('?'), _T('\\'), _T('/')
};

}