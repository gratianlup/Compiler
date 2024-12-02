// Path.hpp
// Copyright (c) Lup Gratian
//
// Defines constants for Path.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#include "Path.hpp"
#include "String.hpp"

namespace Base {

const int PathConst::RANDOM_CHAR_NUMBER = 32;

const string::TChar PathConst::RANDOM_CHARS[] = {
	_T('a'), _T('b'), _T('c'), _T('d'), _T('e'), _T('f'), _T('g'), _T('h'), 
	_T('i'), _T('j'), _T('k'), _T('l'), _T('m'), _T('n'), _T('o'), _T('p'),
	_T('q'), _T('r'), _T('s'), _T('t'), _T('u'), _T('v'), _T('w'), _T('x'), 
	_T('y'), _T('z'), _T('0'), _T('1'), _T('2'), _T('3'), _T('4'), _T('5')
};

} // namespace Base