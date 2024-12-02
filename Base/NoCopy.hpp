// NoCopy.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_NO_COPY_HPP
#define PC_BASE_NO_COPY_HPP

namespace Base {

struct NoCopy {
protected:
	NoCopy() {}
	~NoCopy() {}

private:
	// Don't allow copying.
	NoCopy(const NoCopy& other);
	NoCopy& operator =(const NoCopy& other);
};

}
#endif