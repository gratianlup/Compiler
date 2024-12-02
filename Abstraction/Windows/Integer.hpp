// Integer.hpp
// Copyright (c) Lup Gratian
//
// Implements some target-independent integer operations.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_INTEGER_HPP
#define PC_ABSTRACTION_INTEGER_HPP

#include <intrin.h>

namespace Abstraction {

class Integer {
public:
	// Returns the index of the leftmost bit set to 1.
	// Returns -1 if not bit is set.
	static int LeftmostSetBit(__int64 value) {
        #if defined(_MSC_VER) && defined(_M_X64)
		    unsigned long index;

		    if(_BitScanReverse64(&index, value)) {
			    return (int)index;
		    }
		    else return -1;
        #else
            // Compute the index in a naive way.
            int index = 63;

            while(value) {
                if(value & 0x8000000000000000LL) {
                    return index;
                }

                index--;
                value <<= 1;
            }

            return -1; // No bit is set.
        #endif
	}

	// Returns the index of the rightmost bit set to 1.
	// Returns -1 if no bit is set.
	static int RightmostSetBit(__int64 value) {
        #if defined(_MSC_VER) && defined(_M_X64)
		    unsigned long index;

		    if(_BitScanForward64(&index, value)) {
			    return (int)index;
		    }
		    else return -1;
        #else
            // Compute the index in a naive way.
            int index = 0;

            while(value) {
                if(value & 1) {
                    return index;
                }

                index++;
                value >>= 1;
            }

            return -1; // No bit is set.
        #endif
	}
};

} // namespace Abstraction
#endif