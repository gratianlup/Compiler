// MakePair.hpp
// Copyright (c) Lup Gratian
//
// Defines a macro that can be used to create an object that
// acts like a pair of two values (equivalent to std::pair,
// but can have custom names for the two variables).
// Usage: MAKE_PAIR(PairName, int, FirstValue, float, SecondValue)
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_MAKE_PAIR_HPP
#define PC_BASE_MAKE_PAIR_HPP

#define MAKE_PAIR(name, type1, parameter1, type2, parameter2) \
    struct name { \
        type1 parameter1; \
        type2 parameter2; \
        \
        name() {} \
        name(type1 parameter1##_, type2 parameter2##_) : parameter1(parameter1##_), parameter2(parameter2##_) {} \
        name(const name& other) : parameter1(other.parameter1), parameter2(other.parameter2) {} \
        \
        bool operator== (const name& other) const { \
            return (parameter1 == other.parameter1) && \
                   (parameter2 == other.parameter2); \
        } \
        bool operator!= (const name& other) const { \
            return operator== (other) == false; \
        } \
    }


// Defines a pair that can be ordered by the first member in ascending order.
#define MAKE_PAIR_ORDERED_FIRST(name, type1, parameter1, type2, parameter2) \
    struct name { \
        type1 parameter1; \
        type2 parameter2; \
        \
        name() {} \
        name(type1 parameter1##_, type2 parameter2##_) : parameter1(parameter1##_), parameter2(parameter2##_) {} \
        name(const name& other) : parameter1(other.parameter1), parameter2(other.parameter2) {} \
        \
        bool operator== (const name& other) const { \
            return (parameter1 == other.parameter1) && \
                   (parameter2 == other.parameter2); \
        } \
        bool operator!= (const name& other) const { \
            return operator== (other) == false; \
        } \
        bool operator< (const name& other) const { \
            return parameter1 < other.parameter1; \
        } \
        bool operator<= (const name& other) const { \
            return parameter1 <= other.parameter1; \
        } \
        bool operator> (const name& other) const { \
            return parameter1 > other.parameter1; \
        } \
        bool operator>= (const name& other) const { \
            return parameter1 >= other.parameter1; \
        } \
    }


// Defines a pair that can be ordered by the second member in ascending order.
#define MAKE_PAIR_ORDERED_SECOND(name, type1, parameter1, type2, parameter2) \
    struct name { \
        type1 parameter1; \
        type2 parameter2; \
        \
        name() {} \
        name(type1 parameter1##_, type2 parameter2##_) : parameter1(parameter1##_), parameter2(parameter2##_) {} \
        name(const name& other) : parameter1(other.parameter1), parameter2(other.parameter2) {} \
        \
        bool operator== (const name& other) const { \
            return (parameter1 == other.parameter1) && \
                   (parameter2 == other.parameter2); \
        } \
        bool operator!= (const name& other) const { \
            return operator== (other) == false; \
        } \
        bool operator< (const name& other) const { \
            return parameter2 < other.parameter2; \
        } \
        bool operator<= (const name& other) const { \
            return parameter2 <= other.parameter2; \
        } \
        bool operator> (const name& other) const { \
            return parameter2 > other.parameter2; \
        } \
        bool operator>= (const name& other) const { \
            return parameter2 >= other.parameter2; \
        } \
    }


// Defines a pair that can be ordered by the first member in descending order.
#define MAKE_PAIR_ORDERED_FIRST_DESC(name, type1, parameter1, type2, parameter2) \
    struct name { \
        type1 parameter1; \
        type2 parameter2; \
        \
        name() {} \
        name(type1 parameter1##_, type2 parameter2##_) : parameter1(parameter1##_), parameter2(parameter2##_) {} \
        name(const name& other) : parameter1(other.parameter1), parameter2(other.parameter2) {} \
        \
        bool operator== (const name& other) const { \
            return (parameter1 == other.parameter1) && \
                   (parameter2 == other.parameter2); \
        } \
        bool operator!= (const name& other) const { \
            return operator== (other) == false; \
        } \
        bool operator< (const name& other) const { \
            return parameter1 > other.parameter1; \
        } \
        bool operator<= (const name& other) const { \
            return parameter1 >= other.parameter1; \
        } \
        bool operator> (const name& other) const { \
            return parameter1 < other.parameter1; \
        } \
        bool operator>= (const name& other) const { \
            return parameter1 <= other.parameter1; \
        } \
    }


// Defines a pair that can be ordered by the second member in descending order.
#define MAKE_PAIR_ORDERED_SECOND_DESC(name, type1, parameter1, type2, parameter2) \
    struct name { \
        type1 parameter1; \
        type2 parameter2; \
        \
        name() {} \
        name(type1 parameter1##_, type2 parameter2##_) : parameter1(parameter1##_), parameter2(parameter2##_) {} \
        name(const name& other) : parameter1(other.parameter1), parameter2(other.parameter2) {} \
        \
        bool operator== (const name& other) const { \
            return (parameter1 == other.parameter1) && \
                   (parameter2 == other.parameter2); \
        } \
        bool operator!= (const name& other) const { \
            return operator== (other) == false; \
        } \
        bool operator< (const name& other) const { \
            return parameter2 > other.parameter2; \
        } \
        bool operator<= (const name& other) const { \
            return parameter2 >= other.parameter2; \
        } \
        bool operator> (const name& other) const { \
            return parameter2 < other.parameter2; \
        } \
        bool operator>= (const name& other) const { \
            return parameter2 <= other.parameter2; \
        } \
    }

#endif