// TargetInfo.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_TARGET_INFO_HPP
#define PC_TARGET_INFO_HPP

#include "../IR/IRTypes.hpp"
#include "../IR/Instruction.hpp"

namespace Target {

class CacheLevel {
	// cache size, cache line size
	// associativity
};

class TargetInfo {
public:
	virtual ~TargetInfo() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the integer type that should be used to store pointers.
	virtual IR::IRIntegerKind GetPointerType() const = 0;

	// Returns the size (in bytes) of the pointer type.
	virtual int GetPointerSize() const = 0;

	// Returns the size (in bits) of the pointer type.
	virtual int GetPointerSizeInBits() const = 0;

	// Returns the default alignment for the specified integer type.
	virtual int GetAlignment(IR::IRIntegerKind kind) const = 0;

	// Returns the default alignment for the specified floating type.
	virtual int GetAlignment(IR::IRFloatingKind kind) const = 0;

	// Returns the default alignment for the pointer type.
	virtual int GetPointerAlignment() const = 0;

    // Returns the number of registers available for storing variables
    // (i.e this should exclude the stack pointer, for example).
    virtual int AvailableRegisters() const = 0;

    // Returns the required alignment for variables the are used
    // by the multimedia instructions of the architecture.
    virtual int MultimediaAlignment() const = 0;

    // Returns an estimate for the latency of the instruction.
    // The minimum value should be 1.
    virtual int EstimatedInstructionLatency(IR::Instruction* instr) const = 0;

    // Returns an estimate for the size (in bytes) of the instruction.
    virtual int EstimatedInstructionSize(IR::Instruction* instr) const = 0;

	// info about type size, preferred alignment (including pointer)
	// cache size, cache line size, cache levels, etc

	//? pointers to other *Info objects (instructions, registers, etc.)
};

} // namespace Target

#endif