// AliasAnalyzer.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class from which all alias analysis implementations must derive.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_ALIAS_ANALYZER_HPP
#define PC_ANALYSIS_ALIAS_ANALYZER_HPP

#include "AllocHelper.hpp"
#include "LanguageInfo.hpp"
#include "TypeInfo.hpp"
#include "../IR/Operand.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/StackIntrinsics.hpp"
#include "../Compilation Pass/AnalysisPass.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/List.hpp"
using namespace IR;
using namespace Base;
using namespace CompilationPass;

namespace Analysis {

// Represents the result of an alias query.
// Must - the pointers definitely point to the same memory location
// May  - we're not sure if the pointers point to the same memory location or not
// None - the pointers definitely don't point to the same memory location (no alias)
enum class AliasResult {
    Must,
    May,
    None
};


// Used to cache the address-taken result for a location
// (no need to walk to the base variable anymore).
enum class AddressTakenType {
    Unknown,
    Yes,
    No,
};


// Represents an abstract location used by the alias analysis component
// to represent the locations in the application.
class AliasLocation {
private:
    static const __int64 UNKNOWN_SIZE = -1;

	Operand* base_;  // The base operand.
    __int64 offset_; // The offset (in bytes) relative to the base operand.
	__int64 size_;   // The size of the accessed region.
    AddressTakenType addrTaken_;

public:
	AliasLocation() : base_(nullptr), offset_(0), size_(0), 
                      addrTaken_(AddressTakenType::Unknown) {}

	AliasLocation(Operand* base, __int64 offset, 
                  __int64 size = UNKNOWN_SIZE) : 
            base_(base), offset_(offset), size_(size),
            addrTaken_(AddressTakenType::Unknown) {}

	AliasLocation(const AliasLocation& other) :
			base_(other.base_), size_(other.size_), 
            offset_(other.offset_), addrTaken_(other.addrTaken_) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    static __int64 GetUnknown() {
        return UNKNOWN_SIZE;
    }

	// Returns the operands that acts as the base for this location.
	Operand* Base() {
		return base_;
	}

	const Operand* Base() const {
		return base_;
	}

	void SetBase(Operand* value) {
		base_ = value;
	}

    // Returns the offset (in bytes) of the location
    // relative to the base operand.
    __int64 Offset() const {
        return offset_;
    }

    void SetOffset(__int64 value) {
        offset_ = value;
    }

    // Returns 'true' if the offset is zero.
    bool HasNoOffset() const {
        return offset_ == 0;
    }

    // Returns 'true' if the offset is different from zero.
    bool HasOffset() const {
        return offset_ != 0;
    }

	// Returns the size of the location.
	__int64 Size() const {
		return size_;
	}

	void SetSize(__int64 value) {
		size_ = value;
	}

    // Returns 'true' if the location size is unknown.
    bool HasUnknownSize() const {
        return size_ == UNKNOWN_SIZE;
    }

    // Returns 'true' if the location size is known.
    bool HasKnownSize() const {
        return size_ != UNKNOWN_SIZE;
    }

    AddressTakenType AddressTaken() const {
        return addrTaken_;
    }

    void SetAddressTaken(AddressTakenType value) {
        addrTaken_ = value;
    }

    bool HasAddressTaken() {
        return addrTaken_ == AddressTakenType::Yes;
    }

    bool HasAddressNotTaken() {
        return addrTaken_ == AddressTakenType::No;
    }

    AliasLocation WithAdjustedOffset(__int64 adjustment) {
        return AliasLocation(Base(), Offset() + adjustment, Size());
    }

	unsigned GetHashCode() const {
		// Uses the FNV hash algorithm. 
		// Taken from 'http://www.eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx'
		unsigned hash = 2166136261;
		hash = (hash * 16777619) ^ (unsigned)base_;

		if(sizeof(Operand*) == 8) {
			// For 64 bit pointers;
			hash = (hash * 16777619) ^ (unsigned)((__int64)base_ >> 32);
		}

        hash = (hash * 16777619) ^ (unsigned)offset_;
        hash = (hash * 16777619) ^ (unsigned)(offset_ >> 32);

		hash = (hash * 16777619) ^ (unsigned)size_;
        hash = (hash * 16777619) ^ (unsigned)(size_ >> 32);
		return hash;
	}

	string ToString() const {
		StringBuilder sb;
		sb.AppendFormat(L"Alias location: { Base = %X, ", base_);
		sb.AppendFormat(L"Offset = %d, ", offset_);

		if(size_ == UNKNOWN_SIZE) {
			sb.Append("Size = Unknown ");
		}
		else sb.AppendFormat(L"Size = %d, ", size_);

		sb.Append("}");
		return sb.ToString();
	}

	void Dump() const {
		ObjectDumper(ToString()).Dump();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	AliasLocation& operator= (const AliasLocation& other) {
		if(&other == this) return *this;

		base_ = other.base_;
        offset_ = other.offset_;
		size_ = other.size_;
		return *this;
	}

	bool operator== (const AliasLocation& other) const {
		return (base_ == other.base_) && 
               (offset_ == other.offset_) &&
               (size_ == other.size_);
	}

	bool operator!= (const AliasLocation& other) const {
		return operator== (other) == false;
	}

	bool operator< (const AliasLocation& other) const {
		return false; // Required by Dictionary.
	}
};

// Represents the type of the memory accessed by a 'call' instruction.
// Note that the types can be combined.
enum class MemoryType {
    None           = 0, // Does not access memory at all
    Global         = 1, // Any global variable
    Indirect       = 2, // Indirect memory
    Parameters     = 4, // Any pointer function parameter
    Unknown = Global | Indirect | Parameters
};


// Represents the operation performed by a 'call' instruction
// on a memory location.
enum class MemoryOperation {
    Invalid = 0,
    Read    = 1, // "Ref" behavior
    Write   = 2, // "Mod" behavior
    ReadWrite = Read | Write // Both "Ref" and "Mod" behavior
};


// Represents the type of memory locations accessed by a called function,
// including the operation performed (read, write, or both).
class MemoryResult {
private:
    MemoryType type_;
    char operations_[4];

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    int GetOperationIndex(MemoryType type) const {
        switch(type) {
            case MemoryType::Global :    return 0;
            case MemoryType::Indirect:   return 1;
            case MemoryType::Parameters: return 2;
            default: return 3;
        }
    }

public:
    MemoryResult(MemoryType type = MemoryType::Unknown,
                 MemoryOperation operation = MemoryOperation::ReadWrite) {
        type_ = type;
        *((int*)operations_) = 0;

        if(((int)type & ((int)type - 1)) == 0) {
            // A single type of operation is set.
            SetOperation(type, operation);
        }
        else {
            // Multiple types are set, each gets the same behavior.
            for(int i = 0; i < 4; i++) {
                if((int)type & (1 << i)) {
                    SetOperation((MemoryType)(1 << i), operation);
                }
            }
        }
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Methods for obtaining various memory type/operation combinations.
    static MemoryResult GetNone() {
        return MemoryResult(MemoryType::None, MemoryOperation::Invalid);
    }

    static MemoryResult GetUnknown() {
        return MemoryResult(MemoryType::Unknown, MemoryOperation::ReadWrite);
    }

    static MemoryResult GetGlobalRead() {
        return MemoryResult(MemoryType::Global, MemoryOperation::Read);
    }

    static MemoryResult GetGlobalWrite() {
        return MemoryResult(MemoryType::Global, MemoryOperation::Write);
    }

    static MemoryResult GetGlobalReadWrite() {
        return MemoryResult(MemoryType::Global, MemoryOperation::ReadWrite);
    }

    static MemoryResult GetIndirectMemoryRead() {
        return MemoryResult(MemoryType::Indirect, MemoryOperation::Read);
    }

	static MemoryResult GetIndirectMemoryWrite() {
        return MemoryResult(MemoryType::Indirect, MemoryOperation::Write);
    }

	static MemoryResult GetIndirectMemoryReadWrite() {
        return MemoryResult(MemoryType::Indirect, MemoryOperation::ReadWrite);
    }

    static MemoryResult GetParametersRead() {
        return MemoryResult(MemoryType::Parameters, MemoryOperation::Read);
    }

    static MemoryResult GetParametersWrite() {
        return MemoryResult(MemoryType::Parameters, MemoryOperation::Write);
    }

    static MemoryResult GetParametersReadWrite() {
        return MemoryResult(MemoryType::Parameters, MemoryOperation::ReadWrite);
    }

	// Returns the operation performed on the specified memory type.
    MemoryOperation GetOperation(MemoryType type) const {
        int index = GetOperationIndex(type);
        return (MemoryOperation)operations_[index];
    }

	// Sets the operation performed on the specified memory type.
	void SetOperation(MemoryType type, MemoryOperation operation) {
        int index = GetOperationIndex(type);
        operations_[index] = (char)operation;
    }

    // Methods for testing the type of the accessed memory.
    bool AccessesMemory() const {
        return type_ != MemoryType::None;
    }

    bool DoesNotAccessMemory() const {
        return type_ == MemoryType::None;
    }

    bool AccessesUnknownMemory() const {
        return (int)type_ & (int)MemoryType::Unknown;
    }

    bool AccessesGlobalMemory() const {
        return (int)type_ & (int)MemoryType::Global;
    }

    bool AccessesIndirectMemory() const {
		return (int)type_ & (int)MemoryType::Indirect;
    }

    bool AccessesParametersMemory() const {
        return (int)type_ & (int)MemoryType::Parameters;
    }

    // Returns the memory access for the specified memory type.
    MemoryOperation Operation(MemoryType type) const {
        return GetOperation(type);
    }

    MemoryOperation OperationForGlobalMemory() const {
        return GetOperation(MemoryType::Global);
    }

    MemoryOperation OperationForIndirectMemory() const {
		return GetOperation(MemoryType::Indirect);
    }

    MemoryOperation OperationForParametersMemory() const {
        return GetOperation(MemoryType::Parameters);
    }

    MemoryOperation OperationForUnknownMemory() const {
        return GetOperation(MemoryType::Unknown);
    }

	bool ReadsGlobalMemory() const {
		return AccessesGlobalMemory() &&
			   ((int)OperationForGlobalMemory() & (int)MemoryOperation::Read);
	}

	bool WritesGlobalMemory() const {
		return AccessesGlobalMemory() &&
			   ((int)OperationForGlobalMemory() & (int)MemoryOperation::Write);
	}

	bool ReadsAndWritesGlobalMemory() const {
		return AccessesGlobalMemory() &&
			   ((int)OperationForGlobalMemory() & (int)MemoryOperation::ReadWrite) == 
			    (int)MemoryOperation::ReadWrite;
	}

	bool ReadsIndirectMemory() const {
		return AccessesIndirectMemory() &&
			   ((int)OperationForIndirectMemory() & (int)MemoryOperation::Read);
	}

	bool WritesIndirectMemory() const {
		return AccessesIndirectMemory() &&
			   ((int)OperationForIndirectMemory() & (int)MemoryOperation::Write);
	}

	bool ReadsAndWritesIndirectMemory() const {
		return AccessesIndirectMemory() &&
			   ((int)OperationForIndirectMemory() & (int)MemoryOperation::ReadWrite) == 
			    (int)MemoryOperation::ReadWrite;
	}

	bool ReadsParametersMemory() const {
		return AccessesParametersMemory() &&
			   ((int)OperationForParametersMemory() & (int)MemoryOperation::Read);
	}

	bool WritesParametersMemory() const {
		return AccessesParametersMemory() &&
			   ((int)OperationForParametersMemory() & (int)MemoryOperation::Write);
	}

	bool ReadsAndWritesParametersMemory() const {
		return AccessesParametersMemory() &&
			   ((int)OperationForParametersMemory() & (int)MemoryOperation::ReadWrite) == 
			    (int)MemoryOperation::ReadWrite;
	}

	bool ReadsUnknownMemory() const {
		return AccessesUnknownMemory() &&
			   ((int)OperationForUnknownMemory() & (int)MemoryOperation::Read);
	}

	bool WritesUnknownMemory() const {
		return AccessesUnknownMemory() &&
			   ((int)OperationForUnknownMemory() & (int)MemoryOperation::Write);
	}

	bool ReadsAndWritesUnknownMemory() const {
		return AccessesUnknownMemory() &&
			   ((int)OperationForUnknownMemory() & (int)MemoryOperation::ReadWrite) == 
			    (int)MemoryOperation::ReadWrite;
	}

    unsigned GetHashCode() const {
		// Uses the FNV hash algorithm. 
		// Taken from 'http://www.eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx'
		unsigned hash = 2166136261;
		hash = (hash * 16777619) ^ (unsigned)type_;
        hash = (hash * 16777619) ^ *((unsigned*)operations_);
		return hash;
	}

	string ToString() const {
		StringBuilder sb;
		sb.AppendLine("Memory result: {");

		if(AccessesUnknownMemory()) {
			sb.Append("    Unknown: ");
			if(ReadsUnknownMemory()) sb.Append("Read, ");
			if(WritesUnknownMemory()) sb.Append("Write");
			sb.AppendLine();
		}
		else {
			if(AccessesGlobalMemory()) {
				sb.Append("    Globals: ");
				if(ReadsGlobalMemory()) sb.Append("Read, ");
				if(WritesGlobalMemory()) sb.Append("Write");
				sb.AppendLine();
			}

			if(AccessesParametersMemory()) {
				sb.Append("    Parameters: ");
				if(ReadsParametersMemory()) sb.Append("Read, ");
				if(WritesParametersMemory()) sb.Append("Write");
				sb.AppendLine();
			}

			if(AccessesIndirectMemory()) {
				sb.Append("    Indirect: ");
				if(ReadsIndirectMemory()) sb.Append("Read, ");
				if(WritesIndirectMemory()) sb.Append("Write");
				sb.AppendLine();
			}
		}

		sb.Append("}");
		return sb.ToString();
	}

	void Dump() const {
		ObjectDumper(ToString()).Dump();
	}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	MemoryResult& operator= (const MemoryResult& other) {
		if(&other == this) return *this;

        type_ = other.type_;
        *((int*)operations_) = *((int*)other.operations_);
		return *this;
	}

	bool operator== (const MemoryResult& other) const {
        return (type_ == other.type_) &&
               (*((int*)operations_) == *((int*)other.operations_));
	}

	bool operator< (const MemoryResult& other) const {
		return false; // Required by Dictionary.
	}
};


// Forward declarations.
class AliasInfo;


// Base class for all alias analysis algorithms.
class AliasAnalyzer : public AnalysisPass {
private:
    typedef List<CallInstr*> AllocCallList;

    AliasAnalyzer(const AliasAnalyzer& other); // Should not be assigned.
    AliasAnalyzer& operator= (const AliasAnalyzer& other); // Should not be copied.

    AliasInfo* parent_; // The alias manager.
    bool enabled_;

protected:
    typedef Analysis::TypeInfo TI;

    // Returns 'true' if the ranges associated with the specified locations
    // intersect or are contained one inside the other.
    // A range is the interval [offset, offset + size).
    bool RangesOverlap(AliasLocation& locationA, 
                       AliasLocation& locationB);

    // Returns the operand that acts as the base
    // of a series of addressing instructions.
    Operand* GetBaseOperand(Operand* op);

    // Returns 'true' if the base of a series of addressing instructions
    // is a variable. The variable is assigned to 'variableRef' if specified.
    bool BaseIsVariable(Operand* op, VariableReference** variableRef = nullptr);

	// Returns 'true' if the base of a series of addressing instructions
	// is a local variable whose address doesn't escape (it is not stored in 
	// another data structure, but might be passed as a parameter).
	// The variable is assigned to 'variableRef' if specified.
	bool BaseIsNonEscapedLocalVariable(Operand* op, VariableReference** variableRef = nullptr);

	// Returns 'true' if the base of a series of addressing instructions
	// is definitely not a global variable and 'false' if it may be.
	bool IsNotGlobalVariable(Operand* op);

    // Returns 'true' if the operand originates from a variable
    // whose address is not taken. If specified, 'veriableRef'
    // will point to the non-address-taken variable.
    bool IsAddressNotTaken(Operand* op, VariableReference** variableRef = nullptr);

    bool IsAddressNotTaken(AliasLocation location, 
                           VariableReference** variableRef = nullptr);

    // Returns 'true' if the record is the equivalent of a C 'union'.
    bool IsUnion(const RecordType* recordType);

    // Returns 'true' if the operands is a pointer that did not
    // originate from local/global aggregate variable.
    bool IsNonAggregatePointer(Operand* op);

    bool IsNonAggregatePointer(AliasLocation& location) {
        return IsNonAggregatePointer(location.Base());
    }

    // Returns 'true' if we're certain that the two parameters
    // can't point to the same memory locations. One such case
    // is when both are marked 'restrict' by the programmer.
    bool AreParametersIndependent(Parameter* parameterA, Parameter* parameterB);

    // Returns the operand with all pointer casts removed.
    Operand* WithoutPointerCasts(Operand* op);

public:
    AliasAnalyzer() : parent_(nullptr), enabled_(true) {}

    AliasAnalyzer(AliasInfo* parent) : parent_(parent) {
        DebugValidator::IsNotNull(parent);
    }

    virtual ~AliasAnalyzer() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Analyzes the specified memory locations 
    // and determines their alias relation.
    virtual AliasResult ComputeAlias(AliasLocation locationA, 
                                     AliasLocation locationB) {
        return AliasResult::May;
    }

    // Returns 'true' if the specified address operands 
    // are in a 'Must' alias relation.
    virtual bool HasMustAlias(AliasLocation locationA, 
                              AliasLocation locationB) {
        return ComputeAlias(locationA, locationB) == AliasResult::Must;
    }

    // Returns 'true' if the specified address operands 
    // are in a 'May' alias relation.
    virtual bool HasMayAlias(AliasLocation locationA, 
                             AliasLocation locationB) {
        return ComputeAlias(locationA, locationB) == AliasResult::May;
    }

    // Returns 'true' if the specified address operands
    // are in a 'Must' or 'May' alias relation.
    virtual bool HasAlias(AliasLocation locationA, 
                          AliasLocation locationB) {
        return ComputeAlias(locationA, locationB) != AliasResult::None;
    }

    // Returns 'true' if the specified address operands
    // are not aliases of the same memory location.
    virtual bool HasNoAlias(AliasLocation locationA, 
                            AliasLocation locationB) {
        return ComputeAlias(locationA, locationB) == AliasResult::None;
    }

    virtual MemoryResult ComputeCallEffects(CallInstr* instr) {
        return MemoryResult::GetUnknown();
    }

	virtual AliasResult ComputeCallAlias(CallInstr* instr, 
										 AliasLocation location) {
		return AliasResult::May;
	}

	// TypeAliasTag - constant version
	//   - useful for WritesToOperand/WritesToParameter

	// Returns 'true' if the called function(s) may write to global variables.
	virtual bool WritesToGlobalVariables(CallInstr* instr);

	// Returns 'true' if the called function(s) may read from global variables.
	virtual bool ReadsFromGlobalVariables(CallInstr* instr);

	// Returns 'true' if the called function(s) may read AND write global variables.
	virtual bool ReadsAndWritesGlobalVariables(CallInstr* instr);

	// Returns 'true' if the called function(s) may read OR write global variables.
	virtual bool ReadsOrWritesGlobalVariables(CallInstr* instr);

	// Returns 'true' if the called function(s) may write the pointer parameteres.
	virtual bool WritesToParameters(CallInstr* instr);

	// Returns 'true' if the called function(s) may read from the pointer parameteres.
	virtual bool ReadsFromParameters(CallInstr* instr);

	// Returns 'true' if the called function(s) may write AND read the pointer parameteres.
	virtual bool ReadsAndWritesParameters(CallInstr* instr);

	// Returns 'true' if the called function(s) may write OR read the pointer parameteres.
	virtual bool ReadsOrWritesParameters(CallInstr* instr);

	// Returns 'true' if the called function(s) may write to indirect pointers.
	virtual bool WritesIndirectMemory(CallInstr* instr);

	// Returns 'true' if the called function(s) may read from indirect pointers.
	virtual bool ReadsIndirectMemory(CallInstr* instr);

	// Returns 'true' if the called function(s) reads AND writes indirect pointers.
	virtual bool ReadsAndWritesIndirectMemory(CallInstr* instr);

	// Returns 'true' if the called function(s) reads OR writes indirect pointers.
	virtual bool ReadsOrWritesIndirectMemory(CallInstr* instr);

	// Returns 'true' if the called function(s) definitely don't read or write
	// any memory location (globals, heap, parameters, indirect, etc.).
	virtual bool DoesNotAccessMemory(CallInstr* instr);

	// Returns 'true' if the called function(s) may read memory locations.
	virtual bool AccessesAnyMemory(CallInstr* instr) {
		return DoesNotAccessMemory(instr) == false;
	}
	
	// bool ReadsFromOperand(CallInstr*, Operand*) - include global

	// false if globa const / typealiastag - const
	// bool WritesToOperand(CallInstr*, Operand*) - include global

	// bool AlwaysWritesToOperand(CallInstr*, Operand*) - include global

	// MemoryOperation AccessesOperand(CallInstr*, Operand*) - include global, read/write

    // Returns 'true' if the alias algorithm handles 'call' instructions.
    virtual bool HandlesCalls() const {
        return false;
    }

    // Returns the associated AliasInfo parent.
    AliasInfo* Parent() {
        return parent_;
    }

    void SetParent(AliasInfo* parent) {
        DebugValidator::IsNotNull(parent);
        parent_ = parent;
    }

    bool IsEnabled() const {
        return enabled_;
    }

    void SetIsEnabled(bool value) {
        enabled_ = value;
    }

    // Returns the name of the alias algorithm.
    virtual string Name() = 0;
};

} // namespace Analysis
#endif