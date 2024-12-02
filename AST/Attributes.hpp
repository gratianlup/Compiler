// Atributes.hpp
// Copyright (c) Lup Gratian
//
// Defines the attributes that can be attached to a declaration.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ATRIBUTES_HPP
#define PC_ATRIBUTES_HPP

#include "Attribute.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
using namespace Base;

namespace AST {

// Represents an attribute that forces the associated declaration to be
// aligned at the specified byte boundary.
class AlignmentAttribute : public Attribute {
private:
	int value_;

public:
	AlignmentAttribute(int value) : value_(value) {}

	~AlignmentAttribute() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the alignment value.
	int Value() const {
		return value_;
	}

	void SetValue(int value) {
		value_ = value;
	}
};


// Represents an attribute that forces the members of a struct/union 
// to be aligned to the specified byte boundary.
// Corresponds to the Microsoft extension '#pragma pack'.
class PackAttribute : public Attribute {
private:
	int value_;

public:
	PackAttribute(int value) : value_(value) {}

	~PackAttribute() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the struct/union packing value.
	int Value() const {
		return value_;
	}

	void SetValue(int value) {
		value_ = value;
	}
};


// Defines the supported calling conventions.
// See the VC++ documentation for details.
enum class CallConvention {
	Cdecl,
	Stdcall,
	Fastcall
};


// Represents an attribute that specifies the calling convention to be used
// when calling the associated function.
class CallConventionAttribute : public Attribute {
private:
	CallConvention convention_;

public:
	CallConventionAttribute(CallConvention convention) : 
			convention_(convention) {}

	~CallConventionAttribute() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the calling convention type.
	CallConvention Convention() const {
		return convention_;
	}

	void SetConvention(CallConvention value) {
		convention_ = value;
	}

	// Returns 'true' if this is the 'cdecl' calling convention.
	bool IsCdecl() const {
		return convention_ == CallConvention::Cdecl;
	}

	// Returns 'true' if this is the 'sdtcall' calling convention.
	bool IsStdcall() const {
		return convention_ == CallConvention::Stdcall;
	}

	// Returns 'true' if this is the 'fastcall' calling convention.
	bool IsFastcall() const {
		return convention_ == CallConvention::Fastcall;
	}
};


// Represents the method that should be used when inlining the function.
enum class InlineType {
	Auto,
	Always,
	Never
};


// Represents an attribute that specifies the kind of inline expansion
// to be applied on a function.
class InlineAttribute : public Attribute {
private:
	InlineType type_;

public:
	InlineAttribute(InlineType type) : type_(type) {}

	~InlineAttribute() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	InlineType Type() const {
		return type_;
	}

	void SetType(InlineType value) {
		type_ = value;
	}

	bool IsAuto() const {
		return type_ == InlineType::Auto;
	}

	bool IsAlways() const {
		return type_ == InlineType::Always;
	}

	bool IsNever() const {
		return type_ == InlineType::Never;
	}
};


// Microsoft-style DLL export/import.
enum class DllType {
	Import,
	Export
};


// 
class DllAttribute : public Attribute{
private:
	DllType type_;

public:
	DllAttribute(DllType type) : type_(type) {}

	~DllAttribute() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	DllType Type() const {
		return type_;
	}

	void SetType(DllType value) {
		type_ = value;
	}
};


// 
class SectionAttribute : public Attribute {
private:
	string section_;

public:
	SectionAttribute(const string& section) : section_(section) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 	
	string Section() const {
		return section_;
	}

	void SetSection(const string& value) {
		section_ = value;
	}
};

} // namespace AST
#endif