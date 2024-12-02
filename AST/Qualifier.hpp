// Qualifier.hpp
// Copyright (c) Lup Gratian
//
// Represents a qualifier associated with a type. 
// Multiple combinations are allowed. By default no qualifier is set.
// To use a qualifier a type must be redirected through a QType object.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_QUALIFIER_HPP
#define PC_AST_QUALIFIER_HPP

namespace AST {

class Qualifier {
private:
	// Constants that specify the type of the allowed qualifiers.
	static const char QUAL_NONE     = 0;
	static const char QUAL_CONST    = 1;
	static const char QUAL_VOLATILE = 2;
	static const char QUAL_RESTRICT = 4;

	char type_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Qualifier(char type) : type_(type) {}

public:
	Qualifier() : type_(QUAL_NONE) {}

	Qualifier(const Qualifier& other) : type_(other.type_) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if the type has a 'const' qualifier.
	bool HasConst() const {
		return (type_ & QUAL_CONST) != 0;
	}
	
	Qualifier& SetHasConst(bool value) {
		if(value) type_ |= QUAL_CONST;
		else type_ &= ~QUAL_CONST;
		return *this;
	}

	// Returns 'true' if the type has a 'volatile' qualifier.
	bool HasVolatile() const {
		return (type_ & QUAL_VOLATILE) != 0;
	}
	
	Qualifier& SetHasVolatile(bool value) {
		if(value) type_ |= QUAL_VOLATILE;
		else type_ &= ~QUAL_VOLATILE;
		return *this;
	}

	// Returns 'true' if the type has a 'restrict' qualifier.
	bool HasRestrict() const {
		return (type_ & QUAL_RESTRICT) != 0;
	}
	
	Qualifier& SetHasRestrict(bool value) {
		if(value) type_ |= QUAL_RESTRICT;
		else type_ &= ~QUAL_RESTRICT;
		return *this;
	}

	// Returns 'true' if no qualifier is set (default).
	bool HasNone() const {
		return type_ == QUAL_NONE;
	}

	// Resets all qualifiers.
	void ResetAll() {
		type_ = QUAL_NONE;
	}

	// Returns a new qualifier having the qualifiers from this one and the given one.
	Qualifier Combine(Qualifier b) {
		char combined = type_ | b.type_;
		return Qualifier(combined);
	}

	// Returns a qualifier with 'const' set.
	static Qualifier GetConst() {
		return Qualifier(QUAL_CONST);
	}

	// Returns a qualifier with 'volatile' set.
	static Qualifier GetVolatile() {
		return Qualifier(QUAL_VOLATILE);
	}

	// Returns a qualifier with 'restrict' set.
	static Qualifier GetRestrict() {
		return Qualifier(QUAL_RESTRICT);
	}

	unsigned GetHashCode() const {
		return (unsigned)type_;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const Qualifier& other) const {
		return type_ == other.type_;
	}

	bool operator!= (const Qualifier& other) const {
		return type_ != other.type_;
	}
};

} // namespace AST
#endif