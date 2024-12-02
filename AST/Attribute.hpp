// Attribute.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for the attributes that can be attached to a declaration.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_ATTRIBUTE_HPP
#define PC_AST_ATTRIBUTE_HPP

namespace AST {

class Attribute {
private:
	Attribute(const Attribute&);            // Should not be copied.
	Attribute& operator=(const Attribute&); // Should not be assigned.

public:
	Attribute() {}

	virtual ~Attribute() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return dynamic_cast<T*>(this);
	}

	template <class T>
	const T* As() const {
		return dynamic_cast<T*>(const_cast<Attribute*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return As<T>();
	}
};

} // namespace AST
#endif