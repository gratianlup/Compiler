// ObjectHash.hpp
// Copyright (c) Lup Gratian
//
// Helper used to compute the hash code of one or more objects.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_TYPE_HASH_HPP
#define PC_BASE_TYPE_HASH_HPP

#include "DefaultComparer.hpp"
#include "List.hpp"
#include <utility>

namespace Base {

class ObjectHash {
private:
	List<unsigned> data_;

public:
	ObjectHash() {}

	ObjectHash(const ObjectHash& other) : data_(other.data_) {}

	ObjectHash(ObjectHash&& other) : data_(std::move(other.data_)) {} // Move constructor.
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds the address of the specified type.
	ObjectHash& AddObject(const void* type) {
		data_.Add((unsigned)type);

		if(sizeof(type) > 4) {
			// For 64 bit pointers.
			data_.Add((unsigned)((size_t)type >> 32));
		}

		return *this;
	}

	// Adds the specified value.
	template <class T>
	ObjectHash& Add(const T& value) {
		data_.Add((unsigned)value);
		return *this;
	}

	// Returns the computed hash code.
	unsigned GetHashCode() const {
		// 'HashCalculator' already has a good hash algorithm, so use it.
		return HashCalculator::GetHashCode(data_.GetInternal(), 
										   data_.Count() * sizeof(unsigned));
	}

	// Clears the current hash code.
	void Reset() {
		data_.Clear();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const ObjectHash& other) const {
		if(data_.Count() != other.data_.Count()) return false;

		for(int i = 0; i < data_.Count(); i++) {
			if(data_[i] != other.data_[i]) return false;
		}

		return true;
	}

	// This exists only because 'DefaultComparer' requires it.
	bool operator< (const ObjectHash& other) const { return false; }
};

} // namespace Base
#endif