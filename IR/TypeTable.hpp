// TypeTable.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_TYPE_TABLE_HPP
#define PC_IR_TYPE_TABLE_HPP

#include "IRTypes.hpp"
#include "../Base/ObjectHash.hpp"
#include "../Base/Dictionary.hpp"
using namespace Base;

namespace IR {

class TypeTable {
private:
	Dictionary<ObjectHash, const Type*> types_;
	Dictionary<string*, const Type*, true> typenames_;
	List<const RecordType*> recordTypes_;

public:
	~TypeTable();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Methods to obtain the basic types.
	const Type* GetInt8()   const { return IntegerType::GetInt8();     }
	const Type* GetInt16()  const { return IntegerType::GetInt16();    }
	const Type* GetInt32()  const { return IntegerType::GetInt32();    }
	const Type* GetInt64()  const { return IntegerType::GetInt64();    }
	const Type* GetFloat()  const { return FloatingType::GetFloat();  }
	const Type* GetDouble() const { return FloatingType::GetDouble(); }
	const Type* GetVoid()   const { return VoidType::GetVoid();    }

	// Returns a pointer type that points to the specified type.
	// This can't construct pointer to 'void'.
	const PointerType* GetPointer(const Type* pointee);

	// Returns an array type having the specified element type and size.
	const ArrayType* GetArray(const Type* elementType, __int64 size);

	// Returns a function type having the specified return type and parameters.
	const FunctionType* GetFunction(const Type* returnType, const Type** parameters,
									int paramCount, bool isVarargs = false);

	// Returns a record type having the specified fields.
	const RecordType* GetRecord(const List<RecordField>& fields);

	// Returns a record type with space allocated for the specified number of fields.
	const RecordType* GetRecord(int fieldCount);

	// Adds a type name for the specified type.
	const Type* AddNamed(string* name, const Type* type);

	// Returns the type associated with the specified type name,
	// or 'nullptr' if the types can't bee found.
	const Type* GetNamed(string* name);

	// Returns 'true' if a type with the specified name exists.
	bool ContainsNamed(string* name);

	// Remove the specified named types. Does nothing if such a type cannot be found.
	void RemoveNamed(string* name);

	// Removes all type names.
	void ClearNamed();
};

} // namespace IR
#endif