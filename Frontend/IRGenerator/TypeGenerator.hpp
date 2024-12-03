// TypeGenerator.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_TYPE_GEN_HPP
#define PC_IR_GENERATOR_TYPE_GEN_HPP

#include "NameGenerator.hpp"
#include "../AST/Types.hpp"
#include "../AST/StructLayout.hpp"
#include "../IR/IRTypes.hpp"
#include "../IR/TypeTable.hpp"
#include "../IR/IRGenerator.hpp"
#include "../IR/Unit.hpp"
#include "../Common/TargetData.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
using namespace Base;
using namespace AST;

namespace IRGenerator {

// Forward declarations.
class UnitGenerator;

class TypeGenerator : public Visitor {
private:
	// Caches types that have been already converted.
	Dictionary<const Type*, const IR::Type*> typeMap_;
	IR::Unit* irUnit_;       // The resulting IR unit.
	UnitGenerator* unitGen_; // The associated unit generator.
	IR::IRGenerator irGen_;  // Helper used to generate IR objects.
	const Common::TargetData* target_; // The target of the compilation.
	LayoutCache* layouts_;   // Caches the layout of a record.
	IR::Function* function_; // The current function. Used for records found inside a function.
	const IR::Type* result_; // The generated type.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void HandleRecordType(const StructUnionType* type);

	// Visitor methods used to convert each type.
	virtual void Visit(const BasicType* type) override;
	virtual void Visit(const PointerType* type) override;
	virtual void Visit(const ArrayType* type) override;

	virtual void Visit(const VarArrayType* type) override {
		Visit(static_cast<const ArrayType*>(type));
	}

	virtual void Visit(const FunctionType* type) override;
	virtual void Visit(const EnumType* type) override;
	virtual void Visit(const StructType* type) override;
	virtual void Visit(const UnionType* type) override;
	virtual void Visit(const QType* type) override;

	// Creates parameter types for each of the fields in the specified 'struct'.
	void ExpandStructType(const StructType* structType, List<const IR::Type*>& paramTypes);

public:
	TypeGenerator(const Common::TargetData* target, IR::Unit* unit,
				  UnitGenerator* unitGen, LayoutCache* cache);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Return the IR type corresponding to the specified size.
	static const IR::Type* GetTypeFromSize(int size, bool floating = false);

	// Returns the IR type that corresponds to the specified AST type.
	const IR::Type* GetType(const Type* type);

	// Returns the IR type that corresponds to the specified AST type.
	// If it's a struct/union the created typename will also contain the name
	// of the function where the type was created.
	const IR::Type* GetType(const Type* type, IR::Function* function);

	// Returns the innermost type of a VLA type. For 'int a[E1][E2]' it's 'int'. 
	const Type* GetArrayInnerType(const ArrayType* vlaType);

	// Returns the innermost type of a VLA type, as an IR type.
	const IR::Type* GetIRArrayInnerType(const Type* type) {
		return GetType(type);
	}
};

} // namespace IRGenerator
#endif