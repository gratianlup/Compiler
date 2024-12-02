// TypeGen.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

TypeGenerator::TypeGenerator(const Common::TargetData* target, IR::Unit* unit, 
							 UnitGenerator* unitGen,LayoutCache* cache) :
		target_(target), irUnit_(unit), unitGen_(unitGen), 
		layouts_(cache), function_(nullptr), irGen_(unit) {
	DebugValidator::IsNotNull(target);
	DebugValidator::IsNotNull(unit);
	DebugValidator::IsNotNull(unitGen);
	DebugValidator::IsNotNull(cache);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IR::Type* TypeGenerator::GetTypeFromSize(int size, bool floating) {
	if(floating) {
		switch(size) {
			case 4: return IR::FloatingType::GetFloat();
			case 8: return IR::FloatingType::GetDouble();
		}
	}
	else {
		switch(size) {
			case 1: return IR::IntegerType::GetInt8();
			case 2: return IR::IntegerType::GetInt16();
			case 4: return IR::IntegerType::GetInt32();
			case 8: return IR::IntegerType::GetInt64();
		}
	}

	DebugValidator::Unreachable();
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::Visit(const BasicType* type) {
	// Note that 'void' should not appear here. The only valid construct
	// involving 'void' is 'pointer to void'.
	int size = target_->Size(type->GetKind());

	if(type->IsFloating()) {
		result_ = GetTypeFromSize(size, true);
	}
	else result_ = GetTypeFromSize(size, false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::Visit(const QType* type) {
	// The qualifiers are ignored.
	result_ = GetType(type->InnerType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::Visit(const PointerType* type) {
	auto pointeeType = type->PointeeType()->WithoutQualifiers();

	if(pointeeType->IsVoid()) {
		// If the pointer points to 'void' we make a IR pointer type that
		// points to the integer type designated by the target as large enough
		// to hold any pointer.
		auto irPointeeType = GetTypeFromSize(target_->GetPointerSize());
		result_ = irUnit_->Types().GetPointer(irPointeeType);
	}
	else {
		// If the pointee type is a VLA we don't apply the pointer anymore
		// (VLAs are automatically converted to a pointer to their inner type).
		auto irPointeeType = GetType(type->PointeeType());
		
		if(pointeeType->IsVariable() && pointeeType->IsArray()) {
			result_ = irPointeeType;
		}
		else result_ = irUnit_->Types().GetPointer(irPointeeType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* TypeGenerator::GetArrayInnerType(const ArrayType* vlaType) {
	const Type* lastType;
	const ArrayType* currentType = vlaType;

	do {
		lastType = currentType->ElementType();
		currentType = lastType->WithoutQualifiers()->As<ArrayType>();
	} while(currentType);

	return lastType;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::Visit(const ArrayType* type) {
	// We transform something like 'Array(5) -> Array(2) -> Int'
	// into '[5 [2 int32]]', without considering any qualifiers.
	// VLAs are transformed into a pointer to the innermost element type.
	if(type->IsVariable() == false) {
		auto elementType = GetType(type->ElementType());
		result_ = irUnit_->Types().GetArray(elementType, type->Size());
	}
	else {
		// Variable-length array case.
		auto vlaElemType = GetType(GetArrayInnerType(type));
		result_ = irUnit_->Types().GetPointer(vlaElemType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::Visit(const FunctionType* functionType) {
	// The return type can be 'void'. In this case we generate
	// the special 'void' IR type instead of using 'GetType'.
	const IR::Type* returnType;
	bool hasRecordReturn = functionType->ReturnType()->WithoutQualifiers()->IsRecord();

	// A function returning a record is transformed in a function
	// that takes a pointer to a record as it's last parameter;
	// the return parameter becomes 'void'. For example, 
	// 'struct ABC f(int a)'  ->  'funct f(var a int32, var $retval <$struct_ABC>*) : void'
	if(functionType->IsVoid() || hasRecordReturn) {
		returnType = IR::VoidType::GetVoid();
	}
	else returnType = GetType(functionType->ReturnType());

	// Create the list of parameter types.
	List<const IR::Type*> paramTypes;
	auto& parameters = functionType->Parameters();

	for(int i = 0; i < parameters.Count(); i++) {
		// If the parameter is a 'struct' we can perform a light version of the
		// 'scalar replacement of aggregates' optimization.
		// The 'struct' is a candidate only if it's "simple".
		if(auto recordType = parameters[i]->WithoutQualifiers()->As<StructUnionType>()) {
			if(unitGen_->IsExpandableStruct(recordType)) {
				// We expand the 'struct' into more parameter, one for each field.
				// Note that fields having 'union' type are not allowed.
				ExpandStructType(recordType->As<StructType>(), paramTypes);
			}
			else {
				// "Large" records and unions are passed "by reference".
				auto irRecordType = GetType(recordType);
				auto irRecordPtrType = irGen_.GetPointer(irRecordType);
				paramTypes.Add(irRecordPtrType);
			}
		}
		else {
			// Most types are directly mapped to the IR.
			paramTypes.Add(GetType(parameters[i]));
		}
	}

	// A function that has a record as it's return type gets a last parameter
	// having type 'pointer to record' (used to pass the returned record).
	if(hasRecordReturn) {
		paramTypes.Add(irGen_.GetPointer(GetType(functionType->ReturnType())));
	}

	// Create the IR function type, marking it as Varargs if necessary.
	result_ = irUnit_->Types().GetFunction(returnType, paramTypes.GetInternal(),
										   paramTypes.Count(), functionType->IsVarargs());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::ExpandStructType(const StructType* structType, 
									 List<const IR::Type*>& paramTypes) {
	for(int i = 0; i < structType->FieldCount(); i++) {
		auto fieldType = structType->Fields()[i]->DeclarationType()->WithoutQualifiers();

		// If we have a nested 'struct' expand it's fields before continuing
		// with the fields from this 'struct'.
		if(auto nestedStructType = fieldType->As<StructType>()) {
			ExpandStructType(nestedStructType, paramTypes);
		}
		else paramTypes.Add(GetType(fieldType));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::Visit(const EnumType* type) {
	// We use the type that stores the enumeration.
	result_ = GetType(type->ConstType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::Visit(const StructType* type) {
	HandleRecordType(type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::Visit(const UnionType* type) {
	HandleRecordType(type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeGenerator::HandleRecordType(const StructUnionType* type) {
	// We need to create the record type before the members 
	// because the record can point to itself.
	// Each member is described by it's type and offset.
	auto layout = layouts_->GetOrCreate(type);
	auto& fields = layout->Fields();
		
	// Create the record and add it to the map early, so that the record
	// can be referred from inside (only through a pointer in C).
	auto record = irUnit_->Types().GetRecord(fields.Count()); 
	typeMap_.Add(type, record);
	
	// We also create a typename having the form '$struct_name' if it's declared
	// at unit level and '$rec_funct_name' if we're in a function.
	// If the name is taken we append a number until one available is found.
	// Note that it's possible that the record to not have a name (especially for 'union').
	string name = type->ParentDeclaration()->Name() ?
				  type->ParentDeclaration()->Name()->Name() : "unnamed";
	string prefix = type->IsStruct() ? "struct" : "union";
	IR::SymbolTable* table = function_ ? &function_->Symbols() :
                                         &irUnit_->Symbols();

    // Populate the list with the fields.
	int prevIndex = -1;
	auto& irFields = const_cast<List<IR::RecordField>&>(record->Fields());

	for(int i = 0; i < fields.Count(); i++) {
		auto& field = fields[i];
		if(field->Index() == prevIndex) {
			// This is a bitfield that is packed together
			// with the previous bitfield, so we skip it.
			continue;
		}

		auto fieldType = GetType(field->Field()->DeclarationType());
		irFields.Add(IR::RecordField(fieldType, field->ByteOffset()));
		prevIndex = field->Index();
	}

	// Now the typename can be added. We add it only here because the (possible)
	// child record must be added before it. For example,
	// 'struct ABC { struct DEF { struct GHI { int a,b; } c; } d; }' 
    //  -> GHI, DEF, ABC.
    NameGenerator nameGen;

	string recordName = function_ && type->ParentDeclaration()->HasLinkage() ? 
                        nameGen.GetName(table, name, *function_->Name(), prefix) :
                        nameGen.GetName(table, name, "", prefix);
	auto typeName = IR::Symbol::GetTypename(record, recordName, table);
	unitGen_->AddTypename(typeName);
	result_ = record;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IR::Type* TypeGenerator::GetType(const Type* type) {
	DebugValidator::IsNotNull(type);
	
	// First we check if the type has already been converted.
	// We use the type without any qualifiers, because they don't matter here.
	type = type->WithoutQualifiers();
	const IR::Type* irType = nullptr;

	if(typeMap_.TryGetValue(type, &irType)) {
		return typeMap_[type];
	}

	// The type needs to be created. We add it to the map so that
	// we can find it faster (this is the purpose of the map, because
	// the IR type system already creates a single type if appropriate).
	type->Accept(this);
	typeMap_.Add(type, result_);
	return result_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const IR::Type* TypeGenerator::GetType(const Type* type, IR::Function* function) {
	DebugValidator::IsNotNull(type);
	DebugValidator::IsNotNull(function);
	
	// Set the current function. Used when generating records declared in a function.
	function_ = function;
	auto result = GetType(type);
	function_ = nullptr;
	return result;
}

} // namespace IRGenerator