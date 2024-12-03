// Types.cpp
// Copyright (c) Lup Gratian
//
// Implements all the helper classes derived from Type.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "../Base/StringBuilder.hpp"
#include "Types.hpp"
#include "TypeCombiner.hpp"
#include "Declarations.hpp"
using namespace Base;

namespace AST {

const shared<BasicType> BasicType::types_[TypeKind::END] = {
	new BasicType(TypeKind::Bool),
	new BasicType(TypeKind::Char),
	new BasicType(TypeKind::UChar),
	new BasicType(TypeKind::WChar),
	new BasicType(TypeKind::String),
	new BasicType(TypeKind::WString),
	new BasicType(TypeKind::Short),
	new BasicType(TypeKind::UShort),
	new BasicType(TypeKind::Int),
	new BasicType(TypeKind::UInt),
	new BasicType(TypeKind::Long),
	new BasicType(TypeKind::ULong),
	new BasicType(TypeKind::LongLong),
	new BasicType(TypeKind::ULongLong),
	new BasicType(TypeKind::Float),
	new BasicType(TypeKind::Double),
	new BasicType(TypeKind::Void)
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool BasicType::EqualsImpl(const Type* other) const {
	return this == other;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool BasicType::IsIncompleteImpl() const {
	// Among the basic types, only 'void' is considered incomplete.
	return type_ == TypeKind::Void;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string BasicType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("BasicType: ");

	switch(type_) {
		case TypeKind::Bool      : { sb.Append("Bool"); break; }
		case TypeKind::Char      : { sb.Append("Char"); break; }
		case TypeKind::UChar     : { sb.Append("UChar"); break; }
		case TypeKind::WChar     : { sb.Append("WChar"); break; }
		case TypeKind::String    : { sb.Append("String"); break; }
		case TypeKind::WString   : { sb.Append("WString"); break; }
		case TypeKind::Short     : { sb.Append("Short"); break; }
		case TypeKind::UShort    : { sb.Append("UShort"); break; }
		case TypeKind::Int       : { sb.Append("Int"); break; }
		case TypeKind::UInt      : { sb.Append("UInt"); break; }
		case TypeKind::Long      : { sb.Append("Long"); break; }
		case TypeKind::ULong     : { sb.Append("ULong"); break; }
		case TypeKind::LongLong  : { sb.Append("LongLong"); break; }
		case TypeKind::ULongLong : { sb.Append("ULongLong"); break; }
		case TypeKind::Float     : { sb.Append("Float"); break; }
		case TypeKind::Double    : { sb.Append("Double"); break; }
		case TypeKind::Void      : { sb.Append("Void"); break; }
	}

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int BasicType::Rank() const {
	DebugValidator::IsTrue(IsInteger() || IsChar() || IsUChar());
	
	switch(type_) {
		case TypeKind::Bool:      return 0;
		case TypeKind::Char:      return 1;
		case TypeKind::UChar:     return 2;
		case TypeKind::Short:
		case TypeKind::UShort:    return 3;
		case TypeKind::Int:
		case TypeKind::UInt:      return 4;
		case TypeKind::Long:
		case TypeKind::ULong:     return 5;
		case TypeKind::LongLong:
		case TypeKind::ULongLong: return 6;
	}

	return -1; // Should not be reached.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int BasicType::Size(const TargetData* target) const {
	return target->Size(type_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int BasicType::GetAlignment(const TargetData* target) const {
	return target->GetAlignment(type_);
}

// ######################################################################################
// PointerType
// ######################################################################################
bool PointerType::EqualsImpl(const Type* other) const {
	return this == other;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PointerType::IsVariableImpl() const {
	return pointee_->IsVariable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash PointerType::GetHashCode(const Type* pointee) {
	return ObjectHash().AddObject(pointee);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string PointerType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("PointerType: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- PointeeType: ");
	sb.Append(pointee_->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// ArrayType
// ######################################################################################
bool ArrayType::IsIncompleteImpl() const {
	return IsIncomplete();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ArrayType::EqualsImpl(const Type* other) const {
	return this == other;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ArrayType::IsVariableImpl() const {
	return IsVariableArray() || elemType_->IsVariable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash ArrayType::GetHashCode(const Type* elementType, bool incomplete, __int64 size,
								  bool isStatic, Qualifier qual) {
	ObjectHash hash;
	hash.AddObject(elementType);
	hash.Add(incomplete);
	hash.Add((unsigned)size ^ (unsigned)(size >> 32));
	hash.Add(isStatic);
	hash.Add(qual.GetHashCode());

	return hash;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string ArrayType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("ArrayType: ");

	sb.Append(string::Format(_T("%d, "), size_));
	if(incomplete_) sb.Append("Incomplete, ");
	else sb.Append("Complete, ");

	if(static_) sb.Append("static, ");
	if(qual_.HasRestrict()) sb.Append("restrict");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Element:");
	sb.Append(elemType_->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// VarArrayType
// ######################################################################################
bool VarArrayType::EqualsImpl(const Type* other) const {
	return this == other;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash VarArrayType::GetHashCode(const Type* elementType, shared<Expression> sizeExpr,
								     bool isStatic, Qualifier qual) {
	ObjectHash hash;
	hash.AddObject(elementType);
	//hash.Add(sizeExpr);
	hash.Add(isStatic);
	hash.Add(qual.GetHashCode());

	return hash;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string VarArrayType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("VarArrayType: ");

	if(incomplete_) sb.Append("Incomplete, ");
	else sb.Append("Complete, ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Expression:");
	sb.Append(sizeExpr_->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// FunctionType
// ######################################################################################
bool FunctionType::IsVoid() const {
	// Use the unqualified type because 'const void' is valid.
	if(auto temp = returnType_->WithoutQualifiers()->As<BasicType>()) {
		return temp->IsVoid();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FunctionType::EqualsImpl(const Type* other) const {
	return this == other;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash FunctionType::GetHashCode(const Type* returnType, bool isVarargs,
								     const List<const Type*>* paramTypes) {
	ObjectHash hash;
	hash.AddObject(returnType);
	hash.Add(isVarargs);

	if(paramTypes && (paramTypes->Count() > 0)) {
		hash.Add(paramTypes->Count());
		auto& parameters = *paramTypes;

		for(int i = 0; i < parameters.Count(); i++) {
			hash.AddObject(parameters[i]);
		}
	}

	return hash;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string FunctionType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("FunctionType: ");

	if(isVarargs_) sb.Append("varargs");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Return:");
	sb.Append(returnType_->ToString(level + 1));

	if(IsVoid() == false) {
		sb.AppendLine();
		sb.Append(string('\t', level));
		sb.AppendLine("- Params:");

		params_.ForEach([&sb, level](const Type* declaration) -> bool {
			sb.AppendLine();
			sb.Append(declaration->ToString(level + 1));
			return true;
		});
	}

	return sb.ToString();
}

// ######################################################################################
// EnumType
// ######################################################################################
bool EnumType::IsIncompleteImpl() const {
	return decl_->HasDefinition() == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EnumType::EqualsImpl(const Type* other) const {
	if(auto temp = other->As<EnumType>()) {
		// The types should have the same declaration.
		return decl_ == temp->decl_;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EnumType::HasAssignedConstant() const {
	for(int i = 0; i < constants_.Count(); i++) {
		if(constants_[i]->HasValue()) {
			return true;
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string EnumType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("EnumType: ");
	sb.Append(string::Format(_T("%d, "), constants_.Count()));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Type:");
	sb.Append(constType_->ToString(level + 1));

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Constants:");

	constants_.ForEach([&sb, level](const shared<EnumConstDeclaration>& declaration) -> bool {
		sb.AppendLine();
		sb.Append(declaration->ToString(level + 1));
		return true;
	});

	return sb.ToString();
}

// ######################################################################################
// StructUnionType
// ######################################################################################
bool StructUnionType::EqualsImpl(const Type* other) const {
	auto t = other->As<StructUnionType>();
	if(t == nullptr) return false;

	// The types should have the same declaration.
	return decl_ == t->decl_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StructUnionType::IsIncompleteImpl() const {
	return decl_->HasDefinition() == false;
}
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StructUnionType::HasBitfield() const {
	bool found = false;

	fields_.ForEach([&found](const shared<FieldDeclaration>& field) -> bool {
		if(field->IsBitfield()) {
			found = true;
			return false;
		}

		return true;
	});

	return found;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StructUnionType::HasUnnamedBitfield() const {
	bool found = false;

	fields_.ForEach([&found](shared<FieldDeclaration>& field) -> bool {
		if(field->IsUnnamedBitfield()) {
			found = true;
			return false;
		}

		return true;
	});

	return found;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StructUnionType::HasFlexArray(bool children) const {
	if(fields_.Count() == 0) return false;

	if(auto t = fields_[fields_.Count() - 1]->DeclarationType()) {
		return t->IsArray() && t->As<ArrayType>()->IsIncomplete();
	}
	else if(children) {
		// All children, if they are an aggregate, must be checked too.
		for(int i = 0; i <  fields_.Count(); i++) {
			if(fields_[i]->IsStructDecl()) {
				// Search only in definitions.
				auto temp = fields_[i]->GetDefinition();
				if(temp) {
					if(temp->As<StructDeclaration>()->DeclarationType()->HasFlexArray(true)) {
						return true; // Found a flexible array.
					}
				}
			}
			else if(fields_[i]->IsUnionDecl()) {
				// Search only in definitions.
				auto temp = fields_[i]->GetDefinition();
				if(temp ) {
					if(temp->As<UnionDeclaration>()->DeclarationType()->HasFlexArray(true)) {
						return true; // Found a flexible array.
					}
				}
			}
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StructUnionType::HasConstMember() const {
	// Test if any of the fields has a 'const' qualifiers. If a field
	// is another struct/union, it's fields are tested too, and so on (C99:6.3.2.1).
	for(int i = 0; i < fields_.Count(); i++) {
		auto& field = fields_[i];
		auto type = field->DeclarationType();
		if(type->IsQualified()) {
			if(type->As<QType>()->HasConst()) {
				return true; // Found a 'const'.
			}
		}

		if(auto t = type->As<StructType>()) {
			if(t->HasConstMember()) {
				return true; // The struct has a 'const' field.
			}
		}
		else if(auto t = type->As<UnionType>()) {
			if(t->HasConstMember()) {
				return true; // The union has a 'const' field.
			}
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string StructUnionType::ToStringImpl(int level) const {
	StringBuilder sb;

	if(decl_->HasDefinition()) sb.Append("Defined, ");
	else sb.Append("Undefined, ");

	sb.Append(string('\t', level));
	sb.AppendLine("- Fields:");

	fields_.ForEach([&sb, level](shared<FieldDeclaration>& field) -> bool {
		sb.AppendLine();
		sb.Append(field->ToString(level + 1));
		return true;
	});

	return sb.ToString();
}

// ######################################################################################
// StructType
// ######################################################################################
StructDeclaration* StructType::ParentDeclaration() {
	return decl_->As<StructDeclaration>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const StructDeclaration* StructType::ParentDeclaration() const {
	return decl_->As<StructDeclaration>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string StructType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("StructType: ");
	sb.Append(StructUnionType::ToStringImpl(level));

	return sb.ToString();
}

// ######################################################################################
// UnionType
// ######################################################################################
UnionDeclaration* UnionType::ParentDeclaration() {
	return decl_->As<UnionDeclaration>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const UnionDeclaration* UnionType::ParentDeclaration() const {
	return decl_->As<UnionDeclaration>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string UnionType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("UnionType: ");
	sb.Append(StructUnionType::ToString(level));

	return sb.ToString();
}

// ######################################################################################
// TypedefType
// ######################################################################################
bool TypedefType::EqualsImpl(const Type* other) const {
	return this == other;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TypedefType::IsVariableImpl() const {
	return inner_->IsVariable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash TypedefType::GetHashCode(const Type* parentType) {
	return ObjectHash().AddObject(parentType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string TypedefType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("TypedefType: ");

	sb.Append(string('\t', level));
	sb.AppendLine("- Parent:");
	sb.Append(parent_->ToString(level + 1));

	sb.Append(string('\t', level));
	sb.AppendLine("- Inner:");
	sb.Append(inner_->ToString(level + 1));

	return sb.ToString();
}

} // namespace AST