#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "Declarations.hpp"
#include "Types.hpp"
using namespace Base;

namespace AST {

string VariableDeclaration::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("VariableDeclaration: ");
	if(ident_) sb.Append(ident_->Name());

	if(storage_ == StorageType::Static) sb.Append("Static, ");
	else if(storage_ == StorageType::Extern) sb.Append("Extern, ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Type:");
	sb.Append(type_->ToString(level + 1));

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool VariableDeclaration::EqualsImpl(const Declaration* other) const {
	if(auto t = other->As<VariableDeclaration>()) {
		if(!((init_ == nullptr) && (t->init_ == nullptr))) {
			return false;
		}

		if(init_ && (init_->Equals(t->init_))) {
			return false;
		}

		return (storage_ == t->storage_) && Declaration::EqualsImpl(other);
	}
	else return false;
}

// ######################################################################################
// FunctionDeclaration
// ######################################################################################
const FunctionType* FunctionDeclaration::DeclarationType() const {
	return type_->As<FunctionType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FunctionDeclaration::EqualsImpl(const Declaration* other) const {
	if(auto t = other->As<FunctionDeclaration>()) {
		return (isStatic_ == t->isStatic_) &&
			   (storage_ == t->storage_) &&
			   Declaration::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string FunctionDeclaration::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("FunctionDeclaration: ");
	if(ident_) sb.Append(ident_->Name());

	if(isStatic_) sb.Append("Static, ");

	if(storage_ == StorageType::Static) sb.Append("Static, ");
	else if(storage_ == StorageType::Extern) sb.Append("Extern, ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.AppendLine("- Type:");
	sb.Append(type_->ToString(level + 1));

	if((level < 2) && HasDefinition()) {
		sb.AppendLine();
		sb.Append(string('\t', level));
		sb.AppendLine("- Body:");
		sb.Append(body_->ToString(level + 1));
	}

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FunctionDeclaration::IsGlobal() const {
	return linkage_ == LinkageType::External;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FunctionDeclaration::IsMain() const {
	return ident_->Name() == "main"; // The standard entry point name.
}

// ######################################################################################
// EnumConstDeclaration
// ######################################################################################
bool EnumConstDeclaration::EqualsImpl(const Declaration* other) const {
	if(auto t = other->As<EnumConstDeclaration>()) {
		if(!((valueExpr_ == nullptr) && (t->valueExpr_ == nullptr))) {
			return false;
		}

		if(valueExpr_ && (valueExpr_->Equals(t->valueExpr_) == false)) {
			return false;
		}

		return Declaration::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string EnumConstDeclaration::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("EnumConstDeclaration: ");
	if(ident_) sb.Append(ident_->Name());

	if(HasValue()) {
		sb.AppendLine();
		sb.Append(string('\t', level));
		sb.AppendLine("- Value:");
		sb.Append(valueExpr_->ToString(level + 1));
	}

	return sb.ToString();
}

// ######################################################################################
// EnumDeclaration
// ######################################################################################
const EnumType* EnumDeclaration::DeclarationType() const {
	return type_->As<EnumType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
EnumType* EnumDeclaration::DeclarationType() {
	return const_cast<EnumType*>(type_->As<EnumType>());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool EnumDeclaration::EqualsImpl(const Declaration* other) const {
	if(auto t = other->As<EnumDeclaration>()) {
		return Declaration::EqualsImpl(t);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string EnumDeclaration::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("EnumDeclaration: ");
	if(ident_) sb.Append(ident_->Name());

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.Append("- Type:");
	sb.Append(type_->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// FieldDeclaration
// ######################################################################################
bool FieldDeclaration::EqualsImpl(const Declaration* other) const {
	if(auto t = other->As<FieldDeclaration>()) {
		if((bitfield_ < 0) ^ (t->bitfield_ < 0)) {
			return false;
		}

		if(bitfield_ != t->bitfield_) {
			return false;
		}

		return Declaration::EqualsImpl(other);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string FieldDeclaration::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("FieldDeclaration: ");
	if(ident_) sb.Append(ident_->Name());

	if(IsBitfield()) {
		sb.AppendLine();
		sb.Append(string('\t', level + 1));
		sb.AppendFormat(_T("- Bitfield: %d"), bitfield_);
	}

	return sb.ToString();
}

// ######################################################################################
// StructUnionDeclaration
// ######################################################################################
const StructUnionType* StructUnionDeclaration::DeclarationType() const {
	return type_->As<StructUnionType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StructUnionType* StructUnionDeclaration::DeclarationType() {
	return const_cast<StructUnionType*>(type_->As<StructUnionType>());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool StructUnionDeclaration::EqualsImpl(const Declaration* other) const {
	if(auto t = other->As<StructUnionDeclaration>()) {
		return Declaration::EqualsImpl(t);
	}
	else return false;
}

// ######################################################################################
// StructDeclaration
// ######################################################################################
const StructType* StructDeclaration::DeclarationType() const {
	return type_->As<StructType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StructType* StructDeclaration::DeclarationType() {
	return const_cast<StructType*>(type_->As<StructType>());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string StructDeclaration::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("StructDeclaration: ");
	if(ident_) sb.Append(ident_->Name());

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.Append("- Type:");
	sb.Append(type_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// UnionDeclaration
// ######################################################################################
const UnionType* UnionDeclaration::DeclarationType() const {
	return type_->As<UnionType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UnionType* UnionDeclaration::DeclarationType() {
	return const_cast<UnionType*>(type_->As<UnionType>());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string UnionDeclaration::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("UnionDeclaration: ");
	if(ident_) sb.Append(ident_->Name());

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.Append("Type:");
	sb.Append(type_->ToString(level + 1));
	return sb.ToString();
}

// ######################################################################################
// TypedefDeclaration
// ######################################################################################
const TypedefType* TypedefDeclaration::DeclarationType() const {
	return type_->As<TypedefType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool TypedefDeclaration::EqualsImpl(const Declaration* other) const {
	if(auto t = other->As<TypedefDeclaration>()) {
		return Declaration::EqualsImpl(t);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string TypedefDeclaration::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("TypedefDeclaration: ");
	if(ident_) sb.Append(ident_->Name());

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.Append("- Type:");
	sb.Append(type_->ToString(level + 1));

	return sb.ToString();
}

// ######################################################################################
// DeclarationList
// ######################################################################################
bool DeclarationList::EqualsImpl(const Declaration* other) const {
	if(auto t = other->As<DeclarationList>()) {
		if(list_.Count() != t->list_.Count()) return false;

		for(int i = 0; i < list_.Count(); i++) {
			if(list_[i]->Equals(t->list_[i]) == false) {
				return false;
			}
		}

		return true;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string DeclarationList::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("DeclarationList: ");

	sb.AppendLine();
	sb.Append(string('\t', level));
	sb.Append("- Children:");

	for(int i = 0; i < list_.Count(); i++) {
		auto& child = list_[i];
		sb.Append(child->ToString(level + 1));
	}

	return sb.ToString();
}

} // namespace AST