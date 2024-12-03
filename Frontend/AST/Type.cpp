// Type.hpp
// Copyright (c) Lup Gratian
//
// Implements the Type, QType and Qualifier classes.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#include "../Base/StringBuilder.hpp"
#include "Type.hpp"
#include "Types.hpp"
using namespace Base;

namespace AST {

bool Type::CanPromoteToInt() const {
	// Can be promoted to 'int' or 'unsigned':
	// - an object with an integer type that has rank <= rank(int) (C99:6.3.1.1.2)
	// - enumeration constants (C99:6.7.2.2.4)
	// - extension: boolean types
	if(auto temp = As<BasicType>()) {
		if(temp->IsFloating() || temp->IsWChar() || temp->IsVoid()) {
			return false;
		}

		if(temp->IsBool()) return true;
		else return temp->Rank() < BasicType::GetInt()->Rank();
	}
	else if(auto temp = As<EnumType>()) {
		return temp->ConstType()->Rank() < BasicType::GetInt()->Rank();
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* Type::WithoutQualifiers() const {
	if(kind_ == TYPE_QUALIFIED) {
		return As<QType>()->Base();
	}
	else return this;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsObject() const {
	return (this->Is<FunctionType>() || 
		   (this->IsBasic() && this->As<BasicType>()->IsVoid())) == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* Type::InnerType() const {
	if(auto temp = As<TypedefType>()) {
		return temp->Inner();
	}
	else return this;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsInteger(bool ignoreEnum) const {
	if(auto temp = As<BasicType>()) {
		// As an extension we consider '_Bool' an integer type too (GCC does this).
		return temp->IsInteger() || temp->IsChar() || temp->IsUChar() ||
			   temp->IsWChar() || temp->IsBool();
	}
	else if(IsEnum() && (ignoreEnum == false)) {
		return true;
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsFloating() const {
	if(auto temp = As<BasicType>()) {
		return temp->IsFloating();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsChar() const {
	if(auto temp = As<BasicType>()) {
		return temp->IsChar() || temp->IsWChar();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsString() const {
	if(auto temp = As<BasicType>()) {
		return temp->IsString() || temp->IsWString();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsSigned() const {
	if(auto temp = As<BasicType>()) {
		return temp->IsSignedInteger() || temp->IsChar() || temp->IsWChar();
	}
	else if(IsEnum()) return true; // Extension.
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsUnsigned() const {
	if(auto temp = As<BasicType>()) {
		return temp->IsUnsignedInteger();
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsArithmetic() const {
	return IsInteger() || IsFloating();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsScalar() const {
	return IsArithmetic() || IsPointer();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsRecord() const {
	return IsStruct() || IsUnion();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsAggregate() const {
	return IsRecord() || IsArray();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsVoid() const {
	return Is<BasicType>() && As<BasicType>()->IsVoid();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Type::IsBool() const {
	return Is<BasicType>() && As<BasicType>()->IsBool();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const BasicType* Type::AsIntegerType() const {
	if(IsInteger(true /* ignoreEnum */)) {
		return As<BasicType>();
	}
	else if(IsEnum()) {
		return As<EnumType>()->ConstType();
	}
	else return nullptr;
}

// ######################################################################################
// QType
// ######################################################################################
bool QType::IsIncompleteImpl() const {
	return base_->IsIncomplete();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ObjectHash QType::GetHashCode(const Type* type, Qualifier qual) {
	return ObjectHash().AddObject(type).Add(qual.GetHashCode());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string QType::ToStringImpl(int level) const {
	StringBuilder sb;
	sb.Append(string('\t', level));
	sb.Append("QType: ");

	if(qual_.HasConst()) {
		sb.Append("const, ");
	}

	if(qual_.HasRestrict()) {
		sb.Append("restrict, ");
	}

	if(qual_.HasVolatile()) {
		sb.Append("volatile, ");
	}

	sb.Append(string('\t', level));
	sb.Append("- Base:\n");
	sb.Append(base_->ToString(level + 1));

	return sb.ToString();
}

} // namespace AST