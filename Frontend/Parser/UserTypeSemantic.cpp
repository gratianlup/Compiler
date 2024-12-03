// UserTypeSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements semantic analysis methods for struct/union/enum types.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DeclarationSemantic.hpp"
#include "StatementSemantic.hpp"
#include "SemanticHolder.hpp"
#include "../AST/StructLayout.hpp"
#include "../AST/TypeCombiner.hpp"
#include "../AST/TypeString.hpp"
#include "../Base/DebugValidator.hpp"
using namespace Base;
using namespace AST;

namespace Parsing {

bool DeclarationSemantic::SameTagType(TagType a, shared<Declaration> b) {
	if(a == Tag_Enum) {
		return b->Is<EnumDeclaration>();
	}
	else if(a == Tag_Struct) {
		return b->Is<StructDeclaration>();
	}
	else return b->Is<UnionDeclaration>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclarationSemantic::ReportTagRedefinition(TagType type, shared<Identifier> name, 
										        LocationInfo location) {
	if(type == Tag_Enum) {
		diag_->Report(Error::ENUM_REDEFINITION)<<*name<<location;
	}
	else if(type == Tag_Struct) {
		diag_->Report(Error::STRUCT_REDEFINITION)<<*name<<location;
	}
	else diag_->Report(Error::UNION_REDEFINITION)<<*name<<location;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> 
DeclarationSemantic::CreateTagDecl(TagType type, shared<Identifier> name, 
                                   LocationInfo location, const Type* declType, 
                                   DeclarationContext* context) {
	if(declType == nullptr) {
		// There is no previous declaration from which the type can be borrowed.
		// The type and the declaration are linked. The storage class specifiers
		// are not applied to the tag declaration.
		if(type == Tag_Enum) {
			EnumType* enumType = types_->GetEnum(nullptr);
			EnumDeclaration* enumDecl = 
                    new EnumDeclaration(name, enumType, location, location);

			enumType->SetParentDeclaration(enumDecl);
			enumDecl->SetStorage(StorageType::None);
			enumDecl->SetLinkage(LinkageType::None);
			return enumDecl;
		}
		else if(type == Tag_Struct) {
			StructType* structType = types_->GetStruct();
			StructDeclaration* structDecl = 
                    new StructDeclaration(name, structType, location, location);

			structType->SetParentDeclaration(structDecl);
			structDecl->SetStorage(StorageType::None);
			structDecl->SetLinkage(LinkageType::None);
			return structDecl;
		}
		else {
			UnionType* unionType = types_->GetUnion();
			UnionDeclaration* unionDecl = 
                    new UnionDeclaration(name, unionType, location, location);

			unionType->SetParentDeclaration(unionDecl);
			unionDecl->SetStorage(StorageType::None);
			unionDecl->SetLinkage(LinkageType::None);
			return unionDecl;
		}
	}
	else {
		if(type == Tag_Enum) {
			return new EnumDeclaration(name, declType, location, location);
		}
		else if(type == Tag_Struct) {
			return new StructDeclaration(name, declType, location, location);
		}
		else return new UnionDeclaration(name, declType, location, location);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> 
DeclarationSemantic::HandleTagDecl(shared<DeclarationContext> context, TagType type, 
                                   TagDeclType declType, bool hasName, 
                                   shared<Identifier> name, LocationInfo location, 
                                   bool& isReference) {
	isReference = false;

	if(hasName == false) {
		// Anonymous declarations are always distinct (C99:6.7.2.3.5).
		// Create an unnamed declaration for it.
		DeclarationContext* parent = context->ParentBlock();
		return CreateTagDecl(type, nullptr, location, nullptr, parent);
	}

	if(declType != Tag_Reference) {
		// Search a previous declaration/definition only in the current context.
		// The context in which the declaration is made is the enclosing
		// block context. For example 'struct A { struct B { int a; }; };
		// struct B b; b.a = 3' is valid (B is visible outside).
		DeclarationContext* parent = context->ParentBlock();
		DeclarationContext* temp;
		shared<Declaration> previous = parent->FindTag(name, &temp, false);

		if(previous) {
			// Make sure the declaration/definition doesn't redefine a tag
			// that has a different type (C99:6.7.2.3.2).
			if(SameTagType(type, previous) == false) {
				diag_->Report(Error::TAG_REDECLARATION)<<*name;
				return nullptr;
			}

			// If it's a definition no previous definition should exist (C99:6.7.2.3.1).
			if((declType == Tag_Definition) && previous->HasDefinition()) {
				ReportTagRedefinition(type, name, location);
				return nullptr;
			}
		}

		// Create a new declaration. If a previous one exist, link to it.
		// Else add it to the context.
		const Type* prevType = nullptr;
		if(previous) prevType = previous->DeclarationType();

		// Even if this is a definition don't mark it.  It will be marked by the parser
		// when the } is found. This prevents using a definition before it's complete.
		// Pointers to it are allowed, though. See C99:6.7.2.3.4 and C99:6.7.2.3.10.
		shared<Declaration> newDecl = CreateTagDecl(type, name, location, 
                                                    prevType, parent);

		if(previous) {
			// Link the declaration to the previous one.
			Declaration* last = previous->LastDeclaration();
			last->SetNext(newDecl);
			newDecl->SetPrevious(last);
		}
		else {
			// Add the new declaration to the current context.
			parent->AddTag(name, newDecl);
		}

		// 'enum' cannot be forward-declared. Something like
		// 'enum ABC;' is not valid (C99:6.7.2.3.7, 113).
		if((type == Tag_Enum) && (declType == Tag_Declaration)) {
			// Continue even if this error occurred.
			diag_->Report(Error::ENUM_FORWARD_DECLARATION)<<location<<*name;
		}

		// A tag declaration cannot be made inside the initializer portion
		// of a 'for' loop (C99:6.8.5.3 - only identifiers can be declared).
		// Note that anonymous declarations are allowed though.
		if(hasName && context->IsBlockScope() && 
           (context->Flags() == BlockFlags::Loop)) {
			// Continue even if this error occurred.
			diag_->Report(Error::TAG_DECL_IN_FOR_LOOP_INIT)<<location<<*name;
		}

		// Warn if the declaration was made in a function prototype, because it's
		// visible only inside the function (like in 'void f(struct A { int t; } a);').
		if((previous == nullptr) && context->IsFunctProtoScope()) {
			diag_->Report(Warning::TAG_DECLARATION_IN_FUNCTION_PROTOTYPE)<<location<<*name;
		}

		return newDecl;
	}
	else {
		// If the reference points to a previous declaration, the tag types
		// should match. Else, if no declaration is found, a new incomplete
		// type is declared in the appropriate context.
		DeclarationContext* temp;
		DeclarationContext* parent = context->ParentBlock();

		shared<Declaration> previous = parent->FindTag(name, &temp, true);
		if(previous == nullptr) {
			// This is a declaration of a incomplete type, declare the tag.
			shared<Declaration> incDecl = CreateTagDecl(type, name, location,
                                                        nullptr, parent);
			parent->AddTag(name, incDecl);
			return incDecl;
		}
		else {
			isReference = true;

			if(SameTagType(type, previous) == false) {
				diag_->Report(Error::USE_WRONG_TAG)<<location<<*name;
				return nullptr;
			}
		}

		return previous; // Use the previous declaration.
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<EnumConstDeclaration> 
DeclarationSemantic::HandleEnumConst(shared<DeclarationContext> context, 
                                     shared<EnumDeclaration> enumDecl,
							         shared<Identifier> name, bool hasValue,
							         shared<Expression> valueExpr, 
							         LocationInfo startLocation, LocationInfo endLoc) {
	// Make sure no other declaration with the same name is in the
	// declaration context (C99:6.7.2.2.109). The 'parent' is the outermost context
	// that is either a function or the file.
	DeclarationContext* temp;
	shared<Declaration> previous = context->Find(name, &temp);

	if(previous) {
		diag_->Report(Error::REDEFINITION)<<*name;
		return nullptr;
	}

	// Check the constant declaration 
    // (especially the expression, if present and valid).
	if(hasValue && (Expression::IsInvalid(valueExpr) == false)) {
		// C99:6.7.2.2: the expression should have an evaluated value
		// that can be represented using an 'int'.
		EvaluationInfo eval = valueExpr->EvaluateAsICE(context_, true /* warn */);

		if(eval.HasFailed() || (eval.IsIntConstant() == false)) {
			// The expression is not an ICE; we still generate the declaration
            // to prevent further errors.
			diag_->Report(Error::EXPECTED_CONSTANT_NUMBER)<<*name;
		}
	}

	// Create the constant declaration if all seems OK.
	// Overflow for constants without values is handled later.
	EnumConstDeclaration* constDecl = 
            new EnumConstDeclaration(name, BasicType::GetInt(), startLocation, 
								     endLoc, enumDecl, valueExpr);
	constDecl->SetStorage(StorageType::None);
	constDecl->SetLinkage(LinkageType::None);
	return constDecl;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__int64 DeclarationSemantic::EvalueteEnumConst(shared<EnumConstDeclaration> enumConst) {
    DebugValidator::IsTrue(enumConst->HasValue());
    DebugValidator::IsFalse(Expression::IsInvalid(enumConst->ValueExpr()));

    // We check if it fits into an 'int'.
    EvaluationInfo eval = enumConst->ValueExpr()->EvaluateAsICE(context_, false /* warn */);
    LexemeIntegerType lexemeType = IntType_Int;
    const Type* exprType = BasicType::GetInt();
    
    // Check if there is overflow when representing the number as an 'int'.
    __int64 maxValue = context_->Target()->GetMaxValue(TypeKind::Int);
    __int64 exprValue = eval.Value().IntValue;

    if(exprValue > maxValue) {
        // It doesn't fit, emit a warning and continue using 'long long'.
        diag_->Report(Warning::ENUM_CONSTANT_OVERFLOW)<<*enumConst->Name();
        lexemeType = IntType_LongLong;
        exprType = BasicType::GetLongLong();
    }

    // Replace the expression with a 'NumberConstant'.
    // For now we presume the underlying type is 'int'.
    NumberInfo newValue;
    newValue.IsInteger = true;
    newValue.SetIntType(lexemeType);
    newValue.IntValue = exprValue;
    newValue.IsValid = true;
    enumConst->SetValueExpr(new NumberConstant(newValue, exprType, 
                                               enumConst->StartLocation()));
    return newValue.IntValue;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::HandleEnumList(shared<DeclarationContext> context, 
                                         shared<EnumDeclaration> enumDecl,
								         LocationInfo location) {
	// The list of constants is walked two times.
	// The first time the values of the constants are set (if not
	// specified in the source) and some statistics are collected.
	// The second time the chosen type is set for each constant.
	EnumType* enumType = const_cast<EnumType*>(enumDecl->DeclarationType());
	auto& list = enumType->Constants();

	__int64 maxValue  = -1;    // The maximum value in the list.
	__int64 minValue  =  0;    // The minimum value in the list. Only for negative.
	__int64 prevValue = -1;    // The previous value. Next = previous + 1.
	bool hasNegative  = false; // If a negative number is in the list.

	for(int i = 0; i < list.Count(); i++) {
		// If the constant has no associated value we must assign one.
		// The first value is 0, the next ones are 'previous + 1'.
		__int64 value;
		auto constDecl = list[i];
		
		if(constDecl->HasValue() && 
           (Expression::IsInvalid(constDecl->ValueExpr()) == false)) {
			value = EvalueteEnumConst(constDecl);
		}
		else {
			// Assign the next value and check for overflow.
			value = prevValue + 1;

			if(context_->Target()->IsOveflow(value, prevValue, TypeKind::Int)) {
				// It's overflow, emit a warning, truncate and continue.
				diag_->Report(Warning::ENUM_CONSTANT_OVERFLOW)<<*constDecl->Name();
				value = context_->Target()->Truncate(value, TypeKind::Int);
			}

			NumberInfo newVal;
			newVal.IsInteger = true;
			newVal.SetIntType(IntType_Int);
			newVal.IntValue = value;
			newVal.IsValid = true;
			constDecl->SetValueExpr(new NumberConstant(newVal, BasicType::GetInt(), 
													   location));
		}

		hasNegative |= value < 0;
		prevValue = value;
		maxValue = value > maxValue ? value : maxValue;
		minValue = value < minValue ? value : minValue;
	}

	// Determine the type of the enum values based on the maximum value
	// and the existence of negative values.
	const BasicType* valType = nullptr;

	if(hasNegative) {
		if((minValue >= -context_->Target()->GetMaxValue(TypeKind::Char)) &&
		   (maxValue <=  context_->Target()->GetMaxValue(TypeKind::Char))) {
			valType = BasicType::GetChar();
		}
		else if((minValue >= -context_->Target()->GetMaxValue(TypeKind::Short)) &&
				(maxValue <=  context_->Target()->GetMaxValue(TypeKind::Short))) {
			valType = BasicType::GetShort();
		}
		else valType = BasicType::GetInt();
	}
	else {
		if((unsigned __int64)maxValue <= 
		   (unsigned __int64)context_->Target()->GetMaxValue(TypeKind::UChar)) {
			valType = BasicType::GetUChar();
		}
		else if((unsigned __int64)maxValue <= 
				(unsigned __int64)context_->Target()->GetMaxValue(TypeKind::UShort)) {
			valType = BasicType::GetUShort();
		}
		else valType = BasicType::GetUInt();
	}

	// The type of the constants is also the type of the 'enum'
	// during default argument promotions.
	enumType->SetConstType(valType);
	enumDecl->SetIsDefinition(true);

	// If the determined type is 'int' we make no change
	// because all constants have this type already.
	if(valType->IsInt()) return true;

	for(int i = 0; i < list.Count(); i++) {
		shared<EnumConstDeclaration> constDecl = list[i];
		auto valueExpr = constDecl->ValueExpr().As<NumberConstant>();

		valueExpr->SetResultType(valType);
		constDecl->SetDeclType(valType);
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclarationSemantic::ReportStructDiagnostic(DiagnosticCode code, 
                                                 shared<DI> declaration) {
	// Works with anonymous struct/union too.
	if(declaration->GetName()) {
		diag_->Report(code)<<*declaration->GetName();
	}
	else diag_->Report(code)<<declaration->Location;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<FieldDeclaration> 
DeclarationSemantic::HandleFieldDeclaration(shared<DI> declaration, SpecifierInfo& info, 
									        shared<StructUnionDeclaration> parent, 
									        shared<DeclarationContext> context,
									        shared<Expression> bitfield, 
                                            LocationInfo startLocation, 
                                            LocationInfo endLoc) {
	// Obtain the declaration form the declarator and specifiers.
	// Than validate it according to the rules from C99:6.7.2
	const Type* declType = MakeType(declaration, info, context);
	if(declType == nullptr) {
		// The type is not valid; there is no reason to continue.
		return nullptr;
	}
	   
	// The resulting type should not be incomplete or a function.
	// The exception is an incomplete array that can appear only as the last field
	// of the aggregate. If this is such an array we let it through and test it
	// when the complete field list is available.
	// Variably-modified types are also not allowed (VLA's, pointers to VLA's, etc).
	if(declType->IsFunction()) {
		ReportStructDiagnostic(Error::STRUCT_FIELD_WITH_FUNCTION_TYPE, declaration);
		return nullptr;
	}
	else if(declType->IsVariable()) {
		ReportStructDiagnostic(Error::STRUCT_FIELD_VARIABLE_TYPE, declaration);
		return nullptr;
	}
	else if(declType->IsIncomplete()) {
		if(declType->IsArray()) {
			// We allow arrays for now.
		}
		else {
			ReportStructDiagnostic(Error::STRUCT_FIELD_WITH_INCOMPLETE_TYPE, declaration);
			return nullptr;
		}
	}

	// An aggregate that has a flexible array should not appear
	// in another aggregate (C99:6.7.2.1.2).
	bool hasFlexArray = false;
	if(auto temp = declType->As<StructType>()) {
		if(temp->HasFlexArray(true /* children */)) hasFlexArray = true;
	}
	else if(auto temp = declType->As<UnionType>()) {
		if(temp->HasFlexArray(true /* children */)) hasFlexArray = true;
	}

	if(hasFlexArray) {
		ReportStructDiagnostic(Error::STRUCT_FIELD_FLEX_ARRAY, declaration);
		return nullptr;
	}

	// The field must have a name (note that bitfields can be unnamed).
	shared<Identifier> name = declaration->GetName();
	if((bitfield == nullptr) && (name == nullptr)) {
		ReportStructDiagnostic(Error::STRUCT_UNNAMED_FIELD, declaration);
		return nullptr;
	}

	// If a name is present it should be unique in the aggregate.
	DeclarationContext* temp;
	if(name && context->Find(name, &temp, false /* all */)) {
		ReportStructDiagnostic(Error::STRUCT_FIELD_REDEFINITION, declaration);
		return nullptr;
	}

	// If there is no bitfield we're done. Else check the bitfield type
	// and the value of the constant expression.
	if(bitfield == nullptr) {
		FieldDeclaration* fieldDecl = 
                new FieldDeclaration(name, declType, -1 /* no bitfield */, 
								     parent, startLocation, endLoc);

		fieldDecl->SetStorage(StorageType::None);
		fieldDecl->SetLinkage(LinkageType::None);
		return fieldDecl;
	}
	else return HandleBitfield(declaration, declType, parent, context,
                               bitfield, startLocation, endLoc);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<FieldDeclaration> 
DeclarationSemantic::HandleBitfield(shared<DI> declaration, const Type* declType, 
							        shared<StructUnionDeclaration> parent,
                                    shared<DeclarationContext> context,
							        shared<Expression> bitfield, 
                                    LocationInfo startLocation, LocationInfo endLoc) {
	// The standard says that only '_Bool', 'int' and 'unsigned' should be
	// accepted as bitfield types, but both GCC and VC accept all integer types,
	// so we accept them too.
	const BasicType* basic = declType->As<BasicType>();

	if(basic) {
		// It is a basic type, but it must be an integer type.
		if(basic->Type::IsInteger() == false) {
			ReportStructDiagnostic(Error::STRUCT_BITFIELD_INVALID_TYPE, declaration);
			return nullptr;
		}
	}
	else if(declType->IsEnum() == false) {
		// The bitfield is not a basic type. An exception made by both GCC and Visual C++
		// is that 'enum' is also considered a valid type, because it's underlying
		// type is actually an integer.
		ReportStructDiagnostic(Error::STRUCT_BITFIELD_INVALID_TYPE, declaration);
		return nullptr;
	}

	// Convert and check the constant expression. If the bitfield expression is
	// invalid we continue using the value 0.
	__int64 value = 0;

	if(Expression::IsInvalid(bitfield) == false) {
		EvaluationInfo eval = bitfield->EvaluateAsICE(context_, true /* warn */);

		if(eval.IsConstant() == false) {
			// The expression is not a constant at all.
			ReportStructDiagnostic(Error::STRUCT_BITFIELD_NOT_CONSTANT, declaration);
			return nullptr;
		}
		else if(eval.IsFloatConstant()) {
			// The expression is not an ICE ('int a:2.56' not valid).
			ReportStructDiagnostic(Error::STRUCT_BITFIELD_FLOATING_VALUE, declaration);
			return nullptr;
		}

		// The value must be >= 0 and smaller than the number of bits
		// the type is represented in.
		value = eval.Value().IntValue;

		if(value < 0) {
			ReportStructDiagnostic(Error::STRUCT_BITFIELD_NEGATIVE_VALUE, declaration);
			return nullptr;
		}

		// Get the size in bits from the target.
		auto dataType = declType->IsEnum() ? 
                        declType->As<EnumType>()->ConstType() : basic;
		int maxBits = dataType->Size(context_->Target()) * 8;

		if(value > maxBits) {
			// Value too large ('int a:43' not valid).
			ReportStructDiagnostic(Error::STRUCT_BITFIELD_VALUE_TOO_LARGE, declaration);
			return nullptr;
		}
	}

	// Create the field declaration.
	FieldDeclaration* fieldDecl = 
            new FieldDeclaration(declaration->Name, declType, (int)value,
							     parent, startLocation, endLoc);

	fieldDecl->SetStorage(StorageType::None);
	fieldDecl->SetLinkage(LinkageType::None);
	return fieldDecl;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::HandleFieldList(shared<StructUnionDeclaration> declaration, 
                                          shared<DeclarationContext> context,
								          LocationInfo startLocation) {
	// C99:6.7.2.1.7: the struct/union should contain at least one named member.
	int namedCt = 0;
	auto fields = declaration->DeclarationType()->Fields();
	bool invalid = false;

	for(int i = 0; i < fields.Count(); i++) {
		shared<FieldDeclaration> field = fields[i];

		if(field->DeclarationType()->IsIncomplete()) {
			// We found a flexible array. It is valid only if it's the last
			// field of the aggregate (C99:6.7.2.1.16).
			if(i != (fields.Count() - 1)) {
				diag_->Report(Error::STRUCT_FLEX_ARRAY_NOT_LAST)<<*field->Name();
				invalid = true; // Continue even if this was an error.
			}

			// A flexible array is not allowed if no named field
			// can be found before it.
			if(namedCt == 0) {
				diag_->Report(Error::STRUCT_FLEX_ARRAY_NOT_NAMED)<<*field->Name();
				invalid = true;
			}
		}

		if(field->Name()) {
			namedCt++; // Count the named fields.
		}
	}

	// Test the number of named fields.
	if(namedCt == 0) {
		diag_->Report(Error::STRUCT_NO_NAMED_FIELD)<<declaration->StartLocation();
		invalid = true;
	}

	// See if there is a 'pack' attribute activated ('#pragma pack' for MS extensions).
	// If it is we need to create a new attribute with the value so that
	// 'LayoutBuilder' correctly computes the alignment of the fields.
	int packValue = context_->Options().PackValue();

	if(packValue != context_->Options().DefaultPackValue()) {
		declaration->AddAttribute(new PackAttribute(packValue));
	}

	// Finalize the declaration by making it a definition (complete type).
	declaration->SetIsDefinition(true);
	return invalid == false;
}

} // namespace Parsing