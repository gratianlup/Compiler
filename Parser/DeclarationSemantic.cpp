// DeclarationSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements 'DeclarationSemantic'.
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

bool DeclarationSemantic::CheckRedefinition(StorageType storage, 
                                            shared<DeclarationContext> context,
									        shared<Identifier> name) {
	// This could be a redefinition, or the two declarations need to be combined.
	// If this is block scope and 'static'/'extern' is not found, it's a redefinition.
	if(context->IsBlockScope() && 
	   ((storage == StorageType::Extern) || (storage == StorageType::Static)) == false) {
		diag_->Report(Error::REDEFINITION)<<*name;
		return true;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> 
DeclarationSemantic::HandleDeclaratorBegin(shared<DI> declaration, SpecifierInfo& info,
									       shared<DeclarationContext> context,
                                           bool& isRedeclaration) {
	// Get the type from the declarator and the specifiers.
	bool handledRedef = false;
	isRedeclaration = false; // Presume it's not a redeclaration.
	shared<Declaration> declObj;

	// 1. ###############################################################################
	// Obtain the type after all declarator components have been applied.
	const Type* declType = MakeType(declaration, info, context);

	if(declType == nullptr) {
		// The type couldn't be created, give up.
		return nullptr;
	}

	// 2. ###############################################################################
	// Declarators without names are allowed only at function-prototype scope.
	shared<Identifier> name = declaration->GetName();

	if((name == nullptr) && (context->IsFunctProtoScope() == false)) {
		diag_->Report(Error::DECLARATION_EXPECTED_IDENTIFIER)<<declaration->Location;
		return nullptr;
	}

	// See if there is a previous declaration with the same name.
	Declaration* previous;
	DeclarationContext* temp;

	if(name) {
		// The name should not be '__func__' (it's used as an implicit declaration
		// of a string having the name of the current function).
		if(*name == "__func__") {
			diag_->Report(Error::FUNC_RESERVED_IDENTIFIER)<<*name;
			return nullptr;
		}

		bool scanAll = info.Extern || info.Static;
		previous = context->Find(name, &temp, scanAll);

		// Always use the last declaration.
		if(previous) {
			isRedeclaration = true;
			previous = previous->LastDeclaration();
		}
	}

	// 3. ###############################################################################
	// Figure out the linkage and storage type.
	LinkageType linkage;
	StorageType storage;

	if(HandleLinkage(declaration, info, declType, previous, context, 
                     storage, linkage) == false) {
		// Something about the linkage isn't right.
		return nullptr;
	}

	// 4. ###############################################################################
	// Check the types that are variable (are either a VLA or something like
	// a pointer to VLA). Variable types are allowed only at block level
	// and at prototype-scope level (C99:6.7.5.2.2).
	if(declType->IsVariable()) {
		if((context->IsBlockScope() == false) && 
           (context->IsFunctProtoScope() == false) &&
		   (context->IsFunctionScope() == false)) {
			diag_->Report(Error::VLA_INVALID_SCOPE)<<declaration->Location;
			return nullptr;
		}
		else if(declType->IsObject()) {
			// extern/static declaration with VLA is invalid.
			if(storage == StorageType::Extern) {
				if(name) diag_->Report(Error::VLA_EXTERN_STORAGE_SPECIFIER)<<*name;
				else diag_->Report(Error::VLA_EXTERN_STORAGE_SPECIFIER)<<declaration->Location;
			}
			else if(storage == StorageType::Static) {
				if(declaration->Name) diag_->Report(Error::VLA_STATIC_STORAGE_SPECIFIER)<<*name;
				else diag_->Report(Error::VLA_STATIC_STORAGE_SPECIFIER)<<declaration->Location;
			}
		}
	}

	// 5. ###############################################################################
	// Check if this is an invalid redefinition. Declarations at block scope are
	// handled differently than those at file scope. For example:
	// Block scope: int a; int a; - invalid redefinition
	// File scope: int a; int a; - valid, because it's a tentative declaration.
	bool invalidRedecl = previous && CheckRedefinition(storage, context, name);

	// 6. ###############################################################################
	// Based on the type call the appropriate handling method.
	// When dispatching use the unqualified type to determine the category.
	const Type* unqualDeclType = declType->WithoutQualifiers();

	if(info.Typedef) {
		declObj = HandleTypedef(declaration, info, declType, context, previous);
		handledRedef = true;
	}
	else if(unqualDeclType->IsArray()) {
		const ArrayType* arrayType = declType->WithoutQualifiers()->As<ArrayType>();
		declObj = HandleArrayDeclarator(declaration, info, arrayType, context, 
                                        previous, invalidRedecl);
		handledRedef = true;
	}
	else if(unqualDeclType->IsFunction()) {
		const FunctionType* functionType = declType->As<FunctionType>();
		declObj = HandleFunctionDeclarator(declaration, info, functionType, 
                                           context, previous, invalidRedecl);
		handledRedef = true;
	}
	else if(invalidRedecl == false) {
		// For other types we create here the declaration.
		declObj = new VariableDeclaration(declaration->GetName(),
                                          declType, declaration->Location, 
								          declaration->Location, nullptr /* initializer */);
	}

	// Give up if we failed to create the declaration.
	if(declObj == nullptr) {
		return nullptr;
	}

	// 'inline' can be associated only with functions.
	if(info.Inline && (unqualDeclType->IsFunction() == false)) {
		if(declaration->Name) diag_->Report(Error::INLINE_NOT_ON_FUNCTION)<<*name;
		else diag_->Report(Error::INLINE_NOT_ON_FUNCTION)<<declaration->Location;
		return nullptr;
	}
	
	// 'restrict' can be associated only with pointers.
	if(info.Qual.HasRestrict() && (unqualDeclType->IsPointer() == false)) {
		if(declaration->Name) diag_->Report(Error::RESTRICT_EXPECTED_POINTER)<<*name;
		else diag_->Report(Error::RESTRICT_EXPECTED_POINTER)<<declaration->Location;
		return nullptr;
	}

	// 7. ###############################################################################
	// Check if a previous declaration is visible in the current scope.
	// This step is only for types other than typedef/array/function.
	if((handledRedef == false) && previous) {
		DebugValidator::IsFalse(unqualDeclType->IsTypedef());
		DebugValidator::IsFalse(unqualDeclType->IsArray());
		DebugValidator::IsFalse(unqualDeclType->IsFunction());
		
		// It should be a variable declaration, because all other cases
		// where already treated above.
		if(auto temp = previous->As<VariableDeclaration>()) {
			const Type* combType = typeComb_.Combine(declType, temp->DeclarationType());

			if(combType) {
				declObj->SetDeclType(combType);

				// Link the declarations.
				previous->SetNext(declObj);
				declObj->SetPrevious(previous);
				handledRedef = true;
			}
		}
		
		// Report an invalid redefinition (types not the same).
		if(handledRedef == false) {
			diag_->Report(Error::REDEFINITION)<<*name;
			return nullptr;
		}
	}

	// 8. ###############################################################################
	// Apply the linkage and storage type to the declaration.
	declObj->SetLinkage(linkage);
	declObj->SetStorage(storage);

	// Apply the attributes (and combine them with the ones from the previous
	// declaration, if it's the case).
	ApplyAttributes(declObj, info, previous);
	return declObj;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> 
DeclarationSemantic::HandleDeclaratorEnd(shared<Declaration> declaration, 
                                         SpecifierInfo& info, 
								         shared<DeclarationContext> context) {
	// If the declared object has no linkage and it's at block scope
	// it should be complete (note that incomplete arrays can be completed by initializers).
	// If the incomplete type is part of a 'typedef' it's OK.
	// C99:6.9.2.3: if this is a 'static' tentative definition (with no initializer),
	// the type should not be incomplete.
	bool isInvalid = false;

	if(declaration->DeclarationType()->IsIncomplete()) {
		// Is it a variable declaration with incomplete type?
		if((declaration->HasLinkage() == false) && (info.Typedef == false)) {
			isInvalid = true;
		}
		// Is it a 'static' tentative definition without initializer?
		else if((declaration->Linkage() == LinkageType::Internal) && 
                 declaration->IsVariableDecl() &&
			    (declaration->As<VariableDeclaration>()->Initializer() == nullptr)) {
			isInvalid = true;
		}
	}

	// If something is invalid report the error.
	if(isInvalid) {
		if(declaration->DeclarationType()->IsArray()) {
			diag_->Report(Error::DECLARATION_INCOMPLETE_ARRAY_BLOCK_SCOPE)<<*declaration->Name();
		}
		else diag_->Report(Error::DECLARATION_INCOMPLETE_BLOCK_SCOPE)<<*declaration->Name();

		return nullptr; // Give up.
	}

	// VLA's with initializer are invalid (C99:6.7.8.3).
	// Note that pointers to VLAs are accepted here.
	if((info.Typedef == false) && declaration->DeclarationType()->IsArray()) {
		if(declaration->DeclarationType()->IsVariable() && 
           declaration->As<VariableDeclaration>()->Initializer()) {
			diag_->Report(Error::VLA_WITH_INITIALIZER)<<*declaration->Name();
		}
	}

	return declaration;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclarationSemantic::ApplyAttributes(Declaration* declaration, SpecifierInfo& info, 
								          Declaration* previous) {
	// Copy all attributes from the declarator to the declaration.
	// If a previous declaration is available the attributes are combined.
	auto& attrList = info.Attributes;

	for(int i = 0; i < attrList.Count(); i++) {
		declaration->AddAttribute(attrList[i]);
	}

	// We're done if there is no previous declaration.
	if(previous == nullptr) {
		return;
	}
	
	for(int i = 0; i < previous->AttributeCount(); i++) {
		auto previousAttr = previous->GetAttribute(i);

		if(auto previousAlignAttr = previousAttr->As<AlignmentAttribute>()) {
			auto alignAttr = declaration->AttributeAs<AlignmentAttribute>();

			if(alignAttr) {
				// If this declaration has an alignment attribute, and the previous
				// one has too, we choose the maximum alignment.
				int max = alignAttr->Value() > previousAlignAttr->Value() ?
						  alignAttr->Value() : previousAlignAttr->Value();

				alignAttr->SetValue(max);
			}
			else declaration->AddAttribute(alignAttr);
		}
		else if(previousAttr->Is<PackAttribute>() == false) {
			// Previous 'pack' attributes are ignored.
			declaration->AddAttribute(previousAttr);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> 
DeclarationSemantic::HandleDeclaratorInitializer(shared<Declaration> declaration, 
                                                 SpecifierInfo& info, 
                                                 shared<InitInfo> initializer, 
                                                 shared<DeclarationContext> context) {
	// Check and apply the initializer (if any).
	// If the previous declaration has an initializer this one is not valid.
	// int a; int a = 5; int a = 7;
	//        valid      invalid, 'a' defined already
	Declaration* previous = declaration->Previous();

	if(previous && previous->GetDefinition()) {
        if(auto variableDecl = previous->As<VariableDeclaration>()) {
            if(variableDecl->Initializer()) {
		        diag_->Report(Error::DECLARATOPN_REDEFINED_INITIALIZER)<<*declaration->Name();
		        return nullptr;
            }
        }
	}

	if(HandleInitializer(declaration, info, declaration->DeclarationType(), 
                         initializer, context) == false) {
		// Failed to apply the initializer.
		return nullptr;
	}
	else return declaration;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> 
DeclarationSemantic::HandleTypedef(shared<DI> declaration, SpecifierInfo& info, 
                                   const Type* type, shared<DeclarationContext> context, 
                                   Declaration* previous) {
	bool isInvalid = false;
	shared<Identifier> name = declaration->GetName();
	TypedefDeclaration* prevTdef = nullptr;

	// The 'typedef' should have a name.
	if(name == nullptr) {
		isInvalid = true;
		diag_->Report(Error::TYPEDEF_NO_NAME)<<declaration->Location;
	}

	// Variable-length arrays are allowed in 'typedef' only at
	// the block-scope (in functions).
	if(type->IsArray() && type->As<ArrayType>()->IsVariable() &&
	   (context->IsBlockScope() == false) && 
       (context->IsFunctionScope() == false)) {
		isInvalid = true;
		diag_->Report(Error::TYPEDEF_VLA_FUNCTION_SCOPE)<<declaration->Location;
	}

	// See if another 'typedef' with the same name is visible
	// in this scope. The new one is allowed only if they name the same type.
	if(previous) {
		// The previous declaration should also be a typedef. 
		// If it is, it should point the same type.
		previous = previous->LastDeclaration(); // Always use the last declaration.

		if(previous->IsTypedefDecl() == false) {
			diag_->Report(Error::REDEFINITION)<<*name;
			isInvalid = true;
		}
		else {
			prevTdef = previous->As<TypedefDeclaration>();

			if(prevTdef->DeclarationType()->Inner()->Equals(type->InnerType())) {
				// The typedefs will be linked.
			}
			else {
				prevTdef = nullptr;
				isInvalid = true;
				diag_->Report(Error::TYPEDEF_INVALID_REDEFINITION)<<*name;
			}
		}
	}

	if(isInvalid) {
		return nullptr;
	}
	
	// Make the type and the declaration. If there is a valid previous
	// declaration take the type from there.
	const TypedefType* tdefType;

	if(prevTdef) tdefType = prevTdef->DeclarationType();
	else tdefType = types_->GetTypedef(type);

	shared<TypedefDeclaration> tdefDecl = 
            new TypedefDeclaration(name, tdefType, declaration->Location, 
							       declaration->Location);
	if(prevTdef) {
		// Link the declarations.
		tdefDecl->SetPrevious(prevTdef->LastDeclaration());
		prevTdef->LastDeclaration()->SetNext(tdefDecl);
	}

	return tdefDecl;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> 
DeclarationSemantic::HandleArrayDeclarator(shared<DI> declaration, SpecifierInfo& info, 
									       const ArrayType* arrayType, 
                                           shared<DeclarationContext> context,
									       Declaration* previous, bool isInvalid) {
	shared<Identifier> name = declaration->GetName();

	// Variable-length arrays should not have a 'static' 
	// or 'extern' specifier (C99:6.7.5.3.10).
	if(arrayType->IsVariable() && (info.Extern || info.Static)) {
		isInvalid = true;

		if(info.Extern) {
			if(name) diag_->Report(Error::VLA_EXTERN_STORAGE_SPECIFIER)<<*name;
			else diag_->Report(Error::VLA_EXTERN_STORAGE_SPECIFIER)<<declaration->Location;
		}
		else {
			if(name) diag_->Report(Error::VLA_STATIC_STORAGE_SPECIFIER)<<*name;
			else diag_->Report(Error::VLA_STATIC_STORAGE_SPECIFIER)<<declaration->Location;
		}
	}

	// If this is a invalid redefinition stop here.
	if(isInvalid) {
		return nullptr;
	}

	// ##################################################################################
	// Check if a previous declaration of the array is
	// in the current scope. If yes combine the element types.
	if(previous == nullptr) {
		// No previous declaration was found.
		return new VariableDeclaration(name, arrayType, declaration->Location, 
                                       declaration->Location, nullptr);
	}

	// It should be a variable that declares an array.
	previous = previous->LastDeclaration(); // Always use the last declaration.
	VariableDeclaration* prevVar = previous->As<VariableDeclaration>();

	if((prevVar) && prevVar->DeclarationType()->IsArray()) {
		const Type* combType = typeComb_.Combine(arrayType, prevVar->DeclarationType());

		if(combType) {
			// Link the declarations and return the new one.
			shared<VariableDeclaration> newVar = 
                    new VariableDeclaration(name, combType, declaration->Location, 
						                    declaration->Location, nullptr);
			prevVar->SetNext(newVar);
			newVar->SetPrevious(prevVar);
			return newVar;
		}
	}

	// If we reached this point this is an invalid redefinition.
	// Could be something like: int a[3]; char a[3]/int a[4];
	diag_->Report(Error::REDEFINITION)<<*name;
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Declaration> 
DeclarationSemantic::HandleFunctionDeclarator(shared<DI> declaration, SpecifierInfo& info,
									          const FunctionType* functionType, 
                                              shared<DeclarationContext> context,
									          Declaration* previous, bool isInvalid) {
	shared<Identifier> name = declaration->GetName();

	// Declarators for functions at block-scope should have no other
	// storage-class specifier than 'extern' (C99:6.7.1.5).
	if(context->IsBlockScope() && 
       (info.Static ||  info.Auto  || info.Register || info.Static)) {
		if(name!= nullptr) diag_->Report(Error::PARAMETER_INVALID_STORAGE_SPECIFIER)<<*name;
		else diag_->Report(Error::PARAMETER_INVALID_STORAGE_SPECIFIER)<<declaration->Location;
	}

	// 'static' is allowed only at file scope (C99:6.2.2.22).
	if(info.Static && (context->IsFileScope() == false)) {
		if(name) diag_->Report(Error::FUNCTION_STATIC_IN_DECLARATION)<<*name;
		else diag_->Report(Error::FUNCTION_STATIC_IN_DECLARATION)<<declaration->Location;
	}

	// If this is known to be an invalid redefinition stop here.
	if(isInvalid) {
		return nullptr;
	}

	// ##################################################################################
	// Check if a previous declaration with the same name is visible
	// in the same scope. If true, try to merge the declarations.
	FunctionDeclaration* prevFunctDecl = nullptr;

	if(previous) {
		prevFunctDecl = previous->As<FunctionDeclaration>();

		if(prevFunctDecl == nullptr) {
			// We are redefining something that is not a function, report it.
			diag_->Report(Error::REDEFINITION)<<*name;
			return nullptr;
		}

		functionType = CombineFunctionDeclarations(functionType, prevFunctDecl);

		if(functionType == nullptr) {
			// Failed to combine. Diagnostic already emitted.
			return nullptr;
		}
	}

	// Create the declaration and it's parameters. Because this is a declaration
	// (or definition) we need to preserve the parameter names. The names are found
	// in the last 'DeclaratorInfo' helper object.
	shared<FunctionDeclaration> functDecl = 
            new FunctionDeclaration(name, functionType, declaration->Location, 
                                    declaration->Location, info.Static, info.Inline);
	DI* functInfo = declaration;

	while(functInfo->Next) {
		functInfo = functInfo->Next;
	}

	DebugValidator::IsTrue(functInfo->Type == DI::Function);
	auto& parameters = functDecl->Parameters();
	auto& paramTypes = functionType->Parameters();
	auto& paramInfos = functInfo->Parameters;

	// Create a new variable declaration for each parameter.
	for(int i = 0; i < functionType->ParameterCount(); i++) {
		// If there is a previous declaration we can be only in two situations:
		// 1. 'void a(); void a(int x);' - a function with prototype has been added
		// 2. 'void a(int x); void a();' - a function without prototype has been added
		// In situation (1) we take the parameters from the new declaration,
		// while in the second one we take them from the previous.
		const Type* parameterType;
		shared<Identifier> parameterName;
		LocationInfo parameterLocation;
		StorageType paramStorage;

		if(i < paramInfos.Count()) {
			parameterType = paramTypes[i];
			auto paramInfo = paramInfos[i];
			parameterName = paramInfo->HasName() ? 
                            paramInfo->Declarator->GetName() : nullptr;

			parameterLocation = paramInfo->Location;
			paramStorage = paramInfo->Info.Register ? 
                           StorageType::Register : StorageType::Auto;
		}
		else {
			DebugValidator::IsNotNull(prevFunctDecl);
			auto prevParam = prevFunctDecl->Parameters()[i];
			
            parameterType = prevParam->DeclarationType();
			parameterName = prevParam->Name();

			parameterLocation = declaration->Location;
			paramStorage = prevParam->Storage();
		}

		// Create the parameter variable declaration.
		shared<VariableDeclaration> paramDecl = 
                new VariableDeclaration(parameterName, parameterType, 
                                        parameterLocation, parameterLocation, nullptr);
		paramDecl->SetStorage(paramStorage);
		paramDecl->SetLinkage(LinkageType::None);

		// Add the parameter to the declaration.
		parameters.Add(paramDecl);
	}

	// Check the declaration if we're declaring the 'main' function.
	// We continue even if the declaration is invalid.
	if(functDecl->IsMain()) {
		ValidateMain(declaration, info, functionType, context);
	}

	// Link with the previous declaration, if one exists.
	if(previous == nullptr) return functDecl;
	else {
		// If the other declaration is 'inline' this is 'inline' too.
		functDecl->SetIsInline(previous->As<FunctionDeclaration>()->IsInline());

		// Link the declarations.
		previous->SetNext(functDecl);
		functDecl->SetPrevious(previous);
		return functDecl;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::ValidateMain(shared<DI> declaration, SpecifierInfo& info, 
								       const FunctionType* functionType, 
                                       shared<DeclarationContext> context) {
	// Two prototypes are allowed in the C99 standard, but both gcc and VC allow
	// main returning 'void', so we do it too.
	// Validate the return type first (it should be 'int', 
    // and 'void' as an extension).
	auto returnType = functionType->ReturnType();

	if(auto temp = returnType->As<BasicType>()) {
		if((temp->IsInt() == false) && (temp->IsVoid() == false)) {
			// Warn that the return doesn't conform to the standard.
			diag_->Report(Warning::MAIN_RETURN_NOT_STANDARD)<<declaration->Location<<
						  TypeString(temp).ToString();
		}
	}
	else {
		// Any other types (like floating, struct) don't conform to the standard.
		diag_->Report(Warning::MAIN_RETURN_NOT_STANDARD)<<declaration->Location<<
					  TypeString(temp).ToString();
	}

	// Validate the parameters. The allowed forms are (with their representation):
	// 1. main(void)                   -> main()
	// 2. main(int argc, char* argv[]) -> main(int, char**)
	if(functionType->ParameterCount() == 2) {
		// Check that the parameters have the right type.
		// We work with the unqualified types of the parameters.
		auto parameter1 = functionType->Parameters()[0]->WithoutQualifiers();
		auto parameter2 = functionType->Parameters()[1]->WithoutQualifiers();
		bool parameter1Invalid = true;
		bool parameter2Invalid = true;

		// Parameter 1 (int).
		if(auto parameter1Basic = parameter1->As<BasicType>()) {
			parameter1Invalid = parameter1Basic->IsInt() == false;
		}

		// Parameter 2 (char**).
		if(auto parameter2Ptr = parameter2->As<PointerType>()) {
			if(auto pointee = parameter2Ptr->PointeeType()->WithoutQualifiers()
                                                          ->As<PointerType>()) {
				if(auto pBasic = pointee->PointeeType()->WithoutQualifiers()
                                                       ->As<BasicType>()) {
					parameter2Invalid = pBasic->IsChar() == false;
				}
			}
		}
		
		// Warn if the parameters don't conform to the standard.
		if(parameter1Invalid) {
			diag_->Report(Warning::MAIN_INVALID_PARAMETER_TYPE)<<
                          declaration->Location<<0<<TypeString(parameter1).ToString();
		}

		if(parameter2Invalid) {
			diag_->Report(Warning::MAIN_INVALID_PARAMETER_TYPE)<<
                          declaration->Location<<1<<TypeString(parameter2).ToString();
		}
	}
	else if(functionType->ParameterCount() != 0) {
		// Warn about the incorrect number of parameters.
		diag_->Report(Warning::MAIN_INVALID_PARAMETER_NUMBER)<<declaration->Location;
	}

	// C99:6.7.4.4: 'inline' should not appear in the declaration of 'main'.
	if(declaration->Info.Inline) {
		diag_->Report(Error::MAIN_DECLARATION_INLINE_INVALID)<<declaration->Location;
		return false;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::HandleInitializer(shared<Declaration> declaration, 
                                            SpecifierInfo& info, const Type* type, 
                                            shared<InitInfo> initializer,
									        shared<DeclarationContext> context) {
	// Initializers applied on typedefs are not valid
	// (something like 'typedef int A[] = {1};').
	if(info.Typedef) {
		diag_->Report(Error::INITIALIZER_FOR_TYPEDEF)<<*declaration->Name();
		return false;
	}

	// We use the unqualified type for initialization.
	type = type->WithoutQualifiers();

	// C99:6.7.8.3: The type should be an object and not a VLA.
	if(type->IsFunction()) {
		diag_->Report(Error::INITIALIZER_FOR_FUNCTION)<<*declaration->Name();
		return false;
	}

	if(type->IsArray() && type->As<ArrayType>()->IsVariable()) {
		diag_->Report(Error::INITIALIZER_FOR_VLA)<<*declaration->Name();
		return false;
	}

	// C99:6.9.1.6: the function parameters shouldn't have initializers.
	if(context->IsFunctProtoScope()) {
		diag_->Report(Error::PARAMETER_INITIALIZER_INVALID)<<*declaration->Name();
		return false;
	}

	// C99.6.7.8.16: The initializer for a struct/union should be surrounded by braces.
	// An exception is for block declaration where something like
	// 'struct A value = (struct A) {1,2};' is valid (it's a compound statement).
	if((type->IsStruct() || type->IsUnion()) && 
	   (context->IsBlockScope() == false) && 
       (context->IsFunctionScope() == false) &&
       (initializer->Type != InitInfo::InitList)) {
		diag_->Report(Error::INITIALIZER_FOR_STRUCT_MISSING_BRACES)<<*declaration->Name();
		return false;
	}

	shared<Expression> initExpr;
	shared<InitContext> initCtx = new InitContext(0, type, initializer, 
                                                  declaration->StartLocation());
	initExpr = MakeInitializer(type, initCtx);
	
	if(Expression::IsInvalid(initExpr)) {
		// The initializer was not valid.
		return false;
	}

	// If the type is an incomplete array it is completed by the initializer.
	const Type* newType = CompleteWithInitializer(type, initExpr);

	if(newType) {
		declaration->SetDeclType(newType);
	}
	
	// C99:6.7.8.4: if the declaration is 'static' the initializer should contain 
	// only constant expressions. But it seems that both gcc and VC++ apply this
	// for 'external' declarations too.
	if(declaration->HasLinkage() && (AllConstant(initExpr) == false)) {
		diag_->Report(Error::INITIALIZER_FOR_LINKED_NOT_CONSTANT)<<*declaration->Name();
		return false;
	}

	// All is well, associate the initializer expression with the declaration.
	// Make the declaration a definition.
	declaration->SetIsDefinition(true);
	declaration->As<VariableDeclaration>()->SetInitializer(initExpr);
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* DeclarationSemantic::CompleteWithInitializer(const Type* type,
												         shared<Expression> initExpr) {
	// The initializer can be either a 'InitializerListExpression' or a 'StringConstant'.
	// 'int a[] = {1,2,3};' -> int a[3]
	// 'char a[] = "abc";'  -> char a[4] (one char for the null terminator).
	if(type->IsArray() && type->IsIncomplete()) {
		__int64 size = 0;

		if(auto temp = initExpr->As<InitializerListExpression>()) {
			if(temp->Count() == 1) {
				if(auto stringConst = temp->InitList()[0]->As<StringConstant>()) {
					size = stringConst->Value().Value.Length() /* includes null-terminator */;
				}
				else size = 1; // Array of size 1.
			}
			else size = temp->Count();
		}
		else if(auto stringConst = initExpr->As<StringConstant>()) {
			size = stringConst->Value().Value.Length() /* includes null-terminator */;
		}
		else {
			// Any other combination should not be possible if the initializer is valid.
			DebugValidator::Unreachable();
		}

		// Replace the old type.
		return types_->GetFinalizedArray(type->As<ArrayType>(), size);
	}
	else return nullptr;
}								   

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::HandleLinkage(shared<DI> declaration, SpecifierInfo& info, 
								        const Type* type, Declaration* previous,
								        shared<DeclarationContext> context, 
                                        StorageType& storage, LinkageType& linkage) {
	// First handle common cases.
	// C99:6.9.2: 'auto' and 'register' should not appear in an external declaration.
	storage = StorageFromSpec(info);
	shared<Identifier> name = declaration->GetName();
	bool handled = false;

	if(context->IsFileScope() && (info.Auto || info.Register)) {
		if(info.Auto) diag_->Report(Error::DECLARATION_AUTO_FILE_SCOPE)<<declaration->Location;
		else diag_->Report(Error::DECLARATION_REGISTER_FILE_SCOPE)<<declaration->Location;

		return false;
	}

	if(storage == StorageType::Extern) {
		// If a previous declaration is visible and it's linkage is internal
		// or external, the linkage of the new one is the same.
		// Else, the linkage is external (C99:6.2.2.4).
		if(previous && previous->HasLinkage()) {
			linkage = previous->Linkage();
		}
		else linkage = LinkageType::External;
		return true;
	}
	
	// C99:6.2.2.3: an identifier declared 'static' has internal linkage.
	if(info.Static) {
		linkage = LinkageType::Internal;
		handled = true;
	}

	// Handle objects and functions separately.
	if((handled == false) && type->IsFunction()) {
		// C99:6.2.2:5: if a function has no storage specifier it's linkage
		// is determined as if it were 'extern'.
		if(storage == StorageType::None) {
			storage = StorageType::Extern;

			if(previous && previous->HasLinkage()) {
				linkage = previous->Linkage();
			}
			else linkage = LinkageType::External;
		}
	}
	else if((handled == false) && type->IsObject()) {
		// C99:6.2.2:5: if no linkage is provided and the declaration
		// is at file scope, the linkage becomes external.
		if((storage == StorageType::Extern) ||
			(storage == StorageType::None) && context->IsFileScope()) {
			linkage = LinkageType::External;
		}
		else if(storage == StorageType::Static) {
			linkage = LinkageType::Internal;
		}
		else {
			// Objects without a storage specifier declared at block level
			// have 'auto' storage and no linkage.
			storage = StorageType::Auto;
			linkage = LinkageType::None;
		}
	}
	else if(handled == false) {
		// C99:6.2.2.6: any other type has no linkage.
		linkage = LinkageType::None;
	}

	// A static declaration should not follow one that is not static.
	// This also handled 'static' after 'extern' (C99:6.2.2.7).
	// 'extern' after 'static' is OK because this means the new
	// declaration inherits the specifier (static in this case).
	if(previous) {
		if((linkage == LinkageType::Internal) && 
           (previous->Linkage() != LinkageType::Internal)) {
			// Something like 'extern int a; static int a;' at file scope.
			diag_->Report(Error::FUNCTION_STATIC_AFTER_NON_STATIC)<<*name;
			return false;
		}
		else if((linkage == LinkageType::External) && 
                (previous->Linkage() != LinkageType::External)) {
			// Something like 'static int a; int a' at file scope.
			diag_->Report(Error::FUNCTION_STATIC_AFTER_NON_STATIC)<<*name;
			return false;
		}
	}
	
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const FunctionType*
DeclarationSemantic::CombineFunctionDeclarations(const FunctionType* firstType, 
										         const FunctionDeclaration* previous) {
	const FunctionType* secondType = previous->DeclarationType();

	// If a definition for the function already exist
	// the declaration is no longer valid.
	if(previous->HasDefinition()) {
		diag_->Report(Error::FUNCTION_DECLARATION_AFTER_DEFINITION)<<*previous->Name();
		return false;
	}
	
	// Combine the function types. Emit appropriate diagnostics on failure.
	FunctionCombineResult result;
	const Type* combined = typeComb_.CombineFunctions(firstType, secondType, &result);

	if(combined == nullptr) {
		switch(result) {
			case FunctionCombineResult::IncompatibleReturn: {
				diag_->Report(Error::FUNCTION_CONFLICTING_RETURN_TYPES)<<*previous->Name();
				break;
			}
			case FunctionCombineResult::ParamNumber: {
				diag_->Report(Error::FUNCTION_PARAMETER_NUMBER_MISMATCH)<<*previous->Name();
				break;
			}
			case FunctionCombineResult::ParamsIncompatible: {
				diag_->Report(Error::REDEFINITION)<<*previous->Name();
				break;
			}
			case FunctionCombineResult::Varargs: {
				diag_->Report(Error::FUNCTION_VARARG_MISMATCH)<<*previous->Name();
				break;
			}
			case FunctionCombineResult::Promotion: {
				diag_->Report(Error::FUNCTION_INVALID_PROMOTION)<<*previous->Name();
				break;
			}
		}
	}

	return static_cast<const FunctionType*>(combined);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* DeclarationSemantic::HandleTypename(shared<DI> declaration, SpecifierInfo& info,
							                    shared<DeclarationContext> context) {
	// Create the type for the specified info.
	return MakeType(declaration, info, context);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::HandleFunctionDefinitionBegin(shared<Declaration> declaration, 
                                                        SpecifierInfo& info, 
												        shared<DeclarationContext> context) {
	// The type of the declarator should be a function.
	if(declaration->DeclarationType()->IsFunction() == false) {
		diag_->Report(Error::FUNCTION_DEFINITION_NOT_FUNCTION)<<*declaration->Name();
		return false;
	}

	// A definition can be made only at the top-level (file) scope.
	if(context->Parent()->IsFileScope() == false) {
		diag_->Report(Error::FUNCT_DEFINITION_BLOCK_SCOPE)<<*declaration->Name();
		return false;
	}

	// The type of the function should not originate from a 'typedef'
	// ('typedef int F(); F f() {...}' not allowed, see C99:6.9.1.141).
	if(info.TypedefT && info.TypedefT->IsFunction()) {
		diag_->Report(Error::FUNCTION_DEFINITION_FROM_TYPEDEF)<<*declaration->Name();
		return false;
	}

	// All parameters should be named (C99:6.9.1.5). Note that 'void' can be unnamed,
	// but if it was a parameter it doesn't appear in our list anyway.
	// Each parameter is placed into the function context.
	shared<FunctionDeclaration> functDecl = declaration.As<FunctionDeclaration>();
	auto& parameters = functDecl->Parameters();
	bool isInvalid = false;

	for(int i = 0; i < parameters.Count(); i++) {
		if(parameters[i]->Name() == nullptr) {
			// Continue so we can check all parameters.
			isInvalid = true;
			diag_->Report(Error::FUNCT_DEFINITION_PARAMETER_UNNAMED)<<
						  parameters[i]->StartLocation();
		}
		else if(isInvalid == false) {
			context->Add(parameters[i]->Name(), parameters[i]);
		}
	}
	
	if(isInvalid) {
		return false;
	}

	// The returned type should not be incomplete (C99:6.9.1.3);
	// the only exception is 'void'.
	if(functDecl->DeclarationType()->ReturnType()->IsIncomplete() &&
	   (functDecl->DeclarationType()->IsVoid() == false)) {
		diag_->Report(Error::FUNCTION_RETURN_INCOMPLETE_TYPE)<<*declaration->Name();
	}

	// Place the function declaration in the context now, else recursive calls 
	// would not be possible; Make this the active function (for 'return').
	context->Parent()->Add(declaration->Name(), declaration);
	Holder()->GetStatementSemantic()->SetActiveFunction(functDecl);
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::HandleFunctionDefinitionEnd(shared<Declaration> declaration, 
											          shared<Statement> bodyStatement,
											          shared<DeclarationContext> context) {
	FunctionDeclaration* functDecl = declaration->As<FunctionDeclaration>();
	bool isInvalid = false;

	// Don't allow a function that has a return value to have no 'return' statement.
	// Note that we emit only a warning, because both gcc and VC do so.
	// C99: if 'return' is not found in the 'main' function definition
	// 'return 0;' is added by default (this prevents returning some undefined value).
	auto returnType = functDecl->DeclarationType()->ReturnType();

	if((returnType->IsVoid() == false) && 
       (Holder()->GetStatementSemantic()->ReturnCount() == 0)) {
		// We add 'return 0;' to 'main' if the return type corresponds to the standard.
		if(functDecl->IsMain() && returnType->IsInteger()) {
			// In this case we don't warn.
			CompoundStatement* functBody = bodyStatement->As<CompoundStatement>();
			shared<NumberConstant> zero = NumberConstant::FromInteger(0, BasicType::GetInt());

			shared<ReturnStatement> returnStatement = new ReturnStatement(zero, LocationInfo());
			functBody->Children().Add(returnStatement);
		}
		else diag_->Report(Warning::FUNCTION_EXPECTED_RETURN)<<*declaration->Name();
	}

	// Check if there are 'goto' statements to inexistent labels.
	auto& statementList = bodyStatement->As<CompoundStatement>()->Children();

	for(int i = 0; i < statementList.Count(); i++) {
		if(auto label = statementList[i]->As<LabelStatement>()) {
			if(label->IsReference()) {
				// This label was not found, report.
				isInvalid = true;
				diag_->Report(Error::LABEL_NOT_FOUND)<<*label->Name();
			}
		}
	}
	
	// Make the declaration a definition and set the body.
	// If the body is already set it means that an implicit declaration for
	// '__func__' was created. Take the declaration statement and make it the first
	// on the list with the explicit statements.
	if(auto body = functDecl->Body()) {
		bodyStatement->As<CompoundStatement>()->Children().Insert(0, body->Children()[0]);
	}

	functDecl->SetIsDefinition(true);
	functDecl->SetBody(bodyStatement);

	// The function is no longer active.
	Holder()->GetStatementSemantic()->SetActiveFunction(nullptr);
	return isInvalid == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclarationSemantic::AddDefaultInitializer(VariableDeclaration* declaration, 
                                                shared<DeclarationContext> context) {
	// An incomplete array is initialized to [1] with value 0.
	// All other types are initialized to 0 (including all subtypes).
	const Type* declType = declaration->DeclarationType();

	if(declType->IsArray() && declType->IsIncomplete()) {
		// Emit a warning about this.
		diag_->Report(Warning::ARRAY_ASSUMED_ONE_ELEMENT)<<*declaration->Name();

		const ArrayType* arrayType = declType->As<ArrayType>();
		const ArrayType* replacement = types_->GetFinalizedArray(arrayType, 1);

		shared<InitContext> initCtx = new InitContext();
		InitializerListExpression* list =
                new InitializerListExpression(declaration->StartLocation());

		list->InitList().Add(FillWithDefault(arrayType->ElementType(), initCtx));
		declaration->SetInitializer(list);
		declaration->SetDeclType(replacement);
	}
	else {
		shared<InitContext> initCtx = new InitContext();
		declaration->SetInitializer(FillWithDefault(declType, initCtx));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::HandleUnitEnd(shared<DeclarationContext> context, Unit& unit) {
	// Handle tentative definitions (C99:6.9.2.2). Internal and external declarations
	// that have no initializer are initialized to 0. Incomplete arrays are initialized
	// with {0} (they become of length 1). Use the last declaration when checking.
	auto& intList = unit.Internal();

	for(int i = 0; i < intList.Count(); i++) {
		Declaration* last = intList[i]->LastDeclaration();

		if(auto variableDecl = last->As<VariableDeclaration>()) {
			if(variableDecl->Initializer() == nullptr) {
				AddDefaultInitializer(variableDecl, context);
				variableDecl->SetIsTentative(true);
			}
		}
	}

	// The same for external declarations.
	auto& extList = unit.External();

	for(int i = 0; i < extList.Count(); i++) {
		if(auto first = extList[i]->As<VariableDeclaration>()) {
			// If the first declaration was without 'extern' this is a tentative
			// definition; use the last declaration when applying the initializer though.
			if((first->Storage() == StorageType::None) && 
			   (first->Initializer() == nullptr)) {
				auto variableDecl = extList[i]->LastDeclaration()->As<VariableDeclaration>();

				AddDefaultInitializer(variableDecl, context);
				variableDecl->SetIsTentative(true);
			}
		}
	}

	// Restore the default packing for struct/union (it could have been changed
	// by a #pragma directive, but it should not affect other translation units).
	context_->Options().RestorePackValue();
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclarationSemantic::CreatePrototypeVariables(DeclaratorInfo* declaration, 
                                                   shared<DeclarationContext> context) {
    DebugValidator::IsTrue(declaration->IsFunction());
    auto& parameters = declaration->Parameters;

    for(int i = 0; i < parameters.Count(); i++) {
        auto& parameter = parameters[i];

        if(parameter->Declarator == nullptr) {
            break;
        }

        if(auto name = parameter->Declarator->GetName()) {
            if(context->Find(name) == false) {
                // The parameter is not in the context, create the variable now.
                const Type* parameterType = MakeType(parameter->Declarator, 
                                                     parameter->Info, context);
                const Type* adjType = AdjustType(parameterType);
                auto parameterLocation = parameter->Location;

                shared<VariableDeclaration> paramDecl = 
                        new VariableDeclaration(name, parameterType, parameterLocation,
                                                parameterLocation, nullptr);
                context->Add(name, paramDecl);
            }
        }
    }
}

} // namespace Parsing