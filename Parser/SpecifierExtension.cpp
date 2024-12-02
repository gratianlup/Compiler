// SpecifierExtension.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'TypeExtension' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Parser.hpp"
#include "SpecifierExtension.hpp"
#include "../AST/Attributes.hpp"

namespace Parsing {

SpecifierExtension::SpecifierExtension(Context* context, Parser* parser) :
		Extension(context, parser) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SpecifierExtension::IsTypeToken(Token* token) const {
	switch(parser_->Kwd(*token)) {
		case Keyword_Int8: 
		case Keyword_Int16:
		case Keyword_Int32:
		case Keyword_Int64:
		case Keyword_Unaligned:
		case Keyword_Cdecl:
		case Keyword_Fastcall:
		case Keyword_Stdcall:
		case Keyword_ForceInline: return true;
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool SpecifierExtension::Handle(ExtensionContext& context) {
	if(context.Type != Context_Specifiers) {
		return false;
	}

	switch(parser_->Kwd(parser_->CurrentToken())) {
		case Keyword_Int8: {
			context.Invalid = context.Specifiers->CharAllowed() == false;
			context.Specifiers->Int = true;
			parser_->EatToken();
			return true;
		}
		case Keyword_Int16: {
			context.Invalid = context.Specifiers->ShortAllowed() == false;
			context.Specifiers->Short = true;
			parser_->EatToken();
			return true;
		}
		case Keyword_Int32: {
			context.Invalid = context.Specifiers->IntAllowed() == false;
			context.Specifiers->Int = true;
			parser_->EatToken();
			return true;
		}
		case Keyword_Int64: {
			context.Invalid = context.Specifiers->LongAllowed() == false;
			context.Specifiers->LongCount += 2;
			parser_->EatToken();
			return true;
		}
		case Keyword_ForceInline: {
			// Create an attribute for in this case.
			context.Specifiers->Inline = true;
			context.Specifiers->Attributes.Add(new InlineAttribute(InlineType::Always));
			parser_->EatToken();
			return true;
		}
		case Keyword_Cdecl: {
			// Create the attribute and check later that it's applied
			// on a function declaration or definition.
			context.Specifiers->Attributes.Add(new CallConventionAttribute(CallConvention::Cdecl));
			parser_->EatToken();
			return true;
		}
		case Keyword_Fastcall: {
			context.Specifiers->Attributes.Add(new CallConventionAttribute(CallConvention::Fastcall));
			parser_->EatToken();
			return true;
		}
		case Keyword_Stdcall: {
			context.Specifiers->Attributes.Add(new CallConventionAttribute(CallConvention::Stdcall));
			parser_->EatToken();
			return true;
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void SpecifierExtension::DeclarationSucceeded(Declaration* declaration) {
	// Validate any of the known attributes.
	// '__forceinline' doesn't need to be validated because it's checked
	// together with the standard 'inline' specifier.

	// Call-convention specifiers are allowed only on functions.
	if(declaration->AttributeAs<CallConventionAttribute>() &&
	   (declaration->DeclarationType()->IsFunction() == false)) {
		if(declaration->Name()) {
			diag_->Report(Error::CALL_CONVENTION_NOT_ON_FUNCTION)<<*declaration->Name();
		}
		else diag_->Report(Error::CALL_CONVENTION_NOT_ON_FUNCTION)<<declaration->StartLocation();
	}
}

}