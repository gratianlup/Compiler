// DeclspecExtension.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'DeclspecExtension' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DeclspecExtension.hpp"
#include "Parser.hpp"

namespace Parsing {

DeclspecExtension::DeclspecExtension(Context* context, Parser* parser) :
	  Extension(context, parser) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclspecExtension::Handle(ExtensionContext& context) {
	// __declspec 
	// If the identifier is unknown we emit a warning
    // and return control to the parser.
	if(context.Type != Context_Specifiers) {
		return false;
	}

	if(parser_->Kwd(parser_->CurrentToken()) != Keyword_Declspec) {
		return false;
	}
	else parser_->EatToken();

	// ( should be found
	if(parser_->CurrentToken().Kind() != TokenKind::OpenParen) {
		diag_->Report(Error::DECLSPEC_NO_OPEN_PAREN)<<
					  parser_->CurrentToken().Location();
		return false;
	}
	else parser_->EatToken(); // Skip over (

	// Read the command and apply it.
	Token commandToken = parser_->CurrentToken();
	parser_->EatToken();

	if(commandToken.IsIdentifier() == false) {
		diag_->Report(Error::DECLSPEC_EXPECTED_IDENTIFIER)<<
					  parser_->CurrentToken().Location();
		return false;
	}
	
	NameData* command = commandToken.NameValue();

	if(command->Name == "align") {
		// align(NUMBER)
		// We expect (, a number, then )
		if(parser_->CurrentToken().Kind() != TokenKind::OpenParen) {
			diag_->Report(Error::DECLSPEC_NO_OPEN_PAREN)<<
						  parser_->CurrentToken().Location();
			return false;
		}
		else parser_->EatToken(); // Skip over (

		bool invalid = false;
		Token numberToken = parser_->CurrentToken();
		parser_->EatToken();

		if(numberToken.IsNumber() == false) {
			invalid = true;
		}
		else {
			// MUST BE A POWER OF TWO
			NumberInfo number = NumberParser(diag_).Parse(numberToken);
			if((number.IsValid == false) || number.IsFloating()) {
				invalid = true;
			}
			else if(((number.IntValue & (number.IntValue - 1)) != 0)) {
				// The value must be a power of two.
				diag_->Report(Error::DECLSPEC_ALIGN_NOT_POWER_OF_TWO)<<
							  parser_->CurrentToken().Location();
				invalid = true;
			}

			if(invalid) {
                return false;
            }
			else {
                auto attribute = new AlignmentAttribute((int)number.IntValue);
                context.Specifiers->Attributes.Add(attribute);
            }
		}

		if(parser_->CurrentToken().Kind() != TokenKind::CloseParen) {
			diag_->Report(Error::DECLSPEC_NO_CLOSE_PAREN)<<
						  parser_->CurrentToken().Location();
			return false;
		}
		else parser_->EatToken(); // Skip over )
	}
	else if(command->Name == "dllimport") {
		context.Specifiers->Attributes.Add(new DllAttribute(DllType::Import));
	}
	else if(command->Name == "dllexport") {
		context.Specifiers->Attributes.Add(new DllAttribute(DllType::Export));
	}
	else if(command->Name == "noinline") {
		// This must be used only with functions; the check is made when the
		// declaration is created.
		context.Specifiers->Attributes.Add(new InlineAttribute(InlineType::Never));
	}
	else if(command->Name == "restrict") {
		context.Specifiers->Qual.SetHasRestrict(true);
	}

	// ) should be found
	if(parser_->CurrentToken().Kind() != TokenKind::CloseParen) {
		diag_->Report(Error::DECLSPEC_NO_CLOSE_PAREN)<<
					  parser_->CurrentToken().Location();
		return false;
	}
	else parser_->EatToken(); // Skip over )

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void DeclspecExtension::DeclarationSucceeded(Declaration* declaration) {
	// 'noinline' should be applied only on functions.
	if(declaration->AttributeAs<InlineAttribute>() && 
	   (declaration->DeclarationType()->IsFunction() == false)) {
		diag_->Report(Error::DECLSPEC_NOINLINE_NOT_ON_FUNCTION)<<
					  declaration->StartLocation();
		   
	}
}

} // namespace Parsing