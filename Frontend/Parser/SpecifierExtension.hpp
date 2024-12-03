// SpecifierExtension.hpp
// Copyright (c) Lup Gratian
//
// Defines the handler for the Microsoft type extensions 
// (sized integers: __int8, __int16, __int32, __int64).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_SPECIFIER_EXTENSION_HPP
#define PC_PARSING_SPECIFIER_EXTENSION_HPP

#include "../Base/SharedPointer.hpp"
#include "Parser.hpp"
#include "Extension.hpp"
using namespace Base;

namespace Parsing {

class SpecifierExtension : public Extension {
public:
	SpecifierExtension::SpecifierExtension(Context* context, Parser* parser);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool ReceiveUnknown() const override { 
        return true; 
    }

	virtual bool ReceiveTypeQuery() const override { 
        return true; 
    }

	virtual bool ReceiveDeclarationSucceeded() const override { 
        return true; 
    }
	
	virtual bool IsTypeToken(Token* token) const override;
	virtual bool Handle(ExtensionContext& context) override;
	virtual void DeclarationSucceeded(Declaration* declaration) override;
};

} // namespace Parsing
#endif