// DeclspecExtension.hpp
// Copyright (c) Lup Gratian
//
// Defines the handler for the '_declspec' Microsoft extension.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_DECLSPEC_EXTENSION_HPP
#define PC_PARSING_DECLSPEC_EXTENSION_HPP

#include "../Base/SharedPointer.hpp"
#include "../Base/List.hpp"
#include "Extension.hpp"
using namespace Base;

namespace Parsing {

class DeclspecExtension : public Extension {
public:
	DeclspecExtension(Context* context, Parser* parser);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool ReceiveUnknown() const override {
        return true; 
    }

	virtual bool ReceiveDeclarationSucceeded() const override {
        return true; 
    }

	virtual bool Handle(ExtensionContext& context);
	virtual void DeclarationSucceeded(Declaration* declaration);
};

} // namespace Parsing
#endif