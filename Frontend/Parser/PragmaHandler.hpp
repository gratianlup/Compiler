// PragmaHandler.hpp
// Copyright (c) Lup Gratian
//
// Defines the class that handles the #pragma directive.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_PRAGMA_HANDLER_HPP
#define PC_PARSING_PRAGMA_HANDLER_HPP

#include "PragmaExtension.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/SharedPointer.hpp"
#include "Parser.hpp"
using namespace Base;

namespace Parsing {

class PragmaHandler : public PragmaExtension {
private:
	Dictionary<string, shared<Extension>> pragmaHandlers_;

public:
	PragmaHandler(Context* context, Parser* parser);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool ReceiveUnknown() const override { 
        return true; 
    }

	virtual bool Handle(ExtensionContext& context) override;
};

} // namespace Parsing
#endif