// NullHandler.hpp
// Copyright (c) Lup Gratian
//
// Defines an extension handler that does nothing.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_NULL_HANDLER_HPP
#define PC_PARSING_NULL_HANDLER_HPP

#include "../Base/Dictionary.hpp"
#include "../Base/SharedPointer.hpp"
#include "Extension.hpp"
using namespace Base;

namespace Parsing {

class NullHandler : public Extension {
public:
	NullHandler(Context* context, Parser* parser);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool Handle(ExtensionContext& context) override;
};

} // namespace Parsing
#endif