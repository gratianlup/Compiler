// PragmaExtension.hpp
// Copyright (c) Lup Gratian
//
// Defines an extension handler that does nothing.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_PRAGMA_EXTENSION_HPP
#define PC_PARSING_PRAGMA_EXTENSION_HPP

#include "../Base/SharedPointer.hpp"
#include "Extension.hpp"
using namespace Base;

namespace Parsing {

class PragmaExtension : public Extension {
public:
	PragmaExtension(Context* context, Parser* parser);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Skips all tokens until the end of the line is found.
	void SkipToLineEnd();

	virtual bool Handle(ExtensionContext& context) = 0;
};

} // namespace Parsing
#endif