// MessagePragma.hpp
// Copyright (c) Lup Gratian
//
// Defines the #pragma message extension, which emits an information message.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_MESSAGE_PRAGMA_HPP
#define PC_PARSING_MESSAGE_PRAGMA_HPP

#include "../Base/Dictionary.hpp"
#include "../Base/SharedPointer.hpp"
#include "PragmaExtension.hpp"
using namespace Base;

namespace Parsing {

class MessagePragma : public PragmaExtension {
public:
	MessagePragma(Context* context, Parser* parser);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool Handle(ExtensionContext& context) override;
};

} // namespace Parsing
#endif