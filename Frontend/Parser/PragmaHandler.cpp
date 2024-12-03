// ExpressionSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements the class that handles the #pragma directive.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "PragmaHandler.hpp"
#include "PackPragma.hpp"
#include "PackPragma.hpp"
#include "MessagePragma.hpp"
#include "NullHandler.hpp"
#include "Parser.hpp"

namespace Parsing {

PragmaHandler::PragmaHandler(Context* context, Parser* parser) :
			PragmaExtension(context, parser) {
	// Initialize the dispatch table with the supported directives.
	pragmaHandlers_.Add("pack",    new PackPragma(context, parser));
	pragmaHandlers_.Add("message", new MessagePragma(context, parser));
	pragmaHandlers_.Add("warning", new NullHandler(context, parser));
	pragmaHandlers_.Add("once",    new NullHandler(context, parser));

	// The following directives are ignored.
	pragmaHandlers_.Add("alloc_text",       new NullHandler(context, parser));
	pragmaHandlers_.Add("auto_inline",      new NullHandler(context, parser));
	pragmaHandlers_.Add("bss_seg",          new NullHandler(context, parser));
	pragmaHandlers_.Add("check_stack",      new NullHandler(context, parser));
	pragmaHandlers_.Add("code_seg",         new NullHandler(context, parser));
	pragmaHandlers_.Add("comment",          new NullHandler(context, parser));
	pragmaHandlers_.Add("component",        new NullHandler(context, parser));
	pragmaHandlers_.Add("conform",          new NullHandler(context, parser));
	pragmaHandlers_.Add("hdrstop",          new NullHandler(context, parser));
	pragmaHandlers_.Add("include_alias",    new NullHandler(context, parser));
	pragmaHandlers_.Add("init_seg1",        new NullHandler(context, parser));
	pragmaHandlers_.Add("inline_depth",     new NullHandler(context, parser));
	pragmaHandlers_.Add("inline_recursion", new NullHandler(context, parser));
	pragmaHandlers_.Add("intrinsic",        new NullHandler(context, parser));
	pragmaHandlers_.Add("managed",          new NullHandler(context, parser));
	pragmaHandlers_.Add("optimize",         new NullHandler(context, parser));
	pragmaHandlers_.Add("pop_macro",        new NullHandler(context, parser));
	pragmaHandlers_.Add("push_macro",       new NullHandler(context, parser));
	pragmaHandlers_.Add("runtime_checks",   new NullHandler(context, parser));
	pragmaHandlers_.Add("section",          new NullHandler(context, parser));
	pragmaHandlers_.Add("setlocale",        new NullHandler(context, parser));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool PragmaHandler::Handle(ExtensionContext& context) {
	// If the current token represents a pragma we handle the case.
	// We check if the name of the pragma is in our list; if it is, we handle it,
	// else we emit a warning about the unknown pragma.
	if((context.Type != Context_Unit) && (context.Type != Context_Statement)) {
		return false;
	}

	if(parser_->CurrentToken().IsPragma() == false) {
		return false;
	}

	// Now we should find the name of the pragma. If not skip the whole line and warn.
	parser_->EatToken();
	Token& nameToken = parser_->CurrentToken();

	if(nameToken.IsIdentifier() == false) {
		diag_->Report(Warning::PRAGMA_INVALID_NAME)<<nameToken.Location();
		SkipToLineEnd();
		return false;
	}

	// Try to handle the directive.
	string name = nameToken.NameValue()->Name;
	parser_->EatToken(); // Skip over directive name.

	if(pragmaHandlers_.ContainsKey(name)) {
		Extension* handler = pragmaHandlers_[name];
		return handler->Handle(context);
	}
	else {
		// This is an unknown directive.
		diag_->Report(Warning::PRAGMA_UNKNOWN)<<nameToken.Location();
		SkipToLineEnd();
		return false;
	}
}

} // namespace Parsing
