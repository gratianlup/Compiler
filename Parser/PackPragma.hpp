// PackPragma.hpp
// Copyright (c) Lup Gratian
//
// Defines the class that handles the #pragma pack Microsoft extension.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_PACK_PRAGMA_HPP
#define PC_PARSING_PACK_PRAGMA_HPP

#include "PragmaExtension.hpp"
#include "../Base/List.hpp"
using namespace Base;

namespace Parsing {

class PackPragma : public PragmaExtension {
private:
	struct PackInfo {
		int    Value;
		string Name;
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	List<PackInfo> packStack_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool IsPush(Token& token) {
		return token.IsIdentifier() &&
			   token.NameValue()->Name == "push";
	}

	bool IsPop(Token& token) {
		return token.IsIdentifier() &&
			   token.NameValue()->Name == "pop";
	}

	bool IsShow(Token& token) {
		return token.IsIdentifier() &&
			   token.NameValue()->Name == "show";
	}

public:
	PackPragma(Context* context, Parser* parser);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool Handle(ExtensionContext& context) override;
};

} // namespace Parsing
#endif