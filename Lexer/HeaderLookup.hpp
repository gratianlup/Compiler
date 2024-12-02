// HeaderLookup.hpp
// Copyright (c) Lup Gratian
//
// Performs lookup for user and system-based header files.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_HEADER_LOOKUP_HPP
#define PC_LEXING_HEADER_LOOKUP_HPP

#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/List.hpp"
#include "../Base/StaticStack.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/Path.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Common/FileManager.hpp"
#include "../Common/LocationInfo.hpp"
#include "CharSource.hpp"
using namespace Base;
using namespace Common;

namespace Lexing {

class Lexer; // Forward declaration.

class HeaderLookup {
private:
	static const int STACK_DEPTH = 256;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StaticStack<FileId, STACK_DEPTH> includeStack_;
	FileManager* manager_;
	Lexer* lexer_;

public:
	HeaderLookup(Lexer* lexer, FileManager* manager) : 
			lexer_(lexer), manager_(manager) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Tries to load the content of the specified user or system header
	// and to set it as the current character source.
	bool PushHeader(const string& path, bool isSystem);

	// Returns true if the include depth for this translation unit has been reached.
	bool StackFull() {
		return includeStack_.Count() == STACK_DEPTH;
	}

	// Loads the first file used by the Lexer.
	bool LoadStart(const string& file);

	// Called when the Lexer reached the end of a header file.
	void PopHeader() {
		DebugValidator::IsLarger(includeStack_.Count(), 0);
		includeStack_.Pop();
	}
};

} // namespace Lexing
#endif