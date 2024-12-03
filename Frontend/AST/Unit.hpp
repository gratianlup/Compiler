// Unit.hpp
// Copyright (c) Lup Gratian
//
// Represents the data associated with a translation unit.
// Contains the AST and lists of external/static declarations.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_UNIT_HPP
#define PC_AST_UNIT_HPP

#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Common/FileId.hpp"
#include "Declaration.hpp"
#include "Declarations.hpp"
#include "DeclarationContext.hpp"
using namespace Base;

namespace AST {

class Unit {
private:
	FileId file_;
	List<shared<Declaration>>  externalDecl_;
	List<shared<Declaration>>  internalDecl_; 
	List<shared<FunctionDeclaration>> functionDecl_; // All function definitions.
	List<shared<Declaration>>  decl_;         // All declarations, in order.
	List<shared<Declaration>>  tags_;         // All declared struct/union/enum tags.

public:
	Unit(FileId file) : file_(file) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the file associated with the translation unit.
	FileId File() const {
		return file_;
	}

	// Returns the list of external declarations.
	List<shared<Declaration>>& External() {
		return externalDecl_;
	}

	const List<shared<Declaration>>& External() const {
		return externalDecl_;
	}

	// Returns the number of external declarations.
	int ExternalCount() const {
		return externalDecl_.Count();
	}

	// Returns the list of static declarations.
	List<shared<Declaration>>& Internal() {
		return internalDecl_;
	}

	const List<shared<Declaration>>& Internal() const {
		return internalDecl_;
	}

	// Returns the number of static declarations.
	int InternalCount() const {
		return internalDecl_.Count();
	}

	// Returns the list of defined functions.
	List<shared<FunctionDeclaration>>& Functions() {
		return functionDecl_;
	}

	const List<shared<FunctionDeclaration>>& Functions() const {
		return functionDecl_;
	}

	// Returns the number of defined functions.
	int FunctionCount() const {
		return functionDecl_.Count();
	}

	// Returns the list of top-level declarations and definitions.
	List<shared<Declaration>>& UnitDeclarations() {
		return decl_;
	}

	const List<shared<Declaration>>& UnitDeclarations() const {
		return decl_;
	}

	// Returns the list of the declared tags (at all levels).
	List<shared<Declaration>>& Tags() {
		return tags_;
	}

	const List<shared<Declaration>>& Tags() const {
		return tags_;
	}
};

} // namespace AST
#endif