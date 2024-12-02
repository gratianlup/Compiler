// DeclarationContext.hpp
// Copyright (c) Lup Gratian
//
// Contains info about the context in which declarations were made.
// Plays the role of the symbol table.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_DECLARATION_CONTEXT_HPP
#define PC_AST_DECLARATION_CONTEXT_HPP

#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "Identifier.hpp"
using namespace Base;

namespace AST {

// Represents the types of declaration scopes (C99:6.2.1).
enum class ScopeType {
	File,          // Translation unit.
	Function,      // Only for labels.
	Block,
	FunctPrototype,
	Record
};


// Used when handling labeled and jump statements.
enum class BlockFlags {
	None,    // If it's not a block.
	Default, // Compound statement.
	Loop,    // for/while/do.
	If,      // if/else.
	Switch
};

// Forward declarations.
class Declaration;
class LabelStatement;

// Represents a context in which declarations occur.
// The linked list of contexts plays the role of the symbol table.
class DeclarationContext {
private:
	typedef DeclarationContext DC;
	typedef Dictionary<Identifier*, shared<Declaration>, true> Table;
	typedef Dictionary<Identifier*, shared<LabelStatement>, true> LabelTable;

	// Tags (struct/union/enum) live in their own namespace.
	// Something like 'int A; struct A {..};' is valid code.
	// Labels live in their own namespace too (but only under function scope).
	// enum constants are added to the context of the enum parent
	// and interfere with other declarations (something like
	// 'int a; enum T {a,b};' is not valid because 'a' is redeclared).
	// See C99:6.2.3 for more details.
	enum class NamespaceType {
		Tag   = 0, // struct, union, enum
		Other = 1, // For all other types.
		END   = 2, // Used as the number of namespaces. 
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ScopeType  scope_;
	BlockFlags flags_;
	DC* parent_;        // The parent context.
	void* object_;      // An optional object that can be associated.
	LabelTable labels_; // Labels are not declarations.
	Table tables_[NamespaceType::END]; // The symbol tables, one for each namespace.

public:
	DeclarationContext(ScopeType scope, DeclarationContext* parent, void* object = nullptr):
			scope_(scope), flags_(BlockFlags::None), parent_(parent), object_(object) {}

	DeclarationContext(ScopeType scope, BlockFlags flags, DeclarationContext* parent, 
                void* object = nullptr):
			scope_(scope), flags_(flags), parent_(parent), object_(object) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the scope of this context.
	ScopeType Scope() const {
		return scope_;
	}

	// Returns 'true' if the context was created at file scope.
	bool IsFileScope() const {
		return scope_ == ScopeType::File;
	}

	// Returns 'true' if the context was created at block scope.
	bool IsBlockScope() const {
		return scope_ == ScopeType::Block;
	}

	// Returns 'true' if the context was created at function scope.
	bool IsFunctionScope() const {
		return scope_ == ScopeType::Function;
	}

	// Returns 'true' if the context was created at function-prototype scope.
	bool IsFunctProtoScope() const {
		return scope_ == ScopeType::FunctPrototype;
	}

	// Returns 'true' if the context is associated with a struct/union.
	bool IsRecordScope() const {
		return scope_ == ScopeType::Record;
	}

	// Returns the flags associated with the block scope.
	BlockFlags Flags() const {
		return flags_;
	}

	// Returns the parent context, or nullptr if this is already the file context.
	DC* Parent() {
		return parent_;
	}

	void SetParent(DC* value) {
		parent_ = value;
	}

	// Returns the enclosing block. If no block can be found nullptr is returned.
	DC* ParentBlock();

	// Returns the loop block. If no block can be found nullptr is returned.
	// If 'self' is set the current block is returned if it's a loop block.
	DC* ParentLoop(bool self = true);

	// Returns the switch block. If no block can be found nullptr is returned.
	// If 'self' is set the current block is returned if it's a switch block.
	DC* ParentSwitch(bool self = true);

	// Returns the enclosing function. If no function can be found nullptr is returned.
	// If 'self' is set the current block is returned if it's a function block.
	DC* ParentFunction(bool self = true);

	// Returns the file context (first context in the list).
	DC* FileContext();

	// Returns the depth of this context. The file context is located at depth 0.
	int Depth() const;

	// Returns 'true' if this context is an ancestor for the other context.
	bool IsAncestor(DC* other) const;

	// Searches for the specified label identifier.
	// Walks the context list up until a context with function scope is found, 
	// or the file scope is reached (in this case the label could not be found). 
	// 'parentDC' is set to DeclarationContext where the label was found.
	shared<LabelStatement> FindLabel(Identifier* id, DC** parentDC = nullptr);

	// Searches for the specified label identifier.
	// If 'all' is false, only the current context is searched.
	// Else all contexts are searched, until the identifier is found or
	// no more contexts are available (nullptr is returned in this case).
	shared<Declaration> FindTag(Identifier* id, DC** parentDC = nullptr, 
                                bool all = true);

	// Searches for the specified member identifier.
	// Only the current context is searched, the identifier namespace.
	shared<Declaration> FindMember(Identifier* id, DC** parentDC = nullptr);

	// Searches for the specified normal identifier.
	// Includes variable and function declarations. All contexts are searched, 
	// until the identifier is found or no more contexts are available.
	shared<Declaration> Find(Identifier* id, DC** parentDC = nullptr, 
                             bool all = true);

	// Adds the label declaration to the specified context.
	void AddLabel(Identifier* id, shared<LabelStatement> label);

	// Adds the tag declaration to the specified context.
	void AddTag(Identifier* id, shared<Declaration> label);

	// Adds the declaration to the specified context.
	void Add(Identifier* id, shared<Declaration> label);

    // Returns the object associated with the context.
    void* Object() {
        return object_;
    }

	// Returns a string representation of the context.
	string ToString(int level) const;
};

} // namespace AST
#endif