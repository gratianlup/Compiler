// Statement.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for the statement system.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_STATEMENT_HPP
#define PC_AST_STATEMENT_HPP

#include "../Base/String.hpp"
#include "../Common/LocationInfo.hpp"
#include "Visitor.hpp"
using namespace Base;
using namespace Common;

namespace AST {

class Statement : public Visitable {
private:
	Statement();							 // Should not be created.
	Statement(const Statement&);			 // Should not be copied.
	Statement& operator= (const Statement&); // Should not be assigned.

protected:
	static const char STMT_IF       = 0; // if/else combined.
	static const char STMT_FOR      = 1;
	static const char STMT_WHILE    = 2;
	static const char STMT_DO       = 3;
	static const char STMT_CONTINUE = 4;
	static const char STMT_BREAK    = 5;
	static const char STMT_RETURN   = 6;
	static const char STMT_LABEL    = 7;
	static const char STMT_SWITCH   = 8;
	static const char STMT_CASE     = 9; // 'default' is included.
	static const char STMT_COMPOUND = 10;
	static const char STMT_NULL     = 11; // Statement without content (only ;).
	static const char STMT_DECL     = 12; // Adapter for declarations.
	static const char STMT_EXPR     = 13; // Adapter for expressions.
	static const char STMT_GOTO     = 14;

	char kind_;             // The type of the declaration.
	LocationInfo location_; // Where the statement starts.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Constructors to be used only by derived classes.
	Statement(int kind, LocationInfo location) : 
			kind_(kind), location_(location) {}

	// Should generate a string that describes the information contained
	// in the declaration object. Children should be included.
	virtual string ToStringImpl(int level) const {
		return "";
	}

	// Should return 'true' if the statements are identical.
	virtual bool EqualsImpl(const Statement* other) const {
		return false;
	}

public:
	virtual ~Statement() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the place where the expression start was found.
	LocationInfo Location() const {
		return location_;
	}

	void SetLocation(LocationInfo value) {
		location_ = value;
	}

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return dynamic_cast<T*>(this);
	}

	template <class T>
	const T* As() const {
		return dynamic_cast<T*>(const_cast<Statement*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return As<T>();
	}

	// Returns 'true' if this is an 'if' declaration.
	bool IsIfStatement() const {
		return kind_ == STMT_IF;
	}

	// Returns 'true' if this is a 'for' loop statement.
	bool IsForStatement() const {
		return kind_ == STMT_FOR;
	}

	// Returns 'true' if this is a 'while' loop statement.
	bool IsWhileStatement() const {
		return kind_ == STMT_WHILE;
	}

	// Returns 'true' if this is a 'do' loop statement.
	bool IsDoStatement() const {
		return kind_ == STMT_DO;
	}

    // Returns 'true' if the statement is a 'for', 'while' or 'do' loop.
    bool IsLoopStatement() const {
        return IsForStatement()   || 
               IsWhileStatement() || 
               IsDoStatement();
    }
       
    // Returns 'true' if this is a 'continue' jump statement.
	bool IsContinueStatement() const {
		return kind_ == STMT_CONTINUE;
	}

	// Returns 'true' if this is a 'break' jump statement.
	bool IsBreakStatement() const {
		return kind_ == STMT_BREAK;
	}

	// Returns 'true' if this is a 'return' jump statement.
	bool IsReturnStatement() const {
		return kind_ == STMT_RETURN;
	}

	// Returns 'true' if this is a 'label' statement.
	bool IsLabelStatement() const {
		return kind_ == STMT_LABEL;
	}

	// Returns 'true' if this is a 'goto' jump statement.
	bool IsGotoStatement() const {
		return kind_ == STMT_GOTO;
	}

	// Returns 'true' if this is a 'switch' statement.
	bool IsSwitchStatement() const {
		return kind_ == STMT_SWITCH;
	}

	// Returns 'true' if this is a 'case' statement (found in a switch).
	bool IsCaseStatement() const {
		return kind_ == STMT_CASE;
	}

	// Returns 'true' if this is a 'default' statement (found in a switch).
	bool IsDefaultStatement() const;

	// Returns 'true' if this is a compound statement.
	bool IsCompoundStatement() const {
		return kind_ == STMT_COMPOUND;
	}

	// Returns 'true' if this is a null (empty) statement.
	bool IsNullStatement() const {
		return kind_ == STMT_NULL;
	}

	// Returns 'true' if this is a declaration statement.
	bool IsDeclarationStatement() const {
		return kind_ == STMT_DECL;
	}

	// Returns 'true' if this is an expression statement.
	bool IsExpressionStatement() const {
		return kind_ == STMT_EXPR;
	}

	// Returns 'true' if the statements are identical.
	bool Equals(Statement* other) const {
		return EqualsImpl(other);
	}

	string ToString(int level) const {
		return ToStringImpl(level);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const = 0;
};

} // namespace AST
#endif