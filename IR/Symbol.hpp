// Symbol.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for all symbols.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_SYMBOL_HPP
#define PC_IR_SYMBOL_HPP

#include "IRType.hpp"
#include "Tagged.hpp"
#include "Visitor.hpp"
#include "../Base/String.hpp"
#include "../Base/SharedPointer.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class SymbolTable;
class Variable;
class GlobalVariable;
class Block;
class Function;
class Intrinsic;
class Symbol;

namespace Detail {
	// Used for implementing 'As<T>'/'Is<T>'.
	template <class T>
	struct SymbolPromoter {
		static bool Is(const Symbol* op) {
			static_assert(false, "Type is not an Symbol");
			return false;
		}

		static T* As(Symbol* op) {
			static_assert(false, "Type is not an Symbol");
			return nullptr;
		}
	};
} // namespace Detail


// Represents the type of visibility a symbol can have.
// 'SymbolVisibility::Auto' is used for local variables and function arguments.
enum class SymbolVisibility {
	Auto,
	Static,
	Extern,
	Tentative
};


// Represents the Microsoft extensions '__declsped(dllimport)'
// and '__declspec(dllexport)'. 
enum class DllVisibility {
	None,
	Import,
	Export
};


class Symbol : public Visitable {
private:
	Symbol();                          // Should not be created.
	Symbol(const Symbol&);             // Should not be copied.
	Symbol& operator= (const Symbol&); // Should not be assigned.

protected:
	enum class Kind {
		Variable,
		GlobalVariable,
		Block,
		Function,
		TypeName
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	SymbolTable* parentTable_; // The table in which the symbol resides.
	const Type* type_;         // The type of the symbol.
	shared<string> name_;      // The optional name for the symbol.
	unsigned id_;              // The ID of the symbol.
	unsigned char  alignment_; // The alignment of the symbol, in bytes.

	unsigned char kind_          : 3; // The kind should be set in the constructor of derived classes.
	unsigned char visibility_    : 3; // The visibility of the symbol, in respect to other units.
	unsigned char dllVisibility_ : 2; // The MS visibility extension.
	unsigned char addressTaken_  : 1; // If the address of the symbol is anywhere taken.
    unsigned char other_         : 7; // Can be used by derived classes.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Symbol(Kind kind, const Type* type, shared<string> name, SymbolTable* parent = nullptr,
		   SymbolVisibility visibility = SymbolVisibility::Auto);

	// Should generate a string that describes the symbol object.
	virtual string ToStringImpl(int level) const {
		return "";
	}

	// Should deallocate the space used by the symbol.
	virtual void FreeImpl() {
		delete this; // Default implementation.
	}

public:
	template <class T>
	friend struct Detail::SymbolPromoter; // Give it access to 'kind_'.

	// Factory methods for creating symbol instances.
	static Symbol* GetTypename(const Type* type, const string& name, 
							   SymbolTable* parent = nullptr);

	virtual ~Symbol() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// The methods that shall be used to deallocate the memory used by a symbol.
	void Free() {
		FreeImpl();
	}

	// Returns the ID associated with this symbol.
	unsigned Id() const {
		return id_;
	}

	void SetId(unsigned value) {
		id_ = value;
	}

	// Returns 'true' if the symbol represents a local variable.
	bool IsLocalVariable() const {
		return (Kind)kind_ == Kind::Variable;
	}

	// Returns 'true' if the symbol represents a global variable.
	bool IsGlobalVariable() const {
		return (Kind)kind_ == Kind::GlobalVariable;
	}

	// Returns 'true' if the symbol represents a basic block.
	bool IsBlock() const {
		return (Kind)kind_ == Kind::Block;
	}

	// Returns 'true' if the symbol represents a function.
	bool IsFunction() const {
		return (Kind)kind_ == Kind::Function;
	}

	// Returns 'true' if the symbol represents a type name 
	// (introduces a new, user-defined type).
	bool IsTypename() const {
		return (Kind)kind_ == Kind::TypeName;
	}

	// Returns the type of the symbol.
	const Type* GetType() const {
		return type_;
	}

	void SetType(const Type* value) {
		type_ = value;
	}

	// Returns 'true' if the operand has an associated name.
	bool HasName() const {
		return name_ != nullptr;
	}

	// Returns the associated name. It is 'nullptr' if no name is present.
	shared<string> Name() {
		return name_;
	}

	const shared<string> Name() const {
		return name_;
	}

	void SetName(shared<string> value) {
		name_ = value;
	}

	// Returns the table where the symbol is located.
	SymbolTable* ParentTable() {
		return parentTable_;
	}

	const SymbolTable* ParentTable() const {
		return parentTable_;
	}

	void SetParentTable(SymbolTable* value) {
		parentTable_ = value;
	}

	// Moves the symbol to the specified symbol table.
	void MoveTo(SymbolTable* other);

	// Removes the symbol from the parent symbol table.
	void RemoveFromParent();

	// Returns 'true' if the symbol is visible only in the parent function.
	bool IsAuto() const {
		return (SymbolVisibility)visibility_ == SymbolVisibility::Auto;
	}

	// Returns 'true' if the symbol is visible only in the parent unit.
	bool IsStatic() const {
		return (SymbolVisibility)visibility_ == SymbolVisibility::Static;
	}

	// Returns 'true' if the symbol is visible to all units.
	bool IsExtern() const {
		return (SymbolVisibility)visibility_ == SymbolVisibility::Extern;
	}

	// Returns 'true' if the symbol is a C tentative definition.
	bool IsTentative() const {
		return (SymbolVisibility)visibility_ == SymbolVisibility::Tentative;
	}
	
	// Gets the visibility of the symbol relative to other units.
	SymbolVisibility Visibility() const {
		return (SymbolVisibility)visibility_;
	}

	void SetVisibility(SymbolVisibility value) {
		visibility_ = (unsigned char)value;
	}

	// Returns 'true' if the symbol is marked with the 'dllimport' attribute.
	bool IsDllImport() const {
		return (DllVisibility)dllVisibility_ == DllVisibility::Import;
	}

	// Returns 'true' if the symbol is marked with the 'dllexport' attribute.
	bool IsDllExport() const {
		return (DllVisibility)dllVisibility_ == DllVisibility::Export;
	}

	DllVisibility GetDllVisibility() const {
		return (DllVisibility)dllVisibility_;
	}

	void SetDllVisibility(DllVisibility value) {
		dllVisibility_ = (unsigned char)value;
	}

	// Returns 'true' if the address of the symbol is taken anywhere
	// in the whole program.
	bool IsAddressTaken() const {
		return addressTaken_;
	}

    bool IsAddressNotTaken() const {
        return addressTaken_ == 0;
    }

    void SetIsAddresTaken(bool value) {
		addressTaken_ = value;
	}

	// Returns 'true' if the symbol has the alignment attribute attached.
	bool HasAlignment() const {
		return alignment_ != 0;
	}

	// Returns the value of the alignment attribute.
	int Alignment() const {
		return alignment_;
	}

	// Sets the value of the alignment attribute.
	// If such an attribute doesn't exist yet it's created now.
	void SetAlignment(int value) {
		alignment_ = value;
	}

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return Detail::SymbolPromoter<T>::As(this);
	}

	template <class T>
	const T* As() const {
		return Detail::SymbolPromoter<T>::As(const_cast<Symbol*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return Detail::SymbolPromoter<T>::Is(this);
	}

	// Returns 'true' if the type are of the same kind.
	bool IsSameKind(const Symbol* other) const {
		return kind_ == other->kind_;
	}

	// Returns a string representation of the type information.
	string ToString(int level = 0) const {
		return ToStringImpl(level);
	}

	// Print the string representation to the console.
	void Dump() const;

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};

} // namespace IR
#endif