// SymbolTable.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_SYMBOL_TABLE_HPP
#define PC_SYMBOL_TABLE_HPP

#include "Symbol.hpp"
#include "../Base/Dictionary.hpp"
using namespace Base;

namespace IR {

class SymbolTable {
private:
	Dictionary<const string*, Symbol*, true> nameTable_;
    Dictionary<unsigned, Symbol*> idTable_;
    void* parent_;

public:
    SymbolTable() : parent_(nullptr) {}

    SymbolTable(void* parent) : parent_(parent) {}

	~SymbolTable();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Adds the specified symbol to the table.
	Symbol* Add(Symbol* symbol);

	// Returns 'true' if the symbol with the specified name is found in the table.
	bool Contains(const string* name) const;

    // Returns 'true' if the symbol with the specified name is found in the table.
    bool Contains(int id) const;

	// Returns the symbol with the specified name, 
	// or 'nullptr' if it cannot be found.
	Symbol* Get(const string* name);
	const Symbol* Get(const string* name) const;

    // Returns the symbol with the specified Id, 
    // or 'nullptr' if it cannot be found.
    Symbol* Get(int id);
    const Symbol* Get(int id) const;

	// Removes the symbol with the specified name.
	// Note that the memory used by the symbol is not freed.
	void Remove(const string* name);

    // Removes the symbol with the specified Id.
    // Note that the memory used by the symbol is not freed.
    void Remove(int id);

	// Removes the specified symbol.
	// Note that the memory used by the symbol is not freed.
	void Remove(Symbol* symbol);

	// Removes all symbols from the table.
	void Clear(bool freeSymbols = true);

	// Returns the number of stored symbols.
	int Count() const {
		return idTable_.Count();
	}

    void* Parent() {
        return parent_;
    }

    const void* Parent() const {
        return parent_;
    }

    // Executes the specified action on each symbol in the table.
    template <class Predicate>
    void ForEach(Predicate action) {
        idTable_.ForEachValue(action);
    }

	// Returns a string representation of the table.
	string Dump(bool showParams = true, int level = 0) const;

	// Print the string representation to the console.
	void Dump() const;
};

} // namespace IR
#endif