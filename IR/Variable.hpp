// Variable.hpp
// Copyright (c) Lup Gratian
//
// 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_VARIABLE_HPP
#define PC_IR_VARIABLE_HPP

#include "Symbol.hpp"
#include "Tagged.hpp"
#include "IRType.hpp"

namespace IR {

// Forward declarations.
class Function;
class VariableReference;


class Variable : public Symbol, public Tagged<Tag> {
protected:
	unsigned char isParameter_ : 1;
	unsigned char isRestrict_  : 1;
    unsigned char isNoRead_    : 1;
    unsigned char isNoWrite_   : 1;
    unsigned char isNoEscape_  : 1;
    unsigned char isUnsigned_  : 1;
	unsigned char other_       : 3;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual string ToStringImpl(int level) const override;

	Variable(Kind kind, const Type* type, shared<string> name, int other,
			 Function* parent, SymbolVisibility visibility);

	// To be used by 'GlobalVariable'.
	Variable(Kind kind, const Type* type, shared<string> name, int other,
			 SymbolTable* parent, SymbolVisibility visibility);

public:
	static Variable* GetVariable(const Type* type, shared<string> name, 
								 Function* parent = nullptr, 
								 SymbolVisibility visibility = SymbolVisibility::Auto);

	static Variable* GetVariable(const Type* type, const string& name, 
								 Function* parent = nullptr, 
								 SymbolVisibility visibility = SymbolVisibility::Auto);

	virtual ~Variable() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns a reference that points to the variable.
    VariableReference* GetReference();

    bool IsUnsigned() const {
        return isUnsigned_;
    }

    void SetIsUnsigned(bool value) {
        isUnsigned_ = value;
    }

    bool IsSigned() const {
        return IsUnsigned() == false;
    }

    void SetIsSigned(bool value) {
        SetIsUnsigned(!value);
    }

	// Returns 'true' if the variable is one
    // of the arguments of a function.
	bool IsParameter() const {
		return isParameter_;
	}

	void SetIsParameter(bool value) {
		isParameter_ = value;
	}

	// Returns 'true' if the variable has pointer type
    // and is marked 'restrict'.
	bool IsRestrict() const {
		return isRestrict_;
	}

	void SetIsRestrict(bool value) {
		isRestrict_ = value;
	}

    // Returns 'true' if the function is guaranteed to not to read
    // from the parameter variable (note that it still may write to it).
    bool IsNoRead() const {
        return isNoRead_;
    }

    bool IsRead() const {
        return isNoRead_ == false;
    }

    void SetIsNoRead(bool value) {
		isNoRead_ = value;
	}

    // Returns 'true' if the function is guaranteed to not write 
    // to the parameter variable.
	bool IsNoWrite() const {
		return isNoWrite_;
	}

    bool IsWrite() const {
		return isNoWrite_ == false;
	}

	void SetIsNoWrite(bool value) {
		isNoWrite_ = value;
	}

    // Returns 'true' if the address of the variable is guaranteed 
    // not to be stored into a global variable and into other objects 
    // that may escape the function.
    bool IsNoEscape() const {
		return isNoEscape_;
	}

    // Returns 'true' if the address of the variable may be stored 
    // into a global variable or into objects that may escape the function.
    bool IsEscape() const {
        return isNoEscape_ == false;
    }

	void SetIsNoEscape(bool value) {
		isNoEscape_ = value;
	}

	// Methods that return information about the type.
	bool IsInteger()   const { return type_->IsInteger();  }
	bool IsFloating()  const { return type_->IsFloating(); }
	bool IsArray()     const { return type_->IsArray();    }
	bool IsPointer()   const { return type_->IsPointer();  }
	bool IsRecord()    const { return type_->IsRecord();   }
	bool IsFunction()  const { return type_->IsFunction(); }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<Variable> {
		static bool Is(const Symbol* symbol) {
			return (Symbol::Kind)symbol->kind_ == Symbol::Kind::Variable;
		}

		static Variable* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<Variable*>(symbol) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif