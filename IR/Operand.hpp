// Operand.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_OPERAND_HPP
#define PC_IR_OPERAND_HPP

#include "IRType.hpp"
#include "Symbol.hpp"
#include "Visitor.hpp"
#include "../Base/String.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class Operand;
class Block;
class Function;
class Symbol;
class Variable;
class GlobalVariable;
class Instruction;

namespace Detail {
	// Used for implementing 'As<T>'/'Is<T>'.
	template <class T>
	struct OperandPromoter {
		static bool Is(const Operand* op) {
			static_assert(false, "Type is not an Operand in Is<T>");
			return false;
		}

		static T* As(Operand* op) {
			static_assert(false, "Type is not an Operand in As<T>");
			return nullptr;
		}
	};

	// // Used for implementing 'DefiningInstrAs<T>'/'DefiningInstrIs<T>'.
	template <class T, class Q>
	struct DefiningInstrPromoter {
		static bool Is(const Q* op) {
			static_assert(false, "Instruction.hpp must be included");
			return false;
		}

		static T* As(Q* op) {
			static_assert(false, "Instruction.hpp must be included");
			return nullptr;
		}
	};
} // namespace Detail


class Operand : public Visitable {
private:
	Operand(const Operand&);             // Should not be copied.
	Operand& operator= (const Operand&); // Should not be assigned.

protected:
	enum class Kind {
		Temp,
		VariableRef,
		GlobalVariableRef,
		Constant,
		FunctionRef,
		BlockRef,
		Parameter
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	const Type* type_;   // The type of the operand.

	// One of the following pointers in valid, depending on the type of the operand.
	union {
		Symbol* symbol_;        // The associated symbol. Used only with references.
		Instruction* defInstr_; // The instruction that defines the temporary.
	};

	unsigned char kind_   : 3; // The kind should be set in the constructor of derived classes.
	unsigned char isBool_ : 1; // 'true' if the value should be considered a boolean.
	unsigned char other_  : 5; // Can be used by derived classes to store other information.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Operand(const Type* type, int kind, int other = 0);
	Operand(const Type* type, int kind, Symbol* symbol = nullptr, int other = 0);
	Operand(const Type* type, int kind, Instruction* definingInstr = nullptr, int other = 0);

	// Should generate a string that describes the operand object.
	virtual string ToStringImpl(int level) const;

	// Should deallocate the space used by the operand.
	virtual void FreeImpl() {
		delete this; // Default implementation.
	}

	virtual void FreeImpl(Instruction* user) {
		FreeImpl(); // Default implementation.
	}

	virtual void FreeImpl(Symbol* user) {
		FreeImpl(); // Default implementation.
	}

public:
	template <class T>
	friend struct Detail::OperandPromoter; // Give it access to 'kind_'.

	virtual ~Operand() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// The methods that shall be used to deallocate the memory 
	// used by an operand (instruction user).
	void Free(Instruction* parent) {
		FreeImpl(parent);
	}

	// The methods that shall be used to deallocate the memory 
	// used by an operand (symbol user - global variable).
	void Free(Symbol* parent) {
		FreeImpl(parent);
	}

	// The methods that shall be used to deallocate the memory 
	// used by an operand (user is unknown).
	void Free() {
		FreeImpl();
	}
	
	// Returns 'true' if this is a temporary. Note that it doesn't need to have a symbol.
	bool IsTemporary() const {
		return (Kind)kind_ == Kind::Temp;
	}

	// Returns 'true' if this is a variable, local or global.
	bool IsVariableReference() const {
		return ((Kind)kind_ == Kind::VariableRef) || 
			   ((Kind)kind_ == Kind::GlobalVariableRef);
	}

	// Returns 'true' if this is an operand referring to a local variable.
	bool IsLocalVariableRef() const {
		return (Kind)kind_ == Kind::VariableRef;
	}

	// Returns 'true' if this is an operand referring to a global variable.
	bool IsGlobalVariableRef() const {
		return (Kind)kind_ == Kind::GlobalVariableRef;
	}

	// Returns 'true' if this is a constant integer, floating or string operand.
	bool IsConstant() const {
		return (Kind)kind_ == Kind::Constant;
	}

    // Returns 'true' if this is an operand referring to a variable,
    // function or basic block.
    bool IsReference() const {
        return ((Kind)kind_ == Kind::VariableRef)       || 
               ((Kind)kind_ == Kind::GlobalVariableRef) ||
               ((Kind)kind_ == Kind::FunctionRef)       ||
               ((Kind)kind_ == Kind::BlockRef);
    }

	// Returns 'true' if this is a parameter.
	bool IsParameter() const {
		return (Kind)kind_ == Kind::Parameter;
	}

	// Returns 'true' if this is a constant integer operand.
	bool IsIntConstant() const;
	
	// Returns 'true' if this is a constant floating operand.
	bool IsFloatingConstant() const;

	// Returns 'true' if this is a constant that represents a string.
	bool IsStringConstant() const;

	// Returns 'true' if this is a constant that represents a null pointer.
	bool IsNullConstant() const;

	// Returns 'true' if this is a constant that represents a "undefined" value.
	bool IsUndefinedConstant() const;

	// Returns 'true' if this is a reference to a function.
	bool IsFunctionReference() const {
		return (Kind)kind_ == Kind::FunctionRef;
	}

	// Returns 'true' if this is a reference to a basic block.
	bool IsBlockReference() const {
		return (Kind)kind_ == Kind::BlockRef;
	}

	// Methods that return information about the type.
	bool IsInteger()  const { return type_->IsInteger();  }
	bool IsFloating() const { return type_->IsFloating(); }
	bool IsVoid()     const { return type_->IsVoid();     }
	bool IsArray()    const { return type_->IsArray();    }
	bool IsPointer()  const { return type_->IsPointer();  }
	bool IsRecord()   const { return type_->IsRecord();   }
	bool IsFunction() const { return type_->IsFunction(); }

	bool IsInt8()     const { return type_->IsInt8();     }
	bool IsInt16()    const { return type_->IsInt16();    }
	bool IsInt32()    const { return type_->IsInt32();    }
	bool IsInt64()    const { return type_->IsInt64();    }
	bool IsFloat()    const { return type_->IsFloat();    }
	bool IsDouble()   const { return type_->IsDouble();   }

	// Returns 'true' if the operand has an associated symbol.
	bool HasSymbol() const {
		return symbol_ != nullptr;
	}

	// Returns the associated symbol, if any.
	Symbol* GetSymbol() {
		return symbol_;
	}

	const Symbol* GetSymbol() const {
		return symbol_;
	}

	void SetSymbol(Symbol* value) {
		symbol_ = value;
	}

	// Returns the instruction that defines (generates) the temporary.
	Instruction* DefiningInstruction() {
		return HasDefiningInstruction() ? defInstr_ : nullptr;
	}

	const Instruction* DefiningInstruction() const {
		return HasDefiningInstruction() ? defInstr_ : nullptr;
	}

	// Returns 'true' if the temporary has an associated defining instruction.
	bool HasDefiningInstruction() const {
		return IsTemporary() && defInstr_;
	}

	void SetDefiningInstr(Instruction* value) {
		defInstr_ = value;
	}

	// Returns 'true' if the operand has a defining instruction
	// and its type matches the specified one.
	template <class T>
	bool DefiningInstrIs() const {
		if(HasDefiningInstruction()) {
			return Detail::DefiningInstrPromoter<T, Instruction>::Is(defInstr_);
		}
		else return nullptr;
	}

	// Returns the defining instruction converted to the specified type,
	// or nullptr if there is no such instruction or it doesn't match the type.
	template <class T>
	T* DefiningInstrAs() {
		if(HasDefiningInstruction()) {
			return Detail::DefiningInstrPromoter<T, Instruction>::As(defInstr_);
		}
		else return nullptr;
	}

	template <class T>
	const T* DefiningInstrAs() const {
		if(defInstr_) return Detail::DefiningInstrPromoter<T, Instruction>::As(defInstr_);
		else return nullptr;
	}

	// Returns the type of the operand.
	const Type* GetType() const {
		return type_;
	}

	void SetType(const Type* value) {
        DebugValidator::IsFalse(IsConstant());
		type_ = value;
	}

	// Returns 'true' if the operand is a memory address.
	bool IsAddress() const {
		return type_->IsPointer();
	}

	// Returns 'true' if the operand is a constant integer with value 0.
	bool IsZeroInt() const;

    // Returns 'true' if the operand is a constant integer with value 1.
	bool IsOneInt() const;
    
    // Returns 'true' if the operand is a constant integer with value -1.
	bool IsMinusOneInt() const;

	// Returns 'true' if the values should be considered a boolean.
	bool IsBoolean() const;

	void SetIsBoolean(bool value) {
		isBool_ = value;
	}

	// Returns 'true' if this operand is a temporary with a single user.
	bool HasSingleUser() const;

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return Detail::OperandPromoter<T>::As(this);
	}

	template <class T>
	const T* As() const {
		return Detail::OperandPromoter<T>::As(const_cast<Operand*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return Detail::OperandPromoter<T>::Is(this);
	}

	// Returns 'true' if the type are of the same kind.
	bool IsSameKind(const Operand* other) const {
		return kind_ == other->kind_;
	}

	// Returns a string representation of the operand information.
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