// GlobalVariable.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GLOBAL_VARIABLE_HPP
#define PC_IR_GLOBAL_VARIABLE_HPP

#include "../Base/SharedPointer.hpp"
#include "../Base/DebugValidator.hpp"
#include "Variable.hpp"
#include "Operand.hpp"
using namespace Base;

namespace IR {

// Forward declarations.
class Reference;
class VariableReference;

// Represents the conversion that should be performed on the initializer value.
// Used when the value is a pointer and casts (implicit or explicit) are applied.
enum class InitConversion {
	None,
	PointerToInt,
	IntToPointer,
	PointerToPointer
};


// Represents the values used to initialize a global variable.
class Initializer {
private:
	Operand* value_;            // The value used to initialize.
	const Type* convType_;      // The type to which the value should be converted.
	__int64 adjustment_;        // An adjustment applied to the address of the value.
	InitConversion conversion_; // The kind of conversion performed.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Initializer(Operand* value, __int64 adjustment, 
                InitConversion conversion, const Type* conversionType);

public:
	Initializer();

	virtual ~Initializer();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static shared<Initializer> 
	GetInitializer(Operand* value, __int64 adjustment = 0,
				   InitConversion conversion = InitConversion::None,
				   const Type* conversionType = nullptr);

	// Returns 'true' if this  initializer is an initializer list.
	virtual bool IsInitializerList() const { 
		return false; 
	}

	// Associates the value operand with the specified variable.
	virtual void AssociateWith(GlobalVariable* variable);

	// Disassociates the value operand from the specified variable.
	virtual void DisassociateFrom(GlobalVariable* variable);

	Operand* Value() {
		return value_;
	}

	const Operand* Value() const {
		return value_;
	}

	// Returns 'true' if the value (which is a pointer) needs to be adjusted.
	bool HasAdjustment() const {
		return adjustment_ != 0;
	}

	// Returns the adjustment value (used only with pointers).
	__int64 Adjustment() const {
		return adjustment_;
	}

	void SetAdjustment(__int64 value) {
		adjustment_ = value;
	}

	// Returns the conversion to be performed on the value. Used only with pointers.
	InitConversion Conversion() const {
		return conversion_;
	}

	void SetConversion(InitConversion value) {
		conversion_ = value;
	}

    // Returns 'true' if any pointer cast is applied.
    bool HasConversion() const {
        return conversion_ != InitConversion::None;
    }

	// Returns the type to which the value should be converted.
	const Type* ConversionType() const {
		return convType_;
	}

	void SetConversionType(const Type* value) {
		convType_ = value;
	}
};


// Represents the initializer used with arrays and records.
class InitializerList : public List<shared<Initializer>>, public Initializer {
public:
	virtual ~InitializerList() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static shared<InitializerList> GetList() {
		return new InitializerList();
	}

	virtual bool IsInitializerList() const override { 
		return true; 
	}

	virtual void AssociateWith(GlobalVariable* variable) override;

	virtual void DisassociateFrom(GlobalVariable* variable) override;
};


// Represents a variable declared at unit (global) scope.
class GlobalVariable : public Variable {
protected:
	static const int ZERO_INIT_BIT = 0; // 'true' if it's initialized only with zero.
	static const int CONSTANT_BIT  = 1; // 'true' if the variable is constant.

	shared<Initializer> initializer_; // The associated initializer.
	shared<string> section_;          // The section in which the variable should be placed.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	GlobalVariable(const Type* type, const string& name, SymbolTable* parent,
				   SymbolVisibility visibility, shared<Initializer> initializer, 
				   bool listInit, bool zeroInit);

	// Verifies whether the specified bit is set in the 'other_' variable.
	bool BitSet(int n) const {
		return (other_ & (1 << n)) != 0;
	}
	
	// Sets the specified bit in the 'other_' variable.
	void SetBit(int n, bool value) {
		if(value) {
			other_ |= 1 << n;
		}
		else other_ &= ~(1 << n);
	}

	virtual string ToStringImpl(int level) const override;

public:
	// Factory methods for creating global variable instances.
	static GlobalVariable* GetGlobal(const Type* type, const string& name, 
									 shared<Initializer> initializer = nullptr,
									 SymbolTable* parent = nullptr, 
									 SymbolVisibility visibility = SymbolVisibility::Extern);

	static GlobalVariable*
	GetGlobalAggregate(const Type* type, const string& name, 
					   shared<InitializerList> initList = nullptr,
					   SymbolTable* parent = nullptr,
					   SymbolVisibility visibility = SymbolVisibility::Extern);

	static GlobalVariable*
	GetZeroInitialized(const Type* type, const string& name, SymbolTable* parent = nullptr, 
					   SymbolVisibility visibility = SymbolVisibility::Extern);

	virtual ~GlobalVariable() {
		if(initializer_) {
			initializer_->DisassociateFrom(this);
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns a reference that points to the global variable.
    VariableReference* GetReference();

	// Returns 'true' if the variable is initialized only with the value 0.
	// Used with arrays and records.
	bool HasZeroInitializer() const {
		return BitSet(ZERO_INIT_BIT);
	}

	void SetHasZeroInitializer(bool value) {
		SetBit(ZERO_INIT_BIT, value);
		if(value) initializer_ = nullptr;
	}

	// Returns 'true' if the variable is a constant (it must also have an initializer).
	bool IsConstant() const {
		return BitSet(CONSTANT_BIT);
	}

	void SetIsConstant(bool value) {
		SetBit(CONSTANT_BIT, value);
	}

	// Returns 'true' if this is an array or record with an initializer.
	bool HasInitializerList() const {
		if(initializer_ == nullptr) {
			return false;
		}
		else return initializer_->IsInitializerList();
	}

	// Returns 'true' if this is a simple variable with an initializer.
	bool HasSimpleInitializer() const {
		if(initializer_ == nullptr) {
			return false;
		}
		else return HasInitializer() && 
				   (HasInitializerList() == false);
	}

	// Returns 'true' if the variable is initialized.
	bool HasInitializer() const {
		return HasZeroInitializer() || initializer_;
	}

	void SetInitializer(shared<Initializer> value) {
		if(initializer_) {
			initializer_->DisassociateFrom(this);
		}

		initializer_ = value;
		SetHasZeroInitializer(false);

		if(initializer_) {
			initializer_->AssociateWith(this);
		}
	}

	// Sets the initializer list to the specified value.
	// Invalidates any other initializer type.
	void SetInitializerList(shared<InitializerList> value) {
		if(initializer_) {
			initializer_->DisassociateFrom(this);
		}

		initializer_ = value;
		SetHasZeroInitializer(false);

		if(initializer_) {
			initializer_->AssociateWith(this);
		}
	}

	void AddInitializer(shared<Initializer> value) {
		DebugValidator::IsTrue(HasInitializerList());
		value->AssociateWith(this);
		GetInitializerList()->Add(value);
	}

	// Returns the list of initializers. 
	// Should be called only if the variable has an initializer list.
	shared<InitializerList> GetInitializerList() {
		DebugValidator::IsTrue(HasInitializerList());
		return initializer_.As<InitializerList>();
	}

	const shared<InitializerList> GetInitializerList() const {
		DebugValidator::IsTrue(HasInitializerList());
		return initializer_.As<InitializerList>();
	}

	// Returns the simple initializer.
	shared<Initializer> GetInitializer() {
		DebugValidator::IsTrue(HasInitializer());
		return initializer_;
	}

	const shared<Initializer> GetInitializer() const {
		DebugValidator::IsTrue(HasInitializer());
		return initializer_;
	}

	// Returns 'true' if the variable should be put in an indicated section.
	bool HasSection() const {
		return section_ != nullptr;
	}

	// Returns the section in which the variable (and the initializer) should be put.
	shared<string> Section() {
		return section_;
	}

	const shared<string> Section() const {
		return section_;
	}

	void SetSection(shared<string> value) {
		section_ = value;
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<GlobalVariable> {
		static bool Is(const Symbol* symbol) {
			return (Symbol::Kind)symbol->kind_ == Symbol::Kind::GlobalVariable;
		}

		static GlobalVariable* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<GlobalVariable*>(symbol) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif