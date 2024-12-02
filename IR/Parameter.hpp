// Parameter.hpp
// Copyright (c) Lup Gratian
//
// Defines the Parameter operand, which is used to represent
// the value of a parameter after SSA conversion.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_PARAMETER_HPP
#define PC_IR_PARAMETER_HPP

#include "Operand.hpp"
#include "Variable.hpp"
#include "Tagged.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"

namespace IR {

// Forward declarations.
class ParameterTable;

class Parameter : public Operand, public Tagged<Tag> {
private:
	ParameterTable* parent_;
	int users_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Parameter(Variable* variable, ParameterTable* parent) :
			Operand(variable->GetType(), (int)Kind::Parameter, variable),
			parent_(parent), users_(0) {}

	virtual void FreeImpl();
	virtual string ToStringImpl(int level) const override;

public:
	friend class ParameterTable; // Give it access to the factory methods.

	virtual ~Parameter() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Variable* GetVariable() {
		return symbol_->As<Variable>();
	}

	const Variable* GetVariable() const {
		return symbol_->As<Variable>();
	}

    bool IsRestrict() const {
        return GetVariable()->IsRestrict();
    }

    bool IsNoWrite() const {
        return GetVariable()->IsNoWrite();
    }

    bool IsNoEscape() const {
        return GetVariable()->IsNoEscape();
    }

	// Increments the number of users.
	void AddUser() {
		users_++;
	}

	// Returns the number of times this reference is used.
	int UserCount() const {
		return users_;
	}

    // Implements the visitor pattern.
    virtual void Accept(Visitor* v) override {
        v->Visit(this);
    }
};

namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<Parameter> {
		static bool Is(const Operand* op) {
			return (Operand::Kind)op->kind_ == Operand::Kind::Parameter;
		}

		static Parameter* As(Operand* op) {
			return Is(op) ? static_cast<Parameter*>(op) : nullptr;
		}
	};
} // namespace Detail


// Defines a table used to store the unique instances of the parameters.
class ParameterTable {
private:
	Dictionary<Variable*, Parameter*> params_;

public:
	~ParameterTable();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Returns a parameter that can be used to access the value
	// of the specified variable.
	Parameter* GetParameter(Variable* variable);

	// Releases the reference of the specified parameter.
	void ReleaseParameter(Parameter* parameter);
};

} // namespace IR
#endif