// References.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_REFERENCES_HPP
#define PC_IR_REFERENCES_HPP

#include "Operand.hpp"
#include "Block.hpp"
#include "Function.hpp"
#include "Variable.hpp"
#include "GlobalVariable.hpp"
#include "Tagged.hpp"

namespace IR {

// Forward references.
class ReferenceTable;

// The base class for references to variables and functions.
class Reference : public Operand, public Tagged<Tag> {
protected:
	ReferenceTable* parent_; // The associated reference table.
	int users_;              // The number of times this reference is used.

	Reference(const Type* type, int kind, Symbol* symbol, ReferenceTable* parent) : 
			Operand(type, kind, symbol), parent_(parent), users_(0) {}

public:
	virtual ~Reference() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Increments the number of users (instructin user).
	void AddUser(Instruction* instr);

	// Increments the number of users (symbol user).
	void AddUser(Symbol* symbol);

	// Increments the number of users (user is unknown).
	void AddUser();

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
	struct OperandPromoter<Reference> {
		static bool Is(const Operand* op) {
			return ((Operand::Kind)op->kind_ == Operand::Kind::VariableRef) || 
				   ((Operand::Kind)op->kind_ == Operand::Kind::BlockRef)    || 
				   ((Operand::Kind)op->kind_ == Operand::Kind::FunctionRef) ||
				   ((Operand::Kind)op->kind_ == Operand::Kind::GlobalVariableRef);
		}

		static Reference* As(Operand* op) {
			return Is(op) ? static_cast<Reference*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents a reference to a variable or to a global variable.
class VariableReference : public Reference {
protected:
	VariableReference(Variable* variable, const Type* type, ReferenceTable* parent);
	VariableReference(GlobalVariable* variable, const Type* type, ReferenceTable* parent);

	virtual void FreeImpl() override;
	virtual void FreeImpl(Instruction* user) override;
	virtual void FreeImpl(Symbol* user) override;
	virtual string ToStringImpl(int level) const override;

public:
	friend class ReferenceTable; // Give it access to the factory methods.

	virtual ~VariableReference() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    // Returns the variable represented by the reference
    // (both local and global variables).
    Variable* GetVariable() {
        return static_cast<Variable*>(symbol_);
    }

    const Variable* GetVariable() const {
        return static_cast<const Variable*>(symbol_);
    }

    // Returns the local variable represented by the reference,
    // or 'nullptr' if the variable is global.
	Variable* GetLocalVariable() {
		return symbol_->As<Variable>();
	}

	const Variable* GetLocalVariable() const {
		return symbol_->As<Variable>();
	}

    // Returns the global variable represented by the reference,
    // or 'nullptr' if the variable is local.
	GlobalVariable* GetGlobalVariable() {
		return symbol_->As<GlobalVariable>();
	}

	const GlobalVariable* GetGlobalVariable() const {
		return symbol_->As<GlobalVariable>();
	}

    // Returns 'true' if the represented variable is global.
	bool IsGlobal() const {
		return symbol_->IsGlobalVariable();
	}

    // Returns 'true' if the address of the variable is taken.
    bool IsAddressTaken() const {
        return symbol_->IsAddressTaken();
    }

    // Returns 'true' if the address of the variable is not taken.
    bool IsAddressNotTaken() const {
        return symbol_->IsAddressTaken() == false;
    }

    // Returns 'true' if the address of the referenced variable 
    // is guaranteed  not to be stored into a global variable 
    // and into other objects that may escape the function.
    bool IsNoEscape() const {
        DebugValidator::IsTrue(symbol_->Is<Variable>());
        return symbol_->As<Variable>()->IsNoEscape();
    }

    // Returns 'true' if the address of the referenced variable 
    // may be stored into a global variable or into objects that
    // may escape the function.
    bool IsEscape() const {
        DebugValidator::IsTrue(symbol_->Is<Variable>());
        return symbol_->As<Variable>()->IsEscape();
    }
};

namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<VariableReference> {
		static bool Is(const Operand* op) {
			return ((Operand::Kind)op->kind_ == Operand::Kind::VariableRef) ||
				   ((Operand::Kind)op->kind_ == Operand::Kind::GlobalVariableRef);
		}
		
		static VariableReference* As(Operand* op) {
			return Is(op) ? static_cast<VariableReference*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents a reference to a basic block.
class BlockReference : public Reference {
protected:
	BlockReference(Block* block, ReferenceTable* parent);

	virtual void FreeImpl() override;
	virtual void FreeImpl(Instruction* user) override;
	virtual void FreeImpl(Symbol* user) override;
	virtual string ToStringImpl(int level) const override;

public:
	friend class ReferenceTable; // Give it access to the factory methods.

	virtual ~BlockReference() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the pointed block.
	Block* Target() {
		return static_cast<Block*>(symbol_);
	}

	const Block* Target() const {
		return static_cast<const Block*>(symbol_);
	}

	void SetTarget(Block* value) {
		symbol_ = value;
	}

	// Returns the function to which the block belongs.
	Function* ParentFunction() {
		return Target()->ParentFunction();
	}

	const Function* ParentFunction() const {
		return Target()->ParentFunction();
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};

namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<BlockReference> {
		static bool Is(const Operand* op) {
			return (Operand::Kind)op->kind_ == Operand::Kind::BlockRef;
		}

		static BlockReference* As(Operand* op) {
			return Is(op) ? static_cast<BlockReference*>(op) : nullptr;
		}
	};
} // namespace Detail


// Represents a reference to a function.
class FunctionReference : public Reference {
protected:
	FunctionReference(Function* function, const Type* type, ReferenceTable* parent);

	virtual void FreeImpl() override;
	virtual void FreeImpl(Instruction* user) override;
	virtual void FreeImpl(Symbol* user) override;
	virtual string ToStringImpl(int level) const override;

public:
	friend class ReferenceTable; // Give it access to the factory methods.

	virtual ~FunctionReference() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the pointed function.
	Function* Target() {
		return static_cast<Function*>(symbol_);
	}

	const Function* Target() const {
		return static_cast<const Function*>(symbol_);
	}

	// Returns 'true' if the target is an intrinsic.
	bool IsIntrinsic() const {
		return Target()->IsIntrinsic();
	}

    // Returns 'true' if the function is a definition (has a body).
    bool IsDefinition() const {
        return Target()->IsDefinition();
    }

    // Returns 'true' if the function is a declaration (doesn't have a body).
    bool IsDeclaration() const {
        return Target()->IsDefinition() == false;
    }

    // Returns 'true' if the address of the function is taken.
    bool IsAddressTaken() const {
        return Target()->IsAddressTaken();    
    }

    // Returns 'true' if the address of the function is not taken.
    bool IsAddressNotTaken() const {
        return symbol_->IsAddressTaken() == false;
    }

    // Returns 'true' if the function is visible to all units.
    bool IsExtern() const {
        return Target()->IsExtern();
    }

    // Returns 'true' if the function is visible only in the parent unit.
    bool IsStatic() const {
        return Target()->IsStatic();
    }

    // Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};

namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct OperandPromoter<FunctionReference> {
		static bool Is(const Operand* op) {
			return (Operand::Kind)op->kind_ == Operand::Kind::FunctionRef;
		}

		static FunctionReference* As(Operand* op) {
			return Is(op) ? static_cast<FunctionReference*>(op) : nullptr;
		}
	};
} // namespace Detail

} // namespace IR
#endif