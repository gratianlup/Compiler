// Tag.hpp
// Copyright (c) Lup Gratian
//
// A 'tag' stores additional information that can be attached
// to an instruction, operand or symbol (for example, location in source file).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_TAG_HPP
#define PC_IR_TAG_HPP

#include "Visitor.hpp"
#include "../Base/String.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
using namespace Base;

namespace IR {

// Forward declaration.
class Tag;
class Instruction;
class Block;
class Function;
class Operand;
class Variable;

class TaggedBase;

template <class T>
class Tagged;

namespace Detail {
	// Used for implementing 'As<T>'/'Is<T>'.
	template <class T>
	struct TagPromoter {
		static bool Is(const Tag* op) {
			static_assert(false, "Type is not an Tag in Is<T>");
			return false;
		}

		static T* As(Tag* op) {
			static_assert(false, "Type is not an Tag in As<T>");
			return nullptr;
		}
	};
} // namespace Detail

// Base class from which all tags need to derive.
// Each tag class shall have the following public member and method
// which is used to identify the tag class.
// static const int Id = UNIQUE_ID;
// virtual int GetId() const;
class Tag : public Visitable {
private:
	Tag(const Tag&);			 // Should not be copied.
	Tag& operator= (const Tag&); // Should not be assigned.

protected:
	// Should generate a string that describes the tag object.
	virtual string ToStringImpl(int level) const {
		return "";
	}

public:
	Tag() {}

	virtual ~Tag() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const = 0;

	// Frees the memory associated with this tag object.
	virtual void Free() {
		delete this;
	}

    // Method called when the tag is attached to an object.
    virtual void AttachedToParent(TaggedBase* parent) {}

    // This method is called when the tag is attached to an object
    // or when its parent is removed.
    virtual void DetachedFromParent(TaggedBase* parent) {}

    //! TODO: virtual bool Serialize(...);
    //! TODO: virtual bool Deserialize(...);

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return Detail::TagPromoter<T>::As(this);
	}

	template <class T>
	const T* As() const {
		return Detail::TagPromoter<T>::As(const_cast<Tag*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return Detail::TagPromoter<T>::Is(this);
	}

	// Returns a string representation of the tag information.
	string ToString(int level = 0) const {
		return ToStringImpl(level);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


// Base for tags that are associated with instructions.
class InstructionTag : public Tag {
public:
    // Method called when an operand has been added
    // to the instruction associated with the tag.
    virtual void OperandAdded(Operand* op, int index, Instruction* parent) {}

    // Method called when an operand has been removed
    // to the instruction associated with the tag.
    virtual void OperandRemoved(Operand* op, int index, Instruction* parent) {}

    // Method called when the associated instruction
    // has been added to a new block.
    virtual void AddedToBlock(Block* block, Instruction* parent) {}

    // Method called when the associated instruction
    // was removed from its parent block.
    virtual void RemovedFromBlock(Block* block, Instruction* parent) {}
};


// Base for tags that are associated with blocks.
class BlockTag : public Tag {
public:
    // Method called when an instruction has been added
    // to the block associated with the tag.
    virtual void InstructionAdded(Instruction* instr, Block* parent) {}

    // Method called when an instruction has been removed
    // from the block associated with the tag.
    virtual void InstructionRemoved(Instruction* instr, Block* parent) {}

    // Method called when the associated block
    // has been added to a new function.
    virtual void AddedToFunction(Function* function, Block* parent) {}

    // Method called when the associated block
    // was removed from its parent function.
    virtual void RemovedFromFunction(Function* function, Block* parent) {}

    // Method called when a new parameter has been added
    // to the function which is the block parent.
    virtual void ParameterAdded(Variable* parameter, Function* parent) {}

    // Method called when a parameter has been removed
    // from the function which is the block parent.
    virtual void ParameterRemoved(Variable* parameter, Function* parent) {}

    // Method called when a new variable has been added
    // to the function which is the block parent.
    virtual void VariableAdded(Variable* variable, Function* parent) {}

    // Method called when a variable has been removed
    // from the function which is the block parent.
    virtual void VariableRemoved(Variable* variable, Function* parent) {}
};


// Base for tags that are associated with functions.
class FunctionTag : public Tag {
public:
    // Method called when an instruction has been added
    // to the block associated with the tag.
    virtual void BlockAdded(Block* block, Function* parent) {}

    // Method called when an instruction has been removed
    // from the block associated with the tag.
    virtual void BlockRemoved(Block* block, Function* parent) {}

    // Method called when a new parameter has been added
    // to the function associated with the tag.
    virtual void ParameterAdded(Variable* parameter, Function* parent) {}

    // Method called when a parameter has been removed
    // from the function associated with the tag.
    virtual void ParameterRemoved(Variable* parameter, Function* parent) {}

    // Method called when a new variable has been added
    // to the function associated with the tag.
    virtual void VariableAdded(Variable* variable, Function* parent) {}

    // Method called when a variable has been removed
    // from the function associated with the tag.
    virtual void VariableRemoved(Variable* variable, Function* parent) {}
};

} // namespace IR
#endif