// GeneratorEvents.hpp
// Copyright (c) Lup Gratian
//
// Implements an interface that allows other objects to monitor the IR generation.
// For example, the observer is notified before a 'load' is emitted, so it can
// insert array bounds/null-pointer checks before it.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_GENERATOR_GENERATOR_EVENTS_HPP
#define PC_IR_GENERATOR_GENERATOR_EVENTS_HPP

#include "../Base/List.hpp"
#include "../Base/DebugValidator.hpp"
#include "../AST/Unit.hpp"
#include "../AST/Types.hpp"
#include "../AST/Declarations.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/Block.hpp"
using namespace Base;
using namespace AST;

namespace IRGenerator {

// Forward declarations.
class FunctionGenerator;


// Describes the context at the moment the event happened.
struct GeneratorContext {
	Unit* CurrentUnit;
	const FunctionDeclaration* CurrentFunction;
    const Expression* CurrentExpression;
	FunctionGenerator* CurrentGenerator;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	GeneratorContext() {}

	GeneratorContext(Unit* unit, const FunctionDeclaration* function, 
                     FunctionGenerator* gen, const Expression* expr = nullptr) :
			CurrentUnit(unit), CurrentFunction(function), CurrentGenerator(gen),
            CurrentExpression(expr) {}

	GeneratorContext(const GeneratorContext& other) :
			CurrentUnit(other.CurrentUnit), CurrentFunction(other.CurrentFunction),
			CurrentGenerator(other.CurrentGenerator), 
            CurrentExpression(other.CurrentExpression) {}
};


// The type of the generated event.
enum class GeneratorEvent {
    FunctionBegin,
    FunctionEnd,
	Load,
	Store,
	Call,
	Arithmetic,
	Logical,
	Comparison,
	If,
	Loop,
	END
};


// The type of the loop for which the event was generated.
enum class LoopKind {
	While,
	For,
	Do,
    Infinite // A 'while'/'for'/'do' that was simplified.
};


// Class from which all classes interested in generator events must derive.
class GeneratorObserver {
public:
	virtual ~GeneratorObserver() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// The observer should return 'true' if it's interested in the specified event type.
	// This is called when the observer is registered.
	virtual bool IsInterestedIn(GeneratorEvent genEvent) {
		return false;
	}

    virtual void FunctionBegin(GeneratorContext& context) {}
    virtual void FunctionEnd(GeneratorContext& context) {}

	// Before/after handlers for each of the events; by default no action is taken.
	virtual void BeforeLoad(GeneratorContext& context, IR::Operand* op,
                            const Type* sourceType) {}
	virtual void AfterLoad(GeneratorContext& context, IR::LoadInstr* instr,
                           const Type* sourceType) {}

	virtual void BeforeStore(GeneratorContext& context, IR::Operand* op,
                             const Type* destType, const Type* sourceType) {}
	virtual void AfterStore(GeneratorContext& context, IR::StoreInstr* instr,
                            const Type* destType, const Type* sourceType) {}

	virtual void BeforeCall(GeneratorContext& context, IR::Operand* op) {}
	virtual void AfterCall(GeneratorContext& context, IR::CallInstr* instr) {}

	virtual void BeforeIf(GeneratorContext& context, IR::Operand* op) {}
	virtual void AfterIf(GeneratorContext& context, IR::IfInstr* instr,
						 IR::Block* trueBlock, IR::Block* falseBlock) {}

	virtual void BeforeLoop(GeneratorContext& context, LoopKind loopKind) {}
	virtual void AfterLoop(GeneratorContext& context, LoopKind loopKind,
						   IR::Block* headerBlock, IR::Block* bodyBlock, 
						   IR::Block* incrementBlock) {}
};


// Propagates events to all the registered generator observers.
class GeneratorEvents {
private:
	List<GeneratorObserver*> observers_;
	int observersEvent_[GeneratorEvent::END]; // The number of observers interested in the event.

public:
	GeneratorEvents() {
		for(int i = 0; i < (int)GeneratorEvent::END; i++) {
			observersEvent_[i] = 0;
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Registers the specified object who wants to be notified about events.
	void AddObserver(GeneratorObserver* observer) {
		DebugValidator::IsNotNull(observer);
		observers_.Add(observer);

		// Add the observer to the "interested event" array.
		for(int i = 0; i < (int)GeneratorEvent::END; i++) {
			if(observer->IsInterestedIn((GeneratorEvent)i)) {
				observersEvent_[i]++;
			}
		}
	}

	// Unregisters the specified observer, so that it no longer receives events.
	void RemoveObserver(GeneratorObserver* observer) {
		DebugValidator::IsNotNull(observer);
		observers_.Remove(observer);

		// Remove the observer from the "interested event" array.
		for(int i = 0; i < (int)GeneratorEvent::END; i++) {
			if(observer->IsInterestedIn((GeneratorEvent)i)) {
				observersEvent_[i]--;
			}
		}
	}

    void FunctionBegin(GeneratorContext& context) {
        if(observersEvent_[(int)GeneratorEvent::FunctionBegin] > 0) {
            for(int i = 0; i < observers_.Count(); i++) {
                observers_[i]->FunctionBegin(context);
            }
        }
    }

    void FunctionEnd(GeneratorContext& context) {
        if(observersEvent_[(int)GeneratorEvent::FunctionEnd] > 0) {
            for(int i = 0; i < observers_.Count(); i++) {
                observers_[i]->FunctionEnd(context);
            }
        }
    }

	void BeforeLoad(GeneratorContext& context, IR::Operand* op,
                    const Type* sourceType) {
		if(observersEvent_[(int)GeneratorEvent::Load] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->BeforeLoad(context, op, sourceType);
			}
		}
	}

	void AfterLoad(GeneratorContext& context, IR::LoadInstr* instr,
                   const Type* sourceType) {
		if(observersEvent_[(int)GeneratorEvent::Load] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->AfterLoad(context, instr, sourceType);
			}
		}
	}

	void BeforeStore(GeneratorContext& context, IR::Operand* op,
                     const Type* destType, const Type* sourceType) {
		if(observersEvent_[(int)GeneratorEvent::Store] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->BeforeStore(context, op, destType, sourceType);
			}
		}
	}

	void AfterStore(GeneratorContext& context, IR::StoreInstr* instr,
                    const Type* destType, const Type* sourceType) {
		if(observersEvent_[(int)GeneratorEvent::Store] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->AfterStore(context, instr, destType, sourceType);
			}
		}
	}

	void BeforeCall(GeneratorContext& context, IR::Operand* op) {
		if(observersEvent_[(int)GeneratorEvent::Call] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->BeforeCall(context, op);
			}
		}
	}

	void AfterCall(GeneratorContext& context, IR::CallInstr* instr) {
		if(observersEvent_[(int)GeneratorEvent::Call] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->AfterCall(context, instr);
			}
		}
	}

	void BeforeIf(GeneratorContext& context, IR::Operand* op) {
		if(observersEvent_[(int)GeneratorEvent::If] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->BeforeIf(context, op);
			}
		}
	}

	void AfterIf(GeneratorContext& context, IR::IfInstr* instr,
				 IR::Block* trueBlock, IR::Block* falseBlock) {
		if(observersEvent_[(int)GeneratorEvent::If] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->AfterIf(context, instr, trueBlock, falseBlock);
			}
		}
	}

	void BeforeLoop(GeneratorContext& context, LoopKind loopKind) {
		if(observersEvent_[(int)GeneratorEvent::Loop] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->BeforeLoop(context, loopKind);
			}
		}
	}

	void AfterLoop(GeneratorContext& context, LoopKind loopKind, 
                   IR::Block* headerBlock, IR::Block* bodyBlock,
                   IR::Block* incrementBlock) {
		if(observersEvent_[(int)GeneratorEvent::Loop] > 0) {
			for(int i = 0; i < observers_.Count(); i++) {
				observers_[i]->AfterLoop(context, loopKind, headerBlock,
										 bodyBlock, incrementBlock);
			}
		}
	}
};

} // namespace IRGenerator
#endif