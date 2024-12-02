// MathIntrinsics.hpp
// Copyright (c) Lup Gratian
//
// Defines the intrinsics related to math functions from the standard library.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_MATH_INTRINSICS_HPP
#define PC_IR_MATH_INTRINSICS_HPP

#include "Intrinsic.hpp"
#include "Unit.hpp"
#include "IRTypes.hpp"
#include "Function.hpp"
#include "Variable.hpp"
#include "ControlInstructions.hpp"

namespace IR {

// Defines the kinds of supported math intrinsics.
enum class MathIntrinsicKind {
    Min8,
    Min16,
    Min32,
    Min64,
    Max8,
    Max16,
    Max32,
    Max64,
    Abs8,
    Abs16,
    Abs32,
    Abs64,
    Fabs,
    Atan,
    Exp,
    Pow,
    Log10,
    Sqrt,
    Sqrtf,
    Log,
    Sin,
    Tan,
    Cos,
    Atan2
};


// Defines the base class for all the math intrinsics.
class MathIntrinsic : public Intrinsic {
protected:
	MathIntrinsicKind intrKind_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	MathIntrinsic(MathIntrinsicKind kind, const FunctionType* type,
                  shared<string> name, Unit* parent) :
			Intrinsic(IntrinsicKind::Math, type, name, parent), intrKind_(kind) {}

public:
    virtual bool DoesWriteToMemory() const override {
        return false;
    }

    virtual bool DoesReadFromMemory() const override {
        return false;
    }

    // Returns the kind of the math intrinsic.
    MathIntrinsicKind GetMathKind() const {
        return intrKind_;
    }

    // Returns 'true' if this is any of the 'min' intrinsics.
    bool IsMin() const {
        return (intrKind_ == MathIntrinsicKind::Min8)  || 
			   (intrKind_ == MathIntrinsicKind::Min16) ||
               (intrKind_ == MathIntrinsicKind::Min32) || 
			   (intrKind_ == MathIntrinsicKind::Min64);
    }

    // Returns 'true' if this is any of the 'max' intrinsics.
    bool IsMax() const {
        return (intrKind_ == MathIntrinsicKind::Max8)  || 
			   (intrKind_ == MathIntrinsicKind::Max16) ||
               (intrKind_ == MathIntrinsicKind::Max32) || 
			   (intrKind_ == MathIntrinsicKind::Max64);
    }

    // Returns 'true' if this is any of the 'abs' intrinsics.
    bool IsAbs() const {
        return (intrKind_ == MathIntrinsicKind::Abs8)  || 
			   (intrKind_ == MathIntrinsicKind::Abs16) ||
               (intrKind_ == MathIntrinsicKind::Abs32) || 
			   (intrKind_ == MathIntrinsicKind::Abs64);
    }

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};

namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<MathIntrinsic> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<Intrinsic>()) {
                return temp->IsMathIntrinsic();
			}

			return false;
		}

		static MathIntrinsic* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<MathIntrinsic*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// Used to define the math intrinsics. We use a macro because the only difference
// is the name of the intrinsics.
#define math_intr_two(NAME) \
    class NAME##Intr : public MathIntrinsic { \
    protected: \
        NAME##Intr(const FunctionType* type, shared<string> name, Unit* parent) : \
                MathIntrinsic(MathIntrinsicKind::##NAME, type, name, parent) {} \
    public: \
        static NAME##Intr* Get##NAME(Unit* unit); \
        \
        Operand* GetFirstValue(CallInstr* instr) { \
            return instr->GetArgument(0); \
        } \
        Operand* GetSecondValue(CallInstr* instr) { \
            return instr->GetArgument(1); \
        } \
        \
	    virtual void Accept(Visitor* v) override { \
		    v->Visit(this); \
	    } \
    }; \
    \
    namespace Detail {\
	    template <>\
        struct SymbolPromoter<NAME##Intr> {\
		    static bool Is(const Symbol* symbol) {\
			    if(auto temp = symbol->As<MathIntrinsic>()) {\
                    return temp->GetMathKind() == MathIntrinsicKind::##NAME;\
			    }\
			    return false;\
		    }\
            \
            static NAME##Intr* As(Symbol* symbol) {\
                return Is(symbol) ? static_cast<NAME##Intr*>(symbol) : nullptr;\
		    }\
	    };\
    }

// The definitions of the math intrinsics.
math_intr_two(Min8)
math_intr_two(Min16)
math_intr_two(Min32)
math_intr_two(Min64)
math_intr_two(Max8)
math_intr_two(Max16)
math_intr_two(Max32)
math_intr_two(Max64)
#undef math_intr_two


#define math_intr_one(NAME) \
class NAME##Intr : public MathIntrinsic { \
    protected: \
        NAME##Intr(const FunctionType* type, shared<string> name, Unit* parent) : \
                MathIntrinsic(MathIntrinsicKind::##NAME, type, name, parent) {} \
    public: \
        static NAME##Intr* Get##NAME(Unit* unit); \
        \
        Operand* GetValue(CallInstr* instr) { \
            return instr->GetArgument(0); \
        } \
        \
	    virtual void Accept(Visitor* v) override { \
		    v->Visit(this); \
	    } \
    }; \
    \
    namespace Detail {\
	    template <>\
        struct SymbolPromoter<NAME##Intr> {\
		    static bool Is(const Symbol* symbol) {\
			    if(auto temp = symbol->As<MathIntrinsic>()) {\
                    return temp->GetMathKind() == MathIntrinsicKind::##NAME;\
			    }\
			    return false;\
		    }\
            \
            static NAME##Intr* As(Symbol* symbol) {\
                return Is(symbol) ? static_cast<NAME##Intr*>(symbol) : nullptr;\
		    }\
	    };\
    }

math_intr_one(Abs8)
math_intr_one(Abs16)
math_intr_one(Abs32)
math_intr_one(Abs64)
math_intr_one(Fabs)
math_intr_one(Atan)
math_intr_one(Exp)
math_intr_one(Pow)
math_intr_one(Log10)
math_intr_one(Sqrt)
math_intr_one(Sqrtf)
math_intr_one(Log)
math_intr_one(Sin)
math_intr_one(Tan)
math_intr_one(Cos)
math_intr_one(Atan2)
#undef math_intr

} // namespace IR
#endif