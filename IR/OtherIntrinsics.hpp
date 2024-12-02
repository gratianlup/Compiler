// OtherIntrinsics.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_OTHER_INTRINSICS_HPP
#define PC_IR_OTHER_INTRINSICS_HPP

#include "Intrinsic.hpp"
#include "Unit.hpp"
#include "IRTypes.hpp"
#include "Function.hpp"
#include "Variable.hpp"
#include "ControlInstructions.hpp"

namespace IR {

// Defines the types of implemented bitwise intrinsics.
enum class BitwiseIntrinsicKind {
    Rotate,
    ByteSwap
};

// The base class for all intrinsics that operate on bits and bytes.
class BitwiseIntrinsic : public Intrinsic {
protected:
    BitwiseIntrinsicKind intrKind_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	BitwiseIntrinsic(BitwiseIntrinsicKind kind, const FunctionType* type,
                  shared<string> name, Unit* parent) :
			Intrinsic(IntrinsicKind::Bitwise, type, name, parent), intrKind_(kind) {}

public:
    BitwiseIntrinsicKind GetBitwiseKind() const {
        return intrKind_;
    }

    // Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<BitwiseIntrinsic> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<Intrinsic>()) {
                return temp->IsBitwiseIntrinsic();
			}

			return false;
		}

		static BitwiseIntrinsic* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<BitwiseIntrinsic*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// Defines the types of 'rotate' intrinsics.
enum class RotateIntrinsicKind {
    Left8,
    Left16,
    Left32,
    Left64,
    Right8,
    Right16,
    Right32,
    Right64
};

// Defines an intrinsic that rotates a number to the left
// or to the right by the specified number of bit positions.
// function rotateLeft32(var a int32, var pos int32) : int32
class RotateIntr : public BitwiseIntrinsic {
protected:
    RotateIntrinsicKind intrKind_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	RotateIntr(RotateIntrinsicKind kind, const FunctionType* type,
               shared<string> name, Unit* parent) :
			BitwiseIntrinsic(BitwiseIntrinsicKind::Rotate, type, name, parent),
			intrKind_(kind) {}

public:
    virtual bool DoesWriteToMemory() const override {
        return false;
    }

    virtual bool DoesReadFromMemory() const override {
        return false;
    }

     // Returns the kind of the rotate intrinsic.
    RotateIntrinsicKind GetRotateKind() const {
        return intrKind_;
    }

    bool IsRotateLeft() const {
        return (intrKind_ == RotateIntrinsicKind::Left8)  || 
			   (intrKind_ == RotateIntrinsicKind::Left16) ||
               (intrKind_ == RotateIntrinsicKind::Left32) || 
			   (intrKind_ == RotateIntrinsicKind::Left64);
    }

    bool IsRotateRight() const {
        return (intrKind_ == RotateIntrinsicKind::Right8)  || 
			   (intrKind_ == RotateIntrinsicKind::Right16) ||
               (intrKind_ == RotateIntrinsicKind::Right32) || 
			   (intrKind_ == RotateIntrinsicKind::Right64);
    }

    // Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<RotateIntr> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<BitwiseIntrinsic>()) {
                return temp->GetBitwiseKind() == BitwiseIntrinsicKind::Rotate;
			}

			return false;
		}

		static RotateIntr* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<RotateIntr*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// Macro to create the 'rotate' intrinsics.
#define rotate_intr(NAME) \
    class Rotate##NAME##Intr : public RotateIntr { \
    protected: \
        Rotate##NAME##Intr(const FunctionType* type, shared<string> name, Unit* parent) : \
                RotateIntr(RotateIntrinsicKind::##NAME, type, name, parent) {} \
    public: \
        static Rotate##NAME##Intr* GetRotate##NAME(Unit* unit); \
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
        struct SymbolPromoter<Rotate##NAME##Intr> {\
		    static bool Is(const Symbol* symbol) {\
			    if(auto temp = symbol->As<RotateIntr>()) {\
                    return temp->GetRotateKind() == RotateIntrinsicKind::##NAME;\
			    }\
			    return false;\
		    }\
            \
            static Rotate##NAME##Intr* As(Symbol* symbol) {\
                return Is(symbol) ? static_cast<Rotate##NAME##Intr*>(symbol) : nullptr;\
		    }\
	    };\
    }

rotate_intr(Left8)
rotate_intr(Left16)
rotate_intr(Left32)
rotate_intr(Left64)
#undef rotate_intr


// Defines the types of 'byteSwap' intrinsics.
enum ByteSwapIntrinsicKind {
    ByteSwapIntr_16,
    ByteSwapIntr_32,
    ByteSwapIntr_64
};

// Defines an intrinsic that can be used to swap the order of the bytes in a number.
// funct byteSwap(var a int32) : int32
class ByteSwapIntr : public BitwiseIntrinsic {
protected:
    ByteSwapIntrinsicKind intrKind_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ByteSwapIntr(ByteSwapIntrinsicKind kind, const FunctionType* type,
                 shared<string> name, Unit* parent) :
			BitwiseIntrinsic(BitwiseIntrinsicKind::ByteSwap, type, name, parent), 
			intrKind_(kind) {}

public:
    virtual bool DoesWriteToMemory() const override {
        return false;
    }

    virtual bool DoesReadFromMemory() const override {
        return false;
    }

    // Returns the kind of the byte swap intrinsic.
    ByteSwapIntrinsicKind GetByteSwapKind() const {
        return intrKind_;
    }

    // Implements the visitor pattern.
	virtual void Accept(Visitor* v) override {
		v->Visit(this);
	}
};


namespace Detail {
	// Implements support for "dynamic cast".
	template <>
	struct SymbolPromoter<ByteSwapIntr> {
		static bool Is(const Symbol* symbol) {
			if(auto temp = symbol->As<BitwiseIntrinsic>()) {
                return temp->GetBitwiseKind() == BitwiseIntrinsicKind::ByteSwap;
			}

			return false;
		}

		static ByteSwapIntr* As(Symbol* symbol) {
			return Is(symbol) ? static_cast<ByteSwapIntr*>(symbol) : nullptr;
		}
	};
} // namespace Detail


// Macro to create the 'byteSwap' intrinsics.
#define byteswap_intr(NAME) \
    class ByteSwap##NAME##Intr : public ByteSwapIntr { \
    protected: \
        ByteSwap##NAME##Intr(const FunctionType* type, shared<string> name, Unit* parent) : \
                ByteSwapIntr(ByteSwapIntr_##NAME, type, name, parent) {} \
    public: \
        static ByteSwap##NAME##Intr* GetByteSwap##NAME(Unit* unit); \
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
        struct SymbolPromoter<ByteSwap##NAME##Intr> {\
		    static bool Is(const Symbol* symbol) {\
			    if(auto temp = symbol->As<ByteSwapIntr>()) {\
                    return temp->GetByteSwapKind() == ByteSwapIntr_##NAME;\
			    }\
			    return false;\
		    }\
            \
            static ByteSwap##NAME##Intr* As(Symbol* symbol) {\
                return Is(symbol) ? static_cast<ByteSwap##NAME##Intr*>(symbol) : nullptr;\
		    }\
	    };\
    }

byteswap_intr(16)
byteswap_intr(32)
byteswap_intr(64)
#undef byteswap_intr

// The base class for all intrinsics that operate on bits and bytes.
// funct boundsCheck(var cond int32, var file int8*, var name int8*, var line int32) : int32
class BoundsCheckIntrinsic : public Intrinsic {
protected:
    BoundsCheckIntrinsic(const FunctionType* type, shared<string> name, Unit* parent) :
            Intrinsic(IntrinsicKind::BoundsCheck, type, name, parent) {}

public:
    // Returns the unique instance of the intrinsic.
    static BoundsCheckIntrinsic* GetBoundsCheck(Unit* unit);

    Operand* GetCondition(CallInstr* instr) {
        return instr->GetArgument(0);
    }

    Operand* GetFile(CallInstr* instr) {
        return instr->GetArgument(1);
    }

    Operand* GetName(CallInstr* instr) {
        return instr->GetArgument(2);
    }

    Operand* GetLine(CallInstr* instr) {
        return instr->GetArgument(3);
    }

    // Implements the visitor pattern.
    virtual void Accept(Visitor* v) override {
        v->Visit(this);
    }
};


namespace Detail {
    // Implements support for "dynamic cast".
    template <>
    struct SymbolPromoter<BoundsCheckIntrinsic> {
        static bool Is(const Symbol* symbol) {
            if(auto temp = symbol->As<Intrinsic>()) {
                return temp->IsBoundsCheckIntrinsic();
            }

            return false;
        }

        static BoundsCheckIntrinsic* As(Symbol* symbol) {
            return Is(symbol) ? static_cast<BoundsCheckIntrinsic*>(symbol) : nullptr;
        }
    };
} // namespace Detail

} // namespace IR
#endif