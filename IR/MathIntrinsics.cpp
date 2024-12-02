// MathIntrinsics.cpp
// Copyright (c) Lup Gratian
//
// Implements the math intrinsics.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "MathIntrinsics.hpp"

namespace IR {

#define math_int_one_double(NAME, TEXT) \
    NAME##Intr* NAME##Intr::Get##NAME(Unit* unit) { \
	    DebugValidator::IsNotNull(unit); \
        if(unit->Intrinsics().Contains(TEXT)) { \
            return static_cast<NAME##Intr*>(unit->Intrinsics().Get(TEXT)); \
	    } \
	    const Type* parameters[1]; \
	    parameters[0] = FloatingType::GetDouble(); \
	    auto functionType = unit->Types().GetFunction(FloatingType::GetDouble(), parameters, 1); \
        auto intrinsic = new NAME##Intr(functionType, new string(TEXT), unit); \
        intrinsic->SetIsNoState(true); \
        intrinsic->SetIsNoIndirectWrite(true); \
		intrinsic->SetIsNoIndirectRead(true); \
	    intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("val"), intrinsic)); \
	    unit->Intrinsics().Add(intrinsic); \
	    return intrinsic; \
    } \

// Most math intrinsics have a 'double' parameter and return a 'double'.
math_int_one_double(Fabs,  "#fabs")
math_int_one_double(Sin,   "#sin")
math_int_one_double(Cos,   "#cos")
math_int_one_double(Log,   "#log")
math_int_one_double(Tan,   "#tan")
math_int_one_double(Atan,  "#atan")
math_int_one_double(Exp,   "#exp")
math_int_one_double(Sqrt,  "#sqrt")
math_int_one_double(Log10, "#log10")
#undef math_int_one_double

// ######################################################################################
// SqrtfInstr
// ######################################################################################
SqrtfIntr* SqrtfIntr::GetSqrtf(Unit* unit) {
	DebugValidator::IsNotNull(unit);
	
	// See if the intrinsic was already created.
	if(unit->Intrinsics().Contains("#sqrtf")) {
		return static_cast<SqrtfIntr*>(unit->Intrinsics().Get("#sqrtf"));
	}

	// Create the function type and the intrinsic.
	const Type* parameters[1];
	parameters[0] = FloatingType::GetFloat();

    auto functionType = unit->Types().GetFunction(FloatingType::GetFloat(), parameters, 1);
	auto intrinsic = new SqrtfIntr(functionType, new string("#sqrtf"), unit);
	intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("val"), intrinsic));
    intrinsic->SetIsNoState(true);
    intrinsic->SetIsNoIndirectWrite(true);
	intrinsic->SetIsNoIndirectRead(true);
	unit->Intrinsics().Add(intrinsic);
	return intrinsic;
}

// ######################################################################################
// PowInstr
// ######################################################################################
PowIntr* PowIntr::GetPow(Unit* unit) {
	DebugValidator::IsNotNull(unit);
	
	// See if the intrinsic was already created.
	if(unit->Intrinsics().Contains("#pow")) {
		return static_cast<PowIntr*>(unit->Intrinsics().Get("#pow"));
	}
    
	// Create the function type and the intrinsic.
	const Type* parameters[2];
	parameters[0] = FloatingType::GetDouble();
    parameters[1] = FloatingType::GetDouble();

    auto functionType = unit->Types().GetFunction(FloatingType::GetDouble(), parameters, 2);
	auto intrinsic = new PowIntr(functionType, new string("#pow"), unit);
	intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("val1"), intrinsic));
    intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("val2"), intrinsic));
    intrinsic->SetIsNoState(true);
    intrinsic->SetIsNoIndirectWrite(true);
	intrinsic->SetIsNoIndirectRead(true);
	unit->Intrinsics().Add(intrinsic);
	return intrinsic;
}

// ######################################################################################
// Atan2Instr
// ######################################################################################
Atan2Intr* Atan2Intr::GetAtan2(Unit* unit) {
    DebugValidator::IsNotNull(unit);

    // See if the intrinsic was already created.
    if(unit->Intrinsics().Contains("#atan2")) {
        return static_cast<Atan2Intr*>(unit->Intrinsics().Get("#atan2"));
    }

    // Create the function type and the intrinsic.
    const Type* parameters[2];
    parameters[0] = FloatingType::GetDouble();
    parameters[1] = FloatingType::GetDouble();

    auto functionType = unit->Types().GetFunction(FloatingType::GetDouble(), parameters, 2);
    auto intrinsic = new Atan2Intr(functionType, new string("#atan2"), unit);
    intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("val1"), intrinsic));
    intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("val2"), intrinsic));
    intrinsic->SetIsNoState(true);
    intrinsic->SetIsNoIndirectWrite(true);
	intrinsic->SetIsNoIndirectRead(true);
    unit->Intrinsics().Add(intrinsic);
    return intrinsic;
}

// ######################################################################################
// Min/Max/AbsInstr
// ######################################################################################
#define abs_intr(NAME, TEXT, TYPE) \
    NAME##Intr* NAME##Intr::Get##NAME(Unit* unit) { \
	    DebugValidator::IsNotNull(unit); \
	    if(unit->Intrinsics().Contains(TEXT)) { \
            return static_cast<NAME##Intr*>(unit->Intrinsics().Get(TEXT)); \
	    } \
	    const Type* parameters[2]; \
        parameters[0] = IntegerType::Get##TYPE(); \
        parameters[1] = IntegerType::GetInt32(); \
        auto functionType = unit->Types().GetFunction(IntegerType::Get##TYPE(), parameters, 2); \
        auto intrinsic = new NAME##Intr(functionType, new string(TEXT), unit); \
	    intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("a"), intrinsic)); \
        intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("sign"), intrinsic)); \
        intrinsic->SetIsNoState(true); \
        intrinsic->SetIsNoIndirectWrite(true); \
		intrinsic->SetIsNoIndirectRead(true); \
	    unit->Intrinsics().Add(intrinsic); \
	    return intrinsic; \
    }

abs_intr(Abs8,  "#abs8",  Int8)
abs_intr(Abs16, "#abs16", Int16)
abs_intr(Abs32, "#abs32", Int32)
abs_intr(Abs64, "#abs64", Int64)
#undef abs_intr

// ######################################################################################
// Min/MaxInstr
// ######################################################################################
#define minmaxabs_intr(NAME, TEXT, TYPE) \
    NAME##Intr* NAME##Intr::Get##NAME(Unit* unit) { \
	    DebugValidator::IsNotNull(unit); \
	    if(unit->Intrinsics().Contains(TEXT)) { \
            return static_cast<NAME##Intr*>(unit->Intrinsics().Get(TEXT)); \
	    } \
	    const Type* parameters[3]; \
        parameters[0] = IntegerType::Get##TYPE(); \
        parameters[1] = IntegerType::Get##TYPE(); \
        parameters[2] = IntegerType::GetInt32(); \
        auto functionType = unit->Types().GetFunction(IntegerType::Get##TYPE(), parameters, 3); \
        auto intrinsic = new NAME##Intr(functionType, new string(TEXT), unit); \
	    intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("a"), intrinsic)); \
        intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("b"), intrinsic)); \
        intrinsic->AddParameter(Variable::GetVariable(parameters[2], new string("sign"), intrinsic)); \
        intrinsic->SetIsNoState(true); \
        intrinsic->SetIsNoIndirectWrite(true); \
		intrinsic->SetIsNoIndirectRead(true); \
	    unit->Intrinsics().Add(intrinsic); \
	    return intrinsic; \
    }

minmaxabs_intr(Min8,  "#min8",  Int8)
minmaxabs_intr(Min16, "#min16", Int16)
minmaxabs_intr(Min32, "#min32", Int32)
minmaxabs_intr(Min64, "#min64", Int64)

minmaxabs_intr(Max8,  "#max8",  Int8)
minmaxabs_intr(Max16, "#max16", Int16)
minmaxabs_intr(Max32, "#max32", Int32)
minmaxabs_intr(Max64, "#max64", Int64)

#undef minmax_intr

} // namespace IR