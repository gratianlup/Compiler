// OtherIntrinsics.cpp
// Copyright (c) Lup Gratian
//
// 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "OtherIntrinsics.hpp"

namespace IR {

#define rotate_intr(NAME, TEXT, TYPE) \
    Rotate##NAME##Intr* Rotate##NAME##Intr::GetRotate##NAME(Unit* unit) { \
	    DebugValidator::IsNotNull(unit); \
        if(unit->Intrinsics().Contains(TEXT)) { \
            return static_cast<Rotate##NAME##Intr*>(unit->Intrinsics().Get(TEXT)); \
	    } \
	    const Type* parameters[2]; \
        parameters[0] = IntegerType::Get##TYPE(); \
        parameters[1] = IntegerType::Get##TYPE(); \
        auto functionType = unit->Types().GetFunction(IntegerType::Get##TYPE(), parameters, 2); \
        auto intrinsic = new Rotate##NAME##Intr(functionType, new string(TEXT), unit); \
	    intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("a"), intrinsic)); \
        intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("b"), intrinsic)); \
        intrinsic->SetIsNoState(true); \
        intrinsic->SetIsNoIndirectWrite(true); \
		intrinsic->SetIsNoIndirectRead(true); \
	    unit->Intrinsics().Add(intrinsic); \
	    return intrinsic; \
    } \

rotate_intr(Left8, "#rotateLeft8", Int8)
rotate_intr(Left16, "#rotateLeft16", Int16)
rotate_intr(Left32, "#rotateLeft32", Int32)
rotate_intr(Left64, "#rotateLeft64", Int64)
#undef rotate_intr


#define byteswap_intr(NAME, TEXT, TYPE) \
    ByteSwap##NAME##Intr* ByteSwap##NAME##Intr::GetByteSwap##NAME(Unit* unit) { \
	    DebugValidator::IsNotNull(unit); \
        if(unit->Intrinsics().Contains(TEXT)) { \
            return static_cast<ByteSwap##NAME##Intr*>(unit->Intrinsics().Get(TEXT)); \
	    } \
	    const Type* parameters[1]; \
        parameters[0] = IntegerType::Get##TYPE(); \
        auto functionType = unit->Types().GetFunction(IntegerType::Get##TYPE(), parameters, 1); \
        auto intrinsic = new ByteSwap##NAME##Intr(functionType, new string(TEXT), unit); \
	    intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("a"), intrinsic)); \
        intrinsic->SetIsNoState(true); \
        intrinsic->SetIsNoIndirectWrite(true); \
		intrinsic->SetIsNoIndirectRead(true); \
	    unit->Intrinsics().Add(intrinsic); \
	    return intrinsic; \
    } \

byteswap_intr(16, "#byteSwap16", Int16)
byteswap_intr(32, "#byteSwap32", Int32)
byteswap_intr(64, "#byteSwap64", Int64)
#undef byteswap_intr


// ######################################################################################
// BoundsCheckIntrinsic
// ######################################################################################
BoundsCheckIntrinsic* BoundsCheckIntrinsic::GetBoundsCheck(Unit* unit) {
    DebugValidator::IsNotNull(unit);

    // See if the intrinsic was already created.
    if(unit->Intrinsics().Contains("#boundsCheck")) {
        return static_cast<BoundsCheckIntrinsic*>(unit->Intrinsics().Get("#boundsCheck"));
    }

    // Create the intrinsic now. The function has the following form:
    // funct boundsCheck(var cond int32, var file int8*, var name int8*, var line int32) : int32
    const Type* parameters[4];
    parameters[0] = IntegerType::GetInt32();
    parameters[1] = unit->Types().GetPointer(IntegerType::GetInt8());
    parameters[2] = unit->Types().GetPointer(IntegerType::GetInt8());
    parameters[3] = IntegerType::GetInt32();

    // Create the function type and the intrinsic.
    auto functionType = unit->Types().GetFunction(IntegerType::GetInt32(), parameters, 4);
    auto intrinsic = new BoundsCheckIntrinsic(functionType, new string("#boundsCheck"), unit);
    unit->Intrinsics().Add(intrinsic);

    // Add the parameters.
    intrinsic->AddParameter(Variable::GetVariable(parameters[0], new string("cond"), intrinsic));
    intrinsic->AddParameter(Variable::GetVariable(parameters[1], new string("file"), intrinsic));
    intrinsic->Parameters()[1]->SetIsNoWrite(true);
    intrinsic->Parameters()[1]->SetIsNoEscape(true);

    intrinsic->AddParameter(Variable::GetVariable(parameters[2], new string("name"), intrinsic));
    intrinsic->Parameters()[2]->SetIsNoWrite(true);
    intrinsic->Parameters()[2]->SetIsNoEscape(true);

    intrinsic->AddParameter(Variable::GetVariable(parameters[3], new string("line"), intrinsic));
    return intrinsic;
}

} // namespace IR