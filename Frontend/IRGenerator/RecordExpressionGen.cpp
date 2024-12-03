// ExpressionRecordGen.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

__int64 ExpressionGenerator::GetBitfieldMask(FieldInfo& fieldInfo) {
	// We create a mask that can be used to extract the value of the
	// bitfield from the unit where it's stored. For example,
	// 'int a:2, b:5' -> 'a' has mask 0...011 (0x03) and 'b' mask 0...011100 (0x7C).
	__int64 mask = ((__int64)1 << fieldInfo.Size()) - 1;
	return mask << fieldInfo.UnitOffset();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
__int64 ExpressionGenerator::GetReverseBitfieldMask(FieldInfo& fieldInfo) {
	// Returns the reverse of the bitfield mask. Can be used to eliminate
	// the value of the bitfield, while leaving the other values the same.
	// For example 'int a:2' -> 'a' has the reverse mask 11111...1100 (0xFFFFFFFC).
	return ~GetBitfieldMask(fieldInfo);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::Visit(const MemberExpression* expr) {
	// Generate code for the base, than select the field using an 'field' instruction.
	// If the member expression is in a lvalue position we don't load
	// the value of the field (we compute just its address).
	// If the field is a bitfield we need to mask the loaded/stored value.
	auto objectType = expr->Object()->ResultType()->WithoutQualifiers();
	bool hasAggregateObj = objectType->IsAggregate();
	bool hasFunctionObj = objectType->IsFunction();
	bool shouldLoad = ShouldLoad() && ShouldLoadMemberAddress();

	if(hasAggregateObj || hasFunctionObj) {
		SetAddress(); // Handles 'a[1]->b[2]', where 'a' is 'struct ABC *a[2]'.
		SetMemberAddress();
	}
	else ResetFlags();

	// Generate the base object expression.
	ResetLvalue();
	auto baseOp = GenerateExpression(expr->Object());

	// Get the index of the field in the object.
	FieldDeclaration* field = expr->Member();
	const StructUnionType* recordType = field->Parent()->DeclarationType();

	StructLayout* layout = functGen_->GetLayouts()->GetOrCreate(recordType);
	FieldInfo& fieldInfo = layout->GetFieldInfo(field->Name());

	// Generate the instruction that computes the address of the member.
	// Note that the type of the base will always be 'pointer to record'.
	auto fieldIndex = irGen_->GetInt32Const(fieldInfo.Index());
	auto irFieldType = GetIRType(field->DeclarationType());
	auto irFieldPtrType = irGen_->GetPointer(irFieldType);

	auto addr = irGen_->GetTemporary(irFieldPtrType);
	auto fieldInstr = irGen_->GetField(baseOp, fieldIndex, addr, activeBlock_);
    AppendFieldName(fieldInstr, fieldInfo);

	// If the value of the field is not needed we're done.
	// We must be careful to not load the value of the field if it has
	// record or array type, like in 'a.b', where 'b' is a record.
	auto fieldType = field->DeclarationType()->WithoutQualifiers();
	bool hasAggregateField = fieldType->IsAggregate();
	bool hasFunctionField =  fieldType->IsFunction();

	if((shouldLoad == false) || hasAggregateField || hasFunctionField) {
		result_ = addr;
		return;
	}

	// Load the value of the bitfield. The values of normal fields are easier
	// to load, so it's done here; for bitfields we use a separate method.
	if(field->IsBitfield()) {
		LoadFromBitfield(addr, field, fieldInfo);
	}
	else LoadSimpleValue(addr, field->DeclarationType());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::LoadFromBitfield(IR::Operand* addressOp, FieldDeclaration* field, 
										   FieldInfo& fieldInfo) {
	DebugValidator::IsTrue(field->IsBitfield());
	
	// Loading from a bitfield involves the following steps:
	// - load the unit in which the bitfield is stored
	// - compute the mask used to extract the value of the bitfield (at compile time)
	// - extract the value from the unit by applying the mask
	// - if the type of the bitfield is unsigned:
	//      - shift the value to the right
	//  -else we need to extend the sign:
	//      - shift the value to the left, until the last bit becomes the sign bit
	//      - shift the value to the right, to the final position

	// OPTIMIZATION:
	// If the bitfield has a size multiple of 8 and it's properly aligned,
	// we can extract it's value without applying a mask and shifting.
	if(AccessIsAtByteOffset(fieldInfo)) {
		LoadFromBitfieldOptimized(addressOp, fieldInfo);
		return;
	}

	auto irResultType = GetIRType(field->DeclarationType());
	auto value = irGen_->GetTemporary(irResultType);

    functGen_->BeforeLoad(addressOp, currentExpr_, field->DeclarationType());
	
    auto loadInstr = irGen_->GetLoad(addressOp, value, activeBlock_);
	SetVolatile(loadInstr, field->DeclarationType(), false /* testPointer */);
    
    functGen_->AfterLoad(loadInstr, currentExpr_, field->DeclarationType());

	// Compute and apply the mask.
	__int64 mask = GetBitfieldMask(fieldInfo);
	auto maskedValue = irGen_->GetTemporary(irResultType);
	auto maskConst = irGen_->GetIntConst(irResultType, mask);

	irGen_->GetAnd(value, maskConst, 
                   maskedValue, activeBlock_);

	// Generate the shift based on the type of the integer (signed/unsigned).
	__int64 unitSize = TypeSize(field->DeclarationType(), 
                                functGen_->GetContext()).Size();
	unitSize *= 8; // We use bits in our computations.

	if(field->DeclarationType()->WithoutQualifiers()->IsSigned()) {
		// Shift left until the last bit in the bitfield 
        // becomes the last field in the unit. Then shift to the right 
        // until the first bit in the bitfield becomes the first bit 
        // in the unit, extending the sign at the same time.
		IR::Operand* leftShiftValue = nullptr;
		__int64 leftShiftAmount = unitSize - fieldInfo.Size() - 
                                  fieldInfo.UnitOffset();

		if(leftShiftAmount > 0) {
			leftShiftValue = irGen_->GetTemporary(irResultType);
			auto leftShiftConst = irGen_->GetIntConst(irResultType, 
                                                      leftShiftAmount);
			irGen_->GetShl(maskedValue, leftShiftConst, 
                           leftShiftValue, activeBlock_);
		}

		__int64 rightShiftAmount = unitSize - fieldInfo.Size();
		auto rightShiftConst = irGen_->GetIntConst(irResultType, 
                                                   rightShiftAmount);
		result_ = irGen_->GetTemporary(irResultType);

		if(leftShiftValue) {
			irGen_->GetShr(leftShiftValue, rightShiftConst,
                           result_, activeBlock_);
		}
		else irGen_->GetShr(maskedValue, rightShiftConst, 
                            result_, activeBlock_);
	}
	else {
		// Shift to the right until the first bit in the bitfield 
		// becomes the first in the unit.
		auto shiftConst = irGen_->GetIntConst(irResultType, 
                                              fieldInfo.UnitOffset());
		result_ = irGen_->GetTemporary(irResultType);
		irGen_->GetUshr(maskedValue, shiftConst, 
                        result_, activeBlock_);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionGenerator::LoadFromBitfieldOptimized(IR::Operand* addressOp, 
													FieldInfo& fieldInfo) {
	DebugValidator::IsTrue(AccessIsAtByteOffset(fieldInfo));
	
	// 'struct A { int a:3,b:5,c:8; } t; int v = t.c;' should be converted to
	// t1 = addr t, 3     t2 = ptop t1, int8*     t3 = addr t2, 1
	// t4 = load t3       t5 = sext t4, int32     store v, t5
	// Note that 'sext' is used for signed numbers, 'zext' for unsigned.
	const FieldDeclaration* field = fieldInfo.Field();
	bool isSigned = field->DeclarationType()->IsSigned();
	const IR::Type* conversionType = nullptr;

	switch(fieldInfo.ByteSize()) {
		case 1: { conversionType = irGen_->GetInt8();  break; }
		case 2: { conversionType = irGen_->GetInt16(); break; }
		case 4: { conversionType = irGen_->GetInt32(); break; }
		case 8: { conversionType = irGen_->GetInt64(); break; }
	}

	// We handle only 1, 2, 4 and 8 bytes.
	DebugValidator::IsNotNull(conversionType);

	// Convert to pointer to appropriate type, based on the bitfield size.
    auto convPtrType = functGen_->GetIRUnit()->Types().GetPointer(conversionType);
	auto ptopValue = irGen_->GetTemporary(convPtrType);
	irGen_->GetPtop(addressOp, convPtrType, ptopValue, activeBlock_);
			
	// Index into the pointer to load the right value.
	__int64 index = fieldInfo.UnitOffset() / fieldInfo.Size();
	auto convAddrValue = irGen_->GetTemporary(convPtrType);
	auto indexConst = irGen_->GetIntConst(conversionType, index);

	irGen_->GetAddress(ptopValue, indexConst, convAddrValue, activeBlock_);
		
	// Now load the value and sign/zero-extend it, if needed.
	auto value = irGen_->GetTemporary(conversionType);
    functGen_->BeforeLoad(convAddrValue, currentExpr_, field->DeclarationType());
	
    auto loadInstr = irGen_->GetLoad(convAddrValue, value, activeBlock_);
	SetVolatile(loadInstr, field->DeclarationType(), false /* testPointer */);
    
    functGen_->AfterLoad(loadInstr, currentExpr_, field->DeclarationType());
	auto irResultType = GetIRType(field->DeclarationType());

    if(conversionType != irResultType) {
        // Extension needed.
	    result_ = irGen_->GetTemporary(irResultType);
	    if(isSigned) irGen_->GetSext(value, irResultType, 
                                     result_, activeBlock_);
	    else irGen_->GetZext(value, irResultType, 
                             result_, activeBlock_);
    }
    else result_ = value;

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Instruction*
ExpressionGenerator::StoreToBitfield(IR::Operand* addressOp, IR::Operand* valueOp,
									 FieldDeclaration* field, FieldInfo& fieldInfo) {
	DebugValidator::IsTrue(field->IsBitfield());
	
	// Storing to a bitfield involves the following steps:
	// - compute the value mask, used to limit the value in it's valid range
	// - compute the unit mask, used to remove the previous value of the bitfield
	// - shift the bitfield value to the required offset
	// - limit the value by applying the value mask
	// - load the value of the destination unit
	// - eliminate the old value of the bitfield by applying the unit mask
	// - set the value of the bitfield
	// - store the value of the unit

	// OPTIMIZATION:
	// If the bitfield has a size multiple of 8 and it's properly aligned,
	// we can store it's value without applying a mask and shifting.
	if(AccessIsAtByteOffset(fieldInfo)) {
		return StoreToBitfieldOptimized(addressOp, valueOp,
                                        field, fieldInfo);
	}

	__int64 valueMask = GetBitfieldMask(fieldInfo);
	__int64 unitMask = GetReverseBitfieldMask(fieldInfo);
	auto irResultType = GetIRType(field->DeclarationType());
	auto shiftedValue = irGen_->GetTemporary(irResultType);
	auto shiftConst = irGen_->GetIntConst(irResultType, 
                                          fieldInfo.UnitOffset());
	irGen_->GetShl(valueOp, shiftConst, shiftedValue, activeBlock_);

	auto maskedValue = irGen_->GetTemporary(irResultType);
	auto valueMaskConst = irGen_->GetIntConst(irResultType, valueMask);

	irGen_->GetAnd(shiftedValue, valueMaskConst, maskedValue, activeBlock_);

	// Load the value of the unit and eliminate the old bitfield value.
	auto unitValue = irGen_->GetTemporary(irResultType);

    functGen_->BeforeLoad(addressOp, currentExpr_, field->DeclarationType());
	
    auto loadInstr = irGen_->GetLoad(addressOp, unitValue, activeBlock_);
    SetVolatile(loadInstr, field->DeclarationType(), false /* testPointer */);
    
    functGen_->AfterLoad(loadInstr, currentExpr_, field->DeclarationType());
	
    // Apply the mask.
	auto maskedUnitValue = irGen_->GetTemporary(irResultType);
	auto unitMaskConst = irGen_->GetIntConst(irResultType, unitMask);
	irGen_->GetAnd(unitValue, unitMaskConst, maskedUnitValue, activeBlock_);

	// Combine the value of the bitfield with the unit and store the unit.
	auto combined = irGen_->GetTemporary(irResultType);
	irGen_->GetOr(maskedValue, maskedUnitValue, combined, activeBlock_);

    functGen_->BeforeStore(addressOp, currentExpr_, field->DeclarationType());

	auto storeInstr = irGen_->GetStore(addressOp, combined, activeBlock_);
    SetVolatile(storeInstr, field->DeclarationType(), false /* testPointer */);
    
    functGen_->AfterStore(storeInstr, currentExpr_, field->DeclarationType());
    result_ = combined;
    return storeInstr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Instruction* ExpressionGenerator::StoreToBitfieldOptimized(IR::Operand* addressOp, 
                                                               IR::Operand* valueOp,
											                   FieldDeclaration* field, 
                                                               FieldInfo& fieldInfo) {
	DebugValidator::IsTrue(AccessIsAtByteOffset(fieldInfo));
	
	// 'struct A { int a:3,b:5,c:8; } t; t.c = 2;' should be converted to
	// t1 = addr t, 0         t2 = ptop t1, int8*     t3 = addr t2, 1
	// t4 = trunc 2, int8     store t3, t4
	const IR::Type* conversionType = nullptr;

	switch(fieldInfo.ByteSize()) {
		case 1: { conversionType = irGen_->GetInt8();  break; }
		case 2: { conversionType = irGen_->GetInt16(); break; }
		case 4: { conversionType = irGen_->GetInt32(); break; }
		case 8: { conversionType = irGen_->GetInt64(); break; }
	}

	// Convert to pointer to appropriate type, based on the bitfield size.
	DebugValidator::IsNotNull(conversionType);

	auto convPtrType = functGen_->GetIRUnit()->Types().GetPointer(conversionType);
	auto ptopValue = irGen_->GetTemporary(convPtrType);
	irGen_->GetPtop(addressOp, convPtrType, ptopValue, activeBlock_);
			
	// Index into the pointer to load the right value.
	__int64 index = fieldInfo.UnitOffset() / fieldInfo.Size();
	auto convAddrValue = irGen_->GetTemporary(convPtrType);
	auto indexConst = irGen_->GetIntConst(conversionType, index);
	irGen_->GetAddress(ptopValue, indexConst, 
                       convAddrValue, activeBlock_);

	// Truncate the value to be stored and store it.
	auto truncValue = irGen_->GetTemporary(conversionType);
	irGen_->GetTrunc(valueOp, conversionType, truncValue, activeBlock_);
	
    functGen_->BeforeStore(convAddrValue, currentExpr_, field->DeclarationType());

    auto storeInstr = irGen_->GetStore(convAddrValue, truncValue, activeBlock_);
    SetVolatile(storeInstr, field->DeclarationType(), false /* testPointer */);
    
    functGen_->AfterStore(storeInstr, currentExpr_, field->DeclarationType());
    return storeInstr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateRecordAssignment(IR::Operand* destOp, 
												   IR::Operand* sourceOp, 
												   const Type* destType) {
	DebugValidator::IsTrue(destType->IsRecord());
	
	// OPTIMIZATION:
	// We try to optimize for the case when the record has few fields and all are
	// basic types or records that also have few basic types.
	// For all other cases we use the 'copyMemory' intrinsic.
	//
	// Example of records compatible with this optimization:
	// struct RECT { int x, y, w, h; } a, b; a = b;
	// struct CIRCLE { struct POINT { int x, y }; int r; } a, b; a = b;
	const StructUnionType* recordType = destType->As<StructUnionType>();
    auto unit = functGen_->GetUnitGen();
	
	if(unit->IsSimpleRecord(recordType, SIMPLE_RECORD_MAX_FIELDS, 
							SIMPLE_RECORD_MAX_LEVELS)) {
		// The code generated for the first example:
		// t1 = field a, 0     t2 = field b, 0     t3 = load t2     store t2, t3
		// and the same set of 4 instructions for the rest of the fields.
		GenerateRecordAssignmentOptimized(destOp, sourceOp, 
                                          recordType);
		return;
	}

	// The record is large, so we generate a call to the 'copyMemory' intrinsic.
	// funct copyMemory(var dest int8*, var src int8*, var len int64) : void
	// 'dest' and 'src' are the addresses of the first fields, while
	// 'len' is the size of the record type.
	//
	// The generated code has the following form:
	// destOp = ptop destFieldAddr, int8*
	// srcOp  = ptop srcFieldAddr, int8*
	// call intrinsic copyMemory, desAddrtOp, srcAddrOp, lenOp
	auto zeroConst = irGen_->GetInt32Const(0);
	auto irPtrType = irGen_->GetPointer(GetIRType(recordType));
	auto copyMemType = irGen_->GetInt8Pointer();

	// Get the address of the first field in the destination and convert it to int8*.
	auto destAddrOp = irGen_->GetTemporary(copyMemType);
	irGen_->GetPtop(destOp, copyMemType, destAddrOp, activeBlock_);

	// Get the address of the first field in the destination and convert it to int8*.
	auto srcAddrOp = irGen_->GetTemporary(copyMemType);
	irGen_->GetPtop(sourceOp, copyMemType, srcAddrOp, activeBlock_);

	// Create the constant with the size of the record.
	__int64 length = TypeSize(destType, functGen_->GetContext()).Size();
	auto lengthOp = irGen_->GetInt64Const(length);

	// Now generate the call to 'copyMemory'.
	irGen_->GetCopyMemoryCall(destAddrOp, srcAddrOp, lengthOp, activeBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateRecordAssignmentOptimized(IR::Operand* destOp, 
															IR::Operand* sourceOp, 
															const StructUnionType* destType,
															bool fromExpanded,
															const VariableDeclaration* recordVar) {	
	if(destType->IsUnion()) {
		// If the record type is an 'union' then we need to copy only the largest field.
		int fieldUnit;
		UnitGenerator* unitGen = functGen_->GetUnitGen();
		auto largestField = unitGen->GetLargestUnionField(destType, &fieldUnit);
		GenerateFieldCopy(destOp, sourceOp, fieldUnit, largestField);				 
	}
	else {
		// Generate code to copy each field from 'sourceOp' to 'destOp'.
		// Note that it's possible that a field to have record type, in which case
		// this function is called again, like in the following case:
		// struct CIRCLE { struct POINT { int x, y }; int r; } a, b; a = b;
		auto& fields = destType->Fields();

		for(int i = 0; i < fields.Count(); i++) {
			GenerateFieldCopy(destOp, sourceOp, i, fields[i], 
                              fromExpanded, recordVar);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateFieldCopy(IR::Operand* destOp, IR::Operand* sourceOp,
											int fieldUnit, const FieldDeclaration* field, 
											bool fromExpanded, 
											const VariableDeclaration* recordVar) {
	// Generate temporaries that contain the addresses 
	// of the destination and source fields.
	auto fieldUnitConst = irGen_->GetInt32Const(fieldUnit);
	auto fieldType = field->DeclarationType()->WithoutQualifiers();
	auto irType = GetIRType(fieldType);
	auto irPtrType = irGen_->GetPointer(irType);
	IR::Operand* srcAddrOp;
	
	// destAddrOp = field destOp, fieldUnitConst
	auto destAddrOp = irGen_->GetTemporary(irPtrType);
	irGen_->GetField(destOp, fieldUnitConst, destAddrOp, activeBlock_);

	if(fromExpanded) {
		// The fields are initialized with the values from an expanded 'struct'.
		// Get the reference to the expanded parameter.
		srcAddrOp = functGen_->GetFieldReference(field, recordVar);
	}
	else {
		// We're copying from another record.
		// srcAddrOp  = field sourceOp, fieldUnitConst
		srcAddrOp = irGen_->GetTemporary(irPtrType);
		irGen_->GetField(sourceOp, fieldUnitConst, srcAddrOp, activeBlock_);
	}

	// If we have a basic type we generate the copy here,
    // else we call 'GenerateRecordAssignmentOptimized' 
    // to copy all the fields in the nested record.
	if(auto recordType = fieldType->As<StructUnionType>()) {
		GenerateRecordAssignmentOptimized(destAddrOp, srcAddrOp, recordType,
										  fromExpanded, recordVar);	
	}
	else {
		// Load from the source and store into the destination.
		auto srcValue = irGen_->GetTemporary(irType);

        functGen_->BeforeLoad(srcAddrOp, currentExpr_, fieldType);

		auto loadInstr = irGen_->GetLoad(srcAddrOp, srcValue, activeBlock_);
        SetVolatile(loadInstr, field->DeclarationType(), false /* testPointer */);
        
        functGen_->AfterLoad(loadInstr, currentExpr_, fieldType);
        functGen_->BeforeStore(destAddrOp, currentExpr_, fieldType);
        
        auto storeInstr = irGen_->GetStore(destAddrOp, srcValue, activeBlock_);
        SetVolatile(storeInstr, field->DeclarationType(), false /* testPointer */);

        functGen_->AfterStore(storeInstr, currentExpr_, fieldType);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::InitializeExpandedStruct(IR::Operand* structRef, 
												   const VariableDeclaration* structVar) {
	// Use the same methods as for a record assignment,
    // but inform that the source for the fields is not
    // another record, but variables that represent the fields.
	auto structType = structVar->DeclarationType()->WithoutQualifiers()
                               ->As<StructUnionType>();
	GenerateRecordAssignmentOptimized(structRef, nullptr, structType, 
									  true /* fromExpanded */, structVar);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::AppendFieldName(IR::FieldInstr* fieldInstr, 
                                          FieldInfo& fieldInfo) {
    if(functGen_->GetContext()->Options().ShouldNameFields()) {
        auto nameTag = IR::NameTag::GetName(fieldInfo.Field()->Name()->Name());
        fieldInstr->AddTag(nameTag);
    }
}

} // namespace IRGenerator