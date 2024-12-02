// CallExpressionGen.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionGenerator.hpp"
#include "FunctionGenerator.hpp"
#include "UnitGenerator.hpp"

namespace IRGenerator {

void ExpressionGenerator::Visit(const CallExpression* callExpr) {
	// Generate code for the target of the call, then for each parameter, 
    // and at the end generate the 'call' instruction.
    // The target function (if it is a known function) will not be marked
    // as being address taken.
    DontMarkFunctionAddressTaken();
	auto targetOp = GenerateExpression(callExpr->Function());
    MarkFunctionAddressTaken();

	// It's possible that the type of the function
    // we call doesn't match the function declaration/definition 
    // found in the IR unit. In this case a conversion is needed 
    // (see 'PatchCallTarget' for details).
	targetOp = PatchCallTarget(targetOp, callExpr);

	// For functions that return a value we need to retain it.
	auto returnType = callExpr->ResultType();
	bool hasRecordReturn = false;
	IR::Operand* returnOp;

	if(recordReturnTarget_) {
		// The target is a known variable (like in 'a = f()').
		// This is an optimization recognized while generating
        // the assignment operator.
		returnOp = recordReturnTarget_;
		recordReturnTarget_ = nullptr;
		hasRecordReturn = true;
	}
	else {
		// The target is a simple or record type that
        // doesn't have a known variable (like '2 + f().x'), 
        // or there is no target at all (when 'f' returns 'void').
		returnOp = GenerateReturnTarget(returnType, hasRecordReturn);
	}

	// Generate the call instruction, then set the calling convention.
	// Note that the call is not inserted yet in the block,
    // because we could have calls as arguments, and they must 
    // appear before this call.
	auto callInstr = irGen_->GetCall(targetOp, hasRecordReturn ? nullptr : returnOp,
									 callExpr->ArgCount());
	SetCallConvention(callInstr, callExpr);
    SetCallPromotionPolicy(callInstr, callExpr);

	// If the call has arguments we generate code 
    // for each one and create a list containing their values. 
    // The address of the variable used as a return operand 
    // for a record is appended at the end of the list.
	GenerateCallArguments(callExpr, callInstr, hasRecordReturn ? returnOp : nullptr);

	functGen_->BeforeCall(targetOp, callExpr);
	activeBlock_->InsertInstruction(callInstr); // Can be inserted now.
	functGen_->AfterCall(callInstr, callExpr);

	// The return operand is put into 'result_' because the value 
    // may be used by another expression from the expression tree.
	result_ = returnOp;
	
	// If slots were used (for "large" record arguments or 
	// for passing records as parameters), release them now.
	functGen_->ReleaseRecordSlots();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* ExpressionGenerator::GenerateReturnTarget(const Type* returnType, 
													   bool& hasRecordReturn) {
	returnType = returnType->WithoutQualifiers();
	hasRecordReturn = false;

	if(returnType->IsVoid()) {
        return nullptr;
    }
	else if(auto recordType = returnType->As<StructUnionType>()) {
		// If the return type is a record we use a slot 
        // (a temporary variable) where the called function 
        // copies the returned value.
		auto slotRef = functGen_->GetRecordSlot(recordType);
		hasRecordReturn = true;
		return slotRef;
	}
	else {
		// This is a normal type.
		auto irReturnType = GetIRType(returnType);
		auto returnOp = irGen_->GetTemporary(irReturnType);
		return returnOp;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Operand* ExpressionGenerator::PatchCallTarget(IR::Operand* targetOp, 
										          const CallExpression* callExpr) {
	// Consider the following situation:
	// 'void f();
	//  void g() { f(1,2); }
	//  void f(int a, int b) { ... }
	// The call to 'f' uses the first declaration of the function,
    // while the generator will emit only the second definition. 
    // In this case the type of the called function doesn't match 
    // the type of the function in the IR unit, and a 'ptop' 
    // conversion instruction is needed, like the following:
	// 't1 = ptop f, <funct (...) : void>     call t1, x, y'

	// Note that this can't happen if we're calling through a pointer.
	if(auto functToPtr = callExpr->Function()->As<CastExpression>()) {
		if(functToPtr->Type() == CastType::FunctionToPtr) {
			// We check if the types are different.
			// The declaration/definition emitted in the unit is the last one.
			if(auto functionRef = functToPtr->Target()->As<DeclarationExpression>()) {
				auto functDecl = functionRef->Object()->As<FunctionDeclaration>();
				auto lastFunctDecl = functDecl->LastDeclaration();

				if(functDecl->DeclarationType() == lastFunctDecl->DeclarationType()) {
					// The types match, no need for conversion.
					return false;
				}

				// The types don't match, a conversion is needed.
				// If the target is a function without prototype 
                // called with arguments (like in the example above) 
                // we need to use a function with a variable number
                // of parameters as the conversion type.
				const IR::FunctionType* convFunctType;

				if((functDecl->DeclarationType()->ParameterCount() == 0) &&
				   (callExpr->ArgCount() > 0)) {
					auto returnType = GetIRType(functDecl->DeclarationType()->ReturnType());
					convFunctType = irGen_->GetFunction(returnType, nullptr, 0,
														true /* isVarargs */);
				}
				else convFunctType = static_cast<const IR::FunctionType*>
									 (GetIRType(functDecl->DeclarationType()));

				// Pointers to function are used in the IR.
				auto convFunctPtrType = irGen_->GetPointer(convFunctType);
				irGen_->GetPtop(targetOp, convFunctPtrType, 
                                result_, activeBlock_);
				return result_;
			}
		}
	}

	return targetOp; // No change has been made.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateCallArguments(const CallExpression* callExpr, 
												IR::CallInstr* callInstr,
												IR::Operand* recordRetAddr) {
	// If the call has no arguments and the returned type is not a 'record'
	// nothing needs to be done.
	if((callExpr->ArgCount() == 0) &&
       (recordRetAddr == nullptr)) {
        return;
    }

	// Create the argument list, with adequate capacity.
	auto& argList = callExpr->Arguments();

	for(int i = 0; i < argList.Count(); i++) {
		auto argument = argList[i];

		// An argument can be any expression, so we generate code
        // the usual way. Arguments having record type 
        // must be handled separately, and we shall not try to load
        // the record because we pass its address.
		if(argument->ResultType()->WithoutQualifiers()->IsRecord()) {
            SetAddress();
			argument->Accept(this);
			ResetFlags();

			GenerateRecordArgument(callInstr, result_, argument);
		}
		else {
			// Force loading, but only if the argument is not a function type
            // (records can't appear, they are handled above).
			if(argument->ResultType()->WithoutQualifiers()->IsFunction()) {
				// Don't load it by making it believe it's an lvalue.
				SetLvalue();
			}
			else ResetFlags(); // Force loading.
			
			callInstr->AddArgument(GenerateExpression(argument));
		}
	}

	// A function that returns a record has a last parameter
    // that contains the address of the (temporary) variable 
    // where it should copy the returned value.
	if(recordRetAddr) {
		callInstr->AddArgument(recordRetAddr);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateRecordArgument(IR::CallInstr* callInstr, 
												 IR::Operand* addressOp, 
                                                 Expression* argument) {
	DebugValidator::IsTrue(argument->ResultType()->WithoutQualifiers()->IsRecord());
	
	// If the type of the argument is a expandable 'struct' we need
	// to load and add the value of each field to the argument list.
	auto recordType = argument->ResultType()->WithoutQualifiers()
                                            ->As<StructUnionType>();

	if(functGen_->GetUnitGen()->IsExpandableStruct(recordType)) {
		GenerateExpandedStructArg(callInstr, addressOp, 
                                  recordType->As<StructType>());
		return;
	}

	// For large records we just pass the reference as an argument,
    // because the callee creates a copy of the record 
    // (this guarantees that this record will not be changed by the callee).
	callInstr->AddArgument(addressOp);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::GenerateExpandedStructArg(IR::CallInstr* callInstr, 
													IR::Operand* addressOp,
													const StructType* structType) {
	// Generate code that loads each field and adds it's value
    // to the argument list. If the field is a nested 'struct' 
    // we generate code for it's fields before continuing 
    // generating fields in the current 'struct'.
	auto& fields = structType->Fields();

	for(int i = 0; i < fields.Count(); i++) {
		auto fieldType = fields[i]->DeclarationType()->WithoutQualifiers();
		auto irFieldType = GetIRType(fieldType);
		auto irFieldPtrType = irGen_->GetPointer(irFieldType);

		// Generate the address of the field.
		auto fieldIndexOp = irGen_->GetInt32Const(i);
		auto fieldAddrOp = irGen_->GetTemporary(irFieldPtrType);
		irGen_->GetField(addressOp, fieldIndexOp, 
                         fieldAddrOp, activeBlock_);

		if(auto nestedStructType = fieldType->As<StructType>()) {
			// Nested 'struct', recourse.
			GenerateExpandedStructArg(callInstr, fieldAddrOp, 
                                      nestedStructType);
		}
		else {
			// Load the value of the field. 
            // It is guaranteed that it is a basic type.
			auto fieldValueOp = irGen_->GetTemporary(irFieldType);
			irGen_->GetLoad(fieldAddrOp, fieldValueOp, activeBlock_);

            // Now add it to the argument list.
			callInstr->AddArgument(fieldValueOp); 
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::SetCallConvention(IR::CallInstr* callInstr, 
											const CallExpression* callExpr) {
	// Specify the call convention to use if the called function 
    // has a 'CallConvention' attribute attached. Note that even 
    // if the target function is a 'pointer to function' variable, 
    // it still can have such an attribute.
	if(auto declExpr = callExpr->Function()->As<DeclarationExpression>()) {
		if(auto callConvAttr = declExpr->Object()->AttributeAs<CallConventionAttribute>()) {
			IR::CallConventionType irCallConv;

			switch(callConvAttr->Convention()) {
				case CallConvention::Cdecl:    { irCallConv = IR::CallConventionType::Cdecl;    break; }
				case CallConvention::Stdcall:  { irCallConv = IR::CallConventionType::Stdcall;  break; }
				case CallConvention::Fastcall: { irCallConv = IR::CallConventionType::Fastcall; break; }
				default: DebugValidator::Unreachable();
			}

			// Set the call convention.
			callInstr->SetCallConvention(irCallConv);
			callInstr->SetHasOverridenCallConvention(true);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionGenerator::SetCallPromotionPolicy(IR::CallInstr* callInstr, 
                                                 const CallExpression* callExpr) {
    switch(functGen_->GetContext()->Options().GetFloatingPointMode()) {
		case FPMode_Exact:
		case FPMode_Safe: {
            callInstr->SetIsPromotableMathFunction(false);
			break;
		}
		case FPMode_Fast: {
			// This can be done only if we're allowed to ignore 'errno'.
			bool allowed = functGen_->GetContext()->Options().IsErrnoDisabled();
			callInstr->SetIsPromotableMathFunction(allowed);
			break;
		}
	}
}

} // namespace IRGenerator