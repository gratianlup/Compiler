// ConstantFolderCall.cpp
// Copyright (c) Lup Gratian
//
// Implements the constant folder for 'call' instructions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ConstantFolder.hpp"

namespace Analysis {

Operand* ConstantFolder::HandleCall(CallInstr* callInstr, ArgumentList* replacementArgs) {
	// A call to an 'undef' operand, or to a null pointer yields 
    // undefined behavior, so we're allowed to do everything we want.
	if(callInstr->TargetOp()->IsUndefinedConstant() ||
	   callInstr->TargetOp()->IsNullConstant()) {
		// We should return 'undef' only if the function returns a value.
		if(callInstr->ResultOp()) {
			return GetUndefined(callInstr->ResultOp());
		}
		else return nullptr;
	}

	// The target must be a reference to a known function.
	auto functionRef = callInstr->TargetOp()->As<FunctionReference>();
	
    if(functionRef == nullptr) {
        return nullptr;
    }

    // First test for intrinsics; we fold them using the stdlib functions.
    if(auto intrinsic = functionRef->Target()->As<Intrinsic>()) {
        return HandleIntrinsic(callInstr, intrinsic, replacementArgs);
    }

	// Check if the function is a known standard library function,
	// and try to evaluate it at compile time. Very useful for functions like
	// 'sin', 'cos', 'floor', etc.
	StdlibType functionType = StdlibRecognizer::Recognize(functionRef->Target());

	if(functionType == StdlibType::None) {
        return nullptr;
    }

	// We treat for now only functions from 'math.h'.
	// The functions are evaluated using the implementation
    // provided by the compiler, so note that the results 
    // are dependent on the machine where the program is compiled.
	switch(functionType) {
 		case StdlibType::fabs:  return EvaluateMathOneParam(callInstr, std::fabs, replacementArgs);
		case StdlibType::sqrt:  return EvaluateMathOneParam(callInstr, std::sqrt, replacementArgs);
		case StdlibType::exp:   return EvaluateMathOneParam(callInstr, std::exp, replacementArgs);
		case StdlibType::floor: return EvaluateMathOneParam(callInstr, std::floor, replacementArgs);
		case StdlibType::ceil:  return EvaluateMathOneParam(callInstr, std::ceil, replacementArgs);
		case StdlibType::log:   return EvaluateMathOneParam(callInstr, std::log, replacementArgs);
		case StdlibType::log10: return EvaluateMathOneParam(callInstr, std::log10, replacementArgs);
		case StdlibType::sin:   return EvaluateMathOneParam(callInstr, std::sin, replacementArgs);
		case StdlibType::asin:  return EvaluateMathOneParam(callInstr, std::asin, replacementArgs);
		case StdlibType::cos:   return EvaluateMathOneParam(callInstr, std::cos, replacementArgs);
		case StdlibType::acos:  return EvaluateMathOneParam(callInstr, std::acos, replacementArgs);
		case StdlibType::tan:   return EvaluateMathOneParam(callInstr, std::tan, replacementArgs);
		case StdlibType::atan:  return EvaluateMathOneParam(callInstr, std::atan, replacementArgs);

		case StdlibType::abs:   return EvaluateMathOneParamInt(callInstr, std::abs, replacementArgs);
        case StdlibType::labs:  return EvaluateMathOneParamLong(callInstr, std::labs, replacementArgs);

		case StdlibType::sqrtf:  return EvaluateMathOneParamFloat(callInstr, std::sqrtf, replacementArgs);
		case StdlibType::expf:   return EvaluateMathOneParamFloat(callInstr, std::expf, replacementArgs); 
		case StdlibType::floorf: return EvaluateMathOneParamFloat(callInstr, std::floorf, replacementArgs);
		case StdlibType::ceilf:  return EvaluateMathOneParamFloat(callInstr, std::ceilf, replacementArgs);
		case StdlibType::logf:   return EvaluateMathOneParamFloat(callInstr, std::logf, replacementArgs); 
		case StdlibType::log10f: return EvaluateMathOneParamFloat(callInstr, std::log10f, replacementArgs);
		case StdlibType::sinf:   return EvaluateMathOneParamFloat(callInstr, std::sinf, replacementArgs); 
		case StdlibType::asinf:  return EvaluateMathOneParamFloat(callInstr, std::asinf, replacementArgs);
		case StdlibType::cosf:   return EvaluateMathOneParamFloat(callInstr, std::cosf, replacementArgs); 
		case StdlibType::acosf:  return EvaluateMathOneParamFloat(callInstr, std::acosf, replacementArgs);
		case StdlibType::tanf:   return EvaluateMathOneParamFloat(callInstr, std::tanf, replacementArgs); 
		case StdlibType::atanf:  return EvaluateMathOneParamFloat(callInstr, std::atanf, replacementArgs);

		case StdlibType::pow:   return EvaluateMathTwoParams(callInstr, std::pow, replacementArgs);
		case StdlibType::fmod:  return EvaluateMathTwoParams(callInstr, std::fmod, replacementArgs);
        case StdlibType::atan2: return EvaluateMathTwoParams(callInstr, std::atan2, replacementArgs);

		case StdlibType::powf:   return EvaluateMathTwoParamsFloat(callInstr, std::powf, replacementArgs);
		case StdlibType::fmodf:  return EvaluateMathTwoParamsFloat(callInstr, std::fmodf, replacementArgs);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::HandleIntrinsic(CallInstr* callInstr, Intrinsic* intrinsic,
                                         ArgumentList* replacementArgs) {
    if(auto mathIntrinsic = intrinsic->As<MathIntrinsic>()) {
        switch(mathIntrinsic->GetMathKind()) {
            case MathIntrinsicKind::Abs8:
            case MathIntrinsicKind::Abs16:
            case MathIntrinsicKind::Abs32:
            case MathIntrinsicKind::Abs64: {
                auto argument = GetArgument(0, callInstr, replacementArgs);
                bool is64Bit = mathIntrinsic->GetMathKind() == MathIntrinsicKind::Abs64;

                // abs(quest c, 5, -5) -> 5
                if(auto questInstr = argument->DefiningInstrAs<QuestionInstr>()) {
                    return HandleAbsOnQuestion(questInstr, is64Bit);
                }
                else return EvaluateAbs(callInstr, replacementArgs);
            }
            case MathIntrinsicKind::Min8:
            case MathIntrinsicKind::Min16:
            case MathIntrinsicKind::Min32:
            case MathIntrinsicKind::Min64: {
                return EvaluateMin(callInstr, replacementArgs);
            }
            case MathIntrinsicKind::Max8:
            case MathIntrinsicKind::Max16:
            case MathIntrinsicKind::Max32:
            case MathIntrinsicKind::Max64: {
                return EvaluateMax(callInstr, replacementArgs);
            }
            case MathIntrinsicKind::Fabs:  return EvaluateMathOneParam(callInstr, std::fabs, replacementArgs);
            case MathIntrinsicKind::Atan:  return EvaluateMathOneParam(callInstr, std::atan, replacementArgs);
            case MathIntrinsicKind::Exp:   return EvaluateMathOneParam(callInstr, std::exp, replacementArgs);
            case MathIntrinsicKind::Pow:   return EvaluateMathTwoParams(callInstr, std::pow, replacementArgs);
            case MathIntrinsicKind::Log10: return EvaluateMathOneParam(callInstr, std::log10, replacementArgs);
            case MathIntrinsicKind::Sqrt:  return EvaluateMathOneParam(callInstr, std::sqrt, replacementArgs);
            case MathIntrinsicKind::Sqrtf: return EvaluateMathOneParamFloat(callInstr, std::sqrtf, replacementArgs);
            case MathIntrinsicKind::Log:   return EvaluateMathOneParam(callInstr, std::log, replacementArgs);
            case MathIntrinsicKind::Sin:   return EvaluateMathOneParam(callInstr, std::sin, replacementArgs);
            case MathIntrinsicKind::Tan:   return EvaluateMathOneParam(callInstr, std::tan, replacementArgs);
            case MathIntrinsicKind::Cos:   return EvaluateMathOneParam(callInstr, std::cos, replacementArgs);
            case MathIntrinsicKind::Atan2: return EvaluateMathTwoParams(callInstr, std::atan2, replacementArgs);
        }
    }
        
    // Any other intrinsic is not folded.
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateAbs(CallInstr* callInstr, ArgumentList* replacementArgs) {
    auto intConst = AsIntConstant(GetArgument(0, callInstr, replacementArgs),
                                  callInstr->ParentBlock());
    if(intConst == nullptr) {
        return nullptr;
    }

    __int64 result = std::abs(intConst->Value());
    return irGen_->GetIntConst(intConst->GetType(), result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMin(CallInstr* callInstr, ArgumentList* replacementArgs) {
    auto intConstA = AsIntConstant(GetArgument(0, callInstr, replacementArgs),
                                   callInstr->ParentBlock());
    auto intConstB = AsIntConstant(GetArgument(1, callInstr, replacementArgs),
                                   callInstr->ParentBlock());
    if((intConstA && intConstB) == false) {
        return nullptr;
    }

    bool isSigned = callInstr->GetArgument(2)->IsOneInt();
    __int64 result;
    
    if(isSigned) {
        result = std::min(intConstA->Value(), intConstB->Value());
    }
    else result = std::min((unsigned __int64)intConstA->Value(), 
                           (unsigned __int64)intConstB->Value());
    return irGen_->GetIntConst(intConstA->GetType(), result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMax(CallInstr* callInstr, ArgumentList* replacementArgs) {
    auto intConstA = AsIntConstant(GetArgument(0, callInstr, replacementArgs),
                                   callInstr->ParentBlock());
    auto intConstB = AsIntConstant(GetArgument(1, callInstr, replacementArgs),
                                   callInstr->ParentBlock());

    if((intConstA && intConstB) == false) {
        return nullptr;
    }

     bool isSigned = callInstr->GetArgument(2)->IsOneInt();
    __int64 result;
    
    if(isSigned) {
        result = std::max(intConstA->Value(), intConstB->Value());
    }
    else result = std::max((unsigned __int64)intConstA->Value(), 
                           (unsigned __int64)intConstB->Value());
    return irGen_->GetIntConst(intConstA->GetType(), result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::ParametersValid(CallInstr* callInstr, double& value,
                                     ArgumentList* replacementArgs) {
	auto floatConst = GetArgument(0, callInstr, replacementArgs)->As<FloatConstant>();

	if((floatConst == nullptr) ||
	   FA::IsNaN(floatConst) || FA::IsInfinity(floatConst)) {
        return false;
    }
	
	value = FA::LimitToType(floatConst);
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathOneParam(CallInstr* callInstr, 
                                              MathOneParam function,
                                              ArgumentList* replacementArgs) {
	double value;

	if(ParametersValid(callInstr, value, replacementArgs)) {
		double result = function(value);
		return irGen_->GetDoubleConst(result);
	}
    
    return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathOneParamInt(CallInstr* callInstr,
                                                 MathOneParamInt function,
                                                 ArgumentList* replacementArgs) {
    auto intConst = AsIntConstant(GetArgument(0, callInstr, replacementArgs),
                                  callInstr->ParentBlock());
    if(intConst == nullptr) {
        return nullptr;
    }

	__int64 result = function((int)IA::LimitToType(intConst->Value(), 
	                                               IRIntegerKind::Int32));
    return irGen_->GetInt32Const(result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathOneParamLong(CallInstr* callInstr,
                                                  MathOneParamLong function,
                                                  ArgumentList* replacementArgs) {
    auto intConst = AsIntConstant(GetArgument(0, callInstr, replacementArgs),
                                  callInstr->ParentBlock());
    if(intConst == nullptr) {
        return nullptr;
    }

	__int64 result = function((long)intConst->Value());
    return irGen_->GetInt64Const(result);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathOneParamFloat(CallInstr* callInstr, 
												   MathOneParamFloat function,
                                                   ArgumentList* replacementArgs) {
	double value;

	if(ParametersValid(callInstr, value, replacementArgs)) {
		float result = function((float)value);
		return irGen_->GetFloatConst(result);
	}
    
    return nullptr;    
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ConstantFolder::ParametersValid(CallInstr* callInstr, 
                                     double& value1, double& value2,
                                     ArgumentList* replacementArgs) {
	auto floatConst1 = GetArgument(0, callInstr, replacementArgs)->As<FloatConstant>();
	auto floatConst2 = GetArgument(1, callInstr, replacementArgs)->As<FloatConstant>();
	
	// If the operands are not constants, or they are NaN or Infinity we give up.
	if((floatConst1 && floatConst2) == false) {
        return false;
    }

	if(FA::IsNaN(floatConst1) || FA::IsInfinity(floatConst1) ||
	   FA::IsNaN(floatConst2) || FA::IsInfinity(floatConst2)) {
		return false;
	}

	value1 = FA::LimitToType(floatConst1);
	value2 = FA::LimitToType(floatConst2);
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathTwoParams(CallInstr* callInstr, 
                                               MathTwoParams function,
                                               ArgumentList* replacementArgs) {
	double value1;
	double value2;

	if(ParametersValid(callInstr, value1, value2, replacementArgs)) {
		double result = function(value1, value2);
		return irGen_->GetDoubleConst(result);
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* ConstantFolder::EvaluateMathTwoParamsFloat(CallInstr* callInstr, 
													MathTwoParamsFloat function,
                                                    ArgumentList* replacementArgs) {
	double value1;
	double value2;

	if(ParametersValid(callInstr, value1, value2, replacementArgs)) {
		float result = function((float)value1, (float)value2);
		return irGen_->GetFloatConst(result);
	}
	
	return nullptr;
}

} // namespace Analysis