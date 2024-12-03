// StdlibRecognizer.cpp
// Copyright (c) Lup Gratian
//
// Implements the StdlibRecognizer class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "StdlibRecognizer.hpp"

namespace Analysis {

// Used to mark that the function table has been initialized.
bool StdlibRecognizer::initialized_ = false;

StdlibRecognizer::DescriptorDict StdlibRecognizer::stdLibFuncts_=
	    StdlibRecognizer::DescriptorDict(64);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void StdlibRecognizer::Initialize() {
	if(initialized_) return;
	#define STDLIB_FUNCT(TYPE, NAME, PARAMS, CATEGORY) \
		stdLibFuncts_.Add(NAME, new StdLibDescriptor(TYPE,  NAME, PARAMS, CATEGORY))

	STDLIB_FUNCT(StdlibType::memcpy,  "memcpy",  3, StdlibCategory::Memory);
	STDLIB_FUNCT(StdlibType::memset,  "memset",  3, StdlibCategory::Memory);
	STDLIB_FUNCT(StdlibType::memmove, "memmove", 3, StdlibCategory::Memory);
	STDLIB_FUNCT(StdlibType::memcmp,  "memcmp",  3, StdlibCategory::Memory);
	STDLIB_FUNCT(StdlibType::malloc,  "malloc",  1, StdlibCategory::Memory);
    STDLIB_FUNCT(StdlibType::free,    "free",    1, StdlibCategory::Memory);
    STDLIB_FUNCT(StdlibType::alloca,  "alloca",  1, StdlibCategory::Memory);

	STDLIB_FUNCT(StdlibType::strlen,  "strlen",  1, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strcpy,  "strcpy",  2, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strncpy, "strncpy", 3, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strcmp,  "strcmp",  2, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strncmp, "strncmp", 3, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strcat,  "strcat",  2, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strncat, "strncat", 3, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strchr,  "strchr",  2, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strstr,  "strstr",  2, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strpbrk, "strpbrk", 2, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strspn,  "strspn",  2, StdlibCategory::String);
	STDLIB_FUNCT(StdlibType::strcspn, "strcspn", 2, StdlibCategory::String);

	STDLIB_FUNCT(StdlibType::abs,    "abs",     1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::fabs,   "fabs",    1, StdlibCategory::Math);
    STDLIB_FUNCT(StdlibType::labs,   "labs",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::pow,    "pow",     2, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::powf,   "powf",    2, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::sqrt,   "sqrt",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::sqrtf,  "sqrtf",   1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::exp,    "exp",     1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::expf,   "expf",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::floor,  "floor",   1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::floorf, "floorf",  1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::ceil,   "ceil",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::ceilf,  "ceilf",   1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::log,    "log",     1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::logf,   "logf",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::log10,  "log10",   1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::log10f, "log10f",  1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::fmod,   "fmod",    2, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::fmodf,  "fmodf",   2, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::sin,    "sin",     1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::sinf,   "sinf",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::asin,   "asin",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::asinf,  "asinf",   1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::cos,    "cos",     1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::cosf,   "cosf",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::acos,   "acos",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::acosf,  "acosf",   1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::tan,    "tan",     1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::tanf,   "tanf",    1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::atan,   "atan",    1, StdlibCategory::Math);
    STDLIB_FUNCT(StdlibType::atan2,  "atan2",   1, StdlibCategory::Math);
	STDLIB_FUNCT(StdlibType::atanf,  "atanf",   1, StdlibCategory::Math);

	STDLIB_FUNCT(StdlibType::isdigit, "isdigit", 1, StdlibCategory::Other);
	STDLIB_FUNCT(StdlibType::isascii, "isascii", 1, StdlibCategory::Other);
    STDLIB_FUNCT(StdlibType::exit,    "exit",    1, StdlibCategory::Other);
    STDLIB_FUNCT(StdlibType::abort,   "abort",   0, StdlibCategory::Other);
    STDLIB_FUNCT(StdlibType::setjmp,  "setjmp",  1, StdlibCategory::Other);
    STDLIB_FUNCT(StdlibType::longjmp, "longjmp", 2, StdlibCategory::Other);
    STDLIB_FUNCT(StdlibType::alloca,  "alloca",  1, StdlibCategory::Other);

	#undef STDLIB_FUNCT
	initialized_ = true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StdLibDescriptor* StdlibRecognizer::GetDescriptor(const Function* function) {
	StdLibDescriptor* descriptor;

	if(stdLibFuncts_.TryGetValue(*function->Name(), &descriptor)) {
		if(function->ParameterCount() == descriptor->Parameters) {
			return descriptor;
		}
	}
	
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
StdlibType StdlibRecognizer::Recognize(const Function* function, 
                                       StdlibCategory* category) {
	// If the function is unnamed give up.
	if(function->HasName() == false) {
		return StdlibType::None;
	}

	// The function must be marked as originating from the standard library.
	// This prevents situations where the user wrote a function that looks
	// exactly like one from the standard library, but does something different.
	if(function->IsFromStdlib() == false) {
		return StdlibType::None;
	}

	// Try to get a descriptor for the specified function. If one is returned, 
	// a function with this name and the same number of parameters was found.
	Initialize();
	StdLibDescriptor* descriptor = GetDescriptor(function);
	if(descriptor == nullptr) return StdlibType::None;

	// Now test the types of the parameters, because we must be sure the function
	// matches the one from the standard library.
	auto& parameters = function->Parameters();

	switch(descriptor->Type) {
		case StdlibType::fabs:  
		case StdlibType::pow:   
		case StdlibType::sqrt:  
		case StdlibType::exp:   
		case StdlibType::floor: 
		case StdlibType::ceil:  
		case StdlibType::log:   
		case StdlibType::log10: 
		case StdlibType::fmod:  
		case StdlibType::sin:   
		case StdlibType::asin:  
		case StdlibType::cos:   
		case StdlibType::acos:  
		case StdlibType::tan:   
		case StdlibType::atan:
        case StdlibType::atan2: {
			// The types of the parameters must be 'double'.
			for(int i = 0; i < parameters.Count(); i++) {
				if(parameters[i]->GetType()->IsDouble() == false) {
					return StdlibType::None;
				}
			}
			
			if(function->ReturnType()->IsDouble() == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::powf:  
		case StdlibType::sqrtf: 
		case StdlibType::expf:  
		case StdlibType::floorf:
		case StdlibType::ceilf: 
		case StdlibType::logf:  
		case StdlibType::log10f:
		case StdlibType::fmodf: 
		case StdlibType::sinf:  
		case StdlibType::asinf: 
		case StdlibType::cosf:  
		case StdlibType::acosf: 
		case StdlibType::tanf:  
		case StdlibType::atanf: {
			// The type of the parameters must be 'float'.
			for(int i = 0; i < parameters.Count(); i++) {
				if(parameters[i]->GetType()->IsFloat() == false) {
					return StdlibType::None;
				}
			}

			if(function->ReturnType()->IsFloat() == false) {
				return StdlibType::None;
			}
			break;
		}
        case StdlibType::abs: {
            if(parameters[0]->GetType()->IsInt32() == false) {
				return StdlibType::None;
			}

			if(function->ReturnType()->IsInt32() == false) {
				return StdlibType::None;
			}
			break;
        }
        case StdlibType::labs: {
            if(parameters[0]->GetType()->IsInt64() == false) {
				return StdlibType::None;
			}

			if(function->ReturnType()->IsInt64() == false) {
				return StdlibType::None;
			}
			break;
        }
		case StdlibType::memcpy:
		case StdlibType::memmove:
		case StdlibType::memcmp: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt64()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt64()) &&
				(parameters[2]->GetType()->IsInt64() || 
                 parameters[2]->GetType()->IsInt32())) == false) {
				return StdlibType::None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt64()) == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::memset: {
			if((parameters[0]->GetType()->IsPointer() &&
				parameters[0]->GetType()->As<PointerType>()
                                        ->PointeeType()->IsInt64()) &&
				parameters[1]->GetType()->IsInt32() && 
			   (parameters[2]->GetType()->IsInt64() ||
                parameters[2]->GetType()->IsInt32()) == false) {
				return StdlibType::None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt64()) == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::malloc: {
			if((parameters[0]->GetType()->IsInt32() ||
                parameters[0]->GetType()->IsInt64()) == false) {
				return StdlibType::None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt64()) == false) {
				return StdlibType::None;
			}
			break;
		}
        case StdlibType::free: {
			if((parameters[0]->IsPointer() &&
				parameters[0]->GetType()->As<PointerType>()
                                        ->PointeeType()->IsInt64()) == false) {
				return StdlibType::None;
			}

            if(function->ReturnType()->IsVoid() == false) {
				return StdlibType::None;
			}
			break;
		}
        case StdlibType::alloca: {
            if((parameters[0]->GetType()->IsInt32() ||
                parameters[0]->GetType()->IsInt64()) == false) {
                return StdlibType::None;
            }

            if((function->ReturnType()->IsPointer() &&
                function->ReturnType()->As<PointerType>()
                ->PointeeType()->IsInt64()) == false) {
                return StdlibType::None;
            }
            break;
        }
		case StdlibType::isascii:
		case StdlibType::isdigit: {
			if(parameters[0]->GetType()->IsInt32() == false) {
				return StdlibType::None;
			}

			if(function->ReturnType()->IsInt32() == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::strcpy: 
		case StdlibType::strstr:
		case StdlibType::strpbrk:
		case StdlibType::strcat: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()    
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8())) == false) {
				return StdlibType::None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt8()) == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::strlen: {
			if((parameters[0]->GetType()->IsPointer() &&
				parameters[0]->GetType()->As<PointerType>()
                                        ->PointeeType()->IsInt8()) == false) {
				return StdlibType::None;
			}

			if(function->ReturnType()->IsInt32() == false) {
				return StdlibType::None;
			}
			break;
		}
        case StdlibType::strncpy: {
            if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8())) == false) {
				return StdlibType::None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt8()) == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::strcmp: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8())) == false) {
				return StdlibType::None;
			}

			if(function->ReturnType()->IsInt32() == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::strcspn:
		case StdlibType::strncmp:
		case StdlibType::strncat: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[2]->GetType()->IsInt64() || 
                 parameters[2]->GetType()->IsInt32())) == false) {
				return StdlibType::None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt8()) == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::strchr: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				 parameters[1]->GetType()->IsInt32()) == false) {
				return StdlibType::None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt8()) == false) {
				return StdlibType::None;
			}
			break;
		}
		case StdlibType::strspn: {
			if(((parameters[0]->GetType()->IsPointer() &&
				 parameters[0]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8()) &&
				(parameters[1]->GetType()->IsPointer() &&
				 parameters[1]->GetType()->As<PointerType>()
                                         ->PointeeType()->IsInt8())) == false) {
				return StdlibType::None;
			}

			if((function->ReturnType()->IsPointer() &&
				function->ReturnType()->As<PointerType>()
                                      ->PointeeType()->IsInt64()) == false) {
				return StdlibType::None;
			}
			break;
		}
        case StdlibType::abort: {
            if(function->ReturnType()->IsVoid() == false) {
                return StdlibType::None;
            }
            break;
        }
        case StdlibType::exit: {
            if(parameters[0]->GetType()->IsInt32() == false) {
				return StdlibType::None;
			}

            if(function->ReturnType()->IsVoid() == false) {
                return StdlibType::None;
            }
            break;
        }
        case StdlibType::setjmp: {
            if(parameters[0]->GetType()->IsPointer() == false) {
				return StdlibType::None;
			}

            if(function->ReturnType()->IsInt32() == false) {
                return StdlibType::None;
            }
            break;
        }
        case StdlibType::longjmp: {
            if((parameters[0]->GetType()->IsPointer() == false) ||
               (parameters[1]->GetType()->IsInt32() == false)) {
				return StdlibType::None;
			}

            if(function->ReturnType()->IsVoid() == false) {
                return StdlibType::None;
            }
            break;
        }
	}

	if(category) {
		// The user requests the category of the function.
		*category = descriptor->Category;
	}

	return descriptor->Type;
}

} // namespace Analysis