// StdlibRecognizer.hpp
// Copyright (c) Lup Gratian
//
// Defines a helper that recognizes functions from the C standard library.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_STD_LIB_RECOGNIZER_HPP
#define PC_ANALYSIS_STD_LIB_RECOGNIZER_HPP

#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/IRTypes.hpp"
#include "../Base/Dictionary.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Represents the category to which a standard function belongs.
enum class StdlibCategory {
	None, // This is not a standard library function.
	Memory,
	String,
	Math,
	Other
};


// Represents the functions that are recognized.
enum class StdlibType {
	None, // This is not a standard library function.

	// Memory
	memcpy,
	memset,
	memmove,
	memcmp,
	malloc,
    free,
    alloca,

	// String
	strlen,
	strcpy,
	strncpy,
	strcmp,
	strncmp,
	strcat,
	strncat,
	strchr,
	strstr,
	strpbrk,
	strspn,
	strcspn,
	//! TODO: Add support for wide characters string functions.
	// Math
	abs, 
	fabs, 
    labs,
	pow,  
	sqrt, 
	exp,  
	floor,
	ceil, 
	log,  
	log10,
	fmod, 
	sin,  
	asin, 
	cos,  
	acos, 
	tan,  
	atan, 
    atan2,

	// Math (float)
	powf,  
	sqrtf, 
	expf,  
	floorf,
	ceilf, 
	logf,  
	log10f,
	fmodf, 
	sinf,  
	asinf, 
	cosf,  
	acosf, 
	tanf,  
	atanf, 

	// Other
	isdigit,
	isascii,
    exit,
    abort,
    setjmp,
    longjmp
};


// Describes a function from the standard library.
struct StdLibDescriptor {
	StdlibType Type;         // The type associated with the function.
	const string Name;       // The name, as written in the source file.
	int Parameters;          // The number of expected parameters.
	StdlibCategory Category; // The category to which the function belongs.

	StdLibDescriptor(StdlibType type, const string& name, 
					 int parameters, StdlibCategory category) :
			Type(type), Name(name), Parameters(parameters), Category(category) {}

	StdLibDescriptor(const StdLibDescriptor& other) :
			Type(other.Type), Name(other.Name), Parameters(other.Parameters),
			Category(other.Category) {}
};


class StdlibRecognizer {
private:
	typedef Dictionary<const string, StdLibDescriptor*> DescriptorDict; 

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	static bool initialized_;
	static DescriptorDict stdLibFuncts_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Initializes the table with the supported functions.
	static void Initialize();

	// Returns a descriptor for the specified function if it seems to be
	// part of the standard library.
	static StdLibDescriptor* GetDescriptor(const Function* function);

public:
	// Returns the type of the specified function if it's part of the standard library,
	// or 'StdlibType::None' if it's not. If 'category' is provided, the category in which
	// the the standard function belongs is stored into it.
	static StdlibType Recognize(const Function* function, 
                                StdlibCategory* category = nullptr);

	// Returns the type of the function called by the specified instruction.
	// Behaves exactly like the 'Recognize' in rest.
	static StdlibType Recognize(const CallInstr* instr, 
                                StdlibCategory* category = nullptr) {
		if(auto function = instr->TargetOp()->As<FunctionReference>()) {
			return Recognize(function->Target(), category);
		}

		return StdlibType::None;
	}

	// Returns 'true' if the specified function is part of the standard library.
	static bool IsFromStdLib(const Function* function) {
		return Recognize(function) != StdlibType::None;
	}
};

} // namespace Analysis
#endif