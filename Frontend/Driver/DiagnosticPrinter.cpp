// DiagnosticPrinter.cpp
// Copyright (c) Lup Gratian
//
// Displays diagnostic messages on the console.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DiagnosticPrinter.h"
#include "../Common/ConstantGen.hpp"
#include "../Common/Warnings.hpp"
#include "../Common/Errors.hpp"
using namespace Common;

namespace Driver {

DiagnosticPrinter::DiagnosticPrinter(Context* context, bool toLog) :
	sendToLog_(toLog), context_(context) {
	/*#define errorMessage(id, text) text_.Add(CodeInfo::GetCode(Error::id), text)
	#define warningMessage(id, text) text_.Add(CodeInfo::GetCode(Warning::id), text)

	#include "DiagnosticMessages.def"
	#undef errorMessage

	#undef warningMessage*/

	#define error(id) text_.Add(CodeInfo::GetCode(Error::id), #id);
	#include "../Common/Errors.def"
	#undef error

	#define warning(id) text_.Add(CodeInfo::GetCode(Warning::id), #id);
	#include "../Common/Warnings.def"
	#undef warning
}

}