// DiagnosticPrinter.hpp
// Copyright (c) Lup Gratian
//
// Displays diagnostic messages on the console.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_DRIVER_DIAGNOSTIC_PRINTER_HPP
#define PC_DRIVER_DIAGNOSTIC_PRINTER_HPP

#include "../Base/String.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/Log.hpp"
#include "../Common/Diagnostic.hpp"
#include "../Common/Context.hpp"
#include <iostream>
using namespace Base;
using namespace Common;

namespace Driver {

class DiagnosticPrinter : public DiagnosticHandler {
private:
	// Contains the mapping from a code to a string representation.
	Dictionary<DiagnosticCode, string> text_;
	Context* context_;
	bool sendToLog_; // If 'true' messages are send to the Log too.

public:
	DiagnosticPrinter(Context* context, bool toLog = false);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool SendToLog() const {
		return sendToLog_;
	}

	void SetSendToLog(bool value) {
		sendToLog_ = value;
	}

	virtual bool Open() {
		return true;
	}

	virtual bool Close() {
		return true;
	}

	virtual void Handle(const Message& message) {
		/*string format = text_[message.Code()];
		string text = message.Format(format);

		if(message.IsFatal()) {
			std::wcout<<"[FATAL  ] "<<text.Chars();4
		}
		else if(message.IsError()) {
			std::wcout<<"[ERROR  ] "<<text.Chars();
		}
		else if(message.IsWarning()) {
			std::wcout<<"[WARNING] "<<text.Chars();
		}
		else if(message.IsInfo()) {
			std::wcout<<"[INFO   ] "<<text.Chars();
		}*/

		int code = CodeInfo::GetCode(message.Code());
		LocationInfo location = message.Location();
		FileDetails* info;
		context_->FileMgr().GetDetails(location.File(), info);
		string file = Path::GetFileName(info->Path());
		string header = string::Format(_T("%s %d:%d"), file.Chars(), location.Line() + 1, location.Position() + 1);

		if(message.IsError()) {
			if(text_.ContainsKey(code)) {
				Log::Error(string::Format(_T("%s: %s"), header.Chars(),  
										  message.Format(text_[code]).Chars()));
			}
			//else Log::Error(string::Format(_T("%s %s - %s"), header.Chars(), text_[code].Chars(), 
			//							   message.Format(L"@1, @2").Chars()));
		}
		else if(message.IsWarning()) {
			if(text_.ContainsKey(code)) {
			Log::Warning(string::Format(_T("%s - %s"), header.Chars(),
										message.Format(text_[code]).Chars()));
			}
		}
		else {
			Log::Info(string::Format(_T("%s - %s"), header.Chars(),
										message.Format(L"#1, #2").Chars()));
		}
	}

	virtual void ErrorLimitReached() {

	}
};

} // namespace Driver
#endif