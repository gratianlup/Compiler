// Diagnostics.hpp
// Copyright (c) Lup Gratian
//
// Implements the diagnostic system.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_DIAGNOSTICS_HPP
#define PC_COMMON_DIAGNOSTICS_HPP

#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/Log.hpp"
#include "../Base/NoCopy.hpp"
#include "../AST/Identifier.hpp"
#include "Errors.hpp"
#include "Warnings.hpp"
#include "Info.hpp"
#include "ConstantGen.hpp"
#include "CompileOptions.hpp"
#include "LocationInfo.hpp"
#include <utility>
using namespace AST;
using namespace Base;

namespace Common {

// The kind of argument stored in an 'Argument' object.
enum ArgumentKind {
	Arg_String,
	Arg_Char,
	Arg_Int,
	Arg_Long,
	Arg_Location,
	Arg_Range,
	Arg_Identifier
};


// Stores the type and data of a message argument.
// Can hold basic types and any other object.
class Argument {
private:
	// Always occupies 8 bytes.
	union ArgumentData {
		wchar_t Char_Argument; // For 'Arg_Char'.
		int     IntArgument;   // For 'Arg_Int'.
		__int64 LongArgument;  // For 'Arg_Long'.
		void*   OtherArgument; // For 'Arg_String' and other types.

		ArgumentData() : OtherArgument(nullptr) {}
		ArgumentData(const ArgumentData& other) : OtherArgument(other.OtherArgument) {}

		explicit ArgumentData(wchar_t value) : Char_Argument(value) {}
		explicit ArgumentData(int value) : IntArgument(value) {}
		explicit ArgumentData(__int64 value) : LongArgument(value) {}
		explicit ArgumentData(void* value) : OtherArgument(value) {}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ArgumentKind kind_;
	ArgumentData data_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Argument() : kind_(Arg_Int) {} // Needed for List<T>.

	Argument(wchar_t value) : 
			kind_(Arg_Char), data_(value) {}

	Argument(int value) : 
			kind_(Arg_Int), data_(value) {}

	Argument(__int64 value) : 
			kind_(Arg_Long), data_(value) {}

	Argument(const string& value) : 
			kind_(Arg_String), data_(new string(value)) {}

	Argument(const LocationInfo& value) : 
			kind_(Arg_Location), data_(new LocationInfo(value)) {}

	Argument(const RangeInfo& value) : 
			kind_(Arg_Range), data_(new RangeInfo(value)) {}

	Argument(const Identifier& value) : 
			kind_(Arg_Identifier), data_(new Identifier(value)) {}

	~Argument() {
		// If this doesn't hold a basic type the data must be freed.
		if((IsInt() || IsLong() || IsChar()) == false) {
			delete data_.OtherArgument;
			data_.OtherArgument = nullptr;
			
		}
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the kind of the argument.
	ArgumentKind Kind() const {
		return kind_;
	}

	// Verifies if the kind of the argument is the specified one.
	bool Is(ArgumentKind kind) const {
		return kind_ == kind;
	}

	// Verifies if the argument is of type 'wchar_t'.
	bool IsChar() const {
		return Is(Arg_Char);
	}

	// Verifies if the argument is of type 'int'.
	bool IsInt() const {
		return Is(Arg_Int);
	}

	// Verifies if the argument is of type 'long'.
	bool IsLong() const {
		return Is(Arg_Long);
	}

	// Verifies if the argument is of type 'string'.
	bool IsString() const {
		return Is(Arg_String);
	}

	// Returns the argument data as an 'wchar_t'.
	wchar_t AsChar() const {
		DebugValidator::IsTrue(IsChar());
		return data_.Char_Argument;
	}

	// Returns the argument data as an 'int'.
	int AsInt() const {
		DebugValidator::IsTrue(IsInt());
		return data_.IntArgument;
	}

	// Returns the argument data as a 'long'.
	__int64 AsLong() const {
		DebugValidator::IsTrue(IsLong());
		return data_.LongArgument;
	}

	// Returns the argument data as the specified type.
	// Should not be used with basic types (int, long).
	template <class T>
	T* DataAs() const {
		DebugValidator::IsFalse(IsInt() && IsLong());
		return reinterpret_cast<T*>(data_.OtherArgument);
	}

	// Returns the argument data as a 'string'.
	string* AsString() const {
		DebugValidator::IsTrue(IsString());
		return DataAs<string>();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (Argument& other) const {
		if(kind_ == other.kind_) {
			if(IsInt()) return data_.IntArgument == other.data_.IntArgument;
			else if(IsInt()) return data_.LongArgument == other.data_.LongArgument;
			else if(IsString()) return *AsString() == *other.AsString();
		}

		return false;
	}

	bool operator!= (Argument& other) const {
		return !operator== (other);
	}

	bool operator== (wchar_t other) const {
		return IsChar() && (data_.Char_Argument == other);
	}

	bool operator== (int other) const {
		return IsInt() && (data_.IntArgument == other);
	}

	bool operator== (long other) const {
		return IsLong() && (data_.LongArgument == other);
	}

	bool operator== (const string& other) const {
		return IsString() && (*AsString() == other);
	}

	Argument& operator= (const Argument& other) {
		kind_ = other.kind_;
		data_ = other.data_;
		const_cast<Argument*>(&other)->data_.OtherArgument = nullptr;
		return *this;
	}

	Argument& operator= (Argument&& other) {
		kind_ = other.kind_;
		data_ = other.data_;
		other.data_.OtherArgument = nullptr;
		return *this;
	}
};


// Represents a message generated by an event in the compiler.
// The clients are notified about the event by sending a 'Message' object
// to the registered 'DiagnosticsHandler' objects.
class Message {
private:
	List<Argument> args_;
	DiagnosticCode       code_; // User-mapped code.
	LocationInfo   location_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Converts the specified argument to a string representation.
	string ToString(const Argument& argument) const {
		switch(argument.Kind()) {
			case Arg_Char: {
				return string::Format(_T("%c"), argument.AsChar());
			}
			case Arg_Int: {
				return string::Format(_T("%d"), argument.AsInt());
			}
			case Arg_Long: {
				return string::Format(_T("%I64d"), argument.AsLong());
			}
			case Arg_String: {
				return *argument.AsString();
			}
			case Arg_Location: {
				LocationInfo* location = argument.DataAs<LocationInfo>();
				return string::Format(_T("%d:%d"), location->Line() + 1, location->Position() + 1);
			}
			case Arg_Identifier: {
				Identifier* ident = argument.DataAs<Identifier>();
				return string::Format(_T("%s (%d:%d)"), ident->Name().Chars(),
														ident->Location().Line() + 1,
														ident->Location().Position() + 1);
			}
		}

		return "";
	}

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Message() {}
	
	explicit Message(DiagnosticCode code) : code_(code) {}

	// Implements move semantics.
	Message(Message&& other) {
		args_ = other.args_;
		code_ = other.code_;
		location_ = other.Location();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the diagnostic code associated with the message.
	DiagnosticCode Code() const {
		return code_;
	}

	void SetCode(DiagnosticCode value) {
		code_ = value;
	}

	// Returns true if this is a fatal error.
	bool IsFatal() const {
		return CodeInfo::GetType(code_) == USER_FATAL;
	}

	// Returns true if this is an error.
	bool IsError() const {
		return CodeInfo::GetType(code_) == USER_ERROR;
	}

	// Returns true if this is a warning.
	bool IsWarning() const {
		return CodeInfo::GetType(code_) == USER_WARNING;
	}

	// Returns true if this is an information.
	bool IsInfo() const {
		return CodeInfo::GetType(code_) == USER_INFO;
	}

	// Returns the list of arguments associated with the message.
	List<Argument>& Arguments() {
		return args_;
	}

	// Returns the location associated with the message.
	LocationInfo Location() const {
		return location_;
	}

	void SetLocation(LocationInfo value) {
		location_ = value;
	}

	// Returns the string representation of the arguments according to the
	// specified formatting string. @1...@n are replaced by the arguments.
	string Format(const string& format) const {
		// Implements a simple string formatter that considers #1...#n
		// to be the places to be replaced by arguments.
		// The arguments are replaced one by one.
		string result = format;

		for(int i = 0; i < args_.Count(); i++) {
			result = result.Replace(string::Format(_T("#%d"), i + 1), 
									ToString(args_[i]));
		}

		return result;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Implements move semantics.
	Message& operator= (Message&& other) {
		args_ = std::move(other.args_);
		code_ = other.code_;
		
		return *this;
	}
};


// Base class that must be implemented by modules who want to listen
// to diagnostic messages. Must be registered with a 'Diagnostic' object.
class DiagnosticHandler {
public:
	virtual ~DiagnosticHandler() {}

	// Called when the handler is added to a 'Diagnostic' object.
	virtual bool Open() = 0;

	// Called when the compilation ends or the handler is removed.
	virtual bool Close() = 0;

	// Called when a new message is available.
	virtual void Handle(const Message& message) = 0;

	// Called when the maximum number of errors has been reached.
	virtual void ErrorLimitReached() = 0;
};


// Provides statistics about the number of issued diagnostics.
struct Statistics {
	int FatalCount;
	int ErrorCount;
	int WarningCount;
	int InfoCount;

	Statistics() : FatalCount(0), ErrorCount(0), WarningCount(0), InfoCount(0) {}
};


// The main class of the diagnostic system. Receives diagnostic messages,
// determines the mapping to a user type and notifies the registered handlers.
class Diagnostic {
private:
	// Helper class used to construct a 'Message' object.
	// Overloads the '<<' operator so that arguments can be easily
	// added to the argument list (ex. Report(Warning::Smth)<<arg1<<arg2;).
	// The object is not allowed to be assigned or copied (prevents things
	// like MessageBuilder a = d.Report(...);).
	struct MessageBuilder {
		Diagnostic* parent_;
		Message message_;
		//-------------------------------------------------------------------------------

		MessageBuilder(DiagnosticCode code, Diagnostic* parent) :
				message_(code), parent_(parent) {}

		~MessageBuilder() {
			// Send the message to the handlers before the object is destroyed.
			parent_->SendMessage(message_);
		}

		//-------------------------------------------------------------------------------
		MessageBuilder& operator<< (const Argument& argument) {
			if(message_.Location().IsInvalid()) {
				if(argument.Is(Arg_Location)) {
					message_.SetLocation(*argument.DataAs<LocationInfo>());
				}
				else if(argument.Is(Arg_Range)) {
					message_.SetLocation(argument.DataAs<RangeInfo>()->First());
				}
				else if(argument.Is(Arg_Identifier)) {
					message_.SetLocation(argument.DataAs<Identifier>()->Location());
				}
			}

			message_.Arguments().Add(argument);
			return *this;
		}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	List<shared<DiagnosticHandler>> handlers_;
	CompileOptions* options_;
	Statistics stat_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Verifies if the error limit has been reached, and if true notifies all handlers.
	bool CheckErrorLimit() {
		if(options_->LimitErrors() && (stat_.ErrorCount >= options_->ErrorLimit())) {
			handlers_.ForEach([](shared<DiagnosticHandler> handler) -> bool {
				handler->ErrorLimitReached();
				return true;
			});

			return true;
		}

		return false;
	}

	// Sends the message to all the handlers. Message type codes are translated to
	// user codes depending on the options set by the client.
	void SendMessage(Message& message) {
		// Exit if diagnostics are disabled.
		if(options_->IsDiagnosticDisabled()) return;

		DiagnosticCode code = message.Code();
		int type= CodeInfo::GetType(code);

		// Map the internal type to a user type.
		// TYPE_ERROR   -> USER_ERROR, or USER_FATAL if the FLAG_FATAL flag is set or
		//				   the user wants errors to be fatal always.
		// TYPE_WARNING -> USER_WARNIG, or USER_ERROR if the user want warnings
		//				   to be treated like errors.
		// TYPE_INFO    -> USER_INFO
		switch(type) {
			case TYPE_ERROR: {
				if(CheckErrorLimit()) {
					return; // Maximum number of errors reached.
				}

				if(CodeInfo::IsFatal(message.Code()) || options_->ErrorsAreFatal()) {
					stat_.FatalCount++;
					message.SetCode(CodeInfo::SetType(code, USER_FATAL));
				}
				else {
					stat_.ErrorCount++;
					message.SetCode(CodeInfo::SetType(code, USER_ERROR));
				}

				break;
			}
			case TYPE_WARNING: {
				if(options_->NoWarnings()) {
					// If the client doesn't want warnings, don't send them.
					return;
				}
				else if(options_->WarningsAreErrors()) {
					stat_.ErrorCount++;
					if(CheckErrorLimit()) {
						return; // Maximum number of errors reached.
					}

					message.SetCode(CodeInfo::SetType(code, USER_ERROR));
				}
				else {
					stat_.WarningCount++;
					message.SetCode(CodeInfo::SetType(code, USER_WARNING));
				}

				break;
			}
			case TYPE_INFO: {
				if(options_->IgnoreInfo()) {
					// The client doesn't want info messages.
					return;
				}
				
				stat_.InfoCount++;
				message.SetCode(CodeInfo::SetType(code, USER_INFO));
				break;
			}
		}

		// Notify all handlers about the diagnostic.
		handlers_.ForEach([&message](shared<DiagnosticHandler> handler) -> bool {
			handler->Handle(message);
			return true;
		});
	}

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Diagnostic(CompileOptions* options) : options_(options) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Reports the specified diagnostic. Arguments can be appended using the << operator
	// like in d.Report(Error::Smth)<<arg1<<arg2.
	MessageBuilder Report(DiagnosticCode code) {
		return MessageBuilder(code, this);
	}

	// Adds a new handler that is going to receive diagnostic messages.
	void AddHandler(shared<DiagnosticHandler> handler) {
		if(handler->Open() == false) {
			Log::Error("Failed to open diagnostic handler");
			return;
		}

		handlers_.Add(handler);
	}

	// Removes a handler from the list. Returns 'false' if the handler couldn't be found.
	bool RemoveHandler(shared<DiagnosticHandler> handler) {
		handler->Close();
		return handlers_.Remove(handler);
	}

	// Removes all associated handlers.
	void ClearHandlers() {
		handlers_.ForEach([](shared<DiagnosticHandler> handler) -> bool {
			handler->Close();
			return true;
		});

		handlers_.Clear();
	}

	// Returns the number of associated handlers.
	int HandlerCount() const {
		return handlers_.Count();
	}

	// Returns statistics about the number of errors, warnings, etc.
	Statistics GetStatistics() const {
		return stat_;
	}
};

} // namespace Common
#endif