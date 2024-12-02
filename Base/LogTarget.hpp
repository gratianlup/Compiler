// LogTarget.hpp
// Copyright (c) Lup Gratian
//
// Represents the base class for all log targets.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_LOG_TARGET_HPP
#define PC_BASE_LOG_TARGET_HPP

#include "String.hpp"

namespace Base {

// Represents the severity of the message to be logged.
enum class LogSeverity {
	Error,
	Warning,
	Info,
};


class LogTarget {
public:
	bool enabled_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	LogTarget() : enabled_(true) {}
	virtual ~LogTarget() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool Open() { return true; }
	virtual bool Close() { return true; }
	virtual void Log(const string& message, LogSeverity severity, int level) = 0;
	virtual void Section(const string& title, int level) = 0;

	virtual bool Enabled() {
		return enabled_;
	}

	virtual void SetEnabled(bool value) {
		enabled_ = value;
	}
};

} // namespace Base
#endif