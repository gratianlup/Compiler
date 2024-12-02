// LogFileTarget.hpp
// Copyright (c) Lup Gratian
//
// Represents a log target that writes the messages to a file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_LOG_FILE_TARGET_HPP
#define PC_BASE_LOG_FILE_TARGET_HPP

#include "LogTarget.hpp"
#include "String.hpp"
#include <fstream>

namespace Base {

class LogFileTarget : public LogTarget {
private:
	template <class T>
	struct StreamSelector {
		typedef std::ofstream Type;
	};

	template <>
	struct StreamSelector<wchar_t> {
		typedef std::wofstream Type;
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	string path_;
	StreamSelector<string::TChar>::Type file_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	LogFileTarget(const string& path) : path_(path) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool Open() {
		file_.open(path_.Chars());
		return file_.is_open();
	}

	virtual bool Close() {
		file_.close();
		return true;
	}

	virtual void Log(const string& message, LogSeverity severity, int level) {
		string title;

		switch(severity) {
			case LogSeverity::Error: {
				title = _T("[ERROR  ]");
				break;
			}
			case LogSeverity::Warning: {
				title = _T("[WARNING]");
				break;
			}
			case LogSeverity::Info: {
				title = _T("[INFO   ]");
				break;
			}
		}

		for(int i = 0; i < level; i++) {
			title += _T("\t");
		}

		title += _T(" ");
		file_<<title.Chars()<<message.Chars()<<std::endl;
		file_.flush();
	}

	virtual void Section(const string& message, int level) {
		string title = _T("[SECTION]");

		for(int i = 0; i < level; i++) {
			title += _T("\t");
		}

		title += _T(" ");
		file_<<title.Chars()<<message.Chars()<<std::endl;
		file_.flush();
	}
};

} // namespace Base
#endif