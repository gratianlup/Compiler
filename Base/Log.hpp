// Log.hpp
// Copyright (c) Lup Gratian
//
// Represents a place where special events can be logged.
// Note that if 'LOG_ONLY_DEBUG' is defined logging is enabled
// only if 'DEBUG' is also defined.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_LOG_HPP
#define PC_BASE_LOG_HPP

#include "LogTarget.hpp"
#include "String.hpp"
#include "List.hpp"
#include <cmath>

namespace Base {

class Log {
private:
	static List<LogTarget*> targets_;
	static int indent_;

public:
	static bool AddTarget(LogTarget* target) {
		if(target->Open()) {
			targets_.Add(target);
			return true;
		}

		return false;
	}

	static bool RemoveTarget(LogTarget* target) {
		if(targets_.Contains(target)) {
			targets_.Remove(target);
			return target->Close();
		}

		return false;
	}

	static void Error(const string& message) {
		#if !defined(LOG_ONLY_DEBUG) || defined(DEBUG)
		targets_.ForEach([&](LogTarget* target) -> bool {
			if(target->Enabled()) {
				target->Log(message, LogSeverity::Error, indent_);
			}

			return true;
		});
		#endif
	}

	static void Warning(const string& message) {
		#if !defined(LOG_ONLY_DEBUG) || defined(DEBUG)
		targets_.ForEach([&](LogTarget* target) -> bool {
			if(target->Enabled()) {
				target->Log(message, LogSeverity::Warning, indent_);
			}

			return true;
		});
		#endif
	}

	static void Info(const string& message) {
		#if !defined(LOG_ONLY_DEBUG) || defined(DEBUG)
		targets_.ForEach([&](LogTarget* target) -> bool {
			if(target->Enabled()) {
				target->Log(message, LogSeverity::Info, indent_);
			}

			return true;
		});
		#endif
	}

	static void Section(const string& message = "", int level = 0) {
		#if !defined(LOG_ONLY_DEBUG) || defined(DEBUG)
		targets_.ForEach([&](LogTarget* target) -> bool {
			if(target->Enabled()) {
				target->Section(message, level + indent_);
			}

			return true;
		});
		#endif
	}

	static void Enter() {
		#if !defined(LOG_ONLY_DEBUG) || defined(DEBUG)
		indent_++;
		#endif
	}

	static void Exit() {
		#if !defined(LOG_ONLY_DEBUG) || defined(DEBUG)
		indent_ = std::max(0, indent_ - 1);
		#endif
	}
};

} // namespace Base
#endif