// Context.hpp
// Copyright (c) Lup Gratian
//
// Provides an unified place where data regarding the compilation is stored.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_CONTEXT_HPP
#define PC_COMMON_CONTEXT_HPP

#include "../Base/SharedPointer.hpp"
#include "Diagnostic.hpp"
#include "TargetData.hpp"
#include "FileManager.hpp"
#include "CompileOptions.hpp"
using namespace Base;

namespace Common {

class Context {
private:
	TargetData* target_;
	Diagnostic diag_;
	FileManager manager_;
	CompileOptions options_;

public:
	Context(TargetData* target) : target_(target), diag_(&options_) {
		options_.Initialize();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the object that describes the target architecture.
	const TargetData* Target() const {
		return target_;
	}

	// Returns the module used to emit diagnostic messages.
	Diagnostic& Diagnostic() {
		return diag_;
	}

	// Returns the module used to load files from disk.
	FileManager& FileMgr() {
		return manager_;
	}

	const FileManager& FileMgr() const {
		return manager_;
	}

	// Returns an object that contain the options for the compilation process.
	CompileOptions& Options() {
		return options_;
	}

	const CompileOptions& Options() const {
		return options_;
	}
};

} // namespace Common
#endif