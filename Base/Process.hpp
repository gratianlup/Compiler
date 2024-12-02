// Process.hpp
// Copyright (c) Lup Gratian
//
// Implements methods for creating and manipulating processes.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_PROCESS_HPP
#define PC_BASE_PROCESS_HPP

#include "String.hpp"
#include "List.hpp"
#include "..\Abstraction\Platform.hpp"

namespace Base {

class Process {
public:
	// Launches the specified application.
	static bool Start(const string& path) {
		return Abstraction::ProcessHelper::Create(path);
	}

	// Launches an application having the specified command-line arguments.
	static bool Start(const string& path, const string& arguments) {
		List<string> argList;
		argList.Add(arguments);
		return Abstraction::ProcessHelper::Create(path, argList);
	}

	// Launches an application having the specified list of command-line arguments.
	static bool Start(const string& path, const List<string>& arguments) {
		return Abstraction::ProcessHelper::Create(path, arguments);
	}
};

} // namespace Base
#endif