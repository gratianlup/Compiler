// ProcessHelper.hpp
// Copyright (c) Lup Gratian
//
// Exposes Process-related functionality for the Windows OS.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_PROCESS_HELPER_HPP
#define PC_ABSTRACTION_PROCESS_HELPER_HPP

#define NOMINMAX
#include <Windows.h>
#include "../../Base/String.hpp"
#include "../../Base/List.hpp"
using namespace Base;

namespace Abstraction {

class ProcessHelper {
public:
	// Launches an application having the specified command-line arguments.
	static bool Create(const string& path, const List<string>& arguments) {
		string cmdLine = path;
		::STARTUPINFO startInfo = {sizeof(::STARTUPINFO)};
		::PROCESS_INFORMATION procInfo;

		// Concatenate the arguments with the path.
		for(int i = 0; i < arguments.Count(); i++) {
			cmdLine = cmdLine + " " + arguments[i];
		}
		
		if(::CreateProcess(path.Chars(), cmdLine.Chars(), nullptr, nullptr, true,
						   0, nullptr, nullptr, &startInfo, &procInfo)) {
			::WaitForSingleObject(procInfo.hProcess, INFINITE);
			::CloseHandle(procInfo.hProcess);
			::CloseHandle(procInfo.hThread);
			return true;
		}
		else return false;
	}

	// Launches the specified application
	static bool Create(const string& path) {
		::STARTUPINFO startInfo = {sizeof(::STARTUPINFO)};
		::PROCESS_INFORMATION procInfo;
		startInfo.cb = sizeof(::STARTUPINFO);

		if(::CreateProcess(path.Chars(), nullptr, nullptr, nullptr, true,
						   0, nullptr, nullptr, &startInfo, &procInfo)) {
			::WaitForSingleObject(procInfo.hProcess, INFINITE);
			::CloseHandle(procInfo.hProcess);
			::CloseHandle(procInfo.hThread);
			return true;
		}
		else return false;
	}
};

} // namespace Abstraction
#endif