// DirectoryInfo.hpp
// Copyright (c) Lup Gratian
//
// Exposes instance methods for creating, moving, and enumerating
// through directories and subdirectories
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_DIRECTORY_INFO_HPP
#define PC_BASE_DIRECTORY_INFO_HPP

#include "String.hpp"
#include "FileSystemInfo.hpp"
#include "Directory.hpp"
#include "LocalPointer.hpp"
#include "List.hpp"
#include "SharedPointer.hpp"
#include "FileInfo.hpp"
#include "../Abstraction/Platform.hpp"
using namespace Abstraction;

namespace Base {

class DirectoryInfo : public FileSystemInfo {
	DirectoryInfo(const string& path) : FileSystemInfo(path) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool Delete(bool recursive = false) {
		return Directory::Delete(fullPath_, recursive);
	}

	virtual bool Exists() {
		return Directory::Exists(fullPath_);
	}

	virtual bool Create() {
		return Directory::CreateDirectory(fullPath_);
	}

	virtual shared<List<DirectoryInfo>> GetDirectories(const string& pattern = _T("*"),
													   SearchOptions options = SearchOptions::TopDirectoryOnly){
		local<List<DirectoryInfo>> dirInfos = new List<DirectoryInfo>();
		local<List<string>> dirs = Directory::GetDirectories(fullPath_, pattern, options);

		dirs->ForEach([dirInfos](const string& path) mutable -> bool {
			dirInfos->Add(DirectoryInfo(path));
			return true;
		});

		return dirInfos;
	}

	virtual shared<List<FileInfo>> GetFiles(const string& pattern = _T("*"),
											SearchOptions options = SearchOptions::TopDirectoryOnly){
		local<List<FileInfo>> fileInfos = new List<FileInfo>();
		local<List<string>> files = Directory::GetFiles(fullPath_, pattern, options);

		files->ForEach([fileInfos](const string& path) mutable -> bool {
			fileInfos->Add(FileInfo(path));
			return true;
		});

		return fileInfos;
	}

	virtual string Name() {
		if(fullPath_.Length() > 3) {
			// Strip the possible '\\' ending character.
			if(fullPath_.EndsWith(Path::DirectorySeparatorChar)) {
				return Path::GetFileName(fullPath_.Substring(0, fullPath_.Length() - 1));
			}
			else return Path::GetFileName(fullPath_);
		}

		return fullPath_; // For 'C:\' return 'C:\'.
	}

	virtual bool MoveTo(const string& dest) {
		return Directory::Move(fullPath_, dest);
	} 
};

} // namespace Base
#endif