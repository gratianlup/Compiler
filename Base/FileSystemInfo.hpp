// FileSystemInfo.hpp
// Copyright (c) Lup Gratian
//
// Provides the base class for both FileInfo and DirectoryInfo objects.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_FILE_SYSTEM_INFO_HPP
#define PC_BASE_FILE_SYSTEM_INFO_HPP

#include "String.hpp"
#include "DebugValidator.hpp"
#include "Path.hpp"
#include "../Abstraction/Platform.hpp"
using namespace Abstraction;

namespace Base {

class FileSystemInfo {
protected:
	string fullPath_;
	string origPath_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	FileSystemInfo(const string& path) : origPath_(path) {
		fullPath_ = Path::GetFullPath(origPath_);
	}

	virtual ~FileSystemInfo() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual string FullName() {
		return fullPath_;
	}
	
	virtual FileAttributes Attributes() {
		FileAttributes attr;

		if(IO::GetFileAttributes(fullPath_, attr)) {
			return attr;
		}
		else return FileAttributes::Normal;
	}

	virtual bool SetAttributes(FileAttributes attr) {
		return IO::SetFileAttributes(fullPath_, attr);
	}

	virtual DateTime CreationTime() {
		return IO::GetCreationTime(fullPath_);
	}

	virtual bool SetCreationTime(const DateTime& time) {
		return IO::SetCreationTime(fullPath_, time);
	}

	virtual DateTime LastAccessTime() {
		return IO::LastAccessTime(fullPath_);
	}

	virtual bool SetLastAccessTime(const DateTime& time) {
		return IO::SetLastAccessTime(fullPath_, time);
	}

	virtual DateTime LastWriteTime() {
		return IO::LastWriteTime(fullPath_);
	}

	virtual bool SetLastWriteTime(const DateTime& time) {
		return IO::SetLastWriteTime(fullPath_, time);
	}

	virtual string Extension() {
		return Path::GetExtension(fullPath_);
	}

	virtual string Name() = 0;

	virtual bool Exists() = 0;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator ==(const FileSystemInfo& other) {
		return other.origPath_ == origPath_;
	}

	bool operator !=(const FileSystemInfo& other) {
		return other.origPath_ != origPath_;
	}
};

} // namespace Base
#endif