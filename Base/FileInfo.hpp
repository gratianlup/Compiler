// FileInfo.hpp
// Copyright (c) Lup Gratian
//
// Provides methods for file operations.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_FILE_INFO_HPP
#define PC_BASE_FILE_INFO_HPP

#include "String.hpp"
#include "FileSystemInfo.hpp"
#include "File.hpp"
#include "../Abstraction/Platform.hpp"
using namespace Abstraction;

namespace Base {

class FileInfo : public FileSystemInfo {
public:
	FileInfo(const string& path) : FileSystemInfo(path) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual bool Delete() {
		return File::Delete(fullPath_);
	}

	virtual bool Exists() {
		return File::Exists(fullPath_);
	}

	virtual string Name() {
		return Path::GetFileName(fullPath_);
	}

	virtual bool CopyTo(const string& dest, bool overwrite) {
		return File::Copy(fullPath_, dest, overwrite);
	}

	virtual bool CopyTo(const string& dest) {
		CopyTo(dest, false);
	}

	virtual FileStream* Create() {
		return File::Create(fullPath_);
	}

	virtual bool MoveTo(const string& dest) {
		return File::Move(fullPath_, dest);
	}

	virtual FileStream* Open(FileMode mode, FileAccess access, FileShare share) {
		return File::Open(fullPath_, mode, access, share);
	}

	virtual FileStream* Open(FileMode mode, FileAccess access) {
		return File::Open(fullPath_, mode, access);
	}

	virtual FileStream* Open(FileMode mode) {
		return File::Open(fullPath_, mode);
	}

	virtual FileStream* OpenRead() {
		return File::OpenRead(fullPath_);
	}

	virtual FileStream* OpenWrite() {
		return File::OpenWrite(fullPath_);
	}

	virtual bool Replace(const string& dest) {
		return File::Replace(fullPath_, dest);
	}

	virtual __int64 Length() {
		__int64 size;

		if(IO::GetFileSize(fullPath_, size)) {
			return size;
		}
		else return -1;
};

} // namespace Base
#endif