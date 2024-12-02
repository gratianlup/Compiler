// File.hpp
// Copyright (c) Lup Gratian
//
// Provides static methods for the creation, copying, deletion, moving,
// and opening of files, and aids in the creation of FileStream objects. 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_FILE_HPP
#define PC_BASE_FILE_HPP

#include "String.hpp"
#include "DebugValidator.hpp"
#include "FileStream.hpp"
#include "Path.hpp"
#include "SharedPointer.hpp"
#include "../Abstraction/Platform.hpp"
using namespace Abstraction;

namespace Base {

class File {
private:
	typedef DebugValidator Validator;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Creates or overwrites the specified file, specifying a buffer size 
	// and a FileOptions value that describes how to create or overwrite the file.
	static shared<FileStream> Create(const string& path, int bufferSize, FileOptions options) {
		return new FileStream(Path::GetFullPath(path), FileMode::Create, 
							  FileAccess::ReadWrite, FileShare::None, bufferSize, options);
	}

	// Creates or overwrites the specified file, specifying a buffer size;
	static shared<FileStream> Create(const string& path, int bufferSize) {
		return Create(path, bufferSize, FileOptions::None);
	}

	// Creates or overwrites the specified file.
	static shared<FileStream> Create(const string& path) {
		return Create(path, 4096, FileOptions::None);
	}

	// Copies an existing file to a new file. Overwriting a file of the same name is allowed.
	static bool Copy(const string& source, const string& dest, bool overwrite) {
		return IO::CopyFile(Path::GetFullPath(source), 
							Path::GetFullPath(dest), overwrite);
				
	}

	// Copies an existing file to a new file. Overwriting a file of the same name is allowed.
	static bool Copy(const string& source, const string& dest) {
		return Copy(source, dest, false);
	}

	// Deletes the specified file.
	static bool Delete(const string& path) {
		string fullPath = Path::GetFullPath(path);

		if(IO::DirectoryExists (fullPath)) {
			return false; // It's a directory.
		}

		return IO::DeleteFile(fullPath);
	}

	// Determines whether the specified file exists.
	static bool Exists(const string& path) {
		if(string::IsEmpty(path)) return false;

		return IO::FileExists(Path::GetFullPath(path));
	}

	// Gets the attributes of the specified file.
	static FileAttributes GetAttributes(const string& path) {
		FileAttributes attr;

		if(IO::GetFileAttributes(Path::GetFullPath(path), attr)) {
			return attr;
		}
		else return FileAttributes::Normal;
	}

	// Sets the attributes of the specified file.
	static bool SetAttributes(const string& path, FileAttributes attr) {
		return IO::SetFileAttributes(Path::GetFullPath(path), attr);
	}

	// Returns the creation date and time of the specified file.
	static DateTime GetCreationTime(const string& path) {
		return IO::GetCreationTime(Path::GetFullPath(path));
	}

	// Sets the creation date and time of the specified file.
	static bool SetCreationTime(const string& path, const DateTime& time) {
		return IO::SetCreationTime(Path::GetFullPath(path), time);
	}

	// Returns the date and time of the specified file was last accessed.
	static DateTime GetLastAccessTime(const string& path) {
		return IO::LastAccessTime(Path::GetFullPath(path));
	}

	// Sets the date and time of the specified file was last accessed.
	static bool SetLastAccessTime(const string& path, const DateTime& time) {
		return IO::SetLastAccessTime(Path::GetFullPath(path), time);
	}

	// Returns the date and time of the specified file was last written to.
	static DateTime GetLastWriteTime(const string& path) {
		return IO::LastWriteTime(Path::GetFullPath(path));
	}

	// Sets the date and time of the specified file was last written to
	static bool SetLastWriteTime(const string& path, const DateTime& time) {
		return IO::SetLastWriteTime(Path::GetFullPath(path), time);
	}

	// Moves a specified file to a new location, providing the option 
	// to specify a new file name.
	static bool Move(const string& source, const string& dest) {
		return IO::MoveFile(Path::GetFullPath(source),
							Path::GetFullPath(dest));
	}

	// Opens a FileStream on the specified path, having the specified mode with read, write, 
	// or read/write access and the specified sharing option.
	static shared<FileStream> Open(const string& path, FileMode mode, FileAccess access, 
								   FileShare share) {
		return new FileStream(Path::GetFullPath(path), mode, access, share);
	}

	// Opens a FileStream on the specified path, with the specified mode and access.
	static shared<FileStream> Open(const string& path, FileMode mode, FileAccess access) {
		return Open(path, mode, access, FileShare::None);
	}

	// Opens a FileStream on the specified path with read/write access.
	static shared<FileStream> Open(const string& path, FileMode mode) {
		return Open(path, mode, 
					(mode == FileMode::Append) ? FileAccess::Write : FileAccess::ReadWrite);
	}

	// Opens an existing file for writing.	
	static shared<FileStream> OpenRead(const string& path) {
		return Open(path, FileMode::Open, FileAccess::Read, FileShare::Read);
	}
	
	// Opens an existing file for writing.
	static shared<FileStream> OpenWrite(const string& path) {
		return Open(path, FileMode::OpenOrCreate, FileAccess::Write, FileShare::None);
	}

	// Opens a binary file, reads the contents of the file into a byte array, 
	// and then closes the file.
	static bool File::ReadAllBytes(const string& path, void* buffer, int bufferLength) {
		Validator::IsNotNull(buffer);
		string fullPath = Path::GetFullPath(path);
		FileStream stream(fullPath, FileMode::Open, FileAccess::Read, FileShare::Read);

		__int64 fileLength = stream.Length();

		if(fileLength > bufferLength) {
			return false; // Not enough space.
		}

		stream.Read(buffer, 0, (int)fileLength);
		stream.Close();
		return true;
	}

	// Creates a new file, writes the specified byte array to the file, 
	// and then closes the file. If the target file already exists, it is overwritten.
	static void File::WriteAllBytes(const string& path, void *buffer, int bufferLength) {
		Validator::IsNotNull(buffer);
		string fullPath = Path::GetFullPath(path);
		FileStream stream(fullPath, FileMode::OpenOrCreate, FileAccess::Write);

		stream.Write(buffer, 0, bufferLength);
		stream.Close();
	}

	// Replaces the contents of a specified file with the contents of another file, 
	// deleting the original file and ignoring merge errors.
	static bool Replace(const string& source, const string& dest) {
		return IO::ReplaceFile(Path::GetFullPath(source), 
							   Path::GetFullPath(dest));
	}
};

} // namespace Base
#endif