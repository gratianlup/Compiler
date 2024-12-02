// IO.hpp
// Copyright (c) Lup Gratian
//
// Exposes IO-related functionality for the Windows OS.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_IO_HPP
#define PC_ABSTRACTION_IO_HPP

#define NOMINMAX
#include <Windows.h>
#include "IOConst.hpp"
#include "../../Base/String.hpp"
#include "../../Base/DateTime.hpp"
#include "../../Base/DebugValidator.hpp"
using namespace Base;

namespace Abstraction {

// Specifies how the operating system should open a file.
enum class FileMode : unsigned int {
	CreateNew    = CREATE_NEW,
	Create       = CREATE_ALWAYS,
	Open         = OPEN_EXISTING,
	OpenOrCreate = OPEN_ALWAYS,
	Truncate     = TRUNCATE_EXISTING,
	Append       = OPEN_ALWAYS
};


// Defines constants for read, write, or read/write access to a file.
enum class FileAccess : unsigned int {
	Read      = GENERIC_WRITE,
	Write     = GENERIC_READ,
	ReadWrite = Read | Write
};


// Contains constants for controlling the kind of access other applications have.
enum class FileShare : unsigned int {
	None      = 0,
	Read      = FILE_SHARE_READ,
	Write     = FILE_SHARE_WRITE,
	ReadWrite = Read | Write,
	Delete    = FILE_SHARE_DELETE
};


// Represents additional options for creating a file.
enum class FileOptions : unsigned int {
	None           = 0,
	WriteThrough   = FILE_FLAG_WRITE_THROUGH,
	Asynchronous   = FILE_FLAG_OVERLAPPED,
	RandomAccess   = FILE_FLAG_RANDOM_ACCESS,
	DeleteOnClose  = FILE_FLAG_DELETE_ON_CLOSE,
	SequentialScan = FILE_FLAG_SEQUENTIAL_SCAN
};


// Provides the fields that represent reference points in streams for seeking.
enum class SeekOrigin {
	Begin   = FILE_BEGIN,
	Current = FILE_CURRENT,
	End     = FILE_END
};


// Provides attributes for files and directories.
enum class FileAttributes {
	ReadOnly		  = FILE_ATTRIBUTE_READONLY,
	Hidden			  = FILE_ATTRIBUTE_HIDDEN,
	System			  = FILE_ATTRIBUTE_SYSTEM,
	Directory		  = FILE_ATTRIBUTE_DIRECTORY,
	Archive			  = FILE_ATTRIBUTE_ARCHIVE,
	Device			  = FILE_ATTRIBUTE_DEVICE,
	Normal			  = FILE_ATTRIBUTE_NORMAL,
	Temporary		  = FILE_ATTRIBUTE_TEMPORARY,
	SparseFile		  = FILE_ATTRIBUTE_SPARSE_FILE,
	ReparsePoint	  = FILE_ATTRIBUTE_REPARSE_POINT,
	Compressed		  = FILE_ATTRIBUTE_COMPRESSED,
	Offline			  = FILE_ATTRIBUTE_OFFLINE,
	NotContentIndexed = FILE_ATTRIBUTE_NOT_CONTENT_INDEXED,
	Encrypted		  = FILE_ATTRIBUTE_ENCRYPTED
};


class IO : public IOConst {
public:
	typedef HANDLE Handle;
	typedef DebugValidator Validator;
	typedef bool (*FindHandler)(const string& path, bool isDirectory);
	typedef bool (*DriveHandler)(string path);
	typedef Handle (*HandleGetter)(string path);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const __int64 FAILED = -1;
	const static string::TChar DIRECTORY_SEPARATOR     = _T('\\');
	const static string::TChar ALT_DIRECTORY_SEPARATOR = _T('/');
	const static string::TChar VOLUME_SEPARATOR        = _T(':');

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Creates or opens a file with the specified options.
	static bool CreateFile(const string& path, Handle& handle,
					 FileMode mode = FileMode::CreateNew,
					 FileAccess access = FileAccess::ReadWrite,
					 FileShare share = FileShare::None,
					 FileOptions options = FileOptions::None) {
		Validator::IsFalse(string::IsEmpty(path));
		handle = ::CreateFile(path.Chars(), (DWORD)access, 
							  (DWORD)share, nullptr /* no SECURITY_ATTRIBUTES */,
							  (DWORD)mode, (DWORD)options, nullptr);

		return handle != INVALID_HANDLE_VALUE;
	}

	// Opens a file with the specified options.
	static bool OpenFile(const string& path, Handle& handle, 
					 FileMode mode = FileMode::Open,
					 FileAccess access = FileAccess::Read,
					 FileShare share = FileShare::Read,
					 FileOptions options = FileOptions::None) {
		Validator::IsFalse(string::IsEmpty(path));
		handle = ::CreateFile(path.Chars(), (DWORD)access,
							  (DWORD)share, nullptr /* no SECURITY_ATTRIBUTES */,
							  (DWORD)mode, (DWORD)options, nullptr);

		return handle != INVALID_HANDLE_VALUE;
	}

	// Closes the specified file.
	static bool CloseFile(Handle handle) {
		return ::CloseHandle(handle) != 0;
	}

	// Reads at most 'length' bytes from the specified file.
	static int ReadFile(Handle handle, void* buffer, int length) {
		Validator::IsNotNull(buffer);
		Validator::IsLargerOrEqual(length, 0);
		int bytesRead;

		if(::ReadFile(handle, buffer, (DWORD)length, (LPDWORD)&bytesRead, nullptr) == 0) {
			return 0;
		}
		else return bytesRead;
	}

	// Writes at most 'length' bytes to the specified file.
	static int WriteFile(Handle handle, void* buffer, int length) {
		Validator::IsNotNull(buffer);
		Validator::IsLarger(length, 0);
		int bytesWritten;

		if(::WriteFile(handle, buffer, (DWORD)length, (LPDWORD)&bytesWritten, nullptr) == 0) {
			return 0;
		}
		else return bytesWritten;
	}

	// Seeks into the specified file according to the specified origin.
	static __int64 SeekFile(Handle handle, __int64 offset, 
							SeekOrigin origin = SeekOrigin::Current) {
		LARGE_INTEGER pointer;
		LARGE_INTEGER newPointer;
		pointer.QuadPart = offset;

		if(::SetFilePointerEx(handle, pointer, &newPointer, (DWORD)origin) != 0) {
			return newPointer.QuadPart;
		}
		else return 0;
	}

	// Flushes the contents of the specified file.
	static bool FlushFile(Handle handle) {
		return ::FlushFileBuffers(handle) != 0;
	}

	// Deletes the specified file.
	static bool DeleteFile(const string& path) {
		return ::DeleteFile(path.Chars()) != 0;
	}

	// Checks if the specified file exists.
	static bool FileExists(const string& path) {
		DWORD result = ::GetFileAttributes(path.Chars());
		return (result != INVALID_FILE_ATTRIBUTES) &&
				(result != FILE_ATTRIBUTE_DIRECTORY);
	}

	// Checks if the specified directory exists.
	static bool DirectoryExists(const string& path) {
		DWORD result = ::GetFileAttributes(path.Chars());
		return (result != INVALID_FILE_ATTRIBUTES) &&
				(result == FILE_ATTRIBUTE_DIRECTORY);
	}

	// Creates a temporary file with the specified options and returns its path.
	static bool GetTempName(string& result, const string& directory = ".", 
							const string& prefix = "", int id = 0) {
		Validator::IsSmaller(directory.Length(), MAX_PATH - 14);
		string::TChar buffer[MAX_PATH + 1];

		if(::GetTempFileName(directory.Chars(), prefix.Chars(), id, buffer) != 0) {
			result = string(buffer);
			return true;
		}
		else return false;
	}

	// Gets the path of the OS temporary directory.
	static string GetTempDirectory() {
		string::TChar buffer[MAX_PATH + 1];
		
		if(::GetTempPath(MAX_PATH, buffer) != 0) {
			return string(buffer);
		}
		else return "";
	}

	// Creates the specified directory.
	static bool CreateDirectory(const string& path) {
		Validator::IsLarger(path.Length(), 0);
		return ::CreateDirectory(path.Chars(), nullptr) != 0;
	}

	// Deletes the specified directory.
	static bool DeleteDirectory(const string& path) {
		Validator::IsLarger(path.Length(), 0);
		return ::RemoveDirectory(path.Chars()) != 0;
	}

	// Gets the current directory for the running application.
	static string CurrentDirectory() {
		string::TChar buffer[MAX_PATH + 1];
		
		if(::GetCurrentDirectory(MAX_PATH, buffer)) {
			return string(buffer);
		}

		return "";
	}

	// Sets the current directory.
	static bool SetCurrentDirectory(const string& path) {
		return ::SetCurrentDirectory(path.Chars());
	}

	// Finds the files and folders with the specified pattern and
	// calls the handler for each found file. Stops if the handler returns 'false'.
	template <class Predicate>
	static bool FindFiles(const string& path, const string& pattern, Predicate handler) {
		Validator::IsLarger(path.Length(), 0);
		string searchPath = path;
		if(searchPath.EndsWith(PATH_SEPARATOR) == false) {
			searchPath += PATH_SEPARATOR;
		}

		searchPath += pattern;

		// Search the files.
		::WIN32_FIND_DATA data;
		HANDLE handle = ::FindFirstFile(searchPath.Chars(), &data);
		if(handle == INVALID_HANDLE_VALUE) {
			return false;
		}

		do {
			if(data.cFileName[0] == '.') continue;

			string fileName = path;
			if(fileName.EndsWith(PATH_SEPARATOR) == false) {
				fileName += PATH_SEPARATOR;
			}

			fileName += string(data.cFileName);
			bool isFolder = (data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) != 0;

			// Call handler.
			if(handler(fileName, isFolder) == false) {
				// Stop searching.
				FindClose(handle);
				return false;
			}
		} while(::FindNextFile(handle, &data) != 0);

		::FindClose(handle);
		return true;
	}
			
	// Finds all system drives and calls the handler for each one.
	// Stops if the handler returns 'false'.
	template <class Predicate>
	static bool GetLogicalDrives(Predicate handler) {
		// Get the list of drive labels.
		string::TChar buffer[4];
		DWORD data = ::GetLogicalDrives();

		for(__int64 i = 0; i < (sizeof(DWORD) * 8); i++) {
			if((data & (1 << i)) != 0) {
				buffer[0] = (string::TChar)('A' + i);
				buffer[1] = (string::TChar)(':');
				buffer[2] = (string::TChar)('\\');
				buffer[3] = 0;

				// Call the handler.
				if(handler(string(buffer)) == false) {
					// Operation aborted.
					return false;
				}
			}
		}

		return true;
	}

	// Opens a file/folder for editing it's date attributes.
	static bool FileHandle(const string& path, Handle& handle) {
		handle = ::CreateFile(path.Chars(), OPEN_EXISTING, 0, nullptr, 
							  GENERIC_WRITE, FILE_FLAG_BACKUP_SEMANTICS, nullptr);
		return handle != INVALID_HANDLE_VALUE;
	}

	// Gets the size of the specified file.
	static bool GetFileSize(const string& path, __int64& size) {
		WIN32_FILE_ATTRIBUTE_DATA data;

		if(::GetFileAttributesEx(path.Chars(), GetFileExInfoStandard, &data)) {
			size = (__int64(data.nFileSizeHigh) << 32) | data.nFileSizeLow;
			return true;
		}

		return false;
	}

	// Sets the size of the specified file.
	static bool SetFileSize(Handle handle, __int64 size) {
		return ::SetEndOfFile(handle);
	}

	// Gets the attributes of the specified file.
	static bool GetFileAttributes(const string& path, FileAttributes& attr) {
		DWORD result = ::GetFileAttributes(path.Chars());
		if(result != INVALID_FILE_ATTRIBUTES) {
			attr = (FileAttributes)result;
			return true;
		}

		return false;
	}

	// Sets the attributes of the specified file.
	static bool SetFileAttributes(const string& path, const FileAttributes& attr) {
		return ::SetFileAttributes(path.Chars(), (DWORD)attr);
	}

	// Gets the full path of the specified file.
	static string GetFullPath(const string& path) {
		string::TChar fullPath[MAX_PATH + 1];
		unsigned long length;

		length = ::GetFullPathName(path.Chars(), MAX_PATH, fullPath, nullptr);
		if(length == 0) return path;

		return string(fullPath, length);
	}

	// Gets the time the specified file was created.
	static DateTime GetCreationTime(const string& path) {
		Handle handle;
		::FILETIME time;

		if(FileHandle(path, handle)) {
			if(::GetFileTime(handle, &time, nullptr, nullptr)) {
				CloseFile(handle);
				return DateTime::FromFileTime(*((__int64*)&time));
			}

			CloseFile(handle);
		}
	
		return DateTime::MIN_VALUE();
	}

	// Gets the time the specified file was last accessed.
	static DateTime LastAccessTime(const string& path) {
		Handle handle;
		::FILETIME time;

		if(FileHandle(path, handle)) {
			if(::GetFileTime(handle, nullptr, &time, nullptr)) {
				CloseFile(handle);
				return DateTime::FromFileTime(*((__int64*)&time));
			}

			CloseFile(handle);
		}
	
		return DateTime::MIN_VALUE();
	}

	// Gets the time the specified file was last written to.
	static DateTime LastWriteTime(const string& path) {
		Handle handle;
		::FILETIME time;

		if(FileHandle(path, handle)) {
			if(::GetFileTime(handle, nullptr, nullptr, &time)) {
				CloseFile(handle);
				return DateTime::FromFileTime(*((__int64*)&time));
			}

			CloseFile(handle);
		}
	
		return DateTime::MIN_VALUE();
	}

	// Sets the time the specified file was created.
	static bool SetCreationTime(const string& path, const DateTime& time) {
		Handle handle;
		__int64 time64 = time.ToFileTime();
		::FILETIME timeWin = *((::FILETIME*)&time64);

		if(FileHandle(path, handle)) {
			if(::SetFileTime(handle, &timeWin, nullptr, nullptr)) {
				CloseFile(handle);
				return true;
			}

			CloseFile(handle);
		}

		return false;
	}

	// Sets the time the specified file was last accessed.
	static bool SetLastAccessTime(const string& path, const DateTime& time) {
		Handle handle;
		__int64 time64 = time.ToFileTime();
		::FILETIME timeWin = *((::FILETIME*)&time64);

		if(FileHandle(path, handle)) {
			if(::SetFileTime(handle, nullptr, &timeWin, nullptr)) {
				CloseFile(handle);
				return true;
			}

			CloseFile(handle);
		}

		return false;
	}

	// Sets the time the specified file was last written to.
	static bool SetLastWriteTime(const string& path, const DateTime& time) {
		Handle handle;
		__int64 time64 = time.ToFileTime();
		::FILETIME timeWin = *((::FILETIME*)&time64);

		if(FileHandle(path, handle)) {
			if(::SetFileTime(handle, nullptr, nullptr, &timeWin)) {
				CloseFile(handle);
				return true;
			}

			CloseFile(handle);
		}

		return false;
	}

	// Copies the specified file to the given destination, overwriting the file if specified.
	static bool CopyFile(const string& source, const string& dest, bool overwrite) {
		return ::CopyFile(source.Chars(), dest.Chars(), overwrite == false);
	}

	// Moves the specified file to the given destination, overwriting the existing file.
	static bool MoveFile(const string& source, const string& dest) {
		return ::MoveFile(source.Chars(), dest.Chars());
	}

	// Replaces the specified file with 'dest'.
	static bool ReplaceFile(const string& source, const string& dest) {
		return ::ReplaceFile(source.Chars(), dest.Chars(), nullptr,
							 REPLACEFILE_IGNORE_MERGE_ERRORS, nullptr, nullptr);
	}

	// Moves the specified file to the given destination.
	static bool MoveDirectory(const string& source, const string& dest) {
		return ::MoveFile(source.Chars(), dest.Chars());
	}
};

} // namespace Abstraction
#endif