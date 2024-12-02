// Directory.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_DIRECTORY_HPP
#define PC_BASE_DIRECTORY_HPP

#include "String.hpp"
#include "DebugValidator.hpp"
#include "LocalPointer.hpp"
#include "Path.hpp"
#include "SharedPointer.hpp"
#include "LocalPointer.hpp"
#include "List.hpp"
#include "../Abstraction/Platform.hpp"
using namespace Abstraction;

namespace Base {

// Specifies whether to search the current directory, or the current directory
// and all its subdirectories.
enum class SearchOptions {
	TopDirectoryOnly,
	AllDirectories
};

// Specifies the type of files to be searched.
enum class FileType {
	Directory = 0x01,
	File	  = 0x02,
	All	      = File | Directory
};


class Directory {
private:
	static void GetFilesImpl(const string &path, const string &pattern, 
							 SearchOptions options, FileType fileType, List<string>* list) {
		bool allFilter = (options == SearchOptions::AllDirectories) &&
						 ((pattern == _T("*")) || (pattern == _T("*.*")));

		IO::FindFiles(path, pattern, [=](const string& file, bool isDir) mutable -> bool {
			if(file[0] == _T('.')) {
				return true; // Skip over '.' and '..'.
			}

			if(isDir == false) {
				if(((int)fileType & (int)FileType::File) != 0) {
					list->Add(file);
				}
			}
			else {
				if(((int)fileType & (int)FileType::Directory) != 0) {
					list->Add(file);
				}

				if(allFilter) {
					// Get files from subfolder.
					GetFilesImpl(file, pattern, options, fileType, list);
				}
			}

			return true; // Continue searching.
		});

		if((options == SearchOptions::AllDirectories) && (allFilter == false)) {
			// Scan again to find the subfolders.
			IO::FindFiles(path, _T("*"), [=](const string& file, bool isDir) mutable -> bool {
				if(file[0] == _T('.')) {
					return true; // Skip over '.' and '..'.
				}

				if(isDir) {
					string filePath = Path::Combine(path, file);
					GetFilesImpl(filePath, pattern, options, fileType, list);
				}

				return true; // Continue searching.
			});
		}
	}

public:
	static bool CreateDirectory(const string& path) {
		return IO::CreateDirectory(Path::GetFullPath(path));
	}

	static shared<List<string>> GetFiles(const string &path, const string &pattern,
										 SearchOptions options = SearchOptions::TopDirectoryOnly) {
		local<List<string>> list = new List<string>();
		GetFilesImpl(Path::GetFullPath(path), pattern, options, FileType::File, list);
		return list;
	}

	static shared<List<string>> GetFiles(const string &path) {
		return GetFiles(path, _T("*"));
	}

	static shared<List<string>> GetDirectories(const string &path, const string &pattern,
											   SearchOptions options = SearchOptions::TopDirectoryOnly) {
		local<List<string>> list = new List<string>();
		GetFilesImpl(path, pattern, options, FileType::Directory, list);
		return list;
	}

	static shared<List<string>> GetDirectories(const string &path) {
		return GetDirectories(path, _T("*"));
	}

	static shared<List<string>> GetFileSystemEntries(const string &path, const string &pattern,
													 SearchOptions options = SearchOptions::TopDirectoryOnly) {
		local<List<string>> list = new List<string>();
		GetFilesImpl(path, pattern, options, FileType::All, list);
		return list;
	}

	static List<string>* GetFileSystemEntries(const string &path) {
		return GetFileSystemEntries(path, _T("*"));
	}

	static bool Delete(const string &path, bool recursive) {
		string fullPath = Path::GetFullPath(path);
		if(IO::DirectoryExists(fullPath) == false) {
			return false;
		}

		if(recursive) {
			// Delete all subdirectories first.
			local<List<string>> subDirs = GetDirectories(path, _T("*"), 
														 SearchOptions::AllDirectories);

			subDirs->ForEach([](const string& dir) -> bool {
				IO::DeleteDirectory(dir);
				return true;
			});
		}

		// Delete the root folder.
		IO::DeleteDirectory(fullPath);
	}

	static shared<List<string>> GetLogicalDrives() {
		local<List<string>> list = new List<string>();

		IO::GetLogicalDrives([&list](const string& drive) -> bool {
			list->Add(drive);
			return true;
		});

		return list;
	}

	static bool Exists(const string& path) {
		return IO::DirectoryExists(Path::GetFullPath(path));
	}

	static string GetCurrentDirectory() {
		return IO::CurrentDirectory();
	};

	static bool SetCurrentDirectory(const string& path) {
		return IO::SetCurrentDirectory(Path::GetFullPath(path));
	};

	static string GetParent(const string& path) {
		return Path::GetDirectoryName(path);
	}

	static DateTime GetCreationTime(const string& path) {
		return IO::GetCreationTime(Path::GetFullPath(path));
	}

	static bool SetCreationTime(const string& path, const DateTime& time) {
		return IO::SetCreationTime(Path::GetFullPath(path), time);
	}

	static DateTime GetLastAccessTime(const string& path) {
		return IO::LastAccessTime(Path::GetFullPath(path));
	}

	static bool SetLastAccessTime(const string& path, const DateTime& time) {
		return IO::SetLastAccessTime(Path::GetFullPath(path), time);
	}

	static DateTime GetLastWriteTime(const string& path) {
		return IO::LastWriteTime(Path::GetFullPath(path));
	}

	static bool SetLastWriteTime(const string& path, const DateTime& time) {
		return IO::SetLastWriteTime(Path::GetFullPath(path), time);
	}

	static bool Move(const string& source, const string& dest) {
		return IO::MoveDirectory(Path::GetFullPath(source),
								 Path::GetFullPath(dest));
	}
};

} // namespace Base
#endif