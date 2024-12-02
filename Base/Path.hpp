// Path.hpp
// Copyright (c) Lup Gratian
//
// Performs operations on String instances that contain file or directory path information.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_PATH_HPP
#define PC_BASE_PATH_HPP

#include "String.hpp"
#include "DebugValidator.hpp"
#include "../Abstraction/Platform.hpp"
#include <random>
using namespace Abstraction;

namespace Base {

struct PathConst {
	// Characters used to generate the random name.
	const static string::TChar RANDOM_CHARS[];
	const static int RANDOM_CHAR_NUMBER;
	const static int RANDOM_NAME_LENGTH = 20; // 16.3 format.
};


class Path : public PathConst {
private:
	typedef DebugValidator Validator;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Fills the specified array with random numbers.
	static void FillRandom(void* dest, int length) {
		// Initialize random generator;
		std::srand(Time::TickCount());

		char* destPtr = (char*)dest;

		while(length >= sizeof(int)) {
			*((int*)destPtr) = std::rand();
			destPtr += sizeof(int);
			length -= sizeof(int);
		}

		while(length--) {
			*destPtr++ = std::rand() % 256;
		}
	}

	// Checks if the path contains invalid characters.
	static bool ValidatePathChars(const string& path) {
		int length = path.Length();
		for(int i = 0; i < length; i++) {
			string::TChar c = path[i];

			if((c <= 31) || (c == _T('\"')) || (c == _T('<')) ||
				(c == _T('>')) || (c == _T('|'))) {
				return false;
			}
		}

		return true;
	}

	// Verifies whether the specified character is a directory separator.
	static bool IsDirectorySeparator(wchar_t c) {
		return ((c == IO::DIRECTORY_SEPARATOR) || (c == IO::ALT_DIRECTORY_SEPARATOR));
	}
	
	// Gets the length of the root component of the specified path.
	static int GetRootLength(const string& path) {
		int rootLength = 0;
		int length = path.Length();

		if((length >= 1) && IsDirectorySeparator(path[0])) {
			rootLength = 1;
			if(length >= 2 && IsDirectorySeparator(path[1])) {
				// UNC name
				rootLength = 2;
				while((rootLength < length) && 
					  (IsDirectorySeparator(path[rootLength]) == false)) {
					rootLength++;
				}
			}
		}
		else if((length >= 2) && (path[1] == IO::VOLUME_SEPARATOR)) {
			// Standard path C:\test.abc
			rootLength = 2;
			if((length >= 3) && IsDirectorySeparator(path[2])) { // Test for \ or /.
				rootLength = 3;
			}
		}

		return rootLength;
	}

public:
	// Changes the extension of a path string.
	static string ChangeExtension(const string& path, const string& extension) {
		Validator::IsFalse(string::IsEmpty(path));
		Validator::IsTrue(ValidatePathChars(path));
		
		// Find the start of the extension.
		int length = path.Length();
		for(int i = length - 1; i >= 0; i--) {
			string::TChar c = path[i];

			if(c == _T('.')) {
				if((extension.Length() == 0) || (extension[0] != _T('.'))) {
					return string::Concat(path.Substring(0, i), _T("."), extension);	
				}
				else {
					return string::Concat(path.Substring(0, i), extension);
				}
			}
			else if((c == IO::DIRECTORY_SEPARATOR) || (c == IO::ALT_DIRECTORY_SEPARATOR)) {
				// Reached a directory separator, we need to stop.
				break;
			}
		}

		// Return the path if no extension could be found.
		return path + extension;
	}

	// Returns the directory information for the specified path string.
	static string GetDirectoryName(const string& path) {
		Validator::IsFalse(string::IsEmpty(path));
		Validator::IsTrue(ValidatePathChars(path));
		int rootLength = GetRootLength(path);
		int length = path.Length();

		if(length <= rootLength) {
			return ""; // Only the root is available.
		}

		int i = length - 1;
		while(i >= (rootLength - 1)) {
			if(IsDirectorySeparator(path[i])) {
				// Reached directory separator, we need to stop.
				return path.Substring(0, i);
			}

			i--;
		}

		return "";
	}
	
	// Returns the extension of the specified path string.
	static string GetExtension(const string& path) {
		Validator::IsFalse(string::IsEmpty(path));
		Validator::IsTrue(ValidatePathChars(path));
		
		// Find the first valid extension separator '.'
		int length = path.Length();
		for(int i = length - 1; i >= 0; i--) {
			string::TChar c = path[i];
			if(c == _T('.')) {
				if(i == (length - 1)) {
					return ""; // Not valid.
				}

				return path.Substring(i, length - i);
			}

			if(IsDirectorySeparator(c) || (c == IO::VOLUME_SEPARATOR)) {
				break; // Reached directory/volume separator.
			}
		}

		return "";
	}

	// Returns the file name and extension of the specified path string.
	static string GetFileName(const string& path) {
		Validator::IsTrue(ValidatePathChars(path));
		
		// Find first directory separator
		int length = path.Length();

		for(int i = length - 1; i >= 0; i--) {
			if(IsDirectorySeparator(path[i])) {
				return path.Substring(i + 1, length - i - 1);
			}
		}

		return "";
	}

	// Returns the file name of the specified path string without the extension.
	static string GetFileNameWithoutExtension(const string& path) {
		string name = GetFileName(path);

		if(string::IsEmpty(name) == false) {
			int pointIndex = name.LastIndexOf(_T('.'));
			if(pointIndex != -1)  {
				return name.Substring(0, pointIndex);
			}
		}

		return "";
	}

	// Gets a value indicating whether the specified path string contains absolute 
	// or relative path information.
	static bool IsPathRooted(const string& path) {
		Validator::IsTrue(ValidatePathChars(path));
		
		if(string::IsEmpty(path) == false) {
			int length = path.Length();
			if(((length >= 1) && IsDirectorySeparator(path[0])) || // /xyz
				(length >= 2) && (path[1] == IO::VOLUME_SEPARATOR)) { // X:
				return true;
			}
		}

		return false; // Empty.
	}
	
	// Combines two path strings.
	static string Combine(const string& path1, const string& path2) {
		Validator::IsTrue(ValidatePathChars(path1));
		Validator::IsTrue(ValidatePathChars(path2));
		
		if(string::IsEmpty(path1)) return path2;
		else if(string::IsEmpty(path2)) return path1;

		if(IsPathRooted(path2)) return path2;

		// See if the last character is a directory separator.
		string::TChar c = path1[path1.Length() - 1];
		if(IsDirectorySeparator(c) == false) {
			string::TChar sep = IO::DIRECTORY_SEPARATOR;
			return string::Concat(path1, string(sep, 1), path2);
		}

		return string::Concat(path1, path2);
	}

	// Returns the absolute path for the specified path string.
	static string GetFullPath(const string& path) {
		Validator::IsFalse(string::IsEmpty(path));
		Validator::IsTrue(ValidatePathChars(path));
		// ------------------------------------------------------------------ *
		return IO::GetFullPath(path);
	}

	// Gets the root directory information of the specified path.
	static string GetPathRoot(const string& path) {
		Validator::IsFalse(string::IsEmpty(path));
		Validator::IsTrue(ValidatePathChars(path));
		// ------------------------------------------------------------------ *
		return path.Substring(0, GetRootLength(path));
	}

	// Returns a random folder name or file name.
	static string GetRandomFileName() {
		string::TChar buffer[RANDOM_NAME_LENGTH + 1];
		char random[(int)(RANDOM_NAME_LENGTH / 1.6) + 1];
		int charCount = 0;
		int unused = 0;
		int randomPos = 0;

		// Use only 5 bites/character (enough to do the lookup in our 32-character table).
		FillRandom(random, (int)(RANDOM_NAME_LENGTH / 1.6) + 1);

		while(charCount < RANDOM_NAME_LENGTH) {
			int index = 0;

			if(unused != 0) {
				// Take the unused bits from the previous byte.
				index = random[randomPos - 1] & ((1 << unused) - 1);
			}

			// Take bits from the current byte.
			int needed = 5 - unused;
			index |= (random[randomPos] & (((1 << needed) - 1) << (8 - needed))) >> (8 - needed);
			unused = 8 - needed;

			// Store the char.
			buffer[charCount++] = RANDOM_CHARS[index];

			// See if we can form another char from the current byte.
			if(unused >= 5) {
				index = (buffer[randomPos] & ((0x1F << (3 - needed)))) >> (3 - needed);
				buffer[charCount++] = RANDOM_CHARS[index];
				unused = 3 - needed;
			}

			randomPos++; // Advance the byte position.
		}

		// Put the extension mark and the 0 string terminator.
		buffer[RANDOM_NAME_LENGTH - 4] = _T('.');
		buffer[RANDOM_NAME_LENGTH] = 0;
		return string(buffer);
	}
	
	// Creates a uniquely named, zero-byte temporary file on disk 
	// and returns the full path of that file.
	static string GetTempFile(const string& prefix = "", int id = 0) {
		string result;

		if(IO::GetTempName(result, IO::GetTempDirectory(), prefix, id)) {
			return result;
		}
		else {
			return "";
		}
	}

	// Returns the path of the current system's temporary folder.
	static string GetTempPath() {
		return IO::GetTempDirectory();
	}

	// Determines whether a path includes a file name extension.
	static bool HasExtension(const string& path) {
		Validator::IsFalse(string::IsEmpty(path));
		Validator::IsTrue(ValidatePathChars(path));
		// ------------------------------------------------------------------ *
		// Find the first valid extension separator '.'
		int length = path.Length();
		for(int i = length - 1; i >= 0; i--) {
			string::TChar c = path[i];
			if(c == _T('.')) {
				if(i == (length - 1)) return false; // Not valid.
				return true;
			}

			if(IsDirectorySeparator(c) || (c == IO::VOLUME_SEPARATOR)) {
				return false; // Reached directory/volume separator.
			}
		}

		return false;
	}
};

} // namespace Base
#endif