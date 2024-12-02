// FileManager.hpp
// Copyright (c) Lup Gratian
//
// Defines classes for loading, caching and managing source files.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_FILE_MANAGER_HPP
#define PC_COMMON_FILE_MANAGER_HPP

#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/Stack.hpp"
#include "../Base/Log.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/MemoryStream.hpp"
#include "../Base/FileStream.hpp"
#include "../Base/Directory.hpp"
#include "../Base/File.hpp"
#include "../Base/Stream.hpp"
#include "../Base/Path.hpp"
#include "../Base/StreamReader.hpp"
#include "../Base/StaticQueue.hpp"
#include "FileId.hpp"
using namespace Base;

namespace Common {

// Contains the text of a source file, directly in the proper encoding.
// The whole file is stored in the buffer.
class FileBuffer : protected MemoryStream {
private:
	typedef string::TChar T;

	FileId file_;

public:
	FileBuffer(FileId file, __int64 capacity) :
			file_(file), MemoryStream(capacity) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns a pointer to the beginning of the buffer.
	T* Buffer() const {
		return (T*)MemoryStream::GetBuffer();
	}

	// Returns a pointer to the end of the buffer.
	T* BufferEnd() const {
		return (T*)MemoryStream::GetBuffer() + MemoryStream::Length();
	}

	// Writes the specified data to the buffer.
	void WriteBuffer(const T* data, __int64 length) {
		MemoryStream::Write(data, 0, (int)(length * sizeof(T)));
	}

	// Returns the length of the file (the buffer could be larger).
	__int64 Length() {
		return (__int64)(MemoryStream::Length() / sizeof(T));
	}

	// Returns the associated file ID.
	FileId File() const {
		return file_;
	}

	// Resets the position of the buffer to the beginning.
	void Reset() {
		MemoryStream::SetPosition(0);
	}
};


// Caches frequently used source files. The cached files are in the final format
// (converted to the internal encoding).
class FileCache {
private:
	static const int MAX_CAPACITY = 256;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Dictionary<FileId, shared<FileBuffer>> cache_;
	StaticQueue<FileId, MAX_CAPACITY> cachedFiles_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	FileCache() : cache_(MAX_CAPACITY) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Stores the specified file in the cache. If not enough space is available,
	// an older file is removed first.
	void PutFile(FileId file, shared<FileBuffer> buffer) {
		// Check if the file is already in the cache.
		if(cache_.ContainsKey(file)) return;

		// See if there is enough space in the cache.
		if(cache_.Count() == MAX_CAPACITY) {
			// Delete the oldest file.
			cache_.Remove(cachedFiles_.Dequeue());
		}

		cache_.Add(file, buffer);
		cachedFiles_.Enqueue(file);
	}

	// Gets the buffer associated with the specified file from the cache.
	shared<FileBuffer> GetFile(FileId file) {
		DebugValidator::IsTrue(cache_.ContainsKey(file));
		
		return cache_[file];
	}

	// Verifies if the specified file can be found in the cache.
	bool ContainsFile(FileId file) {
		return cache_.ContainsKey(file);
	}

	// Clears the cache.
	void Clear() {
		cache_.Clear();
		cachedFiles_.Clear();
	}

	// Returns the number of cached files.
	int CachedFiles() const {
		return cachedFiles_.Count();
	}
};


// Stores various information about a source file.
class FileDetails {
private:
	FileId id_;
	string path_;
	__int64 size_;
	bool isSystem_;
	bool isHeader_;

public:
	FileDetails() : size_(0), isSystem_(false), isHeader_(false) {}

	FileDetails(FileId id, const string& path, __int64 size, 
				bool system = false, bool header = false) :
			id_(id), path_(path), size_(size), 
			isSystem_(system), isHeader_(header) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the associated ID.
	FileId Id() {
		return id_;
	}

	// Returns the full path of the file.
	string& Path() {
		return path_;
	}

	// Returns the size of the file, in bytes.
	__int64 Size() {
		return size_;
	}

	// Specifies whether the file was loaded from one of the include directories.
	bool IsSystem() {
		return isSystem_;
	}

	// Specifies whether the file is a header (.h) file.
	bool IsHeader() {
		return isHeader_;
	}

	unsigned GetHashCode() const {
		return id_.GetHashCode();
	}
};


// This is the class that loads and manages the source files.
// The files are loaded in a buffer and immediately cached. Subsequent request
// check the cache first before loading from disk.
// The first time a file is loaded an ID is assigned to it.
class FileManager {
private:
	Dictionary<string, FileId> fileToId_;
	Dictionary<FileId, shared<FileDetails>> idToFile_;
	FileCache cache_;
	List<string>* includeDirs_;
	FileId::TId nextId_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Tries to load the contents of the specified file in a 'FileBuffer' object.
	// The loaded file is automatically added to the cache.
	bool LoadFileContents(const string& path, const FileId& id, 
						  shared<FileBuffer>& buffer);

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	FileManager();

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	void Initialize(List<string>* includeDirs);

	// Loads the file from the specified path. The path must be an absolute path.
	bool LoadFile(const string& path, FileId& id, shared<FileBuffer>& buffer, 
				  bool header = false, bool inSystem = false);

	// Loads the specified file from one of the include directories (the first
	// that contains the file). The path must be a relative one.
	bool LoadSystemFile(const string& path, FileId& id, shared<FileBuffer>& buffer,
						bool header = true);
	
	// Loads the file indicated by the specified ID.
	// Returns 'false' if the ID is invalid.
	bool LoadFile(FileId id, shared<FileBuffer>& buffer);

	// Gets the details of the file associated with the specified ID.
	bool GetDetails(FileId id, FileDetails*& details) {
		shared<FileDetails> temp;

		if(idToFile_.TryGetValue(id, &temp)) {
			details = temp;
			return true;
		}

		return false;
	}
};

} // namespace Common
#endif