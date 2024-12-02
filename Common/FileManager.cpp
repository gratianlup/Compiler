// FileManager.cpp
// Copyright (c) Lup Gratian
//
// Implementation of FileManager.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "FileManager.hpp"

namespace Common {

bool FileManager::LoadFileContents(const string& path, const FileId& id, 
								   shared<FileBuffer>& buffer) {
	// Open the file.
	local<FileStream> stream = new FileStream(path, FileMode::Open);

	if(stream->IsClosed()) {
		// File not found or could not be opened.
		Log::Warning("File not found: " + path);
		return false;
	}

	// Read the contents of the file and convert to the default encoding.
	string::TChar temp[4096];
	StreamReader reader(stream);
	__int64 read = reader.Read(temp, 0, 4096);

	buffer = new FileBuffer(id, read);
	buffer->WriteBuffer(temp, read);

	if(read == 4096) {
		// There may be more data.
		do {
			read = reader.Read(temp, 0, 4096);
			buffer->WriteBuffer(temp, read);
		} while(read > 0);
	}

	// Write a 0 at the end so that empty files are handled correctly.
	buffer->WriteBuffer(_T("\0"), 1);

	// Place the file into the cache.
	cache_.PutFile(id, buffer);
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FileManager::FileManager() : 
		nextId_(FileId::StartId()), includeDirs_(nullptr) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void FileManager::Initialize(List<string>* includeDirs) {
	includeDirs_ = includeDirs;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FileManager::LoadFile(const string& path, FileId& id,
						   shared<FileBuffer>& buffer, bool header, bool inSystem) {
	// Check if the file is already in the cache.
	if(fileToId_.TryGetValue(path, &id)) {
		if(cache_.ContainsFile(id)) {
			// The file is in the cache.
			buffer = cache_.GetFile(id);
			return true;
		}
		else {
			// Reload the file from disk.
			return LoadFileContents(path, id, buffer);
		}
	}
	else {
		if(LoadFileContents(path, id, buffer)) {
			// It's the first time the file is loaded; generate a new ID.
			id = FileId(nextId_);
			nextId_ = FileId::NextId(nextId_);
			fileToId_.Add(path, id);

			local<FileDetails> info = new FileDetails(id, path, 0, inSystem, header);
			idToFile_.Add(id, info);
			return true;
		}

		return false;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FileManager::LoadSystemFile(const string& path, FileId& id, 
								 shared<FileBuffer>& buffer, bool header) {
	if(includeDirs_ == nullptr) return false;

	// The file could be relative to a system directory (ex. /lib1/file.h).
	string relPath = Path::GetDirectoryName(path);
	string fileName = Path::GetFileName(path);
	bool relative = relPath.Length() > 0;

	// Check each directory until the file is found.
	bool found = false;

	includeDirs_->ForEach([&] (const string& dir) mutable -> bool {
		if(relative) {
			// dir1 + /lib1/file.h -> dir1/lib1/file.h
			string absPath = Path::Combine(dir, relPath);
			string filePath = Path::Combine(absPath, fileName);

			if(File::Exists(filePath)) {
				// Found a file.
				found = LoadFile(Path::Combine(dir, path), id, 
								 buffer, header, true /* inSystem */);
				return false; // Stop the loop.
			}
		}
		else {
			string filePath = Path::Combine(dir, path);

			if(File::Exists(filePath)) {
				// The file was found.
				found = LoadFile(filePath, id, buffer, header, true /* inSystem */);
				return false; // Stop the loop.
			}
		}

		return true;
	});

	return found;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool FileManager::LoadFile(FileId id, shared<FileBuffer>& buffer) {
	FileDetails* info;

	if(GetDetails(id, info)) {
		return LoadFile(info->Path(), id, buffer);
	}

	return false;
}

} // namespace Common