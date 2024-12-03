// HeaderLookup.pp
// Copyright (c) Lup Gratian
//
// Implementation of HeaderLookup.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "HeaderLookup.hpp"
#include "Lexer.hpp"
#include "Preprocessor.hpp"

namespace Lexing {

bool HeaderLookup::PushHeader(const string& path, bool isSystem) {
	// Check for infinite include recursion.
	if(StackFull()) {
		return false;
	}

	// If this is a user file, search in the directory of the current file.
	if(isSystem == false) {
		FileDetails* parent;
		manager_->GetDetails(includeStack_.Peek(), parent);
			
		// Always use a full path.
		string actual = Path::Combine(Path::GetDirectoryName(parent->Path()), path);
		shared<FileBuffer> buffer;
		FileId headerId;

		if(manager_->LoadFile(Path::GetFullPath(actual), 
								headerId, buffer, true)) {
			// The file was found. Set the contents as a source for the Lexer
			// and push the FileId to the include stack.
			includeStack_.Push(headerId);
			lexer_->PushSource(new FileCharSource(buffer));
			lexer_->SetName(Path::GetFileName(actual));
			lexer_->SetFile(headerId);
			lexer_->SetLine(0);

			return true;
		}

		// File not found, continue searching in the system files (C99:6.10.1.3).
	}

	// Search in the system files.
	shared<FileBuffer> buffer;
	FileId headerId;

	if(manager_->LoadSystemFile(path, headerId, buffer, true)) {
		// The file was found. Set the contents as a source for the Lexer
		// and push the FileId to the include stack.
		includeStack_.Push(headerId);
		lexer_->PushSource(new FileCharSource(buffer));
		lexer_->SetName(Path::GetFileName(path));
		lexer_->SetFile(headerId);
		lexer_->SetLine(0);

		return true;
	}
	
	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool HeaderLookup::LoadStart(const string& file) {
	// The path to the file should be already an absolute path.
	shared<FileBuffer> buffer;
	FileId id;

	if(manager_->LoadFile(file, id, buffer, true)) {
		// The file was found. Set the contents as a source for the Lexer
		// and push the FileId to the include stack.
		includeStack_.Push(id);
		lexer_->PushSource(new FileCharSource(buffer));
		lexer_->SetFile(id);
		lexer_->SetName(Path::GetFileName(file));
		lexer_->SetLine(0);

		return true;
	}
	
	return false;
}

} // namespace Lexing