// FileId.hpp
// Copyright (c) Lup Gratian
//
// Represents the ID associated with a source code file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_FILE_ID_HPP
#define PC_COMMON_FILE_ID_HPP

namespace Common {

class FileId {
private:
	int id_;

public:
	typedef int TId; // The type visible to others.
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	FileId() : id_(-1) {}

	explicit FileId(TId id) : id_(id) {}

	FileId(const FileId& other) : id_(other.id_) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static TId StartId() {
		return 0;
	}

	static TId NextId(TId oldId) {
		return oldId + 1;
	}

	TId Id() const {
		return id_;
	}

	bool IsInvalid() const {
		return id_ == -1;
	}

	unsigned GetHashCode() const {
		return (unsigned)id_;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator ==(const FileId& other) const {
		return id_ == other.id_;
	}

	bool operator !=(const FileId& other) const {
		return id_ != other.id_;
	}

	bool operator <(const FileId& other) const {
		return id_ < other.id_;
	}
};

} // namespace Common
#endif