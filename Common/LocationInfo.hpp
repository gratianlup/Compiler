// LocationInfo.hpp
// Copyright (c) Lup Gratian
//
// Implements classes that store information about locations in source files.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_COMMON_LOCATION_INFO_HPP
#define PC_COMMON_LOCATION_INFO_HPP

#include "FileId.hpp"

namespace Common {

// Represents a location in a source file.
// Specifies the source file ID, the line number and the position on the line.
class LocationInfo {
private:
	FileId file_;
	int line_;
	int position_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	LocationInfo() : file_(FileId()), line_(0), position_(0) {}

	explicit LocationInfo(FileId file, int line = 0, int pos = 0) :
			file_(file), line_(line), position_(pos) {}

	LocationInfo(const LocationInfo& other) :
			file_(other.file_), line_(other.line_), position_(other.position_) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the ID of the file.
	FileId File() const {
		return file_;
	}

	void SetFile(FileId value) {
		file_ = value;
	}

	// Returns the number of the line in the file.
	int Line() const {
		return line_;
	}

	void SetLine(int value) {
		line_ = value;
	}

	// Returns the character index on the line.
	int Position() const {
		return position_;
	}

	void SetPosition(int value) {
		position_ = value;
	}

	bool IsInvalid() const {
		return file_.IsInvalid();
	}
	
	unsigned GetHashCode() const {
		return file_.GetHashCode() ^ line_ ^ position_;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator ==(const LocationInfo& other) const {
		return (file_ == other.file_) && 
			   (line_ == other.line_) &&
			   (position_ == other.position_);
	}

	bool operator !=(const LocationInfo& other) const {
		return !operator==(other);
	}
};


// Represents a sequence of characters in a source file.
// Specifies the first and last location of the range.
class RangeInfo {
private:
	LocationInfo first_;
	LocationInfo last_;

public:
	RangeInfo() {}

	explicit RangeInfo(LocationInfo first, LocationInfo last) :
			first_(first), last_(last) {}

	RangeInfo(const RangeInfo& other) : 
			first_(other.first_), last_(other.last_) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the left margin of the character range.
	LocationInfo First() const {
		return first_;
	}

	// Returns the right margin of the character range.
	LocationInfo Last() const {
		return last_;
	}

	unsigned GetHashCode() const {
		return first_.GetHashCode() ^ last_.GetHashCode();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator ==(const RangeInfo& other) const {
		return (first_ == other.first_) &&
			   (last_ == other.last_);
	}

	bool operator !=(const RangeInfo& other) const {
		return !operator==(other);
	}
};

} // namespace Common
#endif