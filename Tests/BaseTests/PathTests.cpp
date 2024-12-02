#include "../../Base/Path.hpp"
#include "../../Base/String.hpp"
#include "../../Abstraction/Platform.hpp"
#include <gtest\gtest.h>
using namespace Base;
using namespace Abstraction;

TEST(Path, Combine) {
	EXPECT_STREQ(_T("c:\\temp\\subdir\\file.txt"), 
				 Path::Combine("c:\\temp", "subdir\\file.txt").Chars());
	EXPECT_STREQ(_T("c:\\temp.txt"), 
				 Path::Combine("c:\\temp", "c:\\temp.txt").Chars());
	EXPECT_STREQ(_T("c:\\temp.txt\\subdir\\file.txt"), 
				 Path::Combine("c:\\temp.txt", "subdir\\file.txt").Chars());
	EXPECT_STREQ(_T("subdir\\file.txt"), 
				 Path::Combine("", "subdir\\file.txt").Chars());
}

TEST(Path, ChangeExtension) {
	EXPECT_STREQ(_T("c:\\temp.avi"), 
				 Path::ChangeExtension("c:\\temp.exe", ".avi").Chars());
	EXPECT_STREQ(_T("c:\\temp."), 
				 Path::ChangeExtension("c:\\temp.exe", "").Chars());
	EXPECT_STREQ(_T("c:\\temp.avi"), 
				 Path::ChangeExtension("c:\\temp", ".avi").Chars());
}

TEST(Path, GetDirectoryName) {
	EXPECT_STREQ(_T("c:\\mydir"), 
				 Path::GetDirectoryName("c:\\mydir\\file.txt").Chars());
	EXPECT_STREQ(_T("c:\\mydir"), 
				 Path::GetDirectoryName("c:\\mydir\\").Chars());
	EXPECT_STREQ(_T("c:\\mydir"), 
				 Path::GetDirectoryName("c:\\mydir\\file").Chars());
	EXPECT_STREQ(_T(""), 
				 Path::GetDirectoryName("c:\\").Chars());
}

TEST(Path, GetExtension) {
	EXPECT_STREQ(_T(".txt"), 
				 Path::GetExtension("c:\\mydir\\file.txt").Chars());
	EXPECT_STREQ(_T(".avi"), 
				 Path::GetExtension("c:\\mydir\\file.txt.avi").Chars());
	EXPECT_STREQ(_T(""), 
				 Path::GetExtension("c:\\mydir\\file").Chars());
	EXPECT_STREQ(_T(""), 
				 Path::GetExtension("c:\\mydir\\").Chars());
}

TEST(Path, GetFileName) {
	EXPECT_STREQ(_T("file.txt"), 
				 Path::GetFileName("c:\\mydir\\file.txt").Chars());
	EXPECT_STREQ(_T("file"), 
				 Path::GetFileName("c:\\mydir\\file").Chars());
	EXPECT_STREQ(_T(""), 
				 Path::GetFileName("c:\\mydir\\").Chars());
	EXPECT_STREQ(_T(""), 
				 Path::GetFileName("c:\\").Chars());
}

TEST(Path, GetFullPath) {
	EXPECT_STREQ(_T("c:\\mydir\\file.txt"), 
				 Path::GetFullPath("c:\\mydir\\file.txt").Chars());
	EXPECT_STREQ(Path::Combine(IO::CurrentDirectory(), "file.txt").Chars(), 
				 Path::GetFullPath("file.txt").Chars());
	EXPECT_STREQ(Path::Combine(IO::CurrentDirectory(), "dir").Chars(), 
				 Path::GetFullPath("dir").Chars());
}

TEST(Path, GetPathRoot) {
	EXPECT_STREQ(_T("c:\\"), 
				 Path::GetPathRoot("c:\\mydir\\file.txt").Chars());
	EXPECT_STREQ(_T("c:\\"), 
				 Path::GetPathRoot("c:\\mydir\\file").Chars());
	EXPECT_STREQ(_T(""), 
				 Path::GetPathRoot("file.txt").Chars());
	EXPECT_STREQ(_T("\\"), 
				 Path::GetPathRoot("\\mydir\\").Chars());
}

TEST(Path, GetRandomFileName) {
	string path = Path::GetRandomFileName();
	EXPECT_LE(8, path.Length());
	EXPECT_LT(0, path.LastIndexOf(_T('.')));

	int ct[27] = {0};
	for(int i = 0; i < path.Length(); i++) {
		ct[path[i] - 'a']++;
	}

	for(int i = 0; i < path.Length(); i++) {
		EXPECT_LT(ct[i], path.Length() / 2);
	}
}

TEST(Path, GetTempFile) {
	string path = Path::GetTempFile();
	EXPECT_LE(3, path.Length());
	EXPECT_LT(0, path.LastIndexOf(_T('.')));
	EXPECT_TRUE(IO::FileExists(path));
	IO::DeleteFile(path);
}

TEST(Path, HasExtension) {
	EXPECT_TRUE(Path::HasExtension("c:\\mydir\\file.txt"));
	EXPECT_FALSE(Path::HasExtension("c:\\mydir\\file"));
	EXPECT_TRUE(Path::HasExtension("file.txt"));
	EXPECT_FALSE(Path::HasExtension("c:\\mydir\\"));
	EXPECT_FALSE(Path::HasExtension("c:\\"));
}

TEST(Path, IsPathRooted) {
	EXPECT_TRUE(Path::IsPathRooted("c:\\mydir\\file.txt"));
	EXPECT_TRUE(Path::IsPathRooted("\\\\mypc\\mydir\\file"));
	EXPECT_FALSE(Path::IsPathRooted("mydir\\file.txt"));
}