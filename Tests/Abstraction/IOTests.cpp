//#include "../../Base/String.hpp"
//#include "../../Abstraction/Platform.hpp"
//#include <gtest\gtest.h>
//using namespace Base;
//using namespace Abstraction;
//
//TEST(IO, FindFiles) {
//	int ct = 0;
//
//	IO::FindFiles("C:\\", "*", [&ct](const string& path, bool folder) -> bool {
//		//std::wprintf(L"%s - %d\n", path.Chars(), (int)folder);
//		if(++ct == 3) return false;
//		return true;
//	});
//
//	EXPECT_EQ(3, ct);
//}
//
//TEST(IO, GetLogicalDrives) {
//	int ct = 0;
//
//	IO::GetLogicalDrives([&ct](const string& drive) -> bool {
//		//std::wprintf(L"%s\n", drive.Chars());
//		ct++;
//		return true;
//	});
//
//	EXPECT_LE(1, ct);
//}