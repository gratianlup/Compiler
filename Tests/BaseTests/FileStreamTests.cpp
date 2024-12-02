#include "../../Base/FileStream.hpp"
#include <gtest\gtest.h>
#include <fstream>
#include <cstdio>
using namespace std;
using namespace Base;

TEST(FileStream, Read) {
	char buff[100];
	ofstream test;

	try {
		// Create test file.
		test.open("test1.txt");
		test<<"one two";
		test.close();

		FileStream fs(_T("test1.txt"), FileMode_Open);
		EXPECT_FALSE(fs.IsClosed());
		EXPECT_TRUE(fs.CanRead());
		EXPECT_EQ(7, fs.Length());
		EXPECT_EQ('o', fs.ReadByte());
		EXPECT_EQ(1, fs.Position());

		EXPECT_TRUE(fs.Read(buff, 0, 6));
		buff[6] = 0;
		EXPECT_STREQ("ne two", buff);
		EXPECT_EQ(7, fs.Position());

		EXPECT_FALSE(fs.Read(buff, 0, 1));

		fs.Close();
		remove("test1.txt");
	}
	catch(...) {
		test.close();
		remove("test1.txt");
	}
}

TEST(FileStream, Write) {
	char buff[100];
	FileStream fs(_T("test1.txt"), FileMode_Create);
	EXPECT_FALSE(fs.IsClosed());
	EXPECT_TRUE(fs.CanWrite());

	fs.WriteByte('o');
	fs.WriteByte('n');
	fs.WriteByte('e');
	EXPECT_EQ(3, fs.Position());

	fs.Write(" two", 0, 4);
	EXPECT_EQ(7, fs.Position());
	fs.Close();
	EXPECT_TRUE(fs.IsClosed());

	ifstream test("test1.txt");
	test>>buff;
	EXPECT_STREQ("one", buff);

	test>>buff;
	EXPECT_STREQ("two", buff);

	test.close();
	remove("test1.txt");
}

TEST(FileStream, Seek) {
	char buff[100];

	ofstream test("test1.txt");
	test<<"one two";
	test.close();

	FileStream fs(_T("test1.txt"), FileMode_Open);
	EXPECT_FALSE(fs.IsClosed());
	EXPECT_TRUE(fs.CanRead());

	EXPECT_EQ(4, fs.Seek(4, SeekOrigin_Current));
	EXPECT_TRUE(fs.Read(buff, 0, 3));
	buff[3] = 0;
	EXPECT_STREQ("two", buff);

	EXPECT_EQ(0, fs.Seek(0, SeekOrigin_Begin));
	EXPECT_TRUE(fs.Read(buff, 0, 3));
	buff[3] = 0;
	EXPECT_STREQ("one", buff);

	fs.Close();
	remove("test1.txt");
}

TEST(FileStream, BufferingRead) {
	char buff[100];
	ofstream test("test1.txt");

	for(char i = 'a'; i <= 'z'; i++) {
		for(char j = 'a'; j <= 'z'; j++) {
			for(char k = 'a'; k <= 'z'; k++) {
				test<<i<<j<<k<<" ";
			}
		}
	}

	test.close();
	FileStream fs(_T("test1.txt"), FileMode_Open);
	EXPECT_FALSE(fs.IsClosed());
	EXPECT_TRUE(fs.CanRead());

	for(char i = 'a'; i <= 'z'; i++) {
		for(char j = 'a'; j <= 'z'; j++) {
			for(char k = 'a'; k <= 'z'; k++) {
				EXPECT_TRUE(fs.Read(buff, 0, 4));
				EXPECT_EQ(i, buff[0]);
				EXPECT_EQ(j, buff[1]);
				EXPECT_EQ(k, buff[2]);
				EXPECT_EQ(' ', buff[3]);
			}
		}
	}

	EXPECT_FALSE(fs.Read(buff, 0, 1));

	fs.Close();
	remove("test1.txt");
}

TEST(FileStream, BufferingWrite) {
	char buff[100];
	FileStream fs(_T("test1.txt"), FileMode_Create);
	EXPECT_FALSE(fs.IsClosed());
	EXPECT_TRUE(fs.CanWrite());

	for(char i = 'a'; i <= 'z'; i++) {
		for(char j = 'a'; j <= 'z'; j++) {
			for(char k = 'a'; k <= 'z'; k++) {
				buff[0] = i;
				buff[1] = j;
				buff[2] = k;
				buff[3] = ' ';
				EXPECT_TRUE(fs.Write(buff, 0, 4));
			}
		}
	}

	fs.Close();

	ifstream test("test1.txt");

	for(char i = 'a'; i <= 'z'; i++) {
		for(char j = 'a'; j <= 'z'; j++) {
			for(char k = 'a'; k <= 'z'; k++) {
				test>>buff;
				EXPECT_EQ(i, buff[0]);
				EXPECT_EQ(j, buff[1]);
				EXPECT_EQ(k, buff[2]);
				EXPECT_EQ('\0', buff[3]);
			}
		}
	}

	test.close();
	remove("test1.txt");
}