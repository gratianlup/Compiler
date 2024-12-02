#include "../../Base/MemoryStream.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(MemoryStream, Read) {
	sharedVect<char> buff = new char[100];
	for(int i = 0; i < 26; i++) {
		buff[i] = 'a' + i;
	}

	MemoryStream stream(buff, 26);
	EXPECT_EQ(26, stream.Length());
	EXPECT_EQ(26, stream.Capacity());
	EXPECT_TRUE(stream.CanRead());
	EXPECT_TRUE(stream.CanSeek());
	EXPECT_FALSE(stream.CanWrite());
	EXPECT_EQ(0, stream.Position());

	for(int i = 0; i < 24; i++) {
		EXPECT_EQ('a' + i, stream.ReadByte());
	}

	EXPECT_EQ(2, stream.Read(buff, 0, 2));
	EXPECT_EQ('a' + 24, buff[0]);
	EXPECT_EQ('a' + 25, buff[1]);

	EXPECT_EQ(0, stream.Read(buff, 0, 1));
}

TEST(MemoryStream, ReadLarge) {
	sharedVect<char> buff = new char[1000];
	char buff2[1000] = {0};
	for(int i = 0; i < 50; i++) {
		for(int j = 0; j < 20; j++) {
			buff[i*20 + j] = 'a' + j;
		}
	}

	MemoryStream stream(buff, 1000);

	for(int i = 0; i < 50; i++) {
		EXPECT_EQ(20, stream.Read(buff2, 0, 20));

		for(int j = 0; j < 20; j++) {
			EXPECT_EQ('a' + j, buff2[j]);
		}
	}

	EXPECT_EQ(0, stream.Read(buff, 0, 1));
}

TEST(MemoryStream, Write) {
	sharedVect<char> buff = new char[1000];

	MemoryStream stream;

	char buff2[1000] = {0};
	for(int i = 0; i < 50; i++) {
		for(int j = 0; j < 20; j++) {
			buff[i*20 + j] = 'a' + j;
		}

		EXPECT_EQ(20, stream.Write(buff + i*20, 0, 20));
	}

	EXPECT_EQ(1000, stream.Position());
	EXPECT_EQ(1000, stream.Length());
	EXPECT_LT(1000, stream.Capacity());

	EXPECT_TRUE(stream.SetPosition(0));

	for(int i = 0; i < 50; i++) {
		EXPECT_EQ(20, stream.Read(buff2, 0, 20));

		for(int j = 0; j < 20; j++) {
			EXPECT_EQ('a' + j, buff2[j]);
		}
	}

	EXPECT_EQ(0, stream.Read(buff, 0, 1));
}

TEST(MemoryStream, Seek) {
	sharedVect<char> buff = new char[100];
	memcpy(buff, "one two", 7);
	MemoryStream stream(buff, 7);

	EXPECT_EQ(4, stream.Seek(4, SeekOrigin_Current));
	EXPECT_EQ(4, stream.Position());
	EXPECT_EQ('t', stream.ReadByte());
	EXPECT_EQ('w', stream.ReadByte());
	EXPECT_EQ('o', stream.ReadByte());

	EXPECT_EQ(1, stream.Seek(1, SeekOrigin_Begin));
	EXPECT_EQ(1, stream.Position());
	EXPECT_EQ('n', stream.ReadByte());

	EXPECT_EQ(5, stream.Seek(-2, SeekOrigin_End));
	EXPECT_EQ(5, stream.Position());
	EXPECT_EQ('w', stream.ReadByte());
}

TEST(MemoryStream, SetCapacity) {
	MemoryStream stream;

	stream.Write("one", 0, 3);
	stream.SetCapacity(8000);
	EXPECT_EQ(8000, stream.Capacity());
	EXPECT_EQ(3, stream.Position());
	EXPECT_EQ(3, stream.Length());

	EXPECT_TRUE(stream.SetPosition(0));
	EXPECT_EQ('o', stream.ReadByte());
	EXPECT_EQ('n', stream.ReadByte());
	EXPECT_EQ('e', stream.ReadByte());
}