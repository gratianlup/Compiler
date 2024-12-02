#include "../../Base/StreamReader.hpp"
#include "../../Base/MemoryStream.hpp"
#include "../../Base/StringBuilder.hpp"
#include "../../Base/ASCIIEncoding.hpp"
#include "../../Base/SharedPointer.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(StreamReader, Read) {
	shared<MemoryStream> test = new MemoryStream();
	char* temp = "test1234567890";
	test->Write(temp, 0, 15);
	test->SetPosition(0);

	StreamReader stream(test);
	for(int i = 0; i < 14; i++) {
		EXPECT_EQ(temp[i], stream.Read());
	}

	EXPECT_EQ('\0', stream.Read());
	EXPECT_EQ((string::TChar)-1, stream.Read());
}

TEST(StreamReader, ReadBlock) {
	shared<MemoryStream> test = new MemoryStream();

	for(int i = 0; i < 100; i++) {
		test->Write("test", 0, 5);
	}

	test->SetPosition(0);
	StreamReader stream(test);
	string::TChar buff[100];

	for(int i = 0; i < 100; i++) {
		stream.Read(buff, 0, 5);
		EXPECT_EQ(_T('t'), buff[0]);
		EXPECT_EQ(_T('e'), buff[1]);
		EXPECT_EQ(_T('s'), buff[2]);
		EXPECT_EQ(_T('t'), buff[3]);
		EXPECT_EQ(_T('\0'), buff[4]);
	}

	EXPECT_EQ(0, stream.Read(buff, 0, 5));
}

TEST(StreamReader, ReadLine) {
	StringBuilder t;
	t.AppendLine("One");
	t.AppendLine("Two");
	t.AppendLine("Three");
	string s = t.ToString();
	char buff[100];
	__int64 w;
	ASCIIEncoding().GetBytes(s.Chars(), s.Length(), buff, 100, w);

	shared<MemoryStream> test = new MemoryStream();
	test->Write(buff, 0, w);
	test->SetPosition(0);

	StreamReader stream(test);
	EXPECT_STREQ(_T("One"), stream.ReadLine().Chars());
	EXPECT_STREQ(_T("Two"), stream.ReadLine().Chars());
	EXPECT_STREQ(_T("Three"), stream.ReadLine().Chars());
}

TEST(StreamReader, ReadToEnd) {
	string s = "";
	for(int i = 0; i < 100; i++) {
		s += "abc";
	}

	char buff[1000];
	__int64 w;
	ASCIIEncoding().GetBytes(s.Chars(), s.Length(), buff, 1000, w);

	shared<MemoryStream> test = new MemoryStream();
	test->Write(buff, 0, w);
	test->SetPosition(0);

	StreamReader stream(test);
	EXPECT_STREQ(s.Chars(), stream.ReadToEnd().Chars());
	EXPECT_STREQ(_T(""), stream.ReadToEnd().Chars());
}

TEST(StreamReader, Peek) {
	shared<MemoryStream> test = new MemoryStream();
	char* temp = "test";
	test->Write(temp, 0, 5);
	test->SetPosition(0);

	StreamReader stream(test);
	EXPECT_EQ(_T('t'), stream.Peek());
	EXPECT_EQ(_T('t'), stream.Peek());
	EXPECT_EQ(_T('t'), stream.Read());
	EXPECT_EQ(_T('e'), stream.Read());
}

TEST(StreamReader, EndOfStream) {
	shared<MemoryStream> test = new MemoryStream();
	char* temp = "test123";
	test->Write(temp, 0, 8);
	test->SetPosition(0);

	StreamReader stream(test);
	EXPECT_FALSE(stream.EndOfStream());
	
	for(int i = 0; i < 8; i++) {
		stream.Read();

		if(i == 7) {
			EXPECT_TRUE(stream.EndOfStream());
		}
		else {
			EXPECT_FALSE(stream.EndOfStream());
		}
	}
}