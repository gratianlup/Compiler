#include "../../Base/DateTime.hpp"
#include "../../Base/String.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(DateTime, ConstructorsAccessors) {
	DateTime test1(123456);
	EXPECT_EQ(123456, test1.Ticks());

	DateTime test2(2010, 7, 11, 19, 10, 26, 233);
	EXPECT_EQ(2010, test2.Year());
	EXPECT_EQ(7, test2.Month());
	EXPECT_EQ(11, test2.Day());
	EXPECT_EQ(19, test2.Hour());
	EXPECT_EQ(10, test2.Minute());
	EXPECT_EQ(26, test2.Second());
	EXPECT_EQ(233, test2.Millisecond());

	DateTime test3(test1);
	EXPECT_EQ(123456, test3.Ticks());
}

TEST(DateTime, Operations) {
	DateTime date(2000, 10, 1);

	date = date.AddDays(5);
	EXPECT_EQ(2000, date.Year());
	EXPECT_EQ(10, date.Month());
	EXPECT_EQ(6, date.Day());

	date = date.AddHours(7);
	EXPECT_EQ(2000, date.Year());
	EXPECT_EQ(10, date.Month());
	EXPECT_EQ(6, date.Day());
	EXPECT_EQ(7, date.Hour());

	date = date.AddMinutes(8);
	EXPECT_EQ(2000, date.Year());
	EXPECT_EQ(10, date.Month());
	EXPECT_EQ(6, date.Day());
	EXPECT_EQ(7, date.Hour());
	EXPECT_EQ(8, date.Minute());

	date = date.AddSeconds(9);
	EXPECT_EQ(2000, date.Year());
	EXPECT_EQ(10, date.Month());
	EXPECT_EQ(6, date.Day());
	EXPECT_EQ(7, date.Hour());
	EXPECT_EQ(8, date.Minute());
	EXPECT_EQ(9, date.Second());

	date = date.AddMilliseconds(100);
	EXPECT_EQ(2000, date.Year());
	EXPECT_EQ(10, date.Month());
	EXPECT_EQ(6, date.Day());
	EXPECT_EQ(7, date.Hour());
	EXPECT_EQ(8, date.Minute());
	EXPECT_EQ(9, date.Second());
	EXPECT_EQ(100, date.Millisecond());

	date = date.AddMonths(2);
	EXPECT_EQ(2000, date.Year());
	EXPECT_EQ(12, date.Month());
	EXPECT_EQ(6, date.Day());
	EXPECT_EQ(7, date.Hour());
	EXPECT_EQ(8, date.Minute());
	EXPECT_EQ(9, date.Second());
	EXPECT_EQ(100, date.Millisecond());

	date = date.AddMonths(1);
	EXPECT_EQ(2001, date.Year());
	EXPECT_EQ(1, date.Month());
	EXPECT_EQ(6, date.Day());
	EXPECT_EQ(7, date.Hour());
	EXPECT_EQ(8, date.Minute());
	EXPECT_EQ(9, date.Second());
	EXPECT_EQ(100, date.Millisecond());
	
	date = date.AddYears(9);
	EXPECT_EQ(2010, date.Year());
	EXPECT_EQ(1, date.Month());
	EXPECT_EQ(6, date.Day());
	EXPECT_EQ(7, date.Hour());
	EXPECT_EQ(8, date.Minute());
	EXPECT_EQ(9, date.Second());
	EXPECT_EQ(100, date.Millisecond());
}

TEST(DateTime, Conversions) {
	DateTime date(2000, 10, 1);
	__int64 value = date.ToFileTime();
	DateTime date2 = DateTime::FromFileTime(value);
	EXPECT_EQ(date.Ticks(), date2.Ticks());
}

TEST(DateTime, ToString) {
	DateTime date(2000, 10, 1, 14, 35, 28);

	string test1 = date.ToLongDateString();
	EXPECT_STREQ(L"Sunday,October 01,2000", test1.Chars());

	string test2 = date.ToShortDateString();
	EXPECT_STREQ(L"10/1/2000", test2.Chars());

	string test3 = date.ToLongTimeString();
	EXPECT_STREQ(L"02:35:28 PM", test3.Chars());

	string test4 = date.ToShortTimeString();
	EXPECT_STREQ(L"14:35:28", test4.Chars());
}