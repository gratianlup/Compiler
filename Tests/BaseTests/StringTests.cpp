#include "../../Base/String.hpp"
#include "../../Base/List.hpp"
#include "../../Base/DebugValidator.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(String, Constructors) {
	// Default constructor.
	string test1;
	EXPECT_STREQ(test1.Chars(), L"");
	EXPECT_EQ(test1.Length(), 0);

	// Copy from array.
	wchar_t *temp = L"abc123";
	string test2(temp);
	EXPECT_STREQ(test2.Chars(), temp);
	EXPECT_EQ(test2.Length(), 6);

	// Copy from array, only first N characters.
	string test3(temp, 3);
	EXPECT_STREQ(test3.Chars(), L"abc");
	EXPECT_EQ(test3.Length(), 3);

	// Copy from array, specified region.
	string test4(temp, 3, 3);
	EXPECT_STREQ(test4.Chars(), L"123");
	EXPECT_EQ(test4.Length(), 3);

	// Copy from another string (copy constructor).
	string test5 = string(temp);
	EXPECT_STREQ(test5.Chars(), temp);
	EXPECT_EQ(test5.Length(), 6);

	// Fill with character.
	string test6(L'a', 4);
	EXPECT_STREQ(test6.Chars(), L"aaaa");
	EXPECT_EQ(test6.Length(), 4);
}

TEST(String, Compare) {
	string test1 = L"aBcDeF";
	string test2 = L"abcdef";
	string test3 = L"aBcDeF";
	string test4 = L"abc123";

	// Compare regions from two string, case-insensitive.
	EXPECT_EQ(string::Compare(test1, 0, test2, 0, 6, true), 0);
	EXPECT_NE(string::Compare(test3, 0, test4, 0, 6, true), 0);
	EXPECT_EQ(string::Compare(test3, 0, test4, 0, 3, true), 0);
	EXPECT_EQ(string::Compare(test1, 0, test4, 0, 3, true), 0);

	// Compare regions from two string, case-sensitive.
	EXPECT_NE(string::Compare(test1, 0, test2, 0, 6, false), 0);
	EXPECT_NE(string::Compare(test3, 0, test4, 0, 6, false), 0);
	EXPECT_NE(string::Compare(test3, 0, test4, 0, 3, false), 0);
	EXPECT_NE(string::Compare(test1, 0, test4, 0, 3, false), 0);
	EXPECT_EQ(string::Compare(test1, 0, test3, 0, 6, false), 0);

	// Compare two strings, case-insensitive.
	EXPECT_EQ(string::Compare(test1, test2, true), 0);
	EXPECT_EQ(string::Compare(test1, test3, true), 0);

	// Compare two strings, case-sensitive.
	EXPECT_NE(string::Compare(test1, test2, false), 0);
	EXPECT_EQ(string::Compare(test1, test3, false), 0);
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, CompareOrdinal) {
	string test1 = L"aBcDeF";
	string test2 = L"abcdef";
	string test3 = L"aBcDeF";
	string test4 = L"abc123";

	// Compare regions from two string.
	EXPECT_NE(string::CompareOrdinal(test1, 0, test2, 0, 6), 0);
	EXPECT_NE(string::CompareOrdinal(test3, 0, test4, 0, 6), 0);
	EXPECT_NE(string::CompareOrdinal(test3, 0, test4, 0, 3), 0);
	EXPECT_EQ(string::CompareOrdinal(test1, 0, test3, 0, 3), 0);

	// Compare two strings.
	EXPECT_NE(string::CompareOrdinal(test1, test2), 0);
	EXPECT_EQ(string::CompareOrdinal(test1, test3), 0);
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Equals) {
	string test1 = L"abc123";
	string test2 = L"abcdef";
	string test3 = L"abc123";

	EXPECT_FALSE(string::Equals(test1, test2));
	EXPECT_TRUE(string::Equals(test1, test3));
	EXPECT_FALSE(test1.Equals(test2));
	EXPECT_TRUE(test1.Equals(test3));
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, StartsWith) {
	string test1 = L"abc123";
	string test2 = L"123456";
	string test3 = L"ABC123";
	string test4 = L"abc";

	// Case-insensitive.
	EXPECT_TRUE(test1.StartsWith(test4, true));
	EXPECT_FALSE(test2.StartsWith(test4, true));
	EXPECT_TRUE(test3.StartsWith(test4, true));

	// Case-sensitive.
	EXPECT_TRUE(test1.StartsWith(test4, false));
	EXPECT_FALSE(test2.StartsWith(test4, false));
	EXPECT_FALSE(test3.StartsWith(test4, false));

	// Empty string.
	EXPECT_TRUE(test1.StartsWith(""));
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, EndsWith) {
	string test1 = L"123abc";
	string test2 = L"123456";
	string test3 = L"123ABC";
	string test4 = L"abc";

	// Case-insensitive.
	EXPECT_TRUE(test1.EndsWith(test4, true));
	EXPECT_FALSE(test2.EndsWith(test4, true));
	EXPECT_TRUE(test3.EndsWith(test4, true));

	// Case-sensitive.
	EXPECT_TRUE(test1.EndsWith(test4, false));
	EXPECT_FALSE(test2.EndsWith(test4, false));
	EXPECT_FALSE(test3.EndsWith(test4, false));

	// Empty string.
	EXPECT_TRUE(test1.EndsWith(""));
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Concat) {
	string test1 = L"abc";
	string test2 = L"123";
	string stringArray[] = { test1, test2 };

	// Concatenate two strings.
	EXPECT_STREQ(string::Concat(test1, test2).Chars(), L"abc123");

	// Concatenate string with empty string.
	EXPECT_STREQ(string::Concat(test1, "").Chars(), L"abc");
	EXPECT_STREQ(string::Concat(test1, "").Chars(), L"abc");

	// Concatenate empty strings.
	EXPECT_STREQ(string::Concat("", "").Chars(), L"");

	// Concatenate more strings.
	EXPECT_STREQ(string::Concat(test1, test2, test1).Chars(), L"abc123abc");
	EXPECT_STREQ(string::Concat(test1, test2, test1, test2).Chars(), L"abc123abc123");
	EXPECT_STREQ(string::Concat(test1, test2, "").Chars(), L"abc123");
	EXPECT_STREQ(string::Concat(test1, test2, "", "").Chars(), L"abc123");
	EXPECT_STREQ(string::Concat("", "", "", "").Chars(), L"");
	EXPECT_STREQ(string::Concat("", test1, "", "").Chars(), L"abc");
	EXPECT_STREQ(string::Concat("", test1, "", test2).Chars(), L"abc123");

	// Concatenate an array of strings.
	EXPECT_STREQ(string::Concat(stringArray, 2).Chars(), L"abc123");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Contains) {
	string test1 = L"abc123";

	EXPECT_TRUE(test1.Contains(L"abc"));
	EXPECT_TRUE(test1.Contains(L"123"));
	EXPECT_FALSE(test1.Contains(L"abx"));
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, IndexOf) {
	string test1 = L"abc123";

	// Index of character in a specified region.
	EXPECT_EQ(test1.IndexOf(L'a', 0, 6), 0);
	EXPECT_EQ(test1.IndexOf(L'3', 0, 6), 5);
	EXPECT_EQ(test1.IndexOf(L'x', 0, 6), string::INVALID_INDEX);
	EXPECT_EQ(test1.IndexOf(L'a', 1, 3), string::INVALID_INDEX);
	EXPECT_EQ(test1.IndexOf(L'2', 0, 3), string::INVALID_INDEX);

	// Index of a character starting with the specified position.
	EXPECT_EQ(test1.IndexOf(L'a', 0), 0);
	EXPECT_EQ(test1.IndexOf(L'a', 1), string::INVALID_INDEX);

	// Index of a character (whole string).
	EXPECT_EQ(test1.IndexOf(L'b'), 1);
	EXPECT_EQ(test1.IndexOf(L'x'), string::INVALID_INDEX);

	// Index of a string in a specified region.
	EXPECT_EQ(test1.IndexOf(L"abc", 0, 6), 0);
	EXPECT_EQ(test1.IndexOf(L"123", 0, 6), 3);
	EXPECT_EQ(test1.IndexOf(L"xyz", 0, 6), string::INVALID_INDEX);
	EXPECT_EQ(test1.IndexOf(L"abc", 1, 5), string::INVALID_INDEX);
	EXPECT_EQ(test1.IndexOf(L"abc", 0, 1), string::INVALID_INDEX);
	EXPECT_EQ(test1.IndexOf("", 0, 6), 0);

	// Index of a string starting with the specified position.
	EXPECT_EQ(test1.IndexOf(L"abc", 0), 0);
	EXPECT_EQ(test1.IndexOf(L"abc", 1), string::INVALID_INDEX);

	// Index of a string (whole string).
	EXPECT_EQ(test1.IndexOf(L"bc"), 1);
	EXPECT_EQ(test1.IndexOf(L"xz"), string::INVALID_INDEX);
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, IndexOfAny) {
	string test1 = L"abc123";
	string test2 = L"567xyz";
	wchar_t chars[] = { L'a', L'2', L'5' };

	// Index of any character in a specified region.
	EXPECT_EQ(test1.IndexOfAny(chars, 3, 0, 6), 0); // a
	EXPECT_EQ(test1.IndexOfAny(chars, 3, 3, 2), 4); // 2
	EXPECT_EQ(test1.IndexOfAny(chars, 3, 5, 1), string::INVALID_INDEX);

	// Index of any character starting with the specified positon.
	EXPECT_EQ(test2.IndexOfAny(chars, 3, 0), 0); // 5
	EXPECT_EQ(test2.IndexOfAny(chars, 3, 1), string::INVALID_INDEX);

	// Index of any character (whole string).
	EXPECT_EQ(test1.IndexOfAny(chars, 3), 0); // a
	EXPECT_EQ(test2.IndexOfAny(chars, 1), string::INVALID_INDEX);
	EXPECT_EQ(test1.IndexOfAny(chars, 0), string::INVALID_INDEX);
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, LastIndexOf) {
	string test1 = L"ab1ca123";
	string test2 = L"abc123abc123";

	// Index of character in a specified region.
	EXPECT_EQ(4, test1.LastIndexOf(L'a', 7, 8));
	EXPECT_EQ(7, test1.LastIndexOf(L'3', 7, 8));
	EXPECT_EQ(string::INVALID_INDEX, test1.LastIndexOf(L'x', 7, 6));
	EXPECT_EQ(0, test1.LastIndexOf(L'a', 3, 4));
	EXPECT_EQ(string::INVALID_INDEX, test1.LastIndexOf(L'2', 7, 1));

	// Index of a character before a specified position.
	EXPECT_EQ(9, test2.LastIndexOf(L'1', 11));
	EXPECT_EQ(1, test2.LastIndexOf(L'b', 3));
	EXPECT_EQ(0, test2.LastIndexOf(L'a', 3));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L'x', 11));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L'1', 1));

	// Index of a character (whole string).
	EXPECT_EQ(9, test2.LastIndexOf(L'1'));
	EXPECT_EQ(7, test2.LastIndexOf(L'b'));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L'y'));

	// Index of string in a specified region.
	EXPECT_EQ(9, test2.LastIndexOf(L"123", 11, 12));
	EXPECT_EQ(6, test2.LastIndexOf(L"abc", 11, 12));
	EXPECT_EQ(0, test2.LastIndexOf(L"abc", 5, 6));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L"123", 4, 5));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L"abx", 11, 12));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L"xyz567", 11, 12));
	EXPECT_EQ(11, test2.LastIndexOf("", 11, 12));

	// Index of string before a specified position.
	EXPECT_EQ(9, test2.LastIndexOf(L"123", 11));
	EXPECT_EQ(0, test2.LastIndexOf(L"abc", 4));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L"123", 3));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L"xyz", 11));

	// Index of a string (whole string).
	EXPECT_EQ(6, test2.LastIndexOf(L"abc"));
	EXPECT_EQ(9, test2.LastIndexOf(L"123"));
	EXPECT_EQ(string::INVALID_INDEX, test2.LastIndexOf(L"xyz"));
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Copy) {
	string test1 = L"abc";
	string test2 = string::Copy(test1);

	EXPECT_EQ(test1.Length(), test2.Length());
	EXPECT_STREQ(test1.Chars(), test2.Chars());
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, CopyTo) {
	string source = L"123456abc";
	wchar_t destination[100];

	source.CopyTo(0, destination, 0, source.Length());
	for(size_t i = 0; i < source.Length(); i++) {
		EXPECT_EQ(destination[i], source[i]);
	}

	source.CopyTo(6, destination, 0, 3);
	for(size_t i = 0; i < 3; i++) {
		EXPECT_EQ(destination[i], source[6 + i]);
	}
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, ToLower) {
	string test1 = L"ABCD";
	string test2 = L"xyz";
	string test3 = L"aBcD123xYz";

	EXPECT_STREQ(test1.ToLower().Chars(), L"abcd");
	EXPECT_STREQ(test2.ToLower().Chars(), L"xyz");
	EXPECT_STREQ(test3.ToLower().Chars(), L"abcd123xyz");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, ToUpper) {
	string test1 = L"ABCD";
	string test2 = L"xyz";
	string test3 = L"aBcD123xYz";

	EXPECT_STREQ(test1.ToUpper().Chars(), L"ABCD");
	EXPECT_STREQ(test2.ToUpper().Chars(), L"XYZ");
	EXPECT_STREQ(test3.ToUpper().Chars(), L"ABCD123XYZ");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, TrimStart) {
	string test1 = L"aaabcd";
	string test2 = L"   123";
	wchar_t chars[] = { L'a', L'd' };

	EXPECT_STREQ(test1.TrimStart(chars, 2).Chars(), L"bcd");
	EXPECT_STREQ(test1.TrimStart(chars + 1, 1).Chars(), L"aaabcd");
	EXPECT_STREQ(test1.TrimStart(nullptr, 0).Chars(), L"aaabcd");

	EXPECT_STREQ(test2.TrimStart(chars, 2).Chars(), L"   123");
	EXPECT_STREQ(test2.TrimStart(nullptr, 0).Chars(), L"123");

	EXPECT_STREQ(test1.TrimStart().Chars(), L"aaabcd");
	EXPECT_STREQ(test2.TrimStart().Chars(), L"123");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, TrimEnd) {
	string test1 = L"bcdddd";
	string test2 = L"123  ";
	wchar_t chars[] = { L'a', L'd' };

	EXPECT_STREQ(test1.TrimEnd(chars, 2).Chars(), L"bc");
	EXPECT_STREQ(test1.TrimEnd(chars, 1).Chars(), L"bcdddd");
	EXPECT_STREQ(test1.TrimEnd(nullptr, 0).Chars(),  L"bcdddd");

	EXPECT_STREQ(test2.TrimEnd(chars, 2).Chars(), L"123  ");
	EXPECT_STREQ(test2.TrimEnd(nullptr, 0).Chars(), L"123");

	EXPECT_STREQ(L"bcdddd", test1.TrimEnd().Chars());
	EXPECT_STREQ(L"123", test2.TrimEnd().Chars());
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Trim) {
	string test1 = L"aabcdaa";
	string test2 = L"   123     ";

	EXPECT_STREQ(L"cd", test1.Trim(L"ab", 2).Chars());
	EXPECT_STREQ(L"123", test2.Trim().Chars());
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Substring) {
	string test1 = L"abc123";

	// Substring from specified region.
	EXPECT_STREQ(L"abc123", test1.Substring(0, 6).Chars());
	EXPECT_STREQ(L"abc", test1.Substring(0, 3).Chars());
	EXPECT_STREQ(L"c12", test1.Substring(2, 3).Chars());

	// Substring with all characters after the specified position.
	EXPECT_STREQ(L"abc123", test1.Substring(0).Chars());
	EXPECT_STREQ(L"123", test1.Substring(3).Chars());
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Insert) {
	string test1 = L"";
	string test2 = L"a3";

	EXPECT_STREQ(test1.Insert(0, L"abc").Chars(), L"abc");
	EXPECT_STREQ(test2.Insert(1, L"bc12").Chars(), L"abc123");
	EXPECT_STREQ(test2.Insert(0, L"cb").Chars(), L"cba3");
	EXPECT_STREQ(test2.Insert(2, L"21").Chars(), L"a321");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Remove) {
	string test1 = L"abc123";

	// Remove a specified region.
	EXPECT_STREQ(test1.Remove(0, 3).Chars(), L"123");
	EXPECT_STREQ(test1.Remove(2, 3).Chars(), L"ab3");
	EXPECT_STREQ(L"", test1.Remove(0, 6).Chars());
	EXPECT_STREQ(L"abc123", test1.Remove(0, 0).Chars());

	// Remove all characters after the specified position.
	EXPECT_STREQ(L"", test1.Remove(0).Chars());
	EXPECT_STREQ(L"abc", test1.Remove(3).Chars());
	EXPECT_STREQ(L"abc123", test1.Remove(6).Chars());
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Replace) {
	string test1 = L"ababac";

	// Replace character.
	EXPECT_STREQ(test1.Replace(L'a', L'1').Chars(), L"1b1b1c");
	EXPECT_STREQ(test1.Replace(L'x', L'1').Chars(), L"ababac");

	// Replace string.
	EXPECT_STREQ(test1.Replace(L"ab", L"1").Chars(), L"11ac");
	EXPECT_STREQ(test1.Replace(L"ab", L"12").Chars(), L"1212ac");
	EXPECT_STREQ(test1.Replace(L"ab", L"1234").Chars(), L"12341234ac");
	EXPECT_STREQ(test1.Replace(L"ab", L"").Chars(), L"ac");
	EXPECT_STREQ(test1.Replace(L"xy", L"12").Chars(), L"ababac");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, PadLeft) {
	string test1 = L"1234";

	// Pad left with custom character.
	EXPECT_STREQ(test1.PadLeft(6, L'a').Chars(), L"aa1234");
	EXPECT_STREQ(test1.PadLeft(2, L'a').Chars(), L"1234");

	// Pad left with whitespace.
	EXPECT_STREQ(test1.PadLeft(6).Chars(), L"  1234");
	EXPECT_STREQ(test1.PadLeft(2).Chars(), L"1234");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, PadRight) {
	string test1 = L"1234";

	// Pad left with custom character.
	EXPECT_STREQ(test1.PadRight(6, L'a').Chars(), L"1234aa");
	EXPECT_STREQ(test1.PadRight(2, L'a').Chars(), L"1234");

	// Pad left with whitespace.
	EXPECT_STREQ(test1.PadRight(6).Chars(), L"1234  ");
	EXPECT_STREQ(test1.PadRight(2).Chars(), L"1234");
}

TEST(String, Split) {
	string test1 = L"ab cd 12 34";
	string test2 = L"ab; cd; ef;";
	string test3 = L"abcdefghij";
	List<string> list;
	string sep[] = { L"cd", L"gh" };

	// Split string delimited by characters (empty entries not removed).
	test1.Split(list, L" ", 1, 100, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 4);
	EXPECT_STREQ(list[0].Chars(), L"ab");
	EXPECT_STREQ(list[1].Chars(), L"cd");
	EXPECT_STREQ(list[2].Chars(), L"12");
	EXPECT_STREQ(list[3].Chars(), L"34");

	list.Clear();
	test1.Split(list, L" ", 1, 2, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 2);
	EXPECT_STREQ(list[0].Chars(), L"ab");
	EXPECT_STREQ(list[1].Chars(), L"cd");

	list.Clear();
	test1.Split(list, L"", 0, 200, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 1);
	EXPECT_STREQ(list[0].Chars(), test1.Chars());

	list.Clear();
	test1.Split(list, L"x", 1, 200, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 1);
	EXPECT_STREQ(list[0].Chars(), test1.Chars());

	list.Clear();
	test2.Split(list, L"; ", 2, 200, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 6);
	EXPECT_STREQ(list[0].Chars(), L"ab");
	EXPECT_STREQ(list[1].Chars(), L"");
	EXPECT_STREQ(list[2].Chars(), L"cd");
	EXPECT_STREQ(list[3].Chars(), L"");
	EXPECT_STREQ(list[4].Chars(), L"ef");
	EXPECT_STREQ(list[5].Chars(), L"");

	list.Clear();
	test2.Split(list, L" ", 1, 200, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 3);
	EXPECT_STREQ(list[0].Chars(), L"ab;");
	EXPECT_STREQ(list[1].Chars(), L"cd;");
	EXPECT_STREQ(list[2].Chars(), L"ef;");

	// Split string delimited by characters (default separators; empty entries not removed).
	list.Clear();
	test1.Split(list, (wchar_t*)nullptr, 0, 1, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 1);

	list.Clear();
	test3.Split(list, L"ij", 2, 100, StringSplitOptions_RemoveEmptyEntries);
	EXPECT_EQ(list.Count(), 2);
	EXPECT_STREQ(list[0].Chars(), L"abcdefgh");
	EXPECT_STREQ(list[1].Chars(), L"");

	list.Clear();
	test3.Split(list, sep, 2, 100, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 3);
	EXPECT_STREQ(list[0].Chars(), L"ab");
	EXPECT_STREQ(list[1].Chars(), L"ef");
	EXPECT_STREQ(list[2].Chars(), L"ij");

	list.Clear();
	test3.Split(list, sep, 2, 1, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 1);

	list.Clear();
	test3.Split(list, sep, 1, 100, StringSplitOptions_None);
	EXPECT_EQ(list.Count(), 2);
	EXPECT_STREQ(list[0].Chars(), L"ab");
	EXPECT_STREQ(list[1].Chars(), L"efghij");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Length) {
	string test1 = L"1234";
	string test2 = L"";

	EXPECT_EQ(test1.Length(), 4);
	EXPECT_EQ(test2.Length(), 0);
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Join) {
	string array[] = { L"abc", L"def", L"ghi" };

	EXPECT_STREQ(string::Join(L"_", array, 3).Chars(), L"abc_def_ghi");
	EXPECT_STREQ(string::Join(L"12", array, 2).Chars(), L"abc12def");
	EXPECT_STREQ(string::Join(L"+", array, 0).Chars(), L"");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Format) {
	EXPECT_STREQ(string::Format(L"abc").Chars(), L"abc");
	EXPECT_STREQ(string::Format(L"%d %d", 1, 2).Chars(), L"1 2");
	EXPECT_STREQ(string::Format(L"%s_%d", L"abc", 123).Chars(), L"abc_123");
}

//--------------------------------------------------------------------------------------------------------- *
TEST(String, Operators) {
	// Assignment
	string test1 = L"abc";
	string test2 = L"123";
	string test3 = L"abc";
	string test4 = test2;

	EXPECT_EQ(test4.Length(), test2.Length());
	EXPECT_STREQ(test4.Chars(), test2.Chars());

	// Equality.
	EXPECT_FALSE(test1 == test2);
	EXPECT_TRUE(test1 != test2);
	EXPECT_TRUE(test1 == test3);

	// Comparison.
	EXPECT_FALSE(test1 < test2);
	EXPECT_FALSE(test1 <= test2);
	EXPECT_TRUE(test1 >= test2);
	EXPECT_TRUE(test1 > test2);
	EXPECT_TRUE(test1 <= test3);
	EXPECT_TRUE(test1 >= test3);

	// Other.
	EXPECT_STREQ((test1 + test2).Chars(), L"abc123");
	test2 += test3;
	EXPECT_STREQ(test2.Chars(), L"123abc");
	EXPECT_STREQ(test3.Chars(), L"abc");
}