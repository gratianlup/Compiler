#include "../../Base/StringBuilder.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(StringBuilder, Append) {
	StringBuilder sb;

	EXPECT_STREQ(_T("abc"), sb.Append(_T("abc")).ToString().Chars());
	EXPECT_STREQ(_T("abc12"), sb.Append(_T("12")).ToString().Chars());
	EXPECT_STREQ(_T("abc12"), sb.Append(_T("")).ToString().Chars());
	EXPECT_STREQ(_T("abc123"), sb.Append(_T('3')).ToString().Chars());
	EXPECT_STREQ(_T("abc123ddd"), sb.Append(_T('d'), 3).ToString().Chars());
	EXPECT_STREQ(_T("abc123ddd"), sb.Append(_T('d'), 0).ToString().Chars());
	EXPECT_STREQ(_T("abc123dddxy"), sb.Append(_T("xy"), 0, 2).ToString().Chars());
	EXPECT_STREQ(_T("abc123dddxy"), sb.Append(_T("xy"), 0, 0).ToString().Chars());
	EXPECT_STREQ(_T("abc123dddxyz"), sb.Append(_T("xyz"), 2, 1).ToString().Chars());
}						  

TEST(StringBuilder, AppendFormat) {
	StringBuilder sb;

	EXPECT_STREQ(_T("ab"), sb.AppendFormat(_T("%s"), _T("ab")).ToString().Chars());
	EXPECT_STREQ(_T("ab12"), sb.AppendFormat(_T("%d"), 12).ToString().Chars());
	EXPECT_STREQ(_T("ab1234 5.60"), sb.AppendFormat(_T("%d %.2f"), 34, 5.6).ToString().Chars());
	EXPECT_STREQ(_T("ab1234 5.60"), sb.AppendFormat(_T("")).ToString().Chars());
}

TEST(StringBuilder, AppendLine) {
	StringBuilder sb;

	EXPECT_STREQ(_T("abc\r\n"), sb.AppendLine(_T("abc")).ToString().Chars());
	EXPECT_STREQ(_T("abc\r\n\r\n"), sb.AppendLine(_T("")).ToString().Chars());
	EXPECT_STREQ(_T("abc\r\n\r\n\r\n"), sb.AppendLine().ToString().Chars());
}

TEST(StringBuilder, Insert) {
	StringBuilder sb;

	EXPECT_STREQ(_T("abc"), sb.Insert(0, _T("abc")).ToString().Chars());
	EXPECT_STREQ(_T("12abc"), sb.Insert(0, _T("12")).ToString().Chars());
	EXPECT_STREQ(_T("12abc34"), sb.Insert(5, _T("34")).ToString().Chars());
	EXPECT_STREQ(_T("1x2abc34"), sb.Insert(1, _T("x")).ToString().Chars());
	EXPECT_STREQ(_T("1x2abc3y4"), sb.Insert(7, _T("y")).ToString().Chars());
	EXPECT_STREQ(_T("1x2abc3y4"), sb.Insert(0, _T("")).ToString().Chars());
	EXPECT_STREQ(_T("1xzzz2abc3y4"), sb.Insert(2, string(_T("z")), 3).ToString().Chars());
	EXPECT_STREQ(_T("1xzzz2abc3y4t"), sb.Insert(12, _T('t')).ToString().Chars());
}

TEST(StringBuilder, Remove) {
	StringBuilder sb;

	sb.Append("abcdef");
	EXPECT_STREQ(_T("bcdef"), sb.Remove(0, 1).ToString().Chars());
	EXPECT_STREQ(_T("bcde"), sb.Remove(4, 1).ToString().Chars());
	EXPECT_STREQ(_T("be"), sb.Remove(1, 2).ToString().Chars());
	EXPECT_STREQ(_T(""), sb.Remove(0, 2).ToString().Chars());
}