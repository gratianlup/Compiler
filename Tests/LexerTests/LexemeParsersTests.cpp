#include "../../Lexer/LexemeParsers.hpp"
#include "../../Lexer/Token.hpp"
#include "../../Common/TargetData.hpp"
#include "../../Common/CompileOptions.hpp"
#include "../../Common/Diagnostic.hpp"
#include "../../Base/LocalPointer.hpp"
#include "../../Base/String.hpp"
#include "../../Targets/X64/X64Target.hpp"
#include <gtest\gtest.h>
#include <stdlib.h>
using namespace Lexing;
using namespace Common;
using namespace Base;

Token* MakeNToken(string data) {
	Token* token = new Token(Token_Number);
	token->SetData(new NumberData(data));
	return token;
}

Token* MakeCToken(string data, bool wide) {
	Token* token = new Token(Token_Char);
	token->SetData(new StringData(data, wide));
	return token;
}

Token* MakeSToken(string data, bool wide) {
	Token* token = new Token(Token_String);
	token->SetData(new StringData(data, wide));
	return token;
}

TEST(LexemeParsers, Integer) {
	X64::X64Target target;
	CompileOptions opt;
	Diagnostic diag(&opt);
	NumberParser p(&diag);
	
	local<Token> token = MakeNToken("123");
	NumberInfo info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_TRUE(info.IsInteger);
	EXPECT_TRUE(info.IsInt());
	EXPECT_EQ(123, info.IntValue);

	token = MakeNToken("123456789123456789");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_TRUE(info.IsInteger);
	EXPECT_TRUE(info.IsInt());
	EXPECT_EQ(123456789123456789, info.IntValue);

	token = MakeNToken("123456789123456789123456789123456789");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Oveflow);

	token = MakeNToken("123u");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_TRUE(info.IsInteger);
	EXPECT_TRUE(info.IsUInt());
	EXPECT_EQ(123, info.IntValue);

	token = MakeNToken("123ul");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_TRUE(info.IsInteger);
	EXPECT_TRUE(info.IsULong());
	EXPECT_EQ(123, info.IntValue);

	token = MakeNToken("123ull");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_TRUE(info.IsInteger);
	EXPECT_TRUE(info.IsULongLong());
	EXPECT_EQ(123, info.IntValue);
}

TEST(LexemeParsers, Floating) {
	X64::X64Target target;
	CompileOptions opt;
	Diagnostic diag(&opt);
	NumberParser p(&diag);

	local<Token> token = MakeNToken("123.1");
	NumberInfo info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsInteger);
	EXPECT_TRUE(info.IsDouble());
	EXPECT_FLOAT_EQ(123.1, info.FloatValue);

	token = MakeNToken("1.7976931348623158E+310");
	info = p.Parse(*token);
	EXPECT_FALSE(info.Valid);

	token = MakeNToken("1.0E-1");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsInteger);
	EXPECT_TRUE(info.IsDouble());
	EXPECT_FLOAT_EQ(1.0E-1, info.FloatValue);

	token = MakeNToken("1.0E+2");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsInteger);
	EXPECT_TRUE(info.IsDouble());
	EXPECT_FLOAT_EQ(1.0E+2, info.FloatValue);

	token = MakeNToken("1.0E2");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsInteger);
	EXPECT_TRUE(info.IsDouble());
	EXPECT_FLOAT_EQ(1.0E2, info.FloatValue);

	token = MakeNToken("123.1f");
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsInteger);
	EXPECT_TRUE(info.IsFloat());
	EXPECT_FLOAT_EQ(123.1, info.FloatValue);

	token = MakeNToken("123.1uf");
	info = p.Parse(*token);
	EXPECT_FALSE(info.Valid);

	token = MakeNToken("123.1ff");
	info = p.Parse(*token);
	EXPECT_FALSE(info.Valid);

	token = MakeNToken("123.1lf");
	info = p.Parse(*token);
	EXPECT_FALSE(info.Valid);
}

TEST(LexemeParsers, Char) {
	X64::X64Target target;
	CompileOptions opt;
	Diagnostic diag(&opt);
	CharParser p(&diag);

	local<Token> token = MakeCToken("a", false);
	CharInfo info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsWide);
	EXPECT_EQ('a', info.Value);

	token = MakeCToken("ab", false);
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsWide);
	EXPECT_EQ('b', info.Value);

	token = MakeCToken("\\t", false);
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsWide);
	EXPECT_EQ('\t', info.Value);

	token = MakeCToken("\\123", false);
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsWide);
	EXPECT_EQ((wchar_t)83, info.Value);

	token = MakeCToken("\\x3A", false);
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_FALSE(info.IsWide);
	EXPECT_EQ((wchar_t)58, info.Value);

	token = MakeCToken("\\x3a\\n", true);
	info = p.Parse(*token);
	EXPECT_TRUE(info.Valid);
	EXPECT_TRUE(info.IsWide);
	EXPECT_EQ((wchar_t)14858, info.Value);
}