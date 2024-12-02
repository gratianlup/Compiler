#include "../../Base/String.hpp"
#include "../../Base/List.hpp"
#include "../../Base/Directory.hpp"
#include "../../Lexer/Lexer.hpp"
#include "../../Common/FileManager.hpp"
#include "../../Common/TargetData.hpp"
#include "../../Base/StreamReader.hpp"
#include "../../Lexer/Token.hpp"
#include "../../Targets/X64/X64Target.hpp"
#include <gtest\gtest.h>
using namespace Base;
using namespace Common;
using namespace Lexing;
#include <fstream>
#include <iostream>

// Reads and interprets a file with the expected tokens.
// Format:
// token_type # token_value # options \n
// token_type = ident, numb, str, char, kwd, op
// options = w - wide string/char, s - standard
// op token_value = oparen, cparen
class ExpectReader {
private:
	struct TokenInfo {
		TokenKind Kind;
		string Value;
		bool Wide;
	};

	StreamReader reader_;
	List<TokenInfo> tokens_;
	int pos_;

	void ReadFile() {
		string str;

		while(reader_.EndOfStream() == false) {
			List<string> list;
			str = reader_.ReadLine();
			if(str.Trim().Length() == 0) continue;
			str.Split(list, _T("#"), 1, StringSplitOptions_RemoveEmptyEntries);

			TokenInfo info;
			if(list[0].Trim() == _T("ident")) {
				info.Kind = Token_Identifier;
				if(list.Count() > 1) info.Value = list[1].Trim();
			}
			else if(list[0].Trim() == _T("kwd")) {
				//info.Kind = Token_Keyword;
				if(list.Count() > 1) info.Value = list[1].Trim();
			}
			else if(list[0].Trim() == _T("numb")) {
				info.Kind = Token_Number;
				if(list.Count() > 1) info.Value = list[1].Trim();
			}
			else if(list[0].Trim() == _T("str")) {
				info.Kind = Token_String;
				info.Wide = false;
				if(list.Count() > 1) info.Value = list[1].Trim();
				if(list.Count() > 2) info.Wide = list[2].Trim() == "w";
			}
			else if(list[0].Trim() == _T("char")) {
				info.Kind = Token_Char;
				info.Wide = false;
				if(list.Count() > 1) info.Value = list[1].Trim();
				if(list.Count() > 2) info.Wide = list[2].Trim() == "w";
			}
			else if(list[0].Trim() == _T("op")) {
				if(list.Count() > 1) {
					if(list[1].Trim() == "oparen") {
						info.Kind = Token_OpenParen;
						tokens_.Add(info);
						continue;
					}
					else if(list[1].Trim() == "cparen") {
						info.Kind = Token_CloseParen;
						tokens_.Add(info);
						continue;
					}
				}

				info.Kind = Token_Add;
			}

			tokens_.Add(info);
		};
	}

	void Compare(TokenInfo& info, Token& token) {
		if(token.IsOperator()) {
			ASSERT_EQ(Token_Add, info.Kind);
		}
		else {
			ASSERT_EQ(info.Kind, token.Kind());
		}

		/*if(token.Kind() == Token_Identifier || token.Kind() == Token_Keyword) {
			EXPECT_STREQ(info.Value.Chars(), token.NameValue()->Name.Chars());
		}
		else */if(token.Kind() == Token_Number) {
			EXPECT_STREQ(info.Value.Chars(), token.NumberValue()->Number.Chars());
		}
		else if(token.Kind() == Token_String || token.Kind() == Token_Char) {
			EXPECT_STREQ(info.Value.Chars(), token.StringValue()->Value.Chars());
		}
	}

public:
	ExpectReader(const string& file) : reader_(file) {
		ReadFile();
		pos_ = 0;
	}

	void Check(Token& token) {
		ASSERT_LT(pos_, tokens_.Count());
		Compare(tokens_[pos_++], token);
	}
};

TEST(Lexer, Auto) {
	MacroExpander::InitTokenStrings();
	Preprocessor::InitDirectives();
	X64::X64Target target;
	List<string> include;
	include.Add(Path::GetFullPath(".\\Tests\\LexerTests\\Include"));
	FileManager man;
	man.Initialize(&include);

	//auto files = Directory::GetFiles(".\\Tests\\LexerTests\\Data");
	//files->ForEach([&target, &man](string& file) -> bool {
	//	string path = file;
	//	file = Path::GetFileName(path);

	//	if(file.StartsWith("test")) {
	//		string name = file.Substring(5);
	//		string expect = "expect_" + name;

	//		std::wcout<<"*** "<<name.Chars()<<" ***\n";

	//		ExpectReader reader(Path::Combine(Path::GetDirectoryName(path), expect));
	//		Lexer lexer(&target, &man);
	//		if(lexer.LoadStart(path)) {
	//			Token token;
	//			do {
	//				lexer.NextToken(token);
	//				//token.Debug();
	//				if(token.IsEOF() == false) {
	//					reader.Check(token);
	//				}
	//			} while(token.IsEOF() == false);
	//		}
	//	}

	//	return true;
	//});
}