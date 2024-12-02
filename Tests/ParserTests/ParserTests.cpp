#include "../../Parser/Parser.hpp"
#include "../../Common/FileManager.hpp"
#include "../../Common/TargetData.hpp"
#include "../../Common/Context.hpp"
#include "../../Base/StreamReader.hpp"
#include "../../Driver/DiagnosticPrinter.h"
#include "../../Parser/SemanticHolder.hpp"
#include "../../Parser/DeclarationSemantic.hpp"
#include "../../Parser/ExpressionSemantic.hpp"
#include "../../Parser/StatementSemantic.hpp"
#include "../../Parser/PragmaHandler.hpp"
#include "../../Parser/SpecifierExtension.hpp"
#include "../../Parser/DeclspecExtension.hpp"
#include "../../AST/DotPrinter.hpp"
#include "../../AST/TypeManager.hpp"
#include "../../Targets/X64/X64Target.hpp"
#include "../../Lexer/IdentifierTable.h"
#include <gtest\gtest.h>

using namespace Parsing;
using namespace Base;
using namespace Common;
using namespace Driver;
using namespace X64;
using namespace Lexing;

TEST(Parser, Basic) {
	MacroExpander::InitTokenStrings();
	Preprocessor::InitDirectives();
	List<string> include;
	include.Add(Path::GetFullPath("e:\\include"));
	shared<Context> ctx = new Context(X64Target::Create());
	ctx->FileMgr().Initialize(&include);
	ctx->Diagnostic().AddHandler(new DiagnosticPrinter(ctx, true));

	IdentifierTable table;

	#define keyword(text, type) \
		table.AddKeyword(text, (int)type);

	#include "../../Common/Keywords.def"
	#undef keyword

	// Populate the table with built-in macro definitions.
	table.AddDefinition(new DefinitionInfo("__FILE__", true), BuiltinDef_File);
	table.AddDefinition(new DefinitionInfo("__LINE__", true), BuiltinDef_Line);
	table.AddDefinition(new DefinitionInfo("__DATE__", true), BuiltinDef_Date);
	table.AddDefinition(new DefinitionInfo("__TIME__", true), BuiltinDef_Time);
	table.AddDefinition(new DefinitionInfo("__STDC__", true), BuiltinDef_Time);

	shared<Lexer> lexer = new Lexer(&table, ctx->Target(), &ctx->FileMgr(), &ctx->Diagnostic());
	if(lexer->LoadStart("D:\\p.txt")) {
		shared<SemanticHolder> holder = new SemanticHolder();
		TypeManager typeMan;
		holder->SetDeclSema(new DeclSemantic(ctx, &typeMan));
		holder->SetExprSema(new ExprSemantic(ctx, &typeMan));
		holder->SetStmtSema(new StmtSemantic(ctx, &typeMan));

		Parser parser(lexer, ctx, &typeMan, holder);
		parser.AddExtension(new PragmaHandler(ctx, &parser));
		parser.AddExtension(new SpecifierExtension(ctx, &parser));
		parser.AddExtension(new DeclspecExtension(ctx, &parser));
		shared<DeclContext> ctx = new DeclContext(Scope_File, nullptr);

		if(parser.ParseTranslationUnit()) {
			std::cout<<"\n\nDONE **************************\n\n";
			AST::DotPrinter printer(L"D:\\results.txt", true,true);
			printer.Print(&parser.UnitInfo());
		}
	}
}

