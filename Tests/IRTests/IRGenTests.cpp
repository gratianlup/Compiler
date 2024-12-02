#include "../../IR/IRGenerator.hpp"
#include "../../IR/ConstantTable.hpp"
#include "../../IR/IRVerifier.hpp"
#include "../../IR/IRPrinter.hpp"
#include "../../IR/Interpreter.hpp"
#include "../../IR/InterpreterDriver.hpp"
#include "../../IR/FlowDotPrinter.hpp"
#include "../../IR/IRParserDriver.hpp"
#include "../../IR/Intrinsic.hpp"
#include "../../Common/Context.hpp"
#include "../../Targets/X64/X64Target.hpp"
#include <gtest\gtest.h>
#include <fstream>
using namespace IR;
using namespace Common;
using namespace X64;

TEST(IRGenerator, Basic) {
	Context* context = new Context(X64Target::Create());
	TypeTable types;
	IntrinsicTable instrinsics;
	ConstantTable consts;
	auto unit = Unit::GetUnit(context, &types, &consts, &instrinsics);
	IRGenerator g(unit);
	Interpreter inter(unit);
	string s;

	IRParserDriver parserDriver("D:\\result.irl");

	/*
		struct ABC { int a; __int64 b; }
		struct ABC test = {1,2};

		int main() { return test.b; }

		// --------------------------------------------- *
		type ABC = record { int32, int64 }
		var test <ABC> = {1,2}

		funct main() : int32 {
		entry:
			t1 = addr test, 1 // int64*
			t2 = load t1      // int64
			t3 = trunc t2, int32
			ret t3
		}

	*/

	// type ABC
	//List<RecordField> fields;
	//fields.Add(RecordField(g.GetInt32(), 0));
	//fields.Add(RecordField(g.GetInt64(), 4));
	//auto abc = g.GetNamedRecord("ABC", fields);

	//// test <ABC> = {1,2}
	//shared<InitializerList> init = new InitializerList();
	//init->Add(new Initializer(g.GetInt32Const(1)));
	//init->Add(new Initializer(g.GetInt64Const(0x800000008)));
	//auto test = g.GetGlobalSymbol(g.GetNamed("ABC"), "test", init);

	//// funct main
	//auto functType = g.GetFunction(g.GetInt32());
	//auto funct = g.GetFunctionSymbol(functType, "main", true, unit);

	//// entry
	//auto entry = g.GetBlockSymbol("entry", funct);
	//auto t1 = g.GetTemporary(g.GetInt64Pointer());
	//g.GetAddress(g.GetVariableRef(test), g.GetInt32Const(1), t1, entry);

	//auto t2 = g.GetInt64Temp();
	//g.GetLoad(t1, t2, entry);

	//auto t3 = g.GetInt32Temp();
	//g.GetTrunc(t2, g.GetInt32(), t3, entry);
	//g.GetReturn(t3, entry);

	/* 
		int test(int a) { return a * a; }
		int main() { return test(5); }

		// --------------------------------------------- *
		funct test(a int32) : int32 {
		entry:
			t1 = load a
			t2 = mul t1, t1
			ret t2
		}

		funct main() : int32 {
			t1 = call test, 5
			ret t1
		}
	
	*/

	// test
	//List<const Type*> params;
	//params.Add(g.GetInt32());
	//auto testType = g.GetFunction(g.GetInt32(), params.GetInternal(), 1);
	//auto test = g.GetFunctionSymbol(testType, "test", true, unit);
	//auto a = g.GetVariableSymbol(g.GetInt32(), "a");
	//test->Parameters().Add(a);

	//// block entry
	//auto entry1 = g.GetBlockSymbol("entry", test);
	//auto t1 = g.GetInt32Temp();
	//g.GetLoad(a, t1, entry1);
	//auto t2 = g.GetInt32Temp();
	//g.GetMul(t1, t1, t2, entry1);
	//g.GetReturn(t2, entry1);

	//// main
	//auto functType = g.GetFunction(g.GetInt32());
	//auto funct = g.GetFunctionSymbol(functType, "main", true, unit);

	//// block entry
	//auto entry2 = g.GetBlockSymbol("entry", funct);
	//auto t3 = g.GetInt32Temp();

	//// call
	//shared<CallInstr::TArgList> args = new CallInstr::TArgList();
	//args->Add(g.GetInt32Const(5));
	//g.GetCall(test, args, t3, entry2);
	//g.GetReturn(t3, entry2);

	/*
		int main() {
			int i = 0, sum = 0, n = ReadInt32();
			while(i < n) { sum += 5; i++; }
			return sum;
		}

		funct decl ReadInt32() : int32
		funct decl PrintInt32(int32) : void

		funct main() : int32 {
			var i int32
			var sum int32
			var n int32

		entry:
			store i, 0
			store sum, 0
			t8 = call ReadInt32
			load n, t8
			goto test

		test:
			t1 = load i
			t9 = load n
			t2 = cmp lt t1, t9
			if t2 body, done

		body:
			t3 = load sum
			t4 = add t3, 5
			store sum, t2
			call PrintInt32, t2
			t5 = load i
			t6 = add t5, 1
			store i, t6
			goto test

		done:
			t7 = load sum
			return t7
		}
	*/

	auto readInt = ReadInt32::Register(&inter);
	auto printInt = PrintInt32::Register(&inter);
	auto functType = g.GetFunction(g.GetInt32());
	auto funct = g.GetFunctionSymbol(functType, "main", true, unit);
	auto i = g.GetVariableSymbol(g.GetInt32(), "i", funct);
	auto sum = g.GetVariableSymbol(g.GetInt32(), "sum", funct);
	auto n = g.GetVariableSymbol(g.GetInt32(), "n", funct);

	auto entry = g.GetBlockSymbol("entry", funct);
	auto test = g.GetBlockSymbol("test", funct);
	auto body = g.GetBlockSymbol("body", funct);
	auto done = g.GetBlockSymbol("done", funct);

	// entry
	
	g.GetStore(i, g.GetInt32Const(0), entry);
	g.GetStore(sum, g.GetInt32Const(0), entry);

	auto t8 = g.GetInt32Temp();
//	g.GetCall(readInt, nullptr, t8, entry);
	g.GetStore(n, t8, entry);
	g.GetGoto(test, entry);

	// test
	auto t1 = g.GetInt32Temp();
	g.GetLoad(i, t1, test);
	auto t9 = g.GetInt32Temp();
	g.GetLoad(n, t9, test);
	auto t2 = g.GetInt32Temp();
	t2->SetTagId(g.GetCmpName());
	g.GetCmpLT(t1, t9, t2, test);
	g.GetIf(t2, g.GetBlockRef(body), g.GetBlockRef(done), test);

	// body
	auto t3 = g.GetInt32Temp();
	g.GetLoad(sum, t3, body);
	auto t4 = g.GetInt32Temp();
	g.GetAdd(t3, g.GetInt32Const(5), t4, body);
	g.GetStore(sum, t4, body);

	// print sum
	shared<CallInstr::TArgList> args = new CallInstr::TArgList();
	args->Add(t4);
	//g.GetCall(printInt, args, nullptr, body);
	
	auto t5 = g.GetInt32Temp();
	g.GetLoad(i, t5, body);
	auto t6 = g.GetInt32Temp();
	g.GetAdd(t5, g.GetInt32Const(1), t6, body);
	g.GetStore(i, t6, body);
	g.GetGoto(test, body);

	// done
	auto t7 = g.GetInt32Temp();
	g.GetLoad(sum, t7, done);
	g.GetReturn(t7, done);

	// --------------------------------------------- *	
	DefaultVerifierHandler handler;
	IRVerifier verifier(unit, &handler);

	FlowDotPrinter flowPrinter("D:\\flow.txt");
	flowPrinter.Print(funct);

	//std::wofstream fout("D:\\result.irl");
	IRPrinter printer(unit);
	printer.Dump();
	//fout<<printer.ToString().Chars();
	//fout.close();

	InterpreterDriver driver(&inter);
/*
	Value* result = inter.Start(funct);
	if(result) {
		std::wcout<<"\nResult: "<<result->AsInteger();
		result->Free();
	}*/

	//List<const Type*> params;
	//params.Add(g.GetInt32());
	//auto functType = g.GetFunction(g.GetVoid(), params.GetInternal(), params.Count());

	//g.GetGlobalSymbol(g.GetInt16(), "globalVar");
	//g.GetGlobalSymbol(g.GetInt16(), "globalInit", new Initializer(g.GetInt16Const(12)));

	//InitializerList* list = new InitializerList();
	//list->Add(new Initializer(g.GetInt32Const(1)));
	//list->Add(new Initializer(g.GetInt32Const(2)));
	//list->Add(new Initializer(g.GetInt32Const(3)));
	//list->Add(new Initializer(g.GetInt32Const(4)));
	//list->Add(new Initializer(g.GetInt32Const(5)));
	//g.GetGlobalSymbol(g.GetInt32Array(5), "globalArrayInit", list);


	//List<RecordField> fields;
	//fields.Add(RecordField(g.GetInt32(), 0));
	//fields.Add(RecordField(g.GetInt32Pointer(), 4));
	//fields.Add(RecordField(g.GetInt64Array(24), 8));
	//auto record = g.GetRecord(fields);
	//g.AddNamed("ABC", record);

	//g.GetGlobalSymbol(g.GetNamed("ABC"), "testRecord");

	//auto functSym = g.GetFunctionSymbol(functType, "test");
	//auto nSym = g.GetVariableSymbol(g.GetInt32(), "n", functSym);
	//nSym->SetIsParameter(true);
	//functSym->Parameters().Add(nSym);

	//auto arraySym = g.GetVariableSymbol(g.GetInt8Array(10), "array", functSym);
	//auto iSym = g.GetVariableSymbol(g.GetInt32(), "i", functSym);

	
}
