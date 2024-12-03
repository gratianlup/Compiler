// ASTDotPrinter.cpp
// Copyright (c) Lup Gratian
//
// Implements the module that exports the AST to a Graphviz .dot file.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ASTDotPrinter.hpp"
#include <fstream>

namespace AST {

ASTDotPrinter::ASTDotPrinter(const string& path, bool showTypes, bool useClusters) : 
					   nodes_(0), clusters_(0), fout(path.Chars()), showTypes_(showTypes),
					   identLevel_(0), useClusters_(useClusters) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Write(const string& s) {
	fout<<s.Chars();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeType(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=trapezium, style=filled";
	fout<<", color=gold, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeExpression(const string& label, const string& color) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=ellipse, style=filled";
	fout<<", color="<<color.Chars()<<", label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeOperator(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=circle, style=filled";
	fout<<", color=lightblue, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeUnaryOperator(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=doublecircle, style=filled";
	fout<<", color=lightblue, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeConstant(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=box, style=filled";
	fout<<", color=gray80, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeCast(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=ellipse, style=filled";
	fout<<", color=lightsalmon, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeReference(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=box, style=filled";
	fout<<", color=gray80, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeStatement(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=hexagon, style=filled";
	fout<<", color=lightpink, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeNullStatement(const string& label, bool isError) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=hexagon, style=filled";
	
	if(isError) {
		fout<<", color=red, label=\""<<label.Chars()<<"\"];\n";
	}
	else fout<<", color=lightpink, label=\""<<label.Chars()<<"\"];\n";

	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeLoop(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=doubleoctagon, style=filled";
	fout<<", color=lightpink, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeDeclaration(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=rectangle, style=filled";
	fout<<", color=darkolivegreen3, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeAttribute(const string& label) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<nodes_<<"[shape=parallelogram, style=filled";
	fout<<", color=lightblue, label=\""<<label.Chars()<<"\"];\n";
	nodes_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::MakeLink(int a, int b, const string& label, bool dashed) {
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"n"<<a<<" -> "<<"n"<<b;
	fout<<"[label=\""<<label.Chars()<<"\", fontsize=12, labeldistance=10";
	
	if(dashed) fout<<", style=dashed";
	fout<<"];\n";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::BeginStatementCluster() {
	if(useClusters_ == false) return;
	
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"subgraph cluster"<<clusters_<<" {\n";
	fout<<string(L'\t', identLevel_ + 1).Chars();
	fout<<"color=firebrick3;\n";
	clusters_++;
	identLevel_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::BeginDeclarationCluster() {
	if(useClusters_ == false) return;

	fout<<string(L'\t', identLevel_).Chars();
	fout<<"subgraph cluster"<<clusters_<<" {\n";
	fout<<string(L'\t', identLevel_ + 1).Chars();
	fout<<"color=forestgreen;\n";
	clusters_++;
	identLevel_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::BeginCallCluster() {
	if(useClusters_ == false) return;
	
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"subgraph cluster"<<clusters_<<" {\n";
	fout<<string(L'\t', identLevel_ + 1).Chars();
	fout<<"color=darkorchid3;\n";
	clusters_++;
	identLevel_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::BeginCastCluster() {
	if(useClusters_ == false) return;
	
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"subgraph cluster"<<clusters_<<" {\n";
	fout<<string(L'\t', identLevel_ + 1).Chars();
	fout<<"color=tomato;\n";
	clusters_++;
	identLevel_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::BeginExpressionCluster() {
	if(useClusters_ == false) return;
	
	fout<<string(L'\t', identLevel_).Chars();
	fout<<"subgraph cluster"<<clusters_<<" {\n";
	fout<<string(L'\t', identLevel_ + 1).Chars();
	fout<<"color=blueviolet;\n";
	clusters_++;
	identLevel_++;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::EndCluster() {
	if(useClusters_ == false) return;
	
	fout<<string(L'\t', identLevel_ - 1).Chars();
	fout<<"}\n";
	identLevel_--;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::BeginGraph() {
	fout<<"digraph {\n";
	fout<<"ranksep=0.5;\n";
	//fout<<"concentrate=true;\n";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::EndGraph() {
	fout<<"}\n";
}

void ASTDotPrinter::Print(Visitable* start, bool expandFunct) {
	expandFunct_ = expandFunct;
	BeginGraph();
	start->Accept(this);
	EndGraph();
	fout.close();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::PrintAttribute(const Attribute* attr) {
	if(auto temp = attr->As<PackAttribute>()) {
		MakeAttribute(string::Format(L"Pack: %d", temp->Value()));
	}
	else if(auto temp = attr->As<CallConventionAttribute>()) {
		string detail;
		if(temp->IsStdcall()) detail = "stdcall";
		else if(temp->IsCdecl()) detail = "cdecl";
		else if(temp->IsFastcall()) detail = "fastcall";
		MakeAttribute(string::Format(L"Call: %s", detail.Chars()));
	}
	else if(auto temp = attr->As<InlineAttribute>()) {
		string detail;
		if(temp->IsAlways()) detail = "always";
		else if(temp->IsNever()) detail = "never";
		else detail = "auto";
		MakeAttribute(string::Format(L"Inline: %s", detail.Chars()));
	}
	else if(auto temp = attr->As<AlignmentAttribute>()) {
		MakeAttribute(string::Format(L"Alignment: %d", temp->Value()));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::PrintAttributes(const Declaration* declaration, int parent) {
	if(declaration == nullptr) return;

	for(int i = 0; i < declaration->AttributeCount(); i++) {
		const Attribute* attr = declaration->GetAttribute(i);
		int n = nodes_;
		PrintAttribute(attr);
		MakeLink(parent, n, string::Format(L"%d", i));
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Print(Unit* unit, bool expandFunct) {
	expandFunct_ = expandFunct;
	BeginGraph();
	int n = nodes_;
	MakeExpression("Translation Unit", "azure2");
	auto& list = unit->UnitDeclarations();
    int ct = 0;

    for(int k = 0; k < 2; k++) {
        List<shared<Declaration>>* list = k == 0 ? &unit->Tags() : 
                                                   &unit->UnitDeclarations();
	    
        for(int i = 0; i < list->Count(); i++) {
		    int previous = nodes_;
		    BeginDeclarationCluster();
		    (*list)[i]->Accept(this);
		    EndCluster();
		    MakeLink(n, previous, string::Format(L"%d", ct));
            ct++;
	    }
    }

	EndGraph();
	fout.close();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const QType* type) {
	StringBuilder sb("Qual");
	if(type->HasConst())    sb.Append("\\nconst");
	if(type->HasRestrict()) sb.Append("\\nrestrict");
	if(type->HasVolatile()) sb.Append("\\nvolatile");

	int n = nodes_;
	MakeType(sb.ToString());
	type->Base()->Accept(this);
	MakeLink(n, n + 1, "Base");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const BasicType* type) {
	StringBuilder sb;
	if(type->IsBool())      sb.Append("Bool");
	if(type->IsChar())      sb.Append("Char");
	if(type->IsUChar())     sb.Append("UChar");
	if(type->IsWChar())     sb.Append("WChar");
	if(type->IsString())    sb.Append("String");
	if(type->IsWString())   sb.Append("WString");
	if(type->IsShort())     sb.Append("Short");
	if(type->IsUShort())    sb.Append("UShort");
	if(type->IsInt())       sb.Append("Int");
	if(type->IsUInt())      sb.Append("UInt");
	if(type->IsLong())      sb.Append("Long"); 
	if(type->IsULong())     sb.Append("ULong"); 
	if(type->IsLongLong())  sb.Append("LongLong"); 
	if(type->IsULongLong()) sb.Append("ULongLong");
	if(type->IsFloat())     sb.Append("Float"); 
	if(type->IsDouble())    sb.Append("Double");
	if(type->IsVoid())      sb.Append("Void"); 

	MakeType(sb.ToString());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const PointerType* type) {
	typeIndex_.Add((size_t)type, nodes_);
	StringBuilder sb("Pointer");

	int n = nodes_;
	MakeType(sb.ToString());

	int index;
	if(typeIndex_.TryGetValue((size_t)type->PointeeType(), &index)) {
		MakeLink(n, index, "PointeeType");
	}
	else {
		int previous = nodes_;
		type->PointeeType()->Accept(this);
		MakeLink(n, previous, "PointeeType");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const ArrayType* type) {
	typeIndex_.Add((size_t)type, nodes_);
	StringBuilder sb(type->IsIncomplete() ? "Array []" :
					 string::Format(L"Array [%d]", type->Size()));

	if(type->IsIncomplete()) sb.Append("\\nIncomplete");
	if(type->IsStatic())     sb.Append("\\nStatic");

	int n = nodes_;
	MakeType(sb.ToString());

	int index;
	if(!type->ElementType()->IsBasic() &&
		typeIndex_.TryGetValue((size_t)type->ElementType(), &index)) {
		MakeLink(n, index, "Element");
	}
	else {
		int previous = nodes_;
		type->ElementType()->Accept(this);
		MakeLink(n, previous, "Element");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const VarArrayType* type) {
	typeIndex_.Add((size_t)type, nodes_);
	StringBuilder sb("VarArray");

	if(type->IsIncomplete()) sb.Append("\\nIncomplete");
	if(type->IsStatic())     sb.Append("\\nStatic");

	int n = nodes_;
	MakeType(sb.ToString());
	type->SizeExpression()->Accept(this);
	MakeLink(n, n + 1, "Expr");

	int index;
	if(!type->ElementType()->IsBasic() &&
		typeIndex_.TryGetValue((size_t)type->ElementType(), &index)) {
		MakeLink(n, index, "Element");
	}
	else {
		int previous = nodes_;
		type->ElementType()->Accept(this);
		MakeLink(n, previous, "Element");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const FunctionType* type) {
	typeIndex_.Add((size_t)type, nodes_);
	StringBuilder sb("Function");

	if(type->IsVarargs()) sb.Append("\\nvarargs");
	if(type->HasPrototype()) sb.Append("\\nprototype");

	int n = nodes_;
	MakeType(sb.ToString());

	type->ReturnType()->Accept(this);
	MakeLink(n, n + 1, "Return");

	int paramNode = nodes_;
	MakeExpression(string::Format(L"Params: %d", type->ParameterCount()), "lightgoldenrod1");
	MakeLink(n, paramNode, "");

	for(int i = 0; i < type->ParameterCount(); i++) {
		int previous = nodes_;
		auto parameter = type->Parameters()[i];
		parameter->Accept(this);	
		MakeLink(paramNode, previous, string::Format(L"%d", i));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const EnumType* type) {
	typeIndex_.Add((size_t)type, nodes_);
	StringBuilder sb("Enum");

	int n = nodes_;
	MakeType(sb.ToString());
		
	type->ConstType()->Accept(this);
	MakeLink(n, n + 1, "Type");	

	for(int i = 0; i < type->ConstantCount(); i++) {
		BeginDeclarationCluster();
		int previous = nodes_;
		auto parameter = type->Constants()[i];
		parameter->Accept(this);
		EndCluster();
		MakeLink(n, previous);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const StructType* type) {
	typeIndex_.Add((size_t)type, nodes_);
	int n = nodes_;
	MakeType("Struct");

	for(int i = 0; i < type->FieldCount(); i++) {
		BeginDeclarationCluster();
		int previous = nodes_;
		auto parameter = type->Fields()[i];
		parameter->Accept(this);
		EndCluster();
		MakeLink(n, previous, string::Format(L"%d", i));
	}

	PrintAttributes(type->ParentDeclaration()->GetDefinition(), n);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const UnionType* type) {
	typeIndex_.Add((size_t)type, nodes_);
	int n = nodes_;
	MakeType("Union");

	for(int i = 0; i < type->FieldCount(); i++) {
		BeginDeclarationCluster();
		int previous = nodes_;
		auto parameter = type->Fields()[i];
		parameter->Accept(this);
		EndCluster();
		MakeLink(n, previous, string::Format(L"%d", i));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const TypedefType* type) {
	typeIndex_.Add((size_t)type, nodes_);
	StringBuilder sb("Typedef");

	int n = nodes_;
	MakeType(sb.ToString());
	type->Inner()->Accept(this);
	MakeLink(n, n + 1, "Inner");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const VariableDeclaration* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb("Variable");

	if(declaration->Name()) sb.Append("\\n\\\"" + declaration->Name()->Name() + "\\\"");
	if(declaration->Storage() == StorageType::Static) sb.Append("\\nstatic");
	if(declaration->Storage() == StorageType::Extern) sb.Append("\\nextern");

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	if(declaration->Initializer()) {
		declaration->Initializer()->Accept(this);
		MakeLink(n, n + 1, "Init");
	}

	if(declaration->Previous()) {
		int index;
		if(declIndex_.TryGetValue((size_t)declaration->Previous(), &index)) {
			MakeLink(n, index, "Previous");
		}
		else {
			int previous = nodes_;
			declaration->Previous()->Accept(this);
			MakeLink(n, previous, "Previous");
		}
	}

	if(showTypes_ == false) return;

	int index;
	if((declaration->DeclarationType()->IsBasic() == false) && 
       (declaration->DeclarationType()->IsPointer() == false) &&
		typeIndex_.TryGetValue((size_t)declaration->DeclarationType(), &index)) {
		MakeLink(n, index, "Type", true /* dashed */);
	}
	else {
		int previous = nodes_;
		declaration->DeclarationType()->Accept(this);
		MakeLink(n, previous, "Type", true /* dashed */);
	}

	PrintAttributes(declaration, n);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const FunctionDeclaration* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb("Function");

	if(declaration->Name()) sb.Append("\\n\\\"" + declaration->Name()->Name() + "\\\"");
	if(declaration->IsStatic()) sb.Append("\\nstatic");
	if(declaration->IsInline()) sb.Append("\\ninline");

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	if(showTypes_ == false) return;

	int index;
	if(typeIndex_.TryGetValue((size_t)declaration->DeclarationType(), &index)) {
		MakeLink(n, index, "Type");
	}
	else {
		declaration->DeclarationType()->Accept(this);
		MakeLink(n, n + 1, "Type");
	}

    int paramNode = nodes_;
    MakeExpression(string::Format(L"Params: %d", declaration->ParameterCount()), "darkolivegreen2");
    MakeLink(n, paramNode, "");

    for(int i = 0; i < declaration->ParameterCount(); i++) {
        int previous = nodes_;
        auto parameter = declaration->Parameters()[i];
        parameter->Accept(this);	
        MakeLink(paramNode, previous, string::Format(L"%d", i));
    }

	if(declaration->Body() && expandFunct_) {
		int previous = nodes_;
		declaration->Body()->Accept(this);
		MakeLink(n, previous, "Body");
	}

	if(declaration->Previous()) {
		int index;
		if(declIndex_.TryGetValue((size_t)declaration->Previous(), &index)) {
			MakeLink(n, index, "Previous");
		}
		else {
			int previous = nodes_;
			declaration->Previous()->Accept(this);
			MakeLink(n, previous, "Previous");
		}
	}

	PrintAttributes(declaration, n);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const EnumConstDeclaration* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb("EnumConst");
	if(declaration->Name()) sb.Append("\\n\\\"" + declaration->Name()->Name() + "\\\"");

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	declaration->ValueExpr()->Accept(this);
	MakeLink(n, n + 1, "Value");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const EnumDeclaration* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb("Enum");
	if(declaration->Name()) sb.Append("\\n\\\"" + declaration->Name()->Name() + "\\\"");

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	if(showTypes_ == false) return;

	declaration->DeclarationType()->Accept(this);
	MakeLink(n, n + 1, "Type");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const FieldDeclaration* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb("Field");

	if(declaration->Name()) sb.Append("\\n\\\"" + declaration->Name()->Name() + "\\\"");
	if(declaration->IsBitfield()) {
		sb.AppendFormat(L" : %d", declaration->Bitfield());
	}

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	if(showTypes_ == false) return;

	int index;
	if(typeIndex_.TryGetValue((size_t)declaration->DeclarationType(), &index)) {
		MakeLink(n, index, "Type");
	}
	else {
		declaration->DeclarationType()->Accept(this);
		MakeLink(n, n + 1, "Type");
	}

	PrintAttributes(declaration, n);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const StructDeclaration* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb("Struct");
	if(declaration->Name()) sb.Append("\\n\\\"" + declaration->Name()->Name() + "\\\"");

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	if(declaration->Previous()) {
		int index;
		if(declIndex_.TryGetValue((size_t)declaration->Previous(), &index)) {
			MakeLink(n, index, "Previous");
		}
		else {
			int previous = nodes_;
			declaration->Previous()->Accept(this);
			MakeLink(n, previous, "Previous");
		}
	}

	if(showTypes_ == false) return;

	int index;
	if(typeIndex_.TryGetValue((size_t)declaration->DeclarationType(), &index)) {
		MakeLink(n, index, "Type");
	}
	else {
		declaration->DeclarationType()->Accept(this);
		MakeLink(n, n + 1, "Type");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const UnionDeclaration* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb("Union");
	if(declaration->Name()) sb.Append("\\n\\\"" + declaration->Name()->Name() + "\\\"");

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	if(declaration->Previous()) {
		int index;
		if(declIndex_.TryGetValue((size_t)declaration->Previous(), &index)) {
			MakeLink(n, index, "Previous");
		}
		else {
			int previous = nodes_;
			declaration->Previous()->Accept(this);
			MakeLink(n, previous, "Previous");
		}
	}

	if(showTypes_ == false) return;

	int index;
	if(typeIndex_.TryGetValue((size_t)declaration->DeclarationType(), &index)) {
		MakeLink(n, index, "Type");
	}
	else {
		declaration->DeclarationType()->Accept(this);
		MakeLink(n, n + 1, "Type");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const TypedefDeclaration* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb("Typedef");
	sb.Append("\\n\\\"" + declaration->Name()->Name() + "\\\"");

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	if(declaration->Previous()) {
		int index;
		if(declIndex_.TryGetValue((size_t)declaration->Previous(), &index)) {
			MakeLink(n, index, "Previous");
		}
		else {
			int previous = nodes_;
			declaration->Previous()->Accept(this);
			MakeLink(n, previous, "Previous");
		}
	}

	if(showTypes_ == false) return;

	int index;
	if(typeIndex_.TryGetValue((size_t)declaration->DeclarationType(), &index)) {
		MakeLink(n, index, "Type");
	}
	else {
		declaration->DeclarationType()->Accept(this);
		MakeLink(n, n + 1, "Type");
	}

	PrintAttributes(declaration, n);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const DeclarationList* declaration) {
	declIndex_.Add((size_t)declaration, nodes_);
	StringBuilder sb(string::Format(L"DeclarationList: %d", declaration->Count()));

	int n = nodes_;
	MakeDeclaration(sb.ToString());

	for(int i = 0; i < declaration->Count(); i++) {
		int previous = nodes_;
		BeginDeclarationCluster();
		auto parameter = declaration->Declarations()[i];
		parameter->Accept(this);
		EndCluster();
		MakeLink(n, previous, string::Format(L"%d", i));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const UnaryOperator* expr) {
	StringBuilder sb;
	
	switch(expr->Operator()) {
		case UnaryOpType::Inc: { 
			if(expr->IsPostfix()) sb.Append("E++");
			else sb.Append("++E");
			break;
		}
		case UnaryOpType::Dec: {
			if(expr->IsPostfix()) sb.Append("E--");
			else sb.Append("--E");
			break;
		}
		case UnaryOpType::Address:     { sb.Append("&");  break; }
		case UnaryOpType::Indirection: { sb.Append("*");  break; }
		case UnaryOpType::Add:         { sb.Append("+");  break; }
		case UnaryOpType::Sub:         { sb.Append("-");  break; }
		case UnaryOpType::Complement:        { sb.Append("~");  break; }
		case UnaryOpType::Not:         { sb.Append("!");  break; }
	}
	
	int n = nodes_;
	MakeUnaryOperator(sb.ToString());
	expr->Value()->Accept(this);
	MakeLink(n, n + 1, "Expr");

	if(showTypes_ == false) return;

	if((expr->Operator() == UnaryOpType::Address) ||
	   (expr->Operator() == UnaryOpType::Indirection)) {
		int index;
		if(!expr->ResultType()->IsBasic() &&
			!(expr->ResultType()->IsPointer() && expr->ResultType()->As<PointerType>()->IsBasic()) &&
			typeIndex_.TryGetValue((size_t)expr->ResultType(), &index)) {
			MakeLink(n, index, "Result", true /* dashed */);
		}
		else {
			int previous = nodes_;
			expr->ResultType()->Accept(this);
			MakeLink(n, previous, "Result", true /* dashed */);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const BinaryOperator* expr) {
	StringBuilder sb;
	
	switch(expr->Operator()) {
		case BinaryOpType::Add:       { sb.Append("+");   break; }
		case BinaryOpType::Sub:       { sb.Append("-");   break; }
		case BinaryOpType::Mul:       { sb.Append("*");   break; }
		case BinaryOpType::Div:       { sb.Append("/");   break; }
		case BinaryOpType::Mod:       { sb.Append("%");   break; }
		case BinaryOpType::Eq:        { sb.Append("=");   break; }
		case BinaryOpType::AddEq:     { sb.Append("+=");  break; }
		case BinaryOpType::SubEq:     { sb.Append("-=");  break; }
		case BinaryOpType::MulEq:     { sb.Append("*=");  break; }
		case BinaryOpType::ModEq:     { sb.Append("%=");  break; }
		case BinaryOpType::DivEq:     { sb.Append("/=");  break; }
		case BinaryOpType::AndEq:     { sb.Append("&=");  break; }
		case BinaryOpType::OrEq:      { sb.Append("|=");  break; }
		case BinaryOpType::ShiftLEq:  { sb.Append("<<="); break; }
		case BinaryOpType::ShiftREq:  { sb.Append(">>="); break; }
		case BinaryOpType::XorEq:     { sb.Append("^=");  break; }
		case BinaryOpType::EqEq:      { sb.Append("==");  break; }
		case BinaryOpType::NotEq:     { sb.Append("!=");  break; }
		case BinaryOpType::AndAnd:    { sb.Append("&&");  break; }
		case BinaryOpType::OrOr:      { sb.Append("||");  break; }
		case BinaryOpType::And:       { sb.Append("&");   break; }
		case BinaryOpType::Or:        { sb.Append("|");   break; }
		case BinaryOpType::Xor:       { sb.Append("^");   break; }
		case BinaryOpType::ShiftR:    { sb.Append(">>");  break; }
		case BinaryOpType::ShiftL:    { sb.Append("<<");  break; }
		case BinaryOpType::Less:      { sb.Append("<");   break; }
		case BinaryOpType::LessEq:    { sb.Append("<=");  break; }
		case BinaryOpType::Greater:   { sb.Append(">");   break; }
		case BinaryOpType::GreaterEq: { sb.Append(">=");  break; }
		case BinaryOpType::Comma:     { sb.Append(",");   break; }
	}
	
	int n = nodes_;
	MakeOperator(sb.ToString());
	expr->LeftValue()->Accept(this);
	MakeLink(n, n + 1, "Left");
	
	int previous = nodes_;
	expr->RightValue()->Accept(this);
	MakeLink(n, previous, "Right");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const NumberConstant* expr) {
	StringBuilder sb("Number");
	
	if(expr->IsFloating()) {
		sb.AppendFormat(L"\\nfloat, %.2f", expr->Value().FloatValue);
	}
	else {
		sb.AppendFormat(L"\\nint, %d", expr->Value().IntValue);
	}

	MakeConstant(sb.ToString());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const CharConstant* expr) {
	StringBuilder sb(expr->ResultType()->As<BasicType>()->IsUChar() ? "UChar" : "Char");

	if(expr->IsWide()) sb.Append("\\nwide");
	if(expr->Value().Value == 0) {
		sb.Append(string("\\n0"));
	}
	else {
		sb.Append(string("\\n\'") + expr->Value().Value + "\'");
	}

	MakeConstant(sb.ToString());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const StringConstant* expr) {
	StringBuilder sb("String");

	if(expr->IsWide()) sb.Append("\nwide");
	sb.Append("\\n\\\"" + expr->Value().Value.ToString() + "\\\"");
	MakeConstant(sb.ToString());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const SubscriptExpression* expr) {
	StringBuilder sb("Subscript");
	
	BeginExpressionCluster();
	int n = nodes_;
	MakeExpression(sb.ToString());
	expr->Base()->Accept(this);
	MakeLink(n, n + 1, "Base");

	int previous = nodes_;
	expr->Index()->Accept(this);
	MakeLink(n, previous, "Index");

	if(showTypes_ == false) return;

	previous = nodes_;
	expr->ResultType()->Accept(this);
	MakeLink(n, previous, "Result", true /* dashed */);
	EndCluster();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const MemberExpression* expr) {
	StringBuilder sb("Member");
	
	BeginExpressionCluster();
	int n = nodes_;
	MakeExpression(sb.ToString());
	expr->Object()->Accept(this);
	MakeLink(n, n + 1, "Object");

	int previous = nodes_;
	MakeExpression("\\\"" + expr->Name()->Name() + "\\\"", "thistle");
	MakeLink(n, previous, "Name");

	if(showTypes_ == false) return;

	previous = nodes_;
	expr->ResultType()->Accept(this);
	MakeLink(n, previous, "Result", true /* dashed */);
	EndCluster();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const SizeofOperator* expr) {
	StringBuilder sb("Sizeof");
	
	int n = nodes_;
	MakeExpression(sb.ToString());
	expr->Target()->Accept(this);
	MakeLink(n, n + 1, "Target");

	if(showTypes_ == false) return;

	int previous = nodes_;
	expr->ResultType()->Accept(this);
	MakeLink(n, previous, "Result", true /* dashed */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const CallExpression* expr) {
	StringBuilder sb("Call");
	
	BeginCallCluster();
	int n = nodes_;
	MakeExpression(sb.ToString());
	expr->Function()->Accept(this);
	MakeLink(n, n + 1, "Function");

	int argNode = nodes_;
	MakeExpression(string::Format(L"Arguments: %d", expr->ArgCount()), "lavender");
	MakeLink(n, argNode, "");

	for(int i = 0; i < expr->ArgCount(); i++) {
		int previous = nodes_;
		auto parameter = expr->Arguments()[i];
		parameter->Accept(this);
		MakeLink(argNode, previous, string::Format(L"%d", i));
	}
	
	if(showTypes_ == false) return;

	int previous = nodes_;
	expr->ResultType()->Accept(this);
	MakeLink(n, previous, "Result", true /* dashed */);
	EndCluster();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const ConditionalOperator* expr) {
	StringBuilder sb("? :");
	
	int n = nodes_;
	MakeOperator(sb.ToString());
	expr->Condition()->Accept(this);
	MakeLink(n, n + 1, "Condition");

	int previous = nodes_;
	expr->Left()->Accept(this);
	MakeLink(n, previous, "Left");

	previous = nodes_;
	expr->Right()->Accept(this);
	MakeLink(n, previous, "Right");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const CastExpression* expr) {
	StringBuilder sb;

	switch(expr->Type()) {
		case CastType::IntToFloat:	  { sb.Append("IntToFloat");    break; }
		case CastType::FloatToInt:	  { sb.Append("FloatToInt");    break; }
		case CastType::IntToPointer:  { sb.Append("IntToPointer");  break; }
		case CastType::PointerToInt:  { sb.Append("PointerToInt");  break; }
		case CastType::ToVoid:		  { sb.Append("ToVoid");        break; }
		case CastType::IntToInt:	  { sb.Append("IntToInt");      break; }
		case CastType::FloatToFloat:  { sb.Append("FloatToFloat");  break; }
		case CastType::ArrayToPtr:	  { sb.Append("ArrayToPtr");    break; }
		case CastType::FunctionToPtr: { sb.Append("FunctionToPtr"); break; }
		case CastType::RemoveQual:	  { sb.Append("RemoveQual");    break; }
		case CastType::ToPointer:	  { sb.Append("ToPointer");     break; }
		case CastType::Unknown:		  { sb.Append("Unknown");       break; }
	}

	if(expr->IsExplicit()) sb.Append("\\n(explicit)");

	int n = nodes_;
	//BeginCastCluster();
	MakeCast(sb.ToString());

	expr->Target()->Accept(this);
	MakeLink(n, n + 1, "Target");

	if(showTypes_ == false) {
		EndCluster();
		return;
	}

	int index;
	if(!expr->ResultType()->IsBasic() &&
		!(expr->ResultType()->IsPointer() && expr->ResultType()->As<PointerType>()->IsBasic()) &&
		typeIndex_.TryGetValue((size_t)expr->ResultType(), &index)) {
		MakeLink(n, index, "Result", true /* dashed */);
	}
	else {
		int previous = nodes_;
		expr->ResultType()->Accept(this);
		MakeLink(n, previous, "Result", true /* dashed */);
	}

	//EndCluster();
	
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const InitializerListExpression* expr) {
	StringBuilder sb("InitList");

	int n = nodes_;
	MakeExpression(sb.ToString());

	for(int i = 0; i < expr->InitList().Count(); i++) {
		int previous = nodes_;
		auto parameter = expr->InitList()[i];
		parameter->Accept(this);
		MakeLink(n, previous, string::Format(L"%d", i));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const CompoundExpression* expr) {
	StringBuilder sb("Compound");

	int n = nodes_;
	MakeExpression(sb.ToString());
	expr->InitList()->Accept(this);
	MakeLink(n, n + 1, "Initializer");

	if(showTypes_ == false) return;

	int index;
	if(!expr->ResultType()->IsBasic() && !expr->ResultType()->IsPointer() &&
		typeIndex_.TryGetValue((size_t)expr->ResultType(), &index)) {
		MakeLink(n, index, "Type", true /* dashed */);
	}
	else {
		int previous = nodes_;
		expr->ResultType()->Accept(this);
		MakeLink(n, previous, "Type", true /* dashed */);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const DeclarationExpression* expr) {
	StringBuilder sb("Decl");

	int n = nodes_;
	MakeReference(sb.ToString());

	int declaration;
	if(declIndex_.TryGetValue((size_t)expr->Object(), &declaration)) {
		MakeLink(n, declaration, "Decl");
	}
	else {
		int previous = nodes_;
		expr->Object()->Accept(this);
		MakeLink(n, previous, "Decl");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const InvalidExpression* expr) {
	int n = nodes_;
	MakeExpression("InvalidExpression");

	if(showTypes_ == false) return;

	if(expr->ResultType()) {
		int index;
		if(!expr->ResultType()->IsBasic() &&
			!(expr->ResultType()->IsPointer() && expr->ResultType()->As<PointerType>()->IsBasic()) &&
			typeIndex_.TryGetValue((size_t)expr->ResultType(), &index)) {
				MakeLink(n, index, "Result", true /* dashed */);
		}
		else {
			int previous = nodes_;
			expr->ResultType()->Accept(this);
			MakeLink(n, previous, "Result", true /* dashed */);
		}
	}

}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const NullExpression* expr) {
	int n = nodes_;
	MakeExpression("NullExpression");

	if(showTypes_ == false) return;

	if(expr->ResultType()) {
		int index;
		if(!expr->ResultType()->IsBasic() &&
			!(expr->ResultType()->IsPointer() && expr->ResultType()->As<PointerType>()->IsBasic()) &&
			typeIndex_.TryGetValue((size_t)expr->ResultType(), &index)) {
			MakeLink(n, index, "Result", true /* dashed */);
		}
		else {
			int previous = nodes_;
			expr->ResultType()->Accept(this);
			MakeLink(n, previous, "Result", true /* dashed */);
		}
	}

}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const IfStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("If");

	int n = nodes_;
	MakeStatement(sb.ToString());
	statement->Condition()->Accept(this);
	MakeLink(n, n + 1, "Condition");

	int previous = nodes_;
	if(statement->True()) {
		statement->True()->Accept(this);
		MakeLink(n, previous, "True");
	}

	if(statement->False()) {
		previous = nodes_;
		statement->False()->Accept(this);
		MakeLink(n, previous, "False");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const ForStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("For");

	int n = nodes_;
	MakeLoop(sb.ToString());

	if(statement->Init()) {
		statement->Init()->Accept(this);
		MakeLink(n, n + 1, "Init");
	}

	int previous;

	if(statement->Condition()) {
		previous = nodes_;
		statement->Condition()->Accept(this);
		MakeLink(n, previous, "Condition");
	}

	if(statement->Increment()) {
		previous = nodes_;
		statement->Increment()->Accept(this);
		MakeLink(n, previous, "Increment");
	}

	previous = nodes_;
	statement->Body()->Accept(this);
	MakeLink(n, previous, "Body");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const WhileStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("While");

	int n = nodes_;
	MakeLoop(sb.ToString());
	statement->Condition()->Accept(this);
	MakeLink(n, n + 1, "Condition");

	int previous = nodes_;
	statement->Body()->Accept(this);
	MakeLink(n, previous, "Body");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const DoStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("Do");

	int n = nodes_;
	MakeLoop(sb.ToString());
	statement->Condition()->Accept(this);
	MakeLink(n, n + 1, "Condition");

	int previous = nodes_;
	statement->Body()->Accept(this);
	MakeLink(n, previous, "Body");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const ContinueStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	MakeStatement("ContinueStatement");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const BreakStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	MakeStatement("BreakStatement");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const ReturnStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("Return");

	int n = nodes_;
	MakeStatement(sb.ToString());

	if(statement->Value()) {
		statement->Value()->Accept(this);
		MakeLink(n, n + 1, "Value");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const LabelStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("Label: ");
	sb.Append(statement->Name()->Name());

	int n = nodes_;
	MakeStatement(sb.ToString());
	statement->Target()->Accept(this);
	MakeLink(n, n + 1, "Target");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const CaseStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb(statement->IsDefault() ? "Default" : "Case");

	int n = nodes_;
	MakeStatement(sb.ToString());

	if(statement->Value()) {
		statement->Value()->Accept(this);
		MakeLink(n, n + 1, "Value");
	}	

	Statement* target = statement->Target();
	while(target->IsCaseStatement()) {
		target = target->As<CaseStatement>()->Target();
	}

	int index;
	if(statementIndex_.TryGetValue((size_t)target, &index)) {
		MakeLink(n, index, "Target");
	}
	else {
		int previous = nodes_;
		statement->Target()->Accept(this);
		MakeLink(n, previous, "Target");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const GotoStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);

	int n = nodes_;
	MakeStatement("Goto");

	Statement* target = statement->Target();
	int index;

	if(statementIndex_.TryGetValue((size_t)target, &index)) {
		MakeLink(n, index, "Target");
	}
	else {
		int previous = nodes_;
		statement->Target()->Accept(this);
		MakeLink(n, previous, "Target");
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const SwitchStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("Switch");

	int n = nodes_;
	MakeStatement(sb.ToString());

	statement->Condition()->Accept(this);
	MakeLink(n, n + 1, "Condition");

	for(int i = 0; i < statement->CaseList().Count(); i++) {
		int previous = nodes_;
		auto parameter = statement->CaseList()[i];
		BeginStatementCluster();
		parameter->Accept(this);
		EndCluster();
		MakeLink(n, previous, string::Format(L"%d", i));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const CompoundStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb(string::Format(L"Compound: %d", statement->Children().Count()));

	int n = nodes_;
	MakeStatement(sb.ToString());

	for(int i = 0; i < statement->Children().Count(); i++) {
		int previous = nodes_;
		auto parameter = statement->Children()[i];
		parameter->Accept(this);
		MakeLink(n, previous, string::Format(L"%d", i));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const NullStatement* statement) {
	MakeNullStatement("NullStatement", statement->IsError());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const DeclarationStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("Decl");

	int n = nodes_;
	MakeStatement(sb.ToString());
	statement->Base()->Accept(this);
	MakeLink(n, n + 1, "Decl");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ASTDotPrinter::Visit(const ExpressionStatement* statement) {
	statementIndex_.Add((size_t)statement, nodes_);
	StringBuilder sb("Expr");

	int n = nodes_;
	MakeStatement(sb.ToString());
	statement->Base()->Accept(this);
	MakeLink(n, n + 1, "Expr");
}

} // namespace AST