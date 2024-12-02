// TypeString.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'TypeString' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "TypeString.hpp"

namespace AST {

TypeString::TypeString(const Type* startType) : 
		printElemType_(true) {
	DebugValidator::IsNotNull(startType);
	startType->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const BasicType* type) {
	if(type == nullptr) return;

	if(type->IsBool())           sb_.Append("bool");
	else if(type->IsChar())      sb_.Append("char");
	else if(type->IsUChar())     sb_.Append("unsigned char");
	else if(type->IsWChar())     sb_.Append("wchar_t");
	else if(type->IsShort())     sb_.Append("short");
	else if(type->IsUShort())    sb_.Append("unsigned short");
	else if(type->IsInt())       sb_.Append("int");
	else if(type->IsUInt())      sb_.Append("unsigned");
	else if(type->IsLong())      sb_.Append("long"); 
	else if(type->IsULong())     sb_.Append("unsigned long"); 
	else if(type->IsLongLong())  sb_.Append("long long"); 
	else if(type->IsULongLong()) sb_.Append("unsigned long long");
	else if(type->IsFloat())     sb_.Append("float"); 
	else if(type->IsDouble())    sb_.Append("double");
	else if(type->IsVoid())      sb_.Append("void"); 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const QType* type) {
	if(type == nullptr) return;

	if(type->HasRestrict()) sb_.Append("restrict ");
	if(type->HasConst())    sb_.Append("const ");
	if(type->HasVolatile()) sb_.Append("volatile ");

	type->Base()->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const PointerType* type) {
	if(type == nullptr) return;

	// First print the pointee, then append the *
	// Pointer -> Int = int*
	// For pointers to arrays and function print the array/function element/return type
	// first, than (*) and the array/function info.
	// Pointer -> Array(2) -> int = int (*)[2]
	const Type* pointee = type->PointeeType();
	
	if(auto arrayType = pointee->As<ArrayType>()) {
		const ArrayType* innerType = arrayType;

		while(innerType->ElementType()->IsArray()) {
			// To handle things like int a[2][3][4].
			innerType = innerType->ElementType()->As<ArrayType>();
		}

		innerType->ElementType()->Accept(this);
		sb_.Append(" (*)");
		printElemType_ = false;
		arrayType->Accept(this);
		printElemType_ = true;
	}
	else if(auto functionType = pointee->As<FunctionType>()) {
		functionType->ReturnType()->Accept(this);
		sb_.Append(" (*)");
		printElemType_ = false;
		functionType->Accept(this);
		printElemType_ = true;
	}
	else {
		type->PointeeType()->Accept(this);
		sb_.Append("*");
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const ArrayType* type) {
	if(type == nullptr) return;

	// First print the element type, than the array info.
	// ArrayType(5) -> Int = int [5]
	// If the element type is an array print the innermost element type first.
	if(printElemType_) {
		if(auto arrayType = type->ElementType()->As<ArrayType>()) {
			const ArrayType* innerType = arrayType;
		
			while(innerType->ElementType()->IsArray()) {
				// To handle things like int a[2][3][4].
				innerType = innerType->ElementType()->As<ArrayType>();
			}

			innerType->ElementType()->Accept(this);
			sb_.Append(" ");
			printElemType_ = false;
		}
		else {
			type->ElementType()->Accept(this);
		}
	}

	bool spaceNeeded = false;
	sb_.Append("[");

	if(type->IsStatic()) {
		sb_.Append("static");
		spaceNeeded = true;
	}

	if(type->Qualifiers().HasNone() == false) {
		if(spaceNeeded) sb_.Append(" ");
		spaceNeeded = true;

		if(type->Qualifiers().HasRestrict()) sb_.Append("restrict");
		if(type->Qualifiers().HasConst())    sb_.Append("const");
		if(type->Qualifiers().HasVolatile()) sb_.Append("volatile");
	}

	if(type->IsIncomplete() == false) {
		if(spaceNeeded) sb_.Append(" ");
		sb_.AppendFormat(L"%d", type->Size());
	}

	sb_.Append("]");

	if((printElemType_ == false) && type->ElementType()->IsArray()) {
		type->ElementType()->Accept(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const VarArrayType* type) {
	if(type == nullptr) return;

	// First print the element type, than the array info.
	// VarArrayType(5 + a *  b) -> Int = int [5 + a * b]
	if(printElemType_) {
		type->ElementType()->Accept(this);
	}

	printElemType_ = true;
	bool spaceNeeded = false;
	sb_.Append("[");

	if(type->IsStatic()) {
		sb_.Append("static");
		spaceNeeded = true;
	}

	if(type->Qualifiers().HasNone() == false) {
		if(spaceNeeded) sb_.Append(" ");
		spaceNeeded = true;

		if(type->Qualifiers().HasRestrict()) sb_.Append("restrict");
		if(type->Qualifiers().HasConst())    sb_.Append("const");
		if(type->Qualifiers().HasVolatile()) sb_.Append("volatile");
	}

	if(spaceNeeded) sb_.Append(" ");

	// If there's no expression this is [*].
	if(type->SizeExpression() == nullptr) {
		sb_.Append("*");
	}
	else {
		type->SizeExpression()->Accept(this);
	}

	sb_.Append("]");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const FunctionType* type) {
	if(type == nullptr) return;

	if(printElemType_) {
		type->ReturnType()->Accept(this);
		sb_.Append(" ");
	}

	auto& parameters = type->Parameters();
	sb_.Append("(");

	for(int i = 0; i < parameters.Count(); i++) {
		parameters[i]->Accept(this);
		if(i != (parameters.Count() - 1)) sb_.Append(", ");
	}

	if(type->IsVarargs()) {
		if(parameters.Count() > 0) sb_.Append(", ..."); // (a, b, c, ...)
		else sb_.Append("..."); // (...)
	}

	sb_.Append(")");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const EnumType* type) {
	if(type == nullptr) return;

	sb_.Append("enum");
	if(type->ParentDeclaration()) {
		sb_.Append(" " + type->ParentDeclaration()->Name()->Name());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const StructType* type) {
	if(type == nullptr) return;

	sb_.Append("struct");

	if(type->ParentDeclaration() && type->ParentDeclaration()->Name()) {
		sb_.Append(" " + type->ParentDeclaration()->Name()->Name());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const UnionType* type) {
	if(type == nullptr) return;

	sb_.Append("union");

	if(type->ParentDeclaration()  && type->ParentDeclaration()->Name()) {
		sb_.Append(" " + type->ParentDeclaration()->Name()->Name());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const TypedefType* type) {
	if(type == nullptr) return;

	sb_.Append("typedef ");
	type->Parent()->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const UnaryOperator* expr) {
	if(expr == nullptr) return;

	bool targetPrinted = false;

	switch(expr->Operator()) {
		case UnaryOpType::Inc: { 
			if(expr->IsPostfix()) {
				expr->Value()->Accept(this);
				targetPrinted = true;
			}

			sb_.Append("++");
			break;
		}
		case UnaryOpType::Dec: {
			if(expr->IsPostfix()) {
				expr->Value()->Accept(this);
				targetPrinted = true;
			}

			sb_.Append("--");
			break;
		}
		case UnaryOpType::Address:     { sb_.Append("&");  break; }
		case UnaryOpType::Indirection: { sb_.Append("*");  break; }
		case UnaryOpType::Add:         { sb_.Append("+");  break; }
		case UnaryOpType::Sub:         { sb_.Append("-");  break; }
		case UnaryOpType::Complement:        { sb_.Append("~");  break; }
		case UnaryOpType::Not:         { sb_.Append("!");  break; }
	}

	if(targetPrinted == false) {
		expr->Value()->Accept(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const BinaryOperator* expr) {
	if(expr == nullptr) return;

	if(auto left = expr->LeftValue()->As<BinaryOperator>()) {
		// If the left part is an operator with lower precedence it must
		// be surrounded by parenthesis.
		if(left->Precedence() > expr->Precedence()) {
			sb_.Append("(");
			left->Accept(this);
			sb_.Append(")");
		}
	}
	else expr->LeftValue()->Accept(this);

	switch(expr->Operator()) {
		case BinaryOpType::Add:       { sb_.Append(" + ");   break; }
		case BinaryOpType::Sub:       { sb_.Append(" - ");   break; }
		case BinaryOpType::Mul:       { sb_.Append(" * ");   break; }
		case BinaryOpType::Div:       { sb_.Append(" / ");   break; }
		case BinaryOpType::Mod:       { sb_.Append(" % ");   break; }
		case BinaryOpType::Eq:        { sb_.Append(" = ");   break; }
		case BinaryOpType::AddEq:     { sb_.Append(" += ");  break; }
		case BinaryOpType::SubEq:     { sb_.Append(" -= ");  break; }
		case BinaryOpType::MulEq:     { sb_.Append(" *= ");  break; }
		case BinaryOpType::ModEq:     { sb_.Append(" %= ");  break; }
		case BinaryOpType::DivEq:     { sb_.Append(" /= ");  break; }
		case BinaryOpType::AndEq:     { sb_.Append(" &= ");  break; }
		case BinaryOpType::OrEq:      { sb_.Append(" |= ");  break; }
		case BinaryOpType::ShiftLEq:  { sb_.Append(" <<= "); break; }
		case BinaryOpType::ShiftREq:  { sb_.Append(" >>= "); break; }
		case BinaryOpType::XorEq:     { sb_.Append(" ^= ");  break; }
		case BinaryOpType::EqEq:      { sb_.Append(" == ");  break; }
		case BinaryOpType::NotEq:     { sb_.Append(" != ");  break; }
		case BinaryOpType::AndAnd:    { sb_.Append(" && ");  break; }
		case BinaryOpType::OrOr:      { sb_.Append(" || ");  break; }
		case BinaryOpType::And:       { sb_.Append(" & ");   break; }
		case BinaryOpType::Or:        { sb_.Append(" | ");   break; }
		case BinaryOpType::Xor:       { sb_.Append(" ^ ");   break; }
		case BinaryOpType::ShiftR:    { sb_.Append(" >> ");  break; }
		case BinaryOpType::ShiftL:    { sb_.Append(" << ");  break; }
		case BinaryOpType::Less:      { sb_.Append(" < ");   break; }
		case BinaryOpType::LessEq:    { sb_.Append(" <= ");  break; }
		case BinaryOpType::Greater:   { sb_.Append(" > ");   break; }
		case BinaryOpType::GreaterEq: { sb_.Append(" >= ");  break; }
		case BinaryOpType::Comma:     { sb_.Append(", ");    break; }
	}

	if(auto right = expr->RightValue()->As<BinaryOperator>()) {
		if(right->Precedence() > expr->Precedence()) {
			sb_.Append("(");
			right->Accept(this);
			sb_.Append(")");
		}
	}
	else expr->RightValue()->Accept(this);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const NumberConstant* expr) {
	if(expr == nullptr) return;

	if(expr->IsFloating()){
		sb_.AppendFormat(L"%3f", expr->Value().FloatValue);
	}
	else sb_.AppendFormat(L"%d", expr->Value().IntValue);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const CharConstant* expr) {
	if(expr == nullptr) return;

	if(expr->Value().IsWide) {
		sb_.Append("L");
	}

	sb_.Append("\'");
	sb_.Append(expr->Value().Value);
	sb_.Append("\'");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const StringConstant* expr) {
	if(expr == nullptr) return;

	if(expr->Value().IsWide) {
		sb_.Append("L");
	}

	sb_.Append("\"");
	sb_.Append(expr->Value().Value.ToString());
	sb_.Append("\"");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const SizeofOperator* expr) {
	if(expr == nullptr) return;

	sb_.Append("sizeof(");
	expr->Target()->Accept(this);
	sb_.Append(")");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const SubscriptExpression* expr) {
	if(expr == nullptr) return;

	expr->Base()->Accept(this);
	sb_.Append("[");
	expr->Index()->Accept(this);
	sb_.Append("]");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const MemberExpression* expr) {
	if(expr == nullptr) return;

	expr->Object()->Accept(this);

	if(expr->IsPointer()) {
		sb_.Append("->");
	}
	else sb_.Append(".");

	sb_.Append(expr->Name()->Name());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const CallExpression* expr) {
	if(expr == nullptr) return;

	expr->Function()->Accept(this);
	sb_.Append("(");

	auto& arguments = expr->Arguments();

	for(int i = 0; i < arguments.Count(); i++) {
		arguments[i]->Accept(this);

		if(i != (arguments.Count() - 1)) {
			sb_.Append(", ");
		}
	}

	sb_.Append(")");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const CastExpression* expr) {
	if(expr == nullptr) return;

	sb_.Append("(");
	expr->ResultType()->Accept(this);
	sb_.Append(")");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const ConditionalOperator* expr) {
	if(expr == nullptr) return;

	expr->Condition()->Accept(this);
	sb_.Append(" ? ");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const DeclarationExpression* expr) {
	if(expr == nullptr) return;

	if(expr->Object()->Name()) {
		sb_.Append(expr->Object()->Name()->Name());
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const InitializerListExpression* expr) {
	if(expr == nullptr) return;

	sb_.Append("{");
	auto& list = expr->InitList();

	for(int i = 0; i < list.Count(); i++) {
		list[i]->Accept(this);

		if(i != (list.Count() - 1)) {
			sb_.Append(", ");
		}
	}

	sb_.Append("}");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const InvalidExpression* expr) {
	if(expr == nullptr) return;
	sb_.Append("*ERROR*");
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void TypeString::Visit(const CompoundExpression* expr) {
	if(expr == nullptr) return;
	
	sb_.Append("(");
	expr->ResultType()->Accept(this);
	sb_.Append(")");
	expr->InitList()->Accept(this);
}

} // namespace AST