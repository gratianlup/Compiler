// IRParser.cpp
// Copyright (c) Lup Gratian
//
// Implements the IR parser.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IRParser.hpp"

namespace IR {

IRParser::IRParser(Lexer* lexer, ParserErrorHandler* handler, TypeTable* types) :
				   lexer_(lexer), handler_(handler), types_(types),
				   currentFunct_(nullptr), currentBlock_(nullptr), 
                   inFunctProto_(false) {
	// Make # a custom character, so we get a token when it's found.
	lexer_->SetCustomChar(L'#');
	lexer_->SetHasCustomChar(true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
KeywordType IRParser::Kwd() {
	if(current_.IsKeyword()) {
		return current_.AsKeyword<KeywordType>();
	}
	else return KeywordType::None;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRParser::EatToken() {
	lexer_->NextToken(current_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRParser::SkipToSafePoint() {
	while(current_.IsEOF() == false) {
		if(Kwd() == KeywordType::Var) break;
		else if(Kwd() == KeywordType::Funct) break;
		else if(Kwd() == KeywordType::Type)  break;
		else if((Kwd() == KeywordType::Label) && (currentFunct_ != nullptr)) break;
		else if(IsCloseCurly() && (currentFunct_ != nullptr)) break;
		else EatToken();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRParser::ExpectAndEat(TokenKind kind, int error) {
	if(current_.Kind() != kind) {
		EmitError(error);
		return false;
	}
	else {
		EatToken();
		return true;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRParser::ParseInteger(__int64& value) {
	if(current_.IsNumber() == false) {
		EmitError(Error::EXPECTED_INTEGER);
		return false;
	}

	NumberInfo info = NumberParser(nullptr).Parse(current_);
	EatToken();

	if(info.IsInteger == false) {
		EmitError(Error::EXPECTED_INTEGER);
		return false;
	}

	value = info.IntValue;
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GlobalVariable* IRParser::GetGlobalVariable(const string& name) {
	GlobalVariable* variable;

	if(globalVars_.TryGetValue(name, &variable)) {
		return variable;
	}

	// A new variable is created.
	variable = GlobalVariable::GetGlobal(nullptr, name, nullptr, &unit_->Symbols());
	unit_->AddVariable(variable);
	globalVars_.Add(name, variable);
	return variable;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* IRParser::GetFunction(const string& name) {
	Function* function;

	if(functions_.TryGetValue(name, &function)) {
		return function;
	}

	// A new variable is created.
	function = Function::GetFunction(nullptr, name, false, unit_);
	unit_->AddFunction(function);
	functions_.Add(name, function);
	return function;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* IRParser::GetBlock(const string& functionName, const string& blockName) {
	Block* block;
	BlockName name(functionName, blockName);

	if(blocks_.TryGetValue(name, &block)) {
		return block;
	}

	// A new variable is created.
	block = Block::GetBlock(blockName);
	blocks_.Add(name, block);
	return block;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRParser::AddToPatch(GlobalVariable* variable, Operand* op) {
	if(globalVarsPatch_.ContainsKey(variable)) {
		globalVarsPatch_[variable].Add(op);
	}
	else {
		List<Operand*> list;
		list.Add(op);
		globalVarsPatch_.Add(variable, list);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRParser::EmitError(int error) {
	ParserError info(error, current_.Location(), current_.ToString());
	handler_->Handle(info);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Type* IRParser::ParseType() {
	// type -> typeSpecifier pointer
	// typeSpecifier -> basicType | arrayType | userType
	// basicType -> int8 | int16 | int32 | int64 | float | double | void
	// arrayType -> [ number type ]
	// userType -> < identifier >
	// pointer -> pointer * | * | Eps
	const Type* type = nullptr;

	if(current_.IsKeyword()) {
		KeywordType keyword = Kwd();
		EatToken();

		switch(keyword) {
			case KeywordType::Int8:   { type = IntegerType::GetInt8(); 	  break; }
			case KeywordType::Int16:  { type = IntegerType::GetInt16();	  break; }
			case KeywordType::Int32:  { type = IntegerType::GetInt32();   break; }
			case KeywordType::Int64:  { type = IntegerType::GetInt64();   break; }
			case KeywordType::Float:  { type = FloatingType::GetFloat();  break; }
			case KeywordType::Double: { type = FloatingType::GetDouble(); break; }
			case KeywordType::Void:   { type = VoidType::GetVoid(); 	  break; }
			default: {
				EmitError(Error::INVALID_TYPE);
				return nullptr; // Error.
			}
		}
	}
	else if(IsOpenSquare()) { // [
		// An array type should begin. First we read the size and validate.
		__int64 size;
		EatToken(); // Skip over [
		
		// ? means that the array size needs to be deducted from the initializer.
		if(current_.Kind() == TokenKind::Question) {
			// This will be caught by the verifier if an initializer is not provided.
			size = -1; 
			EatToken();
		}
		else {
			if(ParseInteger(size) == false) return nullptr;

			if(size <= 0) {
				EmitError(Error::INVALID_ARRAY_SIZE);
				return nullptr;
			}
		}

		// Skip over the size and parse the element type.
		// Then we create the array type.
		const Type* elementType = ParseType();
		if(elementType == nullptr) return nullptr;

		// The array must end with ]
		if(ExpectAndEat(TokenKind::CloseSquare, Error::EXPECTED_CLOSE_SQUARE) == false) {
			return nullptr;
		}
		else type = types_->GetArray(elementType, size);
	}
	else if(IsOpenArrow()) { // <
		// A typename should follow, in most cases. If the first token is
		// 'record' or 'function' parse the specified type.
		EatToken();

		if(Kwd() == KeywordType::Funct) {
			// This should be a function type.
			type = ParseFunction();
		}
		else if(Kwd() == KeywordType::Record) {
			// This should be a record type.
			List<RecordField> fields;
			auto recordType = types_->GetRecord(fields);
			type = ParseRecord(const_cast<RecordType*>(recordType));
		}
		else {
			bool hasSharp = false; // #
			if(current_.Kind() == TokenKind::Custom) {
				hasSharp = true;
				EatToken();
			}

			if(current_.IsIdentifier() == false) {
				EmitError(Error::INVALID_TYPENAME);
				return nullptr;
			}

			// Try to the the type associated with the name.
			string name = current_.NameValue()->Name;
			if(hasSharp) name = "#" + name;
			type = types_->GetNamed(&name);
			EatToken();
		}

		// Give up if no type was found.
		if(type == nullptr) {
			EmitError(Error::TYPENAME_NOT_FOUND);
			return nullptr;
		}

		// This must and with >
		if(ExpectAndEat(TokenKind::Greater, Error::INVALID_TYPENAME) == false) {
			return nullptr;
		}
	}

	if(type == nullptr) {
		// The type could not be determined.
		EmitError(Error::INVALID_TYPE);
		return nullptr;
	}

	// Check if any pointers follow. We parse all * and make a pointer at each step.
	while(IsStar()) {
		EatToken();
		type = types_->GetPointer(type);
	}

	return type;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* IRParser::ParseVariable(bool nameRequired) {
	// globalVariableDecl -> variableDecl initializer
	// variableDecl -> variable identifier type attributeList
	// We parse the variable declaration, and if it's 
    // global we try to parse the initializer.
	bool isGlobal = (currentFunct_ == nullptr) && (inFunctProto_ == false);
	EatToken(); // Skip over 'var'.

	// Parse the identifier. The name must be unique.
	string name;
	bool hasSharp = false; // #

	if(current_.Kind() == TokenKind::Custom) {
		hasSharp = true;
		EatToken();
	}

	if(current_.IsIdentifier() == false) {
		if(nameRequired) EmitError(Error::EXPECTED_IDENTIFIER);
	}
	else { 
		name = current_.NameValue()->Name;
		if(hasSharp) name = "#" + name;
		EatToken();
	}

	// Make sure that the name is not taken (test only for global variables here).
	if(isGlobal && unit_->Symbols().Contains(&name)) {
		EmitError(Error::DUPLICATE_SYMBOL);
		return false;
	}

	// Parse the type of the variable.
	const Type* type = ParseType();
	if(type == nullptr) return nullptr;

	// Parse the optional attribute list.
	AttributeHolder attr;
	if(ParseAttributes(attr) == false) return nullptr;
	
	// Create the variable object and add it to the parent.
	Variable* variable;

	if(isGlobal) {
		// It's a global variable. We must patch all previous references
		// to this variable if it's the case.
		bool isZero = false;
		shared<Initializer> initializer;
		GlobalVariable* globalVar = GetGlobalVariable(name);
		
		globalVar->SetType(type);
		variable = globalVar;

		if(globalVarsPatch_.ContainsKey(globalVar)) {
			auto& patchList = globalVarsPatch_[globalVar];

			for(int i = 0; i < patchList.Count(); i++) {
				patchList[i]->SetType(type);
			}
		}

		// A global variable can be followed by an initializer.
		if(current_.Kind() == TokenKind::Eq) {
			EatToken();

			if(ParseInitializer(type, initializer, isZero) == false) {
				return nullptr;
			}
		}

		// It has an initializer, set it. If the type is an array or record
		// and the initializer is 0 we set the 'zero-initialized' property.
		if(isZero) {
			globalVar->SetHasZeroInitializer(true);
		}
		else {
			globalVar->SetInitializer(initializer);

			// If the size of the array is not known yet set it now.
			// It can be initialized by an array or by a string constant.
			auto arrayType = type->As<ArrayType>();

			if(arrayType && (arrayType->Size() < 0)) {
				if(initializer->IsInitializerList()) {
					// Initialized with array.
					auto initList = initializer.AsStatic<InitializerList>();
					auto realType = types_->GetArray(arrayType->ElementType(),
													 initList->Count());
					globalVar->SetType(realType);
				}
				else if(auto stringConst = initializer->Value()->As<StringConstant>()) {
					auto realType = types_->GetArray(arrayType->ElementType(),
													 stringConst->Value().Length());
					// Replace the old type and create a new initializer.
					globalVar->SetType(realType);
					auto newInit = unit_->Constants().GetString(realType, 
                                                                stringConst->Value());
					globalVar->SetInitializer(Initializer::GetInitializer(newInit));
				}
				else {
					EmitError(Error::INVALID_INITIALIZER);
					return nullptr;
				}
			}
		}
	}
	else {
		// It's a variable local to a function.
        string* varName = name.Length() > 0 ? new string(name) : nullptr;
		variable = Variable::GetVariable(type, varName, currentFunct_);
		
		if(currentFunct_) {
			variable->SetId(currentFunct_->GetNextVariableId());
		}
	}

	// Apply the attributes to the variable.
	int validAttr = 0;

	// Visibility, only for global variables.
	if(isGlobal && attr.HasAttribute(AttributeType::Static)) {
		variable->SetVisibility(SymbolVisibility::Static);
		validAttr++;
	}
	else if(isGlobal && attr.HasAttribute(AttributeType::Extern)) {
		variable->SetVisibility(SymbolVisibility::Extern);
		validAttr++;
	}
	else if(isGlobal && attr.HasAttribute(AttributeType::Tentative)) {
		variable->SetVisibility(SymbolVisibility::Tentative);
		validAttr++;
	}

	// DLL visibility, only for global variables.
	if(isGlobal && attr.HasAttribute(AttributeType::Dllimport)) {
		variable->SetDllVisibility(DllVisibility::Import);
		validAttr++;
	}
	else if(isGlobal && attr.HasAttribute(AttributeType::Dllexport)) {
		variable->SetDllVisibility(DllVisibility::Export);
		validAttr++;
	}

	// Other attributes.
	if(attr.HasAttribute(AttributeType::Restrict)) {
		variable->SetIsRestrict(true);
		validAttr++;
	}

	if(attr.HasAttribute(AttributeType::Align)) {
		variable->SetAlignment((int)attr.Align());
		validAttr++;
	}

    if(attr.HasAttribute(AttributeType::Nowrite)) {
        variable->SetIsNoWrite(true);
		validAttr++;
	}

    if(attr.HasAttribute(AttributeType::Noread)) {
        variable->SetIsNoRead(true);
		validAttr++;
	}

    if(attr.HasAttribute(AttributeType::Noescape)) {
        variable->SetIsNoEscape(true);
		validAttr++;
	}

	// Attributes valid only with global variables.
	if(isGlobal) {
		auto globalVar = variable->As<GlobalVariable>();

		if(attr.HasAttribute(AttributeType::Const)) {
			globalVar->SetIsConstant(true);
			validAttr++;
		}

		if(attr.HasAttribute(AttributeType::Section)) {
			globalVar->SetSection(new string(attr.Section()));
		}
	}

	// Check if there where specified attributes that are not valid.
	if(validAttr < attr.Count()) {
		EmitError(Error::INVALID_ATTRIBUTES);
		return nullptr;
	}

	return variable;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRParser::ParseInitializer(const Type* type, shared<Initializer>& initializer,
                                bool& isZero) {
	// initializer -> = initValue | convertedInitValue |  = { initList } | = 0 | EPS
	// initList -> initList , initValue | initValue
	// convertedInitValue -> ptop ( initValue ) | ptoi ( initValue ) | itop ( initValue )
	// simpleInitValue -> number | string | variableInit | labelInit | initList
	// variableInit -> identifier adjustmentAttribute 
	// labelInit -> label identifier . identifier
	isZero = false;
	bool hasMinus = false;
	InitConversion initConv = InitConversion::None;
	const Type* conversionType = nullptr;
	bool hasSharp = false; // #

	if(current_.Kind() == TokenKind::Custom) {
		hasSharp = true;
		EatToken();
	}

	// This may be a converted value.
	if(Kwd() == KeywordType::Ptop) {
		initConv = InitConversion::PointerToPointer;
		EatToken();
	}
	else if(Kwd() == KeywordType::Ptoi) {
		initConv = InitConversion::PointerToInt;
		EatToken();
	}
	else if(Kwd() == KeywordType::Itop) {
		initConv = InitConversion::IntToPointer;
		EatToken();
	}

	// Skip over ( if this is a converted value.
	if(initConv != InitConversion::None) {
		// Skip over (
		if(ExpectAndEat(TokenKind::OpenParen, Error::EXPECTED_OPEN_PAREN) == false) {
			return false;
		}
	}

	// If we have a -  it means that the next number should be negated.
	if(current_.Kind() == TokenKind::Sub) {
		hasMinus = true;
		EatToken();
	}

	if(current_.IsNumber()) {
		// A single number.
		NumberInfo info = NumberParser(nullptr).Parse(current_);
		EatToken();

		if(info.IsValid == false) {
			EmitError(Error::INVALID_INITIALIZER);
			return false;
		}

		if(info.IsInteger) {
			// Zero applied on an array or record type means 
			// that all the elements should be initialized with the default value 0.
			if(type->IsArray() || type->IsRecord() && (info.IntValue == 0)) {
				isZero = true;
			}
			else {
				__int64 value = info.IntValue;
				if(hasMinus) value = -value;
				auto number = unit_->Constants().GetInt(type, value);
				initializer = Initializer::GetInitializer(number);
			}
		}
		else if(type->IsFloat()) {
			double value = info.FloatValue;
			if(hasMinus) value = -value;
			auto number = unit_->Constants().GetFloat(value);
			initializer = Initializer::GetInitializer(number);
		}
		else {
			double value = info.FloatValue;
			if(hasMinus) value = -value;
			auto number = unit_->Constants().GetDouble(value);
			initializer = Initializer::GetInitializer(number);
		}
	}
	else if(current_.IsString()) {
		// A constant character string.
		StringParser::TStringList list;
		list.Add(current_);
		EatToken();

		// The null-terminator should be provided in the source.
		StringInfo info = StringParser(nullptr, false /* appendTerminator */).Parse(list);
		if(info.IsValid == false) return false;

		// The type of strings is always [N int8].
		auto strType = types_->GetArray(IntegerType::GetInt8(), info.Value.Length());
		auto str = unit_->Constants().GetString(strType, info.Value);
		initializer = Initializer::GetInitializer(str);
	}
	else if(current_.IsIdentifier()) {
		// Reference to a variable.
		string name = current_.NameValue()->Name;
		if(hasSharp) name = "#" + name;
		EatToken();

		GlobalVariable* variable = GetGlobalVariable(name);
		Operand* variableRef = 
                unit_->References().GetGlobalVariableRef(variable, variable->GetType());
		initializer = Initializer::GetInitializer(variableRef);

		// The operand needs to be patched if the variable hasn't been declared yet.
		if(variableRef->GetType() == nullptr) {
			AddToPatch(variable, variableRef);
		}

		// An identifier can be adjusted with a specified number of bytes.
		if(Kwd() == KeywordType::Adjust) {
			__int64 value;
			EatToken();

			if(ExpectAndEat(TokenKind::OpenParen, Error::EXPECTED_OPEN_PAREN) == false) {
				return false;
			}

			if(ParseInteger(value) == false) {
				return false;
			}

			if(ExpectAndEat(TokenKind::CloseParen, Error::EXPECTED_CLOSE_PAREN) == false) {
				return false;
			}

			initializer->SetAdjustment(value);
		}
	}
	else if(Kwd() == KeywordType::Label) {
		// Reference to a block.
		string functionName;
		string blockName;
		EatToken(); // Skip over 'label'

		// Read the two identifiers that form the name of the block.
		if(current_.IsIdentifier() == false) {
			EmitError(Error::EXPECTED_IDENTIFIER);
			return false;
		}

		functionName = current_.NameValue()->Name;
		EatToken();

		if(current_.IsIdentifier() == false) {
			EmitError(Error::EXPECTED_IDENTIFIER);
			return false;
		}

		// A . should be found now.
		if(ExpectAndEat(TokenKind::Dot, Error::EXPECTED_DOT) == false) {
			return false;
		}

		blockName = current_.NameValue()->Name;
		EatToken();

		// Create the block reference.
		Block* block = GetBlock(functionName, blockName);
		auto blockRef = unit_->References().GetBlockRef(block);
		initializer = Initializer::GetInitializer(blockRef);
	}
	else if(Kwd() == KeywordType::Nullptr) {
		EatToken();
		auto null = unit_->Constants().GetNull(type);
		initializer = Initializer::GetInitializer(null);
	}
	else if(Kwd() == KeywordType::Undef) {
		EatToken();
		auto null = unit_->Constants().GetUndefined(type);
		initializer = Initializer::GetInitializer(null);
	}
	else if(IsOpenCurly()) { // {
		// A list of initializers.
		if(ParseInitializerList(type, initializer) == false) {
			return false;
		}

		// } should be found now.
		if(ExpectAndEat(TokenKind::CloseCurly, Error::EXPECTED_CLOSE_CURLY) == false) {
			return false;
		}
	}
	
	// Parse the type to which the value is converted.
	if(initConv != InitConversion::None) {
		// Skip over ,
		if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
			return false;
		}

		conversionType = ParseType();
		if(conversionType == nullptr) {
            return false;
        }

		// Skip over )
		if(ExpectAndEat(TokenKind::CloseParen, Error::EXPECTED_CLOSE_PAREN) == false) {
			return false;
		}

		// Now set the type.
		initializer->SetConversion(initConv);
		initializer->SetConversionType(conversionType);
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRParser::ParseInitializerList(const Type* type, shared<Initializer>& initializer) {
	const ArrayType* arrayType = nullptr;
	const RecordType* recordType = nullptr;
	__int64 position = 0;
	__int64 maxPosition = 0;

    // Skip over {
	EatToken();

	if(type->IsArray()) {
		arrayType = type->As<ArrayType>();
		maxPosition = arrayType->Size() < 0 ?
					  std::numeric_limits<int>::max() : arrayType->Size();
	}
	else if(type->IsRecord()) {
		recordType = type->As<RecordType>();
		maxPosition = recordType->FieldCount();
	}
	else {
		// Only arrays and records can be initialized with a list.
		return false;
	}

	shared<InitializerList> initList = InitializerList::GetList();
	initializer = initList;

	while((IsCloseCurly() == false) && (IsEOF() == false)) {
		shared<Initializer> initializer;
		bool isNull;

		// Stop if there are too many initializers.
		if(position >= maxPosition) {
			EmitError(Error::INITIALIZER_COUNT);
			return false;
		}

		if(arrayType) {
			if(ParseInitializer(arrayType->ElementType(), initializer, isNull) == false) {
				return false;
			}
		}
		else {
			auto fieldType = recordType->Fields()[(int)position].FieldType;
			if(ParseInitializer(fieldType, initializer, isNull) == false) return false;
		}

		// 'nullptr' can't appear in an initializer list.
		if(isNull) {
			EmitError(Error::INVALID_INITIALIZER);
			return false;
		}

		initList->Add(initializer);
		position++;

		if(IsComma()) {
            EatToken();
        }
		else if(IsCloseCurly()) {
            break;
        }
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRParser::ParseAttributes(AttributeHolder& holder) {
	// Parse until no attribute is found.
	bool invalid = false;
	bool done = false;

	while(true) {
		switch(Kwd()) {
			default: { return true; }
			case KeywordType::Volatile: { 
				invalid = holder.HasAttribute(AttributeType::Volatile);
				holder.SetAttribute(AttributeType::Volatile);
				EatToken();
				break;
			}
			case KeywordType::Static: { 
				invalid = holder.HasAttribute(AttributeType::Extern) | 
						  holder.HasAttribute(AttributeType::Static) |
						  holder.HasAttribute(AttributeType::Tentative);
				holder.SetAttribute(AttributeType::Static);
				EatToken();
				break;
			}
			case KeywordType::Extern: { 
				invalid = holder.HasAttribute(AttributeType::Extern) | 
						  holder.HasAttribute(AttributeType::Static) |
						  holder.HasAttribute(AttributeType::Tentative);
				holder.SetAttribute(AttributeType::Extern);
				EatToken();
				break;
			}
			case KeywordType::Tentative: { 
				invalid = holder.HasAttribute(AttributeType::Extern) | 
						  holder.HasAttribute(AttributeType::Static) |
						  holder.HasAttribute(AttributeType::Tentative);
				holder.SetAttribute(AttributeType::Tentative);
				EatToken();
				break;
			}
			case KeywordType::Intr: { 
				invalid = holder.HasAttribute(AttributeType::Intr);
				holder.SetAttribute(AttributeType::Intr);
				EatToken();
				break;
			}
			case KeywordType::Stdcall: { 
				invalid = holder.HasAttribute(AttributeType::Fastcall) | 
						  holder.HasAttribute(AttributeType::Cdecl) | 
						  holder.HasAttribute(AttributeType::Stdcall);
				holder.SetAttribute(AttributeType::Stdcall);
				EatToken();
				break;
			}
			case KeywordType::Cdecl: { 
				invalid = holder.HasAttribute(AttributeType::Fastcall) | 
						  holder.HasAttribute(AttributeType::Cdecl) | 
						  holder.HasAttribute(AttributeType::Stdcall);
				holder.SetAttribute(AttributeType::Cdecl);
				EatToken();
				break;
			}
			case KeywordType::Fastcall: { 
				invalid = holder.HasAttribute(AttributeType::Fastcall) | 
						  holder.HasAttribute(AttributeType::Cdecl) | 
						  holder.HasAttribute(AttributeType::Stdcall);
				holder.SetAttribute(AttributeType::Fastcall);
				EatToken();
				break;
			}
			case KeywordType::Const: { 
				invalid = holder.HasAttribute(AttributeType::Const);
				holder.SetAttribute(AttributeType::Const);
				EatToken();
				break;
			}
			case KeywordType::Inline: { 
				invalid = holder.HasAttribute(AttributeType::Inline) |
						  holder.HasAttribute(AttributeType::Forceinline) |
						  holder.HasAttribute(AttributeType::Noinline);
				holder.SetAttribute(AttributeType::Inline);
				EatToken();
				break;
			}
			case KeywordType::Forceinline: { 
				invalid = holder.HasAttribute(AttributeType::Inline) |
						  holder.HasAttribute(AttributeType::Forceinline) |
						  holder.HasAttribute(AttributeType::Noinline);
				holder.SetAttribute(AttributeType::Forceinline);
				EatToken();
				break;
			}
			case KeywordType::Noinline: { 
				invalid = holder.HasAttribute(AttributeType::Inline) |
						  holder.HasAttribute(AttributeType::Forceinline) |
						  holder.HasAttribute(AttributeType::Noinline);
				holder.SetAttribute(AttributeType::Noinline);
				EatToken();
				break;
			}
			case KeywordType::Restrict: { 
				invalid = holder.HasAttribute(AttributeType::Restrict);
				holder.SetAttribute(AttributeType::Restrict);
				EatToken();
				break;
			}
			case KeywordType::Dllimport: { 
				invalid = holder.HasAttribute(AttributeType::Dllimport);
				holder.SetAttribute(AttributeType::Dllimport);
				EatToken();
				break;
			}
			case KeywordType::Dllexport: { 
				invalid = holder.HasAttribute(AttributeType::Dllexport);
				holder.SetAttribute(AttributeType::Dllexport);
				EatToken();
				break;
			}
			case KeywordType::Decl: { 
				invalid = holder.HasAttribute(AttributeType::Decl);
				holder.SetAttribute(AttributeType::Decl);
				EatToken();
				break;
			}
			case KeywordType::Adjust:
			case KeywordType::Align: {
				// align ( number )
				__int64 value;
				invalid = holder.HasAttribute(AttributeType::Align);
				holder.SetAttribute(AttributeType::Align);
				EatToken();

				if(ExpectAndEat(TokenKind::OpenParen, 
                                Error::EXPECTED_OPEN_PAREN) == false) break;
				
                if(ParseInteger(value) == false) break;

				if(ExpectAndEat(TokenKind::CloseParen, 
                                Error::EXPECTED_CLOSE_PAREN) == false) break;


				holder.SetAlign(value);
				break;
			}
			case KeywordType::Section: {
				// section ( identifier )
				invalid = holder.HasAttribute(AttributeType::Section);
				holder.SetAttribute(AttributeType::Section);
				EatToken();

				if(ExpectAndEat(TokenKind::OpenParen, 
                                Error::EXPECTED_OPEN_PAREN) == false) break;

				if(current_.IsIdentifier() == false) {
					EmitError(Error::EXPECTED_IDENTIFIER);
					break;
				}

				holder.SetSection(current_.NameValue()->Name);
				if(ExpectAndEat(TokenKind::CloseParen, 
                                Error::EXPECTED_CLOSE_PAREN) == false) break;
				break;
			}
			case KeywordType::Bool: {
				invalid = holder.HasAttribute(AttributeType::Bool);
				holder.SetAttribute(AttributeType::Bool);
				EatToken();
				break;
			}
			case KeywordType::Stdlib: {
				invalid = holder.HasAttribute(AttributeType::Stdlib);
				holder.SetAttribute(AttributeType::Stdlib);
				EatToken();
				break;
			}
			case KeywordType::Uso: {
				invalid = holder.HasAttribute(AttributeType::Uso) ||
						  holder.HasAttribute(AttributeType::Fpexact) ||
						  holder.HasAttribute(AttributeType::Fpsafe) ||
						  holder.HasAttribute(AttributeType::Fpfast);
				holder.SetAttribute(AttributeType::Uso);
				EatToken();
				break;
			}
			case KeywordType::Fpexact: {
				invalid = holder.HasAttribute(AttributeType::Fpexact) ||
						  holder.HasAttribute(AttributeType::Fpsafe) ||
						  holder.HasAttribute(AttributeType::Fpfast);
				holder.SetAttribute(AttributeType::Fpexact);
				EatToken();
				break;
			}
			case KeywordType::Fpsafe: {
				invalid = holder.HasAttribute(AttributeType::Fpexact) ||
						  holder.HasAttribute(AttributeType::Fpsafe) ||
						  holder.HasAttribute(AttributeType::Fpfast);
				holder.SetAttribute(AttributeType::Fpsafe);
				EatToken();
				break;
			}
			case KeywordType::Fpfast: {
				invalid = holder.HasAttribute(AttributeType::Fpexact) ||
						  holder.HasAttribute(AttributeType::Fpsafe) ||
						  holder.HasAttribute(AttributeType::Fpfast);
				holder.SetAttribute(AttributeType::Fpfast);
				EatToken();
				break;
			}
            case KeywordType::Nowrite: {
				invalid = holder.HasAttribute(AttributeType::Nowrite);
                holder.SetAttribute(AttributeType::Nowrite);
				EatToken();
				break;
			}
            case KeywordType::Noread: {
				invalid = holder.HasAttribute(AttributeType::Nowrite);
                holder.SetAttribute(AttributeType::Noread);
				EatToken();
				break;
			}
            case KeywordType::Noescape: {
				invalid = holder.HasAttribute(AttributeType::Noescape);
                holder.SetAttribute(AttributeType::Noescape);
				EatToken();
				break;
			}
            case KeywordType::Nostate: {
				invalid = holder.HasAttribute(AttributeType::Nostate);
                holder.SetAttribute(AttributeType::Nostate);
				EatToken();
				break;
			}
            case KeywordType::Norem: {
				invalid = holder.HasAttribute(AttributeType::Norem);
                holder.SetAttribute(AttributeType::Norem);
				EatToken();
				break;
			}
            case KeywordType::Noreturn: {
				invalid = holder.HasAttribute(AttributeType::Noreturn);
                holder.SetAttribute(AttributeType::Noreturn);
				EatToken();
				break;
			}
			case KeywordType::Noindirread: {
				invalid = holder.HasAttribute(AttributeType::Noindirread);
                holder.SetAttribute(AttributeType::Noindirread);
				EatToken();
				break;
			}
			case KeywordType::Noindirwrite: {
				invalid = holder.HasAttribute(AttributeType::Noindirwrite);
                holder.SetAttribute(AttributeType::Noindirwrite);
				EatToken();
				break;
			}
		}

		if(invalid) {
			return false;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Symbol* IRParser::ParseTypename() {
	// typeDecl -> type identifier = recordDecl | type = functionDecl
	EatToken(); // Skip over 'type'.
	bool hasSharp = false; // #

	if(current_.Kind() == TokenKind::Custom) {
		hasSharp = true;
		EatToken();
	}

	if(current_.IsIdentifier() == false) {
		EmitError(Error::EXPECTED_IDENTIFIER);
		return nullptr;
	}

	string name = current_.NameValue()->Name;
	if(hasSharp) name = "#" + name;
	EatToken();

	if(ExpectAndEat(TokenKind::Eq, Error::EXPECTED_EQUAL) == false) {
		return nullptr;
	}

	// Check if a record or function follows.
	if(Kwd() == KeywordType::Record) {
		// A record can make a a reference to itself, so we need to
		// add the typename before starting to parse the field list.
		List<RecordField> fields;
		auto recordType = types_->GetRecord(fields);
		Symbol* tn = Symbol::GetTypename(recordType, name, &unit_->Symbols());
		unit_->AddTypename(tn);
		
		ParseRecord(const_cast<RecordType*>(recordType));
		return tn;
	}
	else if(Kwd() == KeywordType::Funct) {
		auto functionType = ParseFunction();
		Symbol* tn = Symbol::GetTypename(functionType, name, &unit_->Symbols());
		unit_->AddTypename(tn);
		return tn;
	}
	else {
		EmitError(Error::INVALID_TOKEN);
		return nullptr;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
RecordType* IRParser::ParseRecord(RecordType* record) {
	// recordDecl -> record { fieldList }
	// fieldList -> fieldList , field | field
	// field -> type fieldOffset
	// fieldOffset -> offset ( integer )
	EatToken(); // Skip over 'record'
	
	if(ExpectAndEat(TokenKind::OpenCurly, Error::EXPECTED_OPEN_CURLY) == false) {
		return nullptr;
	}

    // This is a forward declaration with no fields.
    if(IsCloseCurly()) {
        EatToken();
        return record;
    }

	while(true) {
		// Read the type, than the offset.
		RecordField field;
		field.FieldType = ParseType();
		
		if(field.FieldType == nullptr) {
			EmitError(Error::INVALID_TYPE);
			return nullptr;
		}

		if(Kwd() != KeywordType::Offset) {
			EmitError(Error::EXPECTED_OFFSET);
			return nullptr;
		}
		else EatToken(); // Skip over 'offset'

		if(ExpectAndEat(TokenKind::OpenParen, Error::EXPECTED_OPEN_PAREN) == false) {
			return nullptr;
		}

		if(ParseInteger(field.FieldOffset) == false) {
			return nullptr;
		}

		if(ExpectAndEat(TokenKind::CloseParen, Error::EXPECTED_CLOSE_PAREN) == false) {
			return nullptr;
		}

		// Add the field and check if more follow.
		record->Fields().Add(field);

		if(IsCloseCurly()) {
			EatToken();
			break; // No more fields.
		}
		else if(IsComma() == false) {
			EmitError(Error::EXPECTED_COMMA);
			return nullptr;
		}
		else EatToken(); // Skip over ,
	}

	return record;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* IRParser::ParseFunctionDeclaration() {
	// functionDecl -> function attributeList functIdentifier ( parameterList ) : type
	// functIdentifier -> identifier | EPS
	// parameterList -> parameterList , parameter | parameter | EPS
	// parameter -> variableDecl | …
	AttributeHolder attr;
	string name;
	List<Variable*> parameters;
	const Type* returnType;
	bool isVarargs = false;
	EatToken(); // Skip over 'function'

	// Parse the attributes, then the identifier.
	if(ParseAttributes(attr) == false) {
		inFunctProto_ = false;
		return false;
	}

	if(current_.IsIdentifier() == false) {
		EmitError(Error::EXPECTED_IDENTIFIER);
		inFunctProto_ = false;
		return nullptr;
	}
	else {
		name = current_.NameValue()->Name;
		EatToken();
	}

	// Parse the list of parameters.
	if(ExpectAndEat(TokenKind::OpenParen, Error::EXPECTED_OPEN_PAREN) == false) {
		inFunctProto_ = false;
		return false;
	}

	// Mark that we're in a function prototype.
	inFunctProto_ = true;

	while((IsCloseParen() == false) && (IsEOF() == false)) {
		if(current_.Kind() == TokenKind::Ellipsis) {
			// ... should not be the first parameter,
            // and nothing should follow after it.
			if(parameters.Count() == 0) {
				EmitError(Error::INVALID_ELLIPSIS);
				inFunctProto_ = false;
				return nullptr;
			}

			EatToken();
			isVarargs = true;

			if(IsCloseParen() == false) {
				EmitError(Error::INVALID_ELLIPSIS);
				inFunctProto_ = false;
				return nullptr;
			}

			break;
		}

		Variable* parameter = ParseVariable(false /* nameRequired */);
		if(parameter == nullptr) {
			inFunctProto_ = false;
			return nullptr;
		}

		// Make sure the name is not taken by another variable.
		if(parameters.Find([parameter](Variable* variable) -> bool {
			return (variable->HasName() && parameter->HasName()) ? 
				   *variable->Name() == *parameter->Name() : false;
		})) {
			EmitError(Error::DUPLICATE_SYMBOL);
			inFunctProto_ = false;
			return nullptr;
		}

		// Add the variable to the list and check if there are more variables.
		parameters.Add(parameter);
        parameter->SetIsParameter(true);

		if(IsComma()) EatToken(); // Skip over ,
		else break;
	}

	// No longer in function prototype.
	inFunctProto_ = false;

	if(ExpectAndEat(TokenKind::CloseParen, Error::EXPECTED_CLOSE_PAREN) == false) {
		return nullptr;
	}

	// Parse the return type.
	if(ExpectAndEat(TokenKind::Colon, Error::EXPECTED_COLON) == false) {
		return nullptr;
	}

	returnType = ParseType();
	if(returnType == nullptr) return nullptr;

	// Create the function and attach it to the unit.
	List<const Type*> paramTypes;
	const FunctionType* functionType;

	for(int i = 0; i < parameters.Count(); i++) {
		paramTypes.Add(parameters[i]->GetType());
	}

	functionType = types_->GetFunction(returnType, paramTypes.GetInternal(), 
                                       paramTypes.Count());
	Function* function = Function::GetFunction(functionType, name, true, unit_);

	// Attach the parameters.
	for(int i = 0; i < parameters.Count(); i++) {
		parameters[i]->SetId(function->GetNextVariableId());
		function->AddParameter(parameters[i]);
		function->Symbols().Add(parameters[i]);
		parameters[i]->SetParentTable(&function->Symbols());
	}

	// Apply attributes on the function.
	int validAttr = 0;

	if(attr.HasAttribute(AttributeType::Decl)) {
		function->SetIsDefinition(false);
		validAttr++;
	}

	if(attr.HasAttribute(AttributeType::Static)) {
		function->SetVisibility(SymbolVisibility::Static);
		validAttr++;
	}
	else if(attr.HasAttribute(AttributeType::Extern)) {
		function->SetVisibility(SymbolVisibility::Extern);
		validAttr++;
	}

	if(attr.HasAttribute(AttributeType::Dllimport)) {
		function->SetDllVisibility(DllVisibility::Import);
		validAttr++;
	}
	else if(attr.HasAttribute(AttributeType::Dllexport)) {
		function->SetDllVisibility(DllVisibility::Export);
		validAttr++;
	}

	if(attr.HasAttribute(AttributeType::Inline)) {
		function->SetInline(InlineType::Auto);
		validAttr++;
	}
	else if(attr.HasAttribute(AttributeType::Noinline)) {
		function->SetInline(InlineType::Never);
		validAttr++;
	}
	else if(attr.HasAttribute(AttributeType::Forceinline)) {
		function->SetInline(InlineType::Always);
		validAttr++;
	}

	if(attr.HasAttribute(AttributeType::Cdecl)) {
		function->SetCallConvention(CallConventionType::Cdecl);
		validAttr++;
	}
	else if(attr.HasAttribute(AttributeType::Stdcall)) {
		function->SetCallConvention(CallConventionType::Stdcall);
		validAttr++;
	}
	else if(attr.HasAttribute(AttributeType::Fastcall)) {
		function->SetCallConvention(CallConventionType::Fastcall);
		validAttr++;
	}

	if(attr.HasAttribute(AttributeType::Stdlib)) {
		function->SetIsFromStdlib(true);
		validAttr++;
	}

	if(attr.HasAttribute(AttributeType::Noindirwrite)) {
		function->SetIsNoIndirectWrite(true);
		validAttr++;
	}

	if(attr.HasAttribute(AttributeType::Noindirread)) {
		function->SetIsNoIndirectRead(true);
		validAttr++;
	}

    if(attr.HasAttribute(AttributeType::Nostate)) {
		function->SetIsNoState(true);
		validAttr++;
	}

	// Check if there where specified attributes that are not valid.
	if(validAttr < attr.Count()) {
		EmitError(Error::INVALID_ATTRIBUTES);
		return nullptr;
	}

	return function;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const FunctionType* IRParser::ParseFunction() {
	// functionType -> function ( typeList ) : type
	// typeList -> typeList , type | type | ... | EPS
	List<const Type*> parameters;
	const Type* returnType;
	bool isVarargs = false;
	EatToken(); // Skip over 'function'

	// Parse the list of parameters.
	if(ExpectAndEat(TokenKind::OpenParen, Error::EXPECTED_OPEN_PAREN) == false) {
		inFunctProto_ = false;
		return false;
	}

	while((IsCloseParen() == false) && (IsEOF() == false)) {
		if(current_.Kind() == TokenKind::Ellipsis) {
			// ... should not be the first parameter, and nothing should follow after it.
			if(parameters.Count() == 0) {
				EmitError(Error::INVALID_ELLIPSIS);
				inFunctProto_ = false;
				return nullptr;
			}

			EatToken();
			isVarargs = true;

			if(IsCloseParen() == false) {
				EmitError(Error::INVALID_ELLIPSIS);
				inFunctProto_ = false;
				return nullptr;
			}

			break;
		}

		const Type* type = ParseType();
		if(type == nullptr) return nullptr;
		parameters.Add(type);

		if(IsComma()) EatToken(); // Skip over ,
		else break;
	}

	if(ExpectAndEat(TokenKind::CloseParen, Error::EXPECTED_CLOSE_PAREN) == false) {
		return nullptr;
	}

	// Parse the return type.
	if(ExpectAndEat(TokenKind::Colon, Error::EXPECTED_COLON) == false) {
		return nullptr;
	}

	returnType = ParseType();

	if(returnType == nullptr) {
        return nullptr;
    }
	else return types_->GetFunction(returnType, parameters.GetInternal(), 
                                    parameters.Count());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRParser::ParseFunctionDefinition() {
	// functionDef -> functionDecl { variableList blockList }
	// variableList -> variableList variableDecl | variableDecl | EPS
	// blockList -> blockList block | block
	bool invalid = false;

	if(ExpectAndEat(TokenKind::OpenCurly, Error::EXPECTED_OPEN_CURLY) == false) {
		currentFunct_ = nullptr;
		return false;
	}

	// Parse the optional list of local variables.
	// We parse until we find the first 'label' keyword or }.
	while((Kwd() == KeywordType::Var) && (IsCloseCurly() == false) && (IsEOF() == false)) {
		Variable* variable = ParseVariable();

		if(variable == nullptr) {
			invalid = true;
			SkipToSafePoint();
		}
	}

	// Parse the blocks, until we find }
	while((IsCloseCurly() == false) && (IsEOF() == false)) {
		if(ParseBlock() == false) {
			SkipToSafePoint();
			invalid = true;
		}
	}

	if(ExpectAndEat(TokenKind::CloseCurly, Error::EXPECTED_CLOSE_CURLY) == false) {
		currentFunct_ = nullptr;
		return false;
	}

	tempOps_.Clear(); // Temporaries names no longer needed.
	currentFunct_ = nullptr;
	return invalid == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IRParser::ParseBlock() {
	// block -> label identifier : instrList
	// instrList -> instrList instruction | instruction
	EatToken(); // Skip over 'label'
	bool hasSharp = false; // #

	if(current_.Kind() == TokenKind::Custom) {
		hasSharp = true;
		EatToken();
	}

	if(current_.IsIdentifier() == false) {
		EmitError(Error::EXPECTED_IDENTIFIER);
		return false;
	}

	bool invalid = false;
	string name = current_.NameValue()->Name;
	if(hasSharp) name = "#" + name;
	EatToken();

	// Get the block and insert it on the last position.
	currentBlock_ = GetBlock(*currentFunct_->Name(), name);
	currentBlock_->SetParentFunction(currentFunct_);
	currentFunct_->InsertLastBlock(currentBlock_);

	// A : should follow now.
	if(ExpectAndEat(TokenKind::Colon, Error::EXPECTED_COLON) == false) {
		return false;
	}

	// We parse until we find a 'label' keyword or }.
	while((Kwd() != KeywordType::Label) && (IsCloseCurly() == false) && (IsEOF() == false)) {
		Instruction* instr = ParseInstruction();
		if(instr == nullptr) {
			invalid = true;
			SkipToSafePoint();
		}
	}

	currentBlock_ = nullptr;
	return invalid == false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseInstruction() {
	// instruction -> binaryInstr | conversionInstr | compareIstr | 
	//				  controlInstr | addrInstr | loadInstr | storeInstr
	Operand* resultOp = nullptr;

	// Most instructions have their result stored into a temporary.
	if(current_.IsIdentifier()) {
		resultOp = ParseOperand(true /* create */);

		if(ExpectAndEat(TokenKind::Eq, Error::EXPECTED_EQUAL) == false) {
			return nullptr;
		}
	}

	switch(Kwd()) {
		default: {
			EmitError(Error::INVALID_TOKEN);
			return nullptr;
		}
		case KeywordType::Add    : return ParseBinaryInstr(Opcode::Add,  resultOp, false);
		case KeywordType::Sub    : return ParseBinaryInstr(Opcode::Sub,  resultOp, false);
		case KeywordType::Mul    : return ParseBinaryInstr(Opcode::Mul,  resultOp, false);
		case KeywordType::Div    : return ParseBinaryInstr(Opcode::Div,  resultOp, false);
		case KeywordType::Udiv   : return ParseBinaryInstr(Opcode::Udiv, resultOp, false);
		case KeywordType::Mod    : return ParseBinaryInstr(Opcode::Mod,  resultOp, false);
		case KeywordType::Umod   : return ParseBinaryInstr(Opcode::Umod, resultOp, false);

		case KeywordType::Fadd   : return ParseBinaryInstr(Opcode::Fadd, resultOp, false);
		case KeywordType::Fsub   : return ParseBinaryInstr(Opcode::Fsub, resultOp, false);
		case KeywordType::Fmul   : return ParseBinaryInstr(Opcode::Fmul, resultOp, false);
		case KeywordType::Fdiv   : return ParseBinaryInstr(Opcode::Fdiv, resultOp, false);

		case KeywordType::Shl    : return ParseBinaryInstr(Opcode::Shl,  resultOp, true);
		case KeywordType::Shr    : return ParseBinaryInstr(Opcode::Shr,  resultOp, true);
		case KeywordType::Ushr   : return ParseBinaryInstr(Opcode::Ushr, resultOp, true);
		case KeywordType::And    : return ParseBinaryInstr(Opcode::And,  resultOp, true);
		case KeywordType::Or	    : return ParseBinaryInstr(Opcode::Or,   resultOp, true);
		case KeywordType::Xor    : return ParseBinaryInstr(Opcode::Xor,  resultOp, true);

		case KeywordType::Zext   : return ParseConversionInstr(Opcode::Zext,   resultOp);
		case KeywordType::Sext   : return ParseConversionInstr(Opcode::Sext,   resultOp);
		case KeywordType::Trunc  : return ParseConversionInstr(Opcode::Trunc,  resultOp);
		case KeywordType::Ftoi   : return ParseConversionInstr(Opcode::Ftoi,   resultOp);
		case KeywordType::Ftoui  : return ParseConversionInstr(Opcode::Ftoui,  resultOp);
		case KeywordType::Itof   : return ParseConversionInstr(Opcode::Itof,   resultOp);
		case KeywordType::Uitof  : return ParseConversionInstr(Opcode::Uitof,  resultOp);
		case KeywordType::Ftrunc : return ParseConversionInstr(Opcode::Ftrunc, resultOp);
		case KeywordType::Fext   : return ParseConversionInstr(Opcode::Fext,   resultOp);
		case KeywordType::Ptoi   : return ParseConversionInstr(Opcode::Ptoi,   resultOp);
		case KeywordType::Itop   : return ParseConversionInstr(Opcode::Itop,   resultOp);
		case KeywordType::Ptop   : return ParseConversionInstr(Opcode::Ptop,   resultOp);

		case KeywordType::Cmp    : return ParseCompareInstr(Opcode::Cmp,  resultOp);
		case KeywordType::Ucmp   : return ParseCompareInstr(Opcode::Ucmp, resultOp);
		case KeywordType::Fcmp   : return ParseCompareInstr(Opcode::Fcmp, resultOp);

		case KeywordType::If     : return ParseIfInstr();
		case KeywordType::Goto   : return ParseGotoInstr();
		case KeywordType::Call   : return ParseCallInstr(resultOp);
		case KeywordType::Ret    : return ParseReturnInstr();
		case KeywordType::Switch : return ParseSwitchInstr();
		case KeywordType::Load   : return ParseLoadInstr(resultOp);
		case KeywordType::Store  : return ParseStoreInstr();
		case KeywordType::Addr   : return ParseAddressInstr(resultOp);
		case KeywordType::Element: return ParseElementInstr(resultOp);
		case KeywordType::Index  : return ParseIndexInstr(resultOp);
		case KeywordType::Phi    : return ParsePhiInstr(resultOp);
        case KeywordType::Quest  : return ParseQuestionInstr(resultOp);
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseBinaryInstr(Opcode opcode, Operand* resultOp, bool isLogical) {
	// binaryInstr -> value = binaryOp  value , value attributeList
	EatToken(); // Skip over opcode.

	Operand* leftOp = ParseOperand();
	if(leftOp == nullptr) return nullptr;
	
	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	Operand* rightOp = ParseOperand();
	if(rightOp == nullptr) return nullptr;

	// Parse the optional attributes.
	AttributeHolder attr;
	if(ParseAttributes(attr) == false) {
		return nullptr;
	}

	// Return the appropriate type of instruction.
	PatchOperands(resultOp, leftOp, rightOp);

	if(isLogical) {
		return LogicalInstr::GetLogical(opcode, leftOp, rightOp, resultOp, currentBlock_);
	}
	else {
		auto instr = ArithmeticInstr::GetArithmetic(opcode, leftOp, rightOp, 
													resultOp, currentBlock_);
		// Apply the attributes.
		if(attr.HasAttribute(AttributeType::Uso)) {
			instr->SetHasUndefinedOverflow(true);
		}
		else if(attr.HasAttribute(AttributeType::Fpexact)) {
			instr->SetFPMode(FloatMode::Exact);
		}
		else if(attr.HasAttribute(AttributeType::Fpsafe)) {
			instr->SetFPMode(FloatMode::Safe);
		}
		else if(attr.HasAttribute(AttributeType::Fpfast)) {
			instr->SetFPMode(FloatMode::Fast);
		}

        if(attr.HasAttribute(AttributeType::Norem)) {
            instr->SetHasNoRemainder(true);
        }

        return instr;
	}
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseConversionInstr(Opcode opcode, Operand* resultOp) {
	// conversionInstr -> value = conversionOp value , basicType
	EatToken(); // Skip over opcode.
	Operand* leftOp = ParseOperand();
	if(leftOp == nullptr) return nullptr;
	
	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	// Parse the type to be converted to.
	const Type* type = ParseType();
	if(type == nullptr) return nullptr;

	PatchOperand(resultOp, type);
	return ConversionInstr::GetConversion(opcode, leftOp, type, resultOp, currentBlock_);
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseCompareInstr(Opcode opcode, Operand* resultOp) {
	// compareInstr -> value = compareOp relation value , value
	// relation -> lt | gt | lte | gte | eq | neq
	EatToken(); // Skip over opcode.

	// Determine the relation to be tested.
	OrderType order;

	switch(Kwd()) {
		case KeywordType::Lt	 : { order = OrderType::Less;           break; }
		case KeywordType::Lte : { order = OrderType::LessOrEqual;    break; }
		case KeywordType::Gt  : { order = OrderType::Greater;        break; }
		case KeywordType::Gte : { order = OrderType::GreaterOrEqual; break; }
		case KeywordType::Eq	 : { order = OrderType::Equal;          break; }
		case KeywordType::Neq : { order = OrderType::NotEqual;       break; }
		default: {
			// Invalid comparison relation.
			EmitError(Error::INVALID_TOKEN);
			return nullptr;
		}
	}

	EatToken(); // Skip over relation.
	Operand* leftOp = ParseOperand();
	if(leftOp == nullptr) return nullptr;

	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	Operand* rightOp = ParseOperand();
	if(rightOp == nullptr) return nullptr;

	// Return the right kind of comparison instruction.
	PatchOperands(resultOp, leftOp, rightOp);
	
	// The result needs to have 'int32' type, even if we compare pointers or floats.
	if(resultOp->GetType()->IsInteger() == false) {
		resultOp->SetType(IntegerType::GetInt32());
	}
	
	if(opcode == Opcode::Cmp) {
		return CmpInstr::GetCmp(order, leftOp, rightOp, resultOp, currentBlock_);
	}
	else if(opcode == Opcode::Ucmp) {
		return UcmpInstr::GetUcmp(order, leftOp, rightOp, resultOp, currentBlock_);
	}
	else if(opcode == Opcode::Fcmp) {
		return FcmpInstr::GetFcmp(order, leftOp, rightOp, resultOp, currentBlock_);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseLoadInstr(Operand* resultOp) {
	// loadInstr -> value = load value [volatile]
	EatToken(); // Skip over opcode.

	Operand* sourceOp = ParseOperand();
	if(sourceOp == nullptr) return nullptr;
	if(sourceOp->GetType() == nullptr) return nullptr;

	if(auto temp = sourceOp->GetType()->As<PointerType>()) {
		PatchOperand(resultOp, temp->PointeeType());
	}

	// Parse 'volatile'.
	AttributeHolder attr;
	if(ParseAttributes(attr) == false) {
		return nullptr;
	}

	// Now create the instruction, and mark it as 'volatile' if it's the case.
	auto instr = LoadInstr::GetLoad(sourceOp, resultOp, currentBlock_);
	instr->SetIsVolatile(attr.HasAttribute(AttributeType::Volatile));
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseStoreInstr() {
	// storeInstr -> value = store value , value [volatile]
	EatToken(); // Skip over opcode.

	Operand* destOp = ParseOperand();
	if(destOp == nullptr) return nullptr;

	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	Operand* sourceOp = ParseOperand();
	if(sourceOp == nullptr) return nullptr;

	// Parse 'volatile'.
	AttributeHolder attr;
	if(ParseAttributes(attr) == false) {
		return nullptr;
	}

	if(auto temp = destOp->GetType()->As<PointerType>()) {
		PatchOperand(sourceOp, temp->PointeeType());
	}

	// Now create the instruction, and mark it as 'volatile' if it's the case.
	auto instr = StoreInstr::GetStore(destOp, sourceOp, currentBlock_);
	instr->SetIsVolatile(attr.HasAttribute(AttributeType::Volatile));
	return instr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseAddressInstr(Operand* resultOp) {
	// addressInstr -> value = addr value , value
	EatToken(); // Skip over opcode.

	Operand* baseOp = ParseOperand();
	if(baseOp == nullptr) return nullptr;
	
	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	Operand* indexOp = ParseOperand();
	if(indexOp == nullptr) return nullptr;

	// Return the appropriate type of instruction.
	if(auto temp = baseOp->GetType()->As<PointerType>()) {
		auto pointeeType = temp->PointeeType();
		PatchOperand(resultOp, temp);
	}

	return AddressInstr::GetAddress(baseOp, indexOp, resultOp, currentBlock_);
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseElementInstr(Operand* resultOp) {
	// elementInstr -> value = field value , value
	EatToken(); // Skip over opcode.

	Operand* baseOp = ParseOperand();
	if(baseOp == nullptr) return nullptr;
	
	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	Operand* indexOp = ParseOperand();
	if(indexOp == nullptr) return nullptr;

	// Return the appropriate type of instruction.
	if(auto temp = baseOp->GetType()->As<PointerType>()) {
		auto pointeeType = temp->PointeeType();

		if(auto recordType = pointeeType->As<RecordType>()) {
			if(auto index = indexOp->As<IntConstant>()) {
				if((index->Value() < 0) || (index->Value() >= recordType->FieldCount())) {
					EmitError(Error::INVALID_FIELD);
					return nullptr;
				}

				auto fieldType = recordType->Fields()[(int)index->Value()].FieldType;
				PatchOperand(resultOp, types_->GetPointer(fieldType));
			}
			else {
				EmitError(Error::INVALID_FIELD);
				return nullptr;
			}
		}
	}

	return FieldInstr::GetField(baseOp, indexOp, resultOp, currentBlock_);
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseIndexInstr(Operand* resultOp) {
	// indexInstr -> value = index value , value
	EatToken(); // Skip over opcode.

	Operand* baseOp = ParseOperand();
	if(baseOp == nullptr) return nullptr;
	
	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	Operand* indexOp = ParseOperand();
	if(indexOp == nullptr) return nullptr;

	// Return the appropriate type of instruction.
	if(auto temp = baseOp->GetType()->As<PointerType>()) {
		auto pointeeType = temp->PointeeType();

		if(auto arrayType = pointeeType->As<ArrayType>()) {
			// [N int32]* -> int32*
			PatchOperand(resultOp, types_->GetPointer(arrayType->ElementType()));
		}
	}

	return IndexInstr::GetIndex(baseOp, indexOp, resultOp, currentBlock_);
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseIfInstr() {
	// ifInstr -> if value , identifier , identifier
	EatToken(); // Skip over opcode.

	Operand* conditionOp = ParseOperand();
	if(conditionOp == nullptr) return nullptr;
	
	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	BlockReference* trueOp = ParseBlockOperand();
	if(trueOp == nullptr) return nullptr;

	if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
		return nullptr;
	}

	BlockReference* falseOp = ParseBlockOperand();
	if(falseOp == nullptr) return nullptr;
	else return IfInstr::GetIf(conditionOp, trueOp, falseOp, currentBlock_); 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseGotoInstr() {
	// ifInstr -> if value , identifier , identifier
	EatToken(); // Skip over opcode.
	
	BlockReference* targetOp = ParseBlockOperand();
	if(targetOp == nullptr) return nullptr;
	return GotoInstr::GetGoto(targetOp, currentBlock_); 
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseCallInstr(Operand* resultOp) {
	// callInstr -> value = call attributeList value argList
	// argList -> argList , value | value | EPS
	EatToken(); // Skip over opcode.

	// A list of attributes is accepted here (call convention and intrinsic).
	CallConventionType callConvention = CallConventionType::Auto;
	AttributeHolder attr;

	if(ParseAttributes(attr) == false) {
		return nullptr;
	}

	if(attr.HasAttribute(AttributeType::Cdecl)) {
		callConvention = CallConventionType::Cdecl;
	}
	else if(attr.HasAttribute(AttributeType::Fastcall)) {
		callConvention = CallConventionType::Fastcall;
	}
	else if(attr.HasAttribute(AttributeType::Stdcall)) {
		callConvention = CallConventionType::Stdcall;
	}

	// If it's an intrinsic we need to make sure that the intrinsic 
	// definition is imported into the module.
	Operand* targetOp;
	
	if(attr.HasAttribute(AttributeType::Intr)) {
		targetOp = ParseIntrinsicOperand();
	}
	else targetOp = ParseOperand();

	if(targetOp == nullptr) {
		return nullptr;
	}

	auto pointerType = targetOp->GetType()->As<PointerType>();
	if((pointerType == nullptr) || pointerType->PointeeType()->IsFunction() == false) {
		EmitError(Error::EXPECTED_FUNCTION);
		return nullptr;
	}

	auto callInstr = CallInstr::GetCall(targetOp, resultOp, 0, currentBlock_);	
	auto functionType = pointerType->PointeeType()->As<FunctionType>();
	int argCount = 0;

	if(functionType->ParameterCount() > 0) {
		EatToken(); // Skip over ,

		while(true) {
			Operand* argument = ParseOperand();

			if(argument == nullptr) {
				return nullptr;
			}

			// Set the type of the operand based on the expected one.
			if(argCount < functionType->ParameterCount()) {
				PatchOperand(argument, functionType->Parameters()[argCount]);
			}

			callInstr->AddArgument(argument);
			argCount++;

			// A comma indicates that more arguments follow.
			if(IsComma()) EatToken();
			else break;
		}
	}

	PatchOperand(resultOp, functionType->ReturnType());
	
	// Set the call convention.
	if(callConvention != CallConventionType::Auto) {
		callInstr->SetCallConvention(callConvention);
		callInstr->SetHasOverridenCallConvention(true);
	}

	return callInstr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseReturnInstr() {
	// retInstr -> ret value | ret void
	EatToken(); // Skip over 'ret'

	// A special case is when we return 'void'.
	if(Kwd() == KeywordType::Void) {
		EatToken();
		return ReturnInstr::GetReturn(nullptr, currentBlock_);
	}
	
	Operand* returnedOp = ParseOperand();
	if(returnedOp == nullptr) return nullptr;

	PatchOperand(returnedOp, currentFunct_->ReturnType());
	return ReturnInstr::GetReturn(returnedOp, currentBlock_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseSwitchInstr() {
	// switchInstr -> switch value { caseList }
	// caseList -> caseList, case | case
	// case -> number : identifier | default : identifier
	EatToken(); // Skip over 'switch'

	Operand* conditionOp = ParseOperand();
	if(conditionOp == nullptr) return nullptr;
	
	if(ExpectAndEat(TokenKind::OpenCurly, Error::EXPECTED_OPEN_CURLY) == false) {
		return nullptr;
	}

	// Parse the list of 'case' statements.
	auto switchInstr = SwitchInstr::GetSwitch(conditionOp, 0, nullptr, currentBlock_);

	while((IsCloseCurly() == false) && (IsEOF() == false)) {
		bool isDefault = false;
		__int64 value;

		if(Kwd() == KeywordType::Default) {
			isDefault = true;
			EatToken();
		}

		if(isDefault == false) {
			if(ParseInteger(value) == false) {
				return nullptr;
			}
		}

		if(ExpectAndEat(TokenKind::Colon, Error::EXPECTED_COLON) == false) {
			return nullptr;
		}
		
		BlockReference* result = ParseBlockOperand();
		if(result == nullptr) return nullptr;

		if(isDefault) {
            switchInstr->SetDefaultTargetOp(result);
		}
		else {
			switchInstr->AddCase(value, result);
		}

		// A comma means that more 'case' statements follow.
		if(IsComma()) EatToken();
		else if(IsCloseCurly()) break;
	}

	if(ExpectAndEat(TokenKind::CloseCurly, Error::EXPECTED_CLOSE_CURLY) == false) {
		return nullptr;
	}

	return switchInstr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParsePhiInstr(Operand* resultOp) {
	// phiInstr -> value = phi phiOpList
	// phiOpList -> phiOp , phiOp | phiOp
	// phiOp -> { value , identifier }
	EatToken(); // Skip over 'phi'
	auto phiInstr = PhiInstr::GetPhi(resultOp, 2, currentBlock_);

	while(true) {
		// Skip over {
		if(ExpectAndEat(TokenKind::OpenCurly, Error::EXPECTED_OPEN_CURLY) == false) {
			return false;
		}

		Operand* op = ParseOperand();
		if(op== nullptr) return nullptr;

		// Skip over ,
		if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
			return false;
		}	

		Operand* block = ParseBlockOperand();
		if(block == nullptr) return nullptr;

		// Skip over {
		if(ExpectAndEat(TokenKind::CloseCurly, Error::EXPECTED_CLOSE_CURLY) == false) {
			return false;
		}

		phiInstr->AddOperand(op, block->As<BlockReference>());

		// A comma indicates that more arguments follow.
		if(IsComma()) EatToken();
		else break;
	}

	if(resultOp && (resultOp->GetType() == nullptr)) {
		if(phiInstr->OperandCount() > 0) {
			resultOp->SetType(phiInstr->GetOperand(0)->GetType());
		}
	}

	return phiInstr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Instruction* IRParser::ParseQuestionInstr(Operand* resultOp) {
    // binaryInstr -> value = binaryOp  value , value attributeList
    EatToken(); // Skip over opcode.

    Operand* conditionOp = ParseOperand();
    if(conditionOp == nullptr) return nullptr;

    if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
        return nullptr;
    }

    Operand* trueOp = ParseOperand();
    if(trueOp == nullptr) return nullptr;

    if(ExpectAndEat(TokenKind::Comma, Error::EXPECTED_COMMA) == false) {
        return nullptr;
    }

    Operand* falseOp = ParseOperand();
    if(falseOp == nullptr) return nullptr;

    return QuestionInstr::GetQuestion(conditionOp, trueOp, falseOp, resultOp);
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Operand* IRParser::ParseOperand(bool create) {
	bool hasMinus = false;   // -
	bool hasAddress = false; // &
	bool hasSharp = false;   // #
	Operand* op = nullptr;

	if(current_.Kind() == TokenKind::Sub) {
		hasMinus = true;
		EatToken();
	}
	
	if(current_.Kind() == TokenKind::And) {
		hasAddress = true;
		EatToken();
	}
	
	if(current_.Kind() == TokenKind::Custom) {
		hasSharp = true;
		EatToken();
	}

	if(current_.IsNumber()) {
		// We can't determine now the required type, so we choose a default:
		// 'int32' for integers and 'double' for floating.
		NumberInfo info = NumberParser(nullptr).Parse(current_);
		EatToken();

		if(info.IsInteger) {
			op = unit_->Constants().GetInt32(hasMinus ? -info.IntValue : info.IntValue);
		}
		else op = unit_->Constants().GetDouble(hasMinus ? -info.FloatValue : info.FloatValue);
	}
	else if(current_.IsIdentifier()) {
		string name = current_.NameValue()->Name;
		EatToken();

		// If a '#' has been found before,append it to the name.
		// Used to make the name of the variables unique.
		if(hasSharp) {
			name = "#" + name;
		}

		// The temporary may be followed by the 'bool' attribute.
		AttributeHolder attr;
		if(ParseAttributes(attr) == false) {
			return nullptr;
		}

		// First check if it's a reference to a local symbol.
		// Then if it's a temporary, and last if it's a reference to a global variable.
		if(hasAddress && currentFunct_->Symbols().Contains(&name)) {
			Symbol* symbol = currentFunct_->Symbols().Get(&name);

			if(auto variable = symbol->As<Variable>()) {
				auto refType = types_->GetPointer(variable->GetType());
				op = unit_->References().GetVariableRef(variable, refType);
			}
		}
		else if((hasAddress == false) && tempOps_.ContainsKey(name)) {
			// It's a temporary operand.
			op = tempOps_[name];
		}
		else if(hasAddress && unit_->Symbols().Contains(&name)) {
			// It's a global variable. If it's a function it's treated separately.
			Symbol* symbol = unit_->Symbols().Get(&name);

			if(auto function = symbol->As<Function>()) {
				auto refType = types_->GetPointer(function->GetType());
				op = unit_->References().GetFunctionRef(function, refType);
			}
			else  {
				auto globalVar = symbol->As<GlobalVariable>();
				auto refType = types_->GetPointer(globalVar->GetType());
				op = unit_->References().GetGlobalVariableRef(globalVar, refType);
			}
		}
		else if(unit_->Symbols().Contains(&name)) {
			// For functions & is not necessary.
			Symbol* symbol = unit_->Symbols().Get(&name);

			if(auto function = symbol->As<Function>()) {
				auto refType = types_->GetPointer(function->GetType());
				op = unit_->References().GetFunctionRef(function, refType);
			}
		}
		else if(create) {
			// The identifier was not found, so we consider that it's a new
			// temporary and create it now. Note that we don't know the type yet.
			Temporary* temp = Temporary::GetTemporary(nullptr);
			tempOps_.Add(name, temp);

			// Add a name tag to the temporary so that it keeps the name from the source.
            temp->AddTag(NameTag::GetName(name));
			op = temp;

			if(attr.HasAttribute(AttributeType::Bool)) {
				temp->SetIsBoolean(true);
			}
		}
		else {
			// The identifier was not found.
			EmitError(Error::INVALID_IDENTIFIER);
			return nullptr;
		}
	}
	else if(Kwd() == KeywordType::Nullptr) {
		EatToken();
		return unit_->Constants().GetNull(nullptr);
	}
	else if(Kwd() == KeywordType::Undef) {
		EatToken();
		return unit_->Constants().GetUndefined(nullptr);
	}
	
	if(op == nullptr) {
		// The operand is not valid.
		EmitError(Error::EXPECTED_OPERAND);
		return nullptr;
	}

	return op;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BlockReference* IRParser::ParseBlockOperand() {
	bool hasSharp = false; // #

	if(current_.Kind() == TokenKind::Custom) {
		hasSharp = true;
		EatToken();
	}

	if(current_.IsIdentifier() == false) {
		EmitError(Error::EXPECTED_IDENTIFIER);
		return nullptr;
	}

	string blockName = current_.NameValue()->Name;
	if(hasSharp) blockName = "#" + blockName;
	EatToken();

	Block* block = GetBlock(*currentFunct_->Name(), blockName);
	return unit_->References().GetBlockRef(block);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FunctionReference* IRParser::ParseIntrinsicOperand() {
	bool hasSharp = false; // #

	if(current_.Kind() == TokenKind::Custom) {
		hasSharp = true;
		EatToken();
	}

	if(current_.IsIdentifier()) {
		string name = current_.NameValue()->Name;
		if(hasSharp) name = "#" + name;
		EatToken();

		// Make sure this is an available intrinsic.
		if(unit_->Intrinsics().Contains(name) == false) {
			EmitError(Error::INVALID_INTRINSIC);
			return nullptr;
		}

		// Check if the the intrinsic has a declaration in the unit.
		// If not we add it now.
		Intrinsic* intrinsic = unit_->Intrinsics().Get(name);

		if(unit_->Symbols().Contains(&name) == false) {
			unit_->AddFunction(intrinsic);
		}

		// Make a reference to the intrinsic.
		auto refType = types_->GetPointer(intrinsic->GetType());
		return unit_->References().GetFunctionRef(intrinsic, refType);
	}
	else {
		EmitError(Error::EXPECTED_IDENTIFIER);
		return nullptr;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRParser::PatchOperands(Operand* result, Operand*& left, Operand*& right) {
	if(left == nullptr) return;
	if(right == nullptr) return;

	// Set the type of the operands based on the operands that have a type.
	// The type of the result operand has priority.
	const Type* type = result ? result->GetType() : nullptr;

	if(type == nullptr) {
        type = left->GetType();
    }

	if(type == nullptr) {
        type = right->GetType();
    }

    if(type == nullptr) {
        return;
    }

	if(result->GetType() == nullptr) {
		result->SetType(type);
	}

	if(left->GetType() != type) {
		PatchOperand(left, type);
	}

	if(right->GetType() != type) {
		PatchOperand(right, type);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void IRParser::PatchOperand(Operand*& op, const Type* type) {
	if(op == nullptr) return;

	if(auto intConst = op->As<IntConstant>()) {
		op = unit_->Constants().GetInt(type, intConst->Value());
	}
	else if(auto floatConst = op->As<FloatConstant>()) {
		op = unit_->Constants().GetFloating(type, floatConst->Value());
	}
	else if(auto nullConst = op->As<NullConstant>()) {
		op = unit_->Constants().GetNull(type);
	}
	else if(auto undefConst = op->As<UndefinedConstant>()) {
		op = unit_->Constants().GetUndefined(type);
	}
	else op->SetType(type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Unit> IRParser::ParseUnit(shared<Unit> unit) {
	// unit -> delarationList
	// delarationList -> declarationList declaration | declaration
	// declaration -> globalVariableDecl | typeDecl | functionDef
	unit_ = unit;
	EatToken(); // Read the first token.

	// Parse until the end of the file is found.
	while(IsEOF() == false) {
		switch(Kwd()) {
			case KeywordType::Var: {
				GlobalVariable* variable = static_cast<GlobalVariable*>(ParseVariable());
				if(variable == nullptr) {
					SkipToSafePoint();
				}

				break;
			}
			case KeywordType::Funct: {
				// First parse the function declaration. If the 'declaration' attribute
				// is not set it's a definition and then we parse the rest.
				Function* function = ParseFunctionDeclaration();
				if(function == nullptr) {
					SkipToSafePoint();
				}
				else if(function->IsDefinition()) {
					currentFunct_ = function;
					if(ParseFunctionDefinition() == false) {
						SkipToSafePoint();
					}
				}

				break;
			}
			case KeywordType::Type: {
				// This is a typename.
				Symbol* tn = ParseTypename();
				if(tn == nullptr) {
					SkipToSafePoint();
				}

				break;
			}
			default: {
				// Any other token is invalid.
				EmitError(Error::INVALID_TOKEN);
				SkipToSafePoint();
			}
		}
	}

	return unit_;
}

} // namespace IR