// InitSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements the methods that create and handle initializers.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "DeclarationSemantic.hpp"
#include "ExpressionSemantic.hpp"

namespace Parsing {

shared<Expression> 
DeclarationSemantic::InitializeScalar(const Type* type, shared<InitInfo> initializer,
								      shared<InitContext> context) {
	// The initializer should be of the form:
	// 'int a = 3' or 'int a = {3}';
	// Designators and multiple braces {{3}} are not valid here.
	shared<Expression> value = initializer->Value;

	if(initializer->Type == InitInfo::InitList) {
		auto& children = initializer->Children;
		
		if(children.Count() == 0) {
			// The initializer is empty {}, not valid.
			diag_->Report(Error::INITIALIZER_EMPTY)<<context->Location;
			return new InvalidExpression();
		}
		else if(children.Count() > 1) {
			// Too many initializers for a scalar type (only one allowed).
			diag_->Report(Error::INITIALIZERS_TOO_MANY)<<context->Location;
			return new InvalidExpression();
		}
		else if(auto temp = children[0].As<Designator>()) {
			// Designators not allowed.
			diag_->Report(Error::INITIALIZER_DESIGNATOR_FOR_SCALAR)<<context->Location;
			return new InvalidExpression();
		}

		// This should be a constant expression.
		auto temp = children[0].As<InitInfo>();
		
		if(temp->Type == InitInfo::InitList) {
			// Multiple braces (like '{{3}}') not allowed.
			diag_->Report(Error::INITIALIZERS_TOO_MANY)<<context->Location;
			return new InvalidExpression();
		}

		value = initializer->Value;
	}

	// Convert the expression to the required type and return it.
	// The same constrains as for simple assignment apply (C99:6.7.8.11).
	ExpressionSemantic exprSema(context_, types_);

	if(exprSema.IsSimpleAssignmentValid(type, value, nullptr) == false) {
		// The types are not compatible.
		return new InvalidExpression();
	}

	// Cast the initializer type to the target type.
	value = exprSema.CreateImplicitCast(value, type, CastType::Unknown);
	context->Index++;
	return value;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::InitializeArray(const ArrayType* type, 
                                     shared<InitContext> context) {
	// Incomplete arrays are completed by the initializer. The number of items
	// in the initializer will be the length of the array.
	// Here is a more complex example: int a[] = {1,2, [8] = 3, 4, 5};
	// In this case a[0] = 1, a[1] = 2, a[8] = 3, a[9] = 4 and a[10] = 5.
	// The range a[3] - a[7] must be filled with 0 (the default value).
	// Because of this we don't create the 'InitializerListExpression' directly, but place
	// 'ArrayElement's in a dictionary, sort it's items and fill the "holes" with 0.
	Dictionary<__int64, ArrayElement> items(256);
	shared<InitInfo>& initializer = context->Initializer;
	const Type* elementType = type->ElementType();
	auto& children = initializer->Children;
	bool failed = false;

	__int64 maxLength = type->IsIncomplete() ?
						std::numeric_limits<__int64>::max() :
						type->Size();
	__int64 index = 0; // The current index. Can be modified by designators.
	__int64 ct = 0;    // The number of items in the list.

	// If this is not the first-level initialization and the size of the array
	// is known, we take only the needed amount of initializers from the list
	// The rest can be used to initialize other subelements (C99:6.7.8.20).
	// Ex: 'int a[2][2] = {{1,2}, {3,4}};' is equivalent to 'int a[2][2] = {1,2,3,4};'.
	__int64 limit = context->Level == 0 && type->IsIncomplete() ?
			        children.Count() : type->Size();
	
	while((ct < limit) && (context->Index < children.Count())) {
		// Initialize the element type with the corresponding child.
		context->Level++; // Temporarily increase the level.
		shared<Expression> value = InitializeType(elementType, context);
		context->Level--;

		if(Expression::IsInvalid(value)) {
			// Failed to initialize the element.
			failed = true;
		}
		
		// Verify if the index has changed. If yes check if it's
		// in the valid range and use it for the next step.
		if(context->HasChanged) {
			index = context->NewIndex;

			if(context->Level > 0 && !type->IsIncomplete()) {
				ct = index;
			}

			context->ResetChanged();
		}

		if(index >= maxLength) {
			// Too many initializers for this array. 
			// Note that this cannot happen for incomplete arrays.
			diag_->Report(Error::INITIALIZERS_TOO_MANY)<<context->Location;
			return new InvalidExpression();
		}

		// We must make sure that the last value is stored in the list.
		// When using designators something like this can happen:
		// int a[] = {1, 2, [0]=3, 4};
		// Index:     0, 1,  0,    1
		// 1 and 2 are overwritten by 3 and 4.
		if(items.ContainsKey(index)) {
			items[index].Value = value;
		}
		else items.Add(index, ArrayElement(index, value));

		index++;
		ct++;
	}

	if(failed) {
		// At least one of the elements was not valid.
		return new InvalidExpression();
	}

	// Because of designators, there may be "holes" that must be filled
	// with the default value. If the size of the array is known, the
	// uninitialized elements found at the end of the array
	// are also filled with the default value (C99:6.7.8.21).
	shared<InitializerListExpression> listExpr = 
            new InitializerListExpression(context->Location);

	listExpr->SetResultType(type);
	auto& initList = listExpr->InitList();

	// Sort the items based on their index.
	List<ArrayElement> itemsArray(items.Count());

	items.ForEachValue([&itemsArray](ArrayElement& element) {
		itemsArray.Add(element);
	});

	itemsArray.Sort();
	
	for(int i = 0; i < itemsArray.Count(); i++) {
		// The list was sorted by index. In the above case we have:
		// 0, 1, 8, 9, 10. There is one "hole", between index 1 and 8.
		// Holes can appear at the beginning too.
		if((i == 0) && (itemsArray[i].Index != 0)) {
			for(int j = 0; j < itemsArray[i].Index; j++) {
				initList.Add(FillWithDefault(elementType, context));
			}
		}

		if((i > 0) && (itemsArray[i].Index != (itemsArray[i - 1].Index + 1))) {
			__int64 margin = itemsArray[i].Index;

			for(__int64 j = itemsArray[i - 1].Index + 1; j < margin; j++) {
				initList.Add(FillWithDefault(elementType, context));
			}
		}

		initList.Add(itemsArray[i].Value);
	}
	
	// See if there are uninitialized items at the end.
	if((type->IsIncomplete() == false) && (index < type->Size())) {
		for(__int64 i = index; i < type->Size(); i++) {
			initList.Add(FillWithDefault(elementType, context));
		}
	}

	return listExpr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::InitializeStructUnion(const StructUnionType* type, 
                                           shared<InitContext> context) {
    // Test for a 'struct' initialized with the result of a 'call', for example
    // 'struct ABC x = test();'.
    if(context->Initializer->Type == InitInfo::ConstExpression) {
        return context->Initializer->Value;
    }

	// The members of the struct/union are initialized in declaration order.
	// For union only the firs member is considered, and for both,
	// unnamed bitfields are ignored.
	// The list is scanned twice. The first time to add the initialized members
	// to a dictionary. The second time is to see which members where not 
	// initialized and to initialize them with the default value.
	bool isUnion = type->IsUnion();
	auto& children = context->Initializer->Children;
	auto& fields = type->Fields();
	bool failed = false;

	int index = 0; // The index of the current member.
	int maxLength = type->FieldCount();
	Dictionary<Identifier*, StructElement> handled(32);

	while((context->Index < children.Count()) && 
          (index < type->FieldCount())) {
		// Unnamed bitfields and the flexible array (if any)
		// should not be initialized (C99:6.7.8.9).
		auto field = fields[index];

		if(field->IsUnnamedBitfield() || 
           field->DeclarationType()->IsIncomplete()) {
			index++;
			continue;
		}

		// Initialize the current member and verify if the index
		// was changed by a field designator.
		context->Level++;
		shared<Expression> value = InitializeType(field->DeclarationType(), context);
		context->Level--;

		if(Expression::IsInvalid(value)) {
			// Failed to initialize the element.
			failed = true;
		}

		if(context->HasChanged) {
			// The next member will be the one after the changed one.
			index = context->NewIndex;
			field = fields[index];
			context->ResetChanged();
		}

		if(index >= maxLength) {
			// Too many initializers for this aggregate. 
			diag_->Report(Error::INITIALIZERS_TOO_MANY)<<context->Location;
			return new InvalidExpression();
		}

		// Place the value in the dictionary. Note that any previous value
		// is overwritten (like in 'struct A {int a;} t = {0, .a = 1};').
		StructElement element(field->Name(), value);

		if(handled.ContainsKey(field->Name())) {
			handled[field->Name()] = element;
		}
		else handled.Add(field->Name(), element);

		// If this is an union we stop here.
		if(isUnion) break;
		index++; // Advance to next member;
	}

	if(failed) {
		// At least one of the elements is invalid.
		return new InvalidExpression;
	}

	// If this is the top-level initializer no elements should remain.
	if((context->Level == 0) && 
       (context->Index != context->Initializer->Children.Count())) {
		diag_->Report(Error::INITIALIZERS_TOO_MANY)<<context->Location;
		return new InvalidExpression();
	}

	// Scan again the whole list of fields. If the field is found
	// in the dictionary, it's value is placed in the initializer list.
	// Else a default is generated. Unnamed bitfields and the flex array are skipped.
	shared<InitializerListExpression> listExpr = 
            new InitializerListExpression(context->Location);

	listExpr->SetResultType(type);

	for(int i = 0; i < fields.Count(); i++) {
		auto field = fields[i];

		if(field->IsUnnamedBitfield() || 
           field->DeclarationType()->IsIncomplete()) {
			continue;
		}

		if(handled.ContainsKey(field->Name())) {
			listExpr->InitList().Add(handled[field->Name()].Value);
		}
		else {
			// Generate a default value, but for 'union's only if it's the first item.
			if(isUnion && (i > 0)) {
                continue;
            }

            const Type* declType = field->DeclarationType();
            shared<Expression> defaultValue = FillWithDefault(declType, context);
			listExpr->InitList().Add(defaultValue);
		}
	}

	return listExpr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::MakeInitializer(const Type* type, shared<InitContext> context) {
	// Simple constants (like 'int a = 5') are handled directly.
	if(context->Initializer->Type == InitInfo::ConstExpression) {
		return InitializeTypeSimple(type, context->Initializer, context);
	}
	else {
		context->Level = -1;
		return InitializeTypeSimple(type, context->Initializer, context);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::InitializeType(const Type* type, shared<InitContext> context,
							        bool ignoreDesignators) {
	shared<InitBase> base = context->Initializer->Children[context->Index];
	
	// If this is a designator handle it now.
	if(ignoreDesignators == false) {
		if(auto temp = base.As<Designator>()) {
			return HandleDesignator(temp, type, context);
		}
	}

	return InitializeTypeSimple(type, base.As<InitInfo>(), context);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::InitializeTypeSimple(const Type* type, shared<InitInfo> initializer,
								          shared<InitContext> context) {
	// Select one of the methods based on the unqualified type.
	type = type->WithoutQualifiers();

	// Treat arrays and pointers initialized by strings as a special case
	// (like in 'int a[3] = "abcd"' or 'int a[] = "abcd"').
	if(initializer) {
		if(auto stringConst = AsStringInitializer(initializer)) {
			return InitializeString(type, stringConst, context);
		}

		// If the initializer is in braces a new context is created.
		if(initializer->Type == InitInfo::InitList) {
			// C99:6.7.8.17: when braces are encountered the current object
			// is changed to be the object the braces initialize.
			context->Index++; // Skip over this initializer.
			context = new InitContext(context->Level + 1, type, initializer, 
                                      context->Location, 0);
		}
	}

	if(type->IsBasic() || type->IsPointer()) {
		return InitializeScalar(type, initializer, context);
	}
	else if(type->IsArray()) {
		return InitializeArray(type->As<ArrayType>(), context);
	}
	else if(type->IsStruct() || type->IsUnion()) {
		return InitializeStructUnion(type->As<StructUnionType>(), context);
	}
	else if(type->IsEnum()) {
		return InitializeTypeSimple(type->As<EnumType>()->ConstType(), 
                                    initializer, context);
	}

	DebugValidator::Unreachable();
	return new InvalidExpression(); // Should not be reached!
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression>
DeclarationSemantic::AsStringInitializer(shared<InitInfo> initializer) {
	// A string constant can optionally be enclosed in braces
	// like in 'char a[] = {"abc"}' (C99:6.7.8.14).
	if(initializer->Type == InitInfo::InitList) {
		// Only one level of braces is allowed. Multiple children indicate
		// that it's not a string constant (it may be an array of strings, like in
		// '{"abc", "def"}', but this error will be handled later).
		if(initializer->Children.Count() > 1) {
			return nullptr;
		}

		initializer = initializer->Children[0];
	}

	if((initializer->Type == InitInfo::ConstExpression) && initializer->Value &&
		initializer->Value->IsStringConst()) {
		return initializer->Value;
	}
	else return nullptr; // '{{"abc"}}' and the like not string constants.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::InitializeString(const Type* type, shared<Expression> initializer, 
							          shared<InitContext> context) {
	if(Expression::IsInvalid(initializer)) {
		return initializer;
	}

	// Use the unqualified version of the type.
	type = type->WithoutQualifiers();

	// The type initialized by a string constant should be either
	// an array (incomplete included) with element type 'char' or a pointer to 'char'.
	// The character type must be identical ('char a[] = L"abc";' is not valid).
	if((type->IsArray() == false) && (type->IsPointer() == false)) {
		diag_->Report(Error::INITIALIZER_STRING_FOR_INVALID_TYPE)<<context->Location;
		return new InvalidExpression();
	}
	
	if(auto temp = type->As<ArrayType>()) {
		// The element type of the array must be compatible with the string.
		if(CheckStringCompatibility(temp->ElementType(), initializer, context) == false) {
			return new InvalidExpression();
		}

		// size <= strlen(initializer): the string is assigned, the rest is initialized
		//                              with the default value.
		// size > strlen(initializer):  only 'size' characters are copied.
		// All this cases are treated when generating the code.
		context->Index++;
		return initializer;
	}

	// This is a pointer. Check that the pointed type is compatible with the string.
	auto temp = type->As<PointerType>();

	if(CheckStringCompatibility(temp->PointeeType(), initializer, context) == false) {
		return new InvalidExpression();
	}

	context->Index++;
	return initializer;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::CheckStringCompatibility(const Type* type, 
                                                   shared<Expression> initializer,
											       shared<InitContext> context) {
	if(Expression::IsInvalid(initializer)) {
		return false;
	}

	// Use the unqualified version of the type.
	StringConstant* stringConst = initializer->As<StringConstant>();
	type = type->WithoutQualifiers();

	if(auto basic = type->As<BasicType>()) {
		if(basic->IsChar() || basic->IsUChar()) {
			if(stringConst->Value().IsWide) {
				// 'char' and 'wchar_t' incompatible.
				diag_->Report(Error::INITIALIZER_STRING_INCOMPATIBLE)<<context->Location;
				return false;
			}

			return true;
		}
		else if(basic->IsWChar()) {
			if(stringConst->Value().IsWide == false) {
				// 'wchar_t' and 'char' incompatible.
				diag_->Report(Error::INITIALIZER_STRING_INCOMPATIBLE)<<context->Location;
				return false;
			}

			return true;
		}
		else {
			// Any other basic type is not compatible.
			diag_->Report(Error::INITIALIZER_STRING_FOR_INVALID_TYPE)<<context->Location;
		}
	}
	else {
		// The element type is not valid.
		diag_->Report(Error::INITIALIZER_STRING_FOR_INVALID_TYPE)<<context->Location;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::HandleArrayDesignator(shared<Designator> designator, const Type* type,
									       shared<InitContext> context) {
	// The object for which the designator is applied must be an array.
	// The index must be an ICE >= 0 (C99:6.7.8.6).
	if(context->ObjectType->IsArray() == false) {
		diag_->Report(Error::DESIGNATOR_EXPECTED_ARRAY)<<context->Location;
		return new InvalidExpression();
	}

	const Type* elementType = context->ObjectType->As<ArrayType>()->ElementType();
	EvaluationInfo eval = designator->ConstExpr->EvaluateAsICE(context_, true /* warn */);
	
	if(eval.IsConstant() == false) {
		diag_->Report(Error::DESIGNATOR_INDEX_NOT_ICE)<<context->Location;
		return new InvalidExpression();
	}
	else if(eval.IsFloatConstant()) {
		diag_->Report(Error::DESIGNATOR_INDEX_FLOAT)<<context->Location;
		return new InvalidExpression();
	}
	else if(eval.Value().IntValue < 0) {
		diag_->Report(Error::DESIGNATOR_INDEX_NEGATIVE)<<context->Location;
		return new InvalidExpression();
	}

	// Mark in the context that something has changed.
	// It will be marked only after the children (if any) have executed.
	// (else the children will reset the flag!).
	designator->Disabled = true;
	shared<Expression> result;

	if(designator->Child) {
		// More designator follow, like in '[1].a = 2'.
		const Type* temp = context->ObjectType;
		context->ObjectType = elementType;

		result = InitializeType(elementType, context, true);
		context->ObjectType = temp;
	}
	else result = InitializeTypeSimple(elementType, designator->Value, context);

	context->HasChanged = true;
	context->NewIndex = (int)eval.Value().IntValue;
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::HandleFieldDesignator(shared<Designator> designator, const Type* type,
									       shared<InitContext> context) {
	// C99:6.7.8.7: a field designator should be used only with
	// 'struct' and 'union' types. The name should be one of the aggregate members.
	if((context->ObjectType->IsStruct() == false) && 
       (context->ObjectType->IsUnion() == false)) {
		diag_->Report(Error::DESIGNATOR_FIELD_NOT_IN_STRUCT)
                      <<context->Location<<*designator->Name;
		return new InvalidExpression();
	}

	// Search for the field.
	auto aggregate = context->ObjectType->As<StructUnionType>();
	auto& fields = aggregate->Fields();
	shared<FieldDeclaration> field;
	int fieldIndex;

	for(int i = 0; i < fields.Count(); i++) {
		if(fields[i]->IsUnnamedBitfield()) {
			// Skip over such fields.
			continue;
		}

		// The flexible array (if any) should not be initialized.
		if(fields[i]->DeclarationType()->IsIncomplete()) {
			diag_->Report(Error::DESIGNATOR_FIELD_FLEX_ARRAY)
                          <<context->Location<<*designator->Name;
			return new InvalidExpression();
		}

		if(*fields[i]->Name() == *designator->Name) {
			// Found it.
			field = fields[i];
			fieldIndex = i; // Used for the index of the member after the designator.
			break;
		}
	}

	if(field == nullptr) {
		diag_->Report(Error::DESIGNATOR_FIELD_NOT_FOUND)
                      <<context->Location<<*designator->Name;
		return new InvalidExpression();
	}

	// Mark in the context that something has changed.
	designator->Disabled = true;
	shared<Expression> result;

	if(designator->Child) {
		// More designator follow, like in '.a.b = 2'.
		const Type* temp = context->ObjectType;
		context->ObjectType = field->DeclarationType();
		result = InitializeType(field->DeclarationType(), context, true);
		context->ObjectType = temp;
	}
	else result = InitializeTypeSimple(field->DeclarationType(), 
                                       designator->Value, context);

	context->HasChanged = true;
	context->NewIndex = fieldIndex;
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::HandleDesignator(shared<Designator> designator, const Type* type,
							          shared<InitContext> context) {
	// Skip over all disabled designators
	while(designator && designator->Disabled) {
		designator = designator->Child;
	}

	// The last designator should be handled directly.
	DebugValidator::IsFalse(designator == nullptr);

	if(designator->IsField) {
		return HandleFieldDesignator(designator, type, context);
	}
	else return HandleArrayDesignator(designator, type, context);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
DeclarationSemantic::FillWithDefault(const Type* type, shared<InitContext> context) {
	// C99:6.7.8.10: Uninitialized type are filled with:
	// - pointer: nullptr
	// - arithmetic: 0
	// - struct/union: all members and submembers according to the above rules.
	//                 For union only the first member.
	type = type->WithoutQualifiers();

	if(type->IsPointer()) {
		// Create an integer constant initialized to 0.
		NumberInfo number;
		number.IsValid = true;
		number.IsInteger = true;
		number.SetIntType(IntType_Int);
		number.IntValue = 0;
		
		return new NumberConstant(number, type, context->Location);
	}
	else if(auto temp = type->As<BasicType>()) {
		NumberInfo number;
		number.IsValid = true;

		if(temp->IsInteger()) {
			number.IsInteger = true;
			number.IntValue = 0;

			if(temp->IsShort())          number.SetIntType(IntType_Short);
			else if(temp->IsUShort())    number.SetIntType(IntType_UShort);
			else if(temp->IsInt())       number.SetIntType(IntType_Int);
			else if(temp->IsUInt())      number.SetIntType(IntType_UInt);
			else if(temp->IsLong())      number.SetIntType(IntType_Long);
			else if(temp->IsULong())     number.SetIntType(IntType_ULong);
			else if(temp->IsLongLong())  number.SetIntType(IntType_LongLong);
			else if(temp->IsULongLong()) number.SetIntType(IntType_ULongLong);

			return new NumberConstant(number, type, context->Location);
		}
		else if(temp->IsFloating()) {
			number.IsInteger = false;
			number.FloatValue = 0.0;

			if(temp->IsFloat()) number.SetFloatType(FloatType_Float);
			else number.SetFloatType(FloatType_Double);

			return new NumberConstant(number, type, context->Location);
		}
		else if(temp->IsChar() || temp->IsWChar() || temp->IsUChar()) {
			CharInfo info;
			info.Value = 0;
			info.IsValid = true;
			info.IsWide = temp->IsWChar();

			return new CharConstant(info, type, context->Location);
		}
	}
	else if(auto temp = type->As<EnumType>()) {
		// The constant of the enum is created based on the
		// basic type used to store the values.
		return FillWithDefault(temp->ConstType(), context);
	}
	else if(auto temp = type->As<ArrayType>()) {
		// For arrays an 'InitializerListExpression' is created that contains
		// default values for all the items. We ignore arrays with function type
		// (can appear, but are not valid and used only for recovery).
		if(temp->ElementType()->IsFunction()) {
			return new InitializerListExpression(context->Location);
		}

		shared<InitializerListExpression> list =
                new InitializerListExpression(context->Location);

		list->SetResultType(temp);

		for(int i = 0; i < temp->Size(); i++) {
			list->InitList().Add(FillWithDefault(temp->ElementType(), context));
		}

		return list;
	}
	else if(auto temp = type->As<StructType>()) {
		// All members of the struct are initialized.
		shared<InitializerListExpression> list = 
                new InitializerListExpression(context->Location);

		list->SetResultType(temp);
		auto& fields = temp->Fields();

		for(int i = 0; i < fields.Count(); i++) {
            auto declarationType = fields[i]->DeclarationType();
			list->InitList().Add(FillWithDefault(declarationType, context));
		}

		return list;
	}
	else if(auto temp = type->As<UnionType>()) {
		// Only the first member is initialized.
		shared<InitializerListExpression> list =
                new InitializerListExpression(context->Location);

		list->SetResultType(temp);
		auto& fields = temp->Fields();

		if(fields.Count() > 0) {
            auto declarationType = fields[0]->DeclarationType();
			list->InitList().Add(FillWithDefault(declarationType, context));
		}

		return list;
	}

	DebugValidator::Unreachable();
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool DeclarationSemantic::AllConstant(shared<Expression> expr) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(expr)) {
		return false;
	}

	// The expression can be wrapped in casts, don't consider them.
	Expression* targetExpr = expr->WithoutCasts();

	if(targetExpr->IsInitListExpr() == false) {
		return targetExpr->IsConstant(context_);
	}
	else return targetExpr->As<InitializerListExpression>()
                          ->IsAllConstant(context_, true /* warn */);
}

} // namespace Parsing