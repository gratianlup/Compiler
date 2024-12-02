// ExpressionSemantic.cpp
// Copyright (c) Lup Gratian
//
// Implements semantic analysis for expressions.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "ExpressionSemantic.hpp"
#include "DeclarationSemantic.hpp"
#include "StatementSemantic.hpp"
#include "SemanticHolder.hpp"
#include "../AST/TypeString.hpp"
using namespace AST;

namespace Parsing {

shared<Expression> 
ExpressionSemantic::HandleNumber(NumberInfo& info, LocationInfo location) {
	if(info.IsValid == false) {
		// The number is not well formed, report an error.
		diag_->Report(Error::INVALID_NUMBER)<<location;
		return new InvalidExpression();
	}
	else if(info.Oveflow) {
		// The number is greater that the maximum supported value, report an error.
		diag_->Report(Error::NUMBER_TOO_LARGE)<<location;
		return new InvalidExpression();
	}

	// Select the type based on specifiers like u/l/ll found in the source.
	// The smallest such number is chosen and overflow reported.
	const Type* type = nullptr;
	bool warn = false;
	const TargetData* target = context_->Target();
	DiagnosticCode code;

	if(info.IsInteger) {
		if(info.IsInt()) {
			if(IntFits(info.IntValue, TypeKind::Int)) {
				type = BasicType::GetInt();
			}
		}
		
		// If the number doesn't fit in an 'int' check if it does in an 'unsigned'.
		if((type == nullptr) && 
           (GetIntRank(info) <= GetIntRank(BasicType::GetUInt()))) {
			if(IntFits(info.IntValue, TypeKind::UInt)) {
				type = BasicType::GetUInt();
			}
			else if(info.IsUInt()) {
				warn = true; // Warn if the number is too large.
				code = Warning::NUMBER_TOO_LARGE;
			}
		}

		// Check if it fits in a 'long'.
		if((type == nullptr) && 
           (GetIntRank(info) <= GetIntRank(BasicType::GetLong()))) {
			if(IntFits(info.IntValue, TypeKind::Long)) {
				type = BasicType::GetLong();
			}
		}

		// Check if it fits in an 'unsigned long'.
		if((type == nullptr) && 
           (GetIntRank(info) <= GetIntRank(BasicType::GetULong()))) {
			if(IntFits(info.IntValue, TypeKind::ULong)) {
				type = BasicType::GetULong();
			}
			else if(info.IsULong()) {
				warn = true;
				code = Warning::NUMBER_TOO_LARGE;
			}
		}

		// Check if it fits in a 'long long'.
		if((type == nullptr) && 
           (GetIntRank(info) <= GetIntRank(BasicType::GetLongLong()))) {
			if(IntFits(info.IntValue, TypeKind::LongLong)) {
				type = BasicType::GetLongLong();
			}
		}

		// The only remaining type is 'unsigned long long'.
		if(type == nullptr) {
			type = BasicType::GetULongLong();

			// Report that a signed number couldn't be fit.
			if((info.IsULongLong() == false) && info.IsSigned()) {
				warn = true;
				code = Warning::NUMBER_TOO_LARGE_SIGNED;
			}
		}

		// Report the warning, if any.
		if(warn) diag_->Report(code)<<location;
	}
	else {
		if(info.IsFloat()) type = BasicType::GetFloat();
		else type = BasicType::GetDouble();
	}

	// Create the constant expression.
	return new NumberConstant(info, type, location);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleCharacter(CharInfo& info, LocationInfo location) {
	if(info.IsValid == false) {
		// We only warn about invalid characters.
		diag_->Report(Warning::CHARACTER_INVALID)<<location;
	}

	// Warn about multiple characters in the same constant (for ex. L'ab').
	if(info.MultipleChars) {
		diag_->Report(Warning::CHARACTER_MULTIPLE_CHARS)<<location;
	}

	auto charType = info.IsWide ? BasicType::GetWChar() : BasicType::GetChar();
	return new CharConstant(info, charType, location);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleString(StringInfo& info, LocationInfo location) {
	// The type of a string is a constant character array.
	ArrayType* stringType = 
			types_->GetArray(info.IsWide ? BasicType::GetWChar() : BasicType::GetChar(),
							 info.Value.Length() /* includes string terminator */,
							 false /* isStatic */, Qualifier().SetHasConst(true));

	return new StringConstant(info, stringType, location);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleIdentifier(Identifier& ident, 
                                     shared<DeclarationContext> context) {
	// Check that a declaration having the same identifier is visible
	// in the current scope (or parent scopes).
	DeclarationContext* temp;
	Declaration* declaration = context->Find(&ident, &temp);

	if(declaration == nullptr) {
		// Couldn't find any suitable declaration.
		// Verify if this is a reference to '__func__', which represents the name
		// of the current function. It must be declared implicitly when referenced
		// the first time as 'static const char __func__[N] = "FUNCTION_NAME"'.
		declaration = CheckBuiltinFunc(ident, context);
	}

    if(declaration == nullptr) {
        // If we're parsing at prototype scope it means that we're inside
        // a VLA array expression. This may be a parameter that was declared
        // before this one, so we need to check if it's the case, and create
        // a variable and insert it into the current scope if necessary.
        // For example, in 'void f(int n, int a[n]);' 'n' needs to be created.
        if(context->IsFunctProtoScope()) {
            DebugValidator::IsNotNull(context->Object());
            auto functInfo = static_cast<DeclaratorInfo*>(context->Object());

            Holder()->GetDeclarationSemantic()->CreatePrototypeVariables(functInfo, context);
            declaration = context->Find(&ident, &temp);
        }
    }

    if(declaration == nullptr) {
        diag_->Report(Error::EXPRESSION_IDENTIFIER_NOT_FOUND)<<ident;
        return new InvalidExpression();
    }

	// Use the last available declaration (important especially for functions).
	declaration = declaration->LastDeclaration();

	// The declaration should designate an object or a function (C99:6.5.1.2).
	if((declaration->IsVariableDecl() == false) && 
       (declaration->IsFunctionDecl() == false) &&
	   (declaration->IsEnumConstDecl() == false)) {
		diag_->Report(Error::EXPRESSION_REFERENCE_INVALID)<<ident;
		return new InvalidExpression();
	}

	// Create the declaration expression.
	return new DeclarationExpression(declaration, declaration->DeclarationType(), 
                               ident.Location());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declaration* ExpressionSemantic::CheckBuiltinFunc(Identifier& ident, 
                                                  shared<DeclarationContext> context) {
	// Handles built-in identifiers. The only handled identifier, as for now, is
	// '__func__', which is replaced by a string having the name of the current function.
	if(ident.Name() != "__func__") return nullptr;

	auto activeFunct = Holder()->GetStatementSemantic()->ActiveFunction();

	if(activeFunct == nullptr) {
		// If we're not in a function report the usual error.
		return nullptr;
	}

	// Create the declaration if not already created.
	// We make it the first declaration in the compound statement.
	Declaration* declaration = nullptr;
	CompoundStatement* body = activeFunct->Body();

	if(body) {
		const Statement* first = body->Children()[0];

		if(auto declStatement = first->As<DeclarationStatement>()) {
			if(auto variableDecl = declStatement->Base()->As<VariableDeclaration>()) {
				if(*variableDecl->Name() == "__func__") {
					// Name already created, make a reference to it.
					declaration = variableDecl;
				}
			}
		}
	}

    if(declaration) {
        return declaration;
    }
    else {
		// The declaration was not found, create it now.
		return CreateBuiltinFunc(context);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declaration* ExpressionSemantic::CreateBuiltinFunc(shared<DeclarationContext> context) {
    auto activeFunct = Holder()->GetStatementSemantic()->ActiveFunction();
    LocationInfo location = activeFunct->StartLocation();
	Identifier* functionName = activeFunct->Name();

	// static const char __func__[N] = FUNCTION_NAME
	auto elementType = types_->GetQualified(BasicType::GetChar(),
                                            Qualifier::GetConst());
    int length = functionName->Name().Length() + 1;
	auto arrayType = types_->GetArray(elementType, length, 
										false /* isStatic */, Qualifier());

	StringInfo nameInfo;
	nameInfo.IsValid = true;
	nameInfo.IsWide = false;
	nameInfo.Value = functionName->Name();

	shared<Expression> initExpr = new StringConstant(nameInfo, arrayType, location);
	shared<VariableDeclaration> variableDecl = 
			new VariableDeclaration(new Identifier("__func__", location), arrayType, 
								    location, location, initExpr);

	variableDecl->SetLinkage(LinkageType::Internal);
	variableDecl->SetStorage(StorageType::Static);

    // Now create the variable declaration.
	shared<Statement> declStatement = new DeclarationStatement(variableDecl, location);
	CompoundStatement* body = new CompoundStatement(location);

	body->Children().Insert(0, declStatement);
	activeFunct->SetBody(body);
    return variableDecl;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleSizeof(shared<Expression> target,  LocationInfo startLocation,
						         LocationInfo endLoc, shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
		return target;
	}

	// C99:6.5.3.4.1: the type of the expression should not be 
	// a function type, an incomplete type or a bitfield.
    const Type* type = target->ResultType()->WithoutQualifiers();

	if(type->IsFunction()) {
		diag_->Report(Error::SIZEOF_FOR_FUNCTION_TYPE)<<target->Location();
		return new InvalidExpression();
	}
	else if(type->IsIncomplete()) {
		diag_->Report(Error::SIZEOF_FOR_INCOMPLETE_TYPE)<<target->Location();
		return new InvalidExpression();
	}
	else if(target->GetBitfield()) {
		// It's a bitfield.
		diag_->Report(Error::SIZEOF_FOR_BITFIELD)<<target->Location();
		return new InvalidExpression();
	}

	// All seems well, create the expression. 'size_t' represents the size.
	return new SizeofOperator(target, BasicType::GetSizeT(), startLocation, endLoc);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleCast(const Type* castType, shared<Expression> target,
						       LocationInfo castLoc, shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
		return target;
	}

	// Validates and creates an explicit conversion between types.
	// The type to convert to must be either 'void' or a qualified or
	// unqualified scalar type (integer, floating or pointer).
	if(castType->IsVoid()) {
		// A cast to void accepts any target type, so we're done.
		if(target->ResultType()->IsVoid()) {
			// No cast is needed if the types are the same.
			return target;
		}

		return new CastExpression(CastType::ToVoid, target, castType, 
                            	  true /* isExplicit */, castLoc);	  
	}

	if(castType->IsScalar() == false) {
		diag_->Report(Error::CAST_TYPE_NOT_SCALAR)<<target->Location();
		return new InvalidExpression();
	}

	// Usual conversions are applied to the target type first
	// (else something like 'int a[3]; int *p = (int*)a;' would not be valid).
	target = PromoteFunctionArrayExpr(target);
    const Type* targetType = target->ResultType()->WithoutQualifiers();

	if(targetType->IsScalar() == false) {
		diag_->Report(Error::CAST_TARGET_NOT_SCALAR)<<target->Location();
		return new InvalidExpression();
	}

	// C99:6.5.4.2.3: 'pointer from floating' and 'floating to pointer' is invalid.
	// Catch things like 'float a; int* p = (int*)a;' and
	// 'int *p; float a = p;'. Note that 'int a; int* p = (int*)a' is valid though.
	if(castType->IsPointer() && targetType->IsArithmetic()) {
		if(targetType->IsInteger() == false) { // (pointer from integer is valid)
			// Pointer from floating type.
			diag_->Report(Error::CAST_POINTER_FROM_FLOATING_TYPE)<<target->Location();
			return new InvalidExpression();
		}
	}
	else if(targetType->IsPointer() && castType->IsArithmetic()) {
		if(castType->IsInteger() == false) { // (integer from pointer is valid)
			// Floating type from pointer.
			diag_->Report(Error::CAST_FLOATING_FROM_POINTER_TYPE)<<target->Location();
			return new InvalidExpression();
		}
	}

	// If the types are the same no cast needs to be done (C99:6.5.4.4).
	// The unqualified variants are tested.
	if(castType->WithoutQualifiers()->Equals(targetType->WithoutQualifiers())) {
		return target;
	}

	// The cast must be done. Figure out it's type.
	CastType cast;

	if(castType->IsPointer()) {
		if(targetType->IsInteger()) cast = CastType::IntToPointer;
		else cast = CastType::ToPointer;
	}
	else if(castType->IsInteger()) {
		if(targetType->IsInteger()) cast = CastType::IntToInt;
		else if(targetType->IsFloating()) cast = CastType::FloatToInt;
		else cast = CastType::PointerToInt;
	}
	else {
		if(targetType->IsFloating()) cast = CastType::FloatToFloat;
		else cast = CastType::IntToFloat;
	}

	// Create the cast.
	return new CastExpression(cast, target, castType, 
                              true /* isExplicit */, castLoc);
}	

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleCompoundLiteral(const Type* type, shared<InitInfo> initializer, 
                                          LocationInfo location, 
                                          shared<DeclarationContext> context) {
	// C99:6.5.2.5.1: the type should not be a VLA.
	if(type->IsVariable() && type->IsArray()) {
		diag_->Report(Error::COMPOUND_VLA_TYPE)<<location;
		return new InvalidExpression();
	}

	// Apply the initializer. It should contain only constant values if
	// the compound literal is found at file level (C99:6.5.2.5.2).
	shared<InitContext> initCtx = new InitContext(0, type, initializer, location);
	
	shared<Expression> initExpr = Holder()->GetDeclarationSemantic()
                                          ->MakeInitializer(type, initCtx);
	if(Expression::IsInvalid(initExpr)) {
		// The initializer is invalid, errors already reported.
		return initExpr;
	}

	if(context->IsFileScope() && 
       (Holder()->GetDeclarationSemantic()->AllConstant(initExpr) == false)) {
		diag_->Report(Error::INITIALIZER_FOR_LINKED_NOT_CONSTANT)<<location;
		return new InvalidExpression();
	}

	// If the type is an incomplete array it is completed by the initializer.
	// (int []) {1,2,3} -> (int [3]) {1,2,3}
	const Type* newType = Holder()->GetDeclarationSemantic()
                                  ->CompleteWithInitializer(type, initExpr);
	return new CompoundExpression(initExpr, newType ? newType : type, location);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleSubscript(shared<Expression> base, shared<Expression> index, 
						            LocationInfo leftLoc, LocationInfo rightLoc,
						            shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(base) || Expression::IsInvalid(index)) {
		return new InvalidExpression();
	}

	// First perform the default conversions, so that something like
	// 'int a[3]' is converted to 'int* &a[0]'.
	base  = PromoteFunctionArrayExpr(base);
	index = PromoteFunctionArrayExpr(index);

	// Subscript expression can be written either as base[index] or as index[base] 
	// (something like '3[a]' is valid); figure out which order is used.
	shared<Expression> realBase;
	shared<Expression> realIndex;
	const Type* resultType;

	if(base->ResultType()->WithoutQualifiers()->IsPointer()) {
		// The usual case ('a[3]').
		realBase = base;
		realIndex = index;
	}
	else {
		// The unusual case ('3[a]').
		realBase = index;
		realIndex = base;
	}

	// The base should be a pointer and the index an integer.
	if(auto temp = realBase->ResultType()->WithoutQualifiers()->As<PointerType>()) {
		resultType = temp->PointeeType();
        
		// The type should not be an incomplete type.
		if(resultType->IsIncomplete()) {
			diag_->Report(Error::SUBSCRIPT_INCOMPLETE_RESULT_TYPE)<<base->Location();
			return new InvalidExpression();
		}
	}
	else {
		// Invalid base. Handle subscript on function separately.
		if(realBase->ResultType()->IsFunction()) {
			diag_->Report(Error::SUBSCRIPT_ON_FUNCTION_TYPE)<<base->Location();
			return new InvalidExpression();
		}

		// The same error for all other cases.
		diag_->Report(Error::SUBSCRIPT_BASE_INVALID)<<base->Location();
		return new InvalidExpression();
	}

	// The index should be an integer type.
	if(realIndex->ResultType()->WithoutQualifiers()->IsInteger() == false) {
		// Emit separate error for floating types.
		if(realIndex->ResultType()->IsFloating()) {
			diag_->Report(Error::SUBSCRIPT_INDEX_FLOAT)<<base->Location();
			return new InvalidExpression();
		}

		diag_->Report(Error::SUBSCRIPT_INDEX_NOT_INT)<<base->Location();
		return new InvalidExpression();
	}

	// Catch 'void *' as base (GCC seems to allow it, but it really shouldn't).
	if(resultType->IsVoid()) {
		diag_->Report(Error::SUBSCRIPT_BASE_VOID_POINTER)<<base->Location();
		return new InvalidExpression();
	}

	// Create the subscript expression.
	return new SubscriptExpression(realBase, realIndex, resultType, 
                             leftLoc, rightLoc);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleMember(shared<Expression> object, shared<Identifier> name,
						         bool isPointer, shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(object)) {
		return object;
	}

	// The object should have either 'struct' or 'union' type 
	// (or pointer to one of these types if 'isPointer' is set).
	// The type qualifiers are removed when determining the object type.
	const Type* objectType = object->ResultType()->WithoutQualifiers();
	const Type* structType = nullptr;

	if(auto pointerType = objectType->As<PointerType>()) {
		// If it's a pointer 'isPointer' should be set, else it's
		// an attempt to access a struct/union pointer like an normal object.
		const Type* pointee = pointerType->PointeeType()->WithoutQualifiers();

		if(pointee->IsStruct() || pointee->IsUnion()) {
			if(isPointer == false) {
				// Something like 'struct A *a; a.smth'. Even if it's an error
				// don't stop so that we can parse the rest of the expression.
				diag_->Report(Error::MEMBER_POINTER_EXPECTED)<<object->Location();
			}

			structType = pointee;
		}
		else {
			// The pointed object is not valid.
			diag_->Report(Error::MEMBER_OBJECT_INVALID)<<object->Location();
			return new InvalidExpression();
		}
	}
	else {
		// The object type should be struct/union. If 'isPointer' is set
		// it's an error because the object is accessed like a pointer.
		if(objectType->IsStruct() || objectType->IsUnion()) {
			if(isPointer) {
				// Something like 'struct A a; a->smth'. Even if it's an error
				// don't stop so that we can parse the rest of the expression.
				diag_->Report(Error::MEMBER_POINTER_INVALID)<<object->Location();
			}
			
			structType = objectType;
		}
		else {
			// The pointed object is not valid.
			diag_->Report(Error::MEMBER_OBJECT_INVALID)<<object->Location();
			return new InvalidExpression();
		}
	}

	// The type can be incomplete (because of forward declarations). 
	// This can happen only if the struct/union is accessed through a pointer.
	if(structType->IsIncomplete()) {
		diag_->Report(Error::MEMBER_INCOMPLETE_TYPE)<<object->Location();
		return new InvalidExpression();
	}

	// See if there is a field declaration with the specified name.
	DeclarationContext* bodyCtx = nullptr;

	if(auto temp = structType->As<StructType>()) {
		bodyCtx = temp->BodyContext();
	}
	else bodyCtx = structType->As<UnionType>()->BodyContext();

	shared<Declaration> field = bodyCtx->FindMember(name);
	if(field == nullptr) {
		// The field couldn't be found. Try to suggest a member that could have been
		// used instead. Useful for typos like a.Exrpession -> a.Expression.
		diag_->Report(Error::MEMBER_NOT_FOUND)<<*name;
		SuggestMember(structType->As<StructUnionType>(), name);
		return new InvalidExpression();
	}

	// The field was found, now figure out the type of the expression.
	// If the member is an array we need to promote it to a pointer here, 
	// before adding any qualifiers.
	const Type* fieldType = field->DeclarationType();
	shared<Expression> expr = new MemberExpression(object, name, field, isPointer, 
											 fieldType, object->Location());
	
	expr = PromoteFunctionArrayExpr(expr, false /* removeQualifiers */);

	// The qualifiers from the field and from the object need to be combined.
	// Note that for pointer access -> we need to look on the qualifiers applied
	// on the pointee, not on the pointers. For example, in 'struct ABC * const p; p->A;'
	// 'p->A' is not qualified, because the -pointer- is constant, not the struct.
	const QType* objectQual;

	if(isPointer) {
		auto pointerType = objectType->As<PointerType>();
		objectQual = pointerType->PointeeType()->As<QType>();
	}
	else objectQual = object->ResultType()->As<QType>();

	if(objectQual) {
        // We have qualifiers on the base object.
		Qualifier qual = objectQual->GetQualifiers();

		if(auto temp = field->DeclarationType()->As<QType>()) {
			qual = qual.Combine(temp->GetQualifiers());
		}

		expr->SetResultType(types_->GetQualified(expr->ResultType(), qual));
	}	   

	return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionSemantic::SuggestMember(const StructUnionType* type, 
                                       shared<Identifier> name) {
	struct Suggestion {
		FieldDeclaration* Field;
		int Distance;

		bool operator== (const Suggestion& other) {
			return Distance == other.Distance;
		}

		bool operator< (const Suggestion& other) {
			return Distance < other.Distance;
		}
	};

	// Don't make suggestions for small fields fields.
	if(name->Name().Length() <= 2) return;

	// Make a list of suggestions. These will be then sorted and
	// the suggestion with the minimum distance is chosen.
	auto& fields = type->Fields();
	List<Suggestion> suggestions;

	for(int i = 0; i < fields.Count(); i++) {
		FieldDeclaration* field = fields[i];
		int dist;

		if(EditDistance::AreClose(field->Name(), name, 2, &dist)) {
			Suggestion s = { field, dist };
			suggestions.Add(s);
		}
	}

	if(suggestions.Count() == 0) return; // No suggestions.

	// Sort ascending, based on the number of differences.
	suggestions.Sort();

	// If there are at least 2 suggestions, and the first two most probable
	// have the same score don't suggest any of them.
	if((suggestions.Count() >= 2) &&
	   (suggestions[0] == suggestions[1])) {
		return;
	}

	Suggestion& s = suggestions[0];
	diag_->Report(Warning::MEMBER_SUGGESTION)<<*s.Field->Name();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleCallBegin(shared<Expression> target, 
                                    shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
		return target;
	}

	// Do the default promotion on the expression first.
	// The type of the expression should become 'pointer to function type'.
	shared<Expression> adjustedTarget = PromoteFunctionExpr(target);
    const Type* adjustedTargetType = adjustedTarget->ResultType();
	const FunctionType* functionType = nullptr;

	if(auto pointerType = adjustedTargetType->WithoutQualifiers()->As<PointerType>()) {
		auto pointeeType = pointerType->PointeeType()->WithoutQualifiers();

		if((functionType = pointeeType->As<FunctionType>()) == nullptr) {
			// Not a pointer to function, report.
			diag_->Report(Error::CALL_TARGET_NOT_FUNCTION)<<target->Location();
			return new InvalidExpression();
		}
	}
	else {
		// Not a pointer, report.
		diag_->Report(Error::CALL_TARGET_NOT_FUNCTION)<<target->Location();
		return new InvalidExpression();
	}

	// The type returned by the function should not be an incomplete type.
	// 'void' is the only incomplete type allowed (C99:6.5.2.2.1).
	const Type* returnType = functionType->ReturnType();

	if((returnType->IsVoid() == false) && returnType->IsIncomplete()) {
		diag_->Report(Error::CALL_INCOMPLETE_RETURN_TYPE)<<target->Location();
		return new InvalidExpression();
	}
	
	// Create the expression. The rest will be validated
    // when the parameters are available. The result type 
    // of a call is the type returned by the function (C99:6.5.2.2.5).
	return new CallExpression(adjustedTarget, returnType, target->Location());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleCallEnd(shared<Expression> expr, LocationInfo startLocation, 
							      LocationInfo endLoc, shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(expr)) {
		return new InvalidExpression();
	}

	CallExpression* callExpr = expr->As<CallExpression>();

	// The whole call expression is available now.
	// If the function has a prototype the arguments are converted as for assignment;
	// the binary conversions are performed, and the number of arguments and varargs
	// is checked. If no prototype is available the default argument promotions 
	// are performed and no checks are made.
	const FunctionType* functionType = nullptr;
	bool hasPrototype;

	// First try to obtain the declaration of the function.
	// If one is found use it's type, else use the type of the expression.
	if(auto functDecl = callExpr->GetDeclaration()) {
		// Something like 'foo(a,b);'.
		functionType = functDecl->DeclarationType();
		hasPrototype = functionType->HasPrototype();
	}
	else {
		// Something like 'a->fptr(a,b);', where 'fptr' is a pointer to function.
		auto returnType = callExpr->Function()->ResultType();
		auto pointerType = returnType->WithoutQualifiers()->As<PointerType>();

		functionType = pointerType->PointeeType()->As<FunctionType>();
		hasPrototype = functionType->HasPrototype();
	}

	// Validate and convert the parameters.
	if(hasPrototype) {
		if(HandlePrototypeArguments(callExpr, functionType) == false) {
			return new InvalidExpression(); // Errors already emitted.
		}
	}
	else {
		// Warn about this situation (but only if arguments are present).
		if(callExpr->ArgCount() > 0) {
			diag_->Report(Warning::FUNCTION_CALL_NO_PROTOTYPE)<<startLocation;
		}

		// All parameters are converted using the default argument promotions
		// (even if subsequent declaration./definition specifies a smaller number of args.).
		auto& arguments = callExpr->Arguments();

		for(int i = 0; i < arguments.Count(); i++) {
			shared<Expression> promoted = PromoteArgumentDefault(arguments[i]);

			if(promoted->ResultType()->IsIncomplete()) {
				// An incomplete type cannot be passed as an argument.
				diag_->Report(Error::CALL_INCOMPLETE_ARGUMENT_TYPE)<<arguments[i]->Location();
				return new InvalidExpression();
			}

			arguments[i] = promoted;
		}
	}

	return expr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool ExpressionSemantic::HandlePrototypeArguments(CallExpression* callExpr, 
                                                  const FunctionType* functionType) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(callExpr)) {
		return true;
	}

	// C99:6.5.2.3.7: the number of arguments must match. Arguments are converted
	// as by assignment. If the function is varargs (...) all arguments that 
	// follow use the default argument promotion.
	auto& arguments = callExpr->Arguments();
	auto& parameters = functionType->Parameters();

	if(arguments.Count() != parameters.Count()) {
		if(arguments.Count() > parameters.Count()) {
			// Too many arguments; it's accepted only if the function is varargs.
			if(functionType->IsVarargs() == false) {
				// Report and remove the arguments so that we can continue.
				diag_->Report(Error::CALL_ARGUMENTS_TOO_MANY)<<callExpr->Location();

				while(arguments.Count() != parameters.Count()) {
					arguments.RemoveAt(arguments.Count() - 1);
				}
			}
		}
		else {
			// Too few arguments. Report and continue, so that checks are made.
			diag_->Report(Error::CALL_ARGUMENTS_TOO_FEW)<<callExpr->Location();
		}
	}

	// The assignment conversion is used until '...' is found.
	// Ex: 'void f(int a, int b, ...)' - 'a' and 'b' use assignment conversion;
	// any other parameter will use the standard argument conversion.
	int limit = arguments.Count() < parameters.Count() ? 
                arguments.Count() : parameters.Count();
	bool invalid = false;

	for(int i = 0; i < limit; i++) {
		// Typecheck and make the conversion (if necessary).
		if(IsSimpleAssignmentValid(parameters[i], arguments[i], nullptr)) {
			// Assignment is valid, now convert the right expression
			// to the left (if the types are the same no conversion will be done).
			// C99:6.5.2.3.7: the unqualified version of 'left' is considered.
			const Type* destType = parameters[i]->WithoutQualifiers();

			// Warn if the qualifiers are dropped.
			Identifier* argName = arguments[i]->Is<DeclarationExpression>() ?
								  arguments[i]->As<DeclarationExpression>()->Object()->Name() :
                                  nullptr;
			WarnQualifierDrop(destType, arguments[i]->ResultType(), nullptr /* expr */,
							  argName, true /* isArg */, arguments[i]->Location());

			arguments[i] = CreateImplicitCast(arguments[i], destType, CastType::Unknown);
		}
		else {
			// The types aren't compatible, but continue so that we check
			// the remaining argument/parameter pairs.
			invalid = true;
		}
	}

	if(invalid) {
		// At lest one of the arguments isn't compatible with the parameter, fail.
		return false;
	}

	// The rest of the arguments use the standard promotion if varargs.
	for(int i = limit; i < arguments.Count(); i++) {
		shared<Expression> promoted = PromoteArgumentDefault(arguments[i]);
		if(promoted->ResultType()->IsIncomplete()) {
			// An incomplete type cannot be passed as an argument.
			diag_->Report(Error::CALL_INCOMPLETE_ARGUMENT_TYPE)<<arguments[i]->Location();
			return false;
		}

		arguments[i] = promoted;
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleAddressOf(shared<Expression> target, 
                                    shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
		return target;
	}

	// The target should be either a function designator, 
    // the result of * or [], or an object that is an lvalue, 
    // but not a bitfield or declared with 'register'.
    // C99:6.5.3.2.3: &*E -> E
	if(auto temp = target->As<UnaryOperator>()) {
		if(temp->Operator() == UnaryOpType::Indirection) {
			return temp->Value(); // Strip off the *
		}
	}

	const Type* type = target->ResultType()->WithoutQualifiers();

	if(type->IsFunction()) {
        // OK
    } 
	else if(IsLValue(target) == false) {
		diag_->Report(Error::ADDRESS_OF_INCOMPLETE_TYPE)<<target->Location();
		return new InvalidExpression();
	}
	else if(target->GetBitfield()) {
		diag_->Report(Error::ADDRESS_OF_BITFIELD)<<target->Location();
		return new InvalidExpression();
	}
	else if(auto temp = target->As<DeclarationExpression>()) {
		if(temp->Object()->Storage() == StorageType::Register) {
			diag_->Report(Error::ADDRESS_OF_REGISTER)<<target->Location();
			return new InvalidExpression();
		}
	}

	// Create the expression.
	return new UnaryOperator(UnaryOpType::Address, target, 
                             types_->GetPointer(target->ResultType()),
					         false /* postfix */, target->Location());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleIndirection(shared<Expression> target,
                                      shared<DeclarationContext> context) {
	// If the expression is invalid we have nothing to do.
	if(Expression::IsInvalid(target)) {
		return target;
	}

	// The target of * should be a pointer type.
    auto resultType = target->ResultType()->WithoutQualifiers();

	if(resultType->IsPointer() == false) {
		diag_->Report(Error::INDIRECTION_NOT_ON_POINTER)<<target->Location();
		return new InvalidExpression();
	}

	// The resulting type is the type of the pointee.
	// Catch 'void *' as a target (GCC seems to allow it, but it really shouldn't).
	const Type* pointeeType = resultType->As<PointerType>()->PointeeType();

	if(pointeeType->IsVoid()) {
		diag_->Report(Error::INDIRECTION_TARGET_VOID_POINTER)<<target->Location();
		return new InvalidExpression();
	}

	// Create the expression. The result type is the pointed type.
	return new UnaryOperator(UnaryOpType::Indirection, target, pointeeType,
					   false /* postfix */, target->Location());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleUnaryOp(TokenKind kind, shared<Expression> target,
							      shared<DeclarationContext> context) {
	// This is used only to dispatch to the appropriate method.
	switch(kind) {
		case TokenKind::Inc:   return HandleIncDecOp(target, true  /* isIncrement */, 
											    false /* isPostfix */, context);
		case TokenKind::Dec:   return HandleIncDecOp(target, false /* isIncrement */,
												false /* isPostfix */, context);
		case TokenKind::And:	  return HandleAddressOf(target, context);
		case TokenKind::Mul:   return HandleIndirection(target, context);
		case TokenKind::Add:   return HandlePrefixAddSubOp(target, true  /* isAdd */, context);
		case TokenKind::Sub:   return HandlePrefixAddSubOp(target, false /* isAdd */, context);
		case TokenKind::Tilde: return HandleComplementOp(target, context);
		case TokenKind::Not:   return HandleNotOp(target, context);
	}

	DebugValidator::Unreachable(); // Should not be reached.
	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandlePostfixIncDecOp(shared<Expression> target, bool isIncrement, 
								          shared<DeclarationContext> context) {
	// Use the same method as for prefix ++ and -- because the same rules apply.
	return HandleIncDecOp(target, isIncrement, true /* isPostfix */, context);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BinaryOpType ExpressionSemantic::BinaryOpFromToken(TokenKind kind) {
	switch(kind) {
		case TokenKind::Comma:     return BinaryOpType::Comma;
		case TokenKind::Eq:        return BinaryOpType::Eq;     
		case TokenKind::AddEq:     return BinaryOpType::AddEq;    
		case TokenKind::SubEq:     return BinaryOpType::SubEq;    
		case TokenKind::MulEq:     return BinaryOpType::MulEq;    
		case TokenKind::DivEq:     return BinaryOpType::DivEq;    
		case TokenKind::ModEq:     return BinaryOpType::ModEq;    
		case TokenKind::AndEq:     return BinaryOpType::AndEq;    
		case TokenKind::OrEq:      return BinaryOpType::OrEq;    
		case TokenKind::XorEq:     return BinaryOpType::XorEq;    
		case TokenKind::ShiftLEq:  return BinaryOpType::ShiftLEq;
		case TokenKind::ShiftREq:  return BinaryOpType::ShiftREq; 
		case TokenKind::OrOr:      return BinaryOpType::OrOr;
		case TokenKind::AndAnd:    return BinaryOpType::AndAnd;
		case TokenKind::Or:        return BinaryOpType::Or;
		case TokenKind::Xor:       return BinaryOpType::Xor;
		case TokenKind::And:       return BinaryOpType::And;
		case TokenKind::EqEq:      return BinaryOpType::EqEq;
		case TokenKind::NotEq:     return BinaryOpType::NotEq;
		case TokenKind::Less:      return BinaryOpType::Less;    
		case TokenKind::LessEq:    return BinaryOpType::LessEq;
		case TokenKind::Greater:   return BinaryOpType::Greater;
		case TokenKind::GreaterEq: return BinaryOpType::GreaterEq;
		case TokenKind::ShiftR:    return BinaryOpType::ShiftR;
		case TokenKind::ShiftL:    return BinaryOpType::ShiftL;
		case TokenKind::Add:       return BinaryOpType::Add;
		case TokenKind::Sub:       return BinaryOpType::Sub;
		case TokenKind::Mul:       return BinaryOpType::Mul;
		case TokenKind::Div:       return BinaryOpType::Div;
		case TokenKind::Mod:  	  return BinaryOpType::Mod;
	}

	DebugValidator::Unreachable(); // Should not be reached.
	return BinaryOpType::Comma;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<Expression> 
ExpressionSemantic::HandleBinaryOp(TokenKind kind, shared<Expression> left, 
							       shared<Expression> right, LocationInfo operandLocation, 
							       shared<DeclarationContext> context) {
	const Type* type;
	BinaryOpType opType = BinaryOpFromToken(kind);

	switch(kind) {
	case TokenKind::Comma:     { type = HandleCommaOp(left, right, BinaryOpType::Comma, context); break; }
						   
	case TokenKind::Eq:        { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::AddEq:     { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::SubEq:     { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::MulEq:     { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::DivEq:     { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::ModEq:     { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::AndEq:     { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::OrEq:      { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::XorEq:     { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::ShiftLEq:  { type = HandleAssignmentOp(left, right, opType, context); break; }
	case TokenKind::ShiftREq:  { type = HandleAssignmentOp(left, right, opType, context); break; }
						  
	case TokenKind::OrOr:      { type = HandleLogicalOp(left, right, opType, context); break; }
	case TokenKind::AndAnd:    { type = HandleLogicalOp(left, right, opType, context); break; }
						  
	case TokenKind::Or:        { type = HandleBitwiseOp(left, right, opType, false, context); break; }
	case TokenKind::Xor:       { type = HandleBitwiseOp(left, right, opType, false, context); break; }
	case TokenKind::And:       { type = HandleBitwiseOp(left, right, opType, false, context); break; }
						   
	case TokenKind::EqEq:      { type = HandleComparisonOp(left, right, opType, context, operandLocation); break; }
	case TokenKind::NotEq:     { type = HandleComparisonOp(left, right, opType, context, operandLocation); break; }
	case TokenKind::Less:      { type = HandleComparisonOp(left, right, opType, context, operandLocation); break; }
	case TokenKind::LessEq:    { type = HandleComparisonOp(left, right, opType, context, operandLocation); break; }
	case TokenKind::Greater:   { type = HandleComparisonOp(left, right, opType, context, operandLocation); break; }
	case TokenKind::GreaterEq: { type = HandleComparisonOp(left, right, opType, context, operandLocation); break; }
						  
	case TokenKind::ShiftR:    { type = HandleShiftOp(left, right, opType, false, context); break; }
	case TokenKind::ShiftL:    { type = HandleShiftOp(left, right, opType, false, context); break; }

	case TokenKind::Add:       { type = HandleAdditiveOp(left, right, opType, false, context); break; }
	case TokenKind::Sub:       { type = HandleAdditiveOp(left, right, opType, false, context); break; }
						  
	case TokenKind::Mul:       { type = HandleMultiplicativeOp(left, right, opType, false, context); break; }
	case TokenKind::Div:       { type = HandleMultiplicativeOp(left, right, opType, false, context); break; }
	case TokenKind::Mod:  	  { type = HandleMultiplicativeOp(left, right, opType, false, context); break; }
	}

	if(type == nullptr) {
		// The operator is not valid. Diagnostics already emitted.
		return new InvalidExpression();
	}
	
	return new BinaryOperator(opType, left, right, type, operandLocation);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionSemantic::DiagnoseBinaryOp(shared<Expression> left, shared<Expression> right,
									      DiagnosticCode code, BinaryOpType opType) {
	DiagnoseBinaryOp(left->ResultType(), right, code, opType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ExpressionSemantic::DiagnoseBinaryOp(const Type* left, shared<Expression> right,
									      DiagnosticCode code, BinaryOpType opType) {
	diag_->Report(code)<<right->Location()<<BinaryOperator::OperatorString(opType)<<
						 TypeString(left).ToString()<<
						 TypeString(right->ResultType()).ToString();
}

} // namespace Parsing