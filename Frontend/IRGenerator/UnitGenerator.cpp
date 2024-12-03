// UnitGenerator.hpp
// Copyright (c) Lup Gratian
//
// Implements the generator for the global symbols at the unit level.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "UnitGenerator.hpp"
#include "../IR/IRPrinter.hpp"

namespace IRGenerator {

UnitGenerator::UnitGenerator(Unit* unit, Context* context, IR::TypeTable* types, 
							 IR::ConstantTable* consts, IR::IntrinsicTable* intrinsics,
							 GeneratorEvents* events) :
		unit_(unit), context_(context), currentFunction_(nullptr), events_(events) {
	DebugValidator::IsNotNull(unit);
	DebugValidator::IsNotNull(context);
	DebugValidator::IsNotNull(types);
	DebugValidator::IsNotNull(consts);
	DebugValidator::IsNotNull(intrinsics);

	irUnit_ = IR::Unit::GetUnit(types, consts, intrinsics);
	target_ = context->Target();
	layouts_ = new LayoutCache(context);
	typeGen_ = new TypeGenerator(context->Target(), irUnit_, this, layouts_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UnitGenerator::~UnitGenerator() {
	// We remove all variables and function that have been created,
	// but are not used because they are internal and never referenced.
	globalVars_.ForEachValue([](IR::GlobalVariable* variable) -> bool {
		if(variable->ParentTable() == nullptr) {
			variable->Free();
		}

		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Unit* UnitGenerator::Generate(bool emitTypedef) {
	// We directly generate code only for function definitions and variables/function
    // declarations marked as 'extern' or 'tentative'. Code for struct/union types 
    // and for internal variables is generated only if referred from a function.
	auto& declarations = unit_->UnitDeclarations();

	for(int i = 0; i < declarations.Count(); i++) {
		Declaration* declaration = declarations[i];

		if(auto functDecl = declaration->As<FunctionDeclaration>()) {
			GenerateFunctionDeclaration(functDecl);
		}
		else if(auto variableDecl = declaration->As<VariableDeclaration>()) {
			GenerateVariableDeclaration(variableDecl);
		}
		else if(emitTypedef) {
			auto typedefDecl = declaration->As<TypedefDeclaration>();
			if(typedefDecl == nullptr) continue;
				
			// Create a typename that represents a typedef.
			auto irType = typeGen_->GetType(typedefDecl->DeclarationType()->InnerType());
			string& name = typedefDecl->Name()->Name();
			IR::Symbol* tn = IR::Symbol::GetTypename(irType, name, &irUnit_->Symbols());
			irUnit_->AddTypename(tn);
		}
	}

	return irUnit_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int UnitGenerator::GetAlignment(const Declaration* declaration) {
	// If the declaration has an alignment attribute we use it.
	// Else we use the default alignment of the type.
	if(auto alignAttr = declaration->AttributeAs<AlignmentAttribute>()) {
		return alignAttr->Value();
	}
	else return TypeAlignment(declaration->DeclarationType(), context_).GetAlignment();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::SymbolVisibility UnitGenerator::GetVisibility(const Declaration* declaration) {
	switch(declaration->Linkage()) {
		case LinkageType::External: return IR::SymbolVisibility::Extern;
		case LinkageType::Internal: return IR::SymbolVisibility::Static;
		default: {
			// A variable with a tentative initializer must be marked.
			// Else the standard (auto) visibility is used.
			if(auto variableDecl = declaration->As<VariableDeclaration>()) {
				if(variableDecl->IsTentative()) return IR::SymbolVisibility::Tentative;
			}

			return IR::SymbolVisibility::Auto;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::DllVisibility UnitGenerator::GetDllVisibility(const Declaration* declaration) {
	auto dllAttr = declaration->AttributeAs<DllAttribute>();
	
    if(dllAttr == nullptr) {
        return IR::DllVisibility::None;
    }
	else if(dllAttr->Type() == DllType::Import) {
		return IR::DllVisibility::Import;
	}
	else return IR::DllVisibility::Export;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
shared<string> UnitGenerator::GetSection(const Declaration* declaration) {
	// If the declaration has an alignment attribute we use it.
	// Else we use the default alignment of the type.
	if(auto sectionAttr = declaration->AttributeAs<SectionAttribute>()) {
		return new string(sectionAttr->Section());
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::InlineType UnitGenerator::GetInline(const Declaration* declaration) {
	if(auto inlineAttr = declaration->AttributeAs<InlineAttribute>()) {
		if(inlineAttr->IsAlways())     return IR::InlineType::Always;
		else if(inlineAttr->IsNever()) return IR::InlineType::Never;
	}

	return IR::InlineType::Auto;
}
	
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool UnitGenerator::GetCallConvention(const Declaration* declaration, 
									  IR::CallConventionType& type) {
	if(auto callAttr = declaration->AttributeAs<CallConventionAttribute>()) {
		if(callAttr->IsCdecl())         type = IR::CallConventionType::Cdecl;
		else if(callAttr->IsFastcall()) type = IR::CallConventionType::Fastcall;
		else if(callAttr->IsStdcall())  type = IR::CallConventionType::Stdcall;
		return true;
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::MarkFromStdlib(IR::Function* function, 
                                   const FunctionDeclaration* functDecl) {
	FileId file = functDecl->StartLocation().File();
	FileDetails* details;

	if(context_->FileMgr().GetDetails(file, details)) {
		if(details->IsSystem()) {
			// The function originates from s standard library header.
			function->SetIsFromStdlib(true);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::GenerateFunctionDeclaration(const FunctionDeclaration* functDecl) {
	// If the declaration is already in the map and it's the definition
	// we start generating code for it; it also must be external.
	if(functions_.ContainsKey(functDecl)) {
		if(functDecl->IsDefinition() && 
           (functDecl->Linkage() == LinkageType::External)) {
			// It's possible that the function was already generated.
			if(functions_[functDecl]->Generated == false) {
				BeginFunction(functDecl);
			}
		}
		
		return;
	}

	// We create the declaration of the function having the shape of the last
	// declaration, and link all other declarations to it. 
	// If the function is 'static' we don't emit the declaration now, 
	// but only when it's used the first time.
	auto lastDecl = functDecl->LastDeclaration()->As<FunctionDeclaration>();
	auto functionType = lastDecl->DeclarationType();
	auto irFunctType = static_cast<const IR::FunctionType*>
					   (typeGen_->GetType(functionType));

	// Create the function declaration/definition.
	IR::CallConventionType callConv;
	bool hasCallConv = GetCallConvention(lastDecl, callConv);
	IR::InlineType inlineType = GetInline(lastDecl);

	bool isDefinition = lastDecl->IsDefinition();
	auto name = lastDecl->Name()->Name();
	IR::Function* functionSymbol = IR::Function::GetFunction(irFunctType, name,
													         isDefinition, nullptr, 
													         GetVisibility(lastDecl));
	// Set the attributes.
	MarkFromStdlib(functionSymbol, functDecl);
    functionSymbol->SetInline(inlineType);

	if(hasCallConv) {
        functionSymbol->SetCallConvention(callConv);
    }

    // We presume that a function has its address not taken
    // until we prove otherwise (happens when the function reference
    // is not used only as a known call target).
    functionSymbol->SetIsAddresTaken(false);

	// Add the parameters to the function and add register the function in the generator.
	shared<FunctionHolder> functHolder = new FunctionHolder(functionSymbol);
	GenerateFunctionParameters(lastDecl, functHolder);
	functions_.Add(lastDecl, functHolder);

	// Link all declarations of the function to this symbol.
	auto previousDecl = lastDecl->Previous();

	while(previousDecl) {
		functions_.Add(previousDecl->As<FunctionDeclaration>(), functHolder);
		previousDecl = previousDecl->Previous();
	}
	
	// If the current declaration is also the definition we start generating
	// code for it. If the function is 'static' we don't add it to the IR Unit yet.
	if((functDecl->Linkage() == LinkageType::External) || 
        functDecl->IsDefinition()) {
		// If this is a declaration found in a system header don't emit it yet,
		// because it may not be used.
		auto fileId = functDecl->StartLocation().File();
		FileDetails* details;

		if(context_->FileMgr().GetDetails(fileId, details)) {
			if(details->IsSystem()) return;
		}

		AddFunction(functionSymbol, functHolder);
	}

	// Begin generating code for the function if it's a definition.
	if(functDecl->IsDefinition()) {
		BeginFunction(functDecl);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::AddFunction(IR::Function* irFunct, FunctionHolder* holder) {
	// If this function was created while generating another function
	// make it appear before the other one.
	if(currentFunction_) {
		irUnit_->AddFunction(irFunct, currentFunction_->FunctionNode, 
							 currentFunction_->FunctionSymbolNode, 
							 &holder->FunctionNode,
							 &holder->FunctionSymbolNode);
	}
	else irUnit_->AddFunction(irFunct, nullptr, nullptr, &holder->FunctionNode,
							  &holder->FunctionSymbolNode);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::GenerateFunctionParameters(const FunctionDeclaration* functDecl, 
											   FunctionHolder* functHolder) {
	// Convert each parameter to it's IR counterpart and add it to he IR function.
	// Note that "simple" records are expanded.
	auto& paramVars = functDecl->Parameters();
    auto function = functHolder->Function;

	for(int i = 0; i < paramVars.Count(); i++) {
		auto& parameter = paramVars[i];
		auto parameterType = parameter->DeclarationType();
		auto irParameterType = typeGen_->GetType(parameterType);
		bool handled = false;
        bool largeStruct = false;

		// Parameters are unnamed if part of a function declaration.
		string* parameterName = nullptr;

		if(functDecl->IsDefinition()) {
			DebugValidator::IsTrue(parameter->Name() != nullptr);
			parameterName = &parameter->Name()->Name();
		}

		// OPTIMIZATION:
		// If the parameter has 'struct' type and it's "simple",
        // we don't emit a parameter having record type; instead
        // we emit an individual parameter for each field.
		// This is a light form of the "scalar replacement of aggregates".
		if(auto recordType = parameterType->WithoutQualifiers()->As<StructUnionType>()) {
			if(IsExpandableStruct(recordType)) {
				// Expand the 'struct' into individual parameters.
				handled = true;
				auto structType = recordType->As<StructType>();
				ExpandStructType(structType, parameterName, parameter, functHolder);
			}
			else {
				// "Large" records are passed "by reference", so we convert
				// the type from 'record' to 'pointer to record'.
				irParameterType = irUnit_->Types().GetPointer(irParameterType);
                largeStruct = true;
			}
		}
		
		if(handled == false) {
			auto irParameter = GenerateParameter(irParameterType, parameterType, 
                                                 parameterName, function);
            function->AddParameter(irParameter);
            
            // We mark it as 'noescape' to indicate that the address of the
            // record is not stored in any global variable, or in another object.
            // We also mark it as 'nowrite' because the callee is guaranteed
            // to make a copy of this record.
            irParameter->SetIsNoEscape(largeStruct);
            irParameter->SetIsNoWrite(largeStruct);
		}
	}

	// A function returning a record is transformed in a function
    // that takes a pointer to a record as it's last parameter. For example, 
	// 'struct ABC f(int a) -> funct f(var a int32, var #retval <#struct_ABC>*) : void'
	auto returnType = functDecl->DeclarationType()->ReturnType()->WithoutQualifiers();

	if(auto recordRetType = returnType->As<StructUnionType>()) {
		// Create a parameter having the name '#retval' and type 'pointer to record'.
		auto irRecordType = typeGen_->GetType(recordRetType);
		auto irRecordPtrType = irUnit_->Types().GetPointer(irRecordType);
			
        string parameterName = nameGen_.GetName(&function->Symbols(), "", "", "retval");
		auto retValParam = GenerateParameter(irRecordPtrType, returnType,
                                             &parameterName, function);
        function->AddParameter(retValParam);

        // We mark it as 'noescape' and 'noread',
        // it may improve some optimizations.
        retValParam->SetIsNoEscape(true);
        retValParam->SetIsNoRead(true);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::Variable* UnitGenerator::GenerateParameter(const IR::Type* irType, const Type* type, 
											   string* name, IR::Function* irFunct) {
	// Get the parameter a name, if provided.
	string* parameterName = name ? new string(*name) : nullptr;
		
	// Create a variable marked as being a parameter.
	auto irParameter = IR::Variable::GetVariable(irType, parameterName);
	irParameter->SetIsParameter(true);
    irParameter->SetIsUnsigned(type->IsUnsigned());

	// If the parameter is marked with 'restrict' propagate it into the IR.
	// This may help alias analysis.
	if(auto qualType = type->As<QType>()) {
		if(qualType->HasRestrict()) {
			irParameter->SetIsRestrict(true);
		}
	}

	return irParameter;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::ExpandStructType(const StructType* structType, string* name, 
							         VariableDeclaration* parameter, 
                                     FunctionHolder* functHolder) {
	auto& fields = structType->Fields();
    auto function = functHolder->Function;

	for(int i = 0; i < fields.Count(); i++) {
		auto fieldType = fields[i]->DeclarationType()->WithoutQualifiers();

		// Create the name of the field. If 'name' is not set no name is given
		// to the field (this means that we're in a function declaration).
		// The name has the form '#name_i' (ex: #myName_0, #myName_1).
		string fieldNameBuffer;
		string* fieldName = nullptr;
		
		if(name) {
            fieldNameBuffer = string::Format(L"#%s_%d", name->Chars(), i);
			fieldName = &fieldNameBuffer;
		}

		// Nested 'struct' fields are emitted before the other fields.
		if(auto nestedStructType = fieldType->As<StructType>()) {
			ExpandStructType(nestedStructType, fieldName, parameter, functHolder);			
		}
		else {
			// Create a parameter for this field and add it to the list of mappings
			// between fields and the associated variable. This is later used
			// to find which parameter to use when a field is accessed.
			auto irFieldType = typeGen_->GetType(fieldType, functHolder->Function);
			auto irParameter = GenerateParameter(irFieldType, fieldType, fieldName, 
											     functHolder->Function);
            function->AddParameter(irParameter);
			ExpandedField exField(fields[i], parameter, irParameter);
			functHolder->Fields.Add(exField);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::GlobalVariable* 
UnitGenerator::GenerateVariableDeclaration(const VariableDeclaration* variableDecl) {
	// If the declaration is already in the map we have nothing to do.
	IR::GlobalVariable* irVariable;

	if(globalVars_.TryGetValue(variableDecl, &irVariable)) {
		return irVariable;
	}

	// We create code for the last declaration and make all other ones
	// point to it. This is necessary because there may be functions
	// that use a declaration that is not the final one, like in
	// 'int a[];  void f() { a[1] = 2; }'
	// 'int a[5]; void g() { a[3] = 4; }' - second declaration used here
	auto lastDecl = variableDecl->LastDeclaration()->As<VariableDeclaration>();

	// Create the global variable, but emit it now only if it's marked 'extern'.
	// Internal ('static') variables are emitted only if used.
	auto irType = typeGen_->GetType(variableDecl->DeclarationType());
	string name = nameGen_.GetName(&irUnit_->Symbols(), variableDecl->Name()->Name());

	irVariable = IR::GlobalVariable::GetGlobal(irType, name, nullptr /* initializer */,
										       nullptr /* unit */, 
                                               GetVisibility(variableDecl));

	// Add other attributes and debug information.
	AddAttributes(irVariable, variableDecl);
	AddVariableLocation(irVariable, variableDecl);
	
	// Append the initializer, if any.
	if(variableDecl->Initializer()) {
		AddInitializer(irVariable, variableDecl);
	}

	// Make all declarations point to this symbol.
	globalVars_.Add(lastDecl, irVariable);
	auto previousDecl = lastDecl->Previous();

	while(previousDecl) {
		globalVars_.Add(previousDecl->As<VariableDeclaration>(), irVariable);
		previousDecl = previousDecl->Previous();
	}

	// If the variable is not 'static' add it now.
	if(lastDecl->Linkage() != LinkageType::Internal) {
		// If we're in a function add the variable before the function.
		if(currentFunction_) {
			irUnit_->AddVariable(irVariable, nullptr, 
                                 currentFunction_->FunctionSymbolNode);
		}
		else irUnit_->AddVariable(irVariable);
	}

	return irVariable;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::AddAttributes(IR::Variable* irVariable, 
                                  const VariableDeclaration* variableDecl) {
	irVariable->SetAlignment(GetAlignment(variableDecl));
	irVariable->SetDllVisibility(GetDllVisibility(variableDecl));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::AddAttributes(IR::GlobalVariable* irVariable, 
                                  const VariableDeclaration* variableDecl) {
	irVariable->SetAlignment(GetAlignment(variableDecl));
	irVariable->SetDllVisibility(GetDllVisibility(variableDecl));
	irVariable->SetSection(GetSection(variableDecl));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::AddVariableLocation(IR::Variable* irVariable, 
                                        const VariableDeclaration* variableDecl) {
	// Add location information only if requested.
	if(context_->Options().RecordVariableLocation() == false) {
        return;
    }

	//! TODO: FIXME
	//unsigned id = irUnit_->Tags().GetId();
	//irUnit_->Tags().Add(IR::LocationTag::GetLocation(variableDecl->StartLocation()), id);
	//irVariable->SetTagId(id);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::BeginFunction(const FunctionDeclaration* function) {
	DebugValidator::IsTrue(functions_.ContainsKey(function));
	
	// Set the current function, then begin generating code.
	if(function->IsDefinition() == false) {
        return;
    }
	
	// DEBUG:
	// if(function->Name()) std::wcout<<"=>   "<<function->Name()->Name().Chars()<<"\n";

	// Push the current function (if any) on the stack.
	if(currentFunction_) {
		functStack_.Push(currentFunction_);
	}

	currentFunction_ = functions_[function];
	currentFunction_->Generated = true; // Mark it as "generated".
	FunctionGenerator functGen(this);
	functGen.Generate(function, currentFunction_->Function, currentFunction_->Fields);

	// Restore the previous function.
	if(functStack_.Count() > 0) {
		currentFunction_ = functStack_.Pop();
	}
	else currentFunction_ = nullptr;

	// DEBUG:
	// if(function->Name()) std::wcout<<"<=   "<<function->Name()->Name().Chars()<<"\n";
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
UnitGenerator::GetVariableReference(IR::GlobalVariable* irVariable) {
	DebugValidator::IsNotNull(irVariable);
	auto irVarPtrType = irUnit_->Types().GetPointer(irVariable->GetType());
	return irUnit_->References().GetGlobalVariableRef(irVariable, irVarPtrType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
UnitGenerator::GetVariableReference(const VariableDeclaration* variableDecl) {
	DebugValidator::IsNotNull(variableDecl);
	IR::GlobalVariable* irVariable;
	
	// If the global IR variable hasn't been generated yet do it now.
	if(globalVars_.TryGetValue(variableDecl, &irVariable) == false) {
		irVariable = GenerateVariableDeclaration(variableDecl);
	}

	// Create the reference to the variable.
	auto irVariableRef = GetVariableReference(irVariable);

	// If the variable hasn't been added to the unit yet add it now.
	if(irVariable->ParentTable() == nullptr) {
		// If we're in a function add the variable before the function.
		if(currentFunction_) {
			irUnit_->AddVariable(irVariable, nullptr, 
                                 currentFunction_->FunctionSymbolNode);
		}
		else irUnit_->AddVariable(irVariable);
	}

	return irVariableRef;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
UnitGenerator::GetVariableReference(const DeclarationExpression* variableRef) {
	DebugValidator::IsNotNull(variableRef);
	DebugValidator::IsTrue(variableRef->Object()->IsVariableDecl());

	return GetVariableReference(variableRef->Object()->As<VariableDeclaration>());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool UnitGenerator::HasVariable(const VariableDeclaration* variableDecl) {
	return globalVars_.ContainsKey(variableDecl);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::AddVariable(const VariableDeclaration* variableDecl) {
	GenerateVariableDeclaration(variableDecl);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::FunctionReference*
UnitGenerator::GetFunctionReference(IR::Function* irFunct) {
	DebugValidator::IsNotNull(irFunct);

	auto irFunctPtrType = irUnit_->Types().GetPointer(irFunct->GetType());
	return irUnit_->References().GetFunctionRef(irFunct, irFunctPtrType);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::FunctionReference* 
UnitGenerator::GetFunctionReference(const FunctionDeclaration* functDecl) {
	DebugValidator::IsNotNull(functDecl);

	// Always use the last declaration.
	functDecl = functDecl->LastDeclaration()->As<FunctionDeclaration>();

	// See if the function has been added to the IR unit.
	// If not add it now and generate the code for it.
	if(functions_.ContainsKey(functDecl) == false) {
		GenerateFunctionDeclaration(functDecl);	
	}

	auto functHolder = functions_[functDecl];
	auto irFunctRef = GetFunctionReference(functHolder->Function);
	auto irFunct = irFunctRef->Target();
	
	// Generate the function now if it wasn't already generated.
	if((irFunct->ParentUnit() == nullptr) || (functHolder->Generated == false)) {
		// If the function hasn't been added to the IR Unit yet we add it now.
		if(irFunct->ParentUnit() == nullptr) {
			AddFunction(irFunct, functHolder);
		}

		// Generate code for the function before returning the reference.
		BeginFunction(functDecl);
	}
	
	return irFunctRef;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::FunctionReference* 
UnitGenerator::GetFunctionReference(const DeclarationExpression* functionRef) {
	DebugValidator::IsNotNull(functionRef);
	DebugValidator::IsTrue(functionRef->Object()->IsFunctionDecl());

	return GetFunctionReference(functionRef->Object()->As<FunctionDeclaration>());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
UnitGenerator::AddConstant(IR::Variable* irVariable, Expression* value, 
                           IR::Function* function) {
	DebugValidator::IsNotNull(irVariable);
	DebugValidator::IsNotNull(value);
	DebugValidator::IsNotNull(function);
	
	// Constants can be strings, arrays or records.
	// The generated name has the form '#const_funct_name_var_name'.
	// We just create the global variable and add the expression as an initializer.
	auto irType = irVariable->GetType();
	string& name = nameGen_.GetName(&irUnit_->Symbols(), *irVariable->Name(), 
								    *function->Name(), "const");
	auto irConstVar = IR::GlobalVariable::GetGlobal(irType, name, 
                                                    nullptr /* initializer */,
												    &irUnit_->Symbols(),
                                                    IR::SymbolVisibility::Static);
	irConstVar->SetInitializer(GetInitializer(value, value->ResultType()));
	irConstVar->SetIsConstant(true);
	irUnit_->AddVariable(irConstVar, nullptr, currentFunction_->FunctionSymbolNode);

	return GetVariableReference(irConstVar);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
IR::VariableReference* 
UnitGenerator::AddConstant(IR::GlobalVariable* irVariable) {
	// If the variable hasn't been added to the IR unit, add it now.
	if(irVariable->ParentTable() == nullptr) {
		irUnit_->AddVariable(irVariable, nullptr, 
                             currentFunction_->FunctionSymbolNode);
	}

	return GetVariableReference(irVariable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool UnitGenerator::IsSimpleRecord(const StructUnionType* recordType, int maxFields, 
								   int maxLevels, bool unionAllowed) {
	DebugValidator::IsNotNull(recordType);
	
	// Try to get the result from the cache first.
	bool cachedResult;
	SimpleRecord recordKey(recordType, maxFields, maxLevels, unionAllowed);

	if(simpleRecords_.TryGetValue(recordKey, &cachedResult)) {
		return cachedResult;
	}

	// The record is not in the cache, determine now if it's "simple".
	int fieldCount = 0;
	bool result = IsSimpleRecordImpl(recordType, fieldCount, 1 /* level */, 
									 maxFields, maxLevels, unionAllowed);
	// Add the result to the cache, so that future queries are faster.
	simpleRecords_.Add(recordKey, result);
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool UnitGenerator::IsSimpleRecordImpl(const StructUnionType* recordType, int& fields, 
									   int level, int maxFields, int maxLevels, 
									   bool unionAllowed) {
	// If there are too many nested records we give up.
	if(level > maxLevels) {
		return false;
	}

	// The test performed on the fields depends on the type of record.
	// If we have an 'union' we need to test only the largest field (it's the
	// only one that it's actually copied).
	// If we have a 'struct' we test both the number and the type of the fields.
	if(recordType->IsUnion()) {
		if(unionAllowed == false) {
            return false;
        }
		
        return IsSimpleUnion(recordType, fields, level, maxFields, maxLevels);
	}
	
    return IsSimpleStruct(recordType, fields, level, maxFields, maxLevels);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool UnitGenerator::IsSimpleStruct(const StructUnionType* recordType, int& fields, 
								   int level, int maxFields, int maxLevels, 
								   bool unionAllowed) {
	// If the 'struct' has too many fields we give up.
	if((fields + recordType->FieldCount()) > maxFields) {
		return false;
	}

	// Test the type of each field. Only basic types (that are not bitfields),
	// pointers and records (that are "simple") are accepted.
	auto& recordFields = recordType->Fields();

	for(int i = 0; i < recordFields.Count(); i++) {
		auto fieldType = recordFields[i]->DeclarationType();

		if(recordFields[i]->IsBitfield() ||
		   fieldType->IsArray() || 
           fieldType->IsFunction()) {
            return false;
        }
		else if(auto recordFieldType = fieldType->As<StructUnionType>()) {
			// A nested record is accepted only if it's also a "simple" record
			// (and adding it's fields wouldn't exceed the limit).
			// Example: struct CIRCLE { struct POINT { int x, y } p; int r; };
			if(IsSimpleRecordImpl(recordFieldType, fields, level + 1,
							      maxFields, maxLevels, unionAllowed) == false) {
				return false;
			}
		}
		else fields++;
	}

	// If we made it here all fields are accepted; test again the number of fields.
	return fields <= maxFields;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool UnitGenerator::IsSimpleUnion(const StructUnionType* recordType, int& fields,
								  int level, int maxFields, int maxLevels) {
	// For 'union' records we need to test only the largest field.
	// We also need to test if the limit is exceeded.
	if((fields + 1) > maxFields) {
		return false;
	}

	// Get the largest field and test its type.
	auto maxField = GetLargestUnionField(recordType);
	auto maxFieldType = maxField->DeclarationType();

	// Test the type of each field. Only basic types (that are not bitfields),
	// pointers and records (that are "simple") are accepted.
	if(maxField->IsBitfield()  ||
	   maxFieldType->IsArray() ||
       maxFieldType->IsFunction()) {
       return false;
    }
	else if(auto recordFieldType = maxFieldType->As<StructUnionType>()) {
		// See above for details.
		if(IsSimpleRecordImpl(recordFieldType, fields, level + 1,
						      maxFields, maxLevels) == false) {
			return false;
		}
	}
	else fields++; // Field accepted.

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const FieldDeclaration* 
UnitGenerator::GetLargestUnionField(const StructUnionType* unionType, int* fieldUnit) {
	DebugValidator::IsNotNull(unionType);
	DebugValidator::IsTrue(unionType->IsUnion());
	
	// Note that at least a field should be found (under standard C).
	auto layout = layouts_->GetOrCreate(unionType);
	auto& unionFields = layout->Fields();
	__int64 maxSize = -1;
	FieldInfo* maxField = nullptr;

	for(int i = 0; i < unionFields.Count(); i++) {
		auto field = unionFields[i];

		if(field->Size() > maxSize) {
			maxSize = field->Size();
			maxField = field;
		}
	}

	DebugValidator::IsNotNull(maxField); // Should not happen!

	if(fieldUnit) {
        *fieldUnit = maxField->Index();
    }

	return maxField->Field();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool UnitGenerator::IsExpandableStruct(const Type* type) {
	if(auto structType = type->WithoutQualifiers()->As<StructType>()) {
		return IsSimpleRecord(structType, STRUCT_EXPANSION_MAX_FIELDS,
							  STRUCT_EXPANSION_MAX_LEVELS, false /* unionAllowed */);
	}
	else return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void UnitGenerator::AddTypename(IR::Symbol* typeName) {
	// If the type is added while inside a function place it before it.
	if(currentFunction_) {
		irUnit_->AddTypename(typeName, nullptr, currentFunction_->FunctionSymbolNode);
	}
	else irUnit_->AddTypename(typeName);
}

} // namespace IRGenerator