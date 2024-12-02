// Variable.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Variable.hpp"
#include "Function.hpp"
#include "Unit.hpp"
#include "SymbolTable.hpp"
#include "../Base/StringBuilder.hpp"
using namespace Base;

namespace IR {

Variable::Variable(Kind kind, const Type* type, shared<string> name, int other,
				   Function* parent, SymbolVisibility visibility) :
		Symbol(kind, type, name, parent ? &parent->Symbols() : nullptr, visibility), 
		other_(other), isParameter_(0), isRestrict_(0), isNoWrite_(0), 
        isNoRead_(0), isNoEscape_(0), isUnsigned_(0)  {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable::Variable(Kind kind, const Type* type, shared<string> name, int other,
				   SymbolTable* parent, SymbolVisibility visibility) :
		Symbol(kind, type, name, parent, visibility), 
        other_(other), isParameter_(0), isRestrict_(0), isNoWrite_(0), 
        isNoRead_(0), isNoEscape_(0), isUnsigned_(0) {
	if(parent) {
		parent->Add(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* Variable::GetVariable(const Type* type, shared<string> name, Function* parent,
								SymbolVisibility visibility) {
	return new Variable(Kind::Variable, type, name, 0 /* other */, 
						parent, visibility);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* Variable::GetVariable(const Type* type, const string& name, Function* parent,
								SymbolVisibility visibility) {
	return new Variable(Kind::Variable, type, new string(name), 0 /* other */, 
						parent, visibility);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
VariableReference* Variable::GetReference() {
    auto parentFunction = reinterpret_cast<Function*>(parentTable_->Parent());
    auto parentUnit = parentFunction->ParentUnit();
    auto type = parentUnit->Types().GetPointer(GetType());
    return parentUnit->References().GetVariableRef(this, type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Variable::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append("Variable:").AppendLine(*Name());

	sb.Append('\t', level).AppendLine(type_->ToString(level + 1));
	return sb.ToString();
}

} // namespace IR