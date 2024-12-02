// Parameter.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Parameter.hpp"
#include "../Base/StringBuilder.hpp"

namespace IR {

void Parameter::FreeImpl() {
	if(--users_ == 0) {
		parent_->ReleaseParameter(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Parameter::ToStringImpl(int level) const {
	StringBuilder sb(string('\t', level));
	sb.Append("param: ");
	sb.Append(symbol_->ToString(0));
	return sb.ToString();
}

// ######################################################################################
// ParameterTable
// ######################################################################################
ParameterTable::~ParameterTable() {
	params_.ForEachValue([](Parameter* parameter) -> bool {
		delete parameter;
		return true;
	});
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Parameter* ParameterTable::GetParameter(Variable* variable) {
	DebugValidator::IsNotNull(variable);
	DebugValidator::IsTrue(variable->IsParameter());
	Parameter* parameter;

	if(params_.TryGetValue(variable, &parameter)) {
		return parameter;
	}

	parameter = new Parameter(variable, this);
	params_.Add(variable, parameter);
	return parameter;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void ParameterTable::ReleaseParameter(Parameter* parameter) {
	DebugValidator::IsNotNull(parameter);
	DebugValidator::IsTrue(params_.ContainsKey(parameter->GetVariable()));

	params_.Remove(parameter->GetVariable());
	delete parameter;
}

} // namespace IR