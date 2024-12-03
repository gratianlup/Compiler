// Statement.cpp
// Copyright (c) Lup Gratian
//
// Implements the 'Statement' class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Statement.hpp"
#include "Statements.hpp"

namespace AST {

bool Statement::IsDefaultStatement() const {
	return (kind_ == STMT_CASE) && As<CaseStatement>()->IsDefault();
}

} // namespace AST