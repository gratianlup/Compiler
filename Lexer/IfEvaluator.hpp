// IfEvaluator.hpp
// Copyright (c) Lup Gratian
//
// An evaluator for #if-like expressions.
// Uses a recursive descendent parsing method with "precedence climbing".
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_LEXING_IF_EVALUATOR_HPP
#define PC_LEXING_IF_EVALUATOR_HPP

#include "Token.hpp"
#include "Identifiers.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/String.hpp"
#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/Stack.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/LocalPointer.hpp"
#include "../Base/Log.hpp"
#include "../Common/Diagnostic.hpp"
using namespace Base;
using namespace Common;

namespace Lexing {

class Lexer;
class Preprocessor;

class IfEvaluator {
private:
	// Used to store the intermediate values of the expression.
	struct Result {
		__int64 Value;
		bool Unsigned;

		//-------------------------------------------------------------------------------
		Result() : Value(0), Unsigned(false) {}
		Result(__int64 value) : Value(value), Unsigned(false) {}
		Result(__int64 value, bool us) : Value(value), Unsigned(us) {}
		Result(const Result& other) : Value(other.Value), Unsigned(other.Unsigned) {}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const int INVALID = 255;

	Lexer* lexer_;
	Preprocessor* pp_;
	Diagnostic* diag_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the precedence associated with the specified operator.
	int GetPrecedence(TokenKind kind);

	// Scans and sets the result depending of the existence of the scanned macro
	// definition. If there is invalid data an error is emitted and 'false' is returned.
	bool ScanAndEvalDefined(Result& result);

	// Evaluates a token preceded by an unary operator by applying the specified action.
	template <class Action>
	bool EvaluateUnary(Token& token, Result& result, Action action);

	// Evaluates the specified token. Handles numbers, 'defined', character constants.
	// Handles unary operators and parenthesis too.
	bool EvaluateToken(Token& token, Result& result);

	// Evaluates a subexpression. Handles binary operators.
	bool EvaluateExpression(int minPrecedence, Result& result);

public:
	IfEvaluator(Preprocessor* pp, Lexer* lexer, Diagnostic* diagnostic) :
			lexer_(lexer), pp_(pp), diag_(diagnostic) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Scans and evaluates an #if expression. 'result' will contain the final boolean
	// value. If there is invalid data an error is emitted and 'false' is returned.
	bool Evaluate();
};

} // namespace Lexing
#endif