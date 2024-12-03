// IfEvaluator.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "IfEvaluator.hpp"
#include "Lexer.hpp"
#include "Preprocessor.hpp"
#include "LexemeParsers.hpp"

namespace Lexing {

int IfEvaluator::GetPrecedence(TokenKind kind) {
	// This kind of switch is implemented efficiently by most compilers.
	switch(kind) {
		case TokenKind::LineEnd:    // Fall through.
		case TokenKind::FileEnd:
		case TokenKind::Invalid: 
		case TokenKind::CloseParen: return 0;
		case TokenKind::Colon:      return 1;
		case TokenKind::Comma:      return 2;
		case TokenKind::Question:   return 3;
		case TokenKind::OrOr:       return 4;
		case TokenKind::AndAnd:     return 5;
		case TokenKind::Or:         return 6;
		case TokenKind::Xor:        return 7;
		case TokenKind::And:        return 8;
		case TokenKind::EqEq:
		case TokenKind::NotEq:      return 9;
		case TokenKind::Less:
		case TokenKind::LessEq:
		case TokenKind::Greater:
		case TokenKind::GreaterEq:  return 10;
		case TokenKind::ShiftR:
		case TokenKind::ShiftL:     return 11;
		case TokenKind::Add:
		case TokenKind::Sub:        return 12;
		case TokenKind::Mul:
		case TokenKind::Div:
		case TokenKind::Mod:        return 13;
	}

	return INVALID; // All other tokens are invalid.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfEvaluator::ScanAndEvalDefined(Result& result) {
	// define identifier
	// define(identifier)
	// 'defined' was already scanned. The next token can be '(', then an identifier
	// that represents a macro definition, then ')' (if there was an '(').
	Token token;
	bool hasOpenParen = false;
	pp_->ScanUnexpanded(token);

	if(token.Kind() == TokenKind::OpenParen) {
		hasOpenParen = true;
		pp_->ScanUnexpanded(token); // Get next token in this case.
	}

	if(token.IsIdentifier() == false) {
		diag_->Report(Error::DEFINED_EXPECTED_IDENTIFIER)<<token.Location();
		return false;
	}

	string& name = token.NameValue()->Name;
	DefinitionInfo* def = pp_->GetDefinition(name);

	if(def == nullptr) {
		result.Value = 0; // The macro doesn't exist.
		result.Unsigned = false;
	}
	else {
		result = def->Enabled() ? 1 : 0;
		result.Unsigned = false;
	}

	// Skip over the ')' if an '(' has been found earlier.
	if(hasOpenParen) {
		pp_->ScanUnexpanded(token);

		if(token.Kind() != TokenKind::CloseParen) {
			diag_->Report(Error::DEFINED_EXPECTED_CLOSE_PAREN)<<token.Location();
			return false;
		}
	}

	return true; // All seems OK.
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
template <class Action>
bool IfEvaluator::EvaluateUnary(Token& token, Result& result, Action action) {
	pp_->Scan(token);

	bool status = EvaluateToken(token, result);
	if(status) action(result);

	return status;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfEvaluator::EvaluateToken(Token& token, Result& result) {
	// If the token represents an identifier checks if it's 'defined'.
	// If it is let 'ScanAndEvalDefined' handle it. 
	// Else return 0 (false) for other identifiers.
	if(token.IsIdentifier()) {
		string& name = token.NameValue()->Name;

		if(name == "defined") {
			return ScanAndEvalDefined(result);
		}
		else {
			result.Value = 0; // C99:6.10.1.4
			result.Unsigned = false;
		}
	}
	else if(token.IsNumber()) {
		// Parse the number and take it's value.
		NumberInfo number = NumberParser(diag_).Parse(token);

		if(number.IsValid == false) {
			// The number is invalid.
			diag_->Report(Error::PP_NUMBER_INVALID)<<token.Location();
			return false;
		}
		else if(number.IsInteger == false) {
			// Only integers are allowed in #if expressions.
			diag_->Report(Error::PP_NUMBER_NOT_INTEGER)<<token.Location();
			return false;
		}

		result.Value = number.IntValue;
		result.Unsigned = number.IsUnsigned();
	}
	else if(token.IsChar()) {
		// Take the value of the character constant.
		CharInfo charInfo = CharParser(diag_).Parse(token);

		if(charInfo.IsValid == false) {
			// The character is invalid.
			diag_->Report(Error::PP_CHARACTER_INVALID)<<token.Location();
			return false;
		}

		result.Value = (__int64)charInfo.Value;
		result.Unsigned = false; // Chars are converted to int.
	}
	else if(token.Kind() == TokenKind::Add) { // +
		return EvaluateUnary(token, result, [](Result& data) {
			// Unary + has no effect.
		});
	}
	else if(token.Kind() == TokenKind::Sub) { // -
		return EvaluateUnary(token, result, [](Result& data) {
			data.Value = -data.Value;
		});
	}
	else if(token.Kind() == TokenKind::Tilde) { // ~
		return EvaluateUnary(token, result, [](Result& data) {
			data.Value = ~data.Value;
		});
	}
	else if(token.Kind() == TokenKind::Not) { // !
		return EvaluateUnary(token, result, [](Result& data) {
			data.Value = !data.Value;
		});
	}
	else if(token.Kind() == TokenKind::OpenParen) { // (
		// A subexpression is starting.
		if(EvaluateExpression(1, result) == false) {
			return false;
		}

		// The ')' should be found now.
		pp_->Scan(token);

		if(token.Kind() != TokenKind::CloseParen) {
			diag_->Report(Error::PP_EXPECTED_CLOSE_PAREN)<<token.Location();
			return false;
		}
	}

	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfEvaluator::EvaluateExpression(int minPrecedence, Result& result) {
	// The left of the expression is first evaluated. If the operator token
	// has a lower precedence than the minimum required we return, so that
	// a parent of this recursive invocation can handle the situation.
	// Here we handle all the operators that have a precedence at least equal
	// to the minimum one. ')', EOF and line end have precedence 0, so they will
	// end the subexpression.
	Token token;
	pp_->Scan(token);
	Result left;

	if(EvaluateToken(token, left) == false) {
		// Note that the appropriate error has been already emitted.
		return false;
	}

	// Read the operator.
	pp_->Scan(token);
	TokenKind kind = token.Kind();
	int precedence = GetPrecedence(kind);

	if(precedence == INVALID) {
		diag_->Report(Error::PP_IF_INVALID_TOKEN)<<token.Location();
		return false;
	}

	while(precedence >= minPrecedence) {
		// Skip over the operator and evaluate the right part.
		Result right;
		bool overflow = false;

		// Special case for ?: Right should stop at the ':'.
		int nextPrecedence = (kind == TokenKind::Question) ?
							  GetPrecedence(TokenKind::Comma) : precedence + 1;

		if(EvaluateExpression(nextPrecedence, right) == false) {
			return false; // Something went wrong.
		}

		// Set the type of the result value (signed/unsigned).
		result.Unsigned = left.Unsigned || right.Unsigned;

		// Apply the operand.
		switch(kind) {
			case TokenKind::Add: {
				result.Value = left.Value + right.Value;

				if(left.Unsigned == false) {
					if((left.Value > 0) && (right.Value > 0) && 
					   (result.Value < 0)) {
						overflow = true;
					}
				}

				break;
			}
			case TokenKind::Sub: {
				result.Value = left.Value + right.Value;

				if(left.Unsigned == false) {
					if(((left.Value < 0) || (right.Value < 0)) && 
					   ((result.Value > 0) != (left.Value > 0))) {
						overflow = true;
					}
				}

				break;
			}
			case TokenKind::Mul: {
				result.Value = left.Value * right.Value;

				if((left.Unsigned == false) && (left.Value != 0) && 
				   (right.Value != 0)) {
					overflow = ((result.Value / left.Value) != right.Value) ||
							   ((result.Value / right.Value) != left.Value);
				}

				break;
			}
			case TokenKind::Div: {
				if(right.Value == 0) {
					diag_->Report(Error::PP_IF_DIVISION_BY_ZERO)<<token.Location();
					return false;
				}

				result.Value = left.Value / right.Value;
				break;
			}
			case TokenKind::Mod: {
				if(right.Value == 0) {
					// Diag.error
					diag_->Report(Error::PP_IF_DIVISION_BY_ZERO)<<token.Location();
					return false;
				}

				result.Value = left.Value % right.Value;
				break;
			}
			case TokenKind::ShiftL: {
				__int64 maxShift = lexer_->Target()->GetMaxSize(NumberKind::Integer) * 8;

				if(right.Value >= maxShift) {
					overflow = true;
				}

				result.Value = left.Value << right.Value;
				break;
			}
			case TokenKind::ShiftR: {
				__int64  maxShift = lexer_->Target()->GetMaxSize(NumberKind::Integer) * 8;

				if(right.Value >= maxShift) {
					overflow = true;
				}

				result.Value = left.Value >> right.Value;
				break;
			}
			case TokenKind::Less: {
				result.Value = left.Value < right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::LessEq: {
				result.Value = left.Value <= right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::Greater: {
				result.Value = left.Value > right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::GreaterEq: {
				result.Value = left.Value >= right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::EqEq: {
				result.Value = left.Value == right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::NotEq: {
				result.Value = left.Value != right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::And: {
				result.Value = left.Value & right.Value;
				break;
			}
			case TokenKind::Or: {
				result.Value = left.Value | right.Value;
				break;
			}
			case TokenKind::Xor: {
				result.Value = left.Value ^ right.Value;
				break;
			}
			case TokenKind::AndAnd: {
				result.Value = left.Value && right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::OrOr: {
				result.Value = left.Value || right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::Comma: {
				result.Value = right.Value;
				result.Unsigned = false; // C99:6.8.5.6
				break;
			}
			case TokenKind::Question: {
				// The part before : has been evaluated. The next token should be :
				// Evaluate the last part and perform the operation.
				pp_->Scan(token);

				if(token.Kind() != TokenKind::Colon) {
					diag_->Report(Error::PP_IF_CONDITIONAL_MISSING_COLON)
								  <<token.Location();
					return false;
				}

				Result right2;
				if(EvaluateExpression(precedence, right2) == false) {
					return false; // Something went wrong.
				}

				// Select one of 'right' or 'right2' based on 'left'.
				if(left.Value != 0) {
					result.Value = right.Value;
				}
				else {
					result.Value = right2.Value;
				}

				break;
			}
		}

		// Warn about overflow.
		if(overflow) {
			diag_->Report(Warning::PP_IF_OVERFLOW)<<token.Location();
		}

		// Read the next operator.
		pp_->Scan(token);
		kind = token.Kind();
		precedence = GetPrecedence(kind);

		if(precedence == INVALID) {
			diag_->Report(Error::PP_IF_INVALID_TOKEN)<<token.Location();
			return false;
		}

		// Put the result in 'left' and continue.
		left = result;
	}

	lexer_->PutBack(token);
	result = left;
	return true;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool IfEvaluator::Evaluate() {
	Result exprResult;
	if(EvaluateExpression(1, exprResult) == false) {
		return false;
	}
	else return exprResult.Value != 0;
}

} // namespace Lexing