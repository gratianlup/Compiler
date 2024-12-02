// Copyright (c) Lup Gratian
//
// Implements the general (common) parsing methods.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Parser.hpp"
#include "../Common/LocationInfo.hpp"
#include "../Common/Errors.hpp"
#include "../AST/ASTDotPrinter.hpp"
using namespace Common;

namespace Parsing {

Parser::Parser(Lexer* lexer, Context* context, TypeManager* types, 
               SemanticHolder* semaHolder) :
			lexer_(lexer), context_(context), types_(types), diag_(&context->Diagnostic()),
			semaHolder_(semaHolder), unit_(lexer->Location().File()), 
			declSema_(semaHolder->GetDeclarationSemantic()), 
            exprSema_(semaHolder->GetExpressionSemantic()), 
			statementSema_(semaHolder->GetStatementSemantic()), typeComb_(types_) {
	// Read the first token.
	hasPeekedToken_ = false;
	EatToken();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
KeywordType Parser::Kwd(const Token& token) const {
	if(token.IsKeyword() == false) {
		return Keyword_None;
	}

	return token.AsKeyword<KeywordType>();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Token& Parser::PeekNext() {
	hasPeekedToken_ = true;
	lexer_->NextToken(peekedToken_);
	return peekedToken_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::EatToken() {
	// Get the next token from the Lexer.
	// If there is a token that has been peeked consume it now.
	if(hasPeekedToken_) {
		hasPeekedToken_ = false;
		current_ = peekedToken_;
	}
	else if(current_.IsEOF() == false) {
		// Request a new token only if EOF was not already reached.
		lexer_->NextToken(current_);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::IsStatementKeyword(const Token& token) {
	return (Kwd(token) == Keyword_If) || (Kwd(token) == Keyword_For) ||
		   (Kwd(token) == Keyword_While) || (Kwd(token) == Keyword_Switch) ||
		   (Kwd(token) == Keyword_Do);	
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::SkipToToken(TokenKind kind, bool eat, bool semiHasPriority) {
	// Skip until the specified token is found.
	// This keeps track of the number of found [], {} and () and tries to match them.
	int parenCount  = 0;
	int squareCount = 0;
	int curlyCount  = 0;

	while(current_.IsEOF() == false) {
		if(((parenCount  == 0)  && 
            (squareCount == 0) && 
            (curlyCount  == 0)) &&
		    IsStatementKeyword(current_)) {
			// Found a statement keyword; stop here because it starts a new region of code.
			break;
		}
		if(IsOpenParen()) {
			if(kind != TokenKind::OpenParen) {
				parenCount++;
			}
			else break;
		}
		else if(IsOpenSquare()) {
			if(kind != TokenKind::OpenSquare) {
				squareCount++;
			}
			else break;
		}
		else if(IsOpenCurly()) {
			if(kind != TokenKind::OpenCurly) {
				curlyCount++;
			}
			else break;
		}
		else if(IsCloseParen()) {
			parenCount--;
			if((parenCount <= 0) && (kind == TokenKind::CloseParen)) {
				break;
			}
		}
		else if(IsCloseSquare()) {
			squareCount--;
			if((squareCount <= 0) && (kind == TokenKind::CloseSquare)) {
				break;
			}
		}
		else if(IsCloseCurly()) {
			curlyCount--;
			if((curlyCount <= 0) && ((kind == TokenKind::CloseCurly) ||
                                     (kind == TokenKind::SemiColon))) {
				break;
			}
		}
		else if(IsSemiColon()) {
			if((kind == TokenKind::SemiColon) && ((curlyCount <= 0) && 
			   (parenCount <= 0) && (squareCount <= 0))) break;
			else if(semiHasPriority) break;
		}
		else if(kind == current_.Kind()) {
			break; // Found the token!
		}

		EatToken();
	}

	if(eat && current_.Kind() == kind) {
		EatToken();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::SkipToKwd(KeywordType type, bool eat) {
	while((current_.IsEOF() == false) && (Kwd(current_) != type) &&
		  (IsSemiColon() == false) && (IsCloseCurly() == false)) {
		EatToken();
	}

	if(eat && (Kwd(current_) == type)) {
		EatToken();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::SkipToSemiColon(bool eat) {
	SkipToToken(TokenKind::SemiColon, eat);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::SkipToComma(bool eat) {
	SkipToToken(TokenKind::Comma, eat, true);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::SkipToColon(bool eat) {
	SkipToToken(TokenKind::Colon, eat);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::SkipToCloseCurly(bool eat) {
	SkipToToken(TokenKind::CloseCurly, eat, true /* can stop at ; */);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::SkipToCloseParen(bool eat) {
	SkipToToken(TokenKind::CloseParen, eat);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::SkipToBlockEnd(bool eat, bool stopAtSemi) {
	SkipToToken(TokenKind::CloseCurly, eat, stopAtSemi);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::PushContext(shared<DeclarationContext> context) {
	// Add the context to the unit.
	if(contextStack_.Count() > 0) {
		// Link the parent and the child.
		context->SetParent(contextStack_.Peek());
	}

	contextStack_.Push(context);
	activeCtx_ = context;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::PopContext() {
	activeCtx_ = nullptr;

	if(contextStack_.Count() > 0) {
		activeCtx_ = contextStack_.Pop();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::AddExtension(shared<Extension> extension) {
	if(extension->ReceiveDeclarationFailed()) {
		declFailedExts_.Add(extension);
	}

	if(extension->ReceiveDeclarationSucceeded()) {
		declSucceededExts_.Add(extension);
	}

	if(extension->ReceiveFunctionBegin()) {
		functBeginExts_.Add(extension);
	}

	if(extension->ReceiveFunctionEnd()) {
		functEndExts_.Add(extension);
	}

	if(extension->ReceiveTypeQuery()) {
		typeExts_.Add(extension);
	}

	if(extension->ReceiveUnknown()) {
		unknownExts_.Add(extension);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::ParseTranslationUnit() {
	// translation-unit:
	//		external-declaration
	//		translation-unit external-declaration
	// external-declaration:
	//		function-definition
	//		declaration
	// Create the top-level (file) context.
	shared<DeclarationContext> unitCtx = new DeclarationContext(ScopeType::File, nullptr);
	PushContext(unitCtx);

	// Parse until EOF is reached.
	while(current_.IsEOF() == false) {
		if(IsSemiColon()) {
			// Emit error if a ; is found at the file level.
			diag_->Report(Error::SEMI_COLON_AT_FILE_SCOPE)<<current_.Location();
			EatToken();
		}
		else if(HandleUnknown(ExtensionContext(Context_Unit)) == false) {
			// Handle extensions first, like #pragma. If it's not an extension
			// then it must be an usual declaration.
			ParseFileDeclaration();
		}

		// * DEBUG *
		//AST::ASTDotPrinter printer(L"D:\\results.dot", true, true);
		//printer.Print(&unit_);
	}

	// Do some semantic analysis when the unit ends (uninitialized global variables
	// must be handled, among other things).
	return declSema_->HandleUnitEnd(unitCtx, unit_);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::HandleUnknown(ExtensionContext& context) {
	for(int i = 0; i < unknownExts_.Count(); i++) {
		if(unknownExts_[i]->Handle(context)) {
			return true;
		}
	}

	return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::NotifyDeclarationSucceeded(Declaration* declaration) {
	for(int i = 0; i < declSucceededExts_.Count(); i++) {
		declSucceededExts_[i]->DeclarationSucceeded(declaration);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::NotifyDeclarationFailed() {
	for(int i = 0; i < declFailedExts_.Count(); i++) {
		declFailedExts_[i]->DeclarationFailed();
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::NotifyFunctionBegin(Declaration* declaration) {
	for(int i = 0; i < functBeginExts_.Count(); i++) {
		functBeginExts_[i]->FunctionBegin(declaration);
	}

}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Parser::NotifyFunctionEnd(Declaration* declaration) {
	for(int i = 0; i < functEndExts_.Count(); i++) {
		functEndExts_[i]->FunctionEnd(declaration);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Parser::QueryType(Token& token) {
	for(int i = 0; i < typeExts_.Count(); i++) {
		if(typeExts_[i]->IsTypeToken(&token)) {
			return true;
		}
	}

	return false;
}

} // namespace Parsing