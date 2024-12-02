// IRParser.hpp
// Copyright (c) Lup Gratian
//
// Defines the module used to parse IR code.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_PARSER_HPP
#define PC_IR_PARSER_HPP

#include "Function.hpp"
#include "Block.hpp"
#include "Unit.hpp"
#include "Symbols.hpp"
#include "Constants.hpp"
#include "References.hpp"
#include "Temporary.hpp"
#include "Instructions.hpp"
#include "Tags.hpp"
#include "../Base/String.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Lexer/Lexer.hpp"
#include "../Lexer/Token.hpp"
#include "../Lexer/LexemeParsers.hpp"
#include <limits>
using namespace Base;

namespace IR {

// The keywords of the language.
enum class KeywordType {
	None,

	#undef keyword
	#define keyword(TEXT, NAME) NAME,
	#include "IRKeywords.def"
	#undef keyword
	END
};


// Represents the attributes that can be applied to objects.
enum class AttributeType : __int64 {
	None         = 0,
	Static       = 1 << 0,
	Extern       = 1 << 1,
	Tentative    = 1 << 2,
	Intr         = 1 << 3,
	Fastcall     = 1 << 4,
	Cdecl        = 1 << 5,
	Stdcall      = 1 << 6,
	Const        = 1 << 7,
	Inline       = 1 << 8,
	Forceinline  = 1 << 9,
	Noinline     = 1 << 10,
	Restrict     = 1 << 11,
	Dllimport    = 1 << 12,
	Dllexport    = 1 << 13,
	Decl         = 1 << 14,
	Volatile     = 1 << 15,
	Section      = 1 << 16,
	Align        = 1 << 17,
	Bool         = 1 << 18,
	Stdlib       = 1 << 19,
	Uso          = 1 << 20,
	Fpexact      = 1 << 21,
	Fpsafe       = 1 << 22,
	Fpfast       = 1 << 23,
	Nowrite      = 1 << 24,
    Noescape     = 1 << 25,
    Nostate      = 1 << 26,
    Norem        = 1 << 27,
    Noreturn     = 1 << 28,
    Noread       = 1 << 29,
	Noindirread  = 1 << 30,
	Noindirwrite = 1 << 31,
	END
};


// The errors that can be emitted by the parser.
namespace Error {
	#define error(NAME) static const int NAME = __LINE__;
	#include "IRParserErrors.def"
	#undef error
}


// Represents a message used to signal and provide details about an error.
struct ParserError {
	int Error;
	string Text;
	LocationInfo Location;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ParserError() {}

	ParserError(int error, LocationInfo location, const string& text = "") :
			Error(error), Location(location), Text(text) {}
};


// Base class for objects that can handle error messages.
class ParserErrorHandler {
public:
	virtual ~ParserErrorHandler() {}
	virtual void Handle(ParserError message) = 0;
};


// Helper used to keep track of the read attributes.
class AttributeHolder {
private:
	AttributeType attr_;
	string section_;
	__int64 align_;

public:
	AttributeHolder() : attr_(AttributeType::None), align_(0) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool HasAttribute(AttributeType type) {
		return ((int)attr_ & (int)type) != 0;
	}

	void SetAttribute(AttributeType value) {
		attr_ = (AttributeType)((int)attr_ | (int)value);
	}

	int Count() {
		int count = 0;

		for(__int64 value = 1; value < (__int64)AttributeType::END; value *= 2) {
			count += ((__int64 )attr_ & value) != 0;
		}

		return count;
	}

	string Section() { return section_; }
	void SetSection(const string& value) { section_ = value; }

	__int64 Align() { return align_; }
	void SetAlign(__int64 value) { align_ = value; }
};


class IRParser {
private:
	// Used to represent the identity of a basic block.
	struct BlockName {
		string Function;
		string Block;

		//-------------------------------------------------------------------------------
		BlockName() {}
		BlockName(string function, string block) :
				Function(function), Block(block) {}

		unsigned GetHashCode() const {
			return Function.GetHashCode() ^ Block.GetHashCode();
		}

		bool operator== (const BlockName& other) const {
			return (Function == other.Function) && (Block == other.Block);
		}

		bool operator< (const BlockName& other) const { return false; }
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Support for forward references of symbols.
	Dictionary<string, GlobalVariable*> globalVars_;
	Dictionary<string, Function*> functions_;
	Dictionary<BlockName, Block*> blocks_;
    // The block reference to be patched.
	Dictionary<GlobalVariable*, List<Operand*>> globalVarsPatch_;
	Dictionary<string, Operand*> tempOps_; // The temporaries in a function.

	shared<Unit> unit_;
	Lexing::Lexer* lexer_;
	ParserErrorHandler* handler_;
	TypeTable* types_;
	Token current_;      // The current token.
	Function* currentFunct_;
	Block* currentBlock_;
	bool inFunctProto_;  // True if the function prototype is currently parsed.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Helper methods to test the type of the current token.
	bool IsOpenCurly() const { // {
		return current_.Kind() == TokenKind::OpenCurly;
	}

	bool IsOpenParen() const { // (
		return current_.Kind() == TokenKind::OpenParen;
	}

	bool IsOpenSquare() const { // [
		return current_.Kind() == TokenKind::OpenSquare;
	}

	bool IsCloseCurly() const { // }
		return current_.Kind() == TokenKind::CloseCurly;
	}

	bool IsCloseParen() const { // )
		return current_.Kind() == TokenKind::CloseParen;
	}

	bool IsCloseSquare() const { // ]
		return current_.Kind() == TokenKind::CloseSquare;
	}

	bool IsOpenArrow() const { // <
		return current_.Kind() == TokenKind::Less;
	}

	bool IsCloseArrow() const { // >
		return current_.Kind() == TokenKind::Greater;
	}

	bool IsComma() const { // ,
		return current_.Kind() == TokenKind::Comma;
	}

	bool IsColon() const { // :
		return current_.Kind() == TokenKind::Colon;
	}

	bool IsStar() const { // *
		return current_.Kind() == TokenKind::Mul;
	}

	bool IsEOF() const {
		return current_.IsEOF();
	}

	// Returns the type of the keyword token, or 'KeywordType::None' if it's not a keyword.
	KeywordType Kwd();

	// Obtains the next token from the Lexer.
	void EatToken();

	// Skips all tokens until the declaration of a variable, block or function is found.
	// Used when handling syntactic errors.
	void SkipToSafePoint();

	// Verifies if the current token is the specified one and eats it.
	// If the token is invalid the specified error message is emitted.
	bool ExpectAndEat(TokenKind kind, int error);

	// Returns 'true' if the current token is the start of a declaration at unit scope.
	bool IsDeclarationToken() {
		return (Kwd() == KeywordType::Var)   || 
		       (Kwd() == KeywordType::Funct) || 
			   (Kwd() == KeywordType::Type);
	}

	// Notifies the error handler about the specified error.
	void EmitError(int error);

	// Functions that enable forward declarations.
	GlobalVariable* GetGlobalVariable(const string& name);
	Function* GetFunction(const string& name);
	Block* GetBlock(const string& functionName, const string& name);
	void AddToPatch(GlobalVariable* variable, Operand* op);

	// Methods for parsing according the grammar.
	bool ParseInteger(__int64& value);
	const Type* ParseType();
	Variable* ParseVariable(bool nameRequired = true);
	bool ParseAttributes(AttributeHolder& holder);
	bool ParseInitializer(const Type* type, shared<Initializer>& initializer, bool& isNull);
	bool ParseInitializerList(const Type* type, shared<Initializer>& initializer);

	Symbol* ParseTypename();
	RecordType* ParseRecord(RecordType* record);
	const FunctionType* ParseFunction();
	Function* ParseFunctionDeclaration();
	bool ParseFunctionDefinition();
	
    bool ParseBlock();
	Operand* ParseOperand(bool create = false);
	BlockReference* ParseBlockOperand();
	FunctionReference* ParseIntrinsicOperand();
	Instruction* ParseInstruction();
	Instruction* ParseBinaryInstr(Opcode opcode, Operand* resultOp, bool isLogical);
	Instruction* ParseConversionInstr(Opcode opcode, Operand* resultOp);
	Instruction* ParseLogicalInstr(Opcode opcode, Operand* resultOp);
	Instruction* ParseCompareInstr(Opcode opcode, Operand* resultOp);
	Instruction* ParseIfInstr();
	Instruction* ParseGotoInstr();
	Instruction* ParseSwitchInstr();
	Instruction* ParseCallInstr(Operand* resultOp);
	Instruction* ParseReturnInstr();
	Instruction* ParseLoadInstr(Operand* resultOp);
	Instruction* ParseStoreInstr();
	Instruction* ParseAddressInstr(Operand* resultOp);
	Instruction* ParseIndexInstr(Operand* resultOp);
	Instruction* ParseElementInstr(Operand* resultOp);
	Instruction* ParsePhiInstr(Operand* resultOp);
    Instruction* ParseQuestionInstr(Operand* resultOp);

	// Methods used to determine the type associated with the operands.
	void PatchOperands(Operand* result, Operand*& left, Operand*& right);
	void PatchOperands(Operand* dest, const Type* type);
	void PatchOperand(Operand*& op, const Type* type);

public:
	IRParser(Lexing::Lexer* lexer, ParserErrorHandler* handler, TypeTable* types);

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	shared<Unit> ParseUnit(shared<Unit> unit);
};

} // namespace IR
#endif