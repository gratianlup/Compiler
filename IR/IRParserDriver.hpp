// IRParserDriver.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_PARSER_DRIVER_HPP
#define PC_IR_PARSER_DRIVER_HPP

#include "IRParser.hpp"
#include "IRVerifier.hpp"
#include "InterpreterDriver.hpp"
#include "TypeTable.hpp"
#include "InterpreterBuiltins.hpp"
#include "FlowDotPrinter.hpp"
#include "Intrinsic.hpp"
#include "IRPrinter.hpp"
#include "ConstantTable.hpp"
#include "../Abstraction/Platform.hpp"
#include "../Common/Context.hpp"
#include "../Common/CompileOptions.hpp"
#include "../Common/FileManager.hpp"
#include "../Common/Diagnostic.hpp"
#include "../Base/String.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/Dictionary.hpp"
#include "../Lexer/Lexer.hpp"
#include "../Lexer/IdentifierTable.hpp"
using namespace Abstraction;
using namespace Base;

namespace IR {

class IRParserDriver : public ParserErrorHandler, public VerifierErrorHandler {
private:
	shared<TypeTable> types_;
	shared<ConstantTable> consts_;
	shared<IntrinsicTable> intrinsics_;
	Dictionary<int, string> parserMessages_;
	Dictionary<int, string> verifierMessages_;
	shared<Unit> unit_;
	int errorCount_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual void Handle(VerifierError message) override;
	virtual void Handle(ParserError message) override;

public:
	IRParserDriver(const string& path, bool launchInterpreter = false);
	
	~IRParserDriver() {
		unit_ = nullptr;
		intrinsics_ = nullptr;
		consts_ = nullptr;
		types_ = nullptr;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	shared<Unit> GetUnit() {
		return unit_;
	}
};

} // namespace IR
#endif