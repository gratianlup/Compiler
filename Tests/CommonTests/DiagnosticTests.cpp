#include "../../Common/Diagnostic.hpp"
#include "../../Common/Errors.hpp"
#include "../../Common/CompileOptions.hpp"
#include "../../Base/SharedPointer.hpp"
#include <gtest\gtest.h>
using namespace Common;

class TestHandler : public DiagnosticHandler {
public:
	bool openCalled_;
	bool closeCalled_;
	int handleCalls_;
	int fatal_;
	int error_;
	int warning_;
	int info_;
	bool limit_;
	Message last_;

	TestHandler() : openCalled_(false), closeCalled_(false), handleCalls_(0),
					fatal_(0), error_(0), warning_(0), info_(0), limit_(false) {}

	virtual bool Open() { 
		openCalled_ = true;
		return true;
	}

	virtual bool Close() {
		closeCalled_ = true;
		return true;
	}

	virtual void Handle(const Message& message) {
		handleCalls_++;
		if(message.IsError()) error_++;
		else if(message.IsWarning()) warning_++;
		else if(message.IsFatal()) fatal_++;
		else info_++;
		last_ = message;
	}

	virtual void ErrorLimitReached() {
		limit_ = true;
	}
};

TEST(Diagnostics, Basic) {
	CompileOptions options;
	options.Initialize();
	int error1 = GenError<1>::Value;
	int fatal1 = GenError<2, true>::Value;
	int warning1 = GenWarning<1>::Value;
	int info1 = GenInfo<1>::Value;

	Diagnostic diag(&options);
	shared<TestHandler> handler = new TestHandler();
	diag.AddHandler(handler);
	EXPECT_EQ(true, handler->openCalled_);
	EXPECT_EQ(false, handler->closeCalled_);

	diag.Report(error1)<<1<<(__int64)0xABCDEFABCDEFABC;
	EXPECT_EQ(1, handler->handleCalls_);
	EXPECT_EQ(1, handler->error_);
	EXPECT_EQ(1, diag.GetStatistics().ErrorCount);

	auto& args = handler->last_.Arguments();
	EXPECT_EQ(2, args.Count());
	EXPECT_EQ(Arg_Int, args[0].Kind());
	EXPECT_EQ(Arg_Long, args[1].Kind());
	EXPECT_EQ(1, args[0].AsInt());
	EXPECT_EQ(0xABCDEFABCDEFABC, args[1].AsLong());

	diag.Report(fatal1);
	EXPECT_EQ(2, handler->handleCalls_);
	EXPECT_EQ(1, handler->error_);
	EXPECT_EQ(1, handler->fatal_);
	EXPECT_EQ(1, diag.GetStatistics().ErrorCount);
	EXPECT_EQ(1, diag.GetStatistics().FatalCount);

	diag.Report(warning1);
	EXPECT_EQ(3, handler->handleCalls_);
	EXPECT_EQ(1, handler->warning_);
	EXPECT_EQ(1, diag.GetStatistics().WarningCount);

	diag.Report(info1);
	EXPECT_EQ(4, handler->handleCalls_);
	EXPECT_EQ(1, handler->info_);
	EXPECT_EQ(1, diag.GetStatistics().InfoCount);
}

TEST(Diagnostics, Options) {
	CompileOptions options;
	options.Initialize();
	int error1 = GenError<1>::Value;
	int warning1 = GenWarning<1>::Value;
	int info1 = GenInfo<1>::Value;

	Diagnostic diag(&options);
	shared<TestHandler> handler = new TestHandler();
	diag.AddHandler(handler);

	options.SetIgnoreInfo(true);
	options.SetErrorsAreFatal(true);
	options.SetWarningsAreErrors(true);

	diag.Report(error1);
	EXPECT_EQ(1, handler->fatal_);
	EXPECT_EQ(0, handler->error_);
	EXPECT_EQ(1, diag.GetStatistics().FatalCount);
	EXPECT_EQ(0, diag.GetStatistics().ErrorCount);

	diag.Report(warning1);
	EXPECT_EQ(1, handler->error_);
	EXPECT_EQ(0, handler->warning_);
	EXPECT_EQ(1, diag.GetStatistics().ErrorCount);
	EXPECT_EQ(0, diag.GetStatistics().WarningCount);

	diag.Report(info1);
	EXPECT_EQ(0, handler->info_);
	EXPECT_EQ(0, diag.GetStatistics().InfoCount);

	options.SetNoWarnings(true);
	diag.Report(warning1);
	EXPECT_EQ(0, handler->warning_);
	EXPECT_EQ(0, diag.GetStatistics().WarningCount);

	options.SetIsDiagnosticDisabled(true);
	diag.Report(error1);
	diag.Report(warning1);
	diag.Report(info1);
	EXPECT_EQ(1, handler->error_);
	EXPECT_EQ(1, handler->fatal_);
	EXPECT_EQ(0, handler->warning_);
	EXPECT_EQ(0, handler->info_);
}

TEST(Diagnostics, ErrorLimit) {
	CompileOptions options;
	options.Initialize();
	Diagnostic diag(&options);
	shared<TestHandler> handler = new TestHandler();
	diag.AddHandler(handler);

	options.SetLimitErrors(true);
	options.SetErrorLimit(2);

	int error1 = GenError<1>::Value;
	diag.Report(error1);
	EXPECT_FALSE(handler->limit_);
	diag.Report(error1);
	EXPECT_FALSE(handler->limit_);
	diag.Report(error1);
	EXPECT_TRUE(handler->limit_);
}

TEST(Diagnostics, Format) {
	CompileOptions options;
	options.Initialize();
	Diagnostic diag(&options);
	shared<TestHandler> handler = new TestHandler();
	diag.AddHandler(handler);

	int error1 = GenError<1>::Value;
	diag.Report(error1)<<123<<string("abcd");
	string result = handler->last_.Format("first #1 second #2 first again #1");
	EXPECT_STREQ(_T("first 123 second abcd first again 123"), result.Chars());
}