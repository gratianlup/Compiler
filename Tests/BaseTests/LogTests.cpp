#include "../../Base/String.hpp"
#include "../../Base/Log.hpp"
#include "../../Base/LogFileTarget.hpp"
#include <gtest\gtest.h>
using namespace Base;

class TestTarget : public LogTarget {
public:
	int ErrorCount;
	int WarningCount;
	int InfoCount;
	int SectionCount;

	TestTarget() : ErrorCount(0), WarningCount(0), InfoCount(0), SectionCount(0) {}

	virtual void Log(const string& message, LogSeverity severity, int level) {
		switch(severity) {
			case LogSeverity_Error: {
				ErrorCount++;
				break;
			}
			case LogSeverity_Warning: {
				WarningCount++;
				break;
			}
			case LogSeverity_Info: {
				InfoCount++;
				break;
			}
		}
	}

	virtual void Section(const string& title, int level) {
		SectionCount++;
	}
};

TEST(DISABLED_Log, Error) {
	LogTarget* target = new LogFileTarget(_T("D:\\test.txt"));

	Log::AddTarget(target);
	Log::Section(_T("Test section"));
	Log::Error(_T("Test error 1"));
	Log::Section(_T("Test section"), 1);
	Log::Enter();
	Log::Warning(_T("Test warning 1"));
	Log::Info(_T("Test info 1"));
	Log::Exit();
	Log::Error(_T("Test error 2"));
	//ASSERT_EQ(1, target->ErrorCount);
}
