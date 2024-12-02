// IO.hpp
// Copyright (c) Lup Gratian
//
// Exposes IO-related functionality for the Windows OS.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_TIME_HPP
#define PC_ABSTRACTION_TIME_HPP

#define NOMINMAX
#include <Windows.h>
#include "../../Base/DebugValidator.hpp"
#include "../../Base/String.hpp"
using namespace Base;

namespace Abstraction {

class Time {
public:
	typedef DebugValidator Validator;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const __int64 FILE_TIME_OFFSET = 0;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Converts the specified file time to ticks.
	static __int64 FromFileTime(__int64 time) {
		// Adjust to local time.
		FILETIME utcTime = *reinterpret_cast<FILETIME*>(&time);
		FILETIME localTime;

		if(::FileTimeToLocalFileTime(&utcTime, &localTime)) {
			return *((__int64*)&localTime) + FILE_TIME_OFFSET;
		}

		return time + FILE_TIME_OFFSET; // Couldn't convert to local time
	}

	// Convert the specified tick value to a file time.
	static __int64 ToFileTime(__int64 time) {
		FILETIME localTime = *reinterpret_cast<FILETIME*>(&time);
		FILETIME utcTime;

		if(::LocalFileTimeToFileTime(&localTime, &utcTime) == false) {
			Validator::IsTrue(false);
			utcTime = localTime;
		}

		return *((__int64*)&utcTime);
	}

	// Converts the specified file time to local file time.
	static __int64 ToLocalTime(__int64 time) {
		FILETIME localTime = *reinterpret_cast<FILETIME*>(&time);
		FILETIME utcTime;

		if(::LocalFileTimeToFileTime(&localTime, &utcTime) == false) {
			Validator::IsTrue(false);
			utcTime = localTime;
		}

		return *((__int64*)&utcTime);
	}

	// Converts the specified date to a long string.
	static string ToLongDateString(int year, int month, int day, int dow) {
		wchar_t buffer[MAX_PATH + 1];
		SYSTEMTIME date;

		::ZeroMemory(&date, sizeof(date));
		date.wYear = (WORD)year;
		date.wMonth = (WORD)month;
		date.wDay = (WORD)day;
		date.wDayOfWeek = (WORD)dow;

		if(::GetDateFormat(MAKELCID(LOCALE_INVARIANT, SORT_DEFAULT), 0, &date, 
						   _T("dddd','MMMM dd','yyyy"), buffer, MAX_PATH)) {
			return string(buffer);
		}

		Validator::IsTrue(false);
		return ""; // Failed.
	}

	// Converts the specified date to a short string.
	static string ToShortDateString(int year, int month, int day, int dow) {
		wchar_t buffer[MAX_PATH + 1];
		SYSTEMTIME date;

		::ZeroMemory(&date, sizeof(date));
		date.wYear = (WORD)year;
		date.wMonth = (WORD)month;
		date.wDay = (WORD)day;
		date.wDayOfWeek = (WORD)dow;

		if(::GetDateFormat(MAKELCID(LOCALE_INVARIANT, SORT_DEFAULT), 0, &date, 
						   _T("M'/'d'/'yyyy"), buffer, MAX_PATH)) {
			return string(buffer);
		}

		Validator::IsTrue(false);
		return ""; // Failed.
	}

	// Converts the specified time to a long string.
	static string ToLongTimeString(int hour, int minute, int second, int milli) {
		wchar_t buffer[MAX_PATH + 1];
		SYSTEMTIME date;

		::ZeroMemory(&date, sizeof(date));
		date.wHour = (WORD)hour;
		date.wMinute = (WORD)minute;
		date.wSecond = (WORD)second;
		date.wMilliseconds = (WORD)milli;

		if(::GetTimeFormat(MAKELCID(LOCALE_INVARIANT, SORT_DEFAULT), LOCALE_USE_CP_ACP,
						   &date, _T("hh':'mm':'ss tt"), buffer, MAX_PATH)) {
			return string(buffer);
		}

		Validator::IsTrue(false);
		return ""; // Failed.
	}

	// Converts the specified time to a short string.
	static string ToShortTimeString(int hour, int minute, int second, int milli) {
		wchar_t buffer[MAX_PATH + 1];
		SYSTEMTIME date;

		::ZeroMemory(&date, sizeof(date));
		date.wHour = (WORD)hour;
		date.wMinute = (WORD)minute;
		date.wSecond = (WORD)second;
		date.wMilliseconds = (WORD)milli;

		if(::GetTimeFormat(MAKELCID(LOCALE_INVARIANT, SORT_DEFAULT), 
						   LOCALE_USE_CP_ACP | TIME_FORCE24HOURFORMAT,
						   &date, _T("HH':'mm':'ss"), buffer, MAX_PATH)) {
			return string(buffer);
		}

		Validator::IsTrue(false);
		return ""; // Failed.
	}

	// Gets the current date and time.
	static void Now(int& year, int& month, int& day, int& hour, int& minute, 
					int& second, int& milli) {
		SYSTEMTIME time;
		::GetLocalTime(&time);

		year = time.wYear;
		month = time.wMonth;
		hour = time.wDay;
		minute = time.wMinute;
		second = time.wMinute;
		milli = time.wMilliseconds;
	}

	// Gets the current tick count.
	static int TickCount() {
		return (int)::GetTickCount();
	}

	static __int64 GetHRTimerFrequency() {
		::LARGE_INTEGER temp;
		if(::QueryPerformanceFrequency(&temp)) {
			return temp.QuadPart;
		}
		else return 0;
	}

	// Gets the value of the High Resolution system timer.
	static __int64 GetHRTimerCounter() {
		::LARGE_INTEGER temp;
		if(::QueryPerformanceCounter(&temp)) {
			return temp.QuadPart;
		}
		else return 0;
	}

	// Sleeps for the specified time.
	static void Sleep(int milliseconds) {
		::Sleep(milliseconds);
	}
};

} // namespace Abstraction
#endif