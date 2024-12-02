// DateTime.hpp
// Copyright (c) Lup Gratian
//
// Represents an instant in time, typically expressed as a date and time of day.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_DATE_TIME_HPP
#define PC_BASE_DATE_TIME_HPP

#include "../Abstraction/PlatformTime.hpp"
#include "DateTimeConst.hpp"
#include "DebugValidator.hpp"
#include "TimeSpan.hpp"
#include <cmath>
#include <algorithm>
using namespace Abstraction;

namespace Base {

// Specifies the day of the week.
enum class DayOfWeek {
	Sunday    = 0,
	Monday    = 1,
	Tuesday   = 2,
	Wednesday = 3,
	Thursday  = 4,
	Friday    = 5,
	Saturday  = 6,
};


class DateTime : public DateTimeConst {
private:
	enum class DatePart {
		Year,
		Month,
		Day,
		DayOfYear
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	typedef DebugValidator Validator;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const __int64 TICKS_PER_MILLI = 10000;
	static const __int64 TICKS_PER_SECOND = TICKS_PER_MILLI * 1000;
	static const __int64 TICKS_PER_MINUTE = TICKS_PER_SECOND * 60;
	static const __int64 TICKS_PER_HOUR = TICKS_PER_MINUTE * 60;
	static const __int64 TICKS_PER_DAY = TICKS_PER_HOUR * 24;

	static const __int64 MILLIS_PER_SECOND = 1000;
	static const __int64 MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60;
	static const __int64 MILLIS_PER_HOUR = MILLIS_PER_MINUTE * 60;
	static const __int64 MILLIS_PER_DAY = MILLIS_PER_HOUR * 24;

	static const int DAYS_PER_YEAR = 365;
	static const int DAYS_PER_4_YEARS = DAYS_PER_YEAR * 4 + 1;
	static const int DAYS_PER_100_YEARS = DAYS_PER_4_YEARS * 25 - 1;
	static const int DAYS_PER_400_YEARS = DAYS_PER_100_YEARS * 4 + 1;
	static const __int64 FILE_TIME_OFFSET = DAYS_PER_400_YEARS * 4 * TICKS_PER_DAY;

	static const __int64 MIN_TICKS = 0;
	static const __int64 MAX_TICKS = MAXINT64;
	static const int MIN_YEAR = 1;
	static const int MAX_YEAR = 9999;
	
	__int64 ticks_;
	
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Splits a 64-bit integer into two 32-bit integers (the low and high part).
	static void Int64ToU32(__int64 value, unsigned long &lowPart, 
							unsigned long &highPart) {
		lowPart = (unsigned long)value;
		highPart = (unsigned long)(value >> 32);
	}

	// Splits a 64-bit integer into two 32-bit integers (the low and high part).
	static __int64 U32ToInt64(unsigned long lowPart, unsigned long highPart) {
		return ((__int64(highPart) << 32) | lowPart);
	}

	// Gets the ticks_ for the specified date.
	__int64 TicksFromDate(int year, int month, int day) const {
		const int *days = IsLeapYear(year) ? DAYS_BEFORE_MONTH_LY : DAYS_BEFORE_MONTH;
		int y = year - 1;
		return TICKS_PER_DAY * (y * 365 + y / 4 - y / 100 + y / 400 + 
								 days[month - 1] + day - 1);
	}

	// Gets the ticks_ for the specified time.
	__int64 TicksFromTime(int hour, int minute, int second, int millisecond) const	{
		return ((hour * TICKS_PER_HOUR) + (minute * TICKS_PER_MINUTE) +
				(second * TICKS_PER_SECOND) + (millisecond * TICKS_PER_MILLI));
	}

	// Gets a part (year/month/day) of the date.
	int GetDatePart(DatePart part) const {	
		int n = (int)(ticks_ / TICKS_PER_DAY); // Number of days since 1/1/0001.
		int y400 = n / DAYS_PER_400_YEARS;     // Number of whole 400-year periods since 1/1/0001.
		n -= y400 * DAYS_PER_400_YEARS;        // Day number within 400-year period.
		int y100 = n / DAYS_PER_100_YEARS;     // Number of whole 100-year periods within 400-year period.
		if (y100 == 4) y100 = 3;               // Last 100-year period has an extra day, so decrement result if 4.
		n -= y100 * DAYS_PER_100_YEARS;        // Day number within 100-year period.
		int y4 = n / DAYS_PER_4_YEARS;         // Number of whole 4-year periods within 100-year period.
		n -= y4 * DAYS_PER_4_YEARS;            // Day number within 4-year period.
		int y1 = n / DAYS_PER_YEAR;            // Number of whole years within 4-year period.
		if (y1 == 4) y1 = 3;                   // Last year has an extra day, so decrement result if 4.

		// If year was requested, compute and return it.
		if (part == DatePart::Year) return ((y400 * 400) + (y100 * 100) + (y4 * 4) + y1 + 1); 

		n -= y1 * DAYS_PER_YEAR;    // Day number within year.
		if (part == DatePart::DayOfYear) return n + 1; // If day-of-year was requested, return it.

		// Leap year calculation looks different from IsLeapYear since y1, y4,
		// and y100 are relative to year 1, not year 0.
		bool leapYear = y1 == 3 && (y4 != 24 || y100 == 3);
		const int *days = leapYear? DAYS_BEFORE_MONTH_LY: DAYS_BEFORE_MONTH;

		// All months have less than 32 days, so n >> 5 is a good conservative estimate for the month.
		int m = (n >> 5) + 1;

		// m = 1-based month number.
		while (n >= days[m]) {
			m++;
		}

		if (part == DatePart::Month) return m; // If month was requested, return it.

		// Return 1-based day-of-month.
		return (int)(n - days[m - 1] + 1);
	}

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Initializes a new instance to a specified number of ticks.
	DateTime(__int64 ticks) {
		Validator::IsLargerOrEqual(ticks, MIN_TICKS);
		Validator::IsSmallerOrEqual(ticks, MAX_TICKS);
		ticks_ = ticks;
	}

	// Initializes a new instance as a copy of the given instance.
	DateTime(const DateTime& other) : ticks_(other.ticks_) {}

	// Initializes a new instance to the specified year, month, and day.
	DateTime(int year, int month, int day) {
		ticks_ = TicksFromDate(year, month, day);
	}

	// Initializes a new instance to the specified year,  month, day, hour,
	// minute, and second.
	DateTime(int year, int month, int day, int hour, int minute, int second) {
		ticks_ = TicksFromDate(year, month, day) + 
				 TicksFromTime(hour, minute, second, 0);
	}

	// Initializes a new instance  to the specified year, month,  day, hour,
	// minute, second, and millisecond.
	DateTime(int year, int month, int day, int hour, int minute, 
			 int second, int millisecond) {
		ticks_ = TicksFromDate(year, month, day) + 
				 TicksFromTime(hour, minute, second, millisecond);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Indicates whether the specified year is a leap year.
	bool IsLeapYear(int year) const {
		return (year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0));
	}

	// Adds the value of the specified TimeSpan to the value of this instance.
	DateTime Add(TimeSpan value) const {
		return DateTime(ticks_ + value.Ticks());
	}

	// Adds the specified number of ticks_ to the value of this instance.
	DateTime AddTicks(__int64 value) const {
		return DateTime(ticks_ + value);
	}

	// Adds the specified number of days to the value of this instance.
	DateTime AddDays(double days) const {
		return DateTime((__int64)(ticks_ + (days * MILLIS_PER_DAY * TICKS_PER_MILLI)));
	}

	// Adds the specified number of months to the value of this instance.
	DateTime AddMonths(int months) const {
		int year = GetDatePart(DatePart::Year);
		int month = GetDatePart(DatePart::Month);
		int day = GetDatePart(DatePart::Day);
		int newMonth = month + (int)months - 1;

		// Adjust the month and the year.
		if(newMonth >= 0) {
			month = (newMonth % 12) + 1;
			year = year + (newMonth / 12);
		}
		else {
			month = ((newMonth + 1) % 12) + 12;
			year = year + ((newMonth - 11) / 12);
		}

		day = std::min(day, DaysInMonth(year, month));
		return DateTime(TicksFromDate(year, month, day) + (ticks_ % TICKS_PER_DAY));
	}

	// Adds the specified number of years to the value of this instance.
	DateTime AddYears(int years) const {
		return AddMonths(years * 12);
	}

	// Adds the specified number of hours to the value of this instance.
	DateTime AddHours(double hours) const {
		return DateTime((__int64)(ticks_ + (hours * MILLIS_PER_HOUR * TICKS_PER_MILLI)));
	}

	// Adds the specified number of minutes to the value of this instance.
	DateTime AddMinutes(double minutes) const {
		return DateTime((__int64)(ticks_ + (minutes * MILLIS_PER_MINUTE * TICKS_PER_MILLI)));
	}

	// Adds the specified number of seconds to the value of this instance.
	DateTime AddSeconds(double seconds) const {
		return DateTime((__int64)(ticks_ + (seconds * MILLIS_PER_SECOND * TICKS_PER_MILLI)));
	}

	// Adds the specified number of milliseconds to the value of this instance.
	DateTime AddMilliseconds(double milliseconds) const {
		return DateTime((__int64)(ticks_ + milliseconds * TICKS_PER_MILLI));
	}

	// Returns the number of days in the specified month and year.
	int DaysInMonth(int year, int month) const {
		Validator::IsLarger(month, 0);
		Validator::IsSmaller(month, 13);
		const int *days = IsLeapYear(year) ? DAYS_BEFORE_MONTH_LY : DAYS_BEFORE_MONTH;
		return (days[month] - days[month - 1]);
	}

	// Compares two instances of DateTime and returns an indication of their relative values.
	int Compare(const DateTime &t1, const DateTime &t2) {
		return (int)(t1.ticks_ - t2.ticks_);
	}

	// Converts the specified Windows file time to an equivalent local time.
	static DateTime FromFileTime(__int64 fileTime) {
		return DateTime(Time::FromFileTime(fileTime + FILE_TIME_OFFSET));
	}

	// Converts the specified Windows file time to an equivalent UTC time.
	static DateTime FromFileTimeUtc(__int64 fileTime) {
		return DateTime(fileTime + FILE_TIME_OFFSET);
	}

	// Subtracts the specified date and time from this instance.
	DateTime Substract(const DateTime &value) const {
		return DateTime(ticks_ - value.ticks_);
	}

	// Subtracts the specified duration from this instance.
	DateTime Substract(const TimeSpan &value) const {
		return DateTime(ticks_ - value.Ticks());
	}

	// Converts the value of the current DateTime object to a OS file time.
	__int64 ToFileTime() const {
		return Time::ToFileTime(ticks_ - FILE_TIME_OFFSET);
	}

	// Converts the value of the current DateTime object to local time.
	DateTime ToLocalTime() const {
		return DateTime(Time::ToLocalTime(ticks_) + FILE_TIME_OFFSET);
	}

	// Converts the value of the current DateTime object to its equivalent
	// long date string representation.
	string ToLongDateString() const {
		return Time::ToLongDateString(GetDatePart(DatePart::Year),
									  GetDatePart(DatePart::Month),
									  GetDatePart(DatePart::Day),
									  (int)DayOfWeek());
	}

	// Converts the value of the current DateTime object to its equivalent
	// short date string representation. 
	string ToShortDateString() const {
		return Time::ToShortDateString(GetDatePart(DatePart::Year),
									   GetDatePart(DatePart::Month),
									   GetDatePart(DatePart::Day),
									   (int)DayOfWeek());
	}

	// Converts the value of the current DateTime object to its equivalent
	// long time string representation.
	string ToLongTimeString() const {
		return Time::ToLongTimeString(Hour(), Minute(), Second(), Millisecond());
	}

	// Converts the value of the current DateTime object to its equivalent
	// short time string representation.
	string ToShortTimeString() const {
		return Time::ToShortTimeString(Hour(), Minute(), Second(), Millisecond());
	}

	int Year() const {
		return GetDatePart(DatePart::Year);
	}

	int Month() const {
		return GetDatePart(DatePart::Month);
	}
	 
	int Day() const {
		return GetDatePart(DatePart::Day);
	}
	 
	int DayOfYear() const {
		return GetDatePart(DatePart::DayOfYear);
	}

	int Hour() const {
		return ((ticks_ / TICKS_PER_HOUR) % 24);
	}
	 
	int Minute() const {
		return ((ticks_ / TICKS_PER_MINUTE) % 60);
	}
	 
	int Second() const {
		return ((ticks_ / TICKS_PER_SECOND) % 60);
	}
	 
	int Millisecond() const {
		return ((ticks_ / TICKS_PER_MILLI) % 1000);
	}
	 
	static DateTime Now() {
		int year, month, day, hour, minute, second, milli;

		Time::Now(year, month, day, hour, minute, second, milli);
		return DateTime(year, month, day, minute, second, milli);
	}
	 
	DateTime ToUniversalTime() const {
		return DateTime(ToFileTime());
	}
	
	DateTime Date() const {
		return DateTime(GetDatePart(DatePart::Year), 
						 GetDatePart(DatePart::Month), 
						 GetDatePart(DatePart::Day));
	}
	 
	__int64 Ticks() const {
		return ticks_;
	}
	
	TimeSpan TimeOfDay() const {
		return TimeSpan(0, Minute(), Second(), Millisecond());
	}

	static DateTime Today() {
		DateTime now = Now();
		return DateTime(now.Year(), now.Month(), now.Day());
	}
	 
	DateTime UTCNow() {
		return DateTime(Now().ToFileTime() + FILE_TIME_OFFSET);
	}

	static DateTime MIN_VALUE() {
		return DateTime(DateTime::MIN_TICKS);
	}

	static DateTime MAX_VALUE() {
		return DateTime(DateTime::MAX_TICKS);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const DateTime &dt) const {
		return (ticks_ == dt.ticks_);
	}

	bool operator!= (const DateTime &dt) const {
		return (ticks_ != dt.ticks_);
	}

	bool operator< (const DateTime &dt) const {
		return (ticks_ < dt.ticks_);
	}

	bool operator<= (const DateTime &dt) const {
		return (ticks_ <= dt.ticks_);
	}

	bool operator> (const DateTime &dt) const {
		return (ticks_ > dt.ticks_);
	}

	bool operator>= (const DateTime &dt) const {
		return (ticks_ >= dt.ticks_);
	}

	DateTime operator+ (const TimeSpan &ts) const {
		return Add(ts);
	}

	DateTime operator- (const DateTime &dt) const {
		return Substract(dt);
	}

	DateTime operator- (const TimeSpan &ts) const {
		return Substract(ts);
	}

	DateTime operator+= (const TimeSpan &ts) const {
		return Add(ts);
	}

	DateTime operator-=(const DateTime &dt) const {
		return Substract(dt);
	}

	DateTime operator-=(const TimeSpan &ts) const {
		return Substract(ts);
	}
};

} // namespace Base
#endif