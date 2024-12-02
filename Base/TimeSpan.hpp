// TimeSpan.hpp
// Copyright (c) Lup Gratian
//
// A TimeSpan object represents a time interval or duration of time.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_TIME_SPAN_HPP
#define PC_BASE_TIME_SPAN_HPP

#include <cmath>

namespace Base {

class TimeSpan {
public:
	static const __int64 TicksPerMillisecond = 10000;
	static const __int64 TicksPerSecond = TicksPerMillisecond * 1000;
	static const __int64 TicksPerMinute = TicksPerSecond * 60;
	static const __int64 TicksPerHour = TicksPerMinute * 60;
	static const __int64 TicksPerDay = TicksPerHour * 24;

private:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	static const __int64 MaxMilliseconds = MAXINT64 / TicksPerMillisecond;
	static const __int64 MinMilliseconds = -MaxMilliseconds;
	static const __int64 MaxSeconds = MAXINT64 / TicksPerSecond;
	static const __int64 MinSeconds = -MaxSeconds;

	static const __int64 MillisPerSecond = 1000;
	static const __int64 MillisPerMinute = MillisPerSecond * 60;
	static const __int64 MillisPerHour = MillisPerMinute * 60;
	static const __int64 MillisPerDay = MillisPerHour * 24;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	__int64 ticks_;

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	TimeSpan() : ticks_(0) {}

	TimeSpan(__int64 ticks) : ticks_(ticks) {}
	
	TimeSpan(int hours, int minutes, int seconds) {
		ticks_ = (hours * TicksPerHour) + (minutes * TicksPerMinute) +
				 (seconds * TicksPerSecond);
	}
	
	TimeSpan(int days, int hours, int minutes, int seconds) {
		ticks_ = (days * TicksPerDay) + (hours * TicksPerHour) + 
				 (minutes * TicksPerMinute) + (seconds * TicksPerSecond);
	}
	
	TimeSpan(int days, int hours, int minutes, int seconds, 
					   __int64 milliseconds) {
		ticks_ = (days * TicksPerDay) + (hours * TicksPerHour) + 
				 (minutes * TicksPerMinute) + (seconds * TicksPerSecond) +
				 (milliseconds * TicksPerMillisecond);
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	TimeSpan& Add(TimeSpan ts) {
		ticks_ += ts.ticks_;
		return *this;
	}

	TimeSpan& Substract(TimeSpan ts) {
		ticks_ -= ts.ticks_;
		return *this;
	}

	TimeSpan Negate() {
		return TimeSpan(-ticks_);
	}

	TimeSpan Duration() {
		return TimeSpan(std::abs(ticks_));
	}

	static TimeSpan FromTicks(int ticks) {
		return TimeSpan(ticks);
	}

	
	static TimeSpan FromMilliseconds(__int64 milliseconds) {
		return TimeSpan(0, 0, 0, 0, milliseconds);
	}

	
	static TimeSpan FromSeconds(int seconds) {
		return TimeSpan(0, 0, 0, seconds, 0);
	}

	
	static TimeSpan FromMinutes(int minutes) {
		return TimeSpan(0, 0, minutes, 0, 0);
	}

	
	static TimeSpan FromHours(int hours) {
		return TimeSpan(0, hours, 0, 0, 0);
	}

	
	static TimeSpan FromDays(int days) {
		return TimeSpan(days, 0, 0, 0, 0);
	}

	int Days() const {
		return (int)(ticks_ / TicksPerDay);
	}

	
	int Hours() const {
		return (int)((ticks_ / TicksPerHour) % 24);
	}

	
	int Minutes() const {
		return (int)((ticks_ / TicksPerMinute) % 60);
	}

	
	int Seconds() const {
		return (int)((ticks_ / TicksPerSecond) % 60);
	}

	
	__int64 Ticks() const {
		return ticks_;
	}

	
	double TotalDays() const {
		return (double)ticks_ / TicksPerDay;
	}

	
	double TotalHours() const {
		return (double)ticks_ / TicksPerHour;
	}

	
	double TotalMinutes() const {
		return (double)ticks_ / TicksPerMinute;
	}

	
	double TotalSeconds() const {
		return (double)ticks_ / TicksPerSecond;
	}

	
	double TotalMilliseconds() const {
		return (double)ticks_ / TicksPerMillisecond;
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const TimeSpan& ts) const {
		return (ticks_ == ts.ticks_);
	}

	bool operator!= (const TimeSpan& ts) const {
		return (ticks_ != ts.ticks_);
	}

	bool operator< (const TimeSpan& ts) const {
		return (ticks_ < ts.ticks_);
	}

	bool operator<= (const TimeSpan& ts) const {
		return (ticks_ <= ts.ticks_);
	}

	bool operator> (const TimeSpan& ts) const {
		return (ticks_ > ts.ticks_);
	}

	bool operator>= (const TimeSpan& ts) const {
		return (ticks_ >= ts.ticks_);
	}

	TimeSpan operator+ (const TimeSpan& ts) {
		return Add(ts);
	}

	TimeSpan operator- (const TimeSpan& ts) {
		return Substract(ts);
	}

	TimeSpan& operator+= (const TimeSpan& ts) {
		ticks_ += ts.ticks_;
		return *this;
	}

	TimeSpan& operator-=(const TimeSpan& ts) {
		ticks_ -= ts.ticks_;
		return *this;
	}
};

} // namespace Base
#endif