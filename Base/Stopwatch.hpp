// Stopwatch.hpp
// Copyright (c) Lup Gratian
//
// Provides a set of methods to accurately measure elapsed time. 
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_BASE_STOPWATCH_HPP
#define PC_BASE_STOPWATCH_HPP

#include "TimeSpan.hpp"
#include "../Abstraction/PlatformTime.hpp"
using namespace Abstraction;

namespace Base {

class Stopwatch {
private:
	bool initialized_;
	bool isHighRes_;    // True if a high resolution timer is available.
	__int64 frequency_; // The frequency of the HR timer.

	__int64 startTime_;
	__int64 ellapsed_;
	bool running_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	__int64 GetTime() {
		if(isHighRes_) {
			__int64 counter = Time::GetHRTimerCounter();
			return (__int64)((double)(counter * 1000) / (double)frequency_);
		}
		else return Time::TickCount();
	}

public:
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Stopwatch() {
		frequency_ = Time::GetHRTimerFrequency();
		isHighRes_ = frequency_ != 0;
		initialized_ = true;
		Reset();
	}

	// Starts or resumes measuring elapsed time for an interval.
	void Start() {
		if(running_ == false) {
			startTime_ = GetTime();
			running_ = true;
		}
	}

	// Stops measuring elapsed time for an interval.
	void Stop() {
		if(running_) {
			// Compute final duration.
			__int64 temp = GetTime() - startTime_;
			ellapsed_ += temp;
			running_ = false;
		}
	}

	// Stops time interval measurement and resets the elapsed time to zero.
	void Reset() {
		running_ = false;
		ellapsed_ = 0;
	}

	// Gets the total elapsed time measured by the current instance, in milliseconds.
	__int64 EllapsedMilliseconds() {
		if(running_) {
			__int64 temp = GetTime() - startTime_;
			ellapsed_ += temp;
			startTime_ += temp;
		}

		return ellapsed_;
	}
	
	// Gets the total elapsed time measured by the current instance.
	TimeSpan Ellapsed() {
		return TimeSpan::FromMilliseconds(EllapsedMilliseconds());
	}

	// Gets a value indicating whether the Stopwatch timer is running.
	bool IsRunning() const {
		return running_;
	}

	// Indicates whether the timer is based on a high-resolution performance counter.
	bool IsHighResolution() const {
		return isHighRes_;
	}
};

} // namespace Base
#endif