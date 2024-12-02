// ProfileAdjustment.hpp
// Copyright (c) Lup Gratian
//
// Defines helpers that can be used to adjust certain control values
// based on the probability that a certain path in the CFG is taken
// (for example, boost the limit of hoisted instructions if they
// are very likely to be executed).
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_PROFILE_ADJUSTMENT_HPP
#define PC_ANALYSIS_PROFILE_ADJUSTMENT_HPP

#include "ProfileInfo.hpp"
#include "../Base/DebugValidator.hpp"
#include "../IR/Block.hpp"
#include "../IR/Unit.hpp"
#include "../IR/Function.hpp"
#include "../IR/Instructions.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

template <int VeryHighFrequencyValue,  // The minimum value for the very high frequency.
          int HighFrequencyValue,      // The minimum value for the high frequency.
          int LowFrequencyValue,       // The maximum value for the low frequency.
          int VeryLowFrequencyValue,   // The maximum value for the very low frequency.
          int VeryHighFrequencyBoost,  // Boosting if very high frequency (value *= boost).
          int HighFrequencyBoost,      // Boosting if high frequency (value *= boost).
          int LowFrequencyPenalty,     // Penalty if low frequency (value /= penalty).
          int VeryLowFrequencyPenalty, // Penalty if very low frequency (value /= penalty).
          int LowConfidencePenalty,    // 'true' if the value should be penalized if low confidence.
          bool BoostInLoop,            // 'true' for loop boosting if no frequency information.
          int LoopDepth3Frequency,     // The frequency if the block is in a loop with depth 3.
          int LoopDepth2Frequency,     // The frequency if the block is in a loop with depth 2.
          int LoopDepth1Frequency,     // The frequency if the block is in a loop with depth 1.
          int MaximumValue>            // The maximum allowed value.
struct ProfileAdjustmentPolicy {
    static const int VeryHighFrequencyValue  = VeryHighFrequencyValue;
    static const int HighFrequencyValue      = HighFrequencyValue;
    static const int LowFrequencyValue       = LowFrequencyValue;
    static const int VeryLowFrequencyValue   = VeryLowFrequencyValue;
    static const int VeryHighFrequencyBoost  = VeryHighFrequencyBoost; 
    static const int HighFrequencyBoost      = HighFrequencyBoost;    
    static const int LowFrequencyPenalty     = LowFrequencyPenalty;  
    static const int VeryLowFrequencyPenalty = VeryLowFrequencyPenalty;
    static const int LowConfidencePenalty    = LowConfidencePenalty;   
    static const bool BoostInLoop            = BoostInLoop;
    static const int LoopDepth3Frequency     = LoopDepth3Frequency;    
    static const int LoopDepth2Frequency     = LoopDepth2Frequency;   
    static const int LoopDepth1Frequency     = LoopDepth1Frequency;
    static const int MaximumValue            = MaximumValue;
};


template <class T, class Policy>
class ProfileAdjustment {
private:
    ProfileInfo* profileInfo_;

public:
    ProfileAdjustment() : profileInfo_(nullptr) {}

    ProfileAdjustment(ProfileInfo* profileInfo) : profileInfo_(profileInfo) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    int Adjust(T value, Block* fromBlock, Block* toBlock) {
        DebugValidator::IsNotNull(fromBlock);
        DebugValidator::IsNotNull(toBlock);

        // Try to use profile information. If not available
        // set a frequency based on the fact that the block
        // is found in a loop or not.
        FrequencyInfo info;

        if(profileInfo_) {
            info = profileInfo_->GetExecutionFrequency(fromBlock, toBlock);
        }

        if(info.IsUnknown() && Policy::BoostInLoop && toBlock->IsInLoop()) {
            if(toBlock->LoopDepth() == 1) {
                info = FrequencyInfo(Policy::LoopDepth1Frequency,
                                     FrequencyConfidence::Medium);
            }
            else if(toBlock->LoopDepth() == 2) {
                info = FrequencyInfo(Policy::LoopDepth2Frequency,
                                     FrequencyConfidence::Medium);
            }
            else if(toBlock->LoopDepth() >= 3) {
                info = FrequencyInfo(Policy::LoopDepth3Frequency,
                                     FrequencyConfidence::Medium);
            }
        }

        if(info.IsValid()) {
            if(info.Frequency() >= Policy::VeryHighFrequencyValue) {
                value *= Policy::VeryHighFrequencyBoost;
            }
            else if(info.Frequency() >= Policy::HighFrequencyValue) {
                value *= Policy::HighFrequencyBoost;
            }
            else if(info.Frequency() < Policy::VeryLowFrequencyValue) {
                value /= Policy::VeryLowFrequencyPenalty;
            }
            else if(info.Frequency() < Policy::LowFrequencyValue) {
                value /= Policy::LowFrequencyPenalty;
            }

            // Penalize if the confidence is very low.
            if(info.HasLowConfidence()) {
                value /= Policy::LowConfidencePenalty;
            }
        }

        value = std::min(value, Policy::MaximumValue);
        return value;
    }
};


// The default adjustment policy. Other values can easily
// be defined and used to create new adjustment classes.
typedef ProfileAdjustmentPolicy<90, 75, 45, 10, 4, 2, 2, 10000,
                                2, true, 75, 75, 50, 1000> DefaultProfileAdjustmentPolicy;

// Default adjustment for integer values.
typedef ProfileAdjustment<int, DefaultProfileAdjustmentPolicy> IntProfileAdjustment;
typedef ProfileAdjustment<__int64, DefaultProfileAdjustmentPolicy> Int64ProfileAdjustment;

// Default adjustment for float values.
typedef ProfileAdjustment<float, DefaultProfileAdjustmentPolicy> FloatProfileAdjustment;
typedef ProfileAdjustment<double, DefaultProfileAdjustmentPolicy> DoubleProfileAdjustment;

} // namespace Analysis
#endif