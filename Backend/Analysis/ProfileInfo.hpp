#ifndef PC_ANALYSIS_PROFILE_INFO_HPP
#define PC_ANALYSIS_PROFILE_INFO_HPP  

#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
using namespace IR;

namespace Analysis {

enum class FrequencyConfidence {
    High,
    Medium,
    Low
};


class FrequencyInfo {
private:
    static const int UNKNOWN = -1;

    int frequency_;
    FrequencyConfidence confidence_;

public:
    FrequencyInfo() : 
            frequency_(UNKNOWN), confidence_(FrequencyConfidence::Medium) {}

    FrequencyInfo(int frequency, FrequencyConfidence confidence = FrequencyConfidence::Medium) :
            frequency_(frequency), confidence_(confidence) {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool IsUnknown() const {
        return frequency_ == UNKNOWN;   
    }

    bool IsValid() const {
        return frequency_ != UNKNOWN;
    }

    int Frequency() const {
        return frequency_;
    }

    float FrequencyPercent() const {
        return (float)frequency_ / 100.0f;
    }

    FrequencyConfidence Confidence() const {
        return confidence_;
    }

    bool HasHighConfidence() const {
        return confidence_ == FrequencyConfidence::High;
    }

    bool HasMediumConfidence() const {
        return confidence_ == FrequencyConfidence::Medium;
    }

    bool HasLowConfidence() const {
        return confidence_ == FrequencyConfidence::Low;
    }

    bool HasLowerConfidence(const FrequencyInfo& other) const {
        if(confidence_ == FrequencyConfidence::High) {
            return other.confidence_ != FrequencyConfidence::High;
        }
        else if(confidence_ == FrequencyConfidence::Medium) {
            return other.confidence_ == FrequencyConfidence::Low;
        }
        else return false;
    }

    bool HasHigherConfidence(const FrequencyInfo& other) const {
        if(confidence_ == FrequencyConfidence::Low) {
            return other.confidence_ != FrequencyConfidence::Low;
        }
        else if(confidence_ == FrequencyConfidence::Medium) {
            return other.confidence_ == FrequencyConfidence::High;
        }
        else return false;
    }

    unsigned GetHashCode() const {
        return frequency_  ^ (unsigned)confidence_;
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    bool operator== (const FrequencyInfo& other) const {
        return (frequency_ == other.frequency_) &&
               (confidence_ == other.confidence_);
    }

    bool operator!= (const FrequencyInfo& other) const {
        return !operator== (other);
    }

    bool operator< (const FrequencyInfo& other) const {
        return HasLowerConfidence(other) ||
               (frequency_ < other.frequency_);
    }

    bool operator<= (const FrequencyInfo& other) const {
        return HasLowerConfidence(other) ||
            (frequency_ < other.frequency_);
    }

    bool operator> (const FrequencyInfo& other) const {
        return HasHigherConfidence(other) ||
               (frequency_ > other.frequency_);
    }

    bool operator>= (const FrequencyInfo& other) const {
        return HasHigherConfidence(other) ||
            (frequency_ > other.frequency_);
    }
};


class ProfileInfo {
public:
    int GetExecutionFrequency(Block* block) {
        // sum of incoming edge values
        return 0;
    }

    // 0/100   -1 unknown
    FrequencyInfo GetExecutionFrequency(Block* fromBlock, Block* toBlock) {
        //return 98;
        return FrequencyInfo();
    }

    float GetNormalizedExecutionFrequency(Block* fromBlock, Block* toBlock) {
        return 0;
        /*int edgeFrequency = GetExecutionFrequency(fromBlock, toBlock);
        int blockFrequency = GetExecutionFrequency(toBlock);
        return (float)edgeFrequency / (float)blockFrequency;*/
    }

    FrequencyInfo GetDynamicExecutionFrequency(Instruction* instr) {
        return FrequencyInfo();
    }
};

} // namespace Analysis
#endif