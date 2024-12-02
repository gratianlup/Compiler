// LoopTag.hpp
// Copyright (c) Lup Gratian
//
// Defines a tag used to store the Natural Loops found in a function.
// When blocks are added or removed it tries to update the loops.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_LOOP_TAG_HPP
#define PC_ANALYSIS_LOOP_TAG_HPP

#include "Loop.hpp"
#include "SparseBitVector.hpp"
#include "../IR/Function.hpp"
#include "../IR/Block.hpp"
#include "../IR/Instructions.hpp"
#include "../IR/References.hpp"
#include "../IR/Unit.hpp"
#include "../Base/MakePair.hpp"
#include "../Base/String.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

class LoopTag : public FunctionTag {
private:
    typedef StaticList<shared<Loop>, 2> LoopList;
    typedef Dictionary<unsigned, Loop*> LoopDictionary;

    LoopList loops_;              // The list with the loop roots in the function.
    LoopDictionary headerToLoop_; // Maps a header block to the associated loop.
    SparseBitVector loopBlocks_;  // Marks all blocks that are part of loops.
    bool valid_;                  // 'true' if the information is still valid.

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    LoopTag() : valid_(true) {}          // Should not be created directly.
    LoopTag(const LoopTag&);             // Should not be copied.
    LoopTag& operator= (const LoopTag&); // Should not be assigned.

    virtual void BlockAdded(Block* block, Function* parent) override {
        valid_ = false;
    }

    virtual void BlockRemoved(Block* block, Function* parent) override {
        //! TODO: improve this by updating loops.
        if(loopBlocks_.IsSet(block->Id())) {
            valid_ = false;
        }
    }

public:
    static const int Id = 0x4d312af9;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const override {
        return Id;
    }

    // Creates a new empty loop tag.
    static LoopTag* GetLoop() {
        return new LoopTag();
    }

    // Returns 'true' if the loop information is up to date
    // and can be used to perform optimizations.
    bool IsSafeToUse() const {
        return valid_;
    }

    void SetIsSafeToUse(bool value) {
        valid_ = value;
    }

    // Adds the specified loop to the list.
    void AddLoop(shared<Loop> loop) {
        DebugValidator::IsNotNull(loop.Raw());
        DebugValidator::IsTrue(IsSafeToUse());
        
        loops_.Add(loop);
        headerToLoop_.Add(loop->GetLoopHeader()->Id(), loop);

        // Mark the blocks part of the loop.
        loop->ForEachBlock([this](Block* block) -> bool {
            loopBlocks_.SetBit(block->Id());
            return true;
        });
    }

    // Returns the number of loops identified in the function.
    int LoopCount() const {
        DebugValidator::IsTrue(IsSafeToUse());
        return loops_.Count();
    }

    // Returns 'true' if at least one loop could be identified in the function.
    bool HasLoops() const {
        DebugValidator::IsTrue(IsSafeToUse());
        return loops_.IsNotEmpty();
    }

    // Returns the loop found at the specified position.
    Loop* GetLoop(int index) {
        DebugValidator::IsTrue(IsSafeToUse());
        return loops_[index];
    }

    const Loop* GetLoop(int index) const {
        DebugValidator::IsTrue(IsSafeToUse());
        return loops_[index];
    }

    // Returns the loop which starts with the specified loop header.
    Loop* GetLoop(Block* headerBlock) {
        DebugValidator::IsNotNull(headerBlock);
        DebugValidator::IsTrue(IsSafeToUse());
        Loop* loop;

        if(headerToLoop_.TryGetValue(headerBlock->Id(), &loop)) {
            return loop;
        }
        else return nullptr;
    }

    // Returns the loop which starts with the specified loop header (const version).
    const Loop* GetLoop(Block* headerBlock) const {
        DebugValidator::IsNotNull(headerBlock);
        DebugValidator::IsTrue(IsSafeToUse());
        Loop* loop;

        if(headerToLoop_.TryGetValue(headerBlock->Id(), &loop)) {
            return loop;
        }
        else return nullptr;
    }

    // Returns 'true' if the specified block is the header of a loop.
    bool IsLoopHeader(Block* block) const {
        DebugValidator::IsTrue(IsSafeToUse());
        return headerToLoop_.ContainsKey(block->Id());
    }

    // Removes all information about the loop found at the specified index.
    void RemoveLoop(int index) {
        DebugValidator::IsTrue(IsSafeToUse());
        headerToLoop_.Remove(loops_[index]->GetLoopHeader()->Id());
        loops_.RemoveAt(index);

        if(loops_.IsEmpty()) {
            loopBlocks_.Clear();
        }
    }

    // Performs the specified action on each identified loop.
    // bool Predicate(Loop* loop)
    template <class Predicate>
    void ForEachLoop(Predicate action) {
        DebugValidator::IsTrue(IsSafeToUse());

        for(int i = 0; i < loops_.Count(); i++) {
            if(action(loops_[i]) == false) {
                return;
            }
        }
    }

    // Performs the specified action on each identified loop.
    // bool Predicate(const Loop* loop) const
    template <class Predicate>
    void ForEachLoop(Predicate action) const {
        DebugValidator::IsTrue(IsSafeToUse());

        for(int i = 0; i < loops_.Count(); i++) {
            if(action(loops_[i]) == false) {
                return;
            }
        }
    }

    // Returns 'true' if the specified block is part of a loop.
    bool IsLoopBlock(Block* block) const {
        DebugValidator::IsTrue(IsSafeToUse());
        DebugValidator::IsNotNull(block);

        return loopBlocks_.IsSet(block->Id());
    }
};

} // namespace Analysis

namespace IR {
namespace Detail {
    // Implements support for "dynamic cast".
    template <>
    struct TagPromoter<Analysis::LoopTag> {
        static bool Is(const Tag* tag) {
            return tag->GetId() == Analysis::LoopTag::Id;
        }

        static Analysis::LoopTag* As(Tag* tag) {
            return Is(tag) ? static_cast<Analysis::LoopTag*>(tag) : nullptr;
        }
    };
} // namespace Detail
} // namespace IR
#endif