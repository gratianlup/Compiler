// Tagged.hpp
// Copyright (c) Lup Gratian
//
// The base class from which all objects that can be tagged should derive.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_TAGGED_HPP
#define PC_IR_TAGGED_HPP

#include "Tag.hpp"
#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/StaticList.hpp"
using namespace Base;

namespace IR {

class TaggedBase {
    // This exists just to make all Tagged<T> objects 
    // derive from a common base class.
};


template <class T>
class Tagged : public TaggedBase {
private:
    StaticList<T*, 4>* tags_;

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    void CreateListIfRequired() {
        if(tags_ == nullptr) {
            tags_ = new StaticList<T*, 4>();
        }
    }

    void RemoveListIfAllowed() {
        if(tags_ && (tags_->Count() == 0)) {
            delete tags_;
            tags_ = nullptr;
        }
    }

public:
    Tagged() : tags_(nullptr) {}

    virtual ~Tagged() {
        RemoveAllTags();
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    // Adds the specified tag to the list.
    // Note that each tag class should be represented by a single tag.
    void AddTag(T* tag) {
        DebugValidator::IsNotNull(tag);
        DebugValidator::IsFalse(HasTag(tag->GetId()));

        CreateListIfRequired();
        tags_->Add(tag);
        tag->AttachedToParent(this);
    }

    // Returns 'true' if a tag having the specified class is found.
    template <class T>
    bool HasTag() const {
        if(tags_) {
            // We use a linear search because we usually have few tags.
            for(int i = 0; i < tags_->Count(); i++) {
                if((*tags_)[i]->Is<T>()) {
                    return true;
                }
            }
        }
        
        return false;
    }

    // Returns 'true' if a tag having the specified Id is found.
    bool HasTag(int tagId) const {
        if(tags_) {
            // We use a linear search because we usually have few tags.
            for(int i = 0; i < tags_->Count(); i++) {
                if((*tags_)[i]->GetId() == tagId) {
                    return true;
                }
            }
        }
        
        return false;
    }

    // Returns the tag having the specified tag,
    // or 'nullptr' if no such tag is found in the list.
    template <class T>
    T* GetTag() {
        if(tags_) {
            // We use a linear search because we usually have few tags.
            for(int i = 0; i < tags_->Count(); i++) {
                if(auto tag = (*tags_)[i]->As<T>()) {
                    return tag;
                }
            }
        }

        return nullptr;
    }

    template <class T>
    const T* GetTag() const {
        if(tags_) {
            // We use a linear search because we usually have few tags.
            for(int i = 0; i < tags_->Count(); i++) {
                if(auto tag = (*tags_)[i]->As<T>()) {
                    return tag;
                }
            }
        }

        return nullptr;
    }

    T* GetTag(int index) {
        DebugValidator::IsNotNull(tags_);
        return (*tags_)[index];
    }

    const T* GetTag(int index) const {
        DebugValidator::IsNotNull(tags_);
        return (*tags_)[index];
    }

    // Returns the number of associated tags.
    int TagCount() const {
        if(tags_) return tags_->Count();
        else return 0;
    }

    // Returns 'true' if the object has at least one associated tag.
    bool HasTags() const {
        return tags_ && (tags_->Count() > 0);
    }

    // Performs the specified action on each tag.
    template <class Predicate>
    void ForEachTag(Predicate action) {
        if(tags_ == nullptr) {
            return;
        }

        for(int i = 0; i < tags_->Count(); i++) {
            if(action((*tags_)[i]) == false) {
                return;
            }
        }
    }

    template <class Predicate>
    void ForEachTag(Predicate action) const {
        if(tags_ == nullptr) {
            return;
        }

        for(int i = 0; i < tags_->Count(); i++) {
            if(action((*tags_)[i]) == false) {
                return;
            }
        }
    }

    // Removes the specified tag, if it exists.
    void RemoveTag(T* tag) {
        if(tags_) {
            for(int i = 0; i < tags_->Count(); i++) {
                if((*tags_)[i] == tag) {
                    tags_->RemoveAt(i);
                    break;
                }
            }
        }
    }

    // Removes and frees the tag having the specified type.
    // Returns 'true' if such a tag could be found.
    template <class T>
    bool RemoveTag() {
        if(tags_) {
            for(int i = 0; i < tags_->Count(); i++) {
                auto tag = (*tags_)[i];

                if(tag->GetId() == T::Id) {
                    tag->DetachedFromParent(this);
                    tag->Free();
                    tags_->RemoveAt(i);
                    return true;
                }
            }
        }

        return false;
    }

    // Removes and frees all associated tags.
    void RemoveAllTags() {
        if(tags_) {
            for(int i = 0; i < tags_->Count(); i++) {
                auto tag = (*tags_)[i];
                tag->DetachedFromParent(this);
                tag->Free();
            }

            tags_->Clear();
            RemoveListIfAllowed();
        }
    }

    // void SerializeAllTags
    // void DeserializeAllTags
};

} // namespace IR
#endif