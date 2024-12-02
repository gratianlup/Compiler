// Summary.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for all summary classes.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ANALYSIS_SUMMARY_HPP
#define PC_ANALYSIS_SUMMARY_HPP

#include "../IR/Function.hpp"
#include "../IR/References.hpp"
#include "../Base/Log.hpp"
#include "../Base/StaticList.hpp"
#include "../Base/List.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/ObjectDumper.hpp"
using namespace IR;
using namespace Base;

namespace Analysis {

// Forward declarations.
class Summary;


namespace Detail {
    // Used for implementing 'As<T>'/'Is<T>'.
    template <class T>
    struct SummaryPromoter {
        static bool Is(const Summary* op) {
            static_assert(false, "Type is not a Summary in Is<T>");
            return false;
        }

        static T* As(Summary* op) {
            static_assert(false, "Type is not a Summary in As<T>");
            return nullptr;
        }
    };
} // namespace Detail


class Summary {
private:
    Summary(const Summary&);			 // Should not be copied.
    Summary& operator= (const Summary&); // Should not be assigned.

protected:
    // Should generate a string that describes the summary object.
    virtual string ToStringImpl(int level) const {
        return "";
    }

public:
    Summary() {}

    virtual ~Summary() {}

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    virtual int GetId() const = 0;

    // Frees the memory associated with the summary object.
    virtual void Free() {
        delete this;
    }

    // If the type of the object is the specified one, returns the object
    // converted, else it returns nullptr.
    template <class T>
    T* As() {
        return Detail::SummaryPromoter<T>::As(this);
    }

    template <class T>
    const T* As() const {
        return Detail::SummaryPromoter<T>::As(const_cast<Tag*>(this));
    }

    // Returns 'true' if the object has the specified type.
    template <class T>
    bool Is() const {
        return Detail::SummaryPromoter<T>::Is(this);
    }

    // Returns a string representation of the tag information.
    string ToString(int level = 0) const {
        return ToStringImpl(level);
    }

	void Dump() {
		ObjectDumper(ToString(), "Summary").Dump();
	}
};

} // namespace Analysis
#endif