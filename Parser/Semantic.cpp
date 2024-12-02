#include "Semantic.hpp"
#include "SemanticHolder.hpp"

namespace Parsing {

Semantic::Semantic(SemanticHolder* holder) :
		holder_(holder) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SemanticHolder* Semantic::Holder() {
	return holder_;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Semantic::SetHolder(SemanticHolder* value) {
	holder_ = value;
}

} // namespace Parsing