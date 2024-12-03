// Semantic.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class of the semantic analysis modules.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_SEMANTIC_HPP
#define PC_PARSING_SEMANTIC_HPP

namespace Parsing {

// Forward declaration.
class SemanticHolder;

class Semantic {
protected:
	SemanticHolder* holder_;

public:
	Semantic() : holder_(nullptr) {}

	Semantic(SemanticHolder* holder);

	virtual ~Semantic() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	virtual SemanticHolder* Holder();
	virtual void SetHolder(SemanticHolder* value);
};

} // namespace Parsing
#endif