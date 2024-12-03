// Declaration.hpp
// Copyright (c) Lup Gratian
//
// Implements the Declaration class.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Declaration.hpp"

namespace AST {

bool Declaration::EqualsImpl(const Declaration* other) const {
	if(!((ident_ == nullptr) && (other->ident_ == nullptr))) {
		return false;
	}

	return (*ident_ == *other->ident_) && 
		   (type_->Equals(other->type_));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Declaration* Declaration::FirstDeclaration() const {
	const Declaration* declaration = this;
	while(declaration->prev_) {
		declaration = declaration->prev_;
	}

	return declaration;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declaration* Declaration::FirstDeclaration() {
	Declaration* declaration = this;
	while(declaration->prev_) {
		declaration = declaration->prev_;
	}

	return declaration;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Declaration* Declaration::LastDeclaration() const {
	const Declaration* declaration = this;
	while(declaration->next_) {
		declaration = declaration->next_;
	}

	return declaration;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declaration* Declaration::LastDeclaration() {
	Declaration* declaration = this;
	while(declaration->next_) {
		declaration = declaration->next_;
	}

	return declaration;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Declaration* Declaration::GetDefinition() {
	if(isDefinition_) return this;

	Declaration* declaration = FirstDeclaration();
	while((declaration->isDefinition_ == false) && declaration->next_) {
		declaration = declaration->next_;
	}

	return declaration->isDefinition_ ? declaration : nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Declaration* Declaration::GetDefinition() const {
	if(isDefinition_) return this;

	const Declaration* declaration = FirstDeclaration();
	while((declaration->isDefinition_ == false) && declaration->next_) {
		declaration = declaration->next_;
	}

	return declaration->isDefinition_ ? declaration : nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Declaration::DeclCount() const {
	int ct = 0;
	const Declaration *declaration = FirstDeclaration();

	while(declaration) {
		ct++;
		declaration = declaration->next_;
	}

	return ct;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Declaration::AddAttribute(shared<Attribute> value) {
	if(attrList_ == nullptr) {
		attrList_ = new List<shared<Attribute>>();
	}

	// Make sure a single attribute exists for each type.
	for(int i = 0; i < attrList_->Count(); i++) {
		if(typeid((*(*attrList_)[i].Raw())) == typeid(*value.Raw())) {
			attrList_->RemoveAt(i);
			break;
		}
	}

	attrList_->Add(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Declaration::RemoveAttribute(Attribute* value) {
	DebugValidator::IsNotNull(attrList_.Raw());
	DebugValidator::IsNotNull(value);
	auto& list = *attrList_;

	for(int i = 0; i < list.Count(); i++) {
		if(list[i] == value) {
			list.RemoveAt(i);

			// Remove the list if no attributes are in it.
			if(list.Count() == 0) {
				attrList_ = nullptr;
			}

			break;
		}
	}
}

} // namespace AST