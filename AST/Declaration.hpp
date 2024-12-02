// Declaration.hpp
// Copyright (c) Lup Gratian
//
// Defines the base class for the declaration system.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_AST_DECLARATION_HPP
#define PC_AST_DECLARATION_HPP

#include "../Base/String.hpp"
#include "../Base/DebugValidator.hpp"
#include "../Base/Dictionary.hpp"
#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Common/LocationInfo.hpp"
#include "../Lexer/Token.hpp"
#include "DeclarationContext.hpp"
#include "Identifier.hpp"
#include "Type.hpp"
#include "Visitor.hpp"
#include "Attribute.hpp"
using namespace Common;
using namespace Base;
using namespace Lexing;

namespace AST {

// Represents the linkage used by a declaration.
// 'LinkageType::None' is used by all identifiers that are not objects or functions.
enum class LinkageType {
	None,
	Internal,
	External
};


// Represents the storage-class specifiers (C99:6.7.1).
enum class StorageType {
	None, // Default.
	Auto,
	Static,
	Register,
	Extern,
};


// Represents the base class for a declaration or definition.
// Multiple declarations and definitions can be linked, forming a list.
class Declaration : public Visitable {
private:
	Declaration();                              // Should not be created.
	Declaration(const Declaration&);            // Should not be copied.
	Declaration& operator=(const Declaration&); // Should not be assigned.

protected:
	static const char DECL_VARIABLE   = 1;
	static const char DECL_FUNCTION   = 2;
	static const char DECL_TYPEDEF    = 3;
	static const char DECL_ENUM_CONST = 4;
	static const char DECL_ENUM       = 5;
	static const char DECL_FIELD      = 6; // struct/union field.
	static const char DECL_STRUCT     = 7;
	static const char DECL_UNION      = 8;
	static const char DECL_LIST       = 9;

	char               kind_;         // The type of the declaration.
	bool               isDefinition_; // 'true' if this is a definition.
	const Type*        type_;         // The declared/defined type.
	Declaration*       prev_;         // The previous declaration.
	Declaration*       next_;         // The next declaration.
	shared<Identifier> ident_;        // The identifier described by this declaration.
	LinkageType        linkage_;      // The linkage of the identifier.
	StorageType        storage_;      // The storage type of the identifier.
	LocationInfo       startLoc_;     // The location where the declaration was found.
	LocationInfo       endLoc_;       // The location where the declaration ends.
	shared<List<shared<Attribute>>> attrList_;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Constructors to be used only by derived classes.
	Declaration(int kind, shared<Identifier> ident, const Type* type,
				LocationInfo start,	LocationInfo end, bool definition = false, 
				Declaration* previous = nullptr, Declaration* next = nullptr) :
			kind_(kind), ident_(ident), type_(type),
			startLoc_(start), endLoc_(end), isDefinition_(definition), 
			prev_(previous), next_(next), linkage_(LinkageType::None), storage_(StorageType::None) {}

	// Should generate a string that describes the information contained
	// in the declaration object. Children should be included.
	virtual string ToStringImpl(int level) const {
		return "";
	}

	// Should return 'true' if the declarations are identical.
	virtual bool EqualsImpl(const Declaration* other) const;

public:
	virtual ~Declaration() {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the previous declaration of the identifier.
	Declaration* Previous() {
		return prev_;
	}

	const Declaration* Previous() const {
		return prev_;
	}

	void SetPrevious(Declaration* value) {
		prev_ = value;
	}

	// Returns the next declaration of the identifier.
	Declaration* Next() {
		return next_;
	}

	const Declaration* Next() const {
		return next_;
	}

	void SetNext(Declaration* value) {
		next_ = value;
	}

	// Returns 'true' if this is a definition.
	bool IsDefinition() const {
		return isDefinition_;
	}

	void SetIsDefinition(bool value) {
		isDefinition_ = value;
	}

	// Returns the declared/defined type.
	const Type* DeclarationType() const {
		return type_;
	}

	void SetDeclType(const Type* value) {
		type_ = value;
	}

	// Returns the name of the declaration.
	shared<Identifier> Name() {
		return ident_;
	}

	const shared<Identifier> Name() const {
		return ident_;
	}

	void SetName(shared<Identifier> value) {
		ident_ = value;
	}

	// Returns the location where the start of the declaration was found.
	LocationInfo StartLocation() const {
		return startLoc_;
	}

	void SetStartLocation(LocationInfo value) {
		startLoc_ = value;
	}

	// Returns the location where the declaration ends.
	LocationInfo EndLocation() const {
		return endLoc_;
	}

	void SetEndLocation(LocationInfo value) {
		endLoc_ = value;
	}

	// Returns the source file range covered by the declaration.
	RangeInfo Range() const {
		return RangeInfo(startLoc_, endLoc_);
	}

	void SetRange(RangeInfo range) {
		startLoc_ = range.First();
		endLoc_ = range.Last();
	}

	// Returns the storage-class specifier associated with this variable.
	StorageType Storage() const {
		return storage_;
	}

	void SetStorage(StorageType value) {
		storage_ = value;
	}

	// If the type of the object is the specified one, returns the object
	// converted, else it returns nullptr.
	template <class T>
	T* As() {
		return dynamic_cast<T*>(this);
	}

	template <class T>
	const T* As() const {
		return dynamic_cast<T*>(const_cast<Declaration*>(this));
	}

	// Returns 'true' if the object has the specified type.
	template <class T>
	bool Is() const {
		return As<T>();
	}

	// Returns the linkage of the identifier.
	LinkageType Linkage() const {
		return linkage_;
	}

	void SetLinkage(LinkageType value) {
		linkage_ = value;
	}

	// Returns 'true' if the declaration has 'static' or 'extern' linkage.
	bool HasLinkage() const {
		return linkage_ != LinkageType::None;
	}

	// Returns 'true' if the declarations are identical (children included).
	bool Equals(Declaration* other) const {
		return EqualsImpl(other);
	}

	// Returns the definition associated with the declaration.
	// Returns nullptr if a definition doesn't exist.
	Declaration* GetDefinition();
	const Declaration* GetDefinition() const;

	// Returns the number of declaration/definitions for this identifier.
	int DeclCount() const;

	//  Returns 'true' if this is a variable declaration.
	bool IsVariableDecl() const {
		return kind_ == DECL_VARIABLE;
	}

	//  Returns 'true' if this is a function declaration.
	bool IsFunctionDecl() const {
		return kind_ == DECL_FUNCTION;
	}

	//  Returns 'true' if this is a typedef declaration.
	bool IsTypedefDecl() const {
		return kind_ == DECL_TYPEDEF;
	}

	//  Returns 'true' if this is a enum declaration.
	bool IsEnumConstDecl() const {
		return kind_ == DECL_ENUM_CONST;
	}

	//  Returns 'true' if this is a enumeration declaration.
	bool IsEnumDecl() const {
		return kind_ == DECL_ENUM;
	}

	//  Returns 'true' if this is a struct/union field declaration.
	bool IsFieldDecl() const {
		return kind_ == DECL_FIELD;
	}

	//  Returns 'true' if this is a struct declaration.
	bool IsStructDecl() const {
		return kind_ == DECL_STRUCT;
	}

	//  Returns 'true' if this is a union declaration.
	bool IsUnionDecl() const {
		return kind_ == DECL_UNION;
	}

	//  Returns 'true' if this is a declaration list.
	bool IsDeclList() const {
		return kind_ == DECL_LIST;
	}

	// Returns 'true' if there is a definition for this declaration.
	// Walks the list of declarations if needed.
	bool HasDefinition() const {
		return GetDefinition();
	}

	// Returns the first declaration/definition in the list.
	const Declaration* FirstDeclaration() const;
	Declaration* FirstDeclaration();

	// Returns the last declaration/definition in the list.
	const Declaration* LastDeclaration() const;
	Declaration* LastDeclaration();

	// Adds the specified attribute to the attribute list.
	// If an attribute with the same type is found it is replaced.
	void AddAttribute(shared<Attribute> value);

	// Removes the specified attribute from the attribute list.
	void RemoveAttribute(Attribute* value);

	// Returns 'true' if this declaration has associated attributes.
	bool HasAttributes() const {
		return attrList_;
	}

	// Returns the associated attribute, converted to the specified type,
	// or nullptr if the types don't match any attribute in the list.
	template <class T>
	const T* AttributeAs() const {
		if(attrList_) {
			auto& list = *attrList_;

			for(int i = 0; i < list.Count(); i++) {
				if(list[i]->Is<T>()) {
					return list[i]->As<T>();
				}
			}
		}
		
		return nullptr;
	}

	template <class T>
	T* AttributeAs() {
		if(attrList_) {
			auto& list = *attrList_;

			for(int i = 0; i < list.Count(); i++) {
				if(list[i]->Is<T>()) {
					return list[i]->As<T>();
				}
			}
		}
		
		return nullptr;
	}

	// Returns the number of associated attributes.
	int AttributeCount() const {
		if(attrList_) {
			return attrList_->Count();
		}
		else return 0;
	}

	// Returns the attribute found at the specified index.
	Attribute* GetAttribute(int index) {
		DebugValidator::IsNotNull(attrList_.Raw());
		return (*attrList_)[index];
	}

	const Attribute* GetAttribute(int index) const {
		DebugValidator::IsNotNull(attrList_.Raw());
		return (*attrList_)[index];
	}

	// Returns a string representation of the declaration.
	// Used for debugging.
	string ToString(int level) const {
		return ToStringImpl(level);
	}

	// Implements the visitor pattern.
	virtual void Accept(Visitor* v) const = 0;
};

} // namespace AST
#endif