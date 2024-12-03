// ParserHelpers.hpp
// Copyright (c) Lup Gratian
//
// Helper classes for parsing.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_PARSING_HELPERS_HPP
#define PC_PARSING_HELPERS_HPP

#include "../Base/List.hpp"
#include "../Base/SharedPointer.hpp"
#include "../Common/LocationInfo.hpp"
#include "../AST/DeclarationContext.hpp"
#include "../AST/Declaration.hpp"
#include "../AST/Declarations.hpp"
#include "../AST/Expression.hpp"
#include "../AST/Expressions.hpp"
#include "../AST/Type.hpp"
#include "../AST/Types.hpp"
#include "../AST/Attributes.hpp"
using namespace AST;
using namespace Base;
using namespace Common;

namespace Parsing {

// Contains information about the storage-class, type and qualifiers.
// Has methods to validate certain combinations of the specifiers.
// If it's a struct/union/enum/typedef a pointer to the corresponding type is set.
struct SpecifierInfo {
	// Type qualifiers.
	Qualifier Qual;

	// Type specifiers.
	int  LongCount;
	bool Void;
	bool Char;
	bool WChar;
	bool Float;
	bool Double;
	bool Bool;
	bool Unsigned;
	bool Signed;
	bool Int;
	bool Short;

	// Storage-class specifiers.
	bool Auto;      
	bool Extern;
	bool Static;
	bool Register;
	bool Typedef;
	
	// Function specifiers.
	bool Inline;

	// Other data.
	shared<StructDeclaration> Struct;   // If it has 'struct' type.
	shared<UnionDeclaration> Union;     // If it has 'union' type.
	shared<EnumDeclaration> Enum;       // If it has 'enum' type.
	const TypedefType* TypedefT; // If the type originates from a 'typedef'.
	List<shared<Attribute>> Attributes; // Any associated attributes.
	LocationInfo Start;
	LocationInfo End;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	SpecifierInfo() : 
			LongCount(0), Void(false), Char(false), WChar(false), Float(false),
			Double(false), Bool(false), Unsigned(false), Signed(false), Int(false),
			Short(false), Auto(false), Extern(false), Static(false), Register(false),
			Inline(false), Typedef(false), TypedefT(nullptr) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// A storage-class specifier should appear only once.
	bool HasStorage() const {
		return Auto || Extern || Static || Register || Typedef;
	}
	
	// Methods that verify if specific combinations of type qualifiers are valid.
	bool VoidAllowed() const {
		return NoType();
	}

	bool CharAllowed() const {
		return !Void && !Char && !WChar && !Float && !Double && !Bool &&
			   !Int && !Short && !LongCount &&
			   !IsUserType();
	}

	bool WCharAllowed() const {
		return !Void && !Char && !WChar && !Float && !Double && !Bool &&
			   !Int && !Short && !LongCount &&
			   !IsUserType();
	}

	bool FloatAllowed() const {
		return NoType();
	}

	bool DoubleAllowed() const {
		// 'long double is' valid.
		return !Void && !Char && !WChar && !Float && !Double && !Bool &&
			   !Unsigned && !Signed && !Int && !Short && !IsUserType();
	}

	bool BoolAllowed() const {
		return !Void && !Char && !WChar && !Float && !Double && !Bool && 
			   !Unsigned && !Signed && !Int && !Short && !LongCount &&
			   !IsUserType();
	}

	bool UnsignedAllowed() const {
		return !Void && !WChar && !Float && !Double && !Bool && 
			   !Unsigned && !Signed && !IsUserType();
	}

	bool SignedAllowed() const {
		return !Void && !WChar && !Float && !Double && !Bool &&
			   !Unsigned && !Signed && !IsUserType();
	}

	bool IntAllowed() const {
		return !Void && !Char && !WChar && !Float && !Double && 
			   !Bool && !IsUserType();
	}

	bool ShortAllowed() const {
		return !Void && !Char && !WChar && !Float && !Double &&
			   !Bool && !Short && !LongCount && !IsUserType();
	}

	bool LongAllowed() const {
		return !Void && !Char && !WChar && !Float && !Bool && !Short && !IsUserType();
	}

	bool EnumAllowed() const {
		return IsBasicType() == false && (IsUserType() == false);
	}

	bool StructAllowed() const {
		return IsBasicType() == false && (IsUserType() == false);
	}

	bool TypedefAllowed() const {
		return IsBasicType() == false && (IsUserType() == false);
	}

	// Returns 'true' if a basic type has been declared.
	bool IsBasicType() const {
		return Char || WChar || Int || Short || LongCount || 
			   Float || Double || Bool || Void || Unsigned || Signed;
	}

	// Returns 'true' if the type is one of struct/union/enum/typedef.
	bool IsUserType() const {
		return Struct || Union || Enum || TypedefT;
	}

	// Returns 'true' if no type has been specified yet.
	bool NoType() const {
		return (IsBasicType() == false) && (IsUserType() == false);
	}
};


// Forward declarations.
class InitInfo; 
class Designator;

// Used as a base for designators and normal initializers.
class InitBase {
public:
	virtual ~InitBase() {} // Needed to suppress 'error C6823' under VC.
};


// Contains information about a C99 designator. It can handle both types
// of designators (for fields and arrays).
class Designator : public InitBase {
public:
	shared<Identifier> Name;      // The member name, if this is a field designator.
	shared<Expression> ConstExpr; // The index, if this is an array designator.
	shared<Designator> Child;     // The child designator, if any ([0][1], [1] is a child).
	shared<InitInfo> Value;       // The value to assign ([0] = Value, .a = Value).
	bool IsField;                 // 'true' if it's of the form .member_name.
	bool Disabled;                // 'true' if the designator was handled and shouldn't 
								  // be considered again.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	Designator() : IsField(false), Disabled(false) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns the value to be assigned (handles designators with children too).
	InitInfo* GetValue() {
		Designator* temp = this;
		while((temp) && (temp->Value == nullptr)) {
			temp = temp->Child;
		}

		if(temp) return temp->Value;
		else return nullptr;
	}
};


// Contains information about a standard initializer. It can be a simple expression
// or an expression surrounded by braces {E}.
class InitInfo : public InitBase {
public:
	enum InitType {
		ConstExpression,
		InitList
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	InitType Type;                   // The type (with or without braces).
	List<shared<InitBase>> Children; // The list of children (only when with braces).
	shared<Expression> Value;        // IsValid if 'ConstExpression' is set.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	InitInfo() : Type(ConstExpression) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if the initializer has children.
	// The initializer '{1, [0][1], "abc"}' has 3 children, for example.
	bool HasChildren() const {
		return Children.Count() > 0;
	}
};


// Contains information about a declarator (simple, pointer, array or function).
struct DeclaratorInfo {
	enum DeclaratorType {
		Normal,
		Pointer,
		Array,
		Function
	};

	// Information about a function parameter.
	struct ParameterInfo {
		SpecifierInfo Info;                // The specifiers (type) of the parameter.
		shared<DeclaratorInfo> Declarator; // The declarator, it's optional in prototypes.
		LocationInfo Location;             // Where the parameters starts.

		// Returns true if a declarator is present and it has a name.
		bool HasName() const {
			return Declarator && (Declarator->GetName());
		}

		// Returns the name of the declarator.
		shared<Identifier> GetName() {
			DebugValidator::IsTrue(HasName());
			return Declarator->GetName();
		}
	};

	// Contains information about an array dimension.
	struct ArrayInfo {
		bool               Incomplete; // 'true' if the dimension is [].
		bool               HasStatic;  // 'true' if the dimension is [static NUMB].
		bool               HasStar;    // 'true' if the dimension is [*] (VLA).
        bool               FunctProto;
		SpecifierInfo      Info;       // Contains the optional specifiers, [const 5].
		shared<Expression> Value;      // The expression that represents the length.

		ArrayInfo() : 
                Incomplete(false), HasStatic(false),
                HasStar(false), FunctProto(false) {}
	};

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    shared<DeclaratorInfo> Next;        // The next declarator, if any.
    DeclaratorInfo* Previous;           // Normal pointer to avoid circular references.
	shared<Identifier> Name;            // The name of the declarator, if any.
    shared<InitInfo> Initializer;       // The associated initializer, if any.
    shared<DeclarationContext> Context; // The context in which the declarator appeared.
    List<shared<ParameterInfo>> Parameters; // The parameters of the function.
    DeclaratorType Type;                // The type of the declarator.
	SpecifierInfo Info;                 // Optional specifiers associated with the declarator.
	ArrayInfo ArrayDim;                 // Information about the array dimension.
	LocationInfo Location;              // The location of the declarator start.
    bool IsVarargs;                     // 'true' if the function ends with ...

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	DeclaratorInfo(DeclaratorInfo* previous) : 
			Type(Normal), Previous(previous), IsVarargs(false) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Returns 'true' if this object contains information about a pointer.
	bool IsPointer() const {
		return Type == Pointer;
	}

	// Returns 'true' if this object contains information about an array.
	bool IsArray() const {
		return Type == Array;
	}

	// Returns 'true' if this object contains information about a function.
	bool IsFunction() const {
		return Type == Function;
	}

	// Returns 'true' if this object provides the name for the declarator.
	bool IsNormal() {
		return Type == Normal;
	}

	// Returns 'true' if an identifier was found.	
	shared<Identifier> GetName() {
		DeclaratorInfo* di = this;

		while(di && (di->Name == nullptr)) {
			di = di->Next;
		}

		if(di) return di->Name;
		else return nullptr;
	}
};


// Contains information about the current level of the designator.
// Such a context is created for each brace pair.
struct InitContext {
	const Type* ObjectType;       // The type of the object to initialize.
	shared<InitInfo> Initializer; // The initializer at the current level.
	int Level;                    // 0 for the top-level initializer.
	int Index;                    // The current position in the initializer list.
	int NewIndex;                 // The new index, if a change was made.
	LocationInfo Location;        // The location of the initializer start.
	bool HasChanged;              // 'true' if a designator made a change to the index.

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	InitContext() : 
            HasChanged(false), Level(0), Index(0), NewIndex(0), ObjectType(nullptr) {}

	InitContext(int level, const Type* type, shared<InitInfo> initializer, 
				LocationInfo location, int index = 0) :
			ObjectType(type), Initializer(initializer), Index(index), 
			Location(location), HasChanged(false), Level(level), NewIndex(0) {}
	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	// Reset the flags used by designators to mark that they changed the index.
	void ResetChanged() {
		HasChanged = false;
		NewIndex = -1;
	}

	// The current initializer (at position 'Index').
	shared<InitInfo> Current() {
		return Initializer->Children[Index];
	}
};


// Describes an initialized array element. Used to mark the positions
// that have been explicitly initialized.
struct ArrayElement {
	__int64 Index;
	shared<Expression> Value;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	ArrayElement() : Index(0) {}

	ArrayElement(__int64 index, shared<Expression> value) :
			Index(index), Value(value) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const ArrayElement& other) const {
		return Index == other.Index;
	}

	bool operator!= (const ArrayElement& other) const {
		return Index != other.Index;
	}

	bool operator< (const ArrayElement& other) const {
		return Index < other.Index;
	}

	bool operator> (const ArrayElement& other) const {
		return Index < other.Index;
	}
};


// Describes the initialized field of a struct/union. Used to mark the
// initialized members and to provide default values for those that aren't marked.
struct StructElement {
	shared<Identifier> Name;
	shared<Expression> Value;

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	StructElement() {}

	StructElement(shared<Identifier> name, shared<Expression> value) :
			Name(name), Value(value) {}

	unsigned GetHashCode() const {
		return Name->GetHashCode();
	}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	bool operator== (const StructElement& other) {
		return Name == other.Name;
	}

	bool operator!= (const StructElement& other) {
		return Name != other.Name;
	}

	bool operator< (const StructElement& other) {
		return Name < other.Name;
	}
};


// Helper that verifies if two identifier are close (their edit distance is small).
// Can be used to suggest identifiers in case of an error.
class EditDistance {
private:
	// Very simple matrix implementation.
	struct Matrix {
		local<int, true> matrix_;
		int n_;
		int m_;

		Matrix(int n, int m) : matrix_(new int[n * m]), n_(n), m_(m) {}

		int& operator() (int i, int j) {
			return matrix_[(i * m_) + j];
		}
	};

	// Helpers that compute the minimum value of 3 and 2 numbers.
	static int Min(int a, int b, int c) {
		int min = a;
		if(b < min) min = b;
		if(c < min) return c;
		return min;
	}

	static int Min(int a, int b) {
		return a < b ? a : b;
	}

	// Implements the edit distance algorithm.
	// Returns 'true' if the edit distance is at most 'maxDiff'.
	static bool AreCloseImpl(const string& a, const string& b, int maxDiff, int* dist) {
		// A single character is treated fast.
		if((a.Length() == 1) && (b.Length() == 1)) {
			if(dist) *dist = a[0] == b[0] ? 0 : 1;
			return a[0] == b[0];
		}

		// Uses the Damerau-Levenshtein distance algorithm. In addition to the classic
		// dynamic programming algorithm, this also takes in account transpositions.
		// http://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance
		Matrix matrix(a.Length() + 1, b.Length() + 1);

		// Initialize the matrix.
		for (int i = 0; i <= a.Length(); i++) matrix(i, 0) = i;
		for (int j = 0; j <= b.Length(); j++) matrix(0, j) = j;

		for (int i = 1; i <= a.Length(); i++) {
			for (int j = 1; j <= b.Length(); j++) {
				int cost = (b[j - 1] == a[i - 1]) ? 0 : 1;

				matrix(i, j) = Min(matrix(i - 1, j) + 1,
								   matrix(i, j - 1) + 1,
								   matrix(i - 1, j - 1) + cost);

				if((i > 1) && (j > 1) &&
				   (a[i - 1] == b[j - 2]) &&
				   (a[i - 2] == b[j - 1])) {
					matrix(i, j) = Min(matrix(i, j), matrix(i - 2, j - 2) + cost);
				}
			}
		}

		if(*dist) *dist = matrix(a.Length(), b.Length());
		return matrix(a.Length(), b.Length()) <= maxDiff;
	}

public:
	// Returns 'true' if the specified strings are close.
	// Uses a default maximum allowed difference of 2 characters.
	static bool AreClose(const string& a, const string& b, 
						 int maxDiff = 2,int* dist = nullptr) {
		return AreCloseImpl(a, b, maxDiff, dist);
	}

	// Returns 'true' if the specified identifiers are close.
	// Uses a default maximum allowed difference of 2 characters.
	static bool AreClose(Identifier* a, Identifier* b, 
						 int maxDiff = 2, int* dist = nullptr) {
		return AreClose(a->Name(), b->Name(), maxDiff, dist);
	}
};

} // namespace Parsing
#endif