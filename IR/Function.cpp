// Function.cpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "Function.hpp"
#include "Unit.hpp"
#include "SymbolTable.hpp"
#include "Block.hpp"
#include "../Base/StringBuilder.hpp"
#include "../Base/FileStream.hpp"
#include "../Base/StreamReader.hpp"
#include "../Base/Path.hpp"
#include "../Base/Process.hpp"
#include "../Base/ObjectDumper.hpp"
#include "../Analysis/CFGPrinter.hpp"
#include "../Analysis/DominatorTree.hpp"
#include "../Analysis/DominatorsOrder.hpp"
#include "../Analysis/DominatorTreePrinter.hpp"
using namespace Base;
using namespace Analysis;

namespace IR {

InstructionEnumerator::InstructionEnumerator(const Block* block) : block_(block) {
	if(block) {
		valid_ = block->FirstInstruction() != nullptr;
		instr_ = block->FirstInstruction();
	}
	else valid_ = false;
}

InstructionEnumerator::InstructionEnumerator(const InstructionEnumerator& other) :
	block_(other.block_), instr_(other.instr_), valid_(other.valid_) {}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Instruction* InstructionEnumerator::Next() const {
	if(valid_ == false) return nullptr;

	const Instruction* temp = instr_;
	instr_ = instr_->NextInstruction();
	valid_ = instr_ != nullptr;		

	// We're not done if there are more blocks.
	if(valid_ == false) {
		valid_ = block_->Next();

		if(valid_) {
			block_ = block_->NextBlock();
			instr_ = block_->FirstInstruction();
            valid_ = block_->FirstInstruction() != nullptr;
		}
	}

	return temp;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
InstructionEnumerator& InstructionEnumerator::operator= (const InstructionEnumerator& other) {
	if(&other == this) return *this;

	block_ = other.block_;
	instr_ = other.instr_;
	valid_ = other.valid_;
	return *this;
}

// ######################################################################################
// Function
// ######################################################################################
Function::Function(const FunctionType* type, shared<string> name, bool isDef, 
				   Unit* parent, SymbolVisibility visibility, 
				   CallConventionType callConv, bool isIntr) :
		Symbol(Kind::Function, type, name, parent ? &parent->Symbols() : nullptr, visibility),
		isDefinition_(isDef), parent_(parent), callConvention_((unsigned char)callConv),
		inline_(false), IntrusiveList(), isIntrinsic_(isIntr), isNoIndirWrite_(false),
		isNoIndirRead_(false), isNoState_(false), isStdlib_(false), 
		nextSymbolId_(0), symbolTable_(this) {
	if(parent) {
		parent->AddFunction(this);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function* Function::GetFunction(const FunctionType* type, const string& name, 
								bool isDef, Unit* parent, SymbolVisibility visibility, 
								CallConventionType callConv) {
	return new Function(type, new string(name), isDef,
						parent, visibility, callConv);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::AddCallInstruction(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);
    idToCall_.Add(instr->Id(), instr);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveCallInstruction(int id) {
    DebugValidator::IsTrue(idToCall_.ContainsKey(id));
    idToCall_.Remove(id);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveCallInstruction(CallInstr* instr) {
    DebugValidator::IsNotNull(instr);
    DebugValidator::IsTrue(idToCall_.ContainsKey(instr->Id()));
    idToCall_.Remove(instr->Id());
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::NotifyBlockAdded(Block* block) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->BlockAdded(block, this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::NotifyBlockRemoved(Block* block) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->BlockRemoved(block, this);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::NotifyParameterAdded(Variable* parameter) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->ParameterAdded(parameter, this);
    }

    for(auto block = FirstBlock(); block; block = block->NextBlock()) {
        block->NotifyParameterAdded(parameter);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::NotifyParameterRemoved(Variable* parameter) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->ParameterRemoved(parameter, this);
    }

    for(auto block = FirstBlock(); block; block = block->NextBlock()) {
        block->NotifyParameterRemoved(parameter);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::NotifyVariableAdded(Variable* variable) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->VariableAdded(variable, this);
    }

    for(auto block = FirstBlock(); block; block = block->NextBlock()) {
        block->NotifyVariableAdded(variable);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::NotifyVariableRemoved(Variable* variable) {
    for(int i = 0; i < TagCount(); i++) {
        GetTag(i)->VariableAdded(variable, this);
    }

    for(auto block = FirstBlock(); block; block = block->NextBlock()) {
        block->NotifyVariableRemoved(variable);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Function::ContainsBlock(Block* block) const {
    for(auto currentBlock = FirstBlock(); currentBlock; 
        currentBlock= currentBlock->NextBlock()) {
        if(currentBlock == block) {
            return true;
        }
    }

    return false;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::InsertFirstBlock(Block* value) {
	DebugValidator::IsNotNull(value);
	DebugValidator::IsTrue(value->HasName());
    DebugValidator::IsFalse(ContainsBlock(value));
	
    value->SetId(GetNextBlockId());
	symbolTable_.Add(value);

	value->SetParentFunction(this);
	value->SetParentTable(&symbolTable_);
	IntrusiveList::InsertFirst(value);
    NotifyBlockAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::InsertLastBlock(Block* value) {
	DebugValidator::IsNotNull(value);
	DebugValidator::IsTrue(value->HasName());
    DebugValidator::IsFalse(ContainsBlock(value));
	
    value->SetId(GetNextBlockId());
	symbolTable_.Add(value);

	value->SetParentFunction(this);
	value->SetParentTable(&symbolTable_);
	IntrusiveList::InsertLast(value);
    NotifyBlockAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::InsertBlockBefore(Block* value, Block* other) {
	DebugValidator::IsNotNull(value);
	DebugValidator::IsNotNull(other);
	DebugValidator::IsTrue(value->HasName());
    DebugValidator::IsFalse(ContainsBlock(value));
	
    value->SetId(GetNextBlockId());
	symbolTable_.Add(value);

	value->SetParentFunction(this);
	value->SetParentTable(&symbolTable_);
	IntrusiveList::InsertBefore(value, other);
    NotifyBlockAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::InsertBlockAfter(Block* value, Block* other) {
	DebugValidator::IsNotNull(value);
	DebugValidator::IsNotNull(other);
	DebugValidator::IsTrue(value->HasName());
    DebugValidator::IsFalse(ContainsBlock(value));

    value->SetId(GetNextBlockId());
	symbolTable_.Add(value);

	value->SetParentFunction(this);
	value->SetParentTable(&symbolTable_);
	IntrusiveList::InsertAfter(value, other);
    NotifyBlockAdded(value);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveBlock(Block* value, bool free, bool fromDestructor) {
	DebugValidator::IsNotNull(value);
	DebugValidator::IsTrue(value->HasName());
    DebugValidator::IsTrue(ContainsBlock(value));

    // Remove the block from the symbol table and block list.
	symbolTable_.Remove(value);
	IntrusiveList::Remove(value);
    NotifyBlockRemoved(value);

	if(free) {
        // Free the block and all its instructions.
		value->ClearInstructions(free, fromDestructor);
		value->Free();
	}
    else { 
        // The block is no more in the function.
        value->SetParentFunction(nullptr);
    }
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::ClearBlocks(bool free, bool fromDestructor) {
	Block* block = FirstBlock();

	while(block) {
		Block* next = block->NextBlock();
		RemoveBlock(block, free, fromDestructor);
		block = next;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::AddVariable(Variable* variable) {
    DebugValidator::IsNotNull(variable);
    DebugValidator::IsFalse(variable->IsParameter());

    variable->SetId(GetNextVariableId());
    variables_.Add(variable);

    if(variable->ParentTable() == nullptr) {
        symbolTable_.Add(variable);
        variable->SetParentTable(&symbolTable_);
    }

    NotifyVariableAdded(variable);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Function::HasVariable(const string* name) const {
	DebugValidator::IsNotNull(name);
	const Symbol* symbol = symbolTable_.Get(name);
	return symbol && symbol->IsLocalVariable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* Function::GetVariable(const string* name) {
	DebugValidator::IsNotNull(name);
	Symbol* symbol = symbolTable_.Get(name);

	if(symbol && symbol->IsLocalVariable()) {
		return static_cast<Variable*>(symbol);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Variable* Function::GetVariable(const string* name) const {
	DebugValidator::IsNotNull(name);
	const Symbol* symbol = symbolTable_.Get(name);

	if(symbol && symbol->IsLocalVariable()) {
		return static_cast<const Variable*>(symbol);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* Function::GetVariableWithId(int id) {
	for(int i = 0; i < variables_.Count(); i++) {
		if(variables_[i]->Id() == id) {
			return variables_[i];
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Variable* Function::GetVariableWithId(int id) const {
	for(int i = 0; i < variables_.Count(); i++) {
		if(variables_[i]->Id() == id) {
			return variables_[i];
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveVariable(Variable* variable, bool free) {
	DebugValidator::IsNotNull(variable);
	DebugValidator::IsTrue(variables_.Contains(variable));

	variables_.Remove(variable);
	symbolTable_.Remove(variable);
    NotifyVariableRemoved(variable);
	if(free) variable->Free();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveVariable(const string* name, bool free) {
	DebugValidator::IsNotNull(name);
	DebugValidator::IsTrue(symbolTable_.Contains(name) && 
						   symbolTable_.Get(name)->IsLocalVariable());
	RemoveVariable(GetVariable(name));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveVariable(int id, bool free) {
    for(int i = 0; i < variables_.Count(); i++) {
        if(variables_[i]->Id() == id) {
            RemoveVariable(variables_[i], free);
            return;
        }
    }

    DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Block* Function::GetBlock(int id) {
	for(auto block = FirstBlock(); block; block = block->NextBlock()) {
		if(block->Id() == id) return block;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Block* Function::GetBlock(int id) const {
	for(auto block = FirstBlock(); block; block = block->NextBlock()) {
		if(block->Id() == id) return block;
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Function::InstructionCount() const {
    int count = 0;

    for(auto block = FirstBlock(); block; block = block->NextBlock()) {
        count += block->InstructionCount();
    }

    return count;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::AddParameter(Variable* parameter) {
    DebugValidator::IsNotNull(parameter);

    parameter->SetId(GetNextVariableId());
    parameter->SetIsParameter(true);
    params_.Add(parameter);

    if(parameter->ParentTable() == nullptr) {
        symbolTable_.Add(parameter);
        parameter->SetParentTable(&symbolTable_);
    }

    NotifyParameterAdded(parameter);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
bool Function::HasParameter(const string* name) const {
	DebugValidator::IsNotNull(name);
	const Symbol* symbol = symbolTable_.Get(name);
	return symbol && symbol->IsLocalVariable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* Function::GetParameterVariable(const string* name) {
	DebugValidator::IsNotNull(name);
	Symbol* symbol = symbolTable_.Get(name);

	if(symbol && symbol->IsLocalVariable()) {
		return static_cast<Variable*>(symbol);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Variable* Function::GetParameterVariable(const string* name) const {
	DebugValidator::IsNotNull(name);
	const Symbol* symbol = symbolTable_.Get(name);

	if(symbol && symbol->IsLocalVariable()) {
		return static_cast<const Variable*>(symbol);
	}
	else return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Variable* Function::GetParameterVariableWithId(int id) {
	for(int i = 0; i < params_.Count(); i++) {
		if(params_[i]->Id() == id) {
			return params_[i];
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
const Variable* Function::GetParameterVariableWithId(int id) const {
	for(int i = 0; i < params_.Count(); i++) {
		if(params_[i]->Id() == id) {
			return params_[i];
		}
	}

	return nullptr;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int Function::GetParameterIndex(Variable* parameter) {
    for(int i = 0; i < params_.Count(); i++) {
        if(params_[i] == parameter) {
            return i;
        }
    }

    DebugValidator::Unreachable();
    return -1;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Parameter* Function::GetParameter(Variable* parameter) {
	DebugValidator::IsNotNull(parameter);
	DebugValidator::IsTrue(parameter->IsParameter());
	return paramTable_.GetParameter(parameter);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Parameter* Function::GetParameter(int index) {
	return GetParameter(params_[index]);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveParameter(Variable* variable, bool free) {
	DebugValidator::IsNotNull(variable);
	DebugValidator::IsTrue(params_.Contains(variable));

	params_.Remove(variable);
	symbolTable_.Remove(variable);
    NotifyParameterRemoved(variable);
	if(free) variable->Free();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveParameter(const string* name, bool free) {
	DebugValidator::IsNotNull(name);
	DebugValidator::IsTrue(symbolTable_.Contains(name) && 
						   symbolTable_.Get(name)->IsLocalVariable());
	RemoveParameter(GetParameterVariable(name));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::RemoveParameterWithId(int id, bool free) {
    for(int i = 0; i < params_.Count(); i++) {
        if(params_[i]->Id() == id) {
            RemoveParameter(params_[i], free);
            return;
        }
    }

    DebugValidator::Unreachable();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FunctionReference* Function::GetReference() {
    auto type = ParentUnit()->Types().GetPointer(GetType());
    return  ParentUnit()->References().GetFunctionRef(this, type);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Function::ToStringImpl(int level) const {
	string text = "FUNCTION " + (HasName() ? *Name() : "") + "\n";
	text += "Type:\n" + type_->ToString(level + 1) + "\n";
	text += DumpVariables(level);
	text += "\n";
	text += DumpBlocks(level);
	return text;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Function::DumpBlocks(int level) const {
	StringBuilder sb(string('\t', level));
	sb.AppendFormat(L"Blocks: %d\n", Count());
	
	for(auto p = FirstBlock(); p; p = p->NextBlock()) {
		sb.AppendLine(p->ToString(level + 1));
	}

	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
string Function::DumpVariables(bool showParams, int level) const {
	StringBuilder sb(string('\t', level));

	if(showParams) {
		sb.Append("Parameters: ");
		if(HasName()) sb.Append(*Name());
		sb.AppendLine();
	
		for(int i = 0; i < params_.Count(); i++) {
			sb.AppendLine(params_[i]->ToString(level + 1));
		}
	}

	symbolTable_.Dump(false /* showParams */, level);
	return sb.ToString();
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::ViewCFG(bool markUnreachable, bool immDoms, 
                       Block* frontierBlock) const {
	// Write the CFG to a temporary file.
	string tempFile = Path::GetTempFile();
	CFGPrinter printer(tempFile);
	printer.Print(this, markUnreachable, immDoms, frontierBlock);

	// Launch 'xdot'.
	auto stream = new FileStream("xdot_info", Abstraction::FileMode::Open);
	StreamReader reader(stream);
	string path = reader.ReadToEnd();
	Process::Start(path, "xdot.py " + tempFile);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
void Function::ViewDominatorTree() const {
	// Build the dominator tree.
	typedef Dominators<Block, Function>::TAlgorithm TAlgorithm;
	typedef DominatorTree<Block, Function, TAlgorithm> TDominatorTree;
	TDominatorTree domTree(this);
	domTree.Build();

	// Create a temporary file and write the tree to it.
	string tempFile = Path::GetTempFile();
	DominatorTreePrinter<TDominatorTree> printer(tempFile);
	printer.Print(&domTree);

	// Launch 'xdot'.
	auto stream = new FileStream("xdot_info", Abstraction::FileMode::Open);
	StreamReader reader(stream);
	string path = reader.ReadToEnd();
	Process::Start(path, "xdot.py " + tempFile);
}

} // namespace IR