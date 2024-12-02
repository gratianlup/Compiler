// ObjectDumper.hpp
// Copyright (c) Lup Gratian
//
// A small helper class that displays on the console debug information.
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_IR_OBJECT_DUMPER_HPP
#define PC_IR_OBJECT_DUMPER_HPP

#include "../Base/String.hpp"
#include "../Abstraction/Platform.hpp"
#include <iostream>
using namespace Abstraction;

namespace Base {

class ObjectDumper {
private:
	string header_;
	string text_;

public:
	ObjectDumper(const string& text, const string& header) :
			header_(header), text_(text) {}

	ObjectDumper(const string& text) : text_(text) {}

	// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	void Dump() const {
		if(header_.Length() > 0) {
			Console::SetTextColor(ConsoleColor::Yellow);
			std::wcout<<header_.Chars()<<"\n";
		}

		Console::SetTextColor(ConsoleColor::Gray);
		std::wcout<<text_.Chars()<<"\n";
		std::wcout.flush();
	}
};

} // namespace Base
#endif