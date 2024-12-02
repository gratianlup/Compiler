// Console.hpp
// Copyright (c) Lup Gratian
//
//
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#ifndef PC_ABSTRACTION_CONSOLE_HPP
#define PC_ABSTRACTION_CONSOLE_HPP

#include <Windows.h>

namespace Abstraction {

namespace ConsoleColor {
	static const int Black	     = 0;
	static const int DarkRed  	 = FOREGROUND_RED;
	static const int DarkGreen	 = FOREGROUND_GREEN;
	static const int DarkBlue	 = FOREGROUND_BLUE;
	static const int DarkCyan	 = FOREGROUND_BLUE  | FOREGROUND_GREEN;
	static const int DarkMagenta = FOREGROUND_BLUE  | FOREGROUND_RED;
	static const int DarkYellow	 = FOREGROUND_GREEN | FOREGROUND_RED;
	static const int Gray	     = FOREGROUND_RED   | FOREGROUND_GREEN | FOREGROUND_BLUE;
	static const int Blue	     = DarkBlue         | FOREGROUND_INTENSITY;
	static const int Green	     = DarkGreen        | FOREGROUND_INTENSITY;
	static const int Cyan	     = DarkCyan         | FOREGROUND_INTENSITY;
	static const int Red	     = DarkRed          | FOREGROUND_INTENSITY; 
	static const int Magenta	 = DarkMagenta      | FOREGROUND_INTENSITY; 
	static const int Yellow	     = DarkYellow       | FOREGROUND_INTENSITY;
	static const int White		 = Gray             | FOREGROUND_INTENSITY;
}

class Console {
public:
	static void SetTextColor(int color) {
		HANDLE console = GetStdHandle(STD_OUTPUT_HANDLE);
		SetConsoleTextAttribute(console, color);
	}
};

} // namespace Abstraction
#endif