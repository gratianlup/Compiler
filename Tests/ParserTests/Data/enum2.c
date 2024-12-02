enum Flags {
	Flag_A,
	Flag_B,
	Flag_C
};

enum Flags a, c;
int b;

void g(int a) {
}

void f() { a = Flag_A; }


void h(enum Flags p, enum Flags* q) {
	*q = p;
	*q = Flag_B;
	*q = b;
	*q = (enum Flags)b;
}

int main() {
	a = Flag_A;
	b = Flag_C;
	h(a, &c);
	return 0;
}