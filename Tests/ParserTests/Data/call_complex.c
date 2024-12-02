int foo(int a, int* b, int c[2], int d, const char* e) {
	return 0;
}

struct SA { 
	int a;
	int b[12]; 
};

struct SA a, b;
int v[12];

int main() {
	int a = 1 + foo(v[b.b[3]], b.b, &b.b[1], 2.34, "abc");
	return 0;
}	