#define NULL 0

struct A1 {
	int a;
	int b;
	int c;
};

struct A2 {
	int a,b,c;
};

struct A3 {
	int a;
	char b;
	float c;
	double d;
};

struct A4 {
	int* a;
	int** b;
	int*** c;
};

struct A5 {
	int a[2];
	int b[2][3];
	int c[2][3][4];
};

struct A6 {
	int a;
	int* b;
	int c[2];
	int* d[3];
};

struct A7 {
	struct A7* a;
	struct A7** b;
	struct A7*** c;
};

struct A8 {
	struct A1 a;
	struct A2 b;
	struct A3 c;
};

struct A9 {
	struct A10 {
		struct A11 {
			int a, b;
		} a;
	} a;
};

struct A1 a = {1,2,3};
struct A2 b = {1};
struct A3 c = {1,'x', 1.5, 2.5};
struct A4 d = {NULL, NULL, NULL};
struct A5 e = {{1,2}, {{1,2,3}, {4,5,6}}};
struct A6 f = {1, NULL, {1,2,3}, {NULL, NULL, NULL}};
struct A7 g = {NULL, NULL, NULL};
struct A8 h = {{1,2,3}, {4,5,6}, {1,'x', 1.5, 2.5}};
struct A8 i = {1,2,3, 4,5,6, 1,'x', 1.5, 2.5};
struct A9 j = {1, 2};
struct A10 k = {3,4};
struct A11 l = {5,6};

struct A1 fa(struct A1 s);
struct A2 fb(struct A2 s);
struct A3 fc(struct A3 s);
struct A4 fd(struct A4 s);
struct A5 fe(struct A5 s);
struct A6 ff(struct A6 s);
struct A7 fg(struct A7 s);
struct A8 fh(struct A8 s);
struct A9 fj(struct A9 s);
struct A10 fk(struct A10 s);
struct A11 fl(struct A11 s);

void call_f() {
	a = fa(a);
	b = fb(b);
	c = fc(c);
	d = fd(d);
	e = fe(e);
	f = ff(f);
	g = fg(g);
	i = fh(i);
	j = fj(j);
	k = fk(k);
	l = fl(l);
	j.a = fk(k);
	j.a.a = fl(l);
	k.a = fl(l);
}