void a();
void b(int);
void c(int, int);
void d(int, int, int, int, int, int, int, int, int);
void* e(int a, char b, float d, double e);
int* f(int a, char* b, float** d, double*** e);
int** g(int a, int b[3], int c[2][3], int d[2][3][4]);

struct A { int a, b; };
union B { int a; double b; };
enum C { Ca, Cb };

struct A h(struct A a, struct A* b, struct A c[2], struct A* d[2]);
enum C j(enum C a, struct A b, union B* b);
struct A* k(struct A* a);

void l(int, ...);
void m(int, char, float, ...);
char* n(char*, char**, ...);

void o(void (*a)());
void p(void (*a)(int, int));
void q(void* (*a)(int, char, float, double));
void r(int* (*a)(int, char*, float**, double***));
void s(struct A (*a)(struct A, struct A*, struct A [2], struct A* [2]));

typedef void (*pfa)();
typedef void (*pfc)(int, int);
typedef void* (*pfd)(int, char, float, double);
typedef struct A (*pfh)(struct A, struct A*, struct A [2], struct A* [2]);

pfa t();
pfc u();
pfh v();

void call_f() {
	a();
	b(2);
	c(2,3);
	d(1,2,3,4,5,6,7,8,9);
	
	void* t1 = e(1, 'a', 3.5, 4.5);
	int* t2 = f(1, 0, 0, 0);
	
	int t4[3][, t5[3][4], t6[1][2][3];
	int** t7 = g(t4[0], t4, t5, t6);
	int*** t8;
	*t8 = g(t4[0], t5[0], t6[1], t6);
	
	struct A t9, t10[3];
	struct A* t11[4];
	
	t9 = h(t9, &t9, t10, t11);
	
	union B t12;
	enum C t13;
	
	t13 = j(t13, *t11[2], &t12);
	t10[2] = k(&t10[1]);
	
	l(1,2,3,4,5,6);
	m(1, 'x', 2.4, 2,3, 3.5, "abc", 'a');
	
	char* t14 = "abcdef";
	n(t14, &t14, 1, 2, 3, t14);
	
	o(a);
	p(c);
	q(e);
	r(f);
	s(h);
	
	o(t());
	p(u());
	s(v());
}