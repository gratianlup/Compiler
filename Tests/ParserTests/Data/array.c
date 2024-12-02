int a[2] = {1,2};
int b[2][3] = {{1,2,3}, {4,5,6}};
int c[2][3] = {1,2,3,4,5,6};

int* d[2] = {0, 0};
int* e[2] = {0};

int f[] = {1,2,3};
int g[];                     // g[1]
int h[][2] = {{1,2}, {3,4}}; // h[2][2]
int i[][2] = {1,2,3,4};      // g[2][2]
int j[][2] = {1};            // i[1][2]

struct A1 { int a, b; };

struct A1 k[2] = {1,2,3,4};
struct A1 l[2] = {0};
struct A1 m[] = {{1,2}, {3,4}}; // m[2]
struct A1 n[] = {1,2,3,4};      // n[2]
struct A1 o[] = {1};            // n[1], a,b=1,0
struct A1 p[];                  // p[1], a,b=0,0
struct A1 q[] = {1,2,3};        // p[2], a,b=1,2/3,0

float r[2] = {1.5, 2.5};
char s[2] = {'a', 'b'};
char t[2] = "ab";
char u[][3] = {"abc", "def"};
char* v[] = {"abc", "def", "ghi"};

struct A2 {
	char* a;
	int b;
};

struct A2 w[2] = {{"abc", 1}, {"def", 2}};
struct A2 x[] = {"abc", 1, "def", 2};
struct A2 y[];           // y[1], a,b=0,0
struct A2 z[] = {"abc"}; // z[i], a,b="abc", 0