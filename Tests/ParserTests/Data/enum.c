enum A1 {
	A1_a, A1_b, A1_c // 0,1,2
};

enum A2 {
	A2_b, A2_b, A2_c, // 0,1,2
};

enum A3 {
	A3_a = 1, A3_b = 2, A3_c = 3 // 1,2,3
};

enum A4 {
	A4_a = 1, A4_b, A4_c // 1,2,3
};

enum A5 {
	A5_a, A5_b = 2, A5_c // 0,2,3
};

enum A6 {
	A6_a, A6_b, A6_c = 3 // 0,1,3
};

enum A7 {
	A7_a = 'a', A7_b = 'b', A7_c = 'c' // 97,98,99
};

enum A8 {
	A8_a = 'a', A8_b, A8_c // 97,98,99
};

enum A1 a; // init 0