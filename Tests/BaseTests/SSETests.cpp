#include "../../Base/StaticList.hpp"
#include "../../Abstraction/Windows/SSE.hpp"
#include <gtest\gtest.h>
using namespace Base;
using namespace Abstraction;

TEST(SSE, Arithmetic) {
	SSE_ALIGN StaticList<short, 128> a;
	SSE_ALIGN StaticList<short, 128> b;
	SSE_ALIGN StaticList<short, 128> c(128);

	for(int i = 0; i < 128; i++) {
		a.Add(i);
		b.Add(i);
	}

	SSE::Add(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(c[i], a[i] + b[i]);
	}

	for(int i = 0; i < 128; i++) {
		a[i] = i;
		b[i] = 1000 + i;
	}

	SSE::Max(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(b[i], c[i]);
	}

	SSE::Min(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(a[i], c[i]);
	}
}

TEST(SSE, Comparison) {
	SSE_ALIGN StaticList<int, 128> a;
	SSE_ALIGN StaticList<int, 128> b;
	SSE_ALIGN StaticList<int, 128> c(128);

	for(int i = 0; i < 128; i++) {
		a.Add(i % 2 == 0 ? i : i + 1);
		b.Add(i);
	}

	SSE::Eq(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(i % 2 == 0 ? -1 : 0, c[i]);
	}

	SSE::Neq(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(i % 2 == 0 ? 0 : -1, c[i]);
	}

	for(int i = 0; i < 128; i++) {
		a[i] = i + (i >= 64 ? 1 : 0);
		b[i] = i;
	}

	SSE::Gt(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(0 + (i >= 64 ? -1 : 0), c[i]);
	}

	SSE::Gte(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(-1, c[i]);
	}

	SSE::Lt(b, a, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(0 + (i >= 64 ? -1 : 0), c[i]);
	}

	SSE::Lte(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(0 + (i >= 64 ? 0 : -1), c[i]);
	}
}

TEST(SSE, Logical) {
	SSE_ALIGN StaticList<int, 128> a;
	SSE_ALIGN StaticList<int, 128> b;
	SSE_ALIGN StaticList<int, 128> c(128);

	for(int i = 0; i < 128; i++) {
		a.Add(i);
		b.Add(i);
		c[i] = 0xABCD;
	}

	SSE::Xor(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(0, c[i]);
	}

	for(int i = 0; i < 128; i++) {
		a[i] = 2;
		b[i] = 3;
		c[i] = 0xABCD;
	}

	SSE::And(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(2, c[i]);
	}

	for(int i = 0; i < 128; i++) {
		a[i] = i % 2 ? 0 : 123;
		b[i] = i % 2 ? 123 : 0;
		c[i] = 0xABCD;
	}

	SSE::Or(a, b, c);
	for(int i = 0; i < 128; i++) {
		EXPECT_EQ(123, c[i]);
	}
}

TEST(SSE, Memory) {
	SSE_ALIGN char temp[] = "abcdefghijklmnop";
	SSE_ALIGN char buff[128] = {0};
	SSE_ALIGN char buff2[128] = {0};

	SSE::Fill<128>(buff, temp);
	for(int i = 0; i < 128; i += 16) {
		for(int j = 0; j < 16; j++) {
			EXPECT_EQ(temp[j], buff[i + j]);
		}
	}

	SSE::Copy<128>(buff2, buff);
	for(int i = 0; i < 128; i += 16) {
		for(int j = 0; j < 16; j++) {
			EXPECT_EQ(temp[j], buff2[i + j]);
		}
	}
}