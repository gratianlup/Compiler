#include "../../Base/Dictionary.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(Dictionary, Constructors) {
	Dictionary<int, int> test1;
	EXPECT_EQ(0, test1.Count());
}

TEST(Dictionary, Add) {
	Dictionary<int, int> dict;

	dict.Add(0, 10);
	dict.Add(1, 11);
	dict.Add(2, 12);
	EXPECT_EQ(3, dict.Count());
	EXPECT_TRUE(dict.ContainsKey(0));
	EXPECT_TRUE(dict.ContainsKey(1));
	EXPECT_TRUE(dict.ContainsKey(2));

	dict.Add(0, 10);
	EXPECT_EQ(3, dict.Count());
}

TEST(Dictionary, ContainsKey) {
	Dictionary<int, int> dict;
	dict.Add(0, 0);
	dict.Add(1, 1);
	dict.Add(2, 2);

	EXPECT_TRUE(dict.ContainsKey(0));
	EXPECT_TRUE(dict.ContainsKey(1));
	EXPECT_TRUE(dict.ContainsKey(2));
	EXPECT_FALSE(dict.ContainsKey(3));
}

TEST(Dictionary, ContainsValue) {
	Dictionary<int, int> dict;
	dict.Add(0, 0);
	dict.Add(1, 1);
	dict.Add(2, 2);

	EXPECT_TRUE(dict.ContainsValue(0));
	EXPECT_TRUE(dict.ContainsValue(1));
	EXPECT_TRUE(dict.ContainsValue(2));
	EXPECT_FALSE(dict.ContainsValue(3));
}

TEST(Dictionary, Clear) {
	Dictionary<int, int> dict;
	dict.Add(0, 0);
	dict.Add(1, 1);
	dict.Add(2, 2);

	dict.Clear();
	EXPECT_EQ(0, dict.Count());
	EXPECT_FALSE(dict.ContainsKey(0));
	EXPECT_FALSE(dict.ContainsKey(1));
	EXPECT_FALSE(dict.ContainsKey(2));
}

TEST(Dictionary, Remove) {
	Dictionary<int, int> dict;
	dict.Add(0, 0);
	dict.Add(1, 1);
	dict.Add(2, 2);

	dict.Remove(0);
	EXPECT_EQ(2, dict.Count());
	EXPECT_FALSE(dict.ContainsKey(0));

	dict.Remove(1);
	EXPECT_EQ(1	, dict.Count());
	EXPECT_FALSE(dict.ContainsKey(1));

	dict.Remove(2);
	EXPECT_EQ(0, dict.Count());
	EXPECT_FALSE(dict.ContainsKey(2));
}

TEST(Dictionary, TryGetValue) {
	Dictionary<int, int> dict;
	dict.Add(0, 0);
	dict.Add(1, 1);
	dict.Add(2, 2);

	int temp;
	EXPECT_TRUE(dict.TryGetValue(1, &temp));
	EXPECT_EQ(1, temp);

	EXPECT_FALSE(dict.TryGetValue(5, &temp));
}

TEST(Dictionary, Hash) {
	struct Test {
		unsigned int GetHashCode() const {
			return 1234;
		}

		bool operator ==(const Test& other) { return false; }
		bool operator <(const Test& other) { return false; }
	};

	Dictionary<Test*, int> dict;
	dict.Add(new Test(), 10);
}

TEST(Dictionary, Predicates) {
	typedef Dictionary<int, int>::TPair Pair;
	Dictionary<int, int> dict;
	dict.Add(0, 0);
	dict.Add(1, 1);
	dict.Add(2, 2);
	dict.Add(3, 3);

	// ForEach.
	int temp = 0;
	dict.ForEach([&temp](Pair p) mutable {
		temp += p.Value;
	});
	EXPECT_EQ(6, temp);

	// TrueForAll.
	EXPECT_TRUE(dict.TrueForAll([](Pair p) { return p.Value >= 0; }));
	EXPECT_FALSE(dict.TrueForAll([](Pair p) { return p.Value % 2 == 0; }));

	// RemoveAll.
	EXPECT_EQ(2, dict.RemoveAll([](Pair p) { return p.Value % 2 == 0; }));
	EXPECT_TRUE(dict.ContainsValue(1));
	EXPECT_TRUE(dict.ContainsValue(3));
	EXPECT_FALSE(dict.ContainsValue(0));
	EXPECT_FALSE(dict.ContainsValue(2));
}
//
//TEST(Dictionary, Stress) {
//	Dictionary<int, int> dict;
//	for(int i = 0; i < 10*1000; i++) {
//		dict.Add(i, i);
//	}
//
//	for(int i = 0; i < 10*1000; i++) {
//		EXPECT_TRUE(dict.ContainsKey(i));
//		EXPECT_EQ(i, dict[i]);
//	}
//}