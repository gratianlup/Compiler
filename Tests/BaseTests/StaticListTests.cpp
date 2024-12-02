#include "../../base/StaticList.hpp"
#include "TestValidator.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(StaticList, Add) {
	StaticList<int, 256> list;

	for(int i = 0; i < 10; i++) {
		list.Add(i);
	}

	EXPECT_EQ(10, list.Count());

	for(int i = 0; i < 10; i++) {
		ASSERT_EQ(i, list[i]);
	}
}

TEST(StaticList, Insert) {
	StaticList<int, 256> list;
	list.Add(1);
	list.Add(2);
	list.Add(3);

	list.Insert(0, 90);
	EXPECT_EQ(4, list.Count());
	EXPECT_EQ(90, list[0]);

	list.Insert(4, 91);
	EXPECT_EQ(5, list.Count());
	EXPECT_EQ(91, list[4]);

	list.Insert(2, 92);
	EXPECT_EQ(6, list.Count());
	EXPECT_EQ(92, list[2]);
	EXPECT_EQ(2, list[3]);
}

TEST(StaticList, AddRange) {
	StaticList<int, 256> list;
	int range[] = {1, 2, 3};
	int range2[] = {5};

	list.AddRange(range, 3);
	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(1, list[0]);
	EXPECT_EQ(2, list[1]);
	EXPECT_EQ(3, list[2]);

	list.AddRange(range2, 1);
	EXPECT_EQ(4, list.Count());
	EXPECT_EQ(5, list[3]);
}

TEST(StaticList, InsertRange) {
	StaticList<int, 256> list;
	int range[] = {1, 2, 3};
	int range2[] = {5};

	list.InsertRange(0, range2, 1);
	EXPECT_EQ(1, list.Count());
	EXPECT_EQ(5, list[0]);

	list.InsertRange(0, range, 2);
	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(1, list[0]);
	EXPECT_EQ(2, list[1]);
	EXPECT_EQ(5, list[2]);

	list.InsertRange(2, range2, 1);
	EXPECT_EQ(4, list.Count());
	EXPECT_EQ(1, list[0]);
	EXPECT_EQ(2, list[1]);
	EXPECT_EQ(5, list[2]);
	EXPECT_EQ(5, list[3]);
}

TEST(StaticList, IndexOf) {
	StaticList<int, 256> list;
	
	for(int i = 0; i < 10; i++) {
		list.Add(i);
	}

	for(int i = 0; i < 10; i++) {
		EXPECT_EQ(i, list.IndexOf(i));
	}

	EXPECT_EQ(3, list.IndexOf(3, 0, 10));
	EXPECT_EQ(-1, list.IndexOf(3, 4, 6));
	EXPECT_EQ(9, list.IndexOf(9, 9, 1));
	EXPECT_EQ(-1, list.IndexOf(6, 0, 4));
	EXPECT_EQ(-1, list.IndexOf(12, 0, 10));

	EXPECT_EQ(2, list.IndexOf(2, 0));
	EXPECT_EQ(-1, list.IndexOf(2, 3));
}

TEST(StaticList, LastIndexOf) {
	StaticList<int, 256> list;
	
	for(int i = 0; i < 10; i++) {
		list.Add(i % 5);
	}

	for(int i = 0; i < 5; i++) {
		EXPECT_EQ(5 + i, list.LastIndexOf(i));
	}

	EXPECT_EQ(5 + 4, list.LastIndexOf(4, 9, 10));
	EXPECT_EQ(0 + 4, list.LastIndexOf(4, 5, 5));
	EXPECT_EQ(-1, list.LastIndexOf(12, 9, 10));
	EXPECT_EQ(0, list.LastIndexOf(0, 0, 1));

	EXPECT_EQ(0 + 3, list.LastIndexOf(3, 5, 4));
	EXPECT_EQ(5 + 3, list.LastIndexOf(3, 9, 10));
}

TEST(StaticList, Contains) {
	StaticList<int, 256> list;
	
	for(int i = 0; i < 10; i++) {
		list.Add(i);
	}

	for(int i = 0; i < 10; i++) {
		EXPECT_TRUE(list.Contains(i));
	}
}

TEST(StaticList, RemoveAt) {
	StaticList<int, 256> list;
	list.Add(1);
	list.Add(2);
	list.Add(3);
	list.Add(4);

	list.RemoveAt(3);
	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(1, list[0]);
	EXPECT_EQ(2, list[1]);
	EXPECT_EQ(3, list[2]);

	list.RemoveAt(1);
	EXPECT_EQ(2, list.Count());
	EXPECT_EQ(1, list[0]);
	EXPECT_EQ(3, list[1]);

	list.RemoveAt(0);
	EXPECT_EQ(1, list.Count());
	EXPECT_EQ(3, list[0]);
}

TEST(StaticList, Remove) {
	StaticList<int, 256> list;
	list.Add(1);
	list.Add(2);
	list.Add(3);
	list.Add(1);

	list.Remove(1);
	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(2, list[0]);
	EXPECT_EQ(3, list[1]);
	EXPECT_EQ(1, list[2]);

	list.Remove(1);
	EXPECT_EQ(2, list.Count());
	EXPECT_EQ(2, list[0]);
	EXPECT_EQ(3, list[1]);

	list.Remove(2);
	list.Remove(3);
	EXPECT_EQ(0, list.Count());
}

TEST(StaticList, RemoveRange) {
	StaticList<int, 256> list;
	list.Add(1);
	list.Add(2);
	list.Add(3);
	list.Add(4);
	list.Add(5);

	list.RemoveRange(0, 2);
	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(3, list[0]);
	EXPECT_EQ(4, list[1]);
	EXPECT_EQ(5, list[2]);

	list.RemoveRange(0, 0);
	EXPECT_EQ(3, list.Count());

	list.RemoveRange(1, 1);
	EXPECT_EQ(2, list.Count());
	EXPECT_EQ(3, list[0]);
	EXPECT_EQ(5, list[1]);

	list.RemoveRange(0, list.Count());
	EXPECT_EQ(0, list.Count());
}

TEST(StaticList, BinarySearch) {
	StaticList<int, 256> list;
	list.Add(1);
	list.Add(2);
	list.Add(3);
	list.Add(4);
	list.Add(5);

	EXPECT_EQ(1, list.BinarySearch(2));
	EXPECT_EQ(4, list.BinarySearch(5));
	EXPECT_EQ(-1, list.BinarySearch(9));

	EXPECT_EQ(1, list.BinarySearch(0, 3, 2));
	EXPECT_EQ(-1, list.BinarySearch(2, 3, 2));
	EXPECT_EQ(-1, list.BinarySearch(0, 0, 2));
}

TEST(StaticList, Sort) {
	StaticList<int, 256> list;
	list.Add(6);
	list.Add(1);
	list.Add(3);
	list.Add(7);
	list.Add(9);
	list.Add(2);

	list.Sort();
	EXPECT_EQ(1, list[0]);
	EXPECT_EQ(2, list[1]);
	EXPECT_EQ(3, list[2]);
	EXPECT_EQ(6, list[3]);
	EXPECT_EQ(7, list[4]);
	EXPECT_EQ(9, list[5]);

	StaticList<int, 256> list2;
	for(int i = 0; i < 16; i++) {
		list2.Add(16 - i - 1);
	}

	list2.Sort<16>();
	for(int i = 0; i < 16; i++) {
		ASSERT_EQ(i, list2[i]);
	}
}

TEST(StaticList, Predicates) {
	StaticList<int, 256> list;
	list.Add(1);
	list.Add(2);
	list.Add(3);
	list.Add(4);
	list.Add(5);
	list.Add(6);
	list.Add(1);

	// Find.
	EXPECT_EQ(&list[1], list.Find([](int n) { return n == 2; }));

	// FindLast.
	EXPECT_EQ(&list[6], list.FindLast([](int n) { return n == 1; }));

	// FindIndex.
	EXPECT_EQ(1, list.FindIndex([](int n) { return n == 2; }));
	EXPECT_EQ(2, list.FindIndex(0, [](int n) { return n == 3; }));
	EXPECT_EQ(-1, list.FindIndex(4, [](int n) { return n == 2; }));
	EXPECT_EQ(1, list.FindIndex(0, 4, [](int n) { return n == 2; }));
	EXPECT_EQ(-1, list.FindIndex(0, 1, [](int n) { return n == 2; }));
	EXPECT_EQ(-1, list.FindIndex(2, 3, [](int n) { return n == 2; }));
	EXPECT_EQ(-1, list.FindIndex([](int n) { return n == 9; }));

	// FindLastIndex.
	EXPECT_EQ(1, list.FindLastIndex([](int n) { return n == 2; }));
	EXPECT_EQ(6, list.FindLastIndex([](int n) { return n == 1; }));
	EXPECT_EQ(0, list.FindLastIndex(4, [](int n) { return n == 1; }));
	EXPECT_EQ(-1, list.FindLastIndex(4, 2, [](int n) { return n == 1; }));
	EXPECT_EQ(-1, list.FindLastIndex([](int n) { return n == 9; }));

	// Exists.
	EXPECT_TRUE(list.Exists([](int n) { return n == 3; }));
	EXPECT_FALSE(list.Exists([](int n) { return n == 9; }));

	// ForEach.
	int temp = 0;
	list.ForEach([&temp](int n) mutable { temp += n % 2 == 1 ? n : 0; });
	EXPECT_EQ(10, temp);

	// RemoveAll.
	EXPECT_EQ(2, list.RemoveAll([](int n) { return n == 2 || n == 3; }));
	EXPECT_EQ(5, list.Count());
	EXPECT_EQ(1, list[0]);
	EXPECT_EQ(4, list[1]);

	// TrueForAll.
	EXPECT_TRUE(list.TrueForAll([](int n) { return n > 0; }));
	EXPECT_FALSE(list.TrueForAll([](int n) { return n % 2 == 1; }));
}

TEST(StaticList, Reverse) {
	StaticList<int, 256> list;
	list.Add(1);
	list.Add(2);
	list.Add(3);
	list.Add(4);

	list.Reverse();
	EXPECT_EQ(4, list.Count());
	EXPECT_EQ(4, list[0]);
	EXPECT_EQ(3, list[1]);
	EXPECT_EQ(2, list[2]);
	EXPECT_EQ(1, list[3]);
}