#include "../../Base/LinkedList.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(LinkedList, Constructors) {
	LinkedList<int> test1;

	EXPECT_EQ(0, test1.Count());
	EXPECT_EQ(nullptr, test1.First());
	EXPECT_EQ(nullptr, test1.Last());
}

TEST(LinkedList, Add) {
	LinkedList<int> list;
	list.Add(0);
	list.Add(1);
	list.Add(2);

	EXPECT_EQ(3, list.Count());
	EXPECT_TRUE(list.First());
	EXPECT_TRUE(list.Last());
	EXPECT_EQ(0, list.First()->Value);
	EXPECT_EQ(2, list.Last()->Value);

	auto node = list.First();
	for(int i = 0; i < 3; i++) {
		EXPECT_EQ(i, node->Value);
		node = node->Next;
	}
}

TEST(LinkedList, AddFirst) {
	LinkedList<int> list;
	list.Add(0);

	list.AddFirst(1);
	list.AddFirst(2);

	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(2, list.First()->Value);
	EXPECT_EQ(1, list.First()->Next->Value);
	EXPECT_EQ(0, list.First()->Next->Next->Value);
}

TEST(LinkedList, AddLast) {
	LinkedList<int> list;
	list.Add(0);

	list.AddLast(1);
	list.AddLast(2);

	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(0, list.First()->Value);
	EXPECT_EQ(1, list.First()->Next->Value);
	EXPECT_EQ(2, list.First()->Next->Next->Value);
}

TEST(LinkedList, AddBefore) {
	LinkedList<int> list;
	list.Add(0);
	list.Add(1);

	list.AddBefore(list.First(), 2);
	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(2, list.First()->Value);
	EXPECT_EQ(0, list.First()->Next->Value);

	list.AddBefore(list.First()->Next, 3);
	EXPECT_EQ(4, list.Count());
	EXPECT_EQ(2, list.First()->Value);
	EXPECT_EQ(3, list.First()->Next->Value);
	EXPECT_EQ(0, list.First()->Next->Next->Value);
}

TEST(LinkedList, AddAfter) {
	LinkedList<int> list;
	list.Add(0);
	list.Add(1);

	list.AddAfter(list.Last(), 2);
	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(2, list.Last()->Value);
	EXPECT_EQ(1, list.Last()->Previous->Value);

	list.AddAfter(list.Last()->Previous, 3);
	EXPECT_EQ(4, list.Count());
	EXPECT_EQ(2, list.Last()->Value);
	EXPECT_EQ(3, list.Last()->Previous->Value);
	EXPECT_EQ(1, list.Last()->Previous->Previous->Value);
}

TEST(LinkedList, Remove) {
	LinkedList<int> list;
	list.Add(0);
	list.Add(1);
	list.Add(2);
	list.Add(3);

	list.Remove(list.First());
	EXPECT_EQ(3, list.Count());
	EXPECT_EQ(1, list.First()->Value);

	list.Remove(list.First()->Next);
	EXPECT_EQ(2, list.Count());
	EXPECT_EQ(1, list.First()->Value);
	EXPECT_EQ(3, list.First()->Next->Value);

	list.Remove(list.Last());
	EXPECT_EQ(1, list.Count());
	EXPECT_EQ(1, list.First()->Value);
	EXPECT_EQ(1, list.Last()->Value);

	list.Remove(list.First());
	EXPECT_EQ(0, list.Count());
	EXPECT_EQ(nullptr, list.First());
	EXPECT_EQ(nullptr, list.Last());
}

TEST(LinkedList, RemoveFirst) {
	LinkedList<int> list;
	list.Add(0);
	list.Add(1);

	list.RemoveFirst();
	EXPECT_EQ(1, list.Count());
	EXPECT_EQ(1, list.First()->Value);

	list.RemoveFirst();
	EXPECT_EQ(nullptr, list.First());
	EXPECT_EQ(nullptr, list.Last());
}

TEST(LinkedList, RemoveLast) {
	LinkedList<int> list;
	list.Add(0);
	list.Add(1);

	list.RemoveLast();
	EXPECT_EQ(1, list.Count());
	EXPECT_EQ(0, list.First()->Value);

	list.RemoveLast();
	EXPECT_EQ(nullptr, list.First());
	EXPECT_EQ(nullptr, list.Last());
}

TEST(LinkedList, Stress) {
	LinkedList<int> list;
	for(int i = 0; i < 1000; i++) {
		list.Add(i);
	}

	auto node = list.First();
	for(int i = 0; i < 1000; i++) {
		EXPECT_EQ(i, node->Value);
		node = node->Next;
	}

	// Remove odd numbers.
	node = list.First();
	int ct = 0;
	while(node) {
		auto next = node->Next;

		if(ct++ % 2) {
			list.Remove(node);
		}

		node = next;
	}

	node = list.First();
	for(int i = 0; i < 500; i++) {
		EXPECT_EQ(i * 2, node->Value);
		node = node->Next;
	}
}

TEST(LinkedList, Predicates) {
	LinkedList<int> list;
	list.Add(0);
	list.Add(1);
	list.Add(2);
	list.Add(3);

	// ForEach.
	int temp = 0;
	list.ForEach([&temp](int n) mutable {
		temp += n;
	});
	EXPECT_EQ(6, temp);

	// TrueForAll.
	EXPECT_TRUE(list.TrueForAll([](int n) { return n >= 0; }));
	EXPECT_FALSE(list.TrueForAll([](int n) { return n % 2 == 0; }));
}