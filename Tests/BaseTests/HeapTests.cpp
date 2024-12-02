#include "../../Base/Heap.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(Heap, Constructors) {
	Heap<int> test1;
	EXPECT_EQ(0, test1.Count());
}

TEST(Heap, AddPeek) {
	Heap<int> heap;
	
	heap.Add(3);
	heap.Add(6);
	heap.Add(5);
	EXPECT_EQ(3, heap.Count());
	EXPECT_EQ(6, heap.Peek());
}

TEST(Heap, Extract) {
	Heap<int> heap;
	
	heap.Add(3);
	heap.Add(6);
	heap.Add(5);
	EXPECT_EQ(3, heap.Count());
	EXPECT_EQ(6, heap.Extract());
	EXPECT_EQ(2, heap.Count());
	EXPECT_EQ(5, heap.Extract());
	EXPECT_EQ(1, heap.Count());
	EXPECT_EQ(3, heap.Extract());
	EXPECT_EQ(0, heap.Count());
}

TEST(Heap, RemoveContains) {
	Heap<int> heap;
	heap.Add(1);
	heap.Add(2);
	heap.Add(3);

	heap.Remove(2);
	EXPECT_EQ(2, heap.Count());
	EXPECT_FALSE(heap.Contains(2));
	EXPECT_TRUE(heap.Contains(1));
	EXPECT_TRUE(heap.Contains(3));
}

TEST(Heap, Clear) {
	Heap<int> heap;
	heap.Add(1);
	heap.Add(2);

	heap.Clear();
	EXPECT_EQ(0, heap.Count());
}