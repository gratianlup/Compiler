#include "../../Base/Queue.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(Queue, Constructors) {
	Queue<int> test1;
	EXPECT_EQ(0, test1.Count());
}

TEST(Queue, EnqueuePeek) {
	Queue<int> queue;

	queue.Enqueue(0);
	queue.Enqueue(1);
	queue.Enqueue(2);
	EXPECT_EQ(3, queue.Count());
	EXPECT_EQ(0, queue.Peek());
}

TEST(Queue, Pop) {
	Queue<int> queue;
	queue.Enqueue(0);
	queue.Enqueue(1);

	EXPECT_EQ(0, queue.Dequeue());
	EXPECT_EQ(1, queue.Count());
	EXPECT_EQ(1, queue.Dequeue());
	EXPECT_EQ(0, queue.Count());
}

TEST(Queue, Clear) {
	Queue<int> queue;
	queue.Enqueue(0);
	queue.Enqueue(1);

	queue.Clear();
	EXPECT_EQ(0, queue.Count());
}

TEST(Queue, Contains) {
	Queue<int> queue;
	queue.Enqueue(0);
	queue.Enqueue(1);

	EXPECT_TRUE(queue.Contains(0));
	EXPECT_TRUE(queue.Contains(1));
	EXPECT_FALSE(queue.Contains(2));
}