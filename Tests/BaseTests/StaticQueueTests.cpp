#include "../../Base/StaticQueue.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(StaticQueue, EnqueuePeek) {
	StaticQueue<int, 32> queue;

	queue.Enqueue(1);
	queue.Enqueue(2);
	queue.Enqueue(3);
	EXPECT_EQ(3, queue.Count());
	EXPECT_EQ(1, queue.Peek());
}

TEST(StaticQueue, Dequeue) {
	StaticQueue<int, 32> queue;

	queue.Enqueue(1);
	queue.Enqueue(2);
	queue.Enqueue(3);
	EXPECT_EQ(1, queue.Dequeue());
	EXPECT_EQ(2, queue.Dequeue());
	EXPECT_EQ(3, queue.Dequeue());
	EXPECT_EQ(0, queue.Count());
}

TEST(StaticQueue, CircularBuffer) {
	StaticQueue<int, 8> queue;

	for(int i = 0; i < 6; i++) {
		queue.Enqueue(i);
	}

	queue.Dequeue();
	queue.Dequeue();
	
	for(int i = 0; i < 4; i++) {
		queue.Enqueue(10 + i);
	}

	EXPECT_EQ(8, queue.Count());
	for(int i = 0; i < 4; i++) {
		ASSERT_EQ(2 + i, queue.Dequeue());
	}

	EXPECT_EQ(4, queue.Count());

	for(int i = 0; i < 4; i++) {
		ASSERT_EQ(10 + i, queue.Dequeue());
	}

	EXPECT_EQ(0, queue.Count());
}

TEST(StaticQueue, Predicate) {
	StaticQueue<int, 32> queue;
	queue.Enqueue(1);
	queue.Enqueue(2);
	queue.Enqueue(3);

	queue.DequeueWhile([](int n) -> bool {
		return n <= 2;
	});

	EXPECT_EQ(1, queue.Count());
	EXPECT_EQ(3, queue.Peek());

	queue.Enqueue(2);
	queue.Enqueue(1);

	queue.DequeueUntil([](int n) -> bool {
		return n == 1;
	});

	EXPECT_EQ(1, queue.Count());
	EXPECT_EQ(1, queue.Peek());
}