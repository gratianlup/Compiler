#include "../../Base/StaticStack.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(StaticStack, Constructors) {
	StaticStack<int, 256> test1;
	EXPECT_EQ(0, test1.Count());
}

TEST(StaticStack, PushPeek) {
	StaticStack<int, 256> stack;

	stack.Push(0);
	stack.Push(1);
	stack.Push(2);
	EXPECT_EQ(3, stack.Count());
	EXPECT_EQ(2, stack.Peek());
}

TEST(StaticStack, Pop) {
	StaticStack<int, 256> stack;
	stack.Push(0);
	stack.Push(1);

	EXPECT_EQ(1, stack.Pop());
	EXPECT_EQ(1, stack.Count());
	EXPECT_EQ(0, stack.Pop());
	EXPECT_EQ(0, stack.Count());
}

TEST(StaticStack, Clear) {
	StaticStack<int, 256> stack;
	stack.Push(0);
	stack.Push(1);

	stack.Clear();
	EXPECT_EQ(0, stack.Count());
}

TEST(StaticStack, Contains) {
	StaticStack<int, 256> stack;
	stack.Push(0);
	stack.Push(1);

	EXPECT_TRUE(stack.Contains(0));
	EXPECT_TRUE(stack.Contains(1));
	EXPECT_FALSE(stack.Contains(2));
}

TEST(StaticStack, Predicate) {
	StaticStack<int, 256> stack;
	stack.Push(1);
	stack.Push(2);
	stack.Push(3);

	stack.PopWhile([](int n) -> bool {
		return n > 1;
	});

	EXPECT_EQ(1, stack.Count());
	EXPECT_EQ(1, stack.Peek());

	stack.Push(2);
	stack.Push(3);

	stack.PopUntil([](int n) -> bool {
		return n == 1;
	});

	EXPECT_EQ(1, stack.Count());
	EXPECT_EQ(1, stack.Peek());
}