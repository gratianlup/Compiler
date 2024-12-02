#include "../../Base/Stack.hpp"
#include <gtest\gtest.h>
using namespace Base;

TEST(Stack, Constructors) {
	Stack<int> test1;
	EXPECT_EQ(0, test1.Count());
}

TEST(Stack, PushPeek) {
	Stack<int> stack;

	stack.Push(0);
	stack.Push(1);
	stack.Push(2);
	EXPECT_EQ(3, stack.Count());
	EXPECT_EQ(2, stack.Peek());
}

TEST(Stack, Pop) {
	Stack<int> stack;
	stack.Push(0);
	stack.Push(1);

	EXPECT_EQ(1, stack.Pop());
	EXPECT_EQ(1, stack.Count());
	EXPECT_EQ(0, stack.Pop());
	EXPECT_EQ(0, stack.Count());
}

TEST(Stack, Clear) {
	Stack<int> stack;
	stack.Push(0);
	stack.Push(1);

	stack.Clear();
	EXPECT_EQ(0, stack.Count());
}

TEST(Stack, Contains) {
	Stack<int> stack;
	stack.Push(0);
	stack.Push(1);

	EXPECT_TRUE(stack.Contains(0));
	EXPECT_TRUE(stack.Contains(1));
	EXPECT_FALSE(stack.Contains(2));
}