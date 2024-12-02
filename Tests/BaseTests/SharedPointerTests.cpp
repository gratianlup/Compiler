#include "../../Base/SharedPointer.hpp"
#include "../../Base/LocalPointer.hpp"
#include "../../Base/List.hpp"
#include <gtest\gtest.h>
using namespace Base;

struct TestObj {
	static int ConstCt;
	static int DestCt;

	TestObj() { ConstCt++; }
	~TestObj() { DestCt++; }
};

int TestObj::ConstCt = 0;
int TestObj::DestCt = 0;

TEST(SharedPointer, Basic) {
	shared<TestObj> pt = new TestObj();
	EXPECT_EQ(1, TestObj::ConstCt);

	shared<TestObj> pt2 = new TestObj();
	EXPECT_EQ(2, TestObj::ConstCt);
	
	shared<TestObj> pt3 = pt;
	EXPECT_EQ(2, TestObj::ConstCt);

	pt2 = pt;
	EXPECT_EQ(2, TestObj::ConstCt);
	EXPECT_EQ(1, TestObj::DestCt);
}

TEST(SharedPointer, CheckBasic) {
	EXPECT_EQ(2, TestObj::DestCt);
}

TEST(SharedPointer, Transfer) {
	shared<TestObj> pt = new TestObj();
	local<TestObj> pt2(pt.Get());
}

TEST(SharedPointer, CheckTransfer) {
	EXPECT_EQ(3, TestObj::DestCt);
}

TEST(SharedPointer, Collection) {
	List<shared<TestObj>> list;
	list.Add(new TestObj());
	list.Add(new TestObj());
	list.Add(new TestObj());
}

TEST(SharedPointer, CheckCollection) {
	EXPECT_EQ(6, TestObj::DestCt);
}