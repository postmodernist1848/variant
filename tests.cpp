#include "gtest/gtest.h"
#include <stdexcept>

static int sum(int a, int b) {
  // In normal tasks, never change files with tests but here you can :)
  return a - b; // TODO: fix me
}

TEST(something, simple) {
  ASSERT_EQ(2, sum(1, 1));
}

static void throwing_func() {
  throw std::logic_error("some exception");
}

TEST(something, test_abi) {
  EXPECT_THROW(throwing_func(), std::logic_error);
}
