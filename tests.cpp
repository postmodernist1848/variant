#include "gtest/gtest.h"
#include <stdexcept>

TEST(something, simple) {
  ASSERT_EQ(2, sum(1, 1));
}

static void throwing_func() {
  throw std::logic_error("some exception");
}

TEST(something, test_abi) {
  EXPECT_THROW(throwing_func(), std::logic_error);
}
