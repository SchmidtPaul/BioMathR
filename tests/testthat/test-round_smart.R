library(testthat)
library(BioMathR)

test_that("round_smart rounds numbers as expected", {

  # Test rounding to default signif_digits and max_digits
  expect_equal(round_smart(1.0001234), 1.0001)
  expect_equal(round_smart(1.0001234, signif_digits = 2), 1.00012)
  expect_equal(round_smart(1.0000001234), 1)
  expect_equal(round_smart(1.0000001234, max_digits = Inf), 1.0000001)

  # Test rounding negative numbers
  expect_equal(round_smart(-1.0001234), -1.0001)
  expect_equal(round_smart(-1.0001234, signif_digits = 2), -1.00012)
  expect_equal(round_smart(-1.0000001234), -1)
  expect_equal(round_smart(-1.0000001234, max_digits = Inf), -1.0000001)

  # Test rounding a vector of numbers
  expect_equal(round_smart(c(1.0001234, 2.0001234)), c(1.0001, 2.0001))

  # Test rounding when there are no digits after the decimal separator
  expect_equal(round_smart(1), 1)
  expect_equal(round_smart(12345), 12345)

  # Test rounding with different signif_digits and max_digits values
  expect_equal(round_smart(1.23456789, signif_digits = 3, max_digits = 4), 1.235)
  expect_equal(round_smart(1.23456789, signif_digits = 4, max_digits = 4), 1.2346)

})
