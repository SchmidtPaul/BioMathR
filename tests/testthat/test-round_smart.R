library(testthat)
library(BioMathR)

test_that("round_smart handles NAs", {
  x <- c(1, 1.0003, NA)
  expected_output <- c(1, 1.0003, NA)
  output <- round_smart(x)
  expect_equal(output, expected_output)
})

test_that("round_smart returns expected output", {
  x <- c(1.0001234, 1.0000001234, 123456.789, 0.123456789)
  expected_output <- c(1.0001, 1, 123456.789, 0.1235)
  output <- round_smart(x)
  expect_equal(output, expected_output)
})

test_that("round_smart handles signif_digits argument", {
  x <- c(1.0001234, 1.0000001234, 123456.789, 0.123456789)
  expected_output <- c(1.00012, 1, 123456.789, 0.12346)
  output <- round_smart(x, signif_digits = 2)
  expect_equal(output, expected_output)
})

test_that("round_smart handles max_digits argument", {
  x <- c(1.0001234, 1.0000001234, 123456.789, 0.123456789)
  expected_output <- c(1.0001, 1.000000, 123456.789, 0.1235)
  output <- round_smart(x, max_digits = 6)
  expect_equal(output, expected_output)
})

test_that("round_smart handles both signif_digits and max_digits arguments", {
  x <- c(1.0001234, 1.0000001234, 123456.789, 0.123456789)
  expected_output <- c(1.00012, 1.000000, 123456.789, 0.12346)
  output <- round_smart(x, signif_digits = 2, max_digits = 6)
  expect_equal(output, expected_output)
})

test_that("round_smart handles negative values", {
  x <- c(-1.0001234, -1.0000001234, -123456.789, -0.123456789)
  expected_output <- c(-1.00012, -1.000000, -123456.789, -0.12346)
  output <- round_smart(x, signif_digits = 2, max_digits = 6)
  expect_equal(output, expected_output)
})

test_that("round_smart handles positive and negative and NA values together", {
  x <- c(-1.0001234, 1.0000001234, -123456.789, 0.123456789, NA)
  expected_output <- c(-1.00012, 1.000000, -123456.789, 0.12346, NA)
  output <- round_smart(x, signif_digits = 2, max_digits = 6)
  expect_equal(output, expected_output)
})
