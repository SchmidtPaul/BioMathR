library(dplyr)
library(testthat)
library(BioMathR)

test_that("round_smart handles NAs", {
  x <- c(1, 1.0003, NA)
  expected_output <- c(1, 1.0003, NA)
  expect_no_warning(output <- round_smart(x))
  expect_equal(output, expected_output)
})

test_that("round_smart returns expected output", {
  x <- c(1.0001234, 1.0000001234, 123456.789, 0.123456789)
  expected_output <- c(1.0001, 1, 123456.789, 0.1235)
  expect_no_warning(output <- round_smart(x))
  expect_equal(output, expected_output)
})

test_that("round_smart handles signif_digits argument", {
  x <- c(1.0001234, 1.0000001234, 123456.789, 0.123456789)
  expected_output <- c(1.00012, 1, 123456.789, 0.12346)
  expect_no_warning(output <- round_smart(x, signif_digits = 2))
  expect_equal(output, expected_output)
})

test_that("round_smart handles max_digits argument", {
  x <- c(1.0001234, 1.0000001234, 123456.789, 0.123456789)
  expected_output <- c(1.0001, 1.000000, 123456.789, 0.1235)
  expect_no_warning(output <- round_smart(x, max_digits = 6))
  expect_equal(output, expected_output)
})

test_that("round_smart handles both signif_digits and max_digits arguments", {
  x <- c(1.0001234, 1.0000001234, 123456.789, 0.123456789)
  expected_output <- c(1.00012, 1.000000, 123456.789, 0.12346)
  expect_no_warning(output <- round_smart(x, signif_digits = 2, max_digits = 6))
  expect_equal(output, expected_output)
})

test_that("round_smart handles negative values", {
  x <- c(-1.0001234, -1.0000001234, -123456.789, -0.123456789)
  expected_output <- c(-1.00012, -1.000000, -123456.789, -0.12346)
  expect_no_warning(output <- round_smart(x, signif_digits = 2, max_digits = 6))
  expect_equal(output, expected_output)
})

test_that("round_smart handles positive and negative and NA values together", {
  x <- c(-1.0001234, 1.0000001234, -123456.789, 0.123456789, NA)
  expected_output <- c(-1.00012, 1.000000, -123456.789, 0.12346, NA)
  expect_no_warning(output <- round_smart(x, signif_digits = 2, max_digits = 6))
  expect_equal(output, expected_output)
})

test_that("round_smart handles the data.frame in examples", {
  before <- data.frame(
    V1 = c(123456, 1234),
    V2 = c(-123, -0.12345),
    V3 = c(1.0012345, 0.1),
    V4 = c(1.1, 0.0012345),
    V5 = c(1.000000012345, 0),
    V6 = c(NA, -5.0018)
  )
  expected_output <- data.frame(
    V1 = c(123456, 1234),
    V2 = c(-123.0, -0.1),
    V3 = c(1.001, 0.100),
    V4 = c(1.100, 0.001),
    V5 = c(1, 0),
    V6 = c(NA, -5.002)
  )
  expect_no_warning(output <- mutate(before, across(everything(), ~ round_smart(.))))
  expect_equal(output, expected_output)
})
