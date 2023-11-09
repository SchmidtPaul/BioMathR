library(testthat)

test_that("collapse single letter", {
  expect_equal(str_collapse_with(letters[1:1]), "a")
})

test_that("collapse two letters", {
  expect_equal(str_collapse_with(letters[1:2]), "a and b")
})

test_that("collapse three letters", {
  expect_equal(str_collapse_with(letters[1:3]), "a, b and c")
})

test_that("collapse four letters", {
  expect_equal(str_collapse_with(letters[1:4]), "a, b, c and d")
})

test_that("collapse with custom separators and wrapping", {
  expect_equal(str_collapse_with(levels(PlantGrowth$group), final_sep = " und ", wrap_each_in = '`'), "`ctrl`, `trt1` und `trt2`")
})

test_that("collapse with ampersand and custom wrapping", {
  expect_equal(str_collapse_with(letters[1:5], sep = " & ", final_sep = " as well as ", wrap_each_in = '{}'), "{a} & {b} & {c} & {d} as well as {e}")
})

# Test for handling NULL input
test_that("handle NULL input with default error", {
  expect_error(str_collapse_with(NULL), "Input vector is NULL or empty.")
})

test_that("handle NULL input without error", {
  expect_equal(str_collapse_with(NULL, error_when_NULL = FALSE), NULL)
})

# Test for handling empty vector
test_that("handle empty vector with default error", {
  expect_error(str_collapse_with(character(0)), "Input vector is NULL or empty.")
})

test_that("handle empty vector without error", {
  expect_equal(str_collapse_with(character(0), error_when_NULL = FALSE), NULL)
})

# Test for handling NA and empty strings
test_that("handle NA and empty strings with default error", {
  expect_error(str_collapse_with(c(NA, "")), "Input vector contains NA or empty strings.")
})

test_that("replace NA and empty strings", {
  expect_equal(str_collapse_with(c("a", NA, "", "b"), error_when_NA = FALSE), "a, -, - and b")
})

# Test for custom NA replacement
test_that("custom NA replacement", {
  expect_equal(str_collapse_with(c("a", NA, "", "b"), error_when_NA = FALSE, na_replace = "NONE"), "a, NONE, NONE and b")
})

# Test for custom separators
test_that("custom separators", {
  expect_equal(str_collapse_with(c("a", "b", "c"), sep = " - ", final_sep = " + "), "a - b + c")
})

# Test for wrapping each element
test_that("wrap each element", {
  expect_equal(str_collapse_with(c("a", "b", "c"), wrap_each_in = "\""), "\"a\", \"b\" and \"c\"")
})

# Test for two-character wrapping
test_that("two-character wrapping", {
  expect_equal(str_collapse_with(c("a", "b", "c"), wrap_each_in = "{}"), "{a}, {b} and {c}")
})

# Comprehensive test with all features
test_that("comprehensive test with all features", {
  expect_equal(
    str_collapse_with(
      c("a", NA, "", "b", "c"),
      sep = "; ",
      final_sep = " & ",
      wrap_each_in = "()",
      error_when_NA = FALSE,
      na_replace = "missing"
    ),
    "(a); (missing); (missing); (b) & (c)"
  )
})
