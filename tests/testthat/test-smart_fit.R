library(testthat)
library(BioMathR)

test_that("smart_fit returns a flextable object", {
  df <- data.frame(A = 1:5, B = 6:10)
  ft <- flextable::flextable(df)
  result <- smart_fit(ft)

  expect_s3_class(result, "flextable")
})

test_that("smart_fit doesn't crash with edge cases", {
  # Single column
  df1 <- data.frame(A = 1:5)
  ft1 <- flextable::flextable(df1)
  expect_no_error(smart_fit(ft1))

  # Many columns
  df2 <- data.frame(matrix(1:100, nrow = 10))
  ft2 <- flextable::flextable(df2)
  expect_no_error(smart_fit(ft2))

  # Very long column names
  df3 <- data.frame(
    `This is an extremely long column name that should wrap` = 1:5,
    `Another very long column name here` = 6:10
  )
  names(df3) <- c("This is an extremely long column name that should wrap",
                  "Another very long column name here")
  ft3 <- flextable::flextable(df3)
  expect_no_error(smart_fit(ft3))

  # Mixed content
  df4 <- data.frame(
    Short = c("a", "b", "c"),
    Long = c("very long text here", "short", "antidisestablishmentarianism"),
    Numbers = 1:3
  )
  ft4 <- flextable::flextable(df4)
  expect_no_error(smart_fit(ft4))
})

test_that("smart_fit handles empty and NA values", {
  df <- data.frame(
    A = c("", NA, "text"),
    B = c(NA, "", "data"),
    C = c(1, NA, 3)
  )
  ft <- flextable::flextable(df)
  expect_no_error(smart_fit(ft))
})

test_that("smart_fit validates width parameter", {
  df <- data.frame(A = 1:5, B = 6:10)
  ft <- flextable::flextable(df)

  # Valid paper sizes should work
  expect_no_error(smart_fit(ft, width = "A4"))
  expect_no_error(smart_fit(ft, width = "letter"))
  expect_no_error(smart_fit(ft, width = "legal"))
  expect_no_error(smart_fit(ft, width = "executive"))

  # Numeric width should work
  expect_no_error(smart_fit(ft, width = 15))

  # Invalid width should error
  expect_error(smart_fit(ft, width = "tabloid"))
  expect_error(smart_fit(ft, width = "invalid"))
})

test_that("smart_fit returns same number of columns as input", {
  df <- data.frame(A = 1:5, B = 6:10, C = 11:15)
  ft <- flextable::flextable(df)
  result <- smart_fit(ft)

  # Check that result has same structure
  expect_equal(ncol(result$body$dataset), 3)
})

test_that("smart_fit landscape parameter works", {
  df <- data.frame(matrix(1:50, nrow = 5))
  ft <- flextable::flextable(df)

  # Should not error with landscape
  expect_no_error(smart_fit(ft, landscape = TRUE))
  expect_no_error(smart_fit(ft, landscape = FALSE))
})

test_that("smart_fit handles custom page margins", {
  df <- data.frame(A = 1:5, B = 6:10)
  ft <- flextable::flextable(df)

  expect_no_error(smart_fit(ft, page_margin = 2))
  expect_no_error(smart_fit(ft, page_margin = 5))
  expect_no_error(smart_fit(ft, page_margin = "default"))
})

test_that("smart_fit verbose parameter works", {
  df <- data.frame(A = 1:5, B = 6:10)
  ft <- flextable::flextable(df)

  # verbose = FALSE should not print
  expect_silent(smart_fit(ft, verbose = FALSE))

  # verbose = TRUE should print output
  expect_output(smart_fit(ft, verbose = TRUE), "smart_fit")
})

test_that("smart_fit handles data frames with empty data", {
  # Note: flextable::flextable() itself errors on completely empty data frames
  # So we test smart_fit's handling directly

  # Create a flextable with data, then manually clear it to simulate empty case
  df_with_data <- data.frame(A = 1, B = 2)
  ft <- flextable::flextable(df_with_data)

  # Manually set empty dataset to test smart_fit's empty data handling
  ft$body$dataset <- data.frame()

  expect_warning(
    smart_fit(ft),
    "no data"
  )

  # Data frame with 0 rows but columns defined
  df_zero_rows <- data.frame(A = numeric(0), B = character(0))
  ft_zero <- flextable::flextable(df_zero_rows)

  expect_warning(
    smart_fit(ft_zero),
    "no data"
  )
})

test_that("smart_fit converts non-flextable objects", {
  df <- data.frame(A = 1:5, B = 6:10)

  # Should warn and convert
  expect_warning(result <- smart_fit(df), "not a flextable")
  expect_s3_class(result, "flextable")
})

test_that("smart_fit handles extremely wide tables without warning", {
  # Create a table with many columns with long names
  df <- data.frame(matrix(1:100, nrow = 5))
  names(df) <- paste("Very Long Column Name Number", 1:20)
  ft <- flextable::flextable(df)

  # New smart_fit logic handles wide tables by proportional scaling
  # It should NOT produce a warning - just scale down to fit
  expect_no_warning(result <- smart_fit(ft, width = 10))

  # Verify it returns a flextable
  expect_s3_class(result, "flextable")

  # The table should be scaled to fit the width
  # (We don't check exact dimensions as that's tested elsewhere)
})

test_that("smart_fit handles different data types", {
  df <- data.frame(
    Integer = 1:5,
    Numeric = c(1.1, 2.2, 3.3, 4.4, 5.5),
    Character = letters[1:5],
    Factor = factor(c("A", "B", "C", "D", "E")),
    Logical = c(TRUE, FALSE, TRUE, FALSE, TRUE)
  )
  ft <- flextable::flextable(df)

  expect_no_error(result <- smart_fit(ft))
  expect_s3_class(result, "flextable")
})
