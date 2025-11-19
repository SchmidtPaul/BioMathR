test_that("desc_tabs creates xlsx file with correct sheets", {
  skip_if_not_installed("openxlsx")

  # Create test data
  test_data <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    treatment = rep(c("A", "B"), each = 10),
    block = rep(c("X", "Y"), 10)
  )

  # Create temporary file path
  temp_file <- tempfile(fileext = ".xlsx")

  # Run desc_tabs (suppress any messages/output)
  suppressMessages({
    desc_tabs(
      data = test_data,
      yvars = c("var1", "var2"),
      groupvars = c("treatment", "block"),
      xlsx_path = temp_file,
      xlsx_overwrite = TRUE,
      xlsx_open = FALSE,
      xlsx_data_sheet = TRUE
    )
  })

  # Check that file was created
  expect_true(file.exists(temp_file))

  # Read workbook to check sheets
  wb <- openxlsx::loadWorkbook(temp_file)
  sheet_names <- names(wb)

  # Should have: info, tre (treatment), blo (block), tre-blo (treatment-block), data
  expect_true("info" %in% sheet_names)
  expect_true("data" %in% sheet_names)
  expect_true(length(sheet_names) >= 3)  # At least info, some grouping sheets, and data

  # Clean up
  unlink(temp_file)
})

test_that("desc_tabs works without data sheet", {
  skip_if_not_installed("openxlsx")

  test_data <- data.frame(
    var1 = rnorm(10),
    treatment = rep(c("A", "B"), each = 5)
  )

  temp_file <- tempfile(fileext = ".xlsx")

  suppressMessages({
    desc_tabs(
      data = test_data,
      yvars = "var1",
      groupvars = "treatment",
      xlsx_path = temp_file,
      xlsx_overwrite = TRUE,
      xlsx_open = FALSE,
      xlsx_data_sheet = FALSE
    )
  })

  expect_true(file.exists(temp_file))

  wb <- openxlsx::loadWorkbook(temp_file)
  sheet_names <- names(wb)

  # Should not have data sheet
  expect_false("data" %in% sheet_names)

  unlink(temp_file)
})
