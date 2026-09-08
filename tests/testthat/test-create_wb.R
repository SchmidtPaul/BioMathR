test_that("create_wb returns an openxlsx2 workbook with an info sheet", {
  wb <- create_wb()

  expect_s3_class(wb, "wbWorkbook")
  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), "info")

  info <- openxlsx2::wb_to_df(wb, sheet = "info", col_names = FALSE)
  expect_true(any(grepl("BioMath GmbH", unlist(info))))
  expect_true(any(grepl("Created on:", unlist(info))))
})

test_that("create_wb(infosheet = FALSE) has no sheets", {
  wb <- create_wb(infosheet = FALSE)

  expect_length(openxlsx2::wb_get_sheet_names(wb), 0)
})

test_that("create_wb sets the base font", {
  wb <- create_wb(font_size = 12, font_name = "Calibri", infosheet = FALSE)
  base_font <- openxlsx2::wb_get_base_font(wb)

  expect_identical(base_font$size$val, "12")
  expect_identical(base_font$name$val, "Calibri")
})

test_that("create_wb honours a custom info sheet label", {
  wb <- create_wb(infosheet_label = "Some Client")
  info <- openxlsx2::wb_to_df(wb, sheet = "info", col_names = FALSE)

  expect_true(any(grepl("Some Client", unlist(info))))
})

test_that("create_wb warns about deprecated arguments but still works", {
  withr::local_options(lifecycle_verbosity = "warning")

  expect_warning(
    wb <- create_wb(fontSize = 12, infosheet = FALSE),
    class = "lifecycle_warning_deprecated"
  )
  expect_identical(openxlsx2::wb_get_base_font(wb)$size$val, "12")

  expect_warning(
    wb <- create_wb(fontName = "Calibri", infosheet = FALSE),
    class = "lifecycle_warning_deprecated"
  )
  expect_identical(openxlsx2::wb_get_base_font(wb)$name$val, "Calibri")

  expect_warning(
    wb <- create_wb(infosheetlabel = "Legacy Label"),
    class = "lifecycle_warning_deprecated"
  )
  info <- openxlsx2::wb_to_df(wb, sheet = "info", col_names = FALSE)
  expect_true(any(grepl("Legacy Label", unlist(info))))
})
