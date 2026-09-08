sheet_xml_of <- function(path, n) {
  paste(
    readLines(unz(path, sprintf("xl/worksheets/sheet%d.xml", n)), warn = FALSE),
    collapse = ""
  )
}

test_that("save_wb writes the file and returns the path invisibly", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, head(mtcars, 3), "cars")

  expect_invisible(out <- save_wb(wb, path, open_after_save = FALSE))
  expect_identical(out, path)
  expect_true(file.exists(path))
  expect_identical(
    unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))),
    "cars"
  )
})

test_that("save_wb makes the second sheet active and selected when the first is 'info'", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  wb <- create_wb()
  add_sheet(wb, head(mtcars, 3), "cars")
  save_wb(wb, path, open_after_save = FALSE)

  expect_equal(openxlsx2::wb_load(path)$get_active_sheet(), 2)

  # activeTab alone would leave sheet 1 selected as well (grouped sheets in Excel)
  expect_match(sheet_xml_of(path, 2), "tabSelected=\"1\"")
  expect_no_match(sheet_xml_of(path, 1), "tabSelected=\"1\"")
})

test_that("save_wb leaves the active sheet alone without an info sheet", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, head(mtcars, 3), "cars")
  save_wb(wb, path, open_after_save = FALSE)

  expect_equal(openxlsx2::wb_load(path)$get_active_sheet(), 1)
  expect_match(sheet_xml_of(path, 1), "tabSelected=\"1\"")
})

test_that("save_wb handles a workbook with only the info sheet", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  wb <- create_wb()
  expect_no_error(save_wb(wb, path, open_after_save = FALSE))
  expect_equal(openxlsx2::wb_load(path)$get_active_sheet(), 1)
})

test_that("save_wb respects overwrite", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, head(mtcars, 3), "cars")
  save_wb(wb, path, open_after_save = FALSE)

  expect_error(save_wb(wb, path, open_after_save = FALSE), "already exists")
  expect_no_error(
    save_wb(wb, path, overwrite = TRUE, open_after_save = FALSE)
  )
})

test_that("save_wb no longer takes further arguments", {
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, head(mtcars, 3), "cars")

  expect_error(
    save_wb(wb, tempfile(fileext = ".xlsx"), open_after_save = FALSE, returnValue = TRUE),
    "unused argument"
  )
})
