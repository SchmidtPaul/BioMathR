cf_of <- function(wb, sheet = 1) {
  wb$worksheets[[sheet]]$conditionalFormatting
}

wb_with_sheet <- function() {
  dat <- data.frame(
    grp = c("a", "b", "c", "d"),
    val = c(0, 2, 0, 5),
    stringsAsFactors = FALSE
  )
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, dat, "dat")
  wb
}

test_that("cond_format applies an expression rule to a named column", {
  wb <- wb_with_sheet()
  expect_invisible(out <- cond_format(wb, "dat", columns = "val", expression = ">0"))

  cf <- cf_of(wb)
  expect_identical(out, wb)
  expect_identical(as.character(cf$sqref), "B2:B5")
  expect_match(cf$cf, "expression")
  expect_match(cf$cf, "B2&gt;0")
})

test_that("cond_format registers one dxf style per colour combination", {
  wb <- wb_with_sheet()
  cond_format(wb, "dat", columns = "val", expression = ">0")
  cond_format(wb, "dat", columns = "val", expression = "<0")

  expect_identical(wb$styles_mgr$dxf$name, "BioMathR_white_on_ad0000")

  cond_format(
    wb, "dat", columns = "val", expression = ">3",
    font_colour = "black", bg_fill = "#ffff00"
  )
  expect_length(wb$styles_mgr$dxf$name, 2)
})

test_that("cond_format uses a dxf style registered by name instead of creating one", {
  wb <- wb_with_sheet()
  wb$add_dxfs_style(name = "mine", font_color = openxlsx2::wb_color("blue"))
  cond_format(wb, "dat", columns = "val", expression = ">0", style = "mine")

  expect_identical(wb$styles_mgr$dxf$name, "mine") # no BioMathR_* style added
  expect_match(cf_of(wb)$cf, "dxfId=\"0\"")
})

test_that("cond_format supports the colour_scale shortcut", {
  wb <- wb_with_sheet()
  cond_format(
    wb, "dat",
    columns = "val",
    colour_scale = c("red" = 0, "green" = 5)
  )

  expect_match(cf_of(wb)$cf, "colorScale")
  expect_length(wb$styles_mgr$dxf$name, 0) # colour scales need no dxf style
})

test_that("cond_format translates legacy openxlsx type names (case-insensitively)", {
  # colourScale -> colorScale
  wb <- wb_with_sheet()
  cond_format(
    wb, "dat",
    columns = "val",
    type = "colourScale",
    style = c("#FFFFFF", "#63BE7B"),
    rule = NULL
  )
  expect_match(cf_of(wb)$cf, "colorScale")

  # contains -> containsText via `type`, not via the `contains` shortcut
  wb2 <- wb_with_sheet()
  cond_format(wb2, "dat", columns = "grp", type = "contains", rule = "a")
  expect_match(cf_of(wb2)$cf, "type=\"containsText\"")
  expect_match(cf_of(wb2)$cf, "text=\"a\"")

  # notContains -> notContainsText
  wb3 <- wb_with_sheet()
  cond_format(wb3, "dat", columns = "grp", type = "notContains", rule = "a")
  expect_match(cf_of(wb3)$cf, "type=\"notContainsText\"")

  # all-lowercase names as accepted by {openxlsx}
  wb4 <- wb_with_sheet()
  cond_format(
    wb4, "dat", columns = "val", type = "colourscale",
    style = c("#FFFFFF", "#63BE7B"), rule = NULL
  )
  expect_match(cf_of(wb4)$cf, "colorScale")

  wb5 <- wb_with_sheet()
  cond_format(wb5, "dat", columns = "val", type = "databar")
  expect_match(cf_of(wb5)$cf, "type=\"dataBar\"")
  expect_length(wb5$styles_mgr$dxf$name, 0) # data bars need no dxf style

  wb6 <- wb_with_sheet()
  cond_format(wb6, "dat", columns = "val", type = "duplicates")
  expect_match(cf_of(wb6)$cf, "type=\"duplicateValues\"")
})

test_that("cond_format passes unknown type names through to openxlsx2", {
  wb <- wb_with_sheet()
  expect_error(cond_format(wb, "dat", columns = "val", type = "nonsense"))
})

test_that("cond_format formats non-consecutive rows as separate blocks", {
  wb <- wb_with_sheet()
  cond_format(wb, "dat", columns = "val", rows = c(2, 3, 5), expression = ">0")

  cf <- cf_of(wb)
  expect_setequal(as.character(cf$sqref), c("B2:B3", "B5:B5"))
})

test_that("a colour scale over gaps keeps min/max in every block", {
  # openxlsx2 1.28 writes the *colours* as numeric thresholds into every block
  # after the first when a single call spans non-consecutive cells, so
  # cond_format() must issue one call per contiguous block.
  wb <- wb_with_sheet()
  cond_format(
    wb, "dat",
    columns = "val",
    rows = c(2, 3, 5),
    type = "colorScale",
    style = c("#FFFFFF", "#63BE7B"),
    rule = NULL
  )

  cf <- cf_of(wb)$cf
  expect_length(cf, 2)
  expect_true(all(grepl("<cfvo type=\"min\"/><cfvo type=\"max\"/>", cf)))
  expect_false(any(grepl("cfvo type=\"num\"", cf)))
})

test_that("a colour scale over non-consecutive columns keeps min/max", {
  dat <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, dat, "dat")

  cond_format(
    wb, "dat",
    columns = c("a", "c"),
    type = "colorScale",
    style = c("#FFFFFF", "#63BE7B"),
    rule = NULL
  )

  cf <- cf_of(wb)
  expect_length(cf$cf, 2)
  expect_setequal(as.character(cf$sqref), c("A2:A3", "C2:C3"))
  expect_true(all(grepl("<cfvo type=\"min\"/><cfvo type=\"max\"/>", cf$cf)))
  expect_false(any(grepl("cfvo type=\"num\"", cf$cf)))
})

test_that("consecutive columns form one block", {
  dat <- data.frame(a = c(1, 2), b = c(3, 4), c = c(5, 6))
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, dat, "dat")

  cond_format(
    wb, "dat",
    columns = c("a", "b"),
    type = "colorScale",
    style = c("#FFFFFF", "#63BE7B"),
    rule = NULL
  )

  cf <- cf_of(wb)
  expect_length(cf$cf, 1)
  expect_identical(as.character(cf$sqref), "A2:B3")
})

test_that("cond_format accepts column indices", {
  wb <- wb_with_sheet()
  cond_format(wb, "dat", columns = 2, expression = ">0")

  expect_identical(as.character(cf_of(wb)$sqref), "B2:B5")
})

test_that("cond_format errors on unknown columns", {
  wb <- wb_with_sheet()

  expect_error(
    cond_format(wb, "dat", columns = "nope", expression = ">0"),
    "not found in sheet"
  )
  expect_error(
    cond_format(wb, "dat", columns = 99, expression = ">0"),
    "out of range"
  )
  expect_error(
    cond_format(wb, "dat", columns = 0, expression = ">0"),
    "out of range"
  )
  expect_error(
    cond_format(wb, "dat", columns = integer(0), expression = ">0"),
    "at least one column"
  )
  expect_length(cf_of(wb), 0)
})

test_that("cond_format does nothing on a sheet without data rows", {
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, mtcars[0, ], "empty")

  expect_no_error(cond_format(wb, "empty", columns = "mpg", expression = ">20"))
  expect_length(cf_of(wb), 0) # in particular no rule on the header row
  expect_length(wb$styles_mgr$dxf$name, 0)
})

test_that("cond_format rejects an openxlsx style object", {
  wb <- wb_with_sheet()
  fake_style <- structure(list(), class = "Style")

  expect_error(
    cond_format(wb, "dat", columns = "val", expression = ">0", style = fake_style),
    "no longer accepts"
  )
})

test_that("conditional formatting and its dxf style survive the round trip through a file", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  wb <- wb_with_sheet()
  cond_format(wb, "dat", columns = "val", expression = ">0")
  wb$save(path)

  sheet_xml <- paste(readLines(unz(path, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
  expect_match(sheet_xml, "<conditionalFormatting sqref=\"B2:B5\"")
  expect_match(sheet_xml, "dxfId=\"0\"")

  styles_xml <- paste(readLines(unz(path, "xl/styles.xml"), warn = FALSE), collapse = "")
  expect_match(styles_xml, "<dxfs count=\"1\">")
  expect_match(styles_xml, "FFAD0000") # bg_fill "#ad0000"
})

test_that("cond_format warns about deprecated arguments but still works", {
  withr::local_options(lifecycle_verbosity = "warning")

  wb <- wb_with_sheet()
  expect_warning(
    cond_format(wb, sheetName = "dat", columns = "val", expression = ">0"),
    class = "lifecycle_warning_deprecated"
  )
  expect_identical(as.character(cf_of(wb)$sqref), "B2:B5")

  wb2 <- wb_with_sheet()
  expect_warning(
    cond_format(
      wb2, "dat",
      columns = "val",
      colourScale = c("red" = 0, "green" = 5)
    ),
    class = "lifecycle_warning_deprecated"
  )
  expect_match(cf_of(wb2)$cf, "colorScale")
})
