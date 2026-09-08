test_df <- function() {
  data.frame(
    chr = c("a", NA, "c"),
    num = c(1.5, 2.5, NA),
    day = as.Date("2026-09-08") + 0:2,
    stringsAsFactors = FALSE
  )
}

# style XML of one cell, e.g. xf_of(wb, "A2")
xf_of <- function(wb, dims, sheet = 1) {
  st <- openxlsx2::wb_get_cell_style(wb, sheet = sheet, dims = dims)
  wb$styles_mgr$styles$cellXfs[as.integer(st) + 1L]
}

# number format code applied to one cell (openxlsx2 registers one numFmt per cell)
numfmt_of <- function(wb, dims, sheet = 1) {
  xf <- xf_of(wb, dims, sheet)
  id <- regmatches(xf, regexpr('(?<=numFmtId=")[0-9]+', xf, perl = TRUE))
  fmt <- grep(sprintf('numFmtId="%s"', id), wb$styles_mgr$styles$numFmts, value = TRUE)
  regmatches(fmt, regexpr('(?<=formatCode=")[^"]+', fmt, perl = TRUE))
}

test_that("add_sheet writes data and modifies the workbook in place", {
  wb <- create_wb(infosheet = FALSE)
  expect_invisible(out <- add_sheet(wb, test_df(), "dat"))

  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), "dat")
  expect_identical(out, wb) # returned invisibly, same object

  got <- openxlsx2::wb_to_df(wb, sheet = "dat")
  expect_identical(names(got), c("chr", "num", "day"))
  expect_equal(nrow(got), 3)
})

test_that("add_sheet defaults the sheet name to the data object name", {
  wb <- create_wb(infosheet = FALSE)
  mycars <- head(mtcars, 3)
  add_sheet(wb, mycars)

  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), "mycars")
})

test_that("add_sheet refuses a sheet name that already exists", {
  # {openxlsx2} would silently create "x (1)" and the data would then be
  # written into the *old* sheet "x" (BioMathR 0.9.0 review, P1-B1)
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, head(mtcars, 3), "x")

  expect_error(add_sheet(wb, head(iris, 2), "x"), "already exists")
  expect_error(add_sheet(wb, head(iris, 2), "X"), "already exists") # Excel is case-insensitive

  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), "x")
  got <- openxlsx2::wb_to_df(wb, sheet = "x")
  expect_identical(names(got), names(mtcars))
  expect_equal(nrow(got), 3)

  # the info sheet of create_wb() counts as well
  wb2 <- create_wb()
  expect_error(add_sheet(wb2, head(mtcars, 3), "Info"), "already exists")
  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb2)), "info")
})

test_that("add_sheet writes into the sheet name that openxlsx2 actually assigned", {
  # "/" is illegal in sheet names; {openxlsx2} cleans the name (with a warning)
  # and add_sheet() must keep using the cleaned name for the data and styles
  wb <- create_wb(infosheet = FALSE)
  expect_warning(add_sheet(wb, test_df(), "Mean/SD"), "illegal characters")

  expect_length(openxlsx2::wb_get_sheet_names(wb), 1)
  got <- openxlsx2::wb_to_df(wb, sheet = 1)
  expect_identical(names(got), c("chr", "num", "day"))
  expect_equal(nrow(got), 3)
})

test_that("add_sheet rejects an invalid sheet name", {
  wb <- create_wb(infosheet = FALSE)
  expect_error(add_sheet(wb, test_df(), ""), "single non-empty string")
  expect_error(add_sheet(wb, test_df(), c("a", "b")), "single non-empty string")
})

test_that("add_sheet leaves NA cells empty instead of writing #N/A", {
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, test_df(), "dat")

  values <- wb$worksheets[[1]]$sheet_data$cc$v
  expect_false(any(grepl("#N/A", values, fixed = TRUE)))

  got <- openxlsx2::wb_to_df(wb, sheet = "dat")
  expect_true(is.na(got$chr[2]))
  expect_true(is.na(got$num[3]))
  expect_type(got$num, "double") # empty cells must not turn the column to text
})

test_that("add_sheet writes the na placeholder when one is given", {
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, test_df(), "dat", na = "-")

  got <- openxlsx2::wb_to_df(wb, sheet = "dat")
  expect_identical(got$chr[2], "-")
  expect_identical(got$num[3], "-")
  expect_identical(got$chr[1], "a")
})

test_that("add_sheet formats header, borders, freeze pane and filter", {
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, test_df(), "dat")

  # header row is bold, in the base font and size
  header_xf <- xf_of(wb, "A1")
  font_id <- as.integer(
    regmatches(header_xf, regexpr('(?<=fontId=")[0-9]+', header_xf, perl = TRUE))
  )
  header_font <- wb$styles_mgr$styles$fonts[[font_id + 1L]]
  expect_match(header_font, "<b val=\"1\"/>")
  expect_match(header_font, "Arial")
  expect_match(header_font, "<sz val=\"10\"/>")
  expect_match(header_xf, "wrapText=\"0\"")

  # body cells wrap text
  expect_match(xf_of(wb, "A2"), "wrapText=\"1\"")

  # empty cells keep their border
  na_xf <- xf_of(wb, "A3")
  border_id <- as.integer(
    regmatches(na_xf, regexpr('(?<=borderId=")[0-9]+', na_xf, perl = TRUE))
  )
  expect_match(wb$styles_mgr$styles$borders[[border_id + 1L]], "thin")

  expect_match(wb$worksheets[[1]]$freezePane, "state=\"frozen\"")
  expect_match(wb$worksheets[[1]]$autoFilter, "autoFilter")
  expect_match(wb$worksheets[[1]]$sheetViews, "showGridLines=\"0\"")
  expect_identical(unique(wb$worksheets[[1]]$sheet_data$row_attr$ht), "15")
})

test_that("add_sheet formatting survives the round trip through a file", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, test_df(), "dat")
  wb$save(path)

  # data: NA cells come back empty, not as text
  back <- openxlsx2::wb_to_df(openxlsx2::wb_load(path), sheet = "dat")
  expect_true(is.na(back$chr[2]))
  expect_true(is.na(back$num[3]))
  expect_type(back$num, "double")

  # styles: bold header font, thin borders, date format
  styles_xml <- paste(readLines(unz(path, "xl/styles.xml"), warn = FALSE), collapse = "")
  expect_match(styles_xml, "<b val=\"1\"/>")
  expect_match(styles_xml, "style=\"thin\"")
  expect_match(styles_xml, "formatCode=\"yyyy-mm-dd\"")

  # sheet: freeze pane and filter
  sheet_xml <- paste(readLines(unz(path, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
  expect_match(sheet_xml, "state=\"frozen\"")
  expect_match(sheet_xml, "<autoFilter")
})

test_that("add_sheet applies date and datetime number formats", {
  dat <- data.frame(
    day = as.Date("2026-09-08") + 0:1,
    stamp = as.POSIXct("2026-09-08 10:00:00", tz = "UTC") + 0:1
  )

  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, dat, "dat")

  fmts <- wb$styles_mgr$styles$numFmts
  expect_true(any(grepl("yyyy-mm-dd\"", fmts)))
  expect_true(any(grepl("yyyy-mm-dd hh:mm:ss", fmts)))

  # each format is assigned to its own column
  expect_identical(numfmt_of(wb, "A2"), "yyyy-mm-dd")
  expect_identical(numfmt_of(wb, "B2"), "yyyy-mm-dd hh:mm:ss")
})

test_that("add_sheet formats POSIXct columns without tzone and with NA", {
  dat <- data.frame(stamp = as.POSIXct(c("2026-09-08 10:00:00", NA)))

  wb <- create_wb(infosheet = FALSE)
  expect_no_error(add_sheet(wb, dat, "dat"))
  expect_true(any(grepl("yyyy-mm-dd hh:mm:ss", wb$styles_mgr$styles$numFmts)))
})

test_that("add_sheet can switch formatting off", {
  wb <- create_wb(infosheet = FALSE)
  add_sheet(
    wb, test_df(), "dat",
    add_filters = FALSE,
    freeze_first_row = FALSE,
    date_format = "none"
  )

  expect_length(wb$worksheets[[1]]$autoFilter, 0)
  expect_length(wb$worksheets[[1]]$freezePane, 0)
  expect_length(wb$styles_mgr$styles$numFmts, 0)
})

test_that("add_sheet applies col_width and clamps it to col_width_max", {
  # width in the XML = requested width + 0.711 padding of {openxlsx2}
  wb <- create_wb(infosheet = FALSE)
  add_sheet(wb, test_df(), "dat", col_width = 12)
  expect_match(wb$worksheets[[1]]$cols_attr, "width=\"12.711\"")

  wb2 <- create_wb(infosheet = FALSE)
  add_sheet(wb2, test_df(), "dat", col_width = 50, col_width_max = 25)
  expect_match(wb2$worksheets[[1]]$cols_attr, "width=\"25\"")

  wb3 <- create_wb(infosheet = FALSE)
  add_sheet(wb3, test_df(), "dat", col_width_min = 13, col_width_max = 13)
  expect_match(wb3$worksheets[[1]]$cols_attr, "width=\"13\"")
  expect_no_match(wb3$worksheets[[1]]$cols_attr, "width=\"5.711\"")
})

test_that("add_sheet handles a zero-row data frame", {
  wb <- create_wb(infosheet = FALSE)
  empty <- test_df()[0, ]

  expect_no_error(add_sheet(wb, empty, "empty"))
  expect_identical(
    names(openxlsx2::wb_to_df(wb, sheet = "empty")),
    c("chr", "num", "day")
  )
})

test_that("add_sheet treats an explicit sheetName = NULL like sheet_name = NULL", {
  withr::local_options(lifecycle_verbosity = "quiet")
  wb <- create_wb(infosheet = FALSE)
  mycars <- head(mtcars, 3)
  add_sheet(wb, mycars, sheetName = NULL)

  expect_identical(unname(openxlsx2::wb_get_sheet_names(wb)), "mycars")
})

test_that("every deprecated add_sheet argument warns and has the same effect as its replacement", {
  withr::local_options(lifecycle_verbosity = "warning")

  dat <- data.frame(
    chr = c("a", "b"),
    stamp = as.POSIXct("2026-09-08 10:00:00", tz = "UTC") + 0:1,
    day = as.Date("2026-09-08") + 0:1,
    stringsAsFactors = FALSE
  )

  # one row per alias: old name, new name, non-default value, and a witness
  # that reads the effect off the workbook
  cases <- list(
    list("sheetName", "sheet_name", "legacy",
         function(wb) unname(openxlsx2::wb_get_sheet_names(wb))),
    list("colWidth", "col_width", 12,
         function(wb) wb$worksheets[[1]]$cols_attr),
    list("colWidthMin", "col_width_min", 20,
         function(wb) wb$worksheets[[1]]$cols_attr),
    list("colWidthMax", "col_width_max", 6,
         function(wb) wb$worksheets[[1]]$cols_attr),
    list("gridLines", "grid_lines", TRUE,
         function(wb) wb$worksheets[[1]]$sheetViews),
    list("freezefirstRow", "freeze_first_row", FALSE,
         function(wb) wb$worksheets[[1]]$freezePane),
    list("freezefirstCol", "freeze_first_col", TRUE,
         function(wb) wb$worksheets[[1]]$freezePane),
    list("addFilters", "add_filters", FALSE,
         function(wb) wb$worksheets[[1]]$autoFilter),
    list("rowheight", "row_height", 30,
         function(wb) unique(wb$worksheets[[1]]$sheet_data$row_attr$ht)),
    list("textwrap", "text_wrap", FALSE,
         function(wb) xf_of(wb, "A2")),
    list("dateformat", "date_format", "dd.mm.yyyy",
         function(wb) wb$styles_mgr$styles$numFmts),
    list("datetimeformat", "datetime_format", "dd.mm.yyyy hh:mm",
         function(wb) wb$styles_mgr$styles$numFmts)
  )

  build <- function(args) {
    wb <- create_wb(infosheet = FALSE)
    do.call(add_sheet, c(list(wb = wb, data = dat), args))
    wb
  }

  for (case in cases) {
    old_name <- case[[1]]
    new_name <- case[[2]]
    value    <- case[[3]]
    witness  <- case[[4]]

    args_new <- list(sheet_name = "s")
    args_new[[new_name]] <- value
    args_old <- list(sheet_name = "s")
    args_old[[old_name]] <- value
    if (old_name == "sheetName") args_old$sheet_name <- NULL

    with_default <- witness(build(list(sheet_name = "s")))
    with_new     <- witness(build(args_new))

    # the witness must be sensitive to the argument at all
    expect_false(identical(with_default, with_new), info = new_name)

    expect_warning(
      wb_old <- build(args_old),
      class = "lifecycle_warning_deprecated",
      info = old_name
    )
    expect_identical(witness(wb_old), with_new, info = old_name)
  }
})
