fixture_files <- function() {
  dir <- system.file("extdata", "teams_attendance", package = "BioMathR")
  sort(list.files(dir, pattern = "\\.csv$", full.names = TRUE))
}

# Wrapper for the logic tests: never write a file, and keep the historic
# minutes default so the duration expectations below stay meaningful. An
# explicit `unit = "hours"` passed through `...` overrides the default.
gta <- function(..., unit = "minutes") {
  get_teams_attendance(..., unit = unit, export = FALSE)
}

# Write a minimal Teams-style CSV from a data frame; returns the file path.
# Temp files live in the session tempdir and are cleaned up automatically.
write_teams_csv <- function(rows) {
  f <- tempfile(fileext = ".csv")
  utils::write.csv(rows, f, row.names = FALSE)
  f
}

# Convenience: one participant segment.
seg <- function(display, date, mins, email = "") {
  start <- as.POSIXct(paste0(date, " 09:00:00"), tz = "UTC")
  data.frame(
    display = display,
    userName = email,
    joinDateTime = format(start, "%Y-%m-%dT%H:%M:%SZ"),
    leaveDateTime = format(start + mins * 60, "%Y-%m-%dT%H:%M:%SZ"),
    stringsAsFactors = FALSE
  )
}

test_that("one row per unique participant with chronological day columns", {
  res <- gta(fixture_files())

  expect_s3_class(res, "tbl_df")
  expect_identical(names(res), c("name", "email", "2026-05-18", "2026-05-19", "2026-05-20"))
  # Max (merged via email) and Anna (matched by name) -> 2 rows
  expect_equal(nrow(res), 2L)
})

test_that("display-name variants are merged via e-mail to a canonical name", {
  res <- gta(fixture_files())

  max_row <- res[res$email == "max@firma.de", ]
  expect_equal(nrow(max_row), 1L)
  # most frequent display wins: "Max Mustermann (FIRMA)" (3x) over "Max M." (1x)
  expect_equal(max_row$name, "Max Mustermann (FIRMA)")
})

test_that("rejoins on the same day are summed", {
  res <- gta(fixture_files())
  max_row <- res[res$email == "max@firma.de", ]
  # 14m41s + 15m00s = 29.6833 min on day 1
  expect_equal(max_row[["2026-05-18"]], 14 + 41 / 60 + 15, tolerance = 1e-6)
})

test_that("anonymous guests without e-mail are matched by display name", {
  res <- gta(fixture_files())
  anna_row <- res[res$name == "Anna Gast", ]
  expect_equal(nrow(anna_row), 1L)
  expect_equal(anna_row$email, "")
  expect_equal(anna_row[["2026-05-18"]], 45)
  expect_equal(anna_row[["2026-05-19"]], 30)
})

test_that("NA marks absence on a given day", {
  res <- gta(fixture_files())
  anna_row <- res[res$name == "Anna Gast", ]
  # Anna did not attend on day 3
  expect_true(is.na(anna_row[["2026-05-20"]]))
})

test_that("unit = 'hours' converts and rounds correctly", {
  res <- gta(fixture_files(), unit = "hours")
  anna_row <- res[res$name == "Anna Gast", ]
  expect_equal(anna_row[["2026-05-18"]], 0.8) # 45 min = 0.75 -> rounded to 0.8
  expect_equal(anna_row[["2026-05-19"]], 0.5) # 30 min
})

test_that("match_by_email = FALSE keeps display-name variants separate", {
  # also disable name merging to isolate the e-mail behaviour
  res <- gta(
    fixture_files(),
    match_by_email = FALSE,
    merge_contained_names = FALSE
  )
  # "Max Mustermann (FIRMA)" and "Max M." are now distinct -> 3 rows total
  expect_equal(nrow(res), 3L)
})

test_that("missing key column raises an informative error", {
  bad <- tempfile(fileext = ".csv")
  on.exit(unlink(bad), add = TRUE)
  writeLines(c("display,foo", "Someone,1"), bad)
  expect_error(gta(bad), "missing expected column")
})

test_that("non-existent file raises an error", {
  expect_error(
    gta("definitely_not_a_real_file.csv"),
    "do not exist"
  )
})

test_that("empty or non-character input is rejected", {
  expect_error(gta(character(0)), "non-empty character vector")
  expect_error(gta(123), "non-empty character vector")
})

test_that("a name fully contained in another is merged into the longer one", {
  f1 <- write_teams_csv(seg("Max Mustermann", "2026-06-01", 30))
  f2 <- write_teams_csv(seg("Max Mustermann (FIRMA)", "2026-06-02", 40))

  res <- gta(c(f1, f2))
  expect_equal(nrow(res), 1L)
  expect_equal(res$name, "Max Mustermann (FIRMA)")
  expect_equal(res[["2026-06-01"]], 30)
  expect_equal(res[["2026-06-02"]], 40)
})

test_that("merge_contained_names = FALSE keeps contained names separate", {
  f1 <- write_teams_csv(seg("Max Mustermann", "2026-06-01", 30))
  f2 <- write_teams_csv(seg("Max Mustermann (FIRMA)", "2026-06-02", 40))

  res <- gta(c(f1, f2), merge_contained_names = FALSE)
  expect_equal(nrow(res), 2L)
})

test_that("punctuation/separators are ignored when matching prefixes", {
  f1 <- write_teams_csv(seg("Max Mustermann", "2026-06-01", 10))
  f2 <- write_teams_csv(seg("Max Mustermann, MRI", "2026-06-02", 20))
  f3 <- write_teams_csv(seg("Max Mustermann_BLE", "2026-06-03", 30))

  res <- gta(c(f1, f2, f3))
  expect_equal(nrow(res), 1L)
  expect_equal(res[["2026-06-01"]], 10)
  expect_equal(res[["2026-06-02"]], 20)
  expect_equal(res[["2026-06-03"]], 30)
})

test_that("variants like 'BLE', 'BLE 624' and 'BLE624' collapse into one row", {
  f1 <- write_teams_csv(seg("Max Mustermann BLE", "2026-06-01", 10))
  f2 <- write_teams_csv(seg("Max Mustermann BLE 624", "2026-06-02", 20))
  f3 <- write_teams_csv(seg("Max Mustermann BLE624", "2026-06-03", 30))

  res <- gta(c(f1, f2, f3))
  expect_equal(nrow(res), 1L)
  # longest display name wins as the canonical label
  expect_equal(res$name, "Max Mustermann BLE 624")
})

test_that("single-word names never anchor a merge", {
  f1 <- write_teams_csv(seg("Max", "2026-06-01", 10))
  f2 <- write_teams_csv(seg("Maximilian", "2026-06-02", 20))
  f3 <- write_teams_csv(seg("Max Power", "2026-06-03", 30))

  res <- gta(c(f1, f2, f3))
  # 'Max' (one word) pulls in nobody -> all three stay separate
  expect_equal(nrow(res), 3L)
  expect_setequal(res$name, c("Max", "Maximilian", "Max Power"))
})

test_that("name containment also bridges an e-mail user and an anonymous guest", {
  f1 <- write_teams_csv(seg("Max Mustermann", "2026-06-01", 30)) # guest, no e-mail
  f2 <- write_teams_csv(seg("Max Mustermann (FIRMA)", "2026-06-02", 40, email = "max@firma.de"))

  res <- gta(c(f1, f2))
  expect_equal(nrow(res), 1L)
  expect_equal(res$name, "Max Mustermann (FIRMA)")
  expect_equal(res$email, "max@firma.de")
})

test_that("an empty/blank display name does not crash the name merging", {
  f1 <- write_teams_csv(seg("Max Mustermann", "2026-06-01", 30))
  f2 <- write_teams_csv(seg("", "2026-06-02", 5)) # e.g. a dial-in without a name

  expect_no_error(res <- gta(f2 |> c(f1)))
  expect_true("Max Mustermann" %in% res$name)
  # blank display names get a recognisable placeholder
  expect_true("(ohne Namen)" %in% res$name)
})

test_that("merging is transitive across a shared prefix", {
  f1 <- write_teams_csv(seg("Max Mustermann", "2026-06-01", 10))
  f2 <- write_teams_csv(seg("Max Mustermann BLE", "2026-06-02", 20))
  f3 <- write_teams_csv(seg("Max Mustermann (FIRMA)", "2026-06-03", 30))

  res <- gta(c(f1, f2, f3))
  expect_equal(nrow(res), 1L)
  expect_equal(res[["2026-06-01"]], 10)
  expect_equal(res[["2026-06-02"]], 20)
  expect_equal(res[["2026-06-03"]], 30)
})

test_that("the 'unmerged' attribute holds the unconsolidated table", {
  res <- gta(fixture_files())
  raw <- attr(res, "unmerged")

  expect_s3_class(raw, "tbl_df")
  # No merging: "Max Mustermann (FIRMA)", "Max M." and "Anna Gast" stay distinct
  expect_equal(nrow(raw), 3L)
  expect_setequal(raw$name, c("Anna Gast", "Max M.", "Max Mustermann (FIRMA)"))
})

# ---- Excel export --------------------------------------------------------

test_that("xlsx_path writes a workbook and returns the tibble invisibly", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  expect_invisible(res <- get_teams_attendance(fixture_files(), xlsx_path = path))
  expect_true(file.exists(path))
  expect_s3_class(res, "tbl_df")
})

test_that("the workbook has the two expected sheets", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  get_teams_attendance(fixture_files(), xlsx_path = path)
  expect_identical(unname(openxlsx2::wb_get_sheet_names(openxlsx2::wb_load(path))), c("Zusammengeführt", "Roh"))
})

test_that("the merged sheet matches the returned tibble", {
  path <- tempfile(fileext = ".xlsx")
  withr::defer(unlink(path))

  res <- get_teams_attendance(fixture_files(), xlsx_path = path)
  sheet <- openxlsx2::read_xlsx(path, sheet = "Zusammengeführt")

  expect_identical(names(sheet), names(res))
  expect_equal(nrow(sheet), nrow(res))
})

test_that("export = FALSE writes nothing and returns visibly", {
  default_path <- file.path(dirname(fixture_files()[1]), "teams_attendance.xlsx")
  withr::defer(unlink(default_path))

  res <- get_teams_attendance(fixture_files(), export = FALSE)
  expect_false(file.exists(default_path))
  expect_s3_class(res, "tbl_df")
})

test_that("resolve_xlsx_path defaults to the input folder and appends .xlsx", {
  files <- c("some/folder/day1.csv", "some/folder/day2.csv")
  expect_identical(
    resolve_xlsx_path(NULL, files),
    file.path("some/folder", "teams_attendance.xlsx")
  )
  expect_identical(resolve_xlsx_path("out/report", files), "out/report.xlsx")
  expect_identical(resolve_xlsx_path("out/report.xlsx", files), "out/report.xlsx")
})
