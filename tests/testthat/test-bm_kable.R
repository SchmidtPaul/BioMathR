library(testthat)
library(BioMathR)

default_opts <- options()
options(digits = 10, scipen = 999)

anova <- anova(lm(weight ~ group, data = PlantGrowth))

# Column renaming ---------------------------------------------------------

test_that("column renaming works (English)", {
  result <- bm_kable(anova, lang = "eng", as_kable = FALSE)
  expect_equal(
    colnames(result),
    c("Term", "df", "SS", "MS", "F-value", "p-value")
  )
})

test_that("column renaming works (German)", {
  result <- bm_kable(anova, lang = "ger", as_kable = FALSE)
  expect_equal(
    colnames(result),
    c("Term", "FG", "SQ", "MQ", "F-Wert", "p-Wert")
  )
})

# Return types ------------------------------------------------------------

test_that("as_kable = TRUE returns knitr_kable", {
  result <- bm_kable(anova, as_kable = TRUE)
  expect_s3_class(result, "knitr_kable")
  expect_equal(attr(result, "format"), "pipe")
})

test_that("as_kable = FALSE returns tibble", {
  result <- bm_kable(anova, as_kable = FALSE)
  expect_true("tbl_df" %in% class(result))
})

# P-value formatting ------------------------------------------------------

test_that("p-value formatting works", {
  result <- bm_kable(anova, as_kable = FALSE)
  expect_type(result$`p-value`, "character")
})

test_that("p-value formatting can be disabled", {
  result <- bm_kable(anova, pvalform = NULL, as_kable = FALSE)
  expect_type(result$`p-value`, "double")
})

# Alignment ---------------------------------------------------------------

test_that("auto alignment: numeric right, text left", {
  result <- bm_kable(anova, lang = "eng")
  kable_text <- paste(as.character(result), collapse = "\n")
  # Pipe tables use :--- for left and ---: for right
  expect_match(kable_text, ":---", fixed = TRUE)  # left-aligned (Term)
  expect_match(kable_text, "---:", fixed = TRUE)   # right-aligned (numeric)
})

test_that("custom alignment is respected", {
  custom_align <- c("l", "l", "l", "l", "l", "l")
  result <- bm_kable(anova, align = custom_align)
  kable_text <- paste(as.character(result), collapse = "\n")
  # All should be left-aligned, no right-alignment markers
  expect_false(grepl("---:", kable_text, fixed = TRUE))
})

# Footnotes ---------------------------------------------------------------

test_that("abbreviation footnote is attached", {
  result <- bm_kable(anova, add_abbrev_footnote = TRUE)
  fn <- attr(result, "footnote")
  expect_type(fn, "character")
  expect_match(fn, "df = degrees of freedom")
  expect_match(fn, "MS = mean squares")
  expect_match(fn, "SS = sum of squares")
})

test_that("footnote can be disabled", {
  result <- bm_kable(anova, add_abbrev_footnote = FALSE)
  expect_null(attr(result, "footnote"))
})

test_that("bm_footnote returns asis_output", {
  tab <- bm_kable(anova)
  fn <- bm_footnote(tab)
  expect_s3_class(fn, "knit_asis")
  expect_match(as.character(fn), "^\\*.*\\*$")  # italic markdown
})

test_that("bm_footnote returns NULL when no footnote", {
  tab <- bm_kable(data.frame(A = 1:3, B = 4:6))
  expect_null(bm_footnote(tab))
})

test_that("German abbreviation footnote works", {
  result <- bm_kable(anova, lang = "ger", add_abbrev_footnote = TRUE)
  fn <- attr(result, "footnote")
  expect_type(fn, "character")
  expect_match(fn, "FG = Freiheitsgrade")
  expect_match(fn, "MQ = Mittelquadrate")
})

# German decimals ---------------------------------------------------------

test_that("German decimal format uses comma", {
  x <- data.frame(A = c("text"), B = c(1.5))
  result <- bm_kable(x, lang = "ger")
  kable_text <- paste(as.character(result), collapse = "\n")
  expect_match(kable_text, "1,5", fixed = TRUE)
})

# Input validation --------------------------------------------------------

test_that("invalid lang produces error", {
  expect_error(bm_kable(anova, lang = "french"), "lang must be either")
})

test_that("invalid digits produces error", {
  expect_error(bm_kable(anova, digits = "invalid"), "digits must be numeric")
})

# round_smart integration -------------------------------------------------

test_that("round_smart works in bm_kable", {
  before <- data.frame(
    V1 = c(123456, 1234),
    V2 = c(-123, -0.12345)
  )
  result <- as.data.frame(bm_kable(before, as_kable = FALSE, signif_digits = 2, max_digits = 10))
  expect_equal(result$V1, c(123456, 1234))
  expect_equal(result$V2, c(-123.00, -0.12))
})

# Duplicate column handling -----------------------------------------------

test_that("duplicate columns are handled", {
  x <- data.frame(`Pr(>F)` = 1:3, `Pr(>Chi)` = 4:6)
  result <- bm_kable(x, as_kable = FALSE)
  expect_true("p-value" %in% colnames(result))
  expect_true("p.value.1" %in% colnames(result))
})

# Consistency with docx_tab -----------------------------------------------

test_that("bm_kable tibble matches docx_tab tibble", {
  result_kable <- bm_kable(anova, as_kable = FALSE, lang = "eng")
  result_docx <- docx_tab(anova, asft = FALSE, lang = "eng")
  expect_equal(result_kable, result_docx)
})

options(default_opts)
