library(testthat)
library(BioMathR)

default_opts <- options()
options(digits = 10, scipen = 999)

anova <- anova(lm(weight ~ group, data = PlantGrowth))

test_that("renaming works", {
  expect_equal(colnames(docx_tab(anova, asft = FALSE, lang = "eng")),
               c("Term", "df", "SS", "MS", "F value", "p value"))
  expect_equal(colnames(docx_tab(anova, asft = FALSE, lang = "ger")),
               c("Term", "FG", "SQ", "MQ", "F-Wert", "p-Wert"))
})

test_that("p value formatting works", {
  expect_type(docx_tab(anova, asft = FALSE)$`p value`, "character")
  expect_type(docx_tab(anova, asft = FALSE, pvalform = NULL)$`p value`, "double")

  x <- data.frame(V1 = c(0.0001, 0.001), V2 = c(0.01, 0.1))
  x <- docx_tab(x, pvalform = c("V1", "V2"), asft = FALSE)
  expect_equal(x$V1[1], "<.001***")
  expect_equal(x$V1[2], "0.001**")
  expect_equal(x$V2[1], "0.010*")
  expect_equal(x$V2[2], "0.100")
})

test_that("docx_tab works with pvalform = NULL", {
  x <- docx_tab(anova, pvalform = NULL, asft = FALSE)
  expect_equal("numeric", class(x$`p value`))
})

test_that("flextable does not throw error/warning", {
  expect_no_error(tmp <- docx_tab(anova, asft = TRUE))
  expect_no_warning(tmp <- docx_tab(anova, asft = TRUE))

  expect_true("tbl_df" %in% class(docx_tab(anova, asft = FALSE)))
  expect_equal("flextable", class(docx_tab(anova, asft = TRUE)))
})

test_that("docx_tab unifies column names correctly", {
  x <- data.frame(`Pr(>Chi)` = 1:3, `Chisq` = 4:6, `P(>|Chi|)` = 7:9)
  expected_output <- tibble("p value" = rep(">.999", 3), statistic = 4:6, p.value.3 = rep(">.999", 3))
  output <- docx_tab(x, asft = FALSE)
  expect_equal(output, expected_output)
})

test_that("round_smart works in docx_tab", {
  before <- data.frame(
    V1 = c(123456, 1234),
    V2 = c(-123, -0.12345),
    V3 = c(1.0012345, 0.1),
    V4 = c(1.1, 0.0012345),
    V5 = c(1.000000012345, 0),
    V6 = c(NA_real_, -5.0018),
    V7 = c(NA_real_, NA_real_)
  )
  expected_output <- data.frame(
    V1 = c(123456, 1234),
    V2 = c(-123.00, -0.12),
    V3 = c(1.0012, 0.1000),
    V4 = c(1.1000, 0.0012),
    V5 = c(1.000000012, 0),
    V6 = c(NA_real_, -5.0018),
    V7 = c(NA_real_, NA_real_)
  )
  expect_no_warning(output <- as.data.frame(docx_tab(before, asft = FALSE, signif_digits = 2, max_digits = 10)))
  expect_equal(output, expected_output)
})

options(default_opts)
