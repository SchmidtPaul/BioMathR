library(BioMathR)

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

  expect_equal(docx_tab(anova, asft = FALSE)[[1, "p value"]], "0.016*")
  expect_equal(docx_tab(rename(as.data.frame(anova), "test" = 5), asft = FALSE, pvalform = "test")[[1, "test"]], "0.016*")
})

test_that("flextable does not throw an error", {
  expect_no_error(tmp <- docx_tab(anova, asft = TRUE))
})

