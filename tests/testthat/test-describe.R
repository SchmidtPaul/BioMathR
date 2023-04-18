library(BioMathR)
library(dplyr)
library(tibble)

df <- PlantGrowth %>%
  mutate(
    group2 = as.factor(rep(c("A", "A", "B",  "B", "B"), 6)),
    weight2 = weight * 2,
    weight3 = rep(c(3, NA), 15)
  )

test_that("multiple groups & multiple yvars", {
  expect_no_error(res <- df %>%
                    group_by(group, group2) %>%
                    BioMathR::describe(c("weight", "weight2", "weight3")))

  expect_equal(names(res)[1:3], c("Variable", "group", "group2"))
  expect_equal(unique(res$Variable), c("weight", "weight2", "weight3"))
  expect_equal(nrow(res), 18)
})

test_that("column names 'group' & 'name'", {
  expect_no_error(
    df %>%
      rename(name = group, value = weight) %>%
      group_by(name) %>%
      BioMathR::describe("value")
  )
})

test_that("German", {
  expect_no_error(res <- df %>%
                    group_by(group) %>%
                    BioMathR::describe("weight", lang = "ger"))

  expect_equal(names(res)[4:7],  c("Fehl", "MW", "StdAbw", "IQA"))
})

test_that("no grouping", {
  expect_no_error(res <- df %>%
                    BioMathR::describe("weight"))

  expect_equal(nrow(res), 1)
})
