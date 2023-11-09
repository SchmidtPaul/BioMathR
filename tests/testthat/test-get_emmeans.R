library(emmeans)
library(dplyr)
library(testthat)
library(tibble)

# model is NULL
test_that("get_emmeans returns NULL if model is NULL", {
  expect_null(get_emmeans(NULL))
  expect_error(get_emmeans(NULL), NA)
})


# compare results to emmeans ----------------------------------------------
pigsmod <- lm(conc ~ source, data = pigs)
EM <- emmeans::emmeans(object = pigsmod, specs = "source", lmer.df = "Satterthwaite", infer = c(TRUE, FALSE))
BM <- get_emmeans(model = pigsmod, specs_string = "~ source", lmer.df = "Satterthwaite")

test_that("emmeans-table identical (Satterthwaite)", {
  expect_true(
    identical(
      EM %>% tibble::as_tibble() %>% as.matrix(),
      BM %>% `[[`(1) %>% `[`(1:6) %>% dplyr::rename("SE" = "SEM") %>% as.matrix()
    )
  )
})

test_that("diffs-table identical (t-test)", {
  expect_true(
    identical(
      EM %>% pairs(infer = c(TRUE, TRUE), adjust = "none") %>% tibble::as_tibble() %>% `[`(c(1:6, 8)) %>% as.matrix(),
      BM %>% `[[`(2) %>% `[`(1:7) %>% dplyr::rename("SE" = "SED") %>% as.matrix()
    )
  )
})


