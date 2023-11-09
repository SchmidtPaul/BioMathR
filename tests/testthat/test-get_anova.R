library(testthat)

# model is NULL
test_that("get_anova returns NULL if model is NULL", {
  expect_null(get_anova(NULL))
  expect_error(get_anova(NULL), NA)
})

# lm test
test_that("get_anova does not throw an error for lm class", {
  lm_model <- lm(mpg ~ wt + hp, data = mtcars)
  expect_error(get_anova(lm_model), NA)
})

# lmerTest test
test_that("get_anova does not throw an error for lmerTest class", {
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    lmerTest_model <- lmerTest::lmer(mpg ~ wt + hp + (1 | cyl), data = mtcars)
    expect_error(get_anova(lmerTest_model), NA)
  } else {
    skip()
  }
})

# glmmTMB test
test_that("get_anova does not throw an error for glmmTMB class", {
  if (suppressWarnings(requireNamespace("glmmTMB", quietly = TRUE))) {
    glmmTMB_model <- suppressWarnings(glmmTMB::glmmTMB(mpg ~ wt + hp + (1 | cyl), data = mtcars, REML = TRUE))
    expect_error(get_anova(glmmTMB_model), NA)
  } else {
    skip()
  }
})

# gls test
test_that("get_anova does not throw an error for gls class", {
  if (requireNamespace("nlme", quietly = TRUE)) {
    gls_model <- nlme::gls(mpg ~ wt + hp, data = mtcars)
    expect_error(get_anova(gls_model), NA)
  } else {
    skip()
  }
})

# lme test
test_that("get_anova does not throw an error for lme class", {
  if (requireNamespace("nlme", quietly = TRUE)) {
    lme_model <- nlme::lme(mpg ~ wt + hp, random = ~ 1 | cyl, data = mtcars)
    expect_error(get_anova(lme_model), NA)
  } else {
    skip()
  }
})
