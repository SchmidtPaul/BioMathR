library(testthat)

# model is NULL
test_that("get_residual_plots returns NULL if model is NULL", {
  expect_null(get_residual_plots(NULL))
  expect_error(get_residual_plots(NULL), NA)
})

# lm test
test_that("get_residual_plots does NOT throw an error for lm class", {
  lm_model <- lm(mpg ~ wt + hp, data = mtcars)
  expect_error(get_residual_plots(lm_model), NA)
})

# lmerMod test
test_that("get_residual_plots does NOT throw an error for lmerMod class", {
  if (requireNamespace("lme4", quietly = TRUE)) {
    lmer_model <- lme4::lmer(mpg ~ wt + hp + (1 | cyl), data = mtcars)
    expect_error(get_residual_plots(lmer_model), NA)
  } else {
    skip()
  }
})

# lmerModLmerTest test
test_that("get_residual_plots does NOT throw an error for lmerModLmerTest class", {
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    lmerTest_model <- lmerTest::lmer(mpg ~ wt + hp + (1 | cyl), data = mtcars)
    expect_error(get_residual_plots(lmerTest_model), NA)
  } else {
    skip()
  }
})

# glmmTMB test
test_that("get_residual_plots does NOT throw an error for glmmTMB class", {
  if (suppressWarnings(requireNamespace("glmmTMB", quietly = TRUE))) {
    glmmTMB_model <- suppressWarnings(glmmTMB::glmmTMB(mpg ~ wt + hp + (1 | cyl), data = mtcars))
    expect_error(get_residual_plots(glmmTMB_model, NA))
  } else {
    skip("glmmTMB package not available")
  }
})


# gls test
test_that("get_residual_plots DOES throw an error for gls class", {
  if (requireNamespace("nlme", quietly = TRUE)) {
    gls_model <- nlme::gls(mpg ~ wt + hp, data = mtcars)
    expect_error(get_residual_plots(gls_model) %>% capture.output() %>% suppressWarnings())
  } else {
    skip()
  }
})

# lme test
test_that("get_residual_plots DOES throw an error for lme class", {
  if (requireNamespace("nlme", quietly = TRUE)) {
    lme_model <- nlme::lme(mpg ~ wt + hp, random = ~ 1 | cyl, data = mtcars)
    expect_error(get_residual_plots(lme_model) %>% capture.output() %>% suppressWarnings())
  } else {
    skip()
  }
})
