library(testthat)

# model is NULL
test_that("get_varcomp returns NULL if model is NULL", {
  expect_null(get_varcomp(NULL))
  expect_error(get_varcomp(NULL), NA)
})

# lm test
test_that("get_varcomp does not throw an error for lm class", {
  lm_model <- lm(mpg ~ wt + hp, data = mtcars)
  expect_error(get_varcomp(lm_model), NA)
})

# lmerTest test
test_that("get_varcomp does not throw an error for lmerTest class", {
  if (requireNamespace("lmerTest", quietly = TRUE)) {
    lmerTest_model <- lmerTest::lmer(mpg ~ wt + hp + (1 | cyl), data = mtcars)
    expect_error(get_varcomp(lmerTest_model), NA)
  } else {
    skip()
  }
})

# glmmTMB test
test_that("get_varcomp does not throw an error for glmmTMB class", {
  if (suppressWarnings(requireNamespace("glmmTMB", quietly = TRUE))) {
    glmmTMB_model <- suppressWarnings(glmmTMB::glmmTMB(mpg ~ wt + hp + (1 | cyl), data = mtcars, REML = TRUE))
    expect_error(get_varcomp(glmmTMB_model), NA)
  } else {
    skip()
  }
})

# Column names test
test_that("get_varcomp returns consistent column names", {
  lm_model <- lm(mpg ~ wt + hp, data = mtcars)
  vc <- get_varcomp(lm_model)

  # Should always return these column names (programmatic, not pretty)
  expect_true("group" %in% names(vc))
  expect_true("var" %in% names(vc))
  expect_true("sd" %in% names(vc))
  expect_true("var_p" %in% names(vc))
  expect_true("var_prop" %in% names(vc))
})

# Integration with docx_tab test
test_that("get_varcomp works with docx_tab for formatting", {
  if (requireNamespace("BioMathR", quietly = TRUE)) {
    lm_model <- lm(mpg ~ wt + hp, data = mtcars)

    # Should work without error
    expect_error(get_varcomp(lm_model) %>% BioMathR::docx_tab(asft = FALSE), NA)
  } else {
    skip("BioMathR not available")
  }
})
