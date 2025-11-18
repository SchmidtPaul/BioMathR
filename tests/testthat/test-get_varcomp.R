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

# German language tests
test_that("get_varcomp supports German language", {
  lm_model <- lm(mpg ~ wt + hp, data = mtcars)
  vc_ger <- get_varcomp(lm_model, lang = "ger")

  expect_true("Gruppe" %in% names(vc_ger))
  expect_true("Varianz" %in% names(vc_ger))
  expect_true("Standardabweichung" %in% names(vc_ger))
})

# Flextable output tests
test_that("get_varcomp can output as flextable", {
  if (requireNamespace("flextable", quietly = TRUE)) {
    lm_model <- lm(mpg ~ wt + hp, data = mtcars)
    vc_ft <- get_varcomp(lm_model, asft = TRUE)

    expect_equal(class(vc_ft), "flextable")
  } else {
    skip("flextable not available")
  }
})

# Combined German + flextable test
test_that("get_varcomp supports German language with flextable", {
  if (requireNamespace("lmerTest", quietly = TRUE) && requireNamespace("flextable", quietly = TRUE)) {
    lmerTest_model <- lmerTest::lmer(mpg ~ wt + hp + (1 | cyl), data = mtcars)
    vc_ger_ft <- get_varcomp(lmerTest_model, lang = "ger", asft = TRUE)

    expect_equal(class(vc_ger_ft), "flextable")
  } else {
    skip("lmerTest or flextable not available")
  }
})
