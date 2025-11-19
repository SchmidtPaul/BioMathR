test_that("join_anovas combines multiple ANOVA tables", {
  # Create simple linear models
  model1 <- lm(mpg ~ wt + hp, data = mtcars)
  model2 <- lm(mpg ~ wt + cyl, data = mtcars)

  # Get ANOVA tables
  anova1 <- anova(model1)
  anova2 <- anova(model2)

  # Join them
  result <- join_anovas(
    anova_list = list(anova1, anova2),
    anova_names = c("Model1", "Model2")
  )

  # Check structure
  expect_true(is.data.frame(result))
  expect_true("Term" %in% names(result))
  expect_true("Model1" %in% names(result))
  expect_true("Model2" %in% names(result))

  # Check that residuals and intercept are filtered out
  expect_false("Residuals" %in% result$Term)
  expect_false("(Intercept)" %in% result$Term)
})

test_that("join_anovas handles NULL elements", {
  model1 <- lm(mpg ~ wt, data = mtcars)
  anova1 <- anova(model1)

  result <- join_anovas(
    anova_list = list(anova1, NULL),
    anova_names = c("Model1", "Model2")
  )

  expect_true(is.data.frame(result))
  expect_true("Model1" %in% names(result))
  expect_true("Model2" %in% names(result))
})

test_that("join_anovas formats p-values", {
  model1 <- lm(mpg ~ wt + hp, data = mtcars)
  anova1 <- anova(model1)

  result <- join_anovas(
    anova_list = list(anova1),
    anova_names = c("Model1")
  )

  # P-values should be formatted as strings
  expect_true(is.character(result$Model1))
})
