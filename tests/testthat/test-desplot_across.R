library(testthat)

test_that("desplot_across throws no warnings with special characters", {
  dat <- agridat::yates.oats
  dat$gen <- paste0(dat$gen, "äöüß")

  expect_silent(
    desplot_across(
      data = dat,
      vars = c("gen", "block"),
      cex = 1
    )
  )
})


test_that("desplot_across returns a list of desplots", {
  data <- agridat::yates.oats
  vars <- c("nitro", "gen", "block")
  result <- desplot_across(data = data, vars = vars)
  expect_type(result, "list")
  expect_length(result, length(vars))
  expect_s3_class(result[[1]], "trellis")
})

test_that("desplot_across throws an error with invalid data input", {
  expect_error(desplot_across(data = NULL, vars = c("nitro", "gen", "block")))
})

test_that("desplot_across throws an error for non-existing vars", {
  data <- agridat::yates.oats
  vars <- c("non_existing_var")
  expect_error(desplot_across(data = data, vars = vars))
})

test_that("desplot_across works with different data types", {
  data <- agridat::yates.oats
  vars <- c("nitro", "gen")
  result <- desplot_across(data = data, vars = vars)
  expect_s3_class(result$nitro, "trellis")
  expect_s3_class(result$gen, "trellis")
})

test_that("desplot_across responds to different parameters correctly", {
  data <- agridat::yates.oats
  vars <- c("nitro")
  result_eng <- desplot_across(data = data, vars = vars, lang = "eng")
  result_ger <- desplot_across(data = data, vars = vars, lang = "ger")
  expect_false(identical(result_eng, result_ger))
})

test_that("desplot_across handles edge cases", {
  data <- agridat::yates.oats[0, ]
  vars <- c("nitro", "gen", "block")
  expect_error(desplot_across(data = data, vars = vars) %>% suppressWarnings())
})

# TODO:
# empty_vars <- c()
# expect_error(desplot_across(data = agridat::yates.oats, vars = empty_vars))
