test_that("add_group_column creates correct groups with group_by", {
  data <- data.frame(x = c(1,2,3,1,2,3), y = c("a","b","c","a","b","c"))
  result <- data %>% add_group_column(name = "group_id", group_by = c("x", "y"))

  expect_true("group_id" %in% names(result))
  expect_equal(nrow(result), nrow(data))
  expect_true(is.factor(result$group_id))
  expect_equal(length(unique(result$group_id)), 6)
})

test_that("add_group_column creates default factor when group_by is NULL", {
  data <- data.frame(x = c(1,2,3), y = c("a","b","c"))
  result <- data %>% add_group_column(name = "group_id", group_by = NULL)

  expect_true("group_id" %in% names(result))
  expect_equal(nrow(result), nrow(data))
  expect_true(is.factor(result$group_id))
  expect_equal(as.character(unique(result$group_id)), "-")
})

test_that("add_group_column creates numbered groups", {
  data <- data.frame(treatment = c("A","A","B","B"), block = c(1,2,1,2))
  result <- data %>% add_group_column(name = "grp", group_by = c("treatment", "block"))

  expect_true(all(grepl("^grp\\d{2}$", result$grp)))
})
