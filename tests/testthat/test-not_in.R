test_that("Actually not in there", {
  expect_true(1 %not_in% 2)
  expect_true(1 %not_in% c(3,4))
  expect_true(all(c(1,2) %not_in% c(3,4)))
  expect_true(all(LETTERS %not_in% letters))
})

test_that("Actually in there", {
  expect_false(2 %not_in% 2)
  expect_false(3 %not_in% c(3,4))
  expect_false(all(c(3,4) %not_in% c(3,4)))
  expect_false(all(letters %not_in% letters))
})
