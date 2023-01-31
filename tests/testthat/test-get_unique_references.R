test_that("Dawkins (2008) duplicate becomes Dawkins (2008a) and Dawkins (2008b)", {
  expect_equal(
    get_unique_references(c("Dawkins (2008)", "Dawkins (2008)", "Stephenson (2008)")),
    c("Dawkins (2008a)", "Dawkins (2008b)", "Stephenson (2008)"))
})
