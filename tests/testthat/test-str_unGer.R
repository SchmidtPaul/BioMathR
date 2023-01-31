test_that("Individual lowercase works", {
  expect_equal(str_unGer("ä, ö, ü"), "ae, oe, ue")
})

test_that("Individual uppercase works", {
  expect_equal(str_unGer("Ä, Ö, Ü"), "AE, OE, UE")
})

test_that("Sentence lowercase works", {
  expect_equal(str_unGer("Ich grüße öfter Löwen mit Mähne"), "Ich gruesse oefter Loewen mit Maehne")
})

test_that("Sentence uppercase works", {
  expect_equal(str_unGer("Änderung; Öfter, Über"), "Aenderung; Oefter, Ueber")
})
