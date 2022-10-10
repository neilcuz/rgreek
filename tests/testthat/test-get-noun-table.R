
test_that("get_noun_table works for receipt example", {

  english_noun <- "receipt"
  greek_noun <- "απόδειξη"

  actual <- get_noun_table(english_noun, greek_noun)

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-receipt.rds")
  expected <- read_rds(filename)

  expect_equal(actual, expected)

})


test_that("get_noun_table works for arrow example", {

  english_noun <- "arrow"
  greek_noun <- "βέλος"

  actual <- get_noun_table(english_noun, greek_noun)

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-arrow.rds")

  expected <- read_rds(filename)

  expect_equal(actual, expected)

})

test_that("get_noun_table works for rugby example", {

  english_noun <- "rugby"
  greek_noun <- "ράγκμπι"

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-rugby.rds")

  expected <- read_rds(filename)

  expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
                              expected))

})

test_that("get_noun_table works for engineer example", {

  english_noun <- "engineer"
  greek_noun <- "μηχανικός"

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-engineer.rds")

  expected <- read_rds(filename)

  expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
                              expected))

})

test_that("get_noun_table works for land example", {

  english_noun <- "land"
  greek_noun <- "μηχανικός"

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-land-earth.rds")

  expected <- read_rds(filename)

  expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
                              expected))

})


test_that("get_noun_table works for top (clothes) example", {

  english_noun <- "land"
  greek_noun <- "γη"

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-top-clothes.rds")

  expected <- read_rds(filename)

  expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
                              expected))

})




