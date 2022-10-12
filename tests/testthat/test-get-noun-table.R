
# In each of these .format_x tests we test the get_noun_table function and the
# corresponding helper too. For the receipt example we also check the a
# add_vocative = FALSE option works correctly.

test_that(
  ".format_standard example: receipt works as expected (and no vocative)", {

  english_noun <- "receipt"
  greek_noun <- "απόδειξη"

  actual <- get_noun_table(english_noun, greek_noun, add_vocative = TRUE)
  actual_no_vocative <- get_noun_table(english_noun, greek_noun)
  actual_helper <- .test_format_noun(english_noun, greek_noun,
                                     f = .format_standard)

  expected <- here::here() |>
    file.path("tests", "testthat", "data", "get-noun-table-receipt.rds") |>
    read_rds()

  expected_no_vocative <- mutate(expected,
                                 vocative_singular = "",
                                 vocative_plural = "")

  expect_equal(actual, expected)
  expect_equal(actual_helper, expected)
  expect_equal(actual_no_vocative, expected_no_vocative)

  # Don't want to slam wiktionary so adding these pauses throughout

  sleep(0.5)

})

test_that(".format_indeclineable example: rugby works as expected", {

  english_noun <- "rugby"
  greek_noun <- "ράγκμπι"

  actual <- get_noun_table(english_noun, greek_noun, add_vocative = TRUE)
  actual_helper <- .test_format_noun(english_noun, greek_noun,
                                     f = .format_indeclinable)

  expected <- here::here() |>
    file.path("tests", "testthat", "data", "get-noun-table-rugby.rds") |>
    read_rds()

  expect_equal(actual, expected)
  expect_equal(actual_helper, expected)

  sleep(0.5)

})


test_that(
  ".format_multi_gender example: engineer works as expected (and no neutral)", {

  english_noun <- "engineer"
  greek_noun <- "μηχανικός"

  # Test with and without the neutral option
  # mfn: masculine, femminine, netural

  actual_mfn <- get_noun_table(english_noun, greek_noun, add_vocative = TRUE,
                           drop_neutral = FALSE)

  actual_mf <-  get_noun_table(english_noun, greek_noun, add_vocative = TRUE)

  actual_mf_helper <- .test_format_noun(english_noun, greek_noun,
                                        f = .format_multi_gender)

  expected_mfn <- here::here() |>
    file.path("tests", "testthat", "data", "get-noun-table-engineer-mfn.rds") |>
    read_rds()

  expected_mf <- here::here() |>
    file.path("tests", "testthat", "data", "get-noun-table-engineer-mf.rds") |>
    read_rds()

  expect_equal(actual_mfn, expected_mfn)
  expect_equal(actual_mf_only, expected_mf)
  expect_equal(actual_mf_helper, expected_mf)

})

















test_that("add vocative = TRUE works as expected", {

  english_noun <- "receipt"
  greek_noun <-

  actual <- get_noun_table("receipt", "απόδειξη", add_vocative = TRUE)

  expected <- here::here() |>
    file.path("tests", "testthat", "data", "get-noun-table-receipt.rds") |>
    read_rds()

  expect_equal(actual_helper, expected)

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
  greek_noun <- "γη"

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-land-earth.rds")

  expected <- read_rds(filename)

  expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
                              expected))

})


test_that("get_noun_table works for top (clothing) example", {

  english_noun <- "top (clothing)"
  greek_noun <- "μπλούζα"

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-top-clothes.rds")

  expected <- read_rds(filename)

  expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
                              expected))

})




