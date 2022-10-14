
# In each of these .format_x tests we test the get_noun_table function and the
# corresponding helper too. For the receipt example we also check the
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

  Sys.sleep(0.25)

})

# In this example the first table is the adjective table so we don't want to
# grab that

test_that(".format_standard example: engineer works as expected", {

  english_noun <- "engineer"
  greek_noun <- "μηχανικός"

  actual <-  get_noun_table(english_noun, greek_noun, add_vocative = TRUE)

  actual_helper <- .test_format_noun(english_noun, greek_noun,
                                     f = .format_standard)

  expected <- here::here() |>
    file.path("tests", "testthat", "data", "get-noun-table-engineer.rds") |>
    read_rds()

  expect_equal(actual, expected)
  expect_equal(actual_helper, expected)

  Sys.sleep(0.25)

})

# In this example, the singular and plural genitive return 2 alternatives by
# default but we only want the first one

test_that(".format_standard example: sheep works as expected", {

  english_noun <- "sheep"
  greek_noun <- "πρόβατο"

  actual <-  get_noun_table(english_noun, greek_noun, add_vocative = TRUE)

  actual_helper <- .test_format_noun(english_noun, greek_noun,
                                     f = .format_standard)

  expected <- here::here() |>
    file.path("tests", "testthat", "data", "get-noun-table-sheep.rds") |>
    read_rds()

  expect_equal(actual, expected)
  expect_equal(actual_helper, expected)

  Sys.sleep(0.25)

})


# In this example it can be either masculine or feminine and the singular
# genitive is repeated

test_that(".format_standard example: writer works as expected", {

  english_noun <- "writer"
  greek_noun <- "συγγραφέας"

  actual <-  get_noun_table(english_noun, greek_noun, add_vocative = TRUE)

  actual_helper <- .test_format_noun(english_noun, greek_noun,
                                     f = .format_standard)

  expected <- here::here() |>
    file.path("tests", "testthat", "data", "get-noun-table-sheep.rds") |>
    read_rds()

  expect_equal(actual, expected)
  expect_equal(actual_helper, expected)

  Sys.sleep(0.25)

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

  Sys.sleep(0.25)

})




















# test_that("add vocative = TRUE works as expected", {
#
#   english_noun <- "receipt"
#   greek_noun <-
#
#   actual <- get_noun_table("receipt", "απόδειξη", add_vocative = TRUE)
#
#   expected <- here::here() |>
#     file.path("tests", "testthat", "data", "get-noun-table-receipt.rds") |>
#     read_rds()
#
#   expect_equal(actual_helper, expected)
#
# })
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# test_that("get_noun_table works for arrow example", {
#
#   english_noun <- "arrow"
#   greek_noun <- "βέλος"
#
#   actual <- get_noun_table(english_noun, greek_noun)
#
#   filename <- file.path(here::here(), "tests", "testthat", "data",
#                         "get-noun-table-arrow.rds")
#
#   expected <- read_rds(filename)
#
#   expect_equal(actual, expected)
#
# })
#
#
#
#
#
#
#
# test_that("get_noun_table works for engineer example", {
#
#   english_noun <- "engineer"
#   greek_noun <- "μηχανικός"
#
#   filename <- file.path(here::here(), "tests", "testthat", "data",
#                         "get-noun-table-engineer.rds")
#
#   expected <- read_rds(filename)
#
#   expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
#                               expected))
#
# })
#
# test_that("get_noun_table works for land example", {
#
#   english_noun <- "land"
#   greek_noun <- "γη"
#
#   filename <- file.path(here::here(), "tests", "testthat", "data",
#                         "get-noun-table-land-earth.rds")
#
#   expected <- read_rds(filename)
#
#   expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
#                               expected))
#
# })
#
#
# test_that("get_noun_table works for top (clothing) example", {
#
#   english_noun <- "top (clothing)"
#   greek_noun <- "μπλούζα"
#
#   filename <- file.path(here::here(), "tests", "testthat", "data",
#                         "get-noun-table-top-clothes.rds")
#
#   expected <- read_rds(filename)
#
#   expect_warning(expect_equal(get_noun_table(english_noun, greek_noun),
#                               expected))
#
# })




