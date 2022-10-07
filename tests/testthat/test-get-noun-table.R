
test_that("get_noun_table works for receipt example", {

  actual <- get_noun_table("receipt", "απόδειξη")

  filename <- file.path(here::here(), "tests", "testthat", "data",
                        "get-noun-table-receipt.rds")
  expected <- read_rds(filename)

  expect_equal(actual, expected)

})
