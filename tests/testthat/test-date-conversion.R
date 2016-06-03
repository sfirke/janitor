# Tests for Excel date cleaning function

library(janitor)
context("Excel date cleaning")

test_that("Serial number dates convert correctly", {
  expect_equal(excel_numeric_to_date(42370), as.Date("2016-01-01"))
  expect_equal(excel_numeric_to_date(42370, "modern"), as.Date("2016-01-01"))
  expect_equal(excel_numeric_to_date(40908, "mac pre-2011"), as.Date("2016-01-01"))
})

test_that("Bad inputs handled appropriately", {
  expect_error(excel_numeric_to_date("hello"), "argument `date_num` must be of class numeric")
  expect_error(excel_numeric_to_date(40908, "bad string"), "argument 'created' must be one of 'mac pre-2011' or 'modern'")
  expect_error(excel_numeric_to_date(40908, 4), "argument 'created' must be one of 'mac pre-2011' or 'modern'")
})