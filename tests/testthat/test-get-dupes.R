# Tests for Excel date cleaning function

library(janitor)
context("Duplicate identification")

test_df <- data.frame(a = c(1,3,3,3,5), b = c("a", "c","c", "e", "c"))
test_that("Correct combinations of duplicates are found", {
  expect_equal(excel_numeric_to_date(42370), as.Date("2016-01-01"))
  expect_equal(excel_numeric_to_date(42370, "modern"), as.Date("2016-01-01"))
  expect_equal(excel_numeric_to_date(40908, "mac pre-2011"), as.Date("2016-01-01"))
})