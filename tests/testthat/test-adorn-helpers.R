# Tests functions called by adorn_crosstab()

library(janitor)
context("adorn helper functions")

library(dplyr)

test_that("check_all_numeric function is accurate", {
  expect_silent(check_all_cols_after_first_are_numeric(data.frame(a = 1:2, b = 1:2, c = 1:2)))
  expect_silent(check_all_cols_after_first_are_numeric(data.frame(a = c("a", "b"), b = 1:2, c = 1:2)))
  expect_error(check_all_cols_after_first_are_numeric(data.frame(a = 1:2, b = c("a", "b"), c = 1:2)),
               "all columns after the first one must be numeric")
  expect_error(check_all_cols_after_first_are_numeric(data.frame(a = 1:2, b = 1:2, c = c("a", "b"),
                                                                 stringsAsFactors = FALSE)),
               "all columns after the first one must be numeric")
})
