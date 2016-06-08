# Tests for removing fully-NA rows or columns

library(janitor)
context("remove empty rows or columns")

dat <- data.frame(a = c(NA, NA, 1),
                  b = c(NA, 1, NA),
                  c = c(NA, NA, NA))

test_that("empty rows are removed", {
  expect_equal(remove_empty_rows(dat), dat[2:3, ])
})

test_that("empty cols are removed", {
  expect_equal(remove_empty_cols(dat), dat[, 1:2])
})
