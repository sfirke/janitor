# Tests for crosstab function

library(janitor)
library(dplyr)
context("crosstab()")

dat <- data.frame(
  v1 = factor(c("hi", "lo", "lo", "med", "hi", "med", "med", "med", NA), levels = c("hi", "med", "lo")),
  v2 = c(2, 3, 4, 3, 3, 3, 3, 3, 2),
  v3 = c("a", "a", "a", "b", "b", "b", "a", NA, NA),
  v4 = rep(c("x", "y", "z"), 3),
  stringsAsFactors = FALSE
)

test_that("bad inputs are handled properly", {
  expect_error(crosstab(list(1, 2), dat$v1), "vec1 must be a vector of type logical, numeric, character, or factor")
  expect_error(crosstab(dat$v1, list(1, 2)), "vec2 must be a vector of type logical, numeric, character, or factor")
  })

# simple crosstab w/o NAs
res <- crosstab(dat$v2, dat$v4)


test_that("result column names are correct", {
  expect_equal(names(res), c("dat$v2", "x", "y", "z"))
})

test_that("counts are correct", {
  expect_equal(res[[1]], 2:4)
  expect_equal(res[[2]], c(1, 2, NA))
  expect_equal(res[[3]], c(NA, 3, NA))
  expect_equal(res[[4]], c(1, 1, 1))
})

test_that("percentages are correct", {
  res_row <- crosstab(dat$v2, dat$v4, "row")
  expect_equal(res_row[[2]], c(0.5, 1/3, NA))
  expect_equal(res_row[[3]], c(NA, 0.5, NA))
  expect_equal(res_row[[4]], c(0.5, 1/6, 1))
  
  res_col <- crosstab(dat$v2, dat$v4, "col")
  expect_equal(res_col[[2]], c(1/3, 2/3, NA))
  expect_equal(res_col[[3]], c(NA, 1, NA))
  expect_equal(res_col[[4]], c(1/3, 1/3, 1/3))
  
  res_all <- crosstab(dat$v2, dat$v4, "all")
  expect_equal(res_all[, 2:4],tbl_df(res[, 2:4]/9))
})

test_that("NAs display correctly", {
  
})
