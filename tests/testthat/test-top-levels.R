# Tests for top_levels function

library(janitor)
library(dplyr)
context("count_top_levels()")

fac <- factor(c("a", "b", "c", "d", "e", "f", "f"), levels = rev(letters[1:6]))
fac_odd_lvls <- factor(fac, levels = rev(letters[1:5]))

# more tests - group names and ordering - are in test-get-level-groups.R
test_that("values are correct", {
  expect_equal(count_top_levels(fac)[[3]], c(3/7, 2/7, 2/7)) # default n = 2, num_levels = 6
  expect_equal(count_top_levels(fac)[[2]], c(3, 2, 2))
  expect_equal(count_top_levels(fac, 3)[[3]], c(4/7, 3/7)) # n = 3, num_levels = 6
  expect_equal(count_top_levels(fac, 3)[[2]], c(4, 3))
  expect_equal(count_top_levels(fac_odd_lvls)[[2]], c(2, 1, 2)) # default n = 2, num_levels = 5
  expect_equal(count_top_levels(fac_odd_lvls)[[3]], c(0.4, 0.2, 0.4))
  expect_equal(count_top_levels(fac_odd_lvls, 1)[[2]], c(1, 3, 1)) # n = 1, num_levels = 5
  expect_equal(count_top_levels(fac_odd_lvls, 1)[[3]], c(0.2, 0.6, 0.2))
})


test_that("NA results are treated appropriately",{
  fac_na <- fac
  fac_na[7] <- NA
  expect_equal(count_top_levels(fac_na)[[2]], rep(2, 3))
  expect_equal(count_top_levels(fac_na, show_na = TRUE)[[2]], c(2, 2, 2, 1))
  expect_equal(count_top_levels(fac_na, show_na = TRUE)[[3]], c(2/7, 2/7, 2/7, 1/7))
  expect_equal(count_top_levels(fac_na, show_na = TRUE)[[4]], c(1/3, 1/3, 1/3, NA))
  })

test_that("default n parameter works", {
  expect_equal(count_top_levels(fac), count_top_levels(fac, 2))
})

test_that("missing levels are treated appropriately", {
  fac_missing_lvl <- fac
  fac_missing_lvl[2] <- NA
  expect_equal(count_top_levels(fac_missing_lvl)[[2]], c(3, 2, 1))
})

test_that("bad type inputs are handled", {
  expect_error(count_top_levels(c(0, 1), "factor_vec is not of type 'factor'"))
  expect_error(count_top_levels(c("hi", "lo"), "factor_vec is not of type 'factor'"))
  expect_error(count_top_levels(mtcars, "factor_vec is not of type 'factor'"))
})


test_that("sort works", {
  simpl <- factor(c("a", "b", "b", "c"))
  expect_equal(count_top_levels(simpl, 1, sort = TRUE)[[1]], factor(c("b", "a", "c")))
  expect_equal(count_top_levels(simpl, 1, sort = TRUE)[[2]], c(2, 1, 1))
})

test_that("bad n value is handled", {
  expect_error(count_top_levels(fac, 4))
  expect_error(count_top_levels(fac_odd_lvls, 3))
  expect_error(count_top_levels(fac, 0))
  expect_error(count_top_levels(factor(c("a", "b"))), "input factor variable must have at least 3 levels")
})

test_that("correct variable name assigned to first column of result", {
  expect_equal(names(count_top_levels(fac))[1], "fac")
})