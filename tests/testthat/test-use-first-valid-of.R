# Tests for use_first_valid function

library(janitor)
library(dplyr)
context("use_first_valid_of()")

dat <- data.frame(
  a = c(1, NA, NA),
  b = c(2, 2, NA),
  c = c(3, 3, 3),
  x = c("hi", "med", "lo"),
  d1 = c(as.Date("1999-01-01"), as.Date("1999-02-02"), NA),
  d2 = c(as.Date("2016-01-01"), as.Date("2016-02-02"), as.Date("2016-03-03")),
  stringsAsFactors = FALSE
)

test_that("mismatched classes of input vectors give warning", {
  expect_warning(use_first_valid_of(dat$c, dat$x), "Input vectors do not share a single class - all input vectors will be coerced to class `character`")
})

test_that("input vectors of different lengths throw error", {
  expect_error(use_first_valid_of(c(1, NA), c(NA, 3, NA)), "All input vectors must have the same length")
})

test_that("outputs are correct", {
  expect_equal(use_first_valid_of(dat$a, dat$b, dat$c), c(1, 2, 3))
  expect_equal(use_first_valid_of(dat$d1, dat$d2), c(as.Date("1999-01-01"), as.Date("1999-02-02"), as.Date("2016-03-03")))
  expect_equal(suppressWarnings(use_first_valid_of(dat$a, dat$x)), # mismatched types, supposed to throw warning
               c("1", "med", "lo"))
  expect_equal(suppressWarnings(use_first_valid_of(dat$a, dat$d1)),
               c("1", "1999-02-02", NA)) # mismatched types, supposed to throw warning
})

test_that("if_all_NA works", {
  expect_equal(use_first_valid_of(dat$a, dat$b, if_all_NA = 0), c(1, 2, 0))
  expect_error(use_first_valid_of(dat$a, dat$b, if_all_NA = "missing"), "class(if_all_NA) does not match class of resulting vector", fixed = TRUE)
})

test_that("works with piping", {
  expect_equal(use_first_valid_of(dat$a, dat$b, dat$c),
               dat %>%
                 mutate(new_var = use_first_valid_of(a, b, c)) %>%
                 .$new_var)
})

