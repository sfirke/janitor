# Tests for use_first_valid function

library(janitor)
library(dplyr)
context("use_first_valid_of()")

expect_equal_deprecated <- function(object, expected, ...) { # for retaining tests and not causing check() problems while the function is callable but deprecated
  expect_warning(object, "deprecated", ignore.case = TRUE)
  expect_equal(suppressWarnings(object), expected, ...)
}



dat <- data.frame(
  a = c(1, NA, NA),
  b = c(2, 2, NA),
  c = c(3, 3, 3),
  x = c("hi", "med", "lo"),
  d1 = c(as.Date("1999-01-01"), as.Date("1999-02-02"), NA),
  d2 = c(as.Date("2016-01-01"), as.Date("2016-02-02"), as.Date("2016-03-03")),
  stringsAsFactors = FALSE
)

dat$p1 <- as.POSIXct(dat$d1)
dat$p2 <- as.POSIXct(dat$d2)
p3 <- as.POSIXlt(dat$d1)
p4 <- as.POSIXlt(dat$d2)

test_that("mismatched classes of input vectors give warning", {
  expect_warning(use_first_valid_of(dat$c, dat$x), "Input vectors do not share a single class - all input vectors will be coerced to class `character`")
})

test_that("input vectors of different lengths throw error", {
  expect_error(suppressWarnings(use_first_valid_of(c(1, NA), c(NA, 3, NA))), "All input vectors must have the same length")
})

test_that("outputs are correct", {
  expect_equal_deprecated(use_first_valid_of(dat$a, dat$b, dat$c), c(1, 2, 3))
  expect_equal_deprecated(use_first_valid_of(dat$d1, dat$d2), c(as.Date("1999-01-01"), as.Date("1999-02-02"), as.Date("2016-03-03")))
  expect_equal(
    suppressWarnings(use_first_valid_of(dat$a, dat$x)), # mismatched types, supposed to throw warning
    c("1", "med", "lo")
  )
  expect_equal(
    suppressWarnings(use_first_valid_of(dat$a, dat$d1)),
    c("1", "1999-02-02", NA)
  ) # mismatched types, supposed to throw warning
})

test_that("factors are handled correctly", {
  x <- factor(c("a", "b", NA))
  y <- factor(c("c", "d", "e"))
  expect_equal(suppressWarnings(use_first_valid_of(x, y)), c("a", "b", "e")) # factors should throw warnings
})

test_that("if_all_NA works", {
  expect_equal_deprecated(use_first_valid_of(dat$a, dat$b, if_all_NA = 0), c(1, 2, 0))
  expect_error(suppressWarnings(use_first_valid_of(dat$a, dat$b, if_all_NA = "missing")), "class(if_all_NA) does not match class of resulting vector", fixed = TRUE)
})

test_that("works with piping", {
  expect_equal(
    suppressWarnings(use_first_valid_of(dat$a, dat$b, dat$c)),
    suppressWarnings(dat %>%
      mutate(new_var = use_first_valid_of(a, b, c)) %>%
      .$new_var)
  )
})

test_that("POSIXct outputs are correct and retain proper class", {
  expect_equal_deprecated(
    use_first_valid_of(dat$p1, dat$p2),
    c(dat$p1[1:2], dat$p2[3])
  )
  expect_equal_deprecated(
    use_first_valid_of(dat$p1, dat$p2) %>% class(),
    c("POSIXct", "POSIXt")
  )
})

test_that("POSIXlt throws correct error message, other lists are rejected", {
  expect_error(suppressWarnings(use_first_valid_of(p3, p4)), "At least one input vector is of class POSIXlt, which is a list; convert to POSIXct")
  k <- list("a", c(1, 2))
  expect_error(suppressWarnings(use_first_valid_of(k)), "At least one of the given inputs is a list; supply only vectors to this function")
})
