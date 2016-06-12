# Tests for crosstab function

library(janitor)
library(dplyr)
context("crosstab()")

dat <- data.frame(
  v1 = factor(c("hi", "lo", "lo", "med", "hi", "med", "med", "med", NA), levels = c("hi", "med", "lo")),
  v2 = c(2, 3, 4, 3, 3, 3, 3, 3, 2),
  v3 = c("a", "a", "a", "b", "b", "b", "a", NA, NA),
  stringsAsFactors = FALSE
)

test_that("bad inputs are handled properly", {
  expect_error(crosstab(list(1, 2), dat$v1), "vec1 must be a vector of type logical, numeric, character, or factor")
  expect_error(crosstab(dat$v1, list(1, 2)), "vec2 must be a vector of type logical, numeric, character, or factor")
  })

