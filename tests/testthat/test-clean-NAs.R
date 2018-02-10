# Tests for recoding miscoded string NA values to true NA

library(janitor)
context("recoding of string NA values into NAs")

test_df <- data.frame(
  v1 = c(1, NA, 3),
  v_fac = c("a", "a", "b")
)
test_df$v2 <- c("NA", "#NAME?", "n/a") # this one will be character, not factor


expect_equal_deprecated <- function(object, expected, ...) { # for retaining tests and not causing check() problems while the function is callable but deprecated
  expect_warning(object, "deprecated", ignore.case = TRUE)
  expect_equal(suppressWarnings(object), expected, ...)
}


test_that("values are converted to NAs in a single vector", {
  expect_equal_deprecated(convert_to_NA(c(1, 2), 1), c(NA, 2))
  expect_equal_deprecated(convert_to_NA(c(1, 2), c(1, 2)), as.numeric(c(NA, NA)))
  expect_equal_deprecated(convert_to_NA(c(1:4), 3:4), c(1:2, NA, NA))
  expect_equal_deprecated(convert_to_NA(c("hi", "there"), c("oh", "hi")), c(NA, "there")) # values that aren't present are okay
  expect_equal_deprecated(convert_to_NA(c("#N/A"), "#N/A"), as.character(NA)) # does nothing with NA input
})

cleaned_df <- suppressWarnings(convert_to_NA(test_df, c("NA", "#NAME?", "N/A", "n/a", 1, "a")))

test_that("values are converted to NAs at the data.frame level", {
  expect_equal(cleaned_df[[1]], c(NA, NA, 3))
  expect_equal(cleaned_df[[2]], factor(c(NA, NA, "b"), levels = c("a", "b")))
  expect_equivalent(cleaned_df[[3]], as.character(rep(NA, 3)))
})

# test bad inputs
test_that("1st argument rejects bad inputs", {
  expect_error(suppressWarnings(convert_to_NA(list("a"), 1)), "argument 'dat' must be a vector or data.frame")
})

test_that("2nd argument rejects bad inputs", {
  expect_error(suppressWarnings(convert_to_NA(test_df, list("1", "hi"))), "'strings' parameter should be a vector of class character, numeric, factor, or integer")
})

# non-replacement warning
test_that("non-replacement warning prints", {
  expect_warning(convert_to_NA(mtcars, "gfjklj"), "no replacements made")
  expect_warning(convert_to_NA(1, 2), "no replacements made")
})
