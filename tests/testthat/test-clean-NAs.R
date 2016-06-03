# Tests for recoding miscoded string NA values to true NA

library(janitor)
context("recoding of string NA values into NAs")

test_df <- data.frame(v1 = c(1, NA),
                      v2 = c("NA", "#NAME?"),
                      v3 = c("#N/A", "test")
)
test_df$v4 <- c("N/A", "n/a") # this one will be character, not factor

clean_df <- clean_NA_variants(test_df)
clean_custom <- clean_NA_variants(test_df, "1")
clean_custom_all <- clean_NA_variants(test_df, c(1, "test"))

test_that("values are converted to NAs in a single vector", {
  expect_equal(clean_NA_vec(c(1,2), 1), c(NA, 2))
  expect_equal(clean_NA_vec(c(1,2), c(1,2)), as.numeric(c(NA, NA)))
  expect_equal(clean_NA_vec(c(1:4), 3:4), c(1:2, NA, NA))
  expect_equal(clean_NA_vec(c("hi", "there"), c("oh", "hi")), c(NA, "there"))
  expect_equal(clean_NA_vec(c("#N/A"), "#N/A"), as.character(NA))
})

test_that("default values are cleaned at the data.frame level", {
  expect_equal(clean_df[[1]], c(1, NA))  
  expect_equivalent(clean_df[[2]], as.factor(c(NA, NA)))
  expect_equivalent(clean_df[[3]], as.factor(c(NA, "test")))
  expect_equal(clean_df[[4]], as.character(c(NA, NA)))
})

test_that("custom cleaning values are cleaned with clean_NA_variants", {
  expect_true(is.na(clean_custom[1,1]))  
  expect_false(is.na(clean_custom[2,3]))
  expect_equal(sum(is.na(clean_custom_all)), 8) # all values should convert to NA
})

test_that("2nd argument rejects non-string vectors", {
  expect_error(clean_NA_variants(test_df, 1), "addl_strings parameter should be a character vector, if specified")
  expect_error(clean_NA_variants(test_df, as.factor("hi")), "addl_strings parameter should be a character vector, if specified")
  expect_error(clean_NA_variants(test_df, list("1", "hi")), "addl_strings parameter should be a character vector, if specified")
})