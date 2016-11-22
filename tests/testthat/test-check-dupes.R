# Tests for duplicate checking function

library(janitor)
context("duplicate identification")

library(dplyr)
dupe_df <- data.frame(a = c(1,3,3,3,5), b = c("a", "c","c", "e", "c"), stringsAsFactors = FALSE)

test_that("Simple duplicates are found", {
  expect_true(check_dupes(dupe_df, level="logical"))
  expect_error(check_dupes(dupe_df))
  expect_message(check_dupes(dupe_df, level="message"))
  expect_warning(check_dupes(dupe_df, level="warning"))
})

id_df <- data.frame(a = c(1,3,3,5), b = c("a", "c", "e", "c"), stringsAsFactors = FALSE)

test_that("Handes no duplicates", {
  expect_false(check_dupes(id_df, level="logical"))
  expect_silent(check_dupes(id_df))
  expect_silent(check_dupes(id_df, level="message"))
  expect_silent(check_dupes(id_df, level="warning"))
})

NA_df <- data.frame(a = c(1,3,3,5, NA, NA), b = c("a", "c", "e", "c", NA, NA), stringsAsFactors = FALSE)

test_that("NAs count as duplicates.", {
  expect_true(check_dupes(NA_df, level="logical"))
  expect_error(check_dupes(NA_df))
  expect_message(check_dupes(NA_df, level="message"))
  expect_warning(check_dupes(NA_df, level="warning"))
})

test_that("works on variables with irregular names", {
  test_df <- NA_df %>% rename(`bad name` = a)
  expect_true(check_dupes(test_df, `bad name`, level="logical"))
  expect_error(check_dupes(test_df,  `bad name`))
  expect_message(check_dupes(test_df,  `bad name`, level="message"))
  expect_warning(check_dupes(test_df,  `bad name`, level="warning"))
})

