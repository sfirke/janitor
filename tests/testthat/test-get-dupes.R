# Tests for Excel date cleaning function

library(janitor)
context("duplicate identification")

library(dplyr)
test_df <- data.frame(a = c(1,3,3,3,5), b = c("a", "c","c", "e", "c"), stringsAsFactors = FALSE)

test_that("Correct combinations of duplicates are found", {
  expect_equal(get_dupes(test_df, a), data_frame(a = test_df[[1]][2:4], dupe_count = rep(3L, 3), b = test_df[[2]][2:4]))
  expect_equal(get_dupes(test_df, b), data_frame(b = test_df[[2]][c(2:3,5)], dupe_count = rep(3L, 3), a = test_df[[1]][c(2:3,5)]))
  })

test_that("calling with no specified variable names uses all variable names", {
  expect_equal(get_dupes(test_df), get_dupes(test_df, a, b))
  expect_message(get_dupes(mtcars), "No variable names specified - using all columns.")
})

no_dupes <- data.frame(a = 1, stringsAsFactors = FALSE)

test_that("instance of no dupes throws correct message, returns empty df", {
  expect_message(no_dupes %>% get_dupes(a), "No duplicate combinations found of: a")
  expect_equal(suppressWarnings(no_dupes %>% get_dupes(a)), data_frame(a = double(0), dupe_count = integer(0)))
})

test_that("incorrect variable names are handled", {
  expect_error(get_dupes(mtcars, x), "These variables do not match column names in mtcars: x")
})