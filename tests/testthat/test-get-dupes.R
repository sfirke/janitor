# Tests for duplicate functions

library(janitor)
context("duplicate identification")

library(dplyr)
test_df <- data.frame(a = c(1,3,3,3,5), 
                      b = c("a", "c","c", "e", "c"), 
                      c = c(NA, NA, NaN, NaN, "a"), stringsAsFactors = FALSE)

test_that("Correct combinations of duplicates are found", {
  expect_equal(get_dupes(test_df, a), data_frame(a = test_df[[1]][2:4], dupe_count = rep(3L, 3), b = test_df[[2]][2:4], c = test_df[[3]][2:4]))
  expect_equal(get_dupes(test_df, b), data_frame(b = test_df[[2]][c(2:3,5)], dupe_count = rep(3L, 3), a = test_df[[1]][c(2:3,5)], c = test_df[[3]][c(2:3,5)]))
  expect_equal(get_dupes(test_df, c), data_frame(b = test_df[[2]][c(1:4)], dupe_count = rep(2L, 4), a = test_df[[1]][c(1:4)], c =test_df[[3]][c(1:4)]))

  expect_message(check_unique(test_df, a), "Unique constraint is violated 2 times in test_df. Example rows: c(3, 4)", fixed=T)
  expect_message(check_unique(test_df, b), "Unique constraint is violated 2 times in test_df. Example rows: c(3, 5)\n", fixed=T)
  expect_message(check_unique(test_df, c), "Unique constraint is violated 2 times in test_df. Example rows: c(2, 4)\n", fixed=T)

  expect_error(assert_unique(test_df, a))
  expect_error(assert_unique(test_df, b))
  expect_error(assert_unique(test_df, c))
  
  expect_false(check_unique(test_df, a, b))
  expect_error(assert_unique(test_df, a, b))
  })

test_that("calling with no specified variable names uses all variable names", {
  expect_equal(get_dupes(test_df), get_dupes(test_df, a, b, c))
  expect_message(get_dupes(mtcars), "No variable names specified - using all columns.")
  expect_message(check_unique(iris), "Unique constraint is violated 1 times in iris. Example rows: c(143)", fixed=T)
})

no_dupes <- data.frame(a = 1, stringsAsFactors = FALSE)

test_that("instances of no dupes throw correct messages and return correct values", {
  expect_message(no_dupes %>% get_dupes(a), "No duplicate combinations found of: a")
  expect_equal(suppressWarnings(no_dupes %>% get_dupes(a)), data_frame(a = double(0), dupe_count = integer(0)))
  expect_message(mtcars %>% select(-1) %>% get_dupes(), "No duplicate combinations found of: cyl, disp, hp, drat, wt, qsec, vs, am, gear, carb")
  expect_message(mtcars %>% get_dupes(), "No duplicate combinations found of: mpg, cyl, disp, hp, drat, wt, qsec, vs, am, ... and 2 other variables")
  expect_true(check_unique(no_dupes, a))
  expect_equal(assert_unique(no_dupes, a), no_dupes)
})

test_that("incorrect variable names are handled", {
  expect_error(get_dupes(mtcars, x), "These variables do not match column names in mtcars: x")
  expect_error(check_unique(mtcars, x))
  expect_error(assert_unique(mtcars, x))
})

test_that("works on variables with irregular names", {
  badname_df <- mtcars %>% mutate(`bad name!` = mpg * 1000)
  expect_equal(badname_df %>% get_dupes(`bad name!`, cyl) %>% dim,
               c(10, 13)) # does it return the right-sized result?
  expect_is(badname_df %>% get_dupes(), "data.frame") # test for success, i.e., produces a data.frame (with 0 rows)
  expect_false(check_unique(badname_df, `bad name!`, cyl))
  expect_error(assert_unique(badname_df, `bad name!`, cyl))
})
