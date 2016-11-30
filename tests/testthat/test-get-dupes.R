# Tests for duplicate extraction function

library(janitor)
context("duplicate extraction")

library(dplyr)
test_df <- data.frame(a = c(1,3,3,3,5), b = c("a", "c","c", "e", "c"), stringsAsFactors = FALSE)

test_that("Correct combinations of duplicates are found", {
  expect_equal(nrow(get_dupes(test_df, a)), 3)
  expect_equal(nrow(get_dupes(test_df, a)), 3)
  expect_equal(nrow(get_dupes(test_df)), 2) 
})

no_dupes <- data.frame(a = 1:10, stringsAsFactors = FALSE)

test_that("instances of no dupes throw correct messages, return empty df", {
  expect_equal(no_dupes %>% get_dupes(a) %>% length, 0)
})

test_that("works on variables with irregular names", {
  badname_df <- mtcars %>% mutate(`bad name!` = mpg * 1000)
  expect_equal(badname_df %>% get_dupes(`bad name!`, cyl) %>% dim,
               c(10, 12)) # does it return the right-sized result?
  expect_is(badname_df %>% get_dupes(), "data.frame") # test for success, i.e., produces a data.frame (with 0 rows)
})

test_that("count_dupes example works", {
  df <- data.frame(id = c(1,2,3,3,3), stuff = c("a", "b", "c", "d", "d"))
  t <- df %>%
    get_dupes(., id) %>%
    left_join(count_dupes(., id))
  
  expect_false(check_dupes(df, id, level="logical"))
  expect_is(t, "data.frame")
  expect_equal(nrow(t), 3)
})
