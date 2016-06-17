# Tests for data.frame renaming function

library(janitor)
library(dplyr)
context("tabyl")

cyl_tbl <- tabyl(mtcars$cyl)

test_that("counts are accurate", {
  expect_equal(cyl_tbl$mtcars_cyl, c(4, 6, 8))
  expect_equal(cyl_tbl$n, c(11, 7, 14))
})

test_that("percentages are accurate", {
  expect_equal(cyl_tbl$percent, c(11/32, 7/32, 14/32))
})

# Character input, with and without NA
test_df <- data.frame(grp = c("a", "b", "b", "c"), stringsAsFactors = FALSE)
test_df_na <- data.frame(grp = c("a", "b", "b", "c", NA), stringsAsFactors = FALSE)
test_res <- tabyl(test_df$grp)
test_res_na <- tabyl(test_df_na$grp)

test_that("names are right", {
  expect_equal(names(cyl_tbl), c("mtcars_cyl", "n", "percent"))
  expect_equal(names(test_res_na), c("test_df_na_grp", "n", "percent", "valid_percent"))
})

test_that("NAs handled correctly", {
  expect_equal(test_res_na$percent, c(0.2, 0.4, 0.2, 0.2))
  expect_equal(test_res_na$valid_percent, c(0.25, 0.5, 0.25, NA))
})

test_that("show_NA = FALSE parameter works", {
  expect_equal(test_res %>%
                 stats::setNames(c("test_df_na_grp", names(test_res)[-1])),
               tabyl(test_df_na$grp, show_na = FALSE))
})

test_that("sorting is preserved for factors", {
  expect_equal(tabyl(factor(c("x", "y", "z"), levels = c("y", "z", "x")))[[1]], factor(c("y", "z", "x"), levels = c("y", "z", "x")))
})

# missing factor levels shown, with and without NA
fac <- factor(c("a"), levels = c("b", "a"))
fac_na <- factor(c("a", NA), levels = c("b", "a"))


test_that("missing factor levels are displayed without NA values", {
  expect_equal(tabyl(fac)[[1]], factor(c("b","a"), levels = c("b", "a")))
  expect_equal(tabyl(fac)[[2]], c(NA, 1))
  expect_equal(tabyl(fac)[[3]], c(NA, 1))
})

test_that("missing factor levels are displayed with NA values", {
  expect_equal(tabyl(fac_na)[[1]], factor(c("b","a", NA), levels = c("b", "a")))
  expect_equal(tabyl(fac_na)[[2]], c(NA, 1, 1))
  expect_equal(tabyl(fac_na)[[3]], c(NA, 0.5, 0.5))
  expect_equal(tabyl(fac_na)[[4]], c(NA, 1, NA))
})
  
# check sort parameter
sorted_test_df_na <- tabyl(test_df_na$grp, sort = TRUE)
sorted_with_fac <- data.frame(grp = factor(c("a", "c", "c"), levels = letters[1:3]))
sorted_with_fac <- tabyl(sorted_with_fac$grp, sort = TRUE)

sorted_with_na_and_fac <- data.frame(grp = factor(c("a", "c", "c", NA), levels = letters[1:3]))
sorted_with_na_and_fac <- tabyl(sorted_with_na_and_fac$grp, sort = TRUE)

test_that("sort parameter works", {
  expect_equal(sorted_test_df_na[[1]], c("b", "a", "c", NA))
  expect_equal(sorted_test_df_na[[4]], c(0.5, 0.25, 0.25, NA))
  expect_equal(sorted_with_fac[[1]], factor(c("c", "a", "b"), levels = letters[1:3]))
  expect_equal(sorted_with_fac[[2]], c(2, 1, NA))
  expect_equal(sorted_with_na_and_fac[[1]], factor(c("c", "a", "b", NA), levels = letters[1:3]))
  expect_equal(sorted_with_na_and_fac[[2]], c(2, 1, NA, 1))
})

# bad inputs

test_that("failure occurs when passed a list", {
  expect_error(tabyl(list(1, 2)), "input must be a logical, numeric, or character vector")
})

test_that("a piped name of dot turns into x", {
  expect_equal(mtcars %>% .$gear %>% tabyl %>% names(.) %>% .[1], "x")
})
