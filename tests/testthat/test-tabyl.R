# Tests for data.frame renaming function

library(janitor)
library(dplyr)
context("tabyl")

cyl_tbl <- tabyl(mtcars$cyl)
cyl_tbl_pipe <- mtcars %>% tabyl(cyl)


test_that("supplying a vector yields same results as passing a data frame w/ unquoted variable name", {
  expect_identical(cyl_tbl %>% setNames(c("cyl","n","percent")),
               cyl_tbl_pipe)
})

test_that("counts are accurate", {
  expect_equal(cyl_tbl_pipe$cyl, c(4, 6, 8))
  expect_equal(cyl_tbl_pipe$n, c(11, 7, 14))
})

test_that("percentages are accurate", {
  expect_equal(cyl_tbl_pipe$percent, c(11/32, 7/32, 14/32))
})

# Character input, with and without NA
test_df <- data.frame(grp = c("a", "b", "b", "c"), stringsAsFactors = FALSE)
test_df_na <- data.frame(grp = c("a", "b", "b", "c", NA), stringsAsFactors = FALSE)
test_res <- test_df %>% tabyl(grp)
test_res_na <- test_df_na %>% tabyl(grp)

test_that("names are right", {
  expect_equal(names(cyl_tbl_pipe), c("cyl", "n", "percent"))
  expect_equal(names(test_res_na), c("grp", "n", "percent", "valid_percent"))
})

test_that("NAs handled correctly", {
  expect_equal(test_res_na$percent, c(0.2, 0.4, 0.2, 0.2))
  expect_equal(test_res_na$valid_percent, c(0.25, 0.5, 0.25, NA))
})

test_that("show_NA parameter works", {
  expect_equal(test_res, test_df_na %>% tabyl(grp, show_na = FALSE))
})

test_that("sorting is preserved for factors", {
  expect_equal(tabyl(factor(c("x", "y", "z"), levels = c("y", "z", "x")))[[1]], factor(c("y", "z", "x"), levels = c("y", "z", "x")))
})

# missing factor levels shown, with and without NA
fac_df <- data.frame(x = factor(c("a"), levels = c("b", "a")))
fac <- factor(c("a"), levels = c("b", "a"))
fac_df_na <- data.frame(x = factor(c("a", NA), levels = c("b", "a")))
fac_na <- factor(c("a", NA), levels = c("b", "a"))


test_that("missing factor levels are displayed without NA values", {
  expect_equal(tabyl(fac)[[1]], factor(c("b","a"), levels = c("b", "a")))
  expect_equal(tabyl(fac)[[2]], c(NA, 1))
  expect_equal(tabyl(fac)[[3]], c(NA, 1))
  expect_equal((fac_df %>% tabyl(x))[[1]], factor(c("b","a"), levels = c("b", "a")))
  expect_equal((fac_df %>% tabyl(x))[[2]], c(NA, 1))
  expect_equal((fac_df %>% tabyl(x))[[3]], c(NA, 1))
})

test_that("missing factor levels are displayed with NA values", {
  expect_equal(tabyl(fac_na)[[1]], factor(c("b","a", NA), levels = c("b", "a")))
  expect_equal(tabyl(fac_na)[[2]], c(NA, 1, 1))
  expect_equal(tabyl(fac_na)[[3]], c(NA, 0.5, 0.5))
  expect_equal(tabyl(fac_na)[[4]], c(NA, 1, NA))
  expect_equal((fac_df_na %>% tabyl(x))[[1]], factor(c("b","a", NA), levels = c("b", "a")))
  expect_equal((fac_df_na %>% tabyl(x))[[2]], c(NA, 1, 1))
  expect_equal((fac_df_na %>% tabyl(x))[[3]], c(NA, 0.5, 0.5))
  expect_equal((fac_df_na %>% tabyl(x))[[4]], c(NA, 1, NA))
})
  
# check sort parameter
sorted_test_df_na <- test_df_na %>% tabyl(grp, sort = TRUE)

test_that("sort parameter works", {
  expect_equal(sorted_test_df_na[[1]], c("b", "a", "c", NA))
  expect_equal(sorted_test_df_na[[4]], c(0.5, 0.25, 0.25, NA))
})

# bad inputs

test_that("failure occurs if 0 or multiple variables passed as dots when calling on a data.frame", {
  expect_error(mtcars %>% tabyl(cyl, gear), "more than one variable name specified")
  expect_error(mtcars %>% tabyl(), "no variable name specified")
})
