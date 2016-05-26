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

# check sort parameter
sorted_test_df_na <- test_df_na %>% tabyl(grp, sort = TRUE)

test_that("sort parameter works", {
  expect_equal(sorted_test_df_na[[1]], c("b", "a", "c", NA))
  expect_equal(sorted_test_df_na[[4]], c(0.5, 0.25, 0.25, NA))
})
