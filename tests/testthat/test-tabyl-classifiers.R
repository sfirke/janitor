# Tests tabyl class functions

library(janitor)
library(testthat)
context("as_tabyl() and un_tabyl()")

a <- mtcars %>%
  crosstab(cyl, carb) # will need to rewrite this when this verb goes away

test_that("as_tabyl works on result of a non-janitor count/spread", {
 b <- mtcars %>%
   dplyr::count(cyl, carb) %>%
   tidyr::spread(carb, n, fill = 0) %>%
   as.data.frame() # for comparison purposes, remove the tbl_df aspect
 
 expect_equal(as_tabyl(a),
              as_tabyl(b))
})

test_that("as_tabyl sets attributes correctly", {
  d <- as_tabyl(a)
  expect_equal(class(d), c(class(a), "tabyl"))
  expect_equal(attr(d, "core"), a)
  expect_equal(attr(d, "tabyl_type"), "two_way")
})

test_that("un_tabyl puts back to original form", {
  expect_equal(mtcars, un_tabyl(as_tabyl(mtcars)))
})

test_that("un_tabyl warns if called on non-tabyl", {
  expect_warning(un_tabyl(mtcars),
                 "un_tabyl\\(\\) called on a non-tabyl")
})

test_that("as_tabyl is okay with non-numeric columns", {
  e <- b %>%
    dplyr::mutate(extra = "val")
  expect_equal(attr(as_tabyl(e), "core"), e) # implied success of as_tabyl
})

test_that("as_tabyl fails if no numeric columns in 2:n", {
  bad <- data.frame(a = 1:2,
                    b = c("x", "y"))
  expect_error(as_tabyl(bad), "at least one one of columns 2:n must be of class numeric")
})