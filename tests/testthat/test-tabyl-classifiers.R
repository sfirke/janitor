# Tests tabyl class functions

library(janitor)
library(testthat)
context("as_tabyl() and untabyl()")

a <- mtcars %>%
  tabyl(cyl, carb)

b <- mtcars %>%
  dplyr::count(cyl, carb) %>%
  tidyr::spread(carb, n, fill = 0) %>%
  as.data.frame() # for comparison purposes, remove the tbl_df aspect


test_that("as_tabyl works on result of a non-janitor count/spread", {
  expect_equal(
    as_tabyl(a),
    as_tabyl(b, 2, "cyl", "carb")
  )
})

test_that("as_tabyl sets attributes correctly", {
  d <- as_tabyl(a)
  expect_equal(class(d), class(a))
  expect_equal(attr(d, "core"), untabyl(a))
  expect_equal(attr(d, "tabyl_type"), "two_way")
})

test_that("untabyl puts back to original form", {
  expect_equal(mtcars, untabyl(as_tabyl(mtcars)))
})

test_that("untabyl warns if called on non-tabyl", {
  expect_warning(
    untabyl(mtcars),
    "untabyl\\(\\) called on a non-tabyl"
  )
})

test_that("untabyl automatically invokes purrr::map when called on a 3-way tabyl and strips top-level attrs", {
  three <- tabyl(mtcars, cyl, am, gear)
  manual_untabyled <- purrr::map(three, untabyl)
  attr(manual_untabyled, "var_names") <- NULL
  attr(manual_untabyled, "tabyl_type") <- NULL
  expect_equal(
    untabyl(three), # vanilla call
    manual_untabyled
  )
})

test_that("as_tabyl is okay with non-numeric columns", {
  e <- b %>%
    dplyr::mutate(extra = "val")
  expect_equal(attr(as_tabyl(e), "core"), e) # implied success of as_tabyl
})

test_that("as_tabyl fails if no numeric columns in 2:n", {
  bad <- data.frame(
    a = 1:2,
    b = c("x", "y")
  )
  expect_error(as_tabyl(bad), "at least one one of columns 2:n must be of class numeric")
})

test_that("bad inputs are caught", {
  expect_error(as_tabyl(mtcars, 3),
    "axes must be either 1 or 2",
    fixed = TRUE
  )

  expect_error(as_tabyl(1:10),
    "input must be a data.frame",
    fixed = TRUE
  )

  # don't pass names to a 1-way tabyl
  expect_error(
    as_tabyl(mtcars, axes = 1, row_var_name = "foo"),
    "variable names are only meaningful for two-way tabyls"
  )
})
