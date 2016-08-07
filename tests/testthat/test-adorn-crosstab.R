# Tests function for formatting output of crosstab() function

library(janitor)
context("adorn_crosstab")

library(dplyr)
source1 <- mtcars %>%
  crosstab(gear, cyl)

test_that("default call is correct", {
  expect_equal(adorn_crosstab(source1), data.frame(
    gear = c(3, 4, 5),
    `4` = c("6.7% (1)", "66.7% (8)", "40.0% (2)"),
    `6` = c("13.3% (2)", "33.3% (4)", "20.0% (1)"),
    `8` = c("80.0% (12)", "0.0% (0)", "40.0% (2)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
})


test_that("percentages are correct", {
  expect_equal(adorn_crosstab(source1, "col"), data.frame(
    gear = c(3, 4, 5),
    `4` = c("9.1% (1)", "72.7% (8)", "18.2% (2)"),
    `6` = c("28.6% (2)", "57.1% (4)", "14.3% (1)"),
    `8` = c("85.7% (12)", "0.0% (0)", "14.3% (2)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  expect_equal(adorn_crosstab(source1, "all"), data.frame(
    gear = c(3, 4, 5),
    `4` = c("3.1% (1)", "25.0% (8)", "6.2% (2)"),
    `6` = c("6.2% (2)", "12.5% (4)", "3.1% (1)"),
    `8` = c("37.5% (12)", "0.0% (0)", "6.2% (2)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
})

test_that("rounding is correct", {
  
})

test_that("show_n can suppress Ns", {
  
})

test_that("digits parameter is correct", {
  
})


test_that("bad inputs are caught", {
  expect_error(adorn_crosstab(source1, rounding = "up"), "'rounding' must be one of 'half to even' or 'half up'")
  expect_error(adorn_crosstab(source1, denom = "roww"), "'denom' must be one of 'row', 'col', or 'all'")
})