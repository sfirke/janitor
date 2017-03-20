# Tests function for formatting output of crosstab() function

library(janitor)
context("adorn_crosstab")

library(dplyr)
source1 <- mtcars %>%
  crosstab(gear, cyl)

test_that("default call is correct", {
  expect_equal(adorn_crosstab(source1), data.frame(
    gear = c("3", "4", "5"),
    `4` = c("6.7% (1)", "66.7% (8)", "40.0% (2)"),
    `6` = c("13.3% (2)", "33.3% (4)", "20.0% (1)"),
    `8` = c("80.0% (12)", "0.0%  (0)", "40.0%  (2)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
})

test_that("percentages are correct", {
  expect_equal(adorn_crosstab(source1, "col"), data.frame(
    gear = c("3", "4", "5"),
    `4` = c("9.1% (1)", "72.7% (8)", "18.2% (2)"),
    `6` = c("28.6% (2)", "57.1% (4)", "14.3% (1)"),
    `8` = c("85.7% (12)", "0.0%  (0)", "14.3%  (2)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  expect_equal(adorn_crosstab(source1, "all"), data.frame(
    gear = c("3", "4", "5"),
    `4` = c("3.1% (1)", "25.0% (8)", "6.2% (2)"),
    `6` = c("6.2% (2)", "12.5% (4)", "3.1% (1)"),
    `8` = c("37.5% (12)", "0.0%  (0)", "6.2%  (2)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
})

test_that("rounding is correct", {
  # rounding up
    rounded_up <- mtcars %>%
    crosstab(cyl, am) %>%
    adorn_crosstab(., denom = "all", digits = 0, rounding = "half up") 
    
    expect_equal(rounded_up, data.frame(
      cyl = c("4", "6", "8"),
      `0` = c("9%  (3)", "13%  (4)", "38% (12)"),
      `1` = c("25% (8)", "9% (3)", "6% (2)"),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
    # default base R rounding
    rounded_to_even <- mtcars %>%
      crosstab(cyl, am) %>%
      adorn_crosstab(., denom = "all", digits = 0) 
    
    expect_equal(rounded_to_even, data.frame(
      cyl = c("4", "6", "8"),
      `0` = c("9%  (3)", "12%  (4)", "38% (12)"),
      `1` = c("25% (8)", "9% (3)", "6% (2)"),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
})


test_that("digits parameter is correct", {
  digits0 <- mtcars %>% crosstab(carb, gear) %>% adorn_crosstab(denom = "row", digits = 0)
  expect_equal(digits0, data.frame(
    carb = c(1:4, 6, "8"),
    `3` = c("43% (3)", "40% (4)", "100% (3)", "50% (5)", "0% (0)", "0% (0)"),
    `4` = c("57% (4)", "40% (4)", "0% (0)", "40% (4)", "0% (0)", "0% (0)"),
    `5` = c("0% (0)", "20% (2)", "0% (0)", "10% (1)", "100% (1)", "100% (1)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))    
})

test_that("show_n can suppress Ns, digits parameter is correct", {
  digits3 <- mtcars %>% crosstab(carb, gear) %>% adorn_crosstab(denom = "row", digits = 3, show_n = FALSE)
  expect_equal(digits3, data.frame(
    carb = c(1:4, 6, "8"),
    `3` = c("42.857%", "40.000%", "100.000%", "50.000%", "0.000%", "0.000%"),
    `4` = c("57.143%", "40.000%", "0.000%", "40.000%", "0.000%", "0.000%"),
    `5` = c("0.000%", "20.000%", "0.000%", "10.000%", "100.000%", "100.000%"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
})

test_that("spacing is correct", {
  spacings <- data_frame(
    x = c(rep("a", 500), "b", "b", "c", "d"),
    y = rep(c(0,0,0,0,0,1), 84)
  ) %>%
    crosstab(x, y) %>%
    adorn_crosstab(denom = "all")
  
  expect_equal(spacings, data.frame(
    x = letters[1:4],
    `0` = c("82.7% (417)", "0.4%   (2)", "0.2%   (1)", "0.0%   (0)"),
    `1` = c("16.5% (83)", "0.0%  (0)", "0.0%  (0)", "0.2%  (1)"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ))
})

test_that("totals row and columns are correct", {
  expect_equal(adorn_crosstab(source1, show_totals = TRUE), data.frame( # row
    gear = c(3, 4, 5, "Total"),
    `4` = c("6.7%  (1)", "66.7%  (8)", "40.0%  (2)", "34.4% (11)"),
    `6` = c("13.3% (2)", "33.3% (4)", "20.0% (1)", "21.9% (7)"),
    `8` = c("80.0% (12)", "0.0%  (0)", "40.0%  (2)", "43.8% (14)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  expect_equal(adorn_crosstab(source1, "col", show_totals = TRUE), data.frame( # col
    gear = c("3", "4", "5"),
    `4` = c("9.1% (1)", "72.7% (8)", "18.2% (2)"),
    `6` = c("28.6% (2)", "57.1% (4)", "14.3% (1)"),
    `8` = c("85.7% (12)", "0.0%  (0)", "14.3%  (2)"),
    Total = c("46.9% (15)", "37.5% (12)", "15.6%  (5)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
  expect_equal(adorn_crosstab(source1, "all", show_totals = TRUE), data.frame(
    gear = c("3", "4", "5", "Total"),
    `4` = c("3.1%  (1)", "25.0%  (8)", "6.2%  (2)", "34.4% (11)"),
    `6` = c("6.2% (2)", "12.5% (4)", "3.1% (1)", "21.9% (7)"),
    `8` = c("37.5% (12)", "0.0%  (0)", "6.2%  (2)", "43.8% (14)"),
    Total = c("46.9% (15)", "37.5% (12)", "15.6%  (5)", "100.0% (32)"),
    check.names = FALSE, stringsAsFactors = FALSE
  ))
})

test_that("Totals works with factor column", {
  df1 <- data.frame(x = c("big", "small"),
                    y = 1:2,
                    z = 10:11)
  expect_equal(
    adorn_crosstab(df1, denom = "row", show_totals = TRUE),
    data.frame(x = c("big", "small", "Total"),
               y = c("9.1% (1)", "15.4% (2)", "12.5% (3)"),
               z = c("90.9% (10)", "84.6% (11)", "87.5% (21)"),
               stringsAsFactors = FALSE
  ))
})


test_that("bad inputs are caught", {
  expect_error(adorn_crosstab(source1, rounding = "up"), "'rounding' must be one of 'half to even' or 'half up'")
  expect_error(adorn_crosstab(source1, denom = "roww"), "'denom' must be one of 'row', 'col', or 'all'")
})