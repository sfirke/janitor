# Tests for data.frame renaming function

library(janitor)
library(dplyr)
context("tabyl")

cyl_tbl <- tabyl(mtcars$cyl)

test_that("counts are accurate", {
  expect_equal(cyl_tbl$`mtcars$cyl`, c(4, 6, 8))
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
  expect_equal(names(cyl_tbl), c("mtcars$cyl", "n", "percent"))
  expect_equal(names(test_res_na), c("test_df_na$grp", "n", "percent", "valid_percent"))
})

test_that("NAs handled correctly", {
  expect_equal(test_res_na$percent, c(0.2, 0.4, 0.2, 0.2))
  expect_equal(test_res_na$valid_percent, c(0.25, 0.5, 0.25, NA))
})

test_that("show_NA = FALSE parameter works, incl. with piped input", {
  resss <- test_res
  names(resss)[1] <- "test_df_na$grp"
  names(attr(resss, "core"))[1] <- "test_df_na$grp"
  expect_equal(resss,
               tabyl(test_df_na$grp, show_na = FALSE))
  names(attr(resss, "core"))[1] <- "grp" ;   names(resss)[1] <- "grp" # for this next instance, col name changes
  expect_equal(resss,
               test_df_na %>% tabyl(grp, show_na = FALSE))
})

test_that("ordering of result by factor levels is preserved for factors", {
  expect_equal(tabyl(factor(c("x", "y", "z"), levels = c("y", "z", "x")))[[1]], factor(c("y", "z", "x"), levels = c("y", "z", "x")))
})

# missing factor levels shown, with and without NA
fac <- iris[["Species"]][70:80] # to get versicolor, not the first alphabetically
fac_na <- fac
fac_na[1:2] <- NA


test_that("missing factor levels are displayed without NA values", {
  expect_equal(tabyl(fac)[[1]], factor(c("setosa","versicolor", "virginica"), levels = c("setosa", "versicolor", "virginica")))
  expect_equal(tabyl(fac)[[2]], c(0, 11, 0))
  expect_equal(tabyl(fac)[[3]], c(0, 1, 0))
})

test_that("missing factor levels are displayed with NA values", {
  expect_equal(tabyl(fac_na)[[1]], factor(c("setosa","versicolor", "virginica", NA), levels = c("setosa", "versicolor", "virginica")))
  expect_equal(tabyl(fac_na)[[2]], c(0, 9, 0, 2))
  expect_equal(tabyl(fac_na)[[3]], c(0, 9/11, 0, 2/11))
  expect_equal(tabyl(fac_na)[[4]], c(0, 1, 0, NA))
})
  

# piping
test_that("piping in a data.frame works", {
  x <- tabyl(mtcars$cyl)
  names(x)[1] <- "cyl"
  names(attr(x, "core"))[1] <- "cyl"
  expect_equal(x,
               mtcars %>% tabyl(cyl))
})


# bad inputs

test_that("failure occurs when passed unsupported types", {
  expect_error(tabyl(matrix(1:10, nrow = 5)), "input must be a vector of type logical, numeric, character, list, or factor")
  expect_error(tabyl(complex(10)), "input must be a vector of type logical, numeric, character, list, or factor")
})

test_that("bad input variable name is preserved", {
  expect_equal(mtcars %>% mutate(`bad name` = cyl) %>% tabyl(`bad name`) %>% names %>% .[[1]],
               "bad name")
  k <- mtcars %>% mutate(`bad name` = cyl)
  expect_equal(tabyl(k$`bad name`) %>% names %>% .[[1]],
               "k$`bad name`")
})


test_that("input variable names 'percent' and 'n' are handled", {
  a <- mtcars %>% tabyl(mpg)
  expect_equal(a %>% tabyl(percent),
               as_tabyl(
                 data.frame(percent = c(1/32, 2/32),
                          n = c(18, 7),
                          percent_percent = c(18/25, 7/25)),
               1)
  )
  expect_equal(a %>% tabyl(n),
               as_tabyl(data.frame(n = 1:2,
                          n_n = c(18, 7),
                          percent = c(18/25, 7/25)),
                        1)
  )
})

test_that("bizarre combination of %>%, quotes, and spaces in names is handled", {
  dat <- data.frame(
    `The candidate(s) applied directly to my school` = c("a", "b", "a", "b"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  expect_equal(
    tabyl(dat$`The candidate(s) applied directly to my school` %>% gsub("hi", "there", .)) %>%
      names() %>% .[1],
    "dat$`The candidate(s) applied directly to my school` %>% gsub(\"hi\",     \"there\", .)"
  )
})

test_that("grouped data.frame inputs are handled (#125)", {
  expect_equal(mtcars %>% group_by(cyl) %>% tabyl(carb, gear),
               mtcars %>% tabyl(carb, gear))
})


test_that("if called on non-existent vector, returns useful error message", {
  expect_error(tabyl(mtcars$moose), "object mtcars\\$moose not found")
  expect_error(tabyl(moose), "object 'moose' not found")
  expect_error(mtcars %>% tabyl(moose), "object 'moose' not found")
})

# showing missing factor levels

testthat("show_missing_levels parameter works", {
z <- structure(list(
  a = structure(1, .Label = c("hi", "lo"), class = "factor"),
  b = structure(2, .Label = c("big", "small"), class = "factor"),
  new = structure(1, .Label = c("lvl1", "lvl2"), class = "factor")),
  row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame"),
  .Names = c("a", "b", "new"))

expect_equal(z %>% tabyl(a, b, new, show_missing_levels = TRUE),
             list(lvl1 = data.frame(a = c("hi", "lo"),
                                    big = c(0, 0),
                                    small = c(1, 0)) %>% as_tabyl()))
expect_equal(z %>% tabyl(a, b, new, show_missing_levels = FALSE),
             list(lvl1 = data.frame(a = c("hi"),
                                    small = c(1), stringsAsFactors = FALSE) %>% as_tabyl()))

# Works with numerics
expect_equal(mtcars %>% tabyl(cyl, am),
             data.frame(cyl = c(4, 6, 8),
                        `0` = c(3, 4, 12),
                        `1` = c(8, 3, 2),
                        check.names = FALSE) %>% as_tabyl())
})

