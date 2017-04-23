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

test_that("show_NA = FALSE parameter works", {
  expect_equal(test_res %>%
                 stats::setNames(c("test_df_na$grp", names(test_res)[-1])),
               tabyl(test_df_na$grp, show_na = FALSE))
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
  
# check sort parameter
sorted_test_df_na <- tabyl(test_df_na$grp, sort = TRUE)
sorted_with_fac <- data.frame(grp = factor(c("a", "c", "c"), levels = letters[1:3]))
sorted_with_fac <- tabyl(sorted_with_fac$grp, sort = TRUE)

sorted_with_na_and_fac <- data.frame(grp = factor(c("a", "c", "c", NA), levels = letters[1:3]))
sorted_with_na_and_fac_res <- tabyl(sorted_with_na_and_fac$grp, sort = TRUE)

test_that("sort parameter works", {
  expect_equal(sorted_test_df_na[[1]], c("b", "a", "c", NA))
  expect_equal(sorted_test_df_na[[4]], c(0.5, 0.25, 0.25, NA))
  expect_equal(sorted_with_fac[[1]], factor(c("c", "a", "b"), levels = letters[1:3]))
  expect_equal(sorted_with_fac[[2]], c(2, 1, 0))
  expect_equal(sorted_with_na_and_fac_res[[1]], factor(c("c", "a", "b", NA), levels = letters[1:3]))
  expect_equal(sorted_with_na_and_fac_res[[2]], c(2, 1, 0, 1))
  expect_equal(sorted_with_na_and_fac_res[[3]], c(2/4, 1/4, 0, 1/4))
  expect_equal(sorted_with_na_and_fac_res[[4]], c(2/3, 1/3, 0, NA))
})

# piping
test_that("piping in a data.frame works", {
  expect_equal(tabyl(mtcars$cyl) %>%
                 setNames(., c("cyl", names(.)[2:3])),
               mtcars %>% tabyl(cyl))
  expect_equal(tabyl(sorted_with_na_and_fac$grp, sort = TRUE) %>% # complete levels + correct sorting work for factors with empty categories
                 setNames(., c("grp", names(.)[-1])), sorted_with_na_and_fac %>% tabyl(grp, sort = TRUE))
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
               data.frame(percent = c(1/32, 2/32),
                          n = c(18, 7),
                          percent_percent = c(18/25, 7/25))
  )
  expect_equal(a %>% tabyl(n),
               data.frame(n = 1:2,
                          n_n = c(18, 7),
                          percent = c(18/25, 7/25))
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
