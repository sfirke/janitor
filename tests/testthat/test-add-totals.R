# Tests add_totals_row and add_totals_col

library(janitor)
context("add_totals functions")

library(dplyr)
dat <- data.frame(a = c(1:3, 1:3, 1, 1, 1),
                  b = c(rep(c("big", "small", "big"), 3))
)
ct <- dat %>%
  crosstab(b, a)


test_that("error thrown if column beyond the first is not numeric", {
  expect_error(add_totals_row(dat),
               "all columns after the first one must be numeric")
  expect_error(add_totals_col(dat),
               "all columns after the first one must be numeric")
})
                          

test_that("totals row is correct", {
  expect_equal(add_totals_row(ct),
               data.frame(b = c("big", "small", "Total"),
                          `1` = c(4, 1, 5),
                          `2` = c(0, 2, 2),
                          `3` = c(2, 0, 2),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})


test_that("totals col is correct", {
  expect_equal(add_totals_col(ct),
               data.frame(b = c("big", "small"),
                          `1` = c(4, 1),
                          `2` = c(0, 2),
                          `3` = c(2, 0),
                          Total = c(6, 3),
                          check.names = FALSE,
                          stringsAsFactors = TRUE)
  )
})

               
test_that("totals row and col produce correct results when called together", {
  expect_equal(ct %>%
                 add_totals_col %>%
                 add_totals_row(),
               data.frame(b = c("big", "small", "Total"),
                          `1` = c(4, 1, 5),
                          `2` = c(0, 2, 2),
                          `3` = c(2, 0, 2),
                          Total = c(6, 3, 9),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})

test_that("order doesn't matter when totals row and col are called together", {
  expect_equal(ct %>%
                 add_totals_col %>%
                 add_totals_row,
               ct %>%
                 add_totals_row %>%
                 add_totals_col
  )
})
