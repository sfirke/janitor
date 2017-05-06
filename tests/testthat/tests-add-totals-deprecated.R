# Tests add_totals_row and add_totals_col

library(janitor)
context("deprecated add_totals_* functions")

library(dplyr)
dat <- data.frame(a = c(rep(c("big", "small", "big"), 3)),
                  b = c(1:3, 1:3, 1, 1, 1)
)
ct <- dat %>%
  crosstab(a, b)

expect_equal_deprecated <- function(object, expected, ...){
  expect_warning(object, "deprecated", ignore.case = TRUE)
  expect_equal(suppressWarnings(object), expected, ...)
}


test_that("totals row is correct", {
  expect_equal_deprecated(add_totals_row(ct),
               data.frame(a = c("big", "small", "Total"),
                          `1` = c(4, 1, 5),
                          `2` = c(0, 2, 2),
                          `3` = c(2, 0, 2),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})


test_that("totals col is correct", {
  expect_equal_deprecated(add_totals_col(ct),
               data.frame(a = c("big", "small"),
                          `1` = c(4, 1),
                          `2` = c(0, 2),
                          `3` = c(2, 0),
                          Total = c(6, 3),
                          check.names = FALSE,
                          stringsAsFactors = TRUE)
  )
})


test_that("totals row and col produce correct results when called together", {
  expect_equal_deprecated(ct %>%
                 add_totals_col %>%
                 add_totals_row(),
               data.frame(a = c("big", "small", "Total"),
                          `1` = c(4, 1, 5),
                          `2` = c(0, 2, 2),
                          `3` = c(2, 0, 2),
                          Total = c(6, 3, 9),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})

test_that("order doesn't matter when totals row and col are called together", {
  expect_equal(suppressWarnings(ct %>%
                                  add_totals_col %>%
                                  add_totals_row),
               suppressWarnings(ct %>%
                                  add_totals_row %>%
                                  add_totals_col)
  )
})

test_that("both functions work with a single column", {
  single_col <- data_frame(a = c(as.Date("2016-01-01"), as.Date("2016-02-03")),
                           b = c(1, 2))
  expect_equal_deprecated(single_col %>% add_totals_row(), data_frame(a = c("2016-01-01", "2016-02-03", "Total"), b = 1:3) %>% mutate(b = as.double(b))) # from http://stackoverflow.com/a/30068233
  expect_equal_deprecated(single_col %>% add_totals_col(), data_frame(a = c(as.Date("2016-01-01"), as.Date("2016-02-03")),
                                                           b = c(1, 2)) %>% mutate(Total = as.double(1:2)))
})

test_that("error thrown for no non-numeric cols after 1st position", {
  df2 <- data.frame(x = c("big", "small"),
                    y = c("hi", "lo"))
  expect_error(suppressWarnings(add_totals_row(df2)),
               "at least one one of columns 2:n must be of class numeric")
  expect_error(suppressWarnings(add_totals_col(df2)),
               "at least one one of columns 2:n must be of class numeric")
  
                           
})

