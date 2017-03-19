# Tests add_totals_row and add_totals_col

library(janitor)
context("add_totals function")

library(dplyr)
dat <- data.frame(a = c(rep(c("big", "small", "big"), 3)),
                  b = c(1:3, 1:3, 1, 1, 1)
)
ct <- dat %>%
  crosstab(a, b)

                          

test_that("totals row is correct", {
  expect_equal(add_totals(ct, "row"),
               data.frame(a = c("big", "small", "Total"),
                          `1` = c(4, 1, 5),
                          `2` = c(0, 2, 2),
                          `3` = c(2, 0, 2),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})


test_that("totals col is correct", {
  expect_equal(add_totals(ct, "col"),
               data.frame(a = c("big", "small"),
                          `1` = c(4, 1),
                          `2` = c(0, 2),
                          `3` = c(2, 0),
                          Total = c(6, 3),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})

               
test_that("totals row and col produce correct results when called together", {
  expect_equal(ct %>%
                 add_totals(c("row", "col")),
               data.frame(a = c("big", "small", "Total"),
                          `1` = c(4, 1, 5),
                          `2` = c(0, 2, 2),
                          `3` = c(2, 0, 2),
                          Total = c(6, 3, 9),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})

test_that("order doesn't matter when row and col are called together", {
  expect_equal(ct %>%
                 add_totals(c("row", "col")),
               ct %>%
                 add_totals(c("col", "row"))
  )
})

test_that("both functions work with a single column", {
  single_col <- data_frame(a = c(as.Date("2016-01-01"), as.Date("2016-02-03")),
                         b = c(1, 2))
  expect_error(single_col %>% add_totals("row"), NA) # from http://stackoverflow.com/a/30068233
  expect_error(single_col %>% add_totals("col"), NA)
  expect_error(single_col %>% add_totals(c("col", "row")), NA)
})




dat <- data.frame(
  a = c("hi", "lo"),
  b = c(1, 2),
  c = c(5, 10),
  d = c("big", "small"),
  e = c(20, NA),
  stringsAsFactors = FALSE
)

test_that("numeric first column is ignored", {
  expect_equal(mtcars %>%
                 crosstab(cyl, gear) %>%
                 add_totals("col"),
               data.frame(
                 cyl = c("4", "6", "8"),
                 `3` = c(1, 2, 12),
                 `4` = c(8, 4, 0),
                 `5` = c(2, 1, 2),
                 Total = c(11, 7, 14),
                 check.names = FALSE,
                 stringsAsFactors = FALSE))
})

test_that("grouped_df gets ungrouped and succeeds", {
  ct <- mtcars %>% group_by(cyl, gear) %>% tally() %>% tidyr::spread(gear, n)
  expect_equal(ct %>% add_totals(),
               ct %>% ungroup() %>% add_totals
  )
})

test_that("na.rm value gets passed through", {
  
})


test_that("error thrown if no columns are numeric", {
  
})

test_that("works with non-numeric columns mixed in", {
  
})

test_that("column names are passed through", {
  
})
