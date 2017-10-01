# Tests the adorn_percentages() function

library(janitor)
context("adorn_pct_formatting()")

library(dplyr)

source1 <- mtcars %>%
  tabyl(cyl, am) %>%
  adorn_percentages()

test_that("calculations are accurate", {
  expect_equal(un_tabyl(adorn_pct_formatting(source1)), # default parameter is denom = "row"
               data.frame(cyl = c(4, 6, 8),
                          `0` = c("27.3%", "57.1%", "85.7%"),
                          `1` = c("72.7%", "42.9%", "14.3%"),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})

test_that("data.frames with no numeric columns beyond the first cause failure", {
  expect_error(adorn_pct_formatting(data.frame(a = 1:2, b = c("hi", "lo"))),
               "at least one one of columns 2:n must be of class numeric")
})

dat <- data.frame(Operation = c("Login", "Posted", "Deleted"), `Total Count` = c(5, 25, 40), check.names = FALSE)

test_that("works with a single numeric column per #89", {
  expect_equal(dat %>% adorn_percentages("col") %>% un_tabyl(),
               data.frame(Operation = c("Login", "Posted", "Deleted"),
                          `Total Count` = c(5/70, 25/70, 40/70),
                          check.names = FALSE)
  )
})

test_that("works with totals row", {
  expect_equal(dat %>% adorn_totals("row") %>% adorn_percentages("col") %>% un_tabyl(),
               data.frame(Operation = c("Login", "Posted", "Deleted", "Total"),
                          `Total Count` = c(5/70, 25/70, 40/70, 1),
                          check.names = FALSE, stringsAsFactors = FALSE)
  )
})

test_that("works with one-way tabyl", {
  expect_equal(
    mtcars %>%
      tabyl(carb) %>%
      adorn_pct_formatting(digits = 0) %>%
      un_tabyl(),
    data.frame(
      carb = c(1:4, 6, 8),
      n = c(7, 10, 3, 10, 1, 1),
      percent = c("22%", "31%", "9%", "31%", "3%", "3%"),
      stringsAsFactors = FALSE)
  )
})