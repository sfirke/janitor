# Tests adorn_totals and deprecated add_totals_row, add_totals_col

library(janitor)
context("adorn_totals & deprecated add_totals functions")

library(dplyr)

dat <- data.frame(a = c(rep(c("big", "small", "big"), 3)),
                  b = c(1:3, 1:3, 1, 1, 1)
)
ct <- dat %>%
  crosstab(a, b)

                          

test_that("totals row is correct", {
  expect_equal(adorn_totals(ct, "row"),
               data.frame(a = c("big", "small", "Total"),
                          `1` = c(4, 1, 5),
                          `2` = c(0, 2, 2),
                          `3` = c(2, 0, 2),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})


test_that("totals col is correct", {
  expect_equal(adorn_totals(ct, "col"),
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
  expect_equal(ct %>%
                 adorn_totals(c("row", "col")),
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
                 adorn_totals(c("row", "col")),
               ct %>%
                 adorn_totals(c("col", "row"))
  )
})

test_that("both functions work with a single column", {
  single_col <- data_frame(a = c(as.Date("2016-01-01"), as.Date("2016-02-03")),
                         b = c(1, 2))
  expect_error(single_col %>% adorn_totals("row"), NA) # from http://stackoverflow.com/a/30068233
  expect_error(single_col %>% adorn_totals("col"), NA)
  expect_error(single_col %>% adorn_totals(c("col", "row")), NA)
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
                 adorn_totals("col"),
               data.frame(
                 cyl = c(4, 6, 8),
                 `3` = c(1, 2, 12),
                 `4` = c(8, 4, 0),
                 `5` = c(2, 1, 2),
                 Total = c(11, 7, 14),
                 check.names = FALSE,
                 stringsAsFactors = FALSE))
})

# create input tables for subsequent testing
ct <- mtcars %>% group_by(cyl, gear) %>% tally() %>% tidyr::spread(gear, n)
df1 <- data.frame(x = c(1, 2), y = c(NA, 4))

test_that("grouped_df gets ungrouped and succeeds", {
  ct <- mtcars %>% group_by(cyl, gear) %>% tally() %>% tidyr::spread(gear, n)
  expect_equal(ct %>% adorn_totals(),
               ct %>% ungroup() %>% adorn_totals()
  )
})

test_that("na.rm value works correctly", {
  expect_equal(df1 %>% adorn_totals(na.rm = FALSE),
               data.frame(
                 x = c("1", "2", "Total"),
                 y = c(NA, 4, NA),
                 Total = c(NA, 4, NA),
                 stringsAsFactors = FALSE
               )
  )
})

test_that("add_totals respects if input was data.frame", {
  expect_equal(class(df1),
               class(df1 %>% adorn_totals()))
})

test_that("add_totals respects if input was data_frame", {
  expect_equal(class(df1 %>% as_data_frame()),
               class(df1 %>% as_data_frame() %>% adorn_totals()))
})

test_that("error thrown if no columns past first are numeric", {
  df2 <- data.frame(x = c("big", "small"),
                    y = c("hi", "lo"))
  expect_error(adorn_totals(df2, "col"),
               "at least one one of columns 2:n must be of class numeric")
  
  # Add a test where only the first column is numeric 
  df3 <- data.frame(x = 1:2,
                    y = c("hi", "lo"))
  expect_error(adorn_totals(df3),
               "at least one one of columns 2:n must be of class numeric")
  
})

test_that("works with non-numeric columns mixed in; fill character specification", {
  mixed <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    c = 5:7,
    d = c("big", "med", "small"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(mixed %>% adorn_totals(fill = "*"),
               data.frame(a = c("1", "2", "3", "Total"),
                          b = c("x", "y", "z", "*"),
                          c = c(5, 6, 7, 18),
                          d = c("big", "med", "small", "*"),
                          Total = c(5, 6, 7, 18),
                          stringsAsFactors = FALSE)
               )
})
