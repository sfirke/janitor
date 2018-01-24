# Tests adorn_totals and deprecated add_totals_row, add_totals_col

library(janitor)
context("adorn_totals & deprecated add_totals functions")

library(dplyr)

dat <- data.frame(a = c(rep(c("big", "small", "big"), 3)),
                  b = c(1:3, 1:3, 1, 1, 1)
)
ct <- dat %>%
  tabyl(a, b)

                          

test_that("totals row is correct", {
  expect_equal(untabyl(adorn_totals(ct, "row")),
               data.frame(a = c("big", "small", "Total"),
                          `1` = c(4, 1, 5),
                          `2` = c(0, 2, 2),
                          `3` = c(2, 0, 2),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
               )
})


test_that("totals col is correct", {
  expect_equal(untabyl(adorn_totals(ct, "col")),
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
                 adorn_totals(c("row", "col")) %>%
                 untabyl(),
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
                 adorn_totals(c("row", "col")) %>% untabyl(),
               ct %>%
                 adorn_totals(c("col", "row")) %>% untabyl()
  )
})

test_that("both functions work with a single column", {
  single_col <- data_frame(a = c(as.Date("2016-01-01"), as.Date("2016-02-03")),
                           b = c(1, 2))
  expect_error(single_col %>% adorn_totals("row"), NA) # this method of testing passage is from http://stackoverflow.com/a/30068233
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
                 tabyl(cyl, gear) %>%
                 adorn_totals("col") %>%
                 untabyl(),
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
ct_2 <- mtcars %>% group_by(cyl, gear) %>% tally() %>% tidyr::spread(gear, n)
df1 <- data.frame(x = c(1, 2), y = c(NA, 4))

test_that("grouped_df gets ungrouped and succeeds", {
  ct_2 <- mtcars %>% group_by(cyl, gear) %>% tally() %>% tidyr::spread(gear, n)
  expect_equal(ct_2 %>% adorn_totals(),
               ct_2 %>% ungroup() %>% adorn_totals()
  )
})

test_that("na.rm value works correctly", {
  expect_equal(df1 %>% adorn_totals(c("row", "col"), na.rm = FALSE) %>% untabyl(),
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
               class(df1 %>% adorn_totals() %>% untabyl()))
})

test_that("add_totals respects if input was data_frame", {
  expect_equal(class(df1 %>% as_data_frame()),
               class(df1 %>% as_data_frame() %>% adorn_totals() %>% untabyl()))
})

test_that("error thrown if no columns past first are numeric", {
  df2 <- data.frame(x = c("big", "small"),
                    y = c("hi", "lo"))
  expect_error(adorn_totals(df2, "col"),
               "at least one one of columns 2:n must be of class numeric.  adorn_totals should be called before other adorn_ functions.")
  
  # Add a test where only the first column is numeric 
  df3 <- data.frame(x = 1:2,
                    y = c("hi", "lo"))
  expect_error(adorn_totals(df3),
               "at least one one of columns 2:n must be of class numeric.  adorn_totals should be called before other adorn_ functions.")
  
})

test_that("bad input to where arg is caught",{
  expect_error(mtcars %>%
                 adorn_totals("blargh"),
               paste0('"where" must be one of "row", "col", or c("row", "col")'),
               fixed = TRUE)
}
)

test_that("works with non-numeric columns mixed in; fill character specification", {
  mixed <- data.frame(
    a = 1:3,
    b = c("x", "y", "z"),
    c = 5:7,
    d = c("big", "med", "small"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(mixed %>% adorn_totals(where = c("row", "col"), fill = "*") %>% untabyl(),
               data.frame(a = c("1", "2", "3", "Total"),
                          b = c("x", "y", "z", "*"),
                          c = c(5, 6, 7, 18),
                          d = c("big", "med", "small", "*"),
                          Total = c(5, 6, 7, 18),
                          stringsAsFactors = FALSE)
  )
})

test_that("totals attributes are assigned correctly", {
  post <- adorn_totals(ct, c("row", "col"))
  expect_equal(attr(post, "totals"), c("row", "col"))
  expect_equal(class(post), c("tabyl", "data.frame"))
  expect_equal(attr(post, "tabyl_type"), "two_way")
  expect_equal(attr(post, "core"), untabyl(ct))
  
  post_col <- adorn_totals(ct, "col")
  expect_equal(attr(post_col, "totals"), "col")
  expect_equal(class(post_col), c("tabyl", "data.frame"))
  expect_equal(attr(post_col, "tabyl_type"), "two_way")
  expect_equal(attr(post_col, "core"), untabyl(ct))
})


test_that("trying to re-adorn a dimension fails", {
  expect_error(ct %>% adorn_totals("col") %>% adorn_totals("col"),
               "trying to re-add a totals dimension that is already been added")
  expect_error(ct %>% adorn_totals() %>% adorn_totals(),
               "trying to re-add a totals dimension that is already been added")
})


test_that("automatically invokes purrr::map when called on a 3-way tabyl", {
  three <- tabyl(mtcars, cyl, am, gear)
  expect_equal(adorn_totals(three), # vanilla call
               purrr::map(three, adorn_totals))

    # with arguments passing through
  expect_equal(adorn_totals(three, c("row", "col"), fill = "---", na.rm = FALSE),
               purrr::map(three, adorn_totals, c("row", "col"), fill = "---", FALSE))
  
})

test_that("non-data.frame inputs are handled", {
  expect_error(adorn_totals(1:5), "adorn_totals() must be called on a data.frame or list of data.frames", fixed = TRUE)
})

# Kind of superficial given that add_totals_ have been refactored to call adorn_totals() themselves, but might as well keep until deprecated functions are removed
test_that("deprecated functions adorn_totals_col and adorn_totals_row function as expected", {
  expect_equal(
    mtcars %>%
      adorn_totals(),
    suppressWarnings(mtcars %>%
      add_totals_row())
  )
  expect_equal(
    mtcars %>%
      adorn_totals("col"),
    suppressWarnings(mtcars %>%
      add_totals_col())
  )
})
