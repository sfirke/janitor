# Tests adorn_rounding

library(janitor)
context("adorn_rounding")

library(dplyr)

x <- data.frame(
  a = c(rep("x", 55), rep("y", 45)),
  b = c(rep("x", 50), rep("y", 50)),
  stringsAsFactors = FALSE
)
  
# Crosstab with decimal values ending in .5
y <- x %>%
  tabyl(a, b) %>%
  adorn_percentages("all")

test_that("rounding parameter works", {
  expect_equal(y %>%
                 adorn_rounding(digits = 1, rounding = "half up") %>%
                 un_tabyl(),
               data.frame(
                 a = c("x", "y"),
                 x = c(0.5, 0.0),
                 y = c(0.1, 0.5),
                 stringsAsFactors = FALSE)
               )
  expect_equal(y %>%
                 adorn_rounding(digits = 1) %>% # default rounding: "half to even"
                 un_tabyl(),
               data.frame(
                 a = c("x", "y"),
                 x = c(0.5, 0.0),
                 y = c(0.0, 0.4),
                 stringsAsFactors = FALSE)
  )
  
  
})

test_that("first column toggling works", {
  z <- y
  z$a <- c(0.05, 0.5)
  expect_equal(z %>%
                 adorn_rounding(skip_first_col = FALSE) %>%
                 un_tabyl(),
               data.frame(
                 a = c(0.0, 0.5),
                 x = c(0.5, 0.0),
                 y = c(0.0, 0.4))
  )
  expect_equal(z %>%
                 adorn_rounding() %>% # implied: skip_first_col = TRUE
                 un_tabyl(),
               data.frame(
                 a = c(0.05, 0.5),
                 x = c(0.5, 0.0),
                 y = c(0.0, 0.4))
  )
})

test_that("digit control succeeds", {
  expect_equal(y %>%
                 adorn_rounding(digits = 0, rounding = "half up") %>%
                 un_tabyl(),
               data.frame(
                 a = c("x", "y"),
                 x = c(1, 0),
                 y = c(0, 0),
                 stringsAsFactors = FALSE))
  expect_equal(y %>%
                 adorn_rounding(digits = 2, rounding = "half up"), # shouldn't do anything given the input only having 2 decimal places
               y)
})

test_that("bad rounding argument caught", {
  expect_error(
    y %>%
    adorn_rounding(rounding = "blargh"),
    "'rounding' must be one of 'half to even' or 'half up'",
    fixed = TRUE
  )
})
