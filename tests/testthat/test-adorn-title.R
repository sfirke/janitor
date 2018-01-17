# Test adorn_title function

library(janitor)
context("adorn_title")

library(dplyr)
library(tidyr)
library(tibble)
source1 <- mtcars %>%
  tabyl(gear, cyl)

test_that("placement is correct", {
  # Top
  expect_equal(source1 %>% 
                 adorn_title() %>% names,
               c("", "cyl", rep("", 2)))
  expect_equal(source1 %>% 
                 adorn_title() %>% .[1, ] %>% unlist %>% unname,
               c("gear", "4", "6", "8"))
  # Combined
  expect_equal(source1 %>% 
                 adorn_title("combined") %>% names,
               c("gear/cyl", "4", "6", "8"))
})

test_that("name overrides work", {
  expect_equal(source1 %>% 
                 adorn_title(row_name = "R", col_name = "C") %>% names,
               c("", "C", rep("", 2)))
})

test_that("non-tabyls are treated correctly", {
  non_tab <- mtcars %>% dplyr::count(gear, cyl) %>% tidyr::spread(gear, n)
  expect_error(adorn_title(non_tab), "When input is not a data.frame of class tabyl, a value must be specified for the col_name argument")
  
  expect_equal(non_tab %>% adorn_title(col_name = "col") %>% names,
               c("", "col", rep("", 2)))
  
  expect_equal(non_tab %>% adorn_title(placement = "combined", col_name = "col") %>% names,
               c("cyl/col", 3, 4, 5))
  
  expect_equal(non_tab %>% adorn_title(placement = "combined", row_name = "row!", col_name = "col") %>% names,
               c("row!/col", 3, 4, 5))
})
test_that("bad inputs are caught", {
  expect_error(adorn_title(1:2),
               "\"dat\" must be a data.frame",
               fixed = TRUE)
  expect_error(adorn_title(source1,
                           placement = "blargh"),
               "\"placement\" must be one of \"top\" or \"combined\"",
               fixed = TRUE)
  expect_error(adorn_title(source1,
                           row_name = 1:4),
               "row_name must be a string")
  expect_error(adorn_title(source1,
                           col_name = mtcars),
               "col_name must be a string")
  
  # Doesn't make sense with a one-way tabyl
  expect_warning(mtcars %>% tabyl(cyl) %>% adorn_title,
                 "adorn_title is meant for two-way tables, calling it on a one-way tabyl may not yield a meaningful result")
})

test_that("works with non-count inputs", {
  source2 <- tibble::tibble(sector = c("North", "South"), units = 1:2, group = c("a", "b"))  
  expect_equal(adorn_title(source2, col_name = "Characteristics") %>% names,
               c("", "Characteristics", ""))
})  