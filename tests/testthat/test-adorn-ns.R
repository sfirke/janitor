# Tests the adorn_ns() function

library(janitor)
context("adorn_ns()")

library(dplyr)

source_an <- data_frame(
  x = c(rep("a", 500), "b", "b", "c", "d"),
  y = rep(c(0,0,0,0,0,1), 84)
) %>%
  tabyl(x, y)


test_that("spacing is correct", {
  expect_equal(source_an %>%
                 adorn_totals() %>%
                 adorn_percentages("all") %>%
                 adorn_pct_formatting() %>%
                 adorn_ns() %>%
                 untabyl(),
               data.frame(
                 x = c(letters[1:4], "Total"),
                 `0` = c("82.7% (417)", "0.4%   (2)", "0.2%   (1)", "0.0%   (0)", "83.3% (420)"),
                 `1` = c("16.5% (83)", "0.0%  (0)", "0.0%  (0)", "0.2%  (1)", "16.7% (84)"),
                 check.names = FALSE,
                 stringsAsFactors = FALSE
               )
  )
}
)

test_that("front parameter works", {
  expect_equal(source_an %>%
                 adorn_totals() %>%
                 adorn_percentages("all") %>%
                 adorn_pct_formatting() %>%
                 adorn_ns("front") %>%
                 untabyl(),
               data.frame(
                 x = c(letters[1:4], "Total"),
                 `0` = c("417 (82.7%)", "  2  (0.4%)", "  1  (0.2%)", "  0  (0.0%)", "420 (83.3%)"),
                 `1` = c("83 (16.5%)", " 0  (0.0%)", " 0  (0.0%)", " 1  (0.2%)", "84 (16.7%)"),
                 check.names = FALSE,
                 stringsAsFactors = FALSE
               )
  )
}
)

test_that("bad inputs are caught", {
  expect_error(mtcars %>% adorn_ns(),
               "argument \"ns\" cannot be null; if not calling adorn_ns() on a data.frame of class \"tabyl\", pass your own value for ns",
               fixed = TRUE)
  expect_error(mtcars %>% tabyl(am, cyl) %>% adorn_ns("huh"),
               "\"position\" must be one of \"front\" or \"rear\"",
               fixed = TRUE)
  expect_error(mtcars %>% tabyl(am, cyl) %>% adorn_ns(ns = mtcars$mpg),
               "if supplying a value to the ns argument, it must be of class data.frame")
  reg_df <- mtcars %>% tabyl(am, cyl)
  wide_df <- mtcars %>% tabyl(am, cyl)
  wide_df$extra <- c(10, 20)
  expect_error(adorn_ns(reg_df, ns = wide_df), 
               "if supplying your own data.frame of Ns to append, its dimensions must match those of the data.frame in the \"dat\" argument")
  expect_warning(mtcars %>% tabyl(cyl) %>% adorn_ns(),
                 "adorn_ns() is meant to be called on a two_way tabyl; consider combining columns of a one_way tabyl with tidyr::unite()",
                 fixed = TRUE)
})

test_that("attributes make it through unaltered", {
  expect_equal(
    attributes(source_an %>%
               adorn_totals() %>%
               adorn_percentages("all") %>%
               adorn_pct_formatting() %>%
               adorn_ns("front") # with adorn_ns
  ),
  attributes(source_an %>%
               adorn_totals() %>%
               adorn_percentages("all") %>%
               adorn_pct_formatting() # without adorn_ns
  ))
}
)

test_that("works on smallest tabyls", {
  expect_equal(
    mtcars %>%
      slice(1) %>%
      tabyl(am, cyl) %>%
      rename(new_var_name = `6`) %>%
      adorn_percentages() %>% adorn_pct_formatting() %>% adorn_ns() %>% untabyl(),
    data.frame(am = 1,
               new_var_name = "100.0% (1)", stringsAsFactors = FALSE)
  )
})


test_that("users can supply own Ns", {
  # make tabyl with thousands, convert to Ks to append
  set.seed(2)
  big_tabyl <- data.frame(
    a = sample(c("x", rep("y", 10)), 10000, replace = TRUE),
    b = sample(c("big", "big", "big", "small", "small"), 10000, replace = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    tabyl(a, b)
  
  custom_Ns <- big_tabyl %>%
    mutate(big = paste0(round(big/1000,1), "k"),
           small = paste0(round(small/1000,1), "k"))

    expect_equal(
      big_tabyl %>%
        adorn_percentages("col") %>%
        adorn_pct_formatting() %>%
        adorn_ns(ns = custom_Ns) %>%
        untabyl(),
      data.frame(
        a = c("x", "y"),
        big = c("8.8% (0.5k)", "91.2% (5.5k)"),
        small = c("9.3% (0.4k)", "90.7% (3.6k)"),
        stringsAsFactors = FALSE
      )
    )
})

test_that("automatically invokes purrr::map when called on a 3-way tabyl", { 
  three <- tabyl(mtcars, cyl, am, gear) %>%
    adorn_percentages() %>%
    adorn_pct_formatting()
  expect_equal(adorn_ns(three), # vanilla call 
               purrr::map(three, adorn_ns)) 
  
  # with arguments passing through 
  expect_equal(adorn_ns(three, "front"), 
               purrr::map(three, adorn_ns, "front")) 
  
}) 

test_that("non-data.frame inputs are handled", { 
  expect_error(adorn_ns(1:5), "adorn_ns() must be called on a data.frame or list of data.frames", fixed = TRUE) 
}) 

