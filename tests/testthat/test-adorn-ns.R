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
                 un_tabyl(),
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
                 un_tabyl(),
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
               "adorn_ns() can only be called on a data.frame of class \"tabyl\"",
               fixed = TRUE)
})
