# Tests the adorn_ns() function

library(janitor)
context("adorn_ns()")

library(dplyr)

source1 <- data_frame(
  x = c(rep("a", 500), "b", "b", "c", "d"),
  y = rep(c(0,0,0,0,0,1), 84)
) %>%
  tabyl(x, y)


test_that("spacing is correct", {
  expect_equal(source1 %>%
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

### RESUME HERE ADAPTING OLD TESTS FROM adorn_crosstab()

test_that("calculations are accurate", {
  expect_equal(un_tabyl(adorn_percentages(source1)), # default parameter is denom = "row"
               data.frame(cyl = c(4, 6, 8),
                          `0` = c(3/11, 4/7, 12/14),
                          `1` = c(8/11, 3/7, 2/14),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
  expect_equal(un_tabyl(adorn_percentages(source1, denom = "col")),
               data.frame(cyl = c(4, 6, 8),
                          `0` = c(3/19, 4/19, 12/19),
                          `1` = c(8/13, 3/13, 2/13),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
  expect_equal(un_tabyl(adorn_percentages(source1, denom = "all")),
               data.frame(cyl = c(4, 6, 8),
                          `0` = c(3/32, 4/32, 12/32),
                          `1` = c(8/32, 3/32, 2/32),
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
  )
})




})
