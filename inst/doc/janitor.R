## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(janitor)

## ---- message = FALSE, warning = FALSE-----------------------------------
# Create a data.frame with dirty names
test_df <- as.data.frame(matrix(ncol = 6))
names(test_df) <- c("firstName", "ábc@!*", "% successful (2009)",
                    "REPEAT VALUE", "REPEAT VALUE", "")

## ------------------------------------------------------------------------
test_df %>%
  clean_names()

## ------------------------------------------------------------------------
make.names(names(test_df))

## ------------------------------------------------------------------------
mtcars %>%
  tabyl(gear, cyl) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  adorn_title()

## ------------------------------------------------------------------------
get_dupes(mtcars, wt, cyl) # or mtcars %>% get_dupes(wt, cyl) if you prefer to pipe

## ------------------------------------------------------------------------
excel_numeric_to_date(41103)
excel_numeric_to_date(41103, date_system = "mac pre-2011")

## ------------------------------------------------------------------------
q <- data.frame(v1 = c(1, NA, 3),
                v2 = c(NA, NA, NA),
                v3 = c("a", NA, "b"))
q %>%
  remove_empty(c("rows", "cols"))

## ------------------------------------------------------------------------
nums <- c(2.5, 3.5)
round(nums)
round_half_up(nums)

## ------------------------------------------------------------------------
f <- factor(c("strongly agree", "agree", "neutral", "neutral", "disagree", "strongly agree"),
            levels = c("strongly agree", "agree", "neutral", "disagree", "strongly disagree"))
top_levels(f)
top_levels(f, n = 1)

