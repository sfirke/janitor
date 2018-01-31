## ---- echo = FALSE, message = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
library(janitor)

## ---- message = FALSE, warning = FALSE-----------------------------------
# Load dplyr for the %>% pipe
library(dplyr)
# Create a data.frame with dirty names
test_df <- as.data.frame(matrix(ncol = 6))
names(test_df) <- c("hIgHlo", "REPEAT VALUE", "REPEAT VALUE",
                    "% successful (2009)",  "abc@!*", "")

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
  adorn_ns()

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
  remove_empty_cols() %>%
  remove_empty_rows()

## ------------------------------------------------------------------------
f <- factor(c("strongly agree", "agree", "neutral", "neutral", "disagree", "strongly agree"),
            levels = c("strongly agree", "agree", "neutral", "disagree", "strongly disagree"))
top_levels(f)
top_levels(f, n = 1)

