## ----chunk_options, include = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")

## ----one_way-------------------------------------------------------------
library(janitor)
t1 <- mtcars %>%
  tabyl(cyl)

t1

## ----one_way_vector------------------------------------------------------
x <- c("big", "big", "small", "small", "small", NA)
tabyl(x)

## ----one_way_adorns------------------------------------------------------
t1 %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

## ----two_way-------------------------------------------------------------
t2 <- mtcars %>%
  tabyl(cyl, carb)

t2

## ----two_way_adorns------------------------------------------------------

t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

## ----three_Way-----------------------------------------------------------
t3 <- mtcars %>%
  tabyl(cyl, carb, am)

t3 # the result is a tabyl of cyl x carb, split into a list by the values of am

## ----three_way_adorns----------------------------------------------------
library(purrr)
mtcars %>%
  tabyl(carb, am, cyl, show_missing_levels = FALSE) %>%
  map(adorn_totals, "row") %>%
  map(adorn_percentages, "row") %>%
  map(adorn_pct_formatting, digits = 1) %>%
  map(adorn_ns)


## ------------------------------------------------------------------------
mtcars %>%
  tabyl(cyl, gear) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  knitr::kable()


## ----first_non_tabyl, warning = FALSE, message = FALSE-------------------
library(dplyr)
percent_above_20_mpg <- mtcars %>%
  group_by(cyl) %>%
  summarise(pct_above_20_mpg = mean(mpg > 20))

percent_above_20_mpg %>%
  adorn_pct_formatting()

## ----more_non_tabyls, warning = FALSE, message = FALSE-------------------
library(tidyr) # for spread()
base_table <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(mpg = mean(mpg)) %>%
  spread(am, mpg)

base_table

## ----add_the_Ns----------------------------------------------------------
base_table %>%
  adorn_rounding() %>%
  adorn_ns(
    ns = mtcars %>% # calculate the Ns on the fly by calling tabyl on the original data
      tabyl(cyl, am)
  )

