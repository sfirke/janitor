## ----chunk_options, include = FALSE--------------------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")

## ----clean_starwars, warning = FALSE, message = FALSE--------------------
library(dplyr)
humans <- starwars %>%
  filter(species == "Human")

## ----one_way-------------------------------------------------------------
library(janitor)

t1 <- humans %>%
  tabyl(eye_color)

t1

## ----one_way_vector------------------------------------------------------
x <- c("big", "big", "small", "small", "small", NA)
tabyl(x)

## ----one_way_adorns------------------------------------------------------
t1 %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

## ----two_way-------------------------------------------------------------
t2 <- humans %>%
  tabyl(gender, eye_color)

t2

## ----two_way_adorns------------------------------------------------------

t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

## ----three_Way-----------------------------------------------------------
t3 <- humans %>%
  tabyl(eye_color, skin_color, gender)

# the result is a tabyl of eye color x skin color, split into a list by gender
t3 

## ----three_way_adorns, warning = FALSE, message = FALSE------------------
library(purrr)
humans %>%
  tabyl(eye_color, skin_color, gender, show_missing_levels = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title


## ------------------------------------------------------------------------
humans %>%
  tabyl(gender, eye_color) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()


## ----first_non_tabyl-----------------------------------------------------
percent_above_165_cm <- humans %>%
  group_by(gender) %>%
  summarise(pct_above_165_cm = mean(height > 165, na.rm = TRUE))

percent_above_165_cm %>%
  adorn_pct_formatting()

## ----more_non_tabyls, warning = FALSE, message = FALSE-------------------
library(tidyr) # for spread()
mpg_by_cyl_and_am <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(mpg = mean(mpg)) %>%
  spread(am, mpg)

mpg_by_cyl_and_am

## ----add_the_Ns----------------------------------------------------------
mpg_by_cyl_and_am %>%
  adorn_rounding() %>%
  adorn_ns(
    ns = mtcars %>% # calculate the Ns on the fly by calling tabyl on the original data
      tabyl(cyl, am)
  ) %>%
  adorn_title("combined", row_name = "Cylinders", col_name = "Is Automatic")

