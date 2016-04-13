<!-- README.md is generated from README.Rmd. Please edit that file -->
janitor
=======

janitor helps clean data. It's designed for use with the `%>%` pipe from [magrittr](https://github.com/smbache/magrittr) to fix common dirty data issues as you load data. Functions are human-readable - think `clean_names()` instead of re-using code like`gsub("[.]+", "_", .)` ...

``` r
if (!require("pacman")) install.packages("pacman"); library(pacman) # pacman rules
p_load(janitor, dplyr, readr)
starting_df <- read_csv("http://www.github.com/sfirke/janitor/sample/dirty_data.csv")

# names are malformed and duplicate names will cause dplyr calls to fail
names(starting_df)

clean_df <- starting_df %>%
  clean_names %>%
  remove_empty_rows %>%
  remove_empty_cols

# names are now clean
names(clean_df)
```

Overview
--------

The janitor functions are:

-   Clean data.frame names with `clean_names()`.

-   Remove entirely empty rows with `remove_empty_rows()` and empty columns with `remove_empty_cols()`.

Installation
------------

janitor isn't on CRAN yet. Install the development version from GitHub:

``` r
# install.packages("devtools")
install_github("sfirke/janitor")
```
