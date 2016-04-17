<!-- README.md is generated from README.Rmd. Please edit that file -->
janitor
=======

janitor helps clean up common dirty data problems. Its functions are human-readable - think `clean_names()` replacing a half-dozen lines like `setNames(., gsub("[.]+", "_", names(.)))`.

For maximum elegance, use janitor with the `%>%` pipe from [magrittr](https://github.com/smbache/magrittr), available after loading the [dplyr](https://github.com/hadley/dplyr) package.

Installation
------------

janitor is not yet on CRAN. Install the development version from GitHub:

``` r
# install.packages("devtools")
install_github("sfirke/janitor")
```

janitor in action
-----------------

Start with some dirty data:

``` r
# load demo packages using the pacman package
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(janitor, dplyr, readr)

# read sample dirty data file
starting_df <- read_csv("https://raw.github.com/sfirke/janitor/master/sample/dirty.csv")

# take a look at the messy data
dplyr::glimpse(starting_df)
#> Observations: 6
#> Variables: 11
#> $ Winning Team                 (chr) NA, "Narwhals", "Muskrats", NA, "...
#> $ Losing Team                  (chr) NA, "Ocelots", "Sharks", NA, "Bis...
#> $ Points Scored (winning team) (int) NA, 11, 12, NA, 7, 15
#> $ Points Scored (losing team)  (int) NA, 6, 4, NA, 0, 14
#> $ Winning Team % of Total      (dbl) NA, 0.65, 0.75, NA, 1.00, 0.52
#> $ 1st Half Total Pts           (int) NA, 10, 8, NA, 5, 17
#> $ NA                           (chr) NA, NA, NA, NA, NA, NA
#> $ # Penalties                  (int) NA, 2, 1, NA, 3, 2
#> $ Referee                      (chr) NA, "Einstein", "Galilei", NA, "M...
#> $ Referee                      (chr) NA, "Newton", "Curie", NA, NA, "L...
#> $ Match Date                   (int) NA, 42461, 42464, NA, 42459, 42467
```

This data.frame is dirty in several ways:

-   The 7th column is entirely `NA` values
-   There are several `NA` rows left by blank rows used in the layout of the .csv
-   Variable names contain spaces and other illegal name characters
-   The match date is stored as a serial number.
-   Duplicate names will cause dplyr calls to fail:

``` r
starting_df %>% mutate(year = 2016)
#> Error in eval(expr, envir, enclos): found duplicated column name: Referee
```

Now clean it with janitor:

``` r
clean_df <- starting_df %>%
  clean_names %>%
  remove_empty_rows %>%
  remove_empty_cols %>%
  mutate(match_date = excel_numeric_to_date(match_date))

# the data.frame is now clean, with proper names:
glimpse(clean_df)
#> Observations: 4
#> Variables: 10
#> $ winning_team                  (chr) "Narwhals", "Muskrats", "Clams",...
#> $ losing_team                   (chr) "Ocelots", "Sharks", "Bison", "W...
#> $ points_scored_winning_team    (int) 11, 12, 7, 15
#> $ points_scored_losing_team     (int) 6, 4, 0, 14
#> $ winning_team_percent_of_total (dbl) 0.65, 0.75, 1.00, 0.52
#> $ x1st_half_total_pts           (int) 10, 8, 5, 17
#> $ x_penalties                   (int) 2, 1, 3, 2
#> $ referee                       (chr) "Einstein", "Galilei", "Maxwell"...
#> $ referee_2                     (chr) "Newton", "Curie", NA, "Lamarr"
#> $ match_date                    (date) 2016-04-01, 2016-04-04, 2016-03...
```

Overview
--------

The janitor functions are:

-   Clean data.frame names with `clean_names()`.

-   Remove entirely empty rows with `remove_empty_rows()` and empty columns with `remove_empty_cols()`.

-   Convert dates incorrectly stored as serial numbers to Date class with `excel_numeric_to_date()`, e.g., turning `42500` into `"2016-05-10"`.
