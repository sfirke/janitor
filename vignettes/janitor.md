Overview of janitor functions
================
2018-03-03

-   [Major functions](#major-functions)
    -   [Cleaning](#cleaning)
        -   [Clean data.frame names with `clean_names()`](#clean-data.frame-names-with-clean_names)
    -   [Exploring](#exploring)
        -   [`tabyl()` - a better version of `table()`](#tabyl---a-better-version-of-table)
        -   [Explore records with duplicated values for specific combinations of variables with `get_dupes()`](#explore-records-with-duplicated-values-for-specific-combinations-of-variables-with-get_dupes)
-   [Minor functions](#minor-functions)
    -   [Cleaning](#cleaning-1)
        -   [Fix dates stored as serial numbers with `excel_numeric_to_date()`](#fix-dates-stored-as-serial-numbers-with-excel_numeric_to_date)
        -   [`remove_empty()` rows and columns](#remove_empty-rows-and-columns)
    -   [Exploring](#exploring-1)
        -   [Count factor levels in groups of high, medium, and low with `top_levels()`](#count-factor-levels-in-groups-of-high-medium-and-low-with-top_levels)

The janitor functions expedite the initial data exploration and cleaning that comes with any new data set. This catalog describes the usage for each function.

Major functions
===============

Functions for everyday use.

Cleaning
--------

### Clean data.frame names with `clean_names()`

Call this function every time you read data.

It works in a `%>%` pipeline, and handles problematic variable names, especially those that are so well preserved by `readxl::read_excel()` and `readr::read_csv()`.

-   Parses letter cases and separators to a consistent format.
    -   Default is to snake\_case, but other cases like camelCase are available
-   Handles special characters and spaces, including transilerating characters like `œ` to `oe`.
-   Appends numbers to duplicated names
-   Converts "%" to "percent" and "\#" to "number" to retain meaning
-   Spacing (or lack of) around numbers is preserved

``` r
# Load dplyr for the %>% pipe
library(dplyr)
# Create a data.frame with dirty names
test_df <- as.data.frame(matrix(ncol = 6))
names(test_df) <- c("hIgHlo", "REPEAT VALUE", "REPEAT VALUE",
                    "% successful (2009)",  "abc@!*", "")
```

Clean the variable names, returning a data.frame:

``` r
test_df %>%
  clean_names()
#>   h_ig_hlo repeat_value repeat_value_2 percent_successful_2009 abc  x
#> 1       NA           NA             NA                      NA  NA NA
```

Compare to what base R produces:

``` r
make.names(names(test_df))
#> [1] "hIgHlo"               "REPEAT.VALUE"         "REPEAT.VALUE"        
#> [4] "X..successful..2009." "abc..."               "X"
```

Exploring
---------

### `tabyl()` - a better version of `table()`

`tabyl()` is a tidyverse-oriented replacement for `table()`. It counts combinations of one, two, or three variables, and then can be formatted with a suite of `adorn_*` functions to look just how you want. For instance:

``` r
mtcars %>%
  tabyl(gear, cyl) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  adorn_title()
#>              cyl                                    
#>  gear          4          6           8        Total
#>     3  6.67% (1) 13.33% (2) 80.00% (12) 100.00% (15)
#>     4 66.67% (8) 33.33% (4)  0.00%  (0) 100.00% (12)
#>     5 40.00% (2) 20.00% (1) 40.00%  (2) 100.00%  (5)
```

Learn more in the [tabyls vignette](https://github.com/sfirke/janitor/blob/master/vignettes/tabyls.md).

### Explore records with duplicated values for specific combinations of variables with `get_dupes()`

This is for hunting down and examining duplicate records during data cleaning - usually when there shouldn't be any.

For example, in a tidy data frame you might expect to have a unique ID repeated for each year, and year repeated for each unique ID, but no duplicated pairs of unique ID & year. Say you want to check for their presence, and study any such duplicated records.

`get_dupes()` returns the records (and inserts a count of duplicates) so you can sleuth out the problematic cases:

``` r
get_dupes(mtcars, wt, cyl) # or mtcars %>% get_dupes(wt, cyl) if you prefer to pipe
#> # A tibble: 4 x 12
#>      wt   cyl dupe_count   mpg  disp    hp  drat  qsec    vs    am  gear
#>   <dbl> <dbl>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  3.44  6.00          2  19.2   168   123  3.92  18.3  1.00  0     4.00
#> 2  3.44  6.00          2  17.8   168   123  3.92  18.9  1.00  0     4.00
#> 3  3.57  8.00          2  14.3   360   245  3.21  15.8  0     0     3.00
#> 4  3.57  8.00          2  15.0   301   335  3.54  14.6  0     1.00  5.00
#> # ... with 1 more variable: carb <dbl>
```

Minor functions
===============

Smaller functions for use in particular situations. More human-readable than the equivalent code they replace.

Cleaning
--------

### Fix dates stored as serial numbers with `excel_numeric_to_date()`

Ever load data from Excel and see a value like `42223` where a date should be? This function converts those serial numbers to class `Date`, and contains an option for specifying the alternate date system for files created with Excel for Mac 2008 and earlier versions (which count from a different starting point).

``` r
excel_numeric_to_date(41103)
#> [1] "2012-07-13"
excel_numeric_to_date(41103, date_system = "mac pre-2011")
#> [1] "2016-07-14"
```

### `remove_empty()` rows and columns

Does what it says. For cases like cleaning Excel files containing empty rows and columns.

``` r
q <- data.frame(v1 = c(1, NA, 3),
                v2 = c(NA, NA, NA),
                v3 = c("a", NA, "b"))
q %>%
  remove_empty(c("rows", "cols"))
#>   v1 v3
#> 1  1  a
#> 3  3  b
```

Just a simple wrapper for one-line functions, but improves readability of the code and saves a little thinking.

Exploring
---------

### Count factor levels in groups of high, medium, and low with `top_levels()`

Originally designed for use with Likert survey data stored as factors. Returns a `tbl_df` frequency table with appropriately-named rows, grouped into head/middle/tail groups.

-   Takes a user-specified size for the head/tail groups
-   Automatically calculates a percent column
-   Supports sorting
-   Can show or hide `NA` values.

``` r
f <- factor(c("strongly agree", "agree", "neutral", "neutral", "disagree", "strongly agree"),
            levels = c("strongly agree", "agree", "neutral", "disagree", "strongly disagree"))
top_levels(f)
#>                            f n   percent
#>        strongly agree, agree 3 0.5000000
#>                      neutral 2 0.3333333
#>  disagree, strongly disagree 1 0.1666667
top_levels(f, n = 1)
#>                         f n   percent
#>            strongly agree 2 0.3333333
#>  agree, neutral, disagree 4 0.6666667
#>         strongly disagree 0 0.0000000
```
