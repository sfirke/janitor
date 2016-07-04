Intro to janitor Functions
================
2016-07-04

-   [List of examining functions](#list-of-examining-functions)
-   [List of cleaning functions](#list-of-cleaning-functions)
-   [Examining data with janitor](#examining-data-with-janitor)
    -   [`tabyl()` - a better version of `table()`](#tabyl---a-better-version-of-table)
    -   [Crosstabulate two variables with `crosstab()`](#crosstabulate-two-variables-with-crosstab)
    -   [Explore records with duplicated values for specific combinations of variables with `get_dupes()`](#explore-records-with-duplicated-values-for-specific-combinations-of-variables-with-get_dupes)
    -   [Look at factors grouped into high, medium, and low groups with `top_levels()`](#look-at-factors-grouped-into-high-medium-and-low-groups-with-top_levels)
-   [Cleaning data with janitor](#cleaning-data-with-janitor)
    -   [Clean data.frame names with `clean_names()`](#clean-data.frame-names-with-clean_names)
    -   [Use `excel_numeric_to_date()` to fix dates stored as serial numbers](#use-excel_numeric_to_date-to-fix-dates-stored-as-serial-numbers)
    -   [Use `convert_to_NA()` to clean should-be NA values](#use-convert_to_na-to-clean-should-be-na-values)
    -   [`remove_empty_cols()` and `remove_empty_rows()`](#remove_empty_cols-and-remove_empty_rows)

> Data scientists, according to interviews and expert estimates, spend from 50 percent to 80 percent of their time mired in this more mundane labor of collecting and preparing unruly digital data, before it can be explored for useful nuggets.
>
> -- *"[For Big-Data Scientists, 'Janitor Work' Is Key Hurdle to Insight](http://www.nytimes.com/2014/08/18/technology/for-big-data-scientists-hurdle-to-insights-is-janitor-work.html)" - The New York Times, 2014*

The janitor package has functions to expedite the initial data exploration and cleaning that comes with any new data set.

### List of examining functions

-   `tabyl()` - an enhanced replacement for `table()`
-   `crosstab()`
-   `get_dupes()`
-   `top_levels()`

### List of cleaning functions

-   `clean_names()`
-   `convert_to_NA()`
-   `excel_numeric_to_date()`
-   `remove_empty_cols()` and `remove_empty_rows()`

Examining data with janitor
===========================

`tabyl()` - a better version of `table()`
-----------------------------------------

`tabyl()` takes a vector and returns a frequency table, like `table()`. But its additional features are:

-   It returns a data.frame (actually, a `tbl_df`) - for sending to `ggplot()` or `kable()`, or manipulating further
-   It automatically calculates percentages
-   It can (optionally) display `NA` values
    -   When `NA` values are present, it will calculate an additional column `valid_percent` in the style of SPSS
-   It can (optionally) sort on counts

``` r
x <- c("a", "b", "c", "c", NA)
tabyl(x)
#> Source: local data frame [4 x 4]
#> 
#>       x     n percent valid_percent
#>   <chr> <int>   <dbl>         <dbl>
#> 1     a     1     0.2          0.25
#> 2     b     1     0.2          0.25
#> 3     c     2     0.4          0.50
#> 4  <NA>     1     0.2            NA
```

Compare to:

``` r
table(x)
#> x
#> a b c 
#> 1 1 2
```

Crosstabulate two variables with `crosstab()`
---------------------------------------------

`crosstab()` generates a crosstab table. There many crosstab functions already; this one is distinguished by: + It returns a data.frame (actually, a `tbl_df`) + It is simple. + It calculates frequencies by default but can calculate row, column, and table-wise percentages. + It can (optionally) display `NA` values

It wraps the common pipeline of `group_by %>% summarise %>% mutate %>% spread` from the dplyr and tidyr packages, often used in exploratory analysis.

``` r
y <- c(1, 1, 2, 1, 2)
x <- c("a", "a", "b", "b", NA)

crosstab(x, y)
#> Source: local data frame [3 x 3]
#> 
#>       x     1     2
#> * <chr> <int> <int>
#> 1     a     2    NA
#> 2     b     1     1
#> 3  <NA>    NA     1
crosstab(x, y, percent = "row")
#> Source: local data frame [3 x 3]
#> 
#>       x     1     2
#> * <chr> <dbl> <dbl>
#> 1     a   1.0    NA
#> 2     b   0.5   0.5
#> 3  <NA>    NA   1.0
```

This gives the same result as the much longer pipeline:

``` r
library(dplyr) ; library(tidyr)
#> Warning: package 'dplyr' was built under R version 3.3.1
data_frame(x, y) %>%
  group_by(x, y) %>%
  tally() %>%
  mutate(percent = n / sum(n, na.rm = TRUE)) %>%
  select(-n) %>%
  spread(y, percent) %>%
  ungroup()
```

And is more featured than the base R equivalents:

``` r
table(x, y)
prop.table(table(x, y), 1)
```

Explore records with duplicated values for specific combinations of variables with `get_dupes()`
------------------------------------------------------------------------------------------------

This is a function for hunting down and examining duplicate records during data cleaning - usually when there shouldn't be any.

E.g., in a tidy data frame you might have a unique ID repeated for each year, and year repeated for each unique ID, but you might want to check for duplicated pairs of unique ID & year - what do these duplicated records have in common?

`get_dupes()` returns the records (and inserts a count of duplicates) so you can sleuth out the problematic cases:

``` r
get_dupes(mtcars, wt, cyl)
#> Source: local data frame [4 x 12]
#> 
#>      wt   cyl dupe_count   mpg  disp    hp  drat  qsec    vs    am  gear
#>   <dbl> <dbl>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  3.44     6          2  19.2 167.6   123  3.92 18.30     1     0     4
#> 2  3.44     6          2  17.8 167.6   123  3.92 18.90     1     0     4
#> 3  3.57     8          2  14.3 360.0   245  3.21 15.84     0     0     3
#> 4  3.57     8          2  15.0 301.0   335  3.54 14.60     0     1     5
#> Variables not shown: carb <dbl>.
```

Look at factors grouped into high, medium, and low groups with `top_levels()`
-----------------------------------------------------------------------------

Originally designed for use with Likert survey data stored as factors. Returns a `tbl_df` frequency table with appropriately-named rows, grouped into head/middle/tail groups.

-   Takes a user-specified size for the head/tail groups
-   Automatically calculates a percent column
-   Supports sorting
-   Can show or hide `NA` values.

``` r
f <- factor(c("strongly agree", "agree", "neutral", "neutral", "disagree", "strongly agree"),
            levels = c("strongly agree", "agree", "neutral", "disagree", "strongly disagree"))
top_levels(f)
#> Source: local data frame [3 x 3]
#> 
#>                             f     n   percent
#>                        <fctr> <int>     <dbl>
#> 1       strongly agree, agree     3 0.5000000
#> 2                     neutral     2 0.3333333
#> 3 disagree, strongly disagree     1 0.1666667
top_levels(f, n = 1, sort = TRUE)
#> Source: local data frame [3 x 3]
#> 
#>                          f     n   percent
#>                     <fctr> <int>     <dbl>
#> 1 agree, neutral, disagree     4 0.6666667
#> 2           strongly agree     2 0.3333333
#> 3        strongly disagree    NA        NA
```

Cleaning data with janitor
==========================

Clean data.frame names with `clean_names()`
-------------------------------------------

Call this function every time you read data.

It works in a `%>%` pipeline, and handles the problematic variable names that are so well preserved by `readxl::read_excel()` and `readr::read_csv()`.

-   Returns names with only lowercase letters, with `_` as a separator
-   Handles special characters and spaces
-   Appends numbers to duplicated names
-   Converts "%" to "percent" to retain meaning

``` r
# Load dplyr for the %>% pipe
library(dplyr)
# Create a data.frame with dirty names
test_df <- data.frame(matrix(ncol = 6) %>% as.data.frame())
names(test_df) <- c("two words", "repeat value", "REPEAT VALUE", "% successful (2009)",  "abc@!*", "")

clean_df <- test_df %>% clean_names()
names(clean_df) # they are clean
#> [1] "two_words"               "repeat_value"           
#> [3] "repeat_value_2"          "percent_successful_2009"
#> [5] "abc"                     "x"
```

Use `excel_numeric_to_date()` to fix dates stored as serial numbers
-------------------------------------------------------------------

Sometimes you'll load data from Excel and see `42223` where a date should be. This function converts those serial numbers to class `Date`, and contains an option for specifying the alternate date system for files created with Excel for Mac 2008 and earlier versions (which count from a different starting point).

``` r
excel_numeric_to_date(41103)
#> [1] "2012-07-13"
excel_numeric_to_date(41103, date_system = "mac pre-2011")
#> [1] "2016-07-14"
```

Use `convert_to_NA()` to clean should-be NA values
--------------------------------------------------

Converts instances of user-specified strings into `NA` values. It takes an argument `dat`, which can be either a vector, a data.frame, or a `tibble::tbl_df`, and will return that same type with the substitutions made.

``` r
convert_to_NA(letters[1:5], c("b", "d"))
#> [1] "a" NA  "c" NA  "e"
```

`remove_empty_cols()` and `remove_empty_rows()`
-----------------------------------------------

One-line wrapper functions that do what they say. For cases like cleaning Excel files with empty rows and columns.

``` r
q <- data.frame(v1 = c(1, NA, 3),
                v2 = c(NA, NA, NA),
                v3 = c("a", NA, "b"))
q %>%
  remove_empty_cols() %>%
  remove_empty_rows()
#>   v1 v3
#> 1  1  a
#> 3  3  b
```
