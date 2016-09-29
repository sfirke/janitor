Intro to janitor functions
================
2016-09-29

-   [Major functions](#major-functions)
    -   [Clean data.frame names with `clean_names()`](#clean-data.frame-names-with-clean_names)
    -   [`tabyl()` - a better version of `table()`](#tabyl---a-better-version-of-table)
    -   [Crosstabulate two variables with `crosstab()`](#crosstabulate-two-variables-with-crosstab)
    -   [Format a crosstab table with `adorn_crosstab()`](#format-a-crosstab-table-with-adorn_crosstab)
    -   [Explore records with duplicated values for specific combinations of variables with `get_dupes()`](#explore-records-with-duplicated-values-for-specific-combinations-of-variables-with-get_dupes)
-   [Minor functions](#minor-functions)
    -   [`use_first_valid_of()` replaces nested `ifelse` statements for combining variables](#use_first_valid_of-replaces-nested-ifelse-statements-for-combining-variables)
    -   [Use `convert_to_NA()` to clean should-be NA values](#use-convert_to_na-to-clean-should-be-na-values)
    -   [Fix dates stored as serial numbers with `excel_numeric_to_date()`](#fix-dates-stored-as-serial-numbers-with-excel_numeric_to_date)
    -   [`remove_empty_cols()` and `remove_empty_rows()`](#remove_empty_cols-and-remove_empty_rows)
    -   [`add_totals_col()` and `add_totals_row()`](#add_totals_col-and-add_totals_row)
    -   [Convert a data.frame of numbers to percentages with `ns_to_percents()`](#convert-a-data.frame-of-numbers-to-percentages-with-ns_to_percents)
    -   [Count factor levels in groups of high, medium, and low with `top_levels()`](#count-factor-levels-in-groups-of-high-medium-and-low-with-top_levels)

The janitor functions expedite the initial data exploration and cleaning that comes with any new data set.

Major functions
===============

Functions for frequent use in everyday data cleaning.

Clean data.frame names with `clean_names()`
-------------------------------------------

Call this function every time you read data.

It works in a `%>%` pipeline, and handles problematic variable names, especially those that are so well preserved by `readxl::read_excel()` and `readr::read_csv()`.

-   Returns names with only lowercase letters, with `_` as a separator
-   Handles special characters and spaces
-   Appends numbers to duplicated names
-   Converts "%" to "percent" to retain meaning

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
#>   highlo repeat_value repeat_value_2 percent_successful_2009 abc  x
#> 1     NA           NA             NA                      NA  NA NA
```

Compare to what base R produces:

``` r
make.names(names(test_df))
#> [1] "hIgHlo"               "REPEAT.VALUE"         "REPEAT.VALUE"        
#> [4] "X..successful..2009." "abc..."               "X"
```

`tabyl()` - a better version of `table()`
-----------------------------------------

`tabyl()` takes a vector and returns a frequency table, like `table()`. But its additional features are:

-   It returns a data.frame - for manipulating further, or printing with `knitr::kable()`.
-   It automatically calculates percentages
-   It can (optionally) display `NA` values
    -   When `NA` values are present, it will calculate an additional column `valid_percent` in the style of SPSS
-   It can (optionally) sort on counts
-   It can be called with `%>%` in a pipeline
-   When called on a factor, it will include missing levels in the result (levels not present in the vector)

Usage:

``` r
x <- c("a", "b", "c", "c", NA)
tabyl(x, sort = TRUE)
#>      x n percent valid_percent
#> 1    c 2     0.4          0.50
#> 2    a 1     0.2          0.25
#> 3    b 1     0.2          0.25
#> 4 <NA> 1     0.2            NA
```

Compare to:

``` r
table(x)
#> x
#> a b c 
#> 1 1 2
```

`tabyl()` can be called on a piped-in data.frame, which allows for fast, flexible exploration of data:

``` r
mtcars %>%
  filter(gear > 3) %>%
  tabyl(cyl)
#>   cyl  n   percent
#> 1   4 10 0.5882353
#> 2   6  5 0.2941176
#> 3   8  2 0.1176471
```

Crosstabulate two variables with `crosstab()`
---------------------------------------------

`crosstab()` generates a crosstab table. There many R crosstab functions already; this one is distinguished by:

-   It returns a data.frame
-   It is simple.
    -   It calculates frequencies by default but can calculate row, column, and table-wise percentages.
    -   It can (optionally) display `NA` values
-   It can be called with `%>%` in a pipeline

Usage:

``` r
y <- c(1, 1, 2, 1, 2)
x <- c("a", "a", "b", "b", NA)

crosstab(x, y)
#>      x 1 2
#> 1    a 2 0
#> 2    b 1 1
#> 3 <NA> 0 1
crosstab(x, y, percent = "row")
#>      x   1   2
#> 1    a 1.0 0.0
#> 2    b 0.5 0.5
#> 3 <NA> 0.0 1.0
```

If the variables are in the same data frame, call `crosstab` with the `%>%` pipe:

``` r
dat <- data.frame(x, y)
dat %>%
  crosstab(x, y, percent = "row")
#>      x   1   2
#> 1    a 1.0 0.0
#> 2    b 0.5 0.5
#> 3 <NA> 0.0 1.0
```

This function wraps the common pipeline of `group_by %>% summarise %>% mutate %>% spread` from the dplyr and tidyr packages, often used in exploratory analysis. The simple `crosstab` call above produces the same result\* as this much longer pipeline:

``` r
library(dplyr) ; library(tidyr)
dat %>%
  group_by(x, y) %>%
  tally() %>%
  mutate(percent = n / sum(n, na.rm = TRUE)) %>%
  select(-n) %>%
  spread(y, percent, fill = 0) %>%
  ungroup()
```

And is more featured than the base R equivalents `table(dat$x, dat$y)` and `prop.table(table(dat$x, dat$y), 1)`.

\**not exactly: the long pipeline returns a `tibble`, while crosstab() returns a `data.frame` that prints fully in the console.*

Format a crosstab table with `adorn_crosstab()`
-----------------------------------------------

Builds off of `crosstab()` to approximate the functionality of a quick Microsoft Excel PivotTable. It prints an elegant result, either for interactive analysis or for sharing in a report, e.g., with `knitr::kable()`. The simple default call yields:

``` r
mtcars %>%
  crosstab(cyl, gear) %>%
  adorn_crosstab()
#>   cyl          3         4         5
#> 1   4  9.1%  (1) 72.7% (8) 18.2% (2)
#> 2   6 28.6%  (2) 57.1% (4) 14.3% (1)
#> 3   8 85.7% (12)  0.0% (0) 14.3% (2)
```

The user can specify additional formatting options:

-   Percentages can be calculated by row, column, or overall
-   Display only percentages, or show Ns in parentheses
-   Control how many digits of the percentages to display
-   Display a totals row, column, or both
-   Round percentages either with the default `round()` function, or round-half-to-up using a [custom rounding function](http://stackoverflow.com/a/12688836/4470365)
    -   e.g., round 10.5 up to 11, consistent with Excel's tie-breaking behavior
    -   This contrasts with rounding 10.5 down to 10 as in base R's `round(10.5)`.

*When calling `crosstab()` to feed this function, leave the default argument `percent = "none"` so that the integer values are passed through.*

Explore records with duplicated values for specific combinations of variables with `get_dupes()`
------------------------------------------------------------------------------------------------

This is for hunting down and examining duplicate records during data cleaning - usually when there shouldn't be any.

For example, in a tidy data frame you might expect to have a unique ID repeated for each year, and year repeated for each unique ID, but no duplicated pairs of unique ID & year. Say you want to check for their presence, and study any such duplicated records.

`get_dupes()` returns the records (and inserts a count of duplicates) so you can sleuth out the problematic cases:

``` r
get_dupes(mtcars, wt, cyl) # or mtcars %>% get_dupes(wt, cyl) if you prefer to pipe
#> # A tibble: 4 × 12
#>      wt   cyl dupe_count   mpg  disp    hp  drat  qsec    vs    am  gear
#>   <dbl> <dbl>      <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  3.44     6          2  19.2 167.6   123  3.92 18.30     1     0     4
#> 2  3.44     6          2  17.8 167.6   123  3.92 18.90     1     0     4
#> 3  3.57     8          2  14.3 360.0   245  3.21 15.84     0     0     3
#> 4  3.57     8          2  15.0 301.0   335  3.54 14.60     0     1     5
#> # ... with 1 more variables: carb <dbl>
```

Minor functions
===============

Smaller functions for use in particular situations. More human-readable than the equivalent code they replace.

`use_first_valid_of()` replaces nested `ifelse` statements for combining variables
----------------------------------------------------------------------------------

Say that you have three different temperature sensors whose readings you want to collapse into one variable. Not all records have readings from each sensor. Sensor A is the most accurate, so you want to use that where available, but if it's missing you want Sensor B, and if that's missing, Sensor C.

The common R way to do this would be:

``` r
ifelse(!is.na(sensorA), sensorA,
       ifelse(!is.na(sensorB), sensorB,
              sensorC))
```

The function `use_first_valid_of()` replaces this with:

``` r
use_first_valid_of(sensorA, sensorB, sensorC)
```

One major improvement over the nested-`ifelse` statements: this function can combine factor and date variables, which [ifelse fails to handle](http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-date-objects-into-numeric-objects).

Use `convert_to_NA()` to clean should-be NA values
--------------------------------------------------

Converts instances of user-specified strings into `NA` values. It takes an argument `dat`, which can be either a vector, a data.frame, or a `tibble::tbl_df`, and will return that same type with the substitutions made.

Use if, say, you import an Excel file with values like `#N/A"` present in many columns.

``` r
convert_to_NA(letters[1:5], c("b", "d"))
#> [1] "a" NA  "c" NA  "e"
```

Fix dates stored as serial numbers with `excel_numeric_to_date()`
-----------------------------------------------------------------

Ever load data from Excel and see a value like `42223` where a date should be? This function converts those serial numbers to class `Date`, and contains an option for specifying the alternate date system for files created with Excel for Mac 2008 and earlier versions (which count from a different starting point).

``` r
excel_numeric_to_date(41103)
#> [1] "2012-07-13"
excel_numeric_to_date(41103, date_system = "mac pre-2011")
#> [1] "2016-07-14"
```

`remove_empty_cols()` and `remove_empty_rows()`
-----------------------------------------------

One-line wrapper functions that do what they say. For cases like cleaning Excel files containing empty rows and columns.

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

`add_totals_col()` and `add_totals_row()`
-----------------------------------------

These functions add a totals row or column to a data.frame. These functions exclude the first column of the input data.frame, assuming that it contains a descriptive variable not to be summed.

``` r
mtcars %>%
  crosstab(am, cyl) %>%
  add_totals_row %>%
  add_totals_col
#>      am  4 6  8 Total
#> 1     0  3 4 12    19
#> 2     1  8 3  2    13
#> 3 Total 11 7 14    32
```

Convert a data.frame of numbers to percentages with `ns_to_percents()`
----------------------------------------------------------------------

A helper function for `adorn_crosstab`, but can be called directly. Takes a data.frame of numerics and returns corresponding percentages of rows, columns, or the total sum of the data.frame. Like `prop.table`, except for data.frames, and skips the first column (which is assumed to contain a non-numeric descriptive variable).

``` r
mtcars %>%
  crosstab(cyl, am) %>%
  ns_to_percents("col")
#>   cyl         0         1
#> 1   4 0.1578947 0.6153846
#> 2   6 0.2105263 0.2307692
#> 3   8 0.6315789 0.1538462
```

Count factor levels in groups of high, medium, and low with `top_levels()`
--------------------------------------------------------------------------

Originally designed for use with Likert survey data stored as factors. Returns a `tbl_df` frequency table with appropriately-named rows, grouped into head/middle/tail groups.

-   Takes a user-specified size for the head/tail groups
-   Automatically calculates a percent column
-   Supports sorting
-   Can show or hide `NA` values.

``` r
f <- factor(c("strongly agree", "agree", "neutral", "neutral", "disagree", "strongly agree"),
            levels = c("strongly agree", "agree", "neutral", "disagree", "strongly disagree"))
top_levels(f)
#>                             f n   percent
#> 1       strongly agree, agree 3 0.5000000
#> 2                     neutral 2 0.3333333
#> 3 disagree, strongly disagree 1 0.1666667
top_levels(f, n = 1, sort = TRUE)
#>                          f n   percent
#> 1 agree, neutral, disagree 4 0.6666667
#> 2           strongly agree 2 0.3333333
#> 3        strongly disagree 0 0.0000000
```
