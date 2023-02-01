Overview of janitor functions
================
2023-02-01

- <a href="#major-functions" id="toc-major-functions">Major functions</a>
  - <a href="#cleaning" id="toc-cleaning">Cleaning</a>
    - <a href="#clean-dataframe-names-with-clean_names"
      id="toc-clean-dataframe-names-with-clean_names">Clean data.frame names
      with <code>clean_names()</code></a>
    - <a href="#do-those-dataframes-actually-contain-the-same-columns"
      id="toc-do-those-dataframes-actually-contain-the-same-columns">Do those
      data.frames actually contain the same columns?</a>
  - <a href="#exploring" id="toc-exploring">Exploring</a>
    - <a href="#tabyl---a-better-version-of-table"
      id="toc-tabyl---a-better-version-of-table"><code>tabyl()</code> - a
      better version of <code>table()</code></a>
    - <a
      href="#explore-records-with-duplicated-values-for-specific-combinations-of-variables-with-get_dupes"
      id="toc-explore-records-with-duplicated-values-for-specific-combinations-of-variables-with-get_dupes">Explore
      records with duplicated values for specific combinations of variables
      with <code>get_dupes()</code></a>
    - <a href="#explore-relationships-between-columns-with-get_one_to_one"
      id="toc-explore-relationships-between-columns-with-get_one_to_one">Explore
      relationships between columns with <code>get_one_to_one()</code></a>
- <a href="#minor-functions" id="toc-minor-functions">Minor functions</a>
  - <a href="#cleaning-1" id="toc-cleaning-1">Cleaning</a>
    - <a href="#manipulate-vectors-of-names-with-make_clean_names"
      id="toc-manipulate-vectors-of-names-with-make_clean_names">Manipulate
      vectors of names with <code>make_clean_names()</code></a>
    - <a href="#validate-that-a-column-has-a-single_value-per-group"
      id="toc-validate-that-a-column-has-a-single_value-per-group">Validate
      that a column has a <code>single_value()</code> per group</a>
    - <a href="#remove_empty-rows-and-columns"
      id="toc-remove_empty-rows-and-columns"><code>remove_empty()</code> rows
      and columns</a>
    - <a href="#remove_constant-columns"
      id="toc-remove_constant-columns"><code>remove_constant()</code>
      columns</a>
    - <a href="#directionally-consistent-rounding-behavior-with-round_half_up"
      id="toc-directionally-consistent-rounding-behavior-with-round_half_up">Directionally-consistent
      rounding behavior with <code>round_half_up()</code></a>
    - <a
      href="#round-decimals-to-precise-fractions-of-a-given-denominator-with-round_to_fraction"
      id="toc-round-decimals-to-precise-fractions-of-a-given-denominator-with-round_to_fraction">Round
      decimals to precise fractions of a given denominator with
      <code>round_to_fraction()</code></a>
    - <a href="#fix-dates-stored-as-serial-numbers-with-excel_numeric_to_date"
      id="toc-fix-dates-stored-as-serial-numbers-with-excel_numeric_to_date">Fix
      dates stored as serial numbers with
      <code>excel_numeric_to_date()</code></a>
    - <a href="#convert-a-mix-of-date-and-datetime-formats-to-date"
      id="toc-convert-a-mix-of-date-and-datetime-formats-to-date">Convert a
      mix of date and datetime formats to date</a>
    - <a href="#elevate-column-names-stored-in-a-dataframe-row"
      id="toc-elevate-column-names-stored-in-a-dataframe-row">Elevate column
      names stored in a data.frame row</a>
    - <a href="#find-the-header-row-buried-within-a-messy-dataframe"
      id="toc-find-the-header-row-buried-within-a-messy-dataframe">Find the
      header row buried within a messy data.frame</a>
  - <a href="#exploring-1" id="toc-exploring-1">Exploring</a>
    - <a
      href="#count-factor-levels-in-groups-of-high-medium-and-low-with-top_levels"
      id="toc-count-factor-levels-in-groups-of-high-medium-and-low-with-top_levels">Count
      factor levels in groups of high, medium, and low with
      <code>top_levels()</code></a>

The janitor functions expedite the initial data exploration and cleaning
that comes with any new data set. This catalog describes the usage for
each function.

# Major functions

Functions for everyday use.

## Cleaning

### Clean data.frame names with `clean_names()`

Call this function every time you read data.

It works in a `%>%` pipeline, and handles problematic variable names,
especially those that are so well-preserved by `readxl::read_excel()`
and `readr::read_csv()`.

- Parses letter cases and separators to a consistent format.
  - Default is to snake_case, but other cases like camelCase are
    available
- Handles special characters and spaces, including transliterating
  characters like `œ` to `oe`.
- Appends numbers to duplicated names
- Converts “%” to “percent” and “\#” to “number” to retain meaning
- Spacing (or lack thereof) around numbers is preserved

``` r
# Create a data.frame with dirty names
test_df <- as.data.frame(matrix(ncol = 6))
names(test_df) <- c("firstName", "ábc@!*", "% successful (2009)",
                    "REPEAT VALUE", "REPEAT VALUE", "")
```

Clean the variable names, returning a data.frame:

``` r
test_df %>%
  clean_names()
#>   first_name abc percent_successful_2009 repeat_value repeat_value_2  x
#> 1         NA  NA                      NA           NA             NA NA
```

Compare to what base R produces:

``` r
make.names(names(test_df))
#> [1] "firstName"            "ábc..."               "X..successful..2009." "REPEAT.VALUE"         "REPEAT.VALUE"        
#> [6] "X"
```

This function is powered by the underlying exported function
**`make_clean_names()`**, which accepts and returns a character vector
of names (see below). This allows for cleaning the names of *any*
object, not just a data.frame. `clean_names()` is retained for its
convenience in piped workflows, and can be called on an `sf` simple
features object or a `tbl_graph` tidygraph object in addition to a
data.frame.

### Do those data.frames actually contain the same columns?

#### Check with `compare_df_cols()`

For cases when you are given a set of data files that *should* be
identical, and you wish to read and combine them for analysis. But then
`dplyr::bind_rows()` or `rbind()` fails, because of different columns or
because the column classes don’t match across data.frames.

`compare_df_cols()` takes unquoted names of data.frames / tibbles, or a
list of data.frames, and returns a summary of how they compare. See what
the column types are, which are missing or present in the different
inputs, and how column types differ.

``` r
df1 <- data.frame(a = 1:2, b = c("big", "small"))
df2 <- data.frame(a = 10:12, b = c("medium", "small", "big"), c = 0, stringsAsFactors = TRUE) # here, column b is a factor
df3 <- df1 %>%
  dplyr::mutate(b = as.character(b))

compare_df_cols(df1, df2, df3)
#>   column_name       df1     df2       df3
#> 1           a   integer integer   integer
#> 2           b character  factor character
#> 3           c      <NA> numeric      <NA>

compare_df_cols(df1, df2, df3, return = "mismatch")
#>   column_name       df1    df2       df3
#> 1           b character factor character
compare_df_cols(df1, df2, df3, return = "mismatch", bind_method = "rbind") # default is dplyr::bind_rows
#>   column_name       df1     df2       df3
#> 1           b character  factor character
#> 2           c      <NA> numeric      <NA>
```

`compare_df_cols_same()` returns `TRUE` or `FALSE` indicating if the
data.frames can be successfully row-bound with the given binding method:

``` r
compare_df_cols_same(df1, df3)
#> [1] TRUE
compare_df_cols_same(df2, df3)
#>   column_name    ..1       ..2
#> 1           b factor character
#> [1] FALSE
```

## Exploring

### `tabyl()` - a better version of `table()`

`tabyl()` is a tidyverse-oriented replacement for `table()`. It counts
combinations of one, two, or three variables, and then can be formatted
with a suite of `adorn_*` functions to look just how you want. For
instance:

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

Learn more in the [tabyls
vignette](http://sfirke.github.io/janitor/articles/tabyls.html).

### Explore records with duplicated values for specific combinations of variables with `get_dupes()`

This is for hunting down and examining duplicate records during data
cleaning - usually when there shouldn’t be any.

For example, in a tidy data.frame you might expect to have a unique ID
repeated for each year, but no duplicated pairs of unique ID & year. Say
you want to check for and study any such duplicated records.

`get_dupes()` returns the records (and inserts a count of duplicates) so
you can examine the problematic cases:

``` r
get_dupes(mtcars, wt, cyl) # or mtcars %>% get_dupes(wt, cyl) if you prefer to pipe
#>     wt cyl dupe_count  mpg  disp  hp drat  qsec vs am gear carb
#> 1 3.44   6          2 19.2 167.6 123 3.92 18.30  1  0    4    4
#> 2 3.44   6          2 17.8 167.6 123 3.92 18.90  1  0    4    4
#> 3 3.57   8          2 14.3 360.0 245 3.21 15.84  0  0    3    4
#> 4 3.57   8          2 15.0 301.0 335 3.54 14.60  0  1    5    8
```

### Explore relationships between columns with `get_one_to_one()`

This function shows which, if any, columns in a data.frame have
one-to-one relationships with each other.

Here is a toy example looking at the first four rows of the *starwars*
data.frame from the dplyr package. The variables are grouped into three
sets of one-to-one clusters:

``` r
library(dplyr)
starwars[1:4,] %>%
  get_one_to_one()
#> [[1]]
#> [1] "name"       "height"     "mass"       "skin_color" "birth_year" "films"     
#> 
#> [[2]]
#> [1] "hair_color" "starships" 
#> 
#> [[3]]
#> [1] "sex"     "species"
```

# Minor functions

Smaller functions for use in particular situations. More human-readable
than the equivalent code they replace.

## Cleaning

### Manipulate vectors of names with `make_clean_names()`

Like base R’s `make.names()`, but with the stylings and case choice of
the long-time janitor function `clean_names()`. While `clean_names()` is
still offered for use in data.frame pipeline with `%>%`,
`make_clean_names()` allows for more general usage, e.g., on a vector.

It can also be used as an argument to `.name_repair` in the newest
version of `tibble::as_tibble`:

``` r
tibble::as_tibble(iris, .name_repair = janitor::make_clean_names)
#> # A tibble: 150 × 5
#>    sepal_length sepal_width petal_length petal_width species
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>  
#>  1          5.1         3.5          1.4         0.2 setosa 
#>  2          4.9         3            1.4         0.2 setosa 
#>  3          4.7         3.2          1.3         0.2 setosa 
#>  4          4.6         3.1          1.5         0.2 setosa 
#>  5          5           3.6          1.4         0.2 setosa 
#>  6          5.4         3.9          1.7         0.4 setosa 
#>  7          4.6         3.4          1.4         0.3 setosa 
#>  8          5           3.4          1.5         0.2 setosa 
#>  9          4.4         2.9          1.4         0.2 setosa 
#> 10          4.9         3.1          1.5         0.1 setosa 
#> # … with 140 more rows
```

### Validate that a column has a `single_value()` per group

This function returns the single value in a column, often used in
combination with `dplyr::group_by()` to validate that every value of X
has only one associated value of Y. It will complete that value of Y
into missing values in that column. And if there’s more than a single
value of Y, its `info` argument can help pinpoint where that occurs.

Take this data.frame. One pesky value of X has multiple values of Y
where it should not:

``` r
not_one_to_one <- data.frame(
  X = rep(1:3, each = 2),
  Y = c(rep(1:2, each = 2), 1:2))

not_one_to_one
#>   X Y
#> 1 1 1
#> 2 1 1
#> 3 2 2
#> 4 2 2
#> 5 3 1
#> 6 3 2

# throws informative error:
try(not_one_to_one %>%
      dplyr::group_by(X) %>%
      dplyr::mutate(
        Z = single_value(Y, info = paste("Calculating Z for group X =", X)))
      )
#> Error in dplyr::mutate(., Z = single_value(Y, info = paste("Calculating Z for group X =",  : 
#>   ℹ In argument: `Z = single_value(Y, info = paste("Calculating Z for group X =", X))`.
#> ℹ In group 3: `X = 3`.
#> Caused by error in `single_value()`:
#> ! More than one (2) value found (1, 2): Calculating Z for group X = 3: Calculating Z for group X = 3
```

### `remove_empty()` rows and columns

Does what it says. For cases like cleaning Excel files that contain
empty rows and columns after being read into R.

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

Just a simple wrapper for one-line functions, but it saves a little
thinking for both the code writer and the reader.

### `remove_constant()` columns

Drops columns from a data.frame that contain only a single constant
value (with an `na.rm` option to control whether NAs should be
considered as different values from the constant).

`remove_constant` and `remove_empty` work on matrices as well as
data.frames.

``` r
a <- data.frame(good = 1:3, boring = "the same")
a %>% remove_constant()
#>   good
#> 1    1
#> 2    2
#> 3    3
```

### Directionally-consistent rounding behavior with `round_half_up()`

R uses “banker’s rounding”, i.e., halves are rounded to the nearest
*even* number. This function, an exact implementation of
<https://stackoverflow.com/questions/12688717/round-up-from-5/12688836#12688836>,
will round all halves up. Compare:

``` r
nums <- c(2.5, 3.5)
round(nums)
#> [1] 2 4
round_half_up(nums)
#> [1] 3 4
```

### Round decimals to precise fractions of a given denominator with `round_to_fraction()`

Say your data should only have values of quarters: 0, 0.25, 0.5, 0.75,
1, etc. But there are either user-entered bad values like `0.2` or
floating-point precision problems like `0.25000000001`.
`round_to_fraction()` will enforce the desired fractional distribution
by rounding the values to the nearest value given the specified
denominator.

There’s also a `digits` argument for optional subsequent rounding.

### Fix dates stored as serial numbers with `excel_numeric_to_date()`

Ever load data from Excel and see a value like `42223` where a date
should be? This function converts those serial numbers to class `Date`,
with options for different Excel date encoding systems, preserving
fractions of a date as time (in which case the returned value is of
class `POSIXlt`), and specifying a time zone.

``` r
excel_numeric_to_date(41103)
#> [1] "2012-07-13"
excel_numeric_to_date(41103.01) # ignores decimal places, returns Date object
#> [1] "2012-07-13"
excel_numeric_to_date(41103.01, include_time = TRUE) # returns POSIXlt object
#> [1] "2012-07-13 00:14:24 EDT"
excel_numeric_to_date(41103.01, date_system = "mac pre-2011")
#> [1] "2016-07-14"
```

### Convert a mix of date and datetime formats to date

Building on `excel_numeric_to_date()`, the new functions
`convert_to_date()` and `convert_to_datetime()` are more robust to a mix
of inputs. Handy when reading many spreadsheets that *should* have the
same column formats, but don’t.

For instance, here a vector with a date and an Excel datetime sees both
values successfully converted to Date class:

``` r
convert_to_date(c("2020-02-29", "40000.1"))
#> [1] "2020-02-29" "2009-07-06"
```

### Elevate column names stored in a data.frame row

If a data.frame has the intended variable names stored in one of its
rows, `row_to_names()` will elevate the specified row to become the
names of the data.frame and optionally (by default) remove the row in
which names were stored and/or the rows above it.

``` r
dirt <- data.frame(X_1 = c(NA, "ID", 1:3),
           X_2 = c(NA, "Value", 4:6))

row_to_names(dirt, 2)
#>   ID Value
#> 3  1     4
#> 4  2     5
#> 5  3     6
```

### Find the header row buried within a messy data.frame

The function `find_header()` is a companion function to
`row_to_names()`. By default it will search a data.frame for the first
row with no missing values and return that row number.

It can also be used to return the row number where a given string is
present in the first column, or in any specific column. Then this result
can be supplied to `row_to_names()`.

## Exploring

### Count factor levels in groups of high, medium, and low with `top_levels()`

Originally designed for use with Likert survey data stored as factors.
Returns a `tbl_df` frequency table with appropriately-named rows,
grouped into head/middle/tail groups.

- Takes a user-specified size for the head/tail groups
- Automatically calculates a percent column
- Supports sorting
- Can show or hide `NA` values.

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
