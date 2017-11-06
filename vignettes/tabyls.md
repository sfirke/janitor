Motivation: why tabyl?
----------------------

Analysts do a lot of counting. Indeed, it's been said that "[data science is mostly counting things](https://twitter.com/joelgrus/status/833691273873600512)." But the base R function for counting, `table()`, leaves much to be desired:

-   It doesn't accept data.frame inputs (and thus doesn't play nicely with the tidyverse)
-   It doesn't output data.frames
-   Its results are hard to format. Compare the look and formatting choices of an R table to a Microsoft Excel PivotTable or even the table formatting provided by SPSS.

`tabyl()` is an approach to tabulating variables that addresses these shortcomings. It's part of the janitor package because counting is such a fundamental part of data cleaning and exploration.

`tabyl()` is tidyverse-aligned and is primarily built upon the dplyr and tidyr packages.

How it works
------------

On its surface, `tabyl()` produces frequency tables using 1, 2, or 3 variables. Under the hood, `tabyl()` also attaches a copy of these counts as an attribute of the resulting data.frame.

The result looks like a basic data.frame of counts, but because it's also a `tabyl` containing this metadata, you can use `adorn_` functions to add pretty formatting.

One-way tabyl
-------------

Tabulating a single variable is the simplest kind of tabyl:

``` r
library(janitor)
t1 <- mtcars %>%
  tabyl(cyl)

t1
#>   cyl  n percent
#> 1   4 11 0.34375
#> 2   6  7 0.21875
#> 3   8 14 0.43750
```

When `NA` values are present, `tabyl()` also displays "valid" percentages, i.e., with missing values removed from the denominator. And while `tabyl()` is built to take a data.frame and column names, you can also produce a one-way tabyl by calling it directly on a vector:

``` r
x <- c("big", "big", "small", "small", "small", NA)
tabyl(x)
#>       x n   percent valid_percent
#> 1   big 2 0.3333333           0.4
#> 2 small 3 0.5000000           0.6
#> 3  <NA> 1 0.1666667            NA
```

Most `adorn_` helper functions are built for 2-way tabyls, but those that make sense for a 1-way tabyl do work:

``` r
t1 %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
#>     cyl  n percent
#> 1     4 11   34.4%
#> 2     6  7   21.9%
#> 3     8 14   43.8%
#> 4 Total 32  100.0%
```

Two-way tabyl
-------------

This is often called a "crosstab" or "contingency" table. The initial call produces the same result as the common combination of `dplyr::count()`, followed by `tidyr::spread()` to wide form:

``` r
t2 <- mtcars %>%
  tabyl(cyl, carb)

t2
#>   cyl 1 2 3 4 6 8
#> 1   4 5 6 0 0 0 0
#> 2   6 2 0 0 4 1 0
#> 3   8 0 4 3 6 0 1
```

And since it's a `tabyl`, we can enhance it with `adorn_` helper functions. For instance:

``` r

t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
#>   cyl          1          2          3          4          6         8
#> 1   4 45.45% (5) 54.55% (6)  0.00% (0)  0.00% (0)  0.00% (0) 0.00% (0)
#> 2   6 28.57% (2)  0.00% (0)  0.00% (0) 57.14% (4) 14.29% (1) 0.00% (0)
#> 3   8  0.00% (0) 28.57% (4) 21.43% (3) 42.86% (6)  0.00% (0) 7.14% (1)
```

Each of these has options to control axes, rounding, and other relevant formatting choices.

Three-way tabyl
---------------

Just as `table()` accepts three variables, so does `tabyl()`, producing a list of tabyls:

``` r
t3 <- mtcars %>%
  tabyl(cyl, carb, am)

t3 # the result is a tabyl of cyl x carb, split into a list by the values of am
#> $`0`
#>   cyl 1 2 3 4 6 8
#> 1   4 1 2 0 0 0 0
#> 2   6 2 0 0 2 0 0
#> 3   8 0 4 3 5 0 0
#> 
#> $`1`
#>   cyl 1 2 3 4 6 8
#> 1   4 4 4 0 0 0 0
#> 2   6 0 0 0 2 1 0
#> 3   8 0 0 0 1 0 1
```

One can use `purrr::map()` to apply the `adorn_` helper functions to the entire list:

``` r
library(purrr)
#> Warning: package 'purrr' was built under R version 3.4.2
mtcars %>%
  tabyl(carb, am, cyl, show_missing_levels = FALSE) %>%
  map(adorn_totals, "row") %>%
  map(adorn_percentages, "row") %>%
  map(adorn_pct_formatting, digits = 1) %>%
  map(adorn_ns)
#> $`4`
#>    carb         0         1
#> 1     1 20.0% (1) 80.0% (4)
#> 2     2 33.3% (2) 66.7% (4)
#> 3 Total 27.3% (3) 72.7% (8)
#> 
#> $`6`
#>    carb          0          1
#> 1     1 100.0% (2)   0.0% (0)
#> 2     4  50.0% (2)  50.0% (2)
#> 3     6   0.0% (0) 100.0% (1)
#> 4 Total  57.1% (4)  42.9% (3)
#> 
#> $`8`
#>    carb           0          1
#> 1     2 100.0%  (4)   0.0% (0)
#> 2     3 100.0%  (3)   0.0% (0)
#> 3     4  83.3%  (5)  16.7% (1)
#> 4     8   0.0%  (0) 100.0% (1)
#> 5 Total  85.7% (12)  14.3% (2)
```

### Other features of tabyls

-   When called on a factor, it will include missing levels in the result (levels not present in the vector)
    -   This can be suppressed if not desired behavior
-   `NA` values can be displayed or suppressed
-   They print without row numbers displaying

`adorn_*` functions
-------------------

These modular functions build on a `tabyl` to approximate the functionality of a quick PivotTable in Microsoft Excel. They print elegant results for interactive analysis or for sharing in a report, e.g., with `knitr::kable()`. For example:

``` r
mtcars %>%
  tabyl(cyl, gear) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  knitr::kable()
```

| cyl   | 3        | 4        | 5       | Total     |
|:------|:---------|:---------|:--------|:----------|
| 4     | 9% (1)   | 73% (8)  | 18% (2) | 100% (11) |
| 6     | 29% (2)  | 57% (4)  | 14% (1) | 100% (7)  |
| 8     | 86% (12) | 0% (0)   | 14% (2) | 100% (14) |
| Total | 47% (15) | 38% (12) | 16% (5) | 100% (32) |

### The adorn functions are:

-   **`adorn_totals_row()`**: Add totals row, column, or both. Replaces the janitor functions `add_totals_row` and `add_totals_col`
-   **`adorn_percentages()`**: Calculate percentages along either axis or over the entire tabyl
-   **`adorn_pct_formatting()`**: Format percentage columns, controlling number of digits to display and whether to append the `%` symbol
-   **`adorn_rounding()`**: Round a data.frame of numbers (usually the result of `adorn_percentages`), either using the base R `round()` function or rounding all ties up using a custom rounding function ([thanks, StackOverflow](http://stackoverflow.com/a/12688836/4470365)).
    -   e.g., round 10.5 up to 11, consistent with Excel's tie-breaking behavior.
    -   This contrasts with rounding 10.5 down to 10 as in base R's `round(10.5)`.
    -   `adorn_rounding()` outputs retain the class `numeric`, allowing for graphing, sorting, etc. It's a less-aggressive substitute for `adorn_pct_formatting()`; these two functions should not be called together.
-   **`adorn_ns()`**: add Ns to a tabyl. These can be drawn from the tabyl's `core` attribute (by default), or supplied by the user.

These adornments should be called in a logical order, e.g., you probably want to add totals before percentages are calculated. In general, call them in the order they appear above.

Users of janitor version &lt;= 0.3.0 should replace the obsolete `adorn_crosstab()` with combinations of the above `adorn_` functions.

BYOt (Bring Your Own tabyl)
---------------------------

You can also call `adorn_` functions on other data.frames, not only the results of calls to `tabyl()`. E.g., `mtcars %>% adorn_totals("col") %>% adorn_percentages("col")` performs as expected, despite `mtcars` not being a `tabyl`.

This can be handy when you have a data.frame that is not a simple tabulation generated by `tabyl` but would still benefit from the `adorn_` formatting functions.

A simple example: formatting percentages in a data.frame showing the % of records meeting a certain condition:

``` r
library(dplyr)
percent_above_20_mpg <- mtcars %>%
  group_by(cyl) %>%
  summarise(pct_above_20_mpg = mean(mpg > 20))

percent_above_20_mpg %>%
  adorn_pct_formatting()
#> # A tibble: 3 x 2
#>     cyl pct_above_20_mpg
#>   <dbl>            <chr>
#> 1     4           100.0%
#> 2     6            42.9%
#> 3     8             0.0%
```

Here's a more complex example. We'll create a table containing the mean of a 3rd variable when grouped by two other variables, then use `adorn_` functions to round the values and append Ns. The first part is pretty straightforward:

``` r
library(tidyr) # for spread()
base_table <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(mpg = mean(mpg)) %>%
  spread(am, mpg)

base_table
#> # A tibble: 3 x 3
#> # Groups:   cyl [3]
#>     cyl    `0`      `1`
#> * <dbl>  <dbl>    <dbl>
#> 1     4 22.900 28.07500
#> 2     6 19.125 20.56667
#> 3     8 15.050 15.40000
```

Now to `adorn_` it. Since this is not a result of a `tabyl()` call, it doesn't have the underlying Ns stored in the `core` attribute, so we'll have to supply them:

``` r
base_table %>%
  adorn_rounding() %>%
  adorn_ns(
    ns = mtcars %>% # calculate the Ns on the fly by calling tabyl on the original data
      tabyl(cyl, am)
  )
#> # A tibble: 3 x 3
#> # Groups:   cyl [3]
#>     cyl       `0`      `1`
#> * <dbl>     <chr>    <chr>
#> 1     4 22.9  (3) 28.1 (8)
#> 2     6 19.1  (4) 20.6 (3)
#> 3     8 15.1 (12) 15.4 (2)
```

Or you could tinker with the Ns before appending them, e.g., if you have large Ns in a tabyl, divide them by 1000, round, and append "k" before calling `adorn_ns`.

### Questions? Comments?

File [an issue on GitHub](https://github.com/sfirke/janitor/issues) if you have questions or ideas related to `tabyl()` and its `adorn_` helpers or encounter problems while using them.
