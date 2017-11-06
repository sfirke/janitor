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

Examples
========

This vignette demonstrates `tabyl` in the context of studying humans in the `starwars` dataset from dplyr:

``` r
library(dplyr)
humans <- starwars %>%
  filter(species == "Human")
```

### Installing

The features of `tabyl()` shown here are in the development version of janitor on GitHub and are not on CRAN yet. You can install the dev version with `devtools::install_github("sfirke/janitor")`.

One-way tabyl
-------------

Tabulating a single variable is the simplest kind of tabyl:

``` r
library(janitor)

t1 <- humans %>%
  tabyl(eye_color)

t1
#>   eye_color  n    percent
#> 1      blue 12 0.34285714
#> 2 blue-gray  1 0.02857143
#> 3     brown 17 0.48571429
#> 4      dark  1 0.02857143
#> 5     hazel  2 0.05714286
#> 6    yellow  2 0.05714286
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
#>   eye_color  n percent
#> 1      blue 12   34.3%
#> 2 blue-gray  1    2.9%
#> 3     brown 17   48.6%
#> 4      dark  1    2.9%
#> 5     hazel  2    5.7%
#> 6    yellow  2    5.7%
#> 7     Total 35  100.0%
```

Two-way tabyl
-------------

This is often called a "crosstab" or "contingency" table. The initial call produces the same result as the common combination of `dplyr::count()`, followed by `tidyr::spread()` to wide form:

``` r
t2 <- humans %>%
  tabyl(gender, eye_color)

t2
#>   gender blue blue-gray brown dark hazel yellow
#> 1 female    3         0     5    0     1      0
#> 2   male    9         1    12    1     1      2
```

And since it's a `tabyl`, we can enhance it with `adorn_` helper functions. For instance:

``` r

t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
#>   gender       blue blue-gray       brown      dark      hazel    yellow
#> 1 female 33.33% (3) 0.00% (0) 55.56%  (5) 0.00% (0) 11.11% (1) 0.00% (0)
#> 2   male 34.62% (9) 3.85% (1) 46.15% (12) 3.85% (1)  3.85% (1) 7.69% (2)
```

Adornments have options to control axes, rounding, and other relevant formatting choices (more on that below).

Three-way tabyl
---------------

Just as `table()` accepts three variables, so does `tabyl()`, producing a list of tabyls:

``` r
t3 <- humans %>%
  tabyl(eye_color, skin_color, gender)

t3 # the result is a tabyl of eye color x skin color, split into a list by gender
#> $female
#>   eye_color dark fair light pale tan white
#> 1      blue    0    2     1    0   0     0
#> 2 blue-gray    0    0     0    0   0     0
#> 3     brown    0    1     4    0   0     0
#> 4      dark    0    0     0    0   0     0
#> 5     hazel    0    0     1    0   0     0
#> 6    yellow    0    0     0    0   0     0
#> 
#> $male
#>   eye_color dark fair light pale tan white
#> 1      blue    0    7     2    0   0     0
#> 2 blue-gray    0    1     0    0   0     0
#> 3     brown    3    4     3    0   2     0
#> 4      dark    1    0     0    0   0     0
#> 5     hazel    0    1     0    0   0     0
#> 6    yellow    0    0     0    1   0     1
```

Use `purrr::map()` to apply the `adorn_` helper functions to the entire list:

``` r
library(purrr)
humans %>%
  tabyl(eye_color, skin_color, gender, show_missing_levels = FALSE) %>%
  map(adorn_totals, "row") %>%
  map(adorn_percentages, "all") %>%
  map(adorn_pct_formatting, digits = 1) %>%
  map(adorn_ns)
#> $female
#>   eye_color      fair     light
#> 1      blue 22.2% (2) 11.1% (1)
#> 2     brown 11.1% (1) 44.4% (4)
#> 3     hazel  0.0% (0) 11.1% (1)
#> 4     Total 33.3% (3) 66.7% (6)
#> 
#> $male
#>   eye_color      dark       fair     light     pale      tan    white
#> 1      blue  0.0% (0) 26.9%  (7)  7.7% (2) 0.0% (0) 0.0% (0) 0.0% (0)
#> 2 blue-gray  0.0% (0)  3.8%  (1)  0.0% (0) 0.0% (0) 0.0% (0) 0.0% (0)
#> 3     brown 11.5% (3) 15.4%  (4) 11.5% (3) 0.0% (0) 7.7% (2) 0.0% (0)
#> 4      dark  3.8% (1)  0.0%  (0)  0.0% (0) 0.0% (0) 0.0% (0) 0.0% (0)
#> 5     hazel  0.0% (0)  3.8%  (1)  0.0% (0) 0.0% (0) 0.0% (0) 0.0% (0)
#> 6    yellow  0.0% (0)  0.0%  (0)  0.0% (0) 3.8% (1) 0.0% (0) 3.8% (1)
#> 7     Total 15.4% (4) 50.0% (13) 19.2% (5) 3.8% (1) 7.7% (2) 3.8% (1)
```

### Other features of tabyls

-   When called on a factor, it will include missing levels (levels not present in the data) in the result
    -   This can be suppressed if not desired
-   `NA` values can be displayed or suppressed
-   Prints without displaying row numbers

`adorn_*` functions
-------------------

These modular functions build on a `tabyl` to approximate the functionality of a quick PivotTable in Microsoft Excel. They print elegant results for interactive analysis or for sharing in a report, e.g., with `knitr::kable()`. For example:

``` r
humans %>%
  tabyl(gender, eye_color) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  knitr::kable()
```

| gender | blue     | blue-gray | brown    | dark   | hazel   | yellow | Total     |
|:-------|:---------|:----------|:---------|:-------|:--------|:-------|:----------|
| female | 33% (3)  | 0% (0)    | 56% (5)  | 0% (0) | 11% (1) | 0% (0) | 100% (9)  |
| male   | 35% (9)  | 4% (1)    | 46% (12) | 4% (1) | 4% (1)  | 8% (2) | 100% (26) |
| Total  | 34% (12) | 3% (1)    | 49% (17) | 3% (1) | 6% (2)  | 6% (2) | 100% (35) |

### The adorn functions are:

-   **`adorn_totals()`**: Add totals row, column, or both. Replaces the janitor functions `add_totals_row` and `add_totals_col`
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
percent_above_165_cm <- humans %>%
  group_by(gender) %>%
  summarise(pct_above_165_cm = mean(height > 165, na.rm = TRUE))

percent_above_165_cm %>%
  adorn_pct_formatting()
#> # A tibble: 2 x 2
#>   gender pct_above_165_cm
#>    <chr>            <chr>
#> 1 female            12.5%
#> 2   male           100.0%
```

Here's a more complex example. We'll create a table containing the mean of a 3rd variable when grouped by two other variables, then use `adorn_` functions to round the values and append Ns. The first part is pretty straightforward:

``` r
library(tidyr) # for spread()
mpg_by_cyl_and_am <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(mpg = mean(mpg)) %>%
  spread(am, mpg)

mpg_by_cyl_and_am
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
mpg_by_cyl_and_am %>%
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
