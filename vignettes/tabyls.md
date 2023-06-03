tabyls: a tidy, fully-featured approach to counting things
================
2023-02-03

## Motivation: why tabyl?

Analysts do a lot of counting. Indeed, it’s been said that “data science
is mostly counting things.” But the base R function for counting,
`table()`, leaves much to be desired:

- It doesn’t accept data.frame inputs (and thus doesn’t play nicely with
  the `%>%` pipe)
- It doesn’t output data.frames
- Its results are hard to format. Compare the look and formatting
  choices of an R table to a Microsoft Excel PivotTable or even the
  table formatting provided by SPSS.

`tabyl()` is an approach to tabulating variables that addresses these
shortcomings. It’s part of the janitor package because counting is such
a fundamental part of data cleaning and exploration.

`tabyl()` is tidyverse-aligned and is primarily built upon the dplyr and
tidyr packages.

## How it works

On its surface, `tabyl()` produces frequency tables using 1, 2, or 3
variables. Under the hood, `tabyl()` also attaches a copy of these
counts as an attribute of the resulting data.frame.

The result looks like a basic data.frame of counts, but because it’s
also a `tabyl` containing this metadata, you can use `adorn_` functions
to add additional information and pretty formatting.

The `adorn_` functions are built to work on `tabyls`, but have been
adapted to work with similar, non-tabyl data.frames that need
formatting.

# Examples

This vignette demonstrates `tabyl` in the context of studying humans in
the `starwars` dataset from dplyr:

``` r
library(dplyr)
humans <- starwars %>%
  filter(species == "Human")
```

## One-way tabyl

Tabulating a single variable is the simplest kind of tabyl:

``` r
library(janitor)

t1 <- humans %>%
  tabyl(eye_color)

t1
#>  eye_color  n    percent
#>       blue 12 0.34285714
#>  blue-gray  1 0.02857143
#>      brown 17 0.48571429
#>       dark  1 0.02857143
#>      hazel  2 0.05714286
#>     yellow  2 0.05714286
```

When `NA` values are present, `tabyl()` also displays “valid”
percentages, i.e., with missing values removed from the denominator. And
while `tabyl()` is built to take a data.frame and column names, you can
also produce a one-way tabyl by calling it directly on a vector:

``` r
x <- c("big", "big", "small", "small", "small", NA)
tabyl(x)
#>      x n   percent valid_percent
#>    big 2 0.3333333           0.4
#>  small 3 0.5000000           0.6
#>   <NA> 1 0.1666667            NA
```

Most `adorn_` helper functions are built for 2-way tabyls, but those
that make sense for a 1-way tabyl do work:

``` r
t1 %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
#>  eye_color  n percent
#>       blue 12   34.3%
#>  blue-gray  1    2.9%
#>      brown 17   48.6%
#>       dark  1    2.9%
#>      hazel  2    5.7%
#>     yellow  2    5.7%
#>      Total 35  100.0%
```

## Two-way tabyl

This is often called a “crosstab” or “contingency” table. Calling
`tabyl` on two columns of a data.frame produces the same result as the
common combination of `dplyr::count()`, followed by
`tidyr::pivot_wider()` to wide form:

``` r
t2 <- humans %>%
  tabyl(gender, eye_color)

t2
#>     gender blue blue-gray brown dark hazel yellow
#>   feminine    3         0     5    0     1      0
#>  masculine    9         1    12    1     1      2
```

Since it’s a `tabyl`, we can enhance it with `adorn_` helper functions.
For instance:

``` r

t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
#>     gender       blue blue-gray       brown      dark      hazel    yellow
#>   feminine 33.33% (3) 0.00% (0) 55.56%  (5) 0.00% (0) 11.11% (1) 0.00% (0)
#>  masculine 34.62% (9) 3.85% (1) 46.15% (12) 3.85% (1)  3.85% (1) 7.69% (2)
```

Adornments have options to control axes, rounding, and other relevant
formatting choices (more on that below).

## Three-way tabyl

Just as `table()` accepts three variables, so does `tabyl()`, producing
a list of tabyls:

``` r
t3 <- humans %>%
  tabyl(eye_color, skin_color, gender)

# the result is a tabyl of eye color x skin color, split into a list by gender
t3 
#> $feminine
#>  eye_color dark fair light pale tan white
#>       blue    0    2     1    0   0     0
#>  blue-gray    0    0     0    0   0     0
#>      brown    0    1     4    0   0     0
#>       dark    0    0     0    0   0     0
#>      hazel    0    0     1    0   0     0
#>     yellow    0    0     0    0   0     0
#> 
#> $masculine
#>  eye_color dark fair light pale tan white
#>       blue    0    7     2    0   0     0
#>  blue-gray    0    1     0    0   0     0
#>      brown    3    4     3    0   2     0
#>       dark    1    0     0    0   0     0
#>      hazel    0    1     0    0   0     0
#>     yellow    0    0     0    1   0     1
```

If the `adorn_` helper functions are called on a list of data.frames -
like the output of a three-way `tabyl` call - they will call
`purrr::map()` to apply themselves to each data.frame in the list:

``` r
library(purrr)
humans %>%
  tabyl(eye_color, skin_color, gender, show_missing_levels = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title
#> $feminine
#>            skin_color          
#>  eye_color       fair     light
#>       blue  22.2% (2) 11.1% (1)
#>      brown  11.1% (1) 44.4% (4)
#>      hazel   0.0% (0) 11.1% (1)
#>      Total  33.3% (3) 66.7% (6)
#> 
#> $masculine
#>            skin_color                                                
#>  eye_color       dark       fair     light     pale      tan    white
#>       blue   0.0% (0) 26.9%  (7)  7.7% (2) 0.0% (0) 0.0% (0) 0.0% (0)
#>  blue-gray   0.0% (0)  3.8%  (1)  0.0% (0) 0.0% (0) 0.0% (0) 0.0% (0)
#>      brown  11.5% (3) 15.4%  (4) 11.5% (3) 0.0% (0) 7.7% (2) 0.0% (0)
#>       dark   3.8% (1)  0.0%  (0)  0.0% (0) 0.0% (0) 0.0% (0) 0.0% (0)
#>      hazel   0.0% (0)  3.8%  (1)  0.0% (0) 0.0% (0) 0.0% (0) 0.0% (0)
#>     yellow   0.0% (0)  0.0%  (0)  0.0% (0) 3.8% (1) 0.0% (0) 3.8% (1)
#>      Total  15.4% (4) 50.0% (13) 19.2% (5) 3.8% (1) 7.7% (2) 3.8% (1)
```

This automatic mapping supports interactive data analysis that switches
between combinations of 2 and 3 variables. That way, if a user starts
with `humans %>% tabyl(eye_color, skin_color)`, adds some `adorn_`
calls, then decides to split the tabulation by gender and modifies their
first line to `humans %>% tabyl(eye_color, skin_color, gender`), they
don’t have to rewrite the subsequent adornment calls to use `map()`.

However, if feels more natural to call these with `map()` or `lapply()`,
that is still supported. For instance,
`t3 %>% lapply(adorn_percentages)` would produce the same result as
`t3 %>% adorn_percentages`.

### Other features of tabyls

- When called on a factor, `tabyl` will show missing levels (levels not
  present in the data) in the result
  - This can be suppressed if not desired
- `NA` values can be displayed or suppressed
- `tabyls` print without displaying row numbers

You can call `chisq.test()` and `fisher.test()` on a two-way tabyl to
perform those statistical tests, just like on a base R `table()` object.

## The `adorn_*` functions

These modular functions build on a `tabyl` to approximate the
functionality of a PivotTable in Microsoft Excel. They print elegant
results for interactive analysis or for sharing in a report, e.g., with
`knitr::kable()`. For example:

``` r
humans %>%
  tabyl(gender, eye_color) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()
```

| gender/eye_color | blue     | blue-gray | brown    | dark   | hazel   | yellow | Total     |
|:-----------------|:---------|:----------|:---------|:-------|:--------|:-------|:----------|
| feminine         | 33% (3)  | 0% (0)    | 56% (5)  | 0% (0) | 11% (1) | 0% (0) | 100% (9)  |
| masculine        | 35% (9)  | 4% (1)    | 46% (12) | 4% (1) | 4% (1)  | 8% (2) | 100% (26) |
| Total            | 34% (12) | 3% (1)    | 49% (17) | 3% (1) | 6% (2)  | 6% (2) | 100% (35) |

### The adorn functions are:

- **`adorn_totals()`**: Add totals row, column, or both.
- **`adorn_percentages()`**: Calculate percentages along either axis or
  over the entire tabyl
- **`adorn_pct_formatting()`**: Format percentage columns, controlling
  the number of digits to display and whether to append the `%` symbol
- **`adorn_rounding()`**: Round a data.frame of numbers (usually the
  result of `adorn_percentages`), either using the base R `round()`
  function or using janitor’s `round_half_up()` to round all ties up
  ([thanks,
  StackOverflow](https://stackoverflow.com/a/12688836/4470365)).
  - e.g., round 10.5 up to 11, consistent with Excel’s tie-breaking
    behavior.
    - This contrasts with rounding 10.5 down to 10 as in base R’s
      `round(10.5)`.
  - `adorn_rounding()` returns columns of class `numeric`, allowing for
    graphing, sorting, etc. It’s a less-aggressive substitute for
    `adorn_pct_formatting()`; these two functions should not be called
    together.
- **`adorn_ns()`**: add Ns to a tabyl. These can be drawn from the
  tabyl’s underlying counts, which are attached to the tabyl as
  metadata, or they can be supplied by the user.
- **`adorn_title()`**: add a title to a tabyl (or other data.frame).
  Options include putting the column title in a new row on top of the
  data.frame or combining the row and column titles in the data.frame’s
  first name slot.

These adornments should be called in a logical order, e.g., you probably
want to add totals before percentages are calculated. In general, call
them in the order they appear above.

## BYOt (Bring Your Own tabyl)

You can also call `adorn_` functions on other data.frames, not only the
results of calls to `tabyl()`. E.g.,
`mtcars %>% adorn_totals("col") %>% adorn_percentages("col")` performs
as expected, despite `mtcars` not being a `tabyl`.

This can be handy when you have a data.frame that is not a simple
tabulation generated by `tabyl` but would still benefit from the
`adorn_` formatting functions.

A simple example: calculate the proportion of records meeting a certain
condition, then format the results.

``` r
percent_above_165_cm <- humans %>%
  group_by(gender) %>%
  summarise(pct_above_165_cm = mean(height > 165, na.rm = TRUE), .groups = "drop")

percent_above_165_cm %>%
  adorn_pct_formatting()
#> # A tibble: 2 × 2
#>   gender    pct_above_165_cm
#>   <chr>     <chr>           
#> 1 feminine  12.5%           
#> 2 masculine 100.0%
```

You can control which columns are adorned by using the `...` argument.
It accepts the [tidyselect
helpers](https://r4ds.had.co.nz/transform.html#select). That is, you can
specify columns the same way you would using `dplyr::select()`.

For instance, say you have a numeric column that should not be included
in percentage formatting and you wish to exempt it. Here, only the
`proportion` column is adorned:

``` r
mtcars %>%
  count(cyl, gear) %>%
  rename(proportion = n) %>%
  adorn_percentages("col", na.rm = TRUE, proportion) %>%
  adorn_pct_formatting(,,,proportion) # the commas say to use the default values of the other arguments
#>  cyl gear proportion
#>    4    3       3.1%
#>    4    4      25.0%
#>    4    5       6.2%
#>    6    3       6.2%
#>    6    4      12.5%
#>    6    5       3.1%
#>    8    3      37.5%
#>    8    5       6.2%
```

Here we specify that only two consecutive numeric columns should be
totaled (`year` is numeric but should not be included):

``` r
cases <- data.frame(
  region = c("East", "West"),
  year = 2015,
  recovered = c(125, 87),
  died = c(13, 12)
)

cases %>%
    adorn_totals(c("col", "row"), fill = "-", na.rm = TRUE, name = "Total Cases", recovered:died)
#>       region year recovered died Total Cases
#>         East 2015       125   13         138
#>         West 2015        87   12          99
#>  Total Cases    -       212   25         237
```

Here’s a more complex example that uses a data.frame of means, not
counts. We create a table containing the mean of a 3rd variable when
grouped by two other variables, then use `adorn_` functions to round the
values and append Ns. The first part is pretty straightforward:

``` r
library(tidyr) # for spread()
mpg_by_cyl_and_am <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(mpg = mean(mpg), .groups = "drop") %>%
  spread(am, mpg)

mpg_by_cyl_and_am
#> # A tibble: 3 × 3
#>     cyl   `0`   `1`
#>   <dbl> <dbl> <dbl>
#> 1     4  22.9  28.1
#> 2     6  19.1  20.6
#> 3     8  15.0  15.4
```

Now to `adorn_` it. Since this is not the result of a `tabyl()` call, it
doesn’t have the underlying Ns stored in the `core` attribute, so we’ll
have to supply them:

``` r
mpg_by_cyl_and_am %>%
  adorn_rounding() %>%
  adorn_ns(
    ns = mtcars %>% # calculate the Ns on the fly by calling tabyl on the original data
      tabyl(cyl, am)
  ) %>%
  adorn_title("combined", row_name = "Cylinders", col_name = "Is Automatic")
#>   Cylinders/Is Automatic         0        1
#> 1                      4 22.9  (3) 28.1 (8)
#> 2                      6 19.1  (4) 20.6 (3)
#> 3                      8 15.1 (12) 15.4 (2)
```

If needed, Ns can be manipulated in their own data.frame before they are
appended. Here a tabyl with values in the thousands has its Ns formatted
to include the separating character `,` as typically seen in American
numbers, e.g., `3,000`.

First we create the tabyl to adorn:

``` r
set.seed(1)
raw_data <- data.frame(sex = rep(c("m", "f"), 3000),
                age = round(runif(3000, 1, 102), 0))
raw_data$agegroup = cut(raw_data$age, quantile(raw_data$age, c(0, 1/3, 2/3, 1)))

comparison <- raw_data %>%
  tabyl(agegroup, sex, show_missing_levels = F) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)

comparison
#>  agegroup      f      m  Total
#>    (1,34]  33.9%  32.3%  33.1%
#>   (34,68]  33.0%  33.7%  33.4%
#>  (68,102]  32.7%  33.3%  33.0%
#>      <NA>   0.4%   0.6%   0.5%
#>     Total 100.0% 100.0% 100.0%
```

At this point, the Ns are unformatted:

``` r
comparison %>%
  adorn_ns()
#>  agegroup              f              m          Total
#>    (1,34]  33.9% (1,018)  32.3%   (970)  33.1% (1,988)
#>   (34,68]  33.0%   (990)  33.7% (1,012)  33.4% (2,002)
#>  (68,102]  32.7%   (980)  33.3% (1,000)  33.0% (1,980)
#>      <NA>   0.4%    (12)   0.6%    (18)   0.5%    (30)
#>     Total 100.0% (3,000) 100.0% (3,000) 100.0% (6,000)
```

Now we format them to insert the thousands commas. A tabyl’s raw Ns are
stored in its `"core"` attribute. Here we retrieve those with `attr()`,
then apply the base R function `format()` to all numeric columns.
Lastly, we append these Ns using `adorn_ns()`.

``` r
formatted_ns <- attr(comparison, "core") %>% # extract the tabyl's underlying Ns
  adorn_totals(c("row", "col")) %>% # to match the data.frame we're appending to
  dplyr::mutate(across(where(is.numeric), ~ format(.x, big.mark = ",")))

comparison %>%
  adorn_ns(position = "rear", ns = formatted_ns)
#>  agegroup              f              m          Total
#>    (1,34]  33.9% (1,018)  32.3% (  970)  33.1% (1,988)
#>   (34,68]  33.0% (  990)  33.7% (1,012)  33.4% (2,002)
#>  (68,102]  32.7% (  980)  33.3% (1,000)  33.0% (1,980)
#>      <NA>   0.4% (   12)   0.6% (   18)   0.5% (   30)
#>     Total 100.0% (3,000) 100.0% (3,000) 100.0% (6,000)
```

### Questions? Comments?

File [an issue on GitHub](https://github.com/sfirke/janitor/issues) if
you have suggestions related to `tabyl()` and its `adorn_` helpers or
encounter problems while using them.
