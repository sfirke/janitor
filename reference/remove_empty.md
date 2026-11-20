# Remove empty rows and/or columns from a data.frame or matrix.

Removes all rows and/or columns from a data.frame or matrix that are
composed entirely of `NA` values.

## Usage

``` r
remove_empty(dat, which = c("rows", "cols"), cutoff = 1, quiet = TRUE)
```

## Arguments

- dat:

  the input data.frame or matrix.

- which:

  one of "rows", "cols", or `c("rows", "cols")`. Where no value of which
  is provided, defaults to removing both empty rows and empty columns,
  declaring the behavior with a printed message.

- cutoff:

  a row/col should have more than this fraction of non-NA values to be
  retained. E.g., `cutoff = 0.8` means that rows/cols that are 20% or
  more missing will be dropped.

- quiet:

  Should messages be suppressed (`TRUE`) or printed (`FALSE`) indicating
  the summary of empty columns or rows removed?

## Value

Returns the object without its missing rows or columns.

## See also

[`remove_constant()`](https://sfirke.github.io/janitor/reference/remove_constant.md)
for removing constant columns.

Other remove functions:
[`remove_constant()`](https://sfirke.github.io/janitor/reference/remove_constant.md)

## Examples

``` r
# not run:
# dat %>% remove_empty("rows")
# addressing a common untidy-data scenario where we have a mixture of
# blank values in some (character) columns and NAs in others:
library(dplyr)
dd <- tibble(
  x = c(LETTERS[1:5], NA, rep("", 2)),
  y = c(1:5, rep(NA, 3))
)
# remove_empty() drops row 5 (all NA) but not 6 and 7 (blanks + NAs)
dd %>% remove_empty("rows")
#> # A tibble: 7 × 2
#>   x         y
#>   <chr> <int>
#> 1 "A"       1
#> 2 "B"       2
#> 3 "C"       3
#> 4 "D"       4
#> 5 "E"       5
#> 6 ""       NA
#> 7 ""       NA
# solution: preprocess to convert whitespace/empty strings to NA,
# _then_ remove empty (all-NA) rows
dd %>%
  mutate(across(where(is.character), ~ na_if(trimws(.), ""))) %>%
  remove_empty("rows")
#> # A tibble: 5 × 2
#>   x         y
#>   <chr> <int>
#> 1 A         1
#> 2 B         2
#> 3 C         3
#> 4 D         4
#> 5 E         5
```
