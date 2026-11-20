# Returns first non-`NA` value from a set of vectors.

Warning: Deprecated, do not use in new code. Use
[`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)
instead.

At each position of the input vectors, iterates through in order and
returns the first non-NA value. This is a robust replacement of the
common `ifelse(!is.na(x), x, ifelse(!is.na(y), y, z))`. It's more
readable and handles problems like
[`ifelse()`](https://rdrr.io/r/base/ifelse.html)'s inability to work
with dates in this way.

## Usage

``` r
use_first_valid_of(..., if_all_NA = NA)
```

## Arguments

- ...:

  the input vectors. Order matters: these are searched and prioritized
  in the order they are supplied.

- if_all_NA:

  what value should be used when all of the vectors return `NA` for a
  certain index? Default is `NA`.

## Value

Returns a single vector with the selected values.

## See also

janitor_deprecated
