# Convert string values to true `NA` values.

Warning: Deprecated, do not use in new code. Use
[`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html)
instead.

Converts instances of user-specified strings into `NA`. Can operate on
either a single vector or an entire data.frame.

## Usage

``` r
convert_to_NA(dat, strings)
```

## Arguments

- dat:

  vector or data.frame to operate on.

- strings:

  character vector of strings to convert.

## Value

Returns a cleaned object. Can be a vector, data.frame, or
[`tibble::tbl_df`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
depending on the provided input.

## See also

janitor_deprecated
