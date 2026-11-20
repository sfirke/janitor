# Do the the data.frames have the same columns & types?

Check whether a set of data.frames are row-bindable. Calls
[`compare_df_cols()`](https://sfirke.github.io/janitor/reference/compare_df_cols.md)
and returns `TRUE` if there are no mis-matching rows.

## Usage

``` r
compare_df_cols_same(
  ...,
  bind_method = c("bind_rows", "rbind"),
  verbose = TRUE
)
```

## Arguments

- ...:

  A combination of data.frames, tibbles, and lists of
  data.frames/tibbles. The values may optionally be named arguments; if
  named, the output column will be the name; if not named, the output
  column will be the data.frame name (see examples section).

- bind_method:

  What method of binding should be used to determine matches? With
  "bind_rows", columns missing from a data.frame would be considered a
  match (as in
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html);
  with "rbind", columns missing from a data.frame would be considered a
  mismatch (as in [`base::rbind()`](https://rdrr.io/r/base/cbind.html).

- verbose:

  Print the mismatching columns if binding will fail.

## Value

`TRUE` if row binding will succeed or `FALSE` if it will fail.

## See also

Other data frame type comparison:
[`compare_df_cols()`](https://sfirke.github.io/janitor/reference/compare_df_cols.md),
[`describe_class()`](https://sfirke.github.io/janitor/reference/describe_class.md)

## Examples

``` r
compare_df_cols_same(data.frame(A = 1), data.frame(A = 2))
#> [1] TRUE
compare_df_cols_same(data.frame(A = 1), data.frame(B = 2))
#> [1] TRUE
compare_df_cols_same(data.frame(A = 1), data.frame(B = 2), verbose = FALSE)
#> [1] TRUE
compare_df_cols_same(data.frame(A = 1), data.frame(B = 2), bind_method = "rbind")
#>   column_name     ..1     ..2
#> 1           A numeric    <NA>
#> 2           B    <NA> numeric
#> [1] FALSE
```
