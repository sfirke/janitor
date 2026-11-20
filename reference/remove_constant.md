# Remove constant columns from a data.frame or matrix.

Remove constant columns from a data.frame or matrix.

## Usage

``` r
remove_constant(dat, na.rm = FALSE, quiet = TRUE)
```

## Arguments

- dat:

  the input data.frame or matrix.

- na.rm:

  should `NA` values be removed when considering whether a column is
  constant? The default value of `FALSE` will result in a column not
  being removed if it's a mix of a single value and `NA`.

- quiet:

  Should messages be suppressed (`TRUE`) or printed (`FALSE`) indicating
  the summary of empty columns or rows removed?

## See also

[`remove_empty()`](https://sfirke.github.io/janitor/reference/remove_empty.md)
for removing empty columns or rows.

Other remove functions:
[`remove_empty()`](https://sfirke.github.io/janitor/reference/remove_empty.md)

## Examples

``` r
remove_constant(data.frame(A = 1, B = 1:3))
#>   B
#> 1 1
#> 2 2
#> 3 3

# To find the columns that are constant
data.frame(A = 1, B = 1:3) %>%
  dplyr::select(!dplyr::all_of(names(remove_constant(.)))) %>%
  unique()
#>   A
#> 1 1
```
