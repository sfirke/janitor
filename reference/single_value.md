# Ensure that a vector has only a single value throughout.

Missing values are replaced with the single value, and if all values are
missing, the first value in `missing` is used throughout.

## Usage

``` r
single_value(x, missing = NA, warn_if_all_missing = FALSE, info = NULL)
```

## Arguments

- x:

  The vector which should have a single value

- missing:

  The vector of values to consider missing in `x`

- warn_if_all_missing:

  Generate a warning if all values are missing?

- info:

  If more than one value is found, append this to the warning or error
  to assist with determining the location of the issue.

## Value

`x` as the scalar single value found throughout (or an error if more
than one value is found).

## Examples

``` r
# A simple use case with vectors of input

single_value(c(NA, 1))
#> [1] 1
# Multiple, different values of missing can be given
single_value(c(NA, "a"), missing = c(NA, "a"))
#> [1] NA

# A typical use case with a grouped data.frame used for input and the output
# (`B` is guaranteed to have a single value and only one row, in this case)
data.frame(
  A = rep(1:3, each = 2),
  B = c(rep(4:6, each = 2))
) %>%
  dplyr::group_by(A) %>%
  dplyr::summarize(
    B = single_value(B)
  )
#> # A tibble: 3 × 2
#>       A     B
#>   <int> <int>
#> 1     1     4
#> 2     2     5
#> 3     3     6

try(
  # info is useful to give when multiple values may be found to see what
  # grouping variable or what calculation is causing the error
  data.frame(
    A = rep(1:3, each = 2),
    B = c(rep(1:2, each = 2), 1:2)
  ) %>%
    dplyr::group_by(A) %>%
    dplyr::mutate(
      C = single_value(B, info = paste("Calculating C for group A=", A))
    )
)
#> Error in dplyr::mutate(., C = single_value(B, info = paste("Calculating C for group A=",  : 
#>   ℹ In argument: `C = single_value(B, info = paste("Calculating C for
#>   group A=", A))`.
#> ℹ In group 3: `A = 3`.
#> Caused by error in `single_value()`:
#> ! More than one (2) value found (1, 2): Calculating C for group A= 3: Calculating C for group A= 3
```
