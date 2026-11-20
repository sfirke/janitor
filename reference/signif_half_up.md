# Round a numeric vector to the specified number of significant digits; halves will be rounded up.

In base R [`signif()`](https://rdrr.io/r/base/Round.html), halves are
rounded to even, e.g., `signif(11.5, 2)` and `signif(12.5, 2)` are both
rounded to 12. This function rounds 12.5 to 13 (assuming `digits = 2`).
Negative halves are rounded away from zero, e.g., `signif(-2.5, 1)` is
rounded to -3.

This may skew subsequent statistical analysis of the data, but may be
desirable in certain contexts. This function is implemented from
<https://stackoverflow.com/a/1581007/>; see that question and comments
for discussion of this issue.

## Usage

``` r
signif_half_up(x, digits = 6)
```

## Arguments

- x:

  a numeric vector to round.

- digits:

  integer indicating the number of significant digits to be used.

## Examples

``` r
signif_half_up(12.5, 2)
#> [1] 13
signif_half_up(1.125, 3)
#> [1] 1.13
signif_half_up(-2.5, 1) # negatives get rounded away from zero
#> [1] -3
```
