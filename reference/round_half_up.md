# Round a numeric vector; halves will be rounded up, ala Microsoft Excel.

In base R [`round()`](https://rdrr.io/r/base/Round.html), halves are
rounded to even, e.g., 12.5 and 11.5 are both rounded to 12. This
function rounds 12.5 to 13 (assuming `digits = 0`). Negative halves are
rounded away from zero, e.g., -0.5 is rounded to -1.

This may skew subsequent statistical analysis of the data, but may be
desirable in certain contexts. This function is implemented exactly from
<https://stackoverflow.com/a/12688836>; see that question and comments
for discussion of this issue.

## Usage

``` r
round_half_up(x, digits = 0)
```

## Arguments

- x:

  a numeric vector to round.

- digits:

  how many digits should be displayed after the decimal point?

## Value

A vector with the same length as `x`

## Examples

``` r
round_half_up(12.5)
#> [1] 13
round_half_up(1.125, 2)
#> [1] 1.13
round_half_up(1.125, 1)
#> [1] 1.1
round_half_up(-0.5, 0) # negatives get rounded away from zero
#> [1] -1
```
