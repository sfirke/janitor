# Round to the nearest fraction of a specified denominator.

Round a decimal to the precise decimal value of a specified fractional
denominator. Common use cases include addressing floating point
imprecision and enforcing that data values fall into a certain set.

E.g., if a decimal represents hours and values should be logged to the
nearest minute, `round_to_fraction(x, 60)` would enforce that
distribution and 0.57 would be rounded to 0.566667, the equivalent of
34/60. 0.56 would also be rounded to 34/60.

Set `denominator = 1` to round to whole numbers.

The `digits` argument allows for rounding of the subsequent result.

## Usage

``` r
round_to_fraction(x, denominator, digits = Inf)
```

## Arguments

- x:

  A numeric vector

- denominator:

  The denominator of the fraction for rounding (a scalar or vector
  positive integer).

- digits:

  Integer indicating the number of decimal places to be used after
  rounding to the fraction. This is passed to
  [`base::round()`](https://rdrr.io/r/base/Round.html)). Negative values
  are allowed (see Details). (`Inf` indicates no subsequent rounding)

## Value

the input x rounded to a decimal value that has an integer numerator
relative to `denominator` (possibly subsequently rounded to a number of
decimal digits).

## Details

If `digits` is `Inf`, `x` is rounded to the fraction and then kept at
full precision. If `digits` is `"auto"`, the number of digits is
automatically selected as `ceiling(log10(denominator)) + 1`.

## Examples

``` r
round_to_fraction(1.6, denominator = 2)
#> [1] 1.5
round_to_fraction(pi, denominator = 7) # 22/7
#> [1] 3.142857
round_to_fraction(c(8.1, 9.2), denominator = c(7, 8))
#> [1] 8.142857 9.250000
round_to_fraction(c(8.1, 9.2), denominator = c(7, 8), digits = 3)
#> [1] 8.143 9.250
round_to_fraction(c(8.1, 9.2, 10.3), denominator = c(7, 8, 1001), digits = "auto")
#> [1]  8.1400  9.2500 10.2997
```
