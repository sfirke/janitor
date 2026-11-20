# Apply `stats::chisq.test()` to a two-way tabyl

This generic function overrides
[`stats::chisq.test`](https://rdrr.io/r/stats/chisq.test.html). If the
passed table is a two-way tabyl, it runs it through
janitor::chisq.test.tabyl, otherwise it just calls
[`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

## Usage

``` r
chisq.test(x, ...)

# Default S3 method
chisq.test(x, y = NULL, ...)

# S3 method for class 'tabyl'
chisq.test(x, tabyl_results = TRUE, ...)
```

## Arguments

- x:

  a two-way tabyl, a numeric vector or a factor

- ...:

  other parameters passed to
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)

- y:

  if x is a vector, must be another vector or factor of the same length

- tabyl_results:

  If `TRUE` and `x` is a tabyl object, also return `observed`,
  `expected`, `residuals` and `stdres` as tabyl.

## Value

The result is the same as the one of
[`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html). If
`tabyl_results` is `TRUE`, the returned tables `observed`, `expected`,
`residuals` and `stdres` are converted to tabyls.

## Examples

``` r
tab <- tabyl(mtcars, gear, cyl)
chisq.test(tab)
#> Warning: Chi-squared approximation may be incorrect
#> 
#>  Pearson's Chi-squared test
#> 
#> data:  tab
#> X-squared = 18.036, df = 4, p-value = 0.001214
#> 
chisq.test(tab)$residuals
#> Warning: Chi-squared approximation may be incorrect
#>  gear          4           6          8
#>     3 -1.8303523 -0.70731720  2.1225827
#>     4  1.9079181  0.84866842 -2.2912878
#>     5  0.2145291 -0.08964215 -0.1267731
```
