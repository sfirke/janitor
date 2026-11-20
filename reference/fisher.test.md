# Apply `stats::fisher.test()` to a two-way tabyl

This generic function overrides
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html). If
the passed table is a two-way tabyl, it runs it through
`janitor::fisher.test.tabyl`, otherwise it just calls
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).

## Usage

``` r
fisher.test(x, ...)

# Default S3 method
fisher.test(x, y = NULL, ...)

# S3 method for class 'tabyl'
fisher.test(x, ...)
```

## Arguments

- x:

  A two-way tabyl, a numeric vector or a factor

- ...:

  Parameters passed to
  [`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html)

- y:

  if x is a vector, must be another vector or factor of the same length

## Value

The same as the one of
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html).

## Examples

``` r
tab <- tabyl(mtcars, gear, cyl)
fisher.test(tab)
#> 
#>  Fisher's Exact Test for Count Data
#> 
#> data:  tab
#> p-value = 8.26e-05
#> alternative hypothesis: two.sided
#> 
```
