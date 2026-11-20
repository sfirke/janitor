# Pipe operator

See `magrittr::%>%` for details.

## Usage

``` r
lhs %>% rhs
```

## Arguments

- lhs:

  A value or the magrittr placeholder.

- rhs:

  A function call using the magrittr semantics.

## Value

The result of calling `rhs(lhs)`.

## Examples

``` r
mtcars %>%
  tabyl(carb, cyl) %>%
  adorn_totals()
#>   carb  4 6  8
#>      1  5 2  0
#>      2  6 0  4
#>      3  0 0  3
#>      4  0 4  6
#>      6  0 1  0
#>      8  0 0  1
#>  Total 11 7 14
```
