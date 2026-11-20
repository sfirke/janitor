# Remove `tabyl` attributes from a data.frame.

Strips away all `tabyl`-related attributes from a data.frame.

## Usage

``` r
untabyl(dat)
```

## Arguments

- dat:

  a `data.frame` of class `tabyl`.

## Value

the same `data.frame`, but without the `tabyl` class and attributes.

## Examples

``` r

mtcars %>%
  tabyl(am) %>%
  untabyl() %>%
  attributes() # tabyl-specific attributes are gone
#> $names
#> [1] "am"      "n"       "percent"
#> 
#> $class
#> [1] "data.frame"
#> 
#> $row.names
#> [1] 1 2
#> 
```
