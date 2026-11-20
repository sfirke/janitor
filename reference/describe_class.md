# Describe the class(es) of an object

Describe the class(es) of an object

## Usage

``` r
describe_class(x, strict_description = TRUE)

# S3 method for class 'factor'
describe_class(x, strict_description = TRUE)

# Default S3 method
describe_class(x, strict_description = TRUE)
```

## Arguments

- x:

  The object to describe

- strict_description:

  Should differing factor levels be treated as differences for the
  purposes of identifying mismatches? `strict_description = TRUE` is
  stricter and factors with different levels will be treated as
  different classes. `FALSE` is more lenient: for class comparison
  purposes, the variable is just a "factor".

## Value

A character scalar describing the class(es) of an object where if the
scalar will match, columns in a data.frame (or similar object) should
bind together without issue.

## Details

For package developers, an S3 generic method can be written for
`describe_class()` for custom classes that may need more definition than
the default method. This function is called by
[`compare_df_cols()`](https://sfirke.github.io/janitor/reference/compare_df_cols.md).

## Methods (by class)

- `describe_class(factor)`: Describe factors with their levels and if
  they are ordered.

- `describe_class(default)`: List all classes of an object.

## See also

Other data frame type comparison:
[`compare_df_cols()`](https://sfirke.github.io/janitor/reference/compare_df_cols.md),
[`compare_df_cols_same()`](https://sfirke.github.io/janitor/reference/compare_df_cols_same.md)

## Examples

``` r
describe_class(1)
#> [1] "numeric"
describe_class(factor("A"))
#> [1] "factor(levels=c(\"A\"))"
describe_class(ordered(c("A", "B")))
#> [1] "ordered, factor(levels=c(\"A\", \"B\"))"
describe_class(ordered(c("A", "B")), strict_description = FALSE)
#> [1] "factor"
```
