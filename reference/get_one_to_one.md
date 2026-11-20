# Find the list of columns that have a 1:1 mapping to each other

Find the list of columns that have a 1:1 mapping to each other

## Usage

``` r
get_one_to_one(dat)
```

## Arguments

- dat:

  A `data.frame` or similar object

## Value

A list with one element for each group of columns that map identically
to each other.

## Examples

``` r
foo <- data.frame(
  Lab_Test_Long = c("Cholesterol, LDL", "Cholesterol, LDL", "Glucose"),
  Lab_Test_Short = c("CLDL", "CLDL", "GLUC"),
  LOINC = c(12345, 12345, 54321),
  Person = c("Sam", "Bill", "Sam"),
  stringsAsFactors = FALSE
)
get_one_to_one(foo)
#> [[1]]
#> [1] "Lab_Test_Long"  "Lab_Test_Short" "LOINC"         
#> 
```
