# Like `paste()`, but missing values are omitted

Like [`paste()`](https://rdrr.io/r/base/paste.html), but missing values
are omitted

## Usage

``` r
paste_skip_na(..., sep = " ", collapse = NULL)
```

## Arguments

- ..., sep, collapse:

  See [`base::paste()`](https://rdrr.io/r/base/paste.html)

## Value

A character vector of pasted values.

## Details

If all values are missing, the value from the first argument is
preserved.

## Examples

``` r
paste_skip_na(NA) # NA_character_
#> [1] NA
paste_skip_na("A", NA) # "A"
#> [1] "A"
paste_skip_na("A", NA, c(NA, "B"), sep = ",") # c("A", "A,B")
#> [1] "A"   "A,B"
```
