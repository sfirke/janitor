# Parse dates from many formats

Convert many date and date-time (POSIXct) formats as may be received
from Microsoft Excel.

## Usage

``` r
convert_to_date(
  x,
  ...,
  character_fun = lubridate::ymd,
  string_conversion_failure = c("error", "warning")
)

convert_to_datetime(
  x,
  ...,
  tz = "UTC",
  character_fun = lubridate::ymd_hms,
  string_conversion_failure = c("error", "warning")
)
```

## Arguments

- x:

  The object to convert

- ...:

  Passed to further methods. Eventually may be passed to
  [`excel_numeric_to_date()`](https://sfirke.github.io/janitor/reference/excel_numeric_to_date.md),
  [`base::as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html), or
  [`base::as.Date()`](https://rdrr.io/r/base/as.Date.html).

- character_fun:

  A function to convert non-numeric-looking, non-`NA` values in `x` to
  POSIXct objects.

- string_conversion_failure:

  If a character value fails to parse into the desired class and instead
  returns `NA`, should the function return the result with a warning or
  throw an error?

- tz:

  The timezone for POSIXct output, unless an object is POSIXt already.
  Ignored for Date output.

## Value

POSIXct objects for `convert_to_datetime()` or Date objects for
`convert_to_date()`.

## Details

Character conversion checks if it matches something that looks like a
Microsoft Excel numeric date, converts those to numeric, and then runs
convert_to_datetime_helper() on those numbers. Then, character to Date
or POSIXct conversion occurs via `character_fun(x, ...)` or
`character_fun(x, tz=tz, ...)`, respectively.

## See also

Other date-time cleaning:
[`excel_numeric_to_date()`](https://sfirke.github.io/janitor/reference/excel_numeric_to_date.md),
[`excel_time_to_numeric()`](https://sfirke.github.io/janitor/reference/excel_time_to_numeric.md),
[`sas_numeric_to_date()`](https://sfirke.github.io/janitor/reference/sas_numeric_to_date.md)

## Examples

``` r
convert_to_date("2009-07-06")
#> [1] "2009-07-06"
convert_to_date(40000)
#> [1] "2009-07-06"
convert_to_date("40000.1")
#> [1] "2009-07-06"
# Mixed date source data can be provided.
convert_to_date(c("2020-02-29", "40000.1"))
#> [1] "2020-02-29" "2009-07-06"
convert_to_datetime(
  c("2009-07-06", "40000.1", "40000", NA),
  character_fun = lubridate::ymd_h, truncated = 1, tz = "UTC"
)
#> [1] "2009-07-06 00:00:00 UTC" "2009-07-06 02:24:00 UTC"
#> [3] "2009-07-06 00:00:00 UTC" NA                       
```
