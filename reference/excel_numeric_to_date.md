# Convert dates encoded as serial numbers to Date class.

Converts numbers like `42370` into date values like `2016-01-01`.

Defaults to the modern Excel date encoding system. However, Excel for
Mac 2008 and earlier Mac versions of Excel used a different date system.
To determine what platform to specify: if the date 2016-01-01 is
represented by the number 42370 in your spreadsheet, it's the modern
system. If it's 40908, it's the old Mac system. More on date encoding
systems at
http://support.office.com/en-us/article/Date-calculations-in-Excel-e7fe7167-48a9-4b96-bb53-5612a800b487.

A list of all timezones is available from
[`base::OlsonNames()`](https://rdrr.io/r/base/timezones.html), and the
current timezone is available from
[`base::Sys.timezone()`](https://rdrr.io/r/base/timezones.html).

If your input data has a mix of Excel numeric dates and actual dates,
see the more powerful functions
[`convert_to_date()`](https://sfirke.github.io/janitor/reference/convert_to_date.md)
and
[`convert_to_datetime()`](https://sfirke.github.io/janitor/reference/convert_to_date.md).

## Usage

``` r
excel_numeric_to_date(
  date_num,
  date_system = "modern",
  include_time = FALSE,
  round_seconds = TRUE,
  tz = Sys.timezone()
)
```

## Arguments

- date_num:

  numeric vector of serial numbers to convert.

- date_system:

  the date system, either `"modern"` or `"mac pre-2011"`.

- include_time:

  Include the time (hours, minutes, seconds) in the output? (See
  details)

- round_seconds:

  Round the seconds to an integer (only has an effect when
  `include_time` is `TRUE`)?

- tz:

  Time zone, used when `include_time = TRUE` (see details for more
  information on timezones).

## Value

Returns a vector of class Date if `include_time` is `FALSE`. Returns a
vector of class POSIXlt if `include_time` is `TRUE`.

## Details

When using `include_time=TRUE`, days with leap seconds will not be
accurately handled as they do not appear to be accurately handled by
Windows (as described in
https://support.microsoft.com/en-us/help/2722715/support-for-the-leap-second).

## See also

[`excel_time_to_numeric()`](https://sfirke.github.io/janitor/reference/excel_time_to_numeric.md)

Other date-time cleaning:
[`convert_to_date()`](https://sfirke.github.io/janitor/reference/convert_to_date.md),
[`excel_time_to_numeric()`](https://sfirke.github.io/janitor/reference/excel_time_to_numeric.md),
[`sas_numeric_to_date()`](https://sfirke.github.io/janitor/reference/sas_numeric_to_date.md)

## Examples

``` r
excel_numeric_to_date(40000)
#> [1] "2009-07-06"
excel_numeric_to_date(40000.5) # No time is included
#> [1] "2009-07-06"
excel_numeric_to_date(40000.5, include_time = TRUE) # Time is included
#> [1] "2009-07-06 12:00:00 UTC"
excel_numeric_to_date(40000.521, include_time = TRUE) # Time is included
#> [1] "2009-07-06 12:30:14 UTC"
excel_numeric_to_date(40000.521,
  include_time = TRUE,
  round_seconds = FALSE
) # Time with fractional seconds is included
#> [1] "2009-07-06 12:30:14 UTC"
```
