# Convert a SAS date, time or date/time to an R object

Convert a SAS date, time or date/time to an R object

## Usage

``` r
sas_numeric_to_date(date_num, datetime_num, time_num, tz = "UTC")
```

## Arguments

- date_num:

  numeric vector of serial numbers to convert.

- datetime_num:

  numeric vector of date/time numbers (seconds since midnight
  1960-01-01) to convert

- time_num:

  numeric vector of time numbers (seconds since midnight on the current
  day) to convert

- tz:

  Time zone, used when `include_time = TRUE` (see details for more
  information on timezones).

## Value

If a date and time or datetime are provided, a POSIXct object. If a date
is provided, a Date object. If a time is provided, an hms::hms object

## References

SAS Date, Time, and Datetime Values reference (retrieved on 2022-03-08):
https://v8doc.sas.com/sashtml/lrcon/zenid-63.htm

## See also

Other date-time cleaning:
[`convert_to_date()`](https://sfirke.github.io/janitor/reference/convert_to_date.md),
[`excel_numeric_to_date()`](https://sfirke.github.io/janitor/reference/excel_numeric_to_date.md),
[`excel_time_to_numeric()`](https://sfirke.github.io/janitor/reference/excel_time_to_numeric.md)

## Examples

``` r
sas_numeric_to_date(date_num = 15639) # 2002-10-26
#> [1] "2002-10-26"
sas_numeric_to_date(datetime_num = 1217083532, tz = "UTC") # 1998-07-26T14:45:32Z
#> [1] "1998-07-26 14:45:32 UTC"
sas_numeric_to_date(date_num = 15639, time_num = 3600, tz = "UTC") # 2002-10-26T01:00:00Z
#> [1] "2002-10-26 01:00:00 UTC"
sas_numeric_to_date(time_num = 3600) # 01:00:00
#> 01:00:00
```
