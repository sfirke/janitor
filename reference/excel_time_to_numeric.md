# Convert a time that may be inconsistently or inconveniently formatted from Microsoft Excel to a numeric number of seconds between 0 and 86400.

Convert a time that may be inconsistently or inconveniently formatted
from Microsoft Excel to a numeric number of seconds between 0 and 86400.

## Usage

``` r
excel_time_to_numeric(time_value, round_seconds = TRUE)
```

## Arguments

- time_value:

  A vector of values to convert (see Details)

- round_seconds:

  Should the output number of seconds be rounded to an integer?

## Value

A vector of numbers \>= 0 and \<86400

## Details

`time_value` may be one of the following formats:

- numericThe input must be a value from 0 to 1 (exclusive of 1); this
  value is returned as-is.

- POSIXlt or POSIXctThe input must be on the day 1899-12-31 (any other
  day will cause an error). The time of day is extracted and converted
  to a fraction of a day.

- characterAny of the following (or a mixture of the choices):

  - A character string that is a number between 0 and 1 (exclusive of
    1). This value will be converted like a numeric value.

  - A character string that looks like a date on 1899-12-31
    (specifically, it must start with `"1899-12-31 "`), converted like a
    POSIXct object as described above.

  - A character string that looks like a time. Choices are 12-hour time
    as hour, minute, and optionally second followed by "am" or "pm"
    (case insensitive) or 24-hour time when hour, minute, optionally
    second, and no "am" or "pm" is included.

## See also

[`excel_numeric_to_date()`](https://sfirke.github.io/janitor/reference/excel_numeric_to_date.md)

Other date-time cleaning:
[`convert_to_date()`](https://sfirke.github.io/janitor/reference/convert_to_date.md),
[`excel_numeric_to_date()`](https://sfirke.github.io/janitor/reference/excel_numeric_to_date.md),
[`sas_numeric_to_date()`](https://sfirke.github.io/janitor/reference/sas_numeric_to_date.md)
