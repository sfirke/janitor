# Cleans names of an object (usually a data.frame).

Resulting names are unique and consist only of the `_` character,
numbers, and letters. Capitalization preferences can be specified using
the `case` parameter.

Accented characters are transliterated to ASCII. For example, an "o"
with a German umlaut over it becomes "o", and the Spanish character
"enye" becomes "n".

This function takes and returns a data.frame, for ease of piping with
`%>%`. For the underlying function that works on a character vector of
names, see
[`make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.md).
`clean_names` relies on the versatile function
[`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html),
which accepts many arguments. See that function's documentation for
ideas on getting the most out of `clean_names`. A few examples are
included below.

A common issue is that the micro/mu symbol is replaced by "m" instead of
"u". The replacement with "m" is more correct when doing Greek-to-ASCII
transliteration but less correct when doing scientific data-to-ASCII
transliteration. A warning will be generated if the "m" replacement
occurs. To replace with "u", please add the argument
`replace=janitor:::mu_to_u` which is a character vector mapping all
known mu or micro Unicode code points (characters) to "u".

## Usage

``` r
clean_names(dat, ...)

# Default S3 method
clean_names(dat, ..., set_labels = FALSE)

# S3 method for class 'sf'
clean_names(dat, ..., set_labels = FALSE)

# S3 method for class 'tbl_graph'
clean_names(dat, ...)

# S3 method for class 'tbl_lazy'
clean_names(dat, ...)
```

## Arguments

- dat:

  The input `data.frame`.

- ...:

  Arguments passed on to
  [`make_clean_names`](https://sfirke.github.io/janitor/reference/make_clean_names.md)

  `case`

  :   The desired target case (default is `"snake"`) will be passed to
      [`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)
      with the exception of "old_janitor", which exists only to support
      legacy code (it preserves the behavior of `clean_names()` prior to
      addition of the "case" argument (janitor versions \<= 0.3.1).
      "old_janitor" is not intended for new code. See
      [`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)
      for a wide variety of supported cases, including "sentence" and
      "title" case.

  `replace`

  :   A named character vector where the name is replaced by the value.

  `ascii`

  :   Convert the names to ASCII (`TRUE`, default) or not (`FALSE`).

  `use_make_names`

  :   Should [`make.names()`](https://rdrr.io/r/base/make.names.html) be
      applied to ensure that the output is usable as a name without
      quoting? (Avoiding
      [`make.names()`](https://rdrr.io/r/base/make.names.html) ensures
      that the output is locale-independent but quoting may be
      required.)

  `allow_dupes`

  :   Allow duplicates in the returned names (`TRUE`) or not (`FALSE`,
      the default).

  `sep_in`

  :   (short for separator input) if character, is interpreted as a
      regular expression (wrapped internally into
      [`stringr::regex()`](https://stringr.tidyverse.org/reference/modifiers.html)).
      The default value is a regular expression that matches any
      sequence of non-alphanumeric values. All matches will be replaced
      by underscores (additionally to `"_"` and `" "`, for which this is
      always true, even if `NULL` is supplied). These underscores are
      used internally to split the strings into substrings and specify
      the word boundaries.

  `parsing_option`

  :   An integer that will determine the parsing_option.

      - 1: `"RRRStudio" -> "RRR_Studio"`

      - 2: `"RRRStudio" -> "RRRS_tudio"`

      - 3: `"RRRStudio" -> "RRRSStudio"`. This will become for example
        `"Rrrstudio"` when we convert to lower camel case.

      - -1, -2, -3: These `parsing_options`'s will suppress the
        conversion after non-alphanumeric values.

      - 0: no parsing

  `transliterations`

  :   A character vector (if not `NULL`). The entries of this argument
      need to be elements of
      [`stringi::stri_trans_list()`](https://rdrr.io/pkg/stringi/man/stri_trans_list.html)
      (like "Latin-ASCII", which is often useful) or names of lookup
      tables (currently only "german" is supported). In the order of the
      entries the letters of the input string will be transliterated via
      [`stringi::stri_trans_general()`](https://rdrr.io/pkg/stringi/man/stri_trans_general.html)
      or replaced via the matches of the lookup table. When named
      character elements are supplied as part of \`transliterations\`,
      anything that matches the names is replaced by the corresponding
      value. You should use this feature with care in case of
      `case = "parsed"`, `case = "internal_parsing"` and
      `case = "none"`, since for upper case letters, which have
      transliterations/replacements of length 2, the second letter will
      be transliterated to lowercase, for example Oe, Ae, Ss, which
      might not always be what is intended. In this case you can make
      usage of the option to supply named elements and specify the
      transliterations yourself.

  `numerals`

  :   A character specifying the alignment of numerals (`"middle"`,
      `left`, `right`, `asis` or `tight`). I.e. `numerals = "left"`
      ensures that no output separator is in front of a digit.

- set_labels:

  If set to `TRUE`, old names are stored as labels in each column of the
  returned data.frame.

## Value

A `data.frame` with clean names.

## Details

`clean_names()` is intended to be used on `data.frames` and
`data.frame`-like objects. For this reason there are methods to support
using `clean_names()` on `sf` and `tbl_graph` (from `tidygraph`) objects
as well as on database connections through `dbplyr`. For cleaning other
named objects like named lists and vectors, use
[`make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.md).
When `set_labels` is set to `TRUE`, the old names, stored as column
labels, can be restored using `sjlabelled::label_to_colnames()`.

## See also

Other Set names:
[`find_header()`](https://sfirke.github.io/janitor/reference/find_header.md),
[`mu_to_u`](https://sfirke.github.io/janitor/reference/mu_to_u.md),
[`row_to_names()`](https://sfirke.github.io/janitor/reference/row_to_names.md)

## Examples

``` r

# --- Simple Usage ---
x <- data.frame(caseID = 1, DOB = 2, Other = 3)
clean_names(x)
#>   case_id dob other
#> 1       1   2     3

# or pipe in the input data.frame:
x %>%
  clean_names()
#>   case_id dob other
#> 1       1   2     3

# if you prefer camelCase variable names:
x %>%
  clean_names(., "lower_camel")
#>   caseId dob other
#> 1      1   2     3

# (not run) run clean_names after reading in a spreadsheet:
# library(readxl)
# read_excel("messy_excel_file.xlsx") %>%
#   clean_names()

# --- Taking advantage of the underlying snakecase::to_any_case arguments ---

# Restore column names to Title Case, e.g., for plotting
mtcars %>%
  clean_names(case = "title")
#>                      Mpg Cyl  Disp  Hp Drat    Wt  Qsec Vs Am Gear Carb
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

# Tell clean_names to leave certain abbreviations untouched:
x %>%
  clean_names(case = "upper_camel", abbreviations = c("ID", "DOB"))
#>   CaseID DOB Other
#> 1      1   2     3
```
