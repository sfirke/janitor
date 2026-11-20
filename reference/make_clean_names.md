# Cleans a vector of text, typically containing the names of an object.

Resulting strings are unique and consist only of the `_` character,
numbers, and letters. By default, the resulting strings will only
consist of ASCII characters, but non-ASCII (e.g. Unicode) may be allowed
by setting `ascii = FALSE`. Capitalization preferences can be specified
using the `case` parameter.

For use on the names of a data.frame, e.g., in a `%>%` pipeline, call
the convenience function
[`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.md).

When `ascii = TRUE` (the default), accented characters are
transliterated to ASCII. For example, an "o" with a German umlaut over
it becomes "o", and the Spanish character "enye" becomes "n".

The order of operations is: make replacements, (optional) ASCII
conversion, remove initial spaces and punctuation, apply
[`base::make.names()`](https://rdrr.io/r/base/make.names.html), apply
`snakecase::to_any_case(()`, and add numeric suffixes to resolve any
duplicated names.

This function relies on
[`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)
and can take advantage of its versatility. For instance, an abbreviation
like "ID" can have its capitalization preserved by passing the argument
`abbreviations = "ID"`. See the documentation for
[`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)
for more about how to use its features.

On some systems, not all transliterators to ASCII are available. If this
is the case on your system, all available transliterators will be used,
and a warning will be issued once per session indicating that results
may be different when run on a different system. That warning can be
disabled with `options(janitor_warn_transliterators=FALSE)`.

If the objective of your call to `make_clean_names()` is only to
translate to ASCII, try the following instead:
`stringi::stri_trans_general(x, id="Any-Latin;Greek-Latin;Latin-ASCII")`.

## Usage

``` r
make_clean_names(
  string,
  case = "snake",
  replace = c(`'` = "", `"` = "", `%` = "_percent_", `#` = "_number_"),
  ascii = TRUE,
  use_make_names = TRUE,
  allow_dupes = FALSE,
  sep_in = "\\.",
  transliterations = "Latin-ASCII",
  parsing_option = 1,
  numerals = "asis",
  ...
)
```

## Arguments

- string:

  A character vector of names to clean.

- case:

  The desired target case (default is `"snake"`) will be passed to
  [`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)
  with the exception of "old_janitor", which exists only to support
  legacy code (it preserves the behavior of
  [`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.md)
  prior to addition of the "case" argument (janitor versions \<= 0.3.1).
  "old_janitor" is not intended for new code. See
  [`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)
  for a wide variety of supported cases, including "sentence" and
  "title" case.

- replace:

  A named character vector where the name is replaced by the value.

- ascii:

  Convert the names to ASCII (`TRUE`, default) or not (`FALSE`).

- use_make_names:

  Should [`make.names()`](https://rdrr.io/r/base/make.names.html) be
  applied to ensure that the output is usable as a name without quoting?
  (Avoiding [`make.names()`](https://rdrr.io/r/base/make.names.html)
  ensures that the output is locale-independent but quoting may be
  required.)

- allow_dupes:

  Allow duplicates in the returned names (`TRUE`) or not (`FALSE`, the
  default).

- sep_in:

  (short for separator input) if character, is interpreted as a regular
  expression (wrapped internally into
  [`stringr::regex()`](https://stringr.tidyverse.org/reference/modifiers.html)).
  The default value is a regular expression that matches any sequence of
  non-alphanumeric values. All matches will be replaced by underscores
  (additionally to `"_"` and `" "`, for which this is always true, even
  if `NULL` is supplied). These underscores are used internally to split
  the strings into substrings and specify the word boundaries.

- transliterations:

  A character vector (if not `NULL`). The entries of this argument need
  to be elements of
  [`stringi::stri_trans_list()`](https://rdrr.io/pkg/stringi/man/stri_trans_list.html)
  (like "Latin-ASCII", which is often useful) or names of lookup tables
  (currently only "german" is supported). In the order of the entries
  the letters of the input string will be transliterated via
  [`stringi::stri_trans_general()`](https://rdrr.io/pkg/stringi/man/stri_trans_general.html)
  or replaced via the matches of the lookup table. When named character
  elements are supplied as part of \`transliterations\`, anything that
  matches the names is replaced by the corresponding value. You should
  use this feature with care in case of `case = "parsed"`,
  `case = "internal_parsing"` and `case = "none"`, since for upper case
  letters, which have transliterations/replacements of length 2, the
  second letter will be transliterated to lowercase, for example Oe, Ae,
  Ss, which might not always be what is intended. In this case you can
  make usage of the option to supply named elements and specify the
  transliterations yourself.

- parsing_option:

  An integer that will determine the parsing_option.

  - 1: `"RRRStudio" -> "RRR_Studio"`

  - 2: `"RRRStudio" -> "RRRS_tudio"`

  - 3: `"RRRStudio" -> "RRRSStudio"`. This will become for example
    `"Rrrstudio"` when we convert to lower camel case.

  - -1, -2, -3: These `parsing_options`'s will suppress the conversion
    after non-alphanumeric values.

  - 0: no parsing

- numerals:

  A character specifying the alignment of numerals (`"middle"`, `left`,
  `right`, `asis` or `tight`). I.e. `numerals = "left"` ensures that no
  output separator is in front of a digit.

- ...:

  Arguments passed on to
  [`snakecase::to_any_case`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)

  `abbreviations`

  :   character. (Case insensitive) matched abbreviations are surrounded
      by underscores. In this way, they can get recognized by the
      parser. This is useful when e.g. `parsing_option` 1 is needed for
      the use case, but some abbreviations but some substrings would
      require `parsing_option` 2. Furthermore, this argument also
      specifies the formatting of abbreviations in the output for the
      cases title, mixed, lower and upper camel. E.g. for upper camel
      the first letter is always in upper case, but when the
      abbreviation is supplied in upper case, this will also be visible
      in the output.

      Use this feature with care: One letter abbreviations and
      abbreviations next to each other are hard to read and also not
      easy to parse for further processing.

  `sep_out`

  :   (short for separator output) String that will be used as
      separator. The defaults are `"_"` and `""`, regarding the
      specified `case`. When `length(sep_out) > 1`, the last element of
      `sep_out` gets recycled and separators are incorporated per string
      according to their order.

  `unique_sep`

  :   A string. If not `NULL`, then duplicated names will get a suffix
      integer in the order of their appearance. The suffix is separated
      by the supplied string to this argument.

  `empty_fill`

  :   A string. If it is supplied, then each entry that matches "" will
      be replaced by the supplied string to this argument.

  `prefix`

  :   prefix (string).

  `postfix`

  :   postfix (string).

## Value

Returns the "cleaned" character vector.

## See also

[`snakecase::to_any_case()`](https://rdrr.io/pkg/snakecase/man/to_any_case.html)

## Examples

``` r

# cleaning the names of a vector:
x <- structure(1:3, names = c("name with space", "TwoWords", "total $ (2009)"))
x
#> name with space        TwoWords  total $ (2009) 
#>               1               2               3 
names(x) <- make_clean_names(names(x))
x # now has cleaned names
#> name_with_space       two_words      total_2009 
#>               1               2               3 

# if you prefer camelCase variable names:
make_clean_names(names(x), "small_camel")
#> [1] "nameWithSpace" "twoWords"      "total2009"    

# similar to janitor::clean_names(poorly_named_df):
# not run:
# make_clean_names(names(poorly_named_df))
```
