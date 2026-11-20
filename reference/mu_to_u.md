# Constant to help map from mu to u

This is a character vector with names of all known Unicode code points
that look like the Greek mu or the micro symbol and values of "u". This
is intended to simplify mapping from mu or micro in Unicode to the
character "u" with
[`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.md)
and
[`make_clean_names()`](https://sfirke.github.io/janitor/reference/make_clean_names.md).

## Usage

``` r
mu_to_u
```

## Format

An object of class `character` of length 10.

## Details

See the help in
[`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.md)
for how to use this.

## See also

Other Set names:
[`clean_names()`](https://sfirke.github.io/janitor/reference/clean_names.md),
[`find_header()`](https://sfirke.github.io/janitor/reference/find_header.md),
[`row_to_names()`](https://sfirke.github.io/janitor/reference/row_to_names.md)
