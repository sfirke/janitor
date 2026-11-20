# Deprecated Functions in Package janitor

These functions have already become defunct or may be defunct as soon as
the next release.

## Details

- [`adorn_crosstab()`](https://sfirke.github.io/janitor/reference/adorn_crosstab.md)
  -\> `adorn_`

- [`crosstab()`](https://sfirke.github.io/janitor/reference/crosstab.md)
  -\> [`tabyl()`](https://sfirke.github.io/janitor/reference/tabyl.md)

- [`use_first_valid_of()`](https://sfirke.github.io/janitor/reference/use_first_valid_of.md)
  -\>
  [`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html)

- [`convert_to_NA()`](https://sfirke.github.io/janitor/reference/convert_to_NA.md)
  -\>
  [`dplyr::na_if()`](https://dplyr.tidyverse.org/reference/na_if.html)

- [`add_totals_col()`](https://sfirke.github.io/janitor/reference/add_totals_col.md)
  -\>
  [`adorn_totals(where = "col")`](https://sfirke.github.io/janitor/reference/adorn_totals.md)

- [`add_totals_row()`](https://sfirke.github.io/janitor/reference/add_totals_row.md)
  -\>
  [`adorn_totals()`](https://sfirke.github.io/janitor/reference/adorn_totals.md)

- [`remove_empty_rows()`](https://sfirke.github.io/janitor/reference/remove_empty_rows.md)
  -\>
  [`remove_empty("rows")`](https://sfirke.github.io/janitor/reference/remove_empty.md)

- [`remove_empty_cols()`](https://sfirke.github.io/janitor/reference/remove_empty_cols.md)
  -\>
  [`remove_empty("cols")`](https://sfirke.github.io/janitor/reference/remove_empty.md)
