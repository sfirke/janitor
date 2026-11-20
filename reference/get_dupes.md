# Get rows of a `data.frame` with identical values for the specified variables.

For hunting duplicate records during data cleaning. Specify the
data.frame and the variable combination to search for duplicates and get
back the duplicated rows.

## Usage

``` r
get_dupes(dat, ...)
```

## Arguments

- dat:

  The input `data.frame`.

- ...:

  Unquoted variable names to search for duplicates. This takes a
  tidyselect specification.

## Value

A data.frame with the full records where the specified variables have
duplicated values, as well as a variable `dupe_count` showing the number
of rows sharing that combination of duplicated values. If the input
data.frame was of class `tbl_df`, the output is as well.

## Examples

``` r
get_dupes(mtcars, mpg, hp)
#>   mpg  hp dupe_count cyl disp drat    wt  qsec vs am gear carb
#> 1  21 110          2   6  160  3.9 2.620 16.46  0  1    4    4
#> 2  21 110          2   6  160  3.9 2.875 17.02  0  1    4    4

# or called with the magrittr pipe %>% :
mtcars %>% get_dupes(wt)
#>     wt dupe_count  mpg cyl  disp  hp drat  qsec vs am gear carb
#> 1 3.44          3 18.7   8 360.0 175 3.15 17.02  0  0    3    2
#> 2 3.44          3 19.2   6 167.6 123 3.92 18.30  1  0    4    4
#> 3 3.44          3 17.8   6 167.6 123 3.92 18.90  1  0    4    4
#> 4 3.57          2 14.3   8 360.0 245 3.21 15.84  0  0    3    4
#> 5 3.57          2 15.0   8 301.0 335 3.54 14.60  0  1    5    8

# You can use tidyselect helpers to specify variables:
mtcars %>% get_dupes(-c(wt, qsec))
#>   mpg cyl disp  hp drat vs am gear carb dupe_count    wt  qsec
#> 1  21   6  160 110  3.9  0  1    4    4          2 2.620 16.46
#> 2  21   6  160 110  3.9  0  1    4    4          2 2.875 17.02
mtcars %>% get_dupes(starts_with("cy"))
#>    cyl dupe_count  mpg  disp  hp drat    wt  qsec vs am gear carb
#> 1    8         14 18.7 360.0 175 3.15 3.440 17.02  0  0    3    2
#> 2    8         14 14.3 360.0 245 3.21 3.570 15.84  0  0    3    4
#> 3    8         14 16.4 275.8 180 3.07 4.070 17.40  0  0    3    3
#> 4    8         14 17.3 275.8 180 3.07 3.730 17.60  0  0    3    3
#> 5    8         14 15.2 275.8 180 3.07 3.780 18.00  0  0    3    3
#> 6    8         14 10.4 472.0 205 2.93 5.250 17.98  0  0    3    4
#> 7    8         14 10.4 460.0 215 3.00 5.424 17.82  0  0    3    4
#> 8    8         14 14.7 440.0 230 3.23 5.345 17.42  0  0    3    4
#> 9    8         14 15.5 318.0 150 2.76 3.520 16.87  0  0    3    2
#> 10   8         14 15.2 304.0 150 3.15 3.435 17.30  0  0    3    2
#> 11   8         14 13.3 350.0 245 3.73 3.840 15.41  0  0    3    4
#> 12   8         14 19.2 400.0 175 3.08 3.845 17.05  0  0    3    2
#> 13   8         14 15.8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> 14   8         14 15.0 301.0 335 3.54 3.570 14.60  0  1    5    8
#> 15   4         11 22.8 108.0  93 3.85 2.320 18.61  1  1    4    1
#> 16   4         11 24.4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> 17   4         11 22.8 140.8  95 3.92 3.150 22.90  1  0    4    2
#> 18   4         11 32.4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> 19   4         11 30.4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> 20   4         11 33.9  71.1  65 4.22 1.835 19.90  1  1    4    1
#> 21   4         11 21.5 120.1  97 3.70 2.465 20.01  1  0    3    1
#> 22   4         11 27.3  79.0  66 4.08 1.935 18.90  1  1    4    1
#> 23   4         11 26.0 120.3  91 4.43 2.140 16.70  0  1    5    2
#> 24   4         11 30.4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> 25   4         11 21.4 121.0 109 4.11 2.780 18.60  1  1    4    2
#> 26   6          7 21.0 160.0 110 3.90 2.620 16.46  0  1    4    4
#> 27   6          7 21.0 160.0 110 3.90 2.875 17.02  0  1    4    4
#> 28   6          7 21.4 258.0 110 3.08 3.215 19.44  1  0    3    1
#> 29   6          7 18.1 225.0 105 2.76 3.460 20.22  1  0    3    1
#> 30   6          7 19.2 167.6 123 3.92 3.440 18.30  1  0    4    4
#> 31   6          7 17.8 167.6 123 3.92 3.440 18.90  1  0    4    4
#> 32   6          7 19.7 145.0 175 3.62 2.770 15.50  0  1    5    6
```
