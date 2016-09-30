<!-- README.md is generated from README.Rmd. Please edit that file -->
janitor
=======

------------------------------------------------------------------------

[![Travis-CI Build Status](https://travis-ci.org/sfirke/janitor.svg?branch=master)](https://travis-ci.org/sfirke/janitor) [![Coverage Status](https://img.shields.io/codecov/c/github/sfirke/janitor/master.svg)](https://codecov.io/github/sfirke/janitor?branch=master)

**janitor** has simple little functions for examining and cleaning dirty data. An intermediate R user can already do all of this, but with janitor you can save your brainpower for the fun stuff.

<hr/>
> Data scientists, according to interviews and expert estimates, spend from 50 percent to 80 percent of their time mired in this more mundane labor of collecting and preparing unruly digital data, before it can be explored for useful nuggets.
>
> -- *"[For Big-Data Scientists, 'Janitor Work' Is Key Hurdle to Insight](http://www.nytimes.com/2014/08/18/technology/for-big-data-scientists-hurdle-to-insights-is-janitor-work.html)" - The New York Times, 2014*

<hr/>
### Installation

janitor is not yet on CRAN. Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("sfirke/janitor")
```

Using janitor
-------------

Below are quick examples of how janitor functions are commonly used. A full description of each function can be found in janitor's [catalog of functions](https://github.com/sfirke/janitor/blob/master/vignettes/introduction.md).

Janitor is a [\#tidyverse](https://github.com/hadley/tidyverse/blob/master/vignettes/manifesto.Rmd)-oriented package. Specifically, it plays nicely with the `%>%` pipe and is optimized for cleaning data brought in with the [readr](https://github.com/hadley/readr) and [readxl](https://github.com/hadley/readxl) packages.

### Cleaning dirty data

Take this roster of teachers at a fictional American high school, stored in the Microsoft Excel file [dirty\_data.xlsx](https://github.com/sfirke/janitor/blob/master/dirty_data.xlsx): ![All kinds of dirty.](dirty_data.PNG)

Dirtiness includes:

-   Dreadful column names
-   Rows and columns containing Excel formatting but no data
-   Dates stored as numbers
-   Values spread inconsistently over the "Certification" columns

Here's that data after being read in to R:

``` r
library(pacman) # for loading packages
p_load(readxl, janitor, dplyr)

roster_raw <- read_excel("dirty_data.xlsx") # available at http://github.com/sfirke/janitor
glimpse(roster_raw)
#> Observations: 17
#> Variables: 12
#> $ First Name        <chr> "Jason", "Jason", "Alicia", "Ada", "Desus", "Chien-Shiung", "Chien-Shiung", NA,...
#> $ Last Name         <chr> "Bourne", "Bourne", "Keys", "Lovelace", "Nice", "Wu", "Wu", NA, "Joyce", "Lamar...
#> $ Employee Status   <chr> "Teacher", "Teacher", "Teacher", "Teacher", "Administration", "Teacher", "Teach...
#> $ Subject           <chr> "PE", "Drafting", "Music", NA, "Dean", "Physics", "Chemistry", NA, "English", "...
#> $ Hire Date         <dbl> 39690, 39690, 37118, 27515, 41431, 11037, 11037, NA, 32994, 27919, 42221, 34700...
#> $ % Allocated       <dbl> 0.75, 0.25, 1.00, 1.00, 1.00, 0.50, 0.50, NA, 0.50, 0.50, NA, NA, 0.80, NA, NA,...
#> $ Full time?        <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", NA, "No", "No", "No", "No", "N...
#> $ do not edit! ---> <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $ Certification     <chr> "Physical ed", "Physical ed", "Instr. music", "PENDING", "PENDING", "Science 6-...
#> $ Certification     <chr> "Theater", "Theater", "Vocal music", "Computers", NA, "Physics", "Physics", NA,...
#> $ Certification     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $                   <dttm> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA
```

Excel formatting led to an untitled empty column and 5 empty rows at the bottom of the table (only 12 records have any actual data). Bad column names are preserved.

Clean it with janitor functions:

``` r
roster <- roster_raw %>%
  clean_names() %>%
  remove_empty_rows() %>%
  remove_empty_cols() %>%
  convert_to_NA(c("TBD", "PENDING")) %>%
  mutate(hire_date = excel_numeric_to_date(hire_date),
         main_cert = use_first_valid_of(certification, certification_2)) %>%
  select(-certification, -certification_2) # drop unwanted columns

roster
#> # A tibble: 12 × 8
#>      first_name last_name employee_status    subject  hire_date percent_allocated full_time      main_cert
#>           <chr>     <chr>           <chr>      <chr>     <date>             <dbl>     <chr>          <chr>
#> 1         Jason    Bourne         Teacher         PE 2008-08-30              0.75       Yes    Physical ed
#> 2         Jason    Bourne         Teacher   Drafting 2008-08-30              0.25       Yes    Physical ed
#> 3        Alicia      Keys         Teacher      Music 2001-08-15              1.00       Yes   Instr. music
#> 4           Ada  Lovelace         Teacher       <NA> 1975-05-01              1.00       Yes      Computers
#> 5         Desus      Nice  Administration       Dean 2013-06-06              1.00       Yes           <NA>
#> 6  Chien-Shiung        Wu         Teacher    Physics 1930-03-20              0.50       Yes   Science 6-12
#> 7  Chien-Shiung        Wu         Teacher  Chemistry 1930-03-20              0.50       Yes   Science 6-12
#> 8         James     Joyce         Teacher    English 1990-05-01              0.50        No   English 6-12
#> 9          Hedy    Lamarr         Teacher    Science 1976-06-08              0.50        No           <NA>
#> 10       Carlos    Boozer           Coach Basketball 2015-08-05                NA        No    Physical ed
#> 11        Young    Boozer           Coach       <NA> 1995-01-01                NA        No Political sci.
#> 12      Micheal    Larsen         Teacher    English 2009-09-15              0.80        No    Vocal music
```

The core janitor cleaning function is `clean_names()` - call it whenever you load data into R.

### Examining dirty data

#### Finding duplicates

Use `get_dupes()` to identify and examine duplicate records during data cleaning. Let's see if any teachers are listed more than once:

``` r
roster %>% get_dupes(first_name, last_name)
#> # A tibble: 4 × 9
#>     first_name last_name dupe_count employee_status   subject  hire_date percent_allocated full_time
#>          <chr>     <chr>      <int>           <chr>     <chr>     <date>             <dbl>     <chr>
#> 1 Chien-Shiung        Wu          2         Teacher   Physics 1930-03-20              0.50       Yes
#> 2 Chien-Shiung        Wu          2         Teacher Chemistry 1930-03-20              0.50       Yes
#> 3        Jason    Bourne          2         Teacher        PE 2008-08-30              0.75       Yes
#> 4        Jason    Bourne          2         Teacher  Drafting 2008-08-30              0.25       Yes
#> # ... with 1 more variables: main_cert <chr>
```

Yes, some teachers appear twice. We ought to address this before counting employees.

#### Tabulating tools

janitor has several functions for quick counts. The big ones are `tabyl()` and `crosstab()`.

Notably, they can be called two ways:

-   On vectors - e.g., `tabyl(roster$subject)`
-   On a piped-in data.frame: `roster %>% tabyl(subject)`.
    -   This allows for dplyr commands earlier in the pipeline

##### tabyl()

Like `table()`, but pipe-able and more functional.

``` r
roster %>%
  tabyl(subject)
#>       subject n    percent valid_percent
#> 1  Basketball 1 0.08333333           0.1
#> 2   Chemistry 1 0.08333333           0.1
#> 3        Dean 1 0.08333333           0.1
#> 4    Drafting 1 0.08333333           0.1
#> 5     English 2 0.16666667           0.2
#> 6       Music 1 0.08333333           0.1
#> 7          PE 1 0.08333333           0.1
#> 8     Physics 1 0.08333333           0.1
#> 9     Science 1 0.08333333           0.1
#> 10       <NA> 2 0.16666667            NA
```

##### crosstab()

``` r
roster %>%
  filter(hire_date > as.Date("1950-01-01")) %>%
  crosstab(employee_status, full_time)
#>   employee_status No Yes
#> 1  Administration  0   1
#> 2           Coach  2   0
#> 3         Teacher  3   4
```

##### Prettifying

Other janitor functions dress up the results of these tabulation calls for fast, basic reporting. Here are some of the functions that augment a summary table for reporting:

``` r
roster %>%
  tabyl(employee_status, sort = TRUE) %>%
  add_totals_row()
#>   employee_status  n    percent
#> 1         Teacher  9 0.75000000
#> 2           Coach  2 0.16666667
#> 3  Administration  1 0.08333333
#> 4           Total 12 1.00000000

roster %>%
  crosstab(full_time, employee_status) %>%
  adorn_crosstab(denom = "col", show_totals = TRUE)
#>   full_time Administration      Coach   Teacher     Total
#> 1        No       0.0% (0) 100.0% (2) 33.3% (3) 41.7% (5)
#> 2       Yes     100.0% (1)   0.0% (0) 66.7% (6) 58.3% (7)
```

Together, these tabulation functions reduce R's deficit against Excel and SPSS when it comes to quick, informative counts.

Contact me
----------

You are welcome to:

-   submit suggestions and bug-reports: <https://github.com/sfirke/janitor/issues>
-   send a pull request: <https://github.com/sfirke/janitor/>
-   let me know what you think on twitter @samfirke
-   compose a friendly e-mail to: <img src = "http://samfirke.com/wp-content/uploads/2016/07/email_address_whitespace_top.png" alt = "samuel.firke AT gmail" width = "210"/>
