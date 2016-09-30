<!-- README.md is generated from README.Rmd. Please edit that file -->
janitor
=======

------------------------------------------------------------------------

[![Travis-CI Build Status](https://travis-ci.org/sfirke/janitor.svg?branch=master)](https://travis-ci.org/sfirke/janitor) [![Coverage Status](https://img.shields.io/codecov/c/github/sfirke/janitor/master.svg)](https://codecov.io/github/sfirke/janitor?branch=master)

**janitor** has simple little functions for examining and cleaning dirty data. Save your brain for the fun stuff.

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

It suffers from:

-   Illegal, clunky, and duplicated names
-   Empty columns and rows used as dividers, and rows/columns that are empty but contain Excel formatting
-   Dates stored as numbers
-   Values spread inconsistently over the rightmost two columns
-   Missing values stored as text, i.e., "TBD" and "PENDING" should be treated as NA in this case

Here's that data after being read in to R:

``` r
library(pacman) # for loading packages
p_load(readxl, janitor, dplyr)

roster_raw <- read_excel("dirty_data.xlsx") # available at https://github.com/sfirke/janitor/blob/master/dirty_data.xlsx
roster_raw
#> # A tibble: 17 × 12
#>    `First Name` `Last Name` `Employee Status`    Subject `Hire Date` `% Allocated` `Full time?` `do not edit! --->` Certification  Certification Certification     ``
#>           <chr>       <chr>             <chr>      <chr>       <dbl>         <dbl>        <chr>               <dbl>         <chr>          <chr>         <dbl> <dttm>
#> 1         Jason      Bourne           Teacher         PE       39690          0.75          Yes                  NA   Physical ed        Theater            NA   <NA>
#> 2         Jason      Bourne           Teacher   Drafting       39690          0.25          Yes                  NA   Physical ed        Theater            NA   <NA>
#> 3        Alicia        Keys           Teacher      Music       37118          1.00          Yes                  NA  Instr. music    Vocal music            NA   <NA>
#> 4           Ada    Lovelace           Teacher       <NA>       27515          1.00          Yes                  NA       PENDING      Computers            NA   <NA>
#> 5         Desus        Nice    Administration       Dean       41431          1.00          Yes                  NA       PENDING           <NA>            NA   <NA>
#> 6  Chien-Shiung          Wu           Teacher    Physics       11037          0.50          Yes                  NA  Science 6-12        Physics            NA   <NA>
#> 7  Chien-Shiung          Wu           Teacher  Chemistry       11037          0.50          Yes                  NA  Science 6-12        Physics            NA   <NA>
#> 8          <NA>        <NA>              <NA>       <NA>          NA            NA         <NA>                  NA          <NA>           <NA>            NA   <NA>
#> 9         James       Joyce           Teacher    English       32994          0.50           No                  NA          <NA>   English 6-12            NA   <NA>
#> 10         Hedy      Lamarr           Teacher    Science       27919          0.50           No                  NA       PENDING           <NA>            NA   <NA>
#> 11       Carlos      Boozer             Coach Basketball       42221            NA           No                  NA   Physical ed           <NA>            NA   <NA>
#> 12        Young      Boozer             Coach        TBD       34700            NA           No                  NA          <NA> Political sci.            NA   <NA>
#> 13      Micheal      Larsen           Teacher    English       40071          0.80           No                  NA   Vocal music        English            NA   <NA>
#> 14         <NA>        <NA>              <NA>       <NA>          NA            NA         <NA>                  NA          <NA>           <NA>            NA   <NA>
#> 15         <NA>        <NA>              <NA>       <NA>          NA            NA         <NA>                  NA          <NA>           <NA>            NA   <NA>
#> 16         <NA>        <NA>              <NA>       <NA>          NA            NA         <NA>                  NA          <NA>           <NA>            NA   <NA>
#> 17         <NA>        <NA>              <NA>       <NA>          NA            NA         <NA>                  NA          <NA>           <NA>            NA   <NA>
```

The presence of Excel formatting led to an empty column on the right and empty rows below. The (dreadful) column names are preserved, and the duplicate column names cause `dplyr` functions to choke:

``` r
names(roster_raw)
#>  [1] "First Name"        "Last Name"         "Employee Status"   "Subject"           "Hire Date"         "% Allocated"       "Full time?"        "do not edit! --->" "Certification"    
#> [10] "Certification"     "Certification"     ""
roster_raw %>%
  select(Subject, `% Allocated`)
#> Error in eval(substitute(expr), envir, enclos): found duplicated column name: Certification, Certification
```

Let's clean it:

``` r
roster <- roster_raw %>%
  clean_names() %>%
  remove_empty_rows() %>%
  remove_empty_cols() %>%
  convert_to_NA(c("TBD", "PENDING")) %>%
  mutate(hire_date = excel_numeric_to_date(hire_date),
         main_cert = use_first_valid_of(certification, certification_2))

roster
#> # A tibble: 12 × 10
#>      first_name last_name employee_status    subject  hire_date percent_allocated full_time certification certification_2      main_cert
#>           <chr>     <chr>           <chr>      <chr>     <date>             <dbl>     <chr>         <chr>           <chr>          <chr>
#> 1         Jason    Bourne         Teacher         PE 2008-08-30              0.75       Yes   Physical ed         Theater    Physical ed
#> 2         Jason    Bourne         Teacher   Drafting 2008-08-30              0.25       Yes   Physical ed         Theater    Physical ed
#> 3        Alicia      Keys         Teacher      Music 2001-08-15              1.00       Yes  Instr. music     Vocal music   Instr. music
#> 4           Ada  Lovelace         Teacher       <NA> 1975-05-01              1.00       Yes          <NA>       Computers      Computers
#> 5         Desus      Nice  Administration       Dean 2013-06-06              1.00       Yes          <NA>            <NA>           <NA>
#> 6  Chien-Shiung        Wu         Teacher    Physics 1930-03-20              0.50       Yes  Science 6-12         Physics   Science 6-12
#> 7  Chien-Shiung        Wu         Teacher  Chemistry 1930-03-20              0.50       Yes  Science 6-12         Physics   Science 6-12
#> 8         James     Joyce         Teacher    English 1990-05-01              0.50        No          <NA>    English 6-12   English 6-12
#> 9          Hedy    Lamarr         Teacher    Science 1976-06-08              0.50        No          <NA>            <NA>           <NA>
#> 10       Carlos    Boozer           Coach Basketball 2015-08-05                NA        No   Physical ed            <NA>    Physical ed
#> 11        Young    Boozer           Coach       <NA> 1995-01-01                NA        No          <NA>  Political sci. Political sci.
#> 12      Micheal    Larsen         Teacher    English 2009-09-15              0.80        No   Vocal music         English    Vocal music
```

Use `%>% clean_names()` anytime you read data into R.

### Examining dirty data

#### Finding duplicates

`get_dupes()` is for hunting down and examining duplicate records during data cleaning. Here, let's see if any teachers are listed more than once:

``` r
roster %>% get_dupes(first_name, last_name)
#> # A tibble: 4 × 11
#>     first_name last_name dupe_count employee_status   subject  hire_date percent_allocated full_time certification certification_2    main_cert
#>          <chr>     <chr>      <int>           <chr>     <chr>     <date>             <dbl>     <chr>         <chr>           <chr>        <chr>
#> 1 Chien-Shiung        Wu          2         Teacher   Physics 1930-03-20              0.50       Yes  Science 6-12         Physics Science 6-12
#> 2 Chien-Shiung        Wu          2         Teacher Chemistry 1930-03-20              0.50       Yes  Science 6-12         Physics Science 6-12
#> 3        Jason    Bourne          2         Teacher        PE 2008-08-30              0.75       Yes   Physical ed         Theater  Physical ed
#> 4        Jason    Bourne          2         Teacher  Drafting 2008-08-30              0.25       Yes   Physical ed         Theater  Physical ed
```

Yes, some teachers appear twice. We ought to address this before counting employees.

#### Tabulating variables

janitor has several functions for quick counts. The two major ones are `tabyl()` and `crosstab()`.

Notably, they can be called two ways:

-   On vectors - e.g., `tabyl(roster$subject)`
-   On a piped-in data.frame: `roster %>% tabyl(subject)`.
    -   This allows for dplyr commands earlier in the pipeline

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

roster %>%
  filter(hire_date > as.Date("1950-01-01")) %>%
  crosstab(employee_status, full_time)
#>   employee_status No Yes
#> 1  Administration  0   1
#> 2           Coach  2   0
#> 3         Teacher  3   4
```

Other janitor functions "prettify" the results of these tabulation calls for fast, basic reporting. Here are some of the functions that augment tabyls and crosstabs:

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

Together, these tabulation functions fill R's deficit against Excel and SPSS when it comes to quick, informative counts.

Contact me
----------

You are welcome to:

-   submit suggestions and bug-reports: <https://github.com/sfirke/janitor/issues>
-   send a pull request: <https://github.com/sfirke/janitor/>
-   compose a friendly e-mail to: <img src = "http://samfirke.com/wp-content/uploads/2016/07/email_address_whitespace_top.png" alt = "samuel.firke AT gmail" width = "210"/>
