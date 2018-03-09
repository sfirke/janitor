% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adorn_title.R
\name{adorn_title}
\alias{adorn_title}
\title{Add column name to the top of a two-way tabyl.}
\usage{
adorn_title(dat, placement = "top", row_name, col_name)
}
\arguments{
\item{dat}{a data.frame of class \code{tabyl} or other data.frame with a tabyl-like layout.  If given a list of data.frames, this function will apply itself to each data.frame in the list (designed for 3-way \code{tabyl} lists).}

\item{placement}{whether the column name should be added to the top of the tabyl in an otherwise-empty row \code{"top"} or appended to the already-present row name variable (\code{"combined"}).  The formatting in the \code{"top"} option has the look of base R's \code{table()}; it also wipes out the other column names, making it hard to further use the data.frame besides formatting it for reporting.  The \code{"combined"} option is more conservative in this regard.}

\item{row_name}{(optional) default behavior is to pull the row name from the attributes of the input \code{tabyl} object.  If you wish to override that text, or if your input is not a \code{tabyl}, supply a string here.}

\item{col_name}{(optional) default behavior is to pull the column_name from the attributes of the input \code{tabyl} object.  If you wish to override that text, or if your input is not a \code{tabyl}, supply a string here.}
}
\value{
the input tabyl, augmented with the column title.  Non-tabyl inputs that are of class \code{tbl_df} are downgraded to basic data.frames so that the title row prints correctly.
}
\description{
This function adds the column variable name to the top of a \code{tabyl} for a complete display of information.  This makes the tabyl prettier, but renders the data.frame less useful for further manipulation.
}
\examples{

mtcars \%>\%
  tabyl(am, cyl) \%>\%
  adorn_title(placement = "top")

# Adding a title to a non-tabyl
library(tidyr); library(dplyr)
mtcars \%>\%
  group_by(gear, am) \%>\%
  summarise(avg_mpg = mean(mpg)) \%>\%
  spread(gear, avg_mpg) \%>\%
  adorn_title("top", row_name = "Gears", col_name = "Cylinders")
}