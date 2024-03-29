% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adorn_title.R
\name{adorn_title}
\alias{adorn_title}
\title{Add column name to the top of a two-way tabyl.}
\usage{
adorn_title(dat, placement = "top", row_name, col_name)
}
\arguments{
\item{dat}{A \code{data.frame} of class \code{tabyl} or other \code{data.frame} with a tabyl-like layout.
If given a list of data.frames, this function will apply itself to each \code{data.frame}
in the list (designed for 3-way \code{tabyl} lists).}

\item{placement}{The title placement, one of \code{"top"}, or \code{"combined"}.
See \strong{Details} for more information.}

\item{row_name}{(optional) default behavior is to pull the row name from the
attributes of the input \code{tabyl} object.  If you wish to override that text,
or if your input is not a \code{tabyl}, supply a string here.}

\item{col_name}{(optional) default behavior is to pull the column_name from
the attributes of the input \code{tabyl} object.  If you wish to override that text,
or if your input is not a \code{tabyl}, supply a string here.}
}
\value{
The input \code{tabyl}, augmented with the column title.  Non-tabyl inputs
that are of class \code{tbl_df} are downgraded to basic data.frames so that the
title row prints correctly.
}
\description{
This function adds the column variable name to the top of a \code{tabyl} for a
complete display of information.  This makes the tabyl prettier, but renders
the \code{data.frame} less useful for further manipulation.
}
\details{
The \code{placement} argument indicates whether the column name should be added to
the \code{top} of the tabyl in an otherwise-empty row \code{"top"} or appended to the
already-present row name variable (\code{"combined"}). The formatting in the \code{"top"}
option has the look of base R's \code{table()}; it also wipes out the other column
names, making it hard to further use the \code{data.frame} besides formatting it for reporting.
The \code{"combined"} option is more conservative in this regard.
}
\examples{

mtcars \%>\%
  tabyl(am, cyl) \%>\%
  adorn_title(placement = "top")

# Adding a title to a non-tabyl
library(tidyr)
library(dplyr)
mtcars \%>\%
  group_by(gear, am) \%>\%
  summarise(avg_mpg = mean(mpg), .groups = "drop") \%>\%
  pivot_wider(names_from = am, values_from = avg_mpg) \%>\%
  adorn_rounding() \%>\%
  adorn_title("top", row_name = "Gears", col_name = "Cylinders")
}
