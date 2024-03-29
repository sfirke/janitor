% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_df_cols.R
\name{compare_df_cols}
\alias{compare_df_cols}
\title{Compare data frames columns before merging}
\usage{
compare_df_cols(
  ...,
  return = c("all", "match", "mismatch"),
  bind_method = c("bind_rows", "rbind"),
  strict_description = FALSE
)
}
\arguments{
\item{...}{A combination of data.frames, tibbles, and lists of
data.frames/tibbles.  The values may optionally be named arguments; if
named, the output column will be the name; if not named, the output column
will be the data.frame name (see examples section).}

\item{return}{Should a summary of "all" columns be returned, only return
"match"ing columns, or only "mismatch"ing columns?}

\item{bind_method}{What method of binding should be used to determine
matches? With "bind_rows", columns missing from a data.frame would be
considered a match (as in \code{dplyr::bind_rows()}; with "rbind", columns
missing from a data.frame would be considered a mismatch (as in
\code{base::rbind()}.}

\item{strict_description}{Passed to \code{describe_class}.  Also, see the
Details section.}
}
\value{
A data.frame with a column named "column_name" with a value named
after the input data.frames' column names, and then one column per
data.frame (named after the input data.frame).  If more than one input has
the same column name, the column naming will have suffixes defined by
sequential use of \code{base::merge()} and may differ from expected naming.
The rows within the data.frame-named columns are descriptions of the
classes of the data within the columns (generated by
\code{describe_class}).
}
\description{
Generate a comparison of data.frames (or similar objects) that indicates if
they will successfully bind together by rows.
}
\details{
Due to the returned "column_name" column, no input data.frame may be
named "column_name".

The \code{strict_description} argument is most typically used to understand
if factor levels match or are bindable.  Factors are typically bindable,
but the behavior of what happens when they bind differs based on the
binding method ("bind_rows" or "rbind").  Even when
\code{strict_description} is \code{FALSE}, data.frames may still bind
because some classes (like factors and characters) can bind even if they
appear to differ.
}
\examples{
compare_df_cols(data.frame(A = 1), data.frame(B = 2))
# user-defined names
compare_df_cols(dfA = data.frame(A = 1), dfB = data.frame(B = 2))
# a combination of list and data.frame input
compare_df_cols(listA = list(dfA = data.frame(A = 1), dfB = data.frame(B = 2)), data.frame(A = 3))
}
\seealso{
Other data frame type comparison: 
\code{\link{compare_df_cols_same}()},
\code{\link{describe_class}()}
}
\concept{data frame type comparison}
