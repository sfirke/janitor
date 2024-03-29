% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_df_cols.R
\name{compare_df_cols_same}
\alias{compare_df_cols_same}
\title{Do the the data.frames have the same columns & types?}
\usage{
compare_df_cols_same(
  ...,
  bind_method = c("bind_rows", "rbind"),
  verbose = TRUE
)
}
\arguments{
\item{...}{A combination of data.frames, tibbles, and lists of
data.frames/tibbles.  The values may optionally be named arguments; if
named, the output column will be the name; if not named, the output column
will be the data.frame name (see examples section).}

\item{bind_method}{What method of binding should be used to determine
matches? With "bind_rows", columns missing from a data.frame would be
considered a match (as in \code{dplyr::bind_rows()}; with "rbind", columns
missing from a data.frame would be considered a mismatch (as in
\code{base::rbind()}.}

\item{verbose}{Print the mismatching columns if binding will fail.}
}
\value{
\code{TRUE} if row binding will succeed or \code{FALSE} if it will fail.
}
\description{
Check whether a set of data.frames are row-bindable. Calls \code{compare_df_cols()}
and returns \code{TRUE} if there are no mis-matching rows.
}
\examples{
compare_df_cols_same(data.frame(A = 1), data.frame(A = 2))
compare_df_cols_same(data.frame(A = 1), data.frame(B = 2))
compare_df_cols_same(data.frame(A = 1), data.frame(B = 2), verbose = FALSE)
compare_df_cols_same(data.frame(A = 1), data.frame(B = 2), bind_method = "rbind")
}
\seealso{
Other data frame type comparison: 
\code{\link{compare_df_cols}()},
\code{\link{describe_class}()}
}
\concept{data frame type comparison}
