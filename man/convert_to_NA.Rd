% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/janitor_deprecated.R
\name{convert_to_NA}
\alias{convert_to_NA}
\title{Convert string values to true \code{NA} values.}
\usage{
convert_to_NA(dat, strings)
}
\arguments{
\item{dat}{vector or data.frame to operate on.}

\item{strings}{character vector of strings to convert.}
}
\value{
Returns a cleaned object.  Can be a vector, data.frame, or \code{tibble::tbl_df} depending on the provided input.
}
\description{
Warning: Deprecated, do not use in new code. Use \code{\link[dplyr:na_if]{dplyr::na_if()}} instead.

Converts instances of user-specified strings into \code{NA}.  Can operate on either a single vector or an entire data.frame.
}
\seealso{
janitor_deprecated
}
\keyword{internal}
