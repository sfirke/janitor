% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_to_date.R
\name{convert_to_date}
\alias{convert_to_date}
\alias{convert_to_datetime}
\title{Parse dates from many formats}
\usage{
convert_to_date(
  x,
  ...,
  character_fun = lubridate::ymd,
  string_conversion_failure = c("error", "warning")
)

convert_to_datetime(
  x,
  ...,
  tz = "UTC",
  character_fun = lubridate::ymd_hms,
  string_conversion_failure = c("error", "warning")
)
}
\arguments{
\item{x}{The object to convert}

\item{...}{Passed to further methods.  Eventually may be passed to
\code{excel_numeric_to_date()}, \code{base::as.POSIXct()}, or \code{base::as.Date()}.}

\item{character_fun}{A function to convert non-numeric-looking, non-\code{NA} values
in \code{x} to POSIXct objects.}

\item{string_conversion_failure}{If a character value fails to parse into the
desired class and instead returns \code{NA}, should the function return the
result with a warning or throw an error?}

\item{tz}{The timezone for POSIXct output, unless an object is POSIXt
already.  Ignored for Date output.}
}
\value{
POSIXct objects for \code{convert_to_datetime()} or Date objects for
\code{convert_to_date()}.
}
\description{
Convert many date and date-time (POSIXct) formats as may be received
from Microsoft Excel.
}
\details{
Character conversion checks if it matches something that looks like a
Microsoft Excel numeric date, converts those to numeric, and then runs
convert_to_datetime_helper() on those numbers.  Then, character to Date or
POSIXct conversion occurs via \code{character_fun(x, ...)} or
\code{character_fun(x, tz=tz, ...)}, respectively.
}
\examples{
convert_to_date("2009-07-06")
convert_to_date(40000)
convert_to_date("40000.1")
# Mixed date source data can be provided.
convert_to_date(c("2020-02-29", "40000.1"))
convert_to_datetime(
  c("2009-07-06", "40000.1", "40000", NA),
  character_fun = lubridate::ymd_h, truncated = 1, tz = "UTC"
)
}
\seealso{
Other date-time cleaning: 
\code{\link{excel_numeric_to_date}()},
\code{\link{excel_time_to_numeric}()},
\code{\link{sas_numeric_to_date}()}
}
\concept{date-time cleaning}
