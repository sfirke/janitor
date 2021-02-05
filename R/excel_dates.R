#' @title Convert dates encoded as serial numbers to Date class.
#'
#' @description Converts numbers like \code{42370} into date values like
#' \code{2016-01-01}.
#'
#' Defaults to the modern Excel date encoding system. However, Excel for Mac
#' 2008 and earlier Mac versions of Excel used a different date system. To
#' determine what platform to specify: if the date 2016-01-01 is represented by
#' the number 42370 in your spreadsheet, it's the modern system.  If it's 40908,
#' it's the old Mac system. More on date encoding systems at
#' http://support.office.com/en-us/article/Date-calculations-in-Excel-e7fe7167-48a9-4b96-bb53-5612a800b487.
#'
#' A list of all timezones is available from \code{base::OlsonNames()}, and the
#' current timezone is available from \code{base::Sys.timezone()}.
#'
#' If your input data has a mix of Excel numeric dates and actual dates, see the
#' more powerful functions \code{convert_to_date()} and \code{convert_to_datetime()}.
#'
#' @param date_num numeric vector of serial numbers to convert.
#' @param date_system the date system, either \code{"modern"} or \code{"mac
#'   pre-2011"}.
#' @param include_time Include the time (hours, minutes, seconds) in the output?
#'   (See details)
#' @param round_seconds Round the seconds to an integer (only has an effect when
#'   \code{include_time} is \code{TRUE})?
#' @param tz Time zone, used when \code{include_time = TRUE} (see details for
#'   more information on timezones).
#' @return Returns a vector of class Date if \code{include_time} is
#'   \code{FALSE}.  Returns a vector of class POSIXlt if \code{include_time} is
#'   \code{TRUE}.
#' @details When using \code{include_time=TRUE}, days with leap seconds will not
#'   be accurately handled as they do not appear to be accurately handled by
#'   Windows (as described in
#'   https://support.microsoft.com/en-us/help/2722715/support-for-the-leap-second).
#'
#' @export
#' @examples
#' excel_numeric_to_date(40000)
#' excel_numeric_to_date(40000.5) # No time is included
#' excel_numeric_to_date(40000.5, include_time = TRUE) # Time is included
#' excel_numeric_to_date(40000.521, include_time = TRUE) # Time is included
#' excel_numeric_to_date(40000.521, include_time = TRUE,
#'   round_seconds = FALSE) # Time with fractional seconds is included
#' @family Date-time cleaning
#' @importFrom lubridate as_date as_datetime force_tz hour minute second
excel_numeric_to_date <- function(date_num, date_system = "modern", include_time = FALSE, round_seconds = TRUE, tz = Sys.timezone()) {
  if (all(is.na(date_num))) {
    # For NA input, return the expected type of NA output.
    if (include_time) {
      return(lubridate::as_datetime(date_num, tz=tz))
    } else {
      return(lubridate::as_date(date_num))
    }
  } else if (!is.numeric(date_num)) {
    stop("argument `date_num` must be of class numeric")
  }

  # Manage floating point imprecision; coerce to double to avoid integer
  # overflow.
  date_num_days <- (as.double(date_num) * 86400L + 0.001) %/% 86400L
  date_num_days_no_floating_correction <- date_num %/% 1
  # If the day rolls over due to machine precision, then the seconds should be zero
  mask_day_rollover <- !is.na(date_num) & date_num_days > date_num_days_no_floating_correction
  date_num_seconds <- (date_num - date_num_days) * 86400
  date_num_seconds[mask_day_rollover] <- 0
  if (round_seconds) {
    date_num_seconds <- round(date_num_seconds)
  }
  if (any(mask_day_rollover)) {
    warning(sum(mask_day_rollover), " date_num values are within 0.001 sec of a later date and were rounded up to the next day.")
  }
  mask_excel_leap_day_bug <- !is.na(date_num_days) & floor(date_num_days) == 60
  mask_before_excel_leap_day_bug <- !is.na(date_num_days) & floor(date_num_days) < 60
  date_num_days[mask_excel_leap_day_bug] <- NA_real_
  if (any(!is.na(date_num_days) & (date_num_days < 1))) {
    warning("Only `date_num` >= 1 are valid in Excel, creating an earlier date than Excel supports.")
  }
  date_num_days[mask_before_excel_leap_day_bug] <- date_num_days[mask_before_excel_leap_day_bug] + 1
  ret <-
    if (date_system == "mac pre-2011") {
      lubridate::as_date(floor(date_num_days), origin = "1904-01-01")
    } else if (date_system == "modern") {
      lubridate::as_date(floor(date_num_days), origin = "1899-12-30")
    } else {
      stop("argument 'date_system' must be one of 'mac pre-2011' or 'modern'")
    }
  if (include_time) {
    ret <- lubridate::as_datetime(ret)
    lubridate::second(ret) <- date_num_seconds %% 60
    lubridate::minute(ret) <- floor(date_num_seconds/60) %% 60
    lubridate::hour(ret) <- floor(date_num_seconds/3600)
    ret <- lubridate::force_tz(ret, tzone=tz)
  }
  if (any(mask_excel_leap_day_bug)) {
    warning("NAs introduced by coercion, Excel leap day bug detected in `date_num`.  29 February 1900 does not exist.")
  }
  if (any(is.na(ret) & !is.na(date_num) & !mask_excel_leap_day_bug)) {
    warning("NAs introduced by coercion, possible daylight savings time issue with input.  Consider `tz='UTC'`.")
  }
  ret
}
