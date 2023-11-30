#' Convert a time that may be inconsistently or inconveniently formatted from
#' Microsoft Excel to a numeric number of seconds between 0 and 86400.
#'
#' @details
#'
#' \code{time_value} may be one of the following formats:
#' \itemize{
#'   \item{numeric}{The input must be a value from 0 to 1 (exclusive of 1); this value is returned as-is.}
#'   \item{POSIXlt or POSIXct}{The input must be on the day 1899-12-31 (any other day will cause an error).  The time of day is extracted and converted to a fraction of a day.}
#'   \item{character}{Any of the following (or a mixture of the choices):}
#'   \itemize{
#'     \item{A character string that is a number between 0 and 1 (exclusive of 1).  This value will be converted like a numeric value.}
#'     \item{A character string that looks like a date on 1899-12-31 (specifically, it must start with \code{"1899-12-31 "}), converted like a POSIXct object as described above.}
#'     \item{A character string that looks like a time.  Choices are 12-hour time as hour, minute, and optionally second followed by "am" or "pm" (case insensitive) or 24-hour time when hour, minute, optionally second, and no "am" or "pm" is included.}
#'   }
#' }
#'
#' @param time_value A vector of values to convert (see Details)
#' @param round_seconds Should the output number of seconds be rounded to an
#'   integer?
#' @return A vector of numbers >= 0 and <86400
#' @family Date-time cleaning
#' @seealso \code{\link{excel_numeric_to_date}}
#' @export
excel_time_to_numeric <- function(time_value, round_seconds = TRUE) {
  UseMethod("excel_time_to_numeric")
}

#' @export
excel_time_to_numeric.logical <- function(time_value, round_seconds = TRUE) {
  if (all(is.na(time_value))) {
    rep(NA_real_, length(time_value))
  } else {
    stop("If given as a logical vector, all values must be ")
  }
}

#' @export
excel_time_to_numeric.numeric <- function(time_value, round_seconds = TRUE) {
  if (all(is.na(time_value) |
          (time_value >= 0 &
           time_value < 1))) {
    seconds <- time_value * 86400
    if (round_seconds) {
      seconds <- round(seconds)
    }
  } else {
    stop("When numeric, all `time_value`s must be between 0 and 1 (exclusive of 1)")
  }
  seconds
}

#' @export
excel_time_to_numeric.POSIXct <- function(time_value, round_seconds = TRUE) {
  # using trunc removes timezone inconsistency.  Timezones aren't used in Excel.
  seconds <- as.numeric(time_value) - as.numeric(trunc(time_value, units = "days"))
  mask_good_seconds <- is.na(seconds) | (seconds >= 0 & seconds < 86400)
  if (all(mask_good_seconds)) {
    if (round_seconds) {
      seconds <- round(seconds)
    }
  } else {
    # This should be impossible except for leap seconds
    stop(sum(!mask_good_seconds), " `time_value`s were not at or above 0 and below 86400.") # nocov
  }
  seconds
}

#' @export
excel_time_to_numeric.POSIXlt <- function(time_value, round_seconds = TRUE) {
  excel_time_to_numeric.POSIXct(
    as.POSIXct(time_value),
    round_seconds = round_seconds
  )
}

#' @export
excel_time_to_numeric.character <- function(time_value, round_seconds = TRUE) {
  ret <- rep(NA_real_, length(time_value))
  patterns <-
    list(
      number = "^0(\\.[0-9]*)?$",
      # SI numbers have to have the form [number]E-[number] becasue the number
      # has to be between 0 and 1 and can't be bigger than 1.
      si_number = "^[1-9](\\.[0-9]*)?E-[0-9]+$",
      "12hr" = "^([0]?[1-9]|1[0-2]):([0-5][0-9])(?::([0-5][0-9]))? ?([AP]M)$",
      "24hr" = "^([0-1]?[0-9]|2[0-3]):([0-5][0-9])(?::([0-5][0-9]))?$",
      # The ".*?" at the end of POSIX is to allow for a time zone, but it allows
      #   for imperfect parsing if there were just a date and a space.
      # The the entire time is optional to allow for midnight which shows as
      #   just the date and time zone.
      POSIX = "1899-12-31 (?:([0-1]?[0-9]|2[0-3]):([0-5][0-9])(?::([0-5][0-9]))?)?.*?$"
    )
  mask_na <- is.na(time_value)
  mask_number <-
    grepl(pattern = patterns$number, x = time_value) |
      grepl(pattern = patterns$si_number, x = time_value)
  mask_POSIX <- grepl(pattern = patterns[["POSIX"]], x = time_value)
  mask_12hr <- grepl(pattern = patterns[["12hr"]], x = time_value, ignore.case = TRUE)
  mask_24hr <- grepl(pattern = patterns[["24hr"]], x = time_value)
  unmatched <- !(mask_na | mask_number | mask_POSIX | mask_12hr | mask_24hr)
  if (any(unmatched)) {
    stop(
      "The following character strings did not match an interpretable ",
      "character format for time conversion: ",
      paste(unique(time_value[unmatched]))
    )
  }
  if (any(mask_number)) {
    ret[mask_number] <-
      excel_time_to_numeric.numeric(
        time_value = as.numeric(time_value[mask_number]),
        round_seconds = round_seconds
      )
  }
  mask_clock <- mask_12hr | mask_24hr | mask_POSIX
  if (any(mask_clock)) {
    hours <- minutes <- seconds <- rep(NA_real_, length(time_value))
    if (any(mask_POSIX)) {
      hours[mask_POSIX] <-
        gsub(pattern = patterns$POSIX, replacement = "\\1", x = time_value[mask_POSIX])
      minutes[mask_POSIX] <-
        gsub(pattern = patterns$POSIX, replacement = "\\2", x = time_value[mask_POSIX])
      seconds[mask_POSIX] <-
        gsub(pattern = patterns$POSIX, replacement = "\\3", x = time_value[mask_POSIX])
    }
    if (any(mask_12hr)) {
      mask_pm <- rep(FALSE, length(time_value))
      hours[mask_12hr] <-
        gsub(pattern = patterns[["12hr"]], replacement = "\\1", x = time_value[mask_12hr], ignore.case = TRUE)
      minutes[mask_12hr] <-
        gsub(pattern = patterns[["12hr"]], replacement = "\\2", x = time_value[mask_12hr], ignore.case = TRUE)
      seconds[mask_12hr] <-
        gsub(pattern = patterns[["12hr"]], replacement = "\\3", x = time_value[mask_12hr], ignore.case = TRUE)
      # 12 is 0 hours in the AM and the PM conversion below adds the needed 12
      # at noon.
      mask_0_hours <- mask_12hr & (hours %in% "12")
      hours[mask_0_hours] <- "0"
      mask_pm[mask_12hr] <-
        tolower(
          gsub(pattern = patterns[["12hr"]], replacement = "\\4", x = time_value[mask_12hr], ignore.case = TRUE)
        ) %in% "pm"
      hours[mask_pm] <- 12 + as.numeric(hours[mask_pm])
    }
    if (any(mask_24hr)) {
      hours[mask_24hr] <-
        gsub(pattern = patterns[["24hr"]], replacement = "\\1", x = time_value[mask_24hr])
      minutes[mask_24hr] <-
        gsub(pattern = patterns[["24hr"]], replacement = "\\2", x = time_value[mask_24hr])
      seconds[mask_24hr] <-
        gsub(pattern = patterns[["24hr"]], replacement = "\\3", x = time_value[mask_24hr])
    }
    hours[hours %in% ""] <- "0"
    minutes[minutes %in% ""] <- "0"
    seconds[seconds %in% ""] <- "0"

    ret[mask_clock] <-
      as.numeric(hours[mask_clock]) * 3600 +
      as.numeric(minutes[mask_clock]) * 60 +
      as.numeric(seconds[mask_clock])
  }
  if (round_seconds) {
    ret <- round(ret)
  }
  ret
}
