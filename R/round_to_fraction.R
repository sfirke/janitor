#' Round a value to a fraction, optionally then rounding to a number of digits.
#'
#' @details If \code{digits} is \code{Inf}, \code{x} is rounded to the fraction
#'   and then kept at full precision.  If \code{digits} is \code{"auto"}, the
#'   number of digits is automatically selected as
#'   \code{ceiling(log10(denominator)) + 1}.
#'
#' @param x A numeric vector
#' @param denominator The denominator of the fraction for rounding (a scalar or
#'   vector positive integer).
#' @param digits Integer indicating the number of decimal places to be used
#'   (passed \code{base::round()}). Negative values are allowed (see
#'   Details). The number of digits for rounding after rounding to the
#'   fraction (\code{Inf} indicates no subsequent rounding)
#' @return x rounded to a decimal value that has an integer numerator relative
#'   to \code{denominator} (possibly subsequently rounded to a number of decimal
#'   digits).
#' @examples
#' round_to_fraction(1.6, denominator=2)
#' round_to_fraction(pi, denominator=7) # 22/7
#' round_to_fraction(c(8.1, 9.2), denominator=c(7, 8))
#' round_to_fraction(c(8.1, 9.2), denominator=c(7, 8), digits=3)
#' round_to_fraction(c(8.1, 9.2, 10.3), denominator=c(7, 8, 1001), digits="auto")
#' @export
round_to_fraction <- function(x, denominator, digits = Inf) {
  stopifnot(is.numeric(x))
  stopifnot(length(denominator) %in% c(1, length(x)))
  stopifnot(is.numeric(denominator))
  stopifnot(denominator >= 1)
  if (!(identical(digits, "auto") ||
        (is.numeric(digits) & (length(digits) %in% c(1, length(x)))))) {
    stop('`digits` must be either "auto" or a number that is either a scalar or the same length as `x`.')
  }
  ret <- round(x * denominator, digits=0) / denominator
  if (identical(digits, "auto")) {
    digits <- ceiling(log10(denominator)) + 1
  }
  mask_inf_digits <- is.infinite(digits)
  if (!all(mask_inf_digits)) {
    ret[!mask_inf_digits] <-
      round(ret[!mask_inf_digits], digits = digits[!mask_inf_digits])
  }
  ret
}
