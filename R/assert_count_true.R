#' Verify that a vector of values has the expected number of `TRUE` values
#'
#' @param x A logical vecotor without `NA` values
#' @param n The expected number of `TRUE` values
#' @returns `x` if `sum(x) == n` or an informative error message otherwise
#' @examples
#' data.frame(A = 1:5) %>%
#'   dplyr::mutate(
#'     big_values = assert_count_true(A > 2, n = 3)
#'   )
#' @export
assert_count_true <- function(x, n = 1) {
  stopifnot(is.logical(x))
  if (any(is.na(x))) {
    stop(deparse(substitute(x)), " has NA values")
  }
  if (sum(x) != n) {
    stop_message <-
      sprintf(
        "`%s` expected %g `TRUE` %s but %g %s found.",
        deparse(substitute(x)),
        n,
        ngettext(n, "value", "values"),
        sum(x),
        ngettext(sum(x), "was", "were")
      )
    stop(stop_message)
  }
  x
}
