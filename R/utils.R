# Copied from tidyr/R/utils.R, to export the magrittr pipe

#' Pipe operator
#'
#' @description Exported from the magrittr package.  To learn more, run \code{?magrittr::`\%>\%`}.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom dplyr %>%
#' @usage lhs \%>\% rhs
#' @examples
#' mtcars %>%
#'   crosstab(carb, cyl) %>%
#'   adorn_totals()
NULL