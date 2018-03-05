#' @title Generate a frequency table of a factor grouped into top-n, bottom-n, and all other levels.
#'
#' @description
#' Get a frequency table of a factor variable, grouped into categories by level.
#'
#' @param input_vec the factor variable to tabulate.
#' @param n number of levels to include in top and bottom groups
#' @param show_na should cases where the variable is NA be shown?
#' @return Returns a data.frame (actually a \code{tbl_df}) with the frequencies of the grouped, tabulated variable.  Includes counts and percentages, and valid percentages (calculated omitting \code{NA} values, if present in the vector and \code{show_na = TRUE}.)
#' @export
#' @examples
#' top_levels(as.factor(mtcars$hp), 2)


top_levels <- function(input_vec, n = 2, show_na = FALSE) {

  # Initial type error catching
  if (!is.factor(input_vec)) {
    stop("factor_vec is not of type 'factor'")
  }

  num_levels_in_var <- nlevels(input_vec)

  # handle bad inputs
  if (!num_levels_in_var > 2) {
    stop("input factor variable must have at least 3 levels")
  }
  if (num_levels_in_var < 2 * n) {
    stop(paste0("there are ", num_levels_in_var, " levels in the variable and ", n, " levels in each of the top and bottom groups.\nSince 2 * ", n, " = ", 2 * n, " is greater than ", num_levels_in_var, ", there would be overlap in the top and bottom groups and some records will be double-counted."))
  }
  if (n < 1 || n %% 1 != 0) {
    stop("n must be a whole number at least 1")
  }

  var_name <- deparse(substitute(input_vec))

  # Identify top/mid/bottom group labels for printing
  groups <- get_level_groups(input_vec, n, num_levels_in_var)

  # convert input vector into grouped variable
  new_vec <- ifelse(as.numeric(input_vec) <= n,
                    groups$top,
                    ifelse(as.numeric(input_vec) > (num_levels_in_var - n),
                           groups$bot,
                           groups$mid
                    )
  )

  # recode variable as hi-med-lo factor so table prints w/ correct sorting
  if (!is.na(groups$mid)) {
    new_vec <- factor(new_vec, levels = c(groups$top, groups$mid, groups$bot))
  } else {
    new_vec <- factor(new_vec, levels = c(groups$top, groups$bot))
  }

  # tabulate grouped variable, then reset name to match input variable name
  result <- tabyl(new_vec, show_na = show_na)
  names(result)[1] <- var_name
  result
}
