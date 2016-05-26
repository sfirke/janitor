#' @title Convert all \code{labelled}-class columns to factors.
#'
#' @description
#' Takes a data.frame, checks for columns that are class \code{labelled} from the \code{haven} package, and converts them to factor class.
#'
#' @param labels_df a data.frame containing some columns of class labelled
#' @return Returns a data.frame.
#' @export
#' @examples
#' # not run
#' # haven::read_spss(filepath) %>% labelled_to_factor()

# Convert labelled columns to factors
labelled_to_factors <- function(labels_df){


  labeled_var_index <- unlist(
    lapply(labels_df, function(x) class(x) == "labelled")
  )
  factorized <- labels_df
  factorized[, labeled_var_index] <- lapply(factorized[, labeled_var_index], as_factor)


  # reset label attributes - maybe not needed, and not working
  question_text <- unlist(
    lapply(raw, function(x) {attr(x, "label")})
  )

  for(i in seq_along(factorized)){
    attr(factorized[[i]], "label") <- question_text[names(factorized)[i]]
  }

  factorized
}