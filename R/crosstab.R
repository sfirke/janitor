#' @importFrom magrittr "%>%"
#'
#' @title Generate a crosstabulation of two vectors.
#'
#' @description
#' Create a crosstab, displaying either frequencies or percentages calculated by row, column, or overall.  Vectors don't have to be from the same data.frame, but typically are.
#'
#' @param vec1 the vector to place on the crosstab column.
#' @param vec2 the vector to place on the crosstab row.
#' @param percent which grouping to use for percentages, if desired (defaults to counts).
#' @param show_na should cases where both variables are NA be included?
#' @return Returns a data.frame (actually a \code{tbl_df}) with the frequencies of the crosstabulated variables.
#' @export
#' @examples
#' crosstab(mtcars$cyl, mtcars$gear)
#' crosstab(mtcars$cyl, mtcars$gear, "row")
#'

# Crosstab table of two variables
# Take two vectors and one of "none", "row", "col", and "full" to calculate %s
# Could also take a data.frame and two vector names, for pipeline, but this seems simpler
crosstab <- function(vec1, vec2, percent = "none", show_na = TRUE){

  if(length(vec1) != length(vec2)){ stop("the two vectors are not the same length")}

  dat <- data.frame(vec1, vec2)
  var_name <- deparse(substitute(vec1))

  if(!show_na){
    dat <- dat[!is.na(dat$vec1) & !is.na(dat$vec2), ]
  }
  tabl <- dat %>%
    dplyr::count(vec1, vec2) %>%
    dplyr::ungroup()
  
  if(percent == "none"){
    tabl <- tabl %>%
      tidyr::spread(vec2, n) %>%
      dplyr::ungroup()
  } else if(percent == "row"){
    tabl <- tabl %>%
      dplyr::group_by(vec1) %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE)) %>%
      tidyr::spread(vec2, n) %>%
      dplyr::ungroup()
  } else if (percent == "col"){
    tabl <- tabl %>%
      dplyr::group_by(vec2) %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE)) %>%
      tidyr::spread(vec2, n) %>%
      dplyr::ungroup()
  } else if (percent == "all"){
    tabl <- tabl %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE)) %>%
      tidyr::spread(vec2, n) %>%
      dplyr::ungroup()
  }
  stats::setNames(tabl, c(var_name, names(tabl)[-1])) # put name back of 1st variable
}
