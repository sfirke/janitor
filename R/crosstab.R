#' @title Generate a crosstabulation of two vectors.
#'
#' @description
#' Create a crosstab, displaying either frequencies or percentages calculated by row, column, or overall.  Vectors don't have to be from the same data.frame, but typically are.
#'
#' @param vec1 the vector to place on the crosstab column.
#' @param vec2 the vector to place on the crosstab row.
#' @param percent which grouping to use for percentages, if desired (defaults to counts).  Must be one of "none", "row", "col", or "all".
#' @param show_na should cases where either variable is NA be included?
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
  if(!mode(vec1) %in% c("logical", "numeric", "character")){
    stop("vec1 must be a vector of type logical, numeric, character, or factor")}
  if(!mode(vec2) %in% c("logical", "numeric", "character")){
    stop("vec2 must be a vector of type logical, numeric, character, or factor")}
  if(length(vec1) != length(vec2)){ stop("the two vectors are not the same length")}

  dat <- data.frame(vec1, vec2, stringsAsFactors = FALSE)
  var_name <- deparse(substitute(vec1))

  if(!show_na){
    dat <- dat[!is.na(dat$vec1) & !is.na(dat$vec2), ]
  }
  
  # create long data.frame with initial counts
  tabl <- dat %>%
    dplyr::count(vec1, vec2) %>%
    dplyr::ungroup()
  
  # calculate percentages, if specified
  if(percent == "row"){
    tabl <- tabl %>%
      dplyr::group_by(vec1) %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE))
  } else if (percent == "col"){
    tabl <- tabl %>%
      dplyr::group_by(vec2) %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE))
  } else if (percent == "all"){
    tabl <- tabl %>%
      dplyr::mutate(n = n / sum(n, na.rm = TRUE))
  }
  
  # replace NA with string NA in vec2 to avoid invalid col name after spreading
    # if this col is a factor, need to add that level to the factor
  if(is.factor(tabl$vec2)){
    levels(tabl$vec2) <- c(levels(tabl$vec2), "NA")
  }
  tabl$vec2[is.na(tabl$vec2)] <- "NA"

  # spread to wide, ungroup() for cleanliness of result, and rename 1st col
  tabl %>%
    tidyr::spread(vec2, n) %>%
    dplyr::ungroup() %>%
    stats::setNames(., c(var_name, names(.)[-1]))
}
